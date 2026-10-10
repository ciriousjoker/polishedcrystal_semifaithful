//! Transport proof only. Persistent lobbies and transactions follow later.
use mobile_protocol::{payload_len, Frame, ECHO_REPLY, ECHO_REQUEST, HEADER_LEN};
use std::io;
use std::time::Duration;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};

pub async fn read_frame<S: AsyncRead + Unpin>(stream: &mut S) -> io::Result<Frame> {
    let mut header = [0u8; HEADER_LEN];
    // EOF before a new header is a clean disconnect. EOF inside a frame is
    // malformed input and must not be mistaken for that clean boundary.
    stream.read_exact(&mut header[..1]).await?;
    stream
        .read_exact(&mut header[1..])
        .await
        .map_err(truncated)?;
    let length = payload_len(&header)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, format!("{error:?}")))?;
    let mut bytes = header.to_vec();
    bytes.resize(HEADER_LEN + length, 0);
    stream
        .read_exact(&mut bytes[HEADER_LEN..])
        .await
        .map_err(truncated)?;
    Frame::decode(&bytes)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, format!("{error:?}")))
}

fn truncated(error: io::Error) -> io::Error {
    if error.kind() == io::ErrorKind::UnexpectedEof {
        io::Error::new(io::ErrorKind::InvalidData, "truncated frame")
    } else {
        error
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub enum TestFault {
    #[default]
    None,
    Split,
    BadMagic,
    Silent,
    Close,
    Oversize,
    Truncated,
}

pub async fn serve_connection<S: AsyncRead + AsyncWrite + Unpin>(
    mut stream: S,
    session: u64,
    fault: TestFault,
) -> io::Result<()> {
    loop {
        let frame =
            match tokio::time::timeout(Duration::from_secs(3), read_frame(&mut stream)).await {
                Ok(Ok(frame)) => frame,
                Ok(Err(error)) if error.kind() == io::ErrorKind::UnexpectedEof => return Ok(()),
                Ok(Err(error)) => return Err(error),
                Err(_) => {
                    return Err(io::Error::new(
                        io::ErrorKind::TimedOut,
                        "frame deadline exceeded",
                    ))
                }
            };
        if frame.kind != ECHO_REQUEST {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "unsupported message",
            ));
        }
        println!(
            "ECHO session={session} sequence={} bytes={}",
            frame.sequence,
            frame.payload.len()
        );
        let mut bytes = Frame {
            kind: ECHO_REPLY,
            ..frame
        }
        .encode()
        .unwrap();
        match fault {
            TestFault::Oversize => {
                bytes[7] = 121;
                bytes.resize(129, 0xa5);
                stream.write_all(&bytes).await?;
            }
            TestFault::Truncated => {
                stream.write_all(&bytes[..10]).await?;
                return Ok(());
            }
            TestFault::Silent => tokio::time::sleep(Duration::from_secs(15)).await,
            TestFault::Close => return Ok(()),
            TestFault::BadMagic => {
                bytes[0] ^= 0xff;
                stream.write_all(&bytes).await?;
            }
            TestFault::Split => {
                for byte in bytes {
                    stream.write_all(&[byte]).await?;
                    tokio::time::sleep(Duration::from_millis(15)).await;
                }
            }
            TestFault::None => stream.write_all(&bytes).await?,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::net::{TcpListener, TcpStream};

    fn runtime() -> tokio::runtime::Runtime {
        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap()
    }

    #[test]
    fn fragmented_and_coalesced_stream_frames_keep_boundaries() {
        runtime().block_on(async {
            let (mut reader, mut writer) = tokio::io::duplex(3);
            let first = Frame {
                kind: 1,
                sequence: 100,
                payload: b"one".to_vec(),
            };
            let second = Frame {
                kind: 1,
                sequence: 101,
                payload: b"two".to_vec(),
            };
            let mut bytes = first.encode().unwrap();
            bytes.extend(second.encode().unwrap());
            let task = tokio::spawn(async move {
                for byte in bytes {
                    writer.write_all(&[byte]).await.unwrap();
                }
            });
            assert_eq!(read_frame(&mut reader).await.unwrap(), first);
            assert_eq!(read_frame(&mut reader).await.unwrap(), second);
            task.await.unwrap();
        });
    }

    #[test]
    fn three_real_tcp_clients_exchange_distinct_sequences() {
        runtime().block_on(async {
            let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
            let address = listener.local_addr().unwrap();
            let accept = tokio::spawn(async move {
                let mut handlers = Vec::new();
                for id in 1..=3 {
                    let (stream, _) = listener.accept().await.unwrap();
                    handlers.push(tokio::spawn(serve_connection(stream, id, TestFault::None)));
                }
                for handler in handlers {
                    handler.await.unwrap().unwrap();
                }
            });
            let mut clients = Vec::new();
            for sequence in 1..=3 {
                clients.push(tokio::spawn(async move {
                    let mut stream = TcpStream::connect(address).await.unwrap();
                    let request = Frame {
                        kind: 1,
                        sequence,
                        payload: vec![sequence as u8; 4],
                    };
                    stream.write_all(&request.encode().unwrap()).await.unwrap();
                    assert_eq!(
                        read_frame(&mut stream).await.unwrap(),
                        Frame { kind: 2, ..request }
                    );
                }));
            }
            for client in clients {
                client.await.unwrap();
            }
            accept.await.unwrap();
        });
    }

    #[test]
    fn incomplete_frames_and_unsupported_messages_are_rejected() {
        runtime().block_on(async {
            let frame = Frame {
                kind: ECHO_REQUEST,
                sequence: 1,
                payload: vec![1, 2],
            };
            let bytes = frame.encode().unwrap();
            for length in 1..bytes.len() {
                let (mut reader, mut writer) = tokio::io::duplex(128);
                writer.write_all(&bytes[..length]).await.unwrap();
                drop(writer);
                assert_eq!(
                    read_frame(&mut reader).await.unwrap_err().kind(),
                    io::ErrorKind::InvalidData
                );
            }
            let (server, mut client) = tokio::io::duplex(128);
            let task = tokio::spawn(serve_connection(server, 1, TestFault::None));
            client
                .write_all(&Frame { kind: 255, ..frame }.encode().unwrap())
                .await
                .unwrap();
            assert_eq!(
                task.await.unwrap().unwrap_err().kind(),
                io::ErrorKind::InvalidData
            );
        });
    }

    #[test]
    fn oversized_header_is_rejected_without_waiting_for_body() {
        runtime().block_on(async {
            let (mut reader, mut writer) = tokio::io::duplex(8);
            writer
                .write_all(&[0x50, 0x43, 1, 1, 0, 0, 0xff, 0xff])
                .await
                .unwrap();
            assert_eq!(
                read_frame(&mut reader).await.unwrap_err().kind(),
                io::ErrorKind::InvalidData
            );
        });
    }
}
