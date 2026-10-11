//! Bounded request/reply transport and persistent multi-player lobbies.
pub mod lobby;
use mobile_protocol::lobby as wire;
use mobile_protocol::{payload_len, Frame, ECHO_REPLY, ECHO_REQUEST, HEADER_LEN};
use std::io;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};
use tokio::sync::{mpsc, watch, Semaphore};

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

pub struct Service {
    model: Mutex<lobby::Lobbies>,
    started: Instant,
    test_one_way_delay: Duration,
    changed: watch::Sender<u64>,
}

impl Default for Service {
    fn default() -> Self {
        Self {
            model: Mutex::new(lobby::Lobbies::default()),
            started: Instant::now(),
            test_one_way_delay: Duration::ZERO,
            changed: watch::channel(0).0,
        }
    }
}

impl Service {
    /// Legacy application fault delay. This serializes reads and is not a network
    /// RTT model. Actual latency acceptance uses the probe's transparent TCP hop.
    pub fn with_test_delay(delay: Duration) -> Self {
        Self {
            test_one_way_delay: delay,
            ..Self::default()
        }
    }
    fn now(&self) -> u64 {
        self.started.elapsed().as_millis() as u64
    }

    fn notify(&self) {
        self.changed
            .send_modify(|revision| *revision = revision.wrapping_add(1));
    }

    fn latest_roster(&self, session: u64) -> Option<Frame> {
        let revision = *self.changed.borrow();
        let (player, generation, peers) = self.model.lock().unwrap().roster(session, self.now())?;
        Some(Frame {
            kind: wire::ROSTER,
            sequence: revision as u16,
            payload: wire::roster(player, generation, &peers),
        })
    }

    fn reply(&self, session: u64, frame: &Frame) -> Result<Frame, wire::Error> {
        let mut token = [0; 8];
        if frame.kind == wire::HELLO {
            getrandom::fill(&mut token).map_err(|_| wire::Error::Capacity)?;
        }
        let mut model = self.model.lock().unwrap();
        let (kind, payload) = match frame.kind {
            wire::HELLO => {
                let hello = wire::Hello::decode(&frame.payload)?;
                let welcome = model.join(session, hello, token, self.now())?;
                println!(
                    "LOBBY_JOIN session={session} lobby={} player={} generation={}",
                    welcome.lobby, welcome.player, welcome.generation
                );
                (wire::WELCOME, welcome.encode())
            }
            wire::PRESENCE | wire::LEAVE => {
                let expected = if frame.kind == wire::PRESENCE {
                    wire::PRESENCE_LEN
                } else {
                    4
                };
                if frame.payload.len() != expected {
                    return Err(wire::Error::Malformed);
                }
                let player = u16::from_be_bytes([frame.payload[0], frame.payload[1]]);
                let generation = u16::from_be_bytes([frame.payload[2], frame.payload[3]]);
                if frame.kind == wire::LEAVE {
                    model.leave(session, player, generation)?;
                    self.notify();
                    println!(
                        "LOBBY_LEAVE session={session} player={player} generation={generation}"
                    );
                    (wire::LEFT, frame.payload.clone())
                } else {
                    let snapshot = wire::Snapshot::decode(&frame.payload[4..])?;
                    let changed = model.snapshot(session) != Some(snapshot);
                    let peers = model.presence(
                        session,
                        player,
                        generation,
                        frame.sequence,
                        snapshot,
                        self.now(),
                    )?;
                    if changed {
                        self.notify();
                    }
                    (wire::ROSTER, wire::roster(player, generation, &peers))
                }
            }
            _ => return Err(wire::Error::Malformed),
        };
        Ok(Frame {
            kind,
            sequence: frame.sequence,
            payload,
        })
    }
}

struct SessionGuard {
    service: Arc<Service>,
    session: u64,
}
impl Drop for SessionGuard {
    fn drop(&mut self) {
        if let Some(player) = self
            .service
            .model
            .lock()
            .unwrap()
            .disconnect(self.session, self.service.now())
        {
            println!("LOBBY_OFFLINE session={} player={player}", self.session);
        }
        // Expiry may already have removed this socket's live membership.
        self.service.notify();
    }
}

pub async fn serve_connection<S: AsyncRead + AsyncWrite + Unpin>(
    stream: S,
    session: u64,
    fault: TestFault,
) -> io::Result<()> {
    serve_session(stream, session, fault, Arc::new(Service::default())).await
}

pub async fn serve_session<S: AsyncRead + AsyncWrite + Unpin>(
    mut stream: S,
    session: u64,
    fault: TestFault,
    service: Arc<Service>,
) -> io::Result<()> {
    let _guard = SessionGuard {
        service: service.clone(),
        session,
    };
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
            if !service.test_one_way_delay.is_zero() {
                tokio::time::sleep(service.test_one_way_delay).await;
            }
            let reply = match service.reply(session, &frame) {
                Ok(reply) => reply,
                Err(error) => {
                    println!("LOBBY_REJECTED session={session} reason={error:?}");
                    let bytes = Frame {
                        kind: wire::ERROR,
                        sequence: frame.sequence,
                        payload: vec![error as u8],
                    }
                    .encode()
                    .unwrap();
                    tokio::time::timeout(Duration::from_secs(2), stream.write_all(&bytes))
                        .await
                        .map_err(|_| io::Error::new(io::ErrorKind::TimedOut, "write deadline"))??;
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("{error:?}"),
                    ));
                }
            };
            let bytes = reply.encode().unwrap();
            if !service.test_one_way_delay.is_zero() {
                tokio::time::sleep(service.test_one_way_delay).await;
            }
            tokio::time::timeout(Duration::from_secs(2), stream.write_all(&bytes))
                .await
                .map_err(|_| io::Error::new(io::ErrorKind::TimedOut, "write deadline"))??;
            if frame.kind == wire::HELLO {
                service.notify();
                return serve_lobby(stream, session, service).await;
            }
            if frame.kind == wire::LEAVE {
                return Ok(());
            }
            continue;
        }
        println!(
            "ECHO session={session} sequence={} bytes={}",
            frame.sequence,
            frame.payload.len()
        );
        let sequence = frame.sequence;
        let payload_bytes = frame.payload.len();
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
            TestFault::None => {
                stream.write_all(&bytes).await?;
                println!("ECHO_REPLY session={session} sequence={sequence} bytes={payload_bytes}");
            }
        }
    }
}

// Reader and writer progress independently. A watch notification holds one
// latest version per connection, so slow clients cannot build movement queues.
async fn serve_lobby<S: AsyncRead + AsyncWrite + Unpin>(
    stream: S,
    session: u64,
    service: Arc<Service>,
) -> io::Result<()> {
    let (mut reader, mut writer) = tokio::io::split(stream);
    let (control_tx, mut control_rx) = mpsc::channel::<Frame>(1);
    let mut changed = service.changed.subscribe();
    // One pending delivery credit per adapter DATA request bounds each slow
    // receiver independently of the sender's emulator speed.
    let credit = Semaphore::new(1);
    let grant = || {
        if credit.available_permits() == 0 {
            credit.add_permits(1);
        }
    };
    let read_loop = async {
        let mut last_presence = Instant::now();
        loop {
            let frame = tokio::time::timeout(Duration::from_secs(3), read_frame(&mut reader))
                .await
                .map_err(|_| io::Error::new(io::ErrorKind::TimedOut, "presence deadline"))??;
            if frame.kind == wire::POLL && frame.payload.is_empty() && frame.sequence == 0 {
                if last_presence.elapsed() >= Duration::from_secs(3) {
                    return Err(io::Error::new(
                        io::ErrorKind::TimedOut,
                        "presence lease expired",
                    ));
                }
                grant();
                continue;
            }
            if !service.test_one_way_delay.is_zero() {
                tokio::time::sleep(service.test_one_way_delay).await;
            }
            match service.reply(session, &frame) {
                Ok(reply) => {
                    last_presence = Instant::now();
                    grant();
                    if reply.kind == wire::LEFT {
                        control_tx
                            .send(reply)
                            .await
                            .map_err(|_| io::ErrorKind::BrokenPipe)?;
                        std::future::pending::<()>().await;
                    }
                }
                Err(error) => {
                    println!("LOBBY_REJECTED session={session} reason={error:?}");
                    control_tx
                        .send(Frame {
                            kind: wire::ERROR,
                            sequence: frame.sequence,
                            payload: vec![error as u8],
                        })
                        .await
                        .map_err(|_| io::ErrorKind::BrokenPipe)?;
                    std::future::pending::<()>().await;
                }
            }
        }
        #[allow(unreachable_code)]
        Ok::<(), io::Error>(())
    };
    let write_loop = async {
        let mut last_state = Vec::new();
        loop {
            let (permit, control) = tokio::select! {
                biased;
                frame = control_rx.recv() => (None, Some(frame.ok_or(io::ErrorKind::BrokenPipe)?)),
                permit = credit.acquire() => (Some(permit.map_err(|_| io::ErrorKind::BrokenPipe)?), None)
            };
            if control.is_none() {
                tokio::time::sleep(Duration::from_millis(8)).await;
            }
            let frame = if let Some(frame) = control {
                frame
            } else {
                loop {
                    if let Ok(frame) = control_rx.try_recv() {
                        break frame;
                    }
                    changed.borrow_and_update();
                    let Some(mut frame) = service.latest_roster(session) else {
                        return Err(io::ErrorKind::NotConnected.into());
                    };
                    let mut state = frame.payload.clone();
                    for peer in state[5..].chunks_exact_mut(wire::PEER_LEN) {
                        peer[4] = 0;
                        peer[5] = 0;
                    }
                    if state == last_state {
                        // Retain the credit until a peer changes. Heartbeats never
                        // consume it or create redundant outgoing state.
                        tokio::select! {
                            biased;
                            control = control_rx.recv() => {
                                break control.ok_or(io::ErrorKind::BrokenPipe)?;
                            },
                            result = changed.changed() => { result.map_err(|_| io::ErrorKind::BrokenPipe)?; }
                        }
                        continue;
                    }
                    if state.len() == last_state.len()
                        && state[..5] == last_state[..5]
                        && state[5..]
                            .chunks_exact(wire::PEER_LEN)
                            .zip(last_state[5..].chunks_exact(wire::PEER_LEN))
                            .all(|(new, old)| new[..4] == old[..4])
                    {
                        let changed_peers: Vec<_> = frame.payload[5..]
                            .chunks_exact(wire::PEER_LEN)
                            .zip(state[5..].chunks_exact(wire::PEER_LEN))
                            .zip(last_state[5..].chunks_exact(wire::PEER_LEN))
                            .filter_map(|((wire, new), old)| (new != old).then_some(wire))
                            .collect();
                        let mut payload = frame.payload[..4].to_vec();
                        payload.push(changed_peers.len() as u8);
                        for peer in changed_peers {
                            payload.extend(peer);
                        }
                        frame.kind = wire::DELTA;
                        frame.payload = payload;
                    }
                    last_state = state;
                    break frame;
                }
            };
            if let Some(permit) = permit {
                permit.forget();
            }
            if !service.test_one_way_delay.is_zero() {
                tokio::time::sleep(service.test_one_way_delay).await;
            }
            tokio::time::timeout(
                Duration::from_secs(2),
                writer.write_all(&frame.encode().unwrap()),
            )
            .await
            .map_err(|_| io::Error::new(io::ErrorKind::TimedOut, "write deadline"))??;
            if frame.kind == wire::LEFT {
                return Ok(());
            }
            if frame.kind == wire::ERROR {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "rejected lobby frame",
                ));
            }
        }
    };
    tokio::select! { result = read_loop => result, result = write_loop => result }
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

    async fn until(stream: &mut TcpStream, predicate: impl Fn(&Frame) -> bool) -> Frame {
        tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                let frame = read_frame(stream).await.unwrap();
                if predicate(&frame) {
                    return frame;
                }
            }
        })
        .await
        .expect("missing pushed state")
    }

    #[test]
    fn slow_receiver_gets_latest_position_without_a_fast_sender_backlog() {
        runtime().block_on(async {
            let service = Arc::new(Service::default());
            let (mut client, server) = tokio::io::duplex(16384);
            let task = tokio::spawn(serve_session(server, 1, TestFault::None, service.clone()));
            let hello = Frame {
                kind: wire::HELLO,
                sequence: 1,
                payload: wire::Hello {
                    mechanics: 0,
                    lobby: 7,
                    resume: 0,
                    token: [0; 8],
                }
                .encode(),
            };
            client.write_all(&hello.encode().unwrap()).await.unwrap();
            assert_eq!(read_frame(&mut client).await.unwrap().kind, wire::WELCOME);
            let initial = read_frame(&mut client).await.unwrap();
            assert_eq!(initial.kind, wire::ROSTER);
            assert_eq!(initial.payload[4], 0);
            let welcome =
                wire::Welcome::decode(&service.reply(2, &hello).unwrap().payload).unwrap();
            for end in [1000u16, 2000] {
                for sequence in end - 999..=end {
                    let mut payload = welcome.player.to_be_bytes().to_vec();
                    payload.extend(welcome.generation.to_be_bytes());
                    payload.extend(
                        wire::Snapshot {
                            x: sequence,
                            status: 1,
                            ..wire::Snapshot::default()
                        }
                        .encode(),
                    );
                    service
                        .reply(
                            2,
                            &Frame {
                                kind: wire::PRESENCE,
                                sequence,
                                payload,
                            },
                        )
                        .unwrap();
                }
                // Without another adapter DATA request, no positions may pile
                // up in the receiver's socket, regardless of sender speed.
                assert!(
                    tokio::time::timeout(Duration::from_millis(40), read_frame(&mut client))
                        .await
                        .is_err()
                );
                client
                    .write_all(
                        &Frame {
                            kind: wire::POLL,
                            sequence: 0,
                            payload: vec![],
                        }
                        .encode()
                        .unwrap(),
                    )
                    .await
                    .unwrap();
                let latest = tokio::time::timeout(Duration::from_secs(1), read_frame(&mut client))
                    .await
                    .unwrap()
                    .unwrap();
                assert!(matches!(latest.kind, wire::ROSTER | wire::DELTA));
                assert_eq!(latest.payload[4], 1);
                assert_eq!(
                    wire::Snapshot::decode(&latest.payload[11..]).unwrap().x,
                    end
                );
            }
            task.abort();
        });
    }

    #[test]
    fn shared_tcp_lobby_bounds_pushes_keeps_partial_reads_and_cleans_up_leave() {
        runtime().block_on(async {
            let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
            let address = listener.local_addr().unwrap();
            let service = Arc::new(Service::default());
            let accept = tokio::spawn(async move {
                let mut handlers = Vec::new();
                for id in 1..=3 {
                    let (stream, _) = listener.accept().await.unwrap();
                    handlers.push(tokio::spawn(serve_session(
                        stream,
                        id,
                        TestFault::None,
                        service.clone(),
                    )));
                }
                for handler in handlers {
                    handler.await.unwrap().unwrap();
                }
            });
            let mut clients = Vec::new();
            let mut identities = Vec::new();
            for _ in 0..3 {
                let mut stream = TcpStream::connect(address).await.unwrap();
                stream
                    .write_all(
                        &Frame {
                            kind: wire::HELLO,
                            sequence: 1,
                            payload: wire::Hello {
                                mechanics: 0,
                                lobby: 7,
                                resume: 0,
                                token: [0; 8],
                            }
                            .encode(),
                        }
                        .encode()
                        .unwrap(),
                    )
                    .await
                    .unwrap();
                let reply = until(&mut stream, |f| f.kind == wire::WELCOME).await;
                assert_eq!(reply.sequence, 1);
                identities.push(wire::Welcome::decode(&reply.payload).unwrap());
                clients.push(stream);
            }
            let payload = |index: usize, x| {
                let identity = &identities[index];
                let mut payload = identity.player.to_be_bytes().to_vec();
                payload.extend(identity.generation.to_be_bytes());
                payload.extend(
                    wire::Snapshot {
                        x,
                        status: 1,
                        ..wire::Snapshot::default()
                    }
                    .encode(),
                );
                payload
            };
            for (index, client) in clients.iter_mut().enumerate() {
                client
                    .write_all(
                        &Frame {
                            kind: wire::PRESENCE,
                            sequence: 2,
                            payload: payload(index, index as u16 * 16),
                        }
                        .encode()
                        .unwrap(),
                    )
                    .await
                    .unwrap();
            }
            for client in &mut clients {
                let reply = until(client, |f| f.kind == wire::ROSTER && f.payload[4] == 2).await;
                assert_eq!(reply.payload.len(), 5 + 2 * wire::PEER_LEN);
            }
            let poll = Frame {
                kind: wire::POLL,
                sequence: 0,
                payload: vec![],
            }
            .encode()
            .unwrap();
            for client in &mut clients {
                client.write_all(&poll).await.unwrap();
            }
            // Leave half a header in flight. Incoming pushes must not cancel
            // or restart the reader, otherwise its frame boundary is lost.
            let bytes = Frame {
                kind: wire::PRESENCE,
                sequence: 3,
                payload: payload(0, 77),
            }
            .encode()
            .unwrap();
            clients[0].write_all(&bytes[..3]).await.unwrap();
            clients[1]
                .write_all(
                    &Frame {
                        kind: wire::PRESENCE,
                        sequence: 3,
                        payload: payload(1, 99),
                    }
                    .encode()
                    .unwrap(),
                )
                .await
                .unwrap();
            until(&mut clients[0], |f| {
                matches!(f.kind, wire::ROSTER | wire::DELTA)
                    && f.payload[5..].chunks_exact(wire::PEER_LEN).any(|peer| {
                        u16::from_be_bytes([peer[0], peer[1]]) == identities[1].player
                            && wire::Snapshot::decode(&peer[6..]).unwrap().x == 99
                    })
            })
            .await;
            clients[0].write_all(&bytes[3..]).await.unwrap();
            until(&mut clients[1], |f| {
                matches!(f.kind, wire::ROSTER | wire::DELTA)
                    && f.payload[5..].chunks_exact(wire::PEER_LEN).any(|peer| {
                        u16::from_be_bytes([peer[0], peer[1]]) == identities[0].player
                            && wire::Snapshot::decode(&peer[6..]).unwrap().x == 77
                    })
            })
            .await;
            clients[1]
                .write_all(
                    &Frame {
                        kind: wire::LEAVE,
                        sequence: 4,
                        payload: payload(1, 0)[..4].to_vec(),
                    }
                    .encode()
                    .unwrap(),
                )
                .await
                .unwrap();
            until(&mut clients[1], |f| f.kind == wire::LEFT && f.sequence == 4).await;
            let reply = until(&mut clients[0], |f| {
                f.kind == wire::ROSTER && f.payload[4] == 1
            })
            .await;
            assert_eq!(
                u16::from_be_bytes([reply.payload[5], reply.payload[6]]),
                identities[2].player
            );
            clients[0]
                .write_all(
                    &Frame {
                        kind: wire::LEAVE,
                        sequence: 4,
                        payload: payload(0, 0)[..4].to_vec(),
                    }
                    .encode()
                    .unwrap(),
                )
                .await
                .unwrap();
            until(&mut clients[0], |f| f.kind == wire::LEFT && f.sequence == 4).await;
            clients[2]
                .write_all(
                    &Frame {
                        kind: wire::LEAVE,
                        sequence: 3,
                        payload: payload(2, 0)[..4].to_vec(),
                    }
                    .encode()
                    .unwrap(),
                )
                .await
                .unwrap();
            until(&mut clients[2], |f| f.kind == wire::LEFT && f.sequence == 3).await;
            drop(clients);
            accept.await.unwrap();
        });
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
