//! Exercise the actual CLI so pipe buffering and session lifecycle stay visible.
use mobile_protocol::{Frame, ECHO_REPLY, ECHO_REQUEST};
use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpStream;
use std::process::{Child, Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

struct Server(Child);

impl Drop for Server {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

#[test]
fn cli_logs_connections_replies_and_clean_or_invalid_disconnects() {
    let mut server = Server(
        Command::new(env!("CARGO_BIN_EXE_mobile-server"))
            .args(["--bind", "127.0.0.1:0"])
            .stdout(Stdio::piped())
            .spawn()
            .unwrap(),
    );
    let stdout = server.0.stdout.take().unwrap();
    let (sender, receiver) = mpsc::channel();
    std::thread::spawn(move || {
        for line in BufReader::new(stdout).lines() {
            if sender.send(line.unwrap()).is_err() {
                break;
            }
        }
    });
    let next = || receiver.recv_timeout(Duration::from_secs(5)).unwrap();
    let ready = next();
    let address = ready.strip_prefix("READY ").unwrap();
    let mut client = TcpStream::connect(address).unwrap();
    client
        .set_read_timeout(Some(Duration::from_secs(5)))
        .unwrap();
    let peer = client.local_addr().unwrap();
    assert_eq!(next(), format!("CLIENT_CONNECTED session=1 peer={peer}"));
    let request = Frame {
        kind: ECHO_REQUEST,
        sequence: 42,
        payload: b"test".to_vec(),
    };
    client.write_all(&request.encode().unwrap()).unwrap();
    let mut reply = vec![0; 12];
    client.read_exact(&mut reply).unwrap();
    assert_eq!(
        Frame::decode(&reply).unwrap(),
        Frame {
            kind: ECHO_REPLY,
            ..request
        }
    );
    assert_eq!(next(), "ECHO session=1 sequence=42 bytes=4");
    assert_eq!(next(), "ECHO_REPLY session=1 sequence=42 bytes=4");
    drop(client);
    assert_eq!(
        next(),
        format!("CLIENT_DISCONNECTED session=1 peer={peer} reason=closed")
    );

    let mut invalid = TcpStream::connect(address).unwrap();
    let peer = invalid.local_addr().unwrap();
    assert_eq!(next(), format!("CLIENT_CONNECTED session=2 peer={peer}"));
    invalid.write_all(b"invalid!").unwrap();
    assert_eq!(
        next(),
        format!("CLIENT_DISCONNECTED session=2 peer={peer} reason=error")
    );
}
