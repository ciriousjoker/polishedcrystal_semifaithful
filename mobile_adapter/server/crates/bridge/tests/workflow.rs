use mobile_bridge::{Config, Mode, SharedStatus, Status};
use mobile_protocol::{lobby as wire, Frame};
use std::{
    net::SocketAddr,
    sync::{Arc, Mutex},
    time::Duration,
};
use tokio::{
    io::AsyncWriteExt,
    net::{TcpListener, TcpStream},
    sync::watch,
    task::JoinHandle,
};

struct Service {
    address: SocketAddr,
    stop: watch::Sender<bool>,
    task: JoinHandle<std::io::Result<()>>,
    status: SharedStatus,
}
impl Service {
    async fn bridge(mode: Mode) -> Self {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let (stop, rx) = watch::channel(false);
        let status = Arc::new(Mutex::new(Status::default()));
        let config = Config {
            bind: address,
            lobby: 7,
            mode,
        };
        let task = tokio::spawn(mobile_bridge::serve(listener, config, status.clone(), rx));
        Self {
            address,
            stop,
            task,
            status,
        }
    }
    async fn host(delay: u64) -> Self {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let (stop, rx) = watch::channel(false);
        let task = tokio::spawn(mobile_bridge::hosted::serve(
            listener,
            Duration::from_millis(delay),
            rx,
        ));
        Self {
            address,
            stop,
            task,
            status: Arc::new(Mutex::new(Status::default())),
        }
    }
    async fn finish(self) {
        self.stop.send(true).unwrap();
        self.task.await.unwrap().unwrap();
    }
}

struct Client {
    stream: TcpStream,
    welcome: wire::Welcome,
    roster: Vec<wire::Peer>,
}
impl Client {
    async fn join(address: SocketAddr) -> Self {
        let mut stream = TcpStream::connect(address).await.unwrap();
        let hello = Frame {
            kind: wire::HELLO,
            sequence: 1,
            payload: wire::Hello {
                mechanics: 0,
                lobby: 1,
                resume: 0,
                token: [0; 8],
            }
            .encode(),
        };
        stream.write_all(&hello.encode().unwrap()).await.unwrap();
        // Actual libmobile polls while the delayed authoritative join is pending.
        stream
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
        let frame = receive(&mut stream).await;
        assert_eq!(frame.kind, wire::WELCOME);
        let welcome = wire::Welcome::decode(&frame.payload).unwrap();
        assert_eq!(
            welcome.lobby, 1,
            "Bridge remaps the selected room to the ROM's local room"
        );
        let mut client = Self {
            stream,
            welcome,
            roster: Vec::new(),
        };
        client.poll().await;
        loop {
            let frame = receive(&mut client.stream).await;
            if frame.kind == wire::ROSTER {
                break;
            }
            assert_eq!(frame.kind, wire::RECEIPT);
            tokio::time::sleep(Duration::from_millis(10)).await;
            client.poll().await;
        }
        client.poll().await;
        client
    }
    async fn send(&mut self, frame: Frame) {
        self.stream
            .write_all(&frame.encode().unwrap())
            .await
            .unwrap();
    }
    async fn poll(&mut self) {
        self.send(Frame {
            kind: wire::POLL,
            sequence: 0,
            payload: vec![],
        })
        .await;
    }
    async fn position(&mut self, x: u16) {
        let mut payload = self.welcome.player.to_be_bytes().to_vec();
        payload.extend(self.welcome.generation.to_be_bytes());
        payload.extend(
            wire::Snapshot {
                x,
                epoch: 1,
                ..Default::default()
            }
            .encode(),
        );
        self.send(Frame {
            kind: wire::PRESENCE,
            sequence: x,
            payload,
        })
        .await;
    }
    async fn peers(&mut self, count: u8) -> Frame {
        loop {
            let frame = receive(&mut self.stream).await;
            if frame.kind == wire::RECEIPT {
                tokio::time::sleep(Duration::from_millis(10)).await;
                self.poll().await;
                continue;
            }
            assert!(matches!(frame.kind, wire::ROSTER | wire::DELTA));
            let incoming: Vec<_> = frame.payload[5..]
                .chunks_exact(wire::PEER_LEN)
                .map(|bytes| wire::Peer::decode(bytes).unwrap())
                .collect();
            if frame.kind == wire::ROSTER {
                self.roster = incoming;
            } else {
                for peer in incoming {
                    let previous = self
                        .roster
                        .iter_mut()
                        .find(|old| old.player == peer.player && old.generation == peer.generation)
                        .expect("Delta must have a known binding");
                    *previous = peer;
                }
            }
            self.poll().await;
            if self.roster.len() == usize::from(count) {
                return Frame {
                    kind: wire::ROSTER,
                    sequence: frame.sequence,
                    payload: wire::roster(
                        self.welcome.player,
                        self.welcome.generation,
                        &self.roster,
                    ),
                };
            }
        }
    }
}
async fn receive(stream: &mut TcpStream) -> Frame {
    tokio::time::timeout(Duration::from_secs(3), mobile_server::read_frame(stream))
        .await
        .unwrap()
        .unwrap()
}

async fn exercise(first: &Service, second: &Service) {
    let mut clients = Vec::new();
    for i in 0..wire::CAPACITY {
        clients.push(Client::join(if i < 2 { first.address } else { second.address }).await);
    }
    for (i, client) in clients.iter_mut().enumerate() {
        client.position(100 + i as u16).await;
    }
    for client in &mut clients {
        client.peers(wire::CAPACITY - 1).await;
    }
    let mut ids: Vec<_> = clients.iter().map(|c| c.welcome.player).collect();
    ids.sort();
    ids.dedup();
    assert_eq!(ids.len(), wire::CAPACITY as usize);
    let mut extra = TcpStream::connect(first.address).await.unwrap();
    extra
        .write_all(
            &Frame {
                kind: wire::HELLO,
                sequence: 1,
                payload: wire::Hello {
                    mechanics: 0,
                    lobby: 1,
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
    let full = receive(&mut extra).await;
    assert_eq!(
        (full.kind, full.payload),
        (wire::ERROR, vec![wire::Error::Full as u8])
    );
    drop(extra);
    let mut bad = TcpStream::connect(first.address).await.unwrap();
    bad.write_all(
        &Frame {
            kind: wire::HELLO,
            sequence: 1,
            payload: vec![255],
        }
        .encode()
        .unwrap(),
    )
    .await
    .unwrap();
    assert!(
        tokio::time::timeout(Duration::from_secs(1), mobile_server::read_frame(&mut bad))
            .await
            .unwrap()
            .is_err()
    );
    let victim = clients.remove(1);
    drop(victim);
    for client in &mut clients {
        client.peers(wire::CAPACITY - 2).await;
    }
    clients[0].position(200).await;
    let frame = clients[1].peers(wire::CAPACITY - 2).await;
    let xs: Vec<_> = frame.payload[5..]
        .chunks_exact(wire::PEER_LEN)
        .map(|peer| wire::Snapshot::decode(&peer[6..]).unwrap().x)
        .collect();
    assert!(
        xs.contains(&200),
        "The sibling remains usable after malformed and closed streams"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn embedded_lobby_groups_multiple_emulators_without_an_upstream() {
    let bridge = Service::bridge(Mode::Local).await;
    exercise(&bridge, &bridge).await;
    bridge.finish().await;
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn two_bridges_multiplex_households_through_routing_and_room_owner() {
    let host = Service::host(0).await;
    let origin = format!("ws://{}", host.address);
    let first = Service::bridge(Mode::Internet {
        origin: origin.clone(),
    })
    .await;
    let second = Service::bridge(Mode::Internet { origin }).await;
    exercise(&first, &second).await;
    assert!(first.status.lock().unwrap().connected);
    first.finish().await;
    second.finish().await;
    host.finish().await;
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn simulated_latency_is_propagation_delay_and_server_loss_closes_local_sessions() {
    let host = Service::host(50).await;
    let bridge = Service::bridge(Mode::Internet {
        origin: format!("ws://{}", host.address),
    })
    .await;
    let started = std::time::Instant::now();
    let mut client = Client::join(bridge.address).await;
    assert!(started.elapsed() >= Duration::from_millis(100));
    assert!(started.elapsed() < Duration::from_millis(1000));
    client.poll().await;
    assert!(
        tokio::time::timeout(
            Duration::from_millis(40),
            mobile_server::read_frame(&mut client.stream)
        )
        .await
        .is_err(),
        "An unchanged local poll must not enqueue artificial receipt traffic"
    );
    host.finish().await;
    tokio::time::timeout(Duration::from_secs(2), async {
        while let Ok(frame) = mobile_server::read_frame(&mut client.stream).await {
            // Already queued replies may precede the transport closure.
            assert!(matches!(frame.kind, wire::RECEIPT | wire::ROSTER));
        }
    })
    .await
    .unwrap();
    bridge.finish().await;
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn local_polls_and_presence_do_not_hide_a_silent_room_authority() {
    use futures_util::{SinkExt, StreamExt};
    use mobile_lobby_core::group::{Envelope, Owner};
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let address = listener.local_addr().unwrap();
    let silent = tokio::spawn(async move {
        let (stream, _) = listener.accept().await.unwrap();
        let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
        let message = socket.next().await.unwrap().unwrap();
        let envelope = Envelope::decode(&message.into_data()).unwrap();
        let mut owner = Owner::new(7).unwrap();
        for effect in owner.receive(1, envelope, [1; 8], 0) {
            socket
                .send(tokio_tungstenite::tungstenite::Message::Binary(
                    effect.envelope.encode().unwrap().into(),
                ))
                .await
                .unwrap();
        }
        // TCP and WebSocket remain alive, but the room stops confirming presence.
        while socket.next().await.is_some() {}
    });
    let bridge = Service::bridge(Mode::Internet {
        origin: format!("ws://{address}"),
    })
    .await;
    let client = Client::join(bridge.address).await;
    let identity = client.welcome;
    let (mut reader, mut writer) = client.stream.into_split();
    let producer = tokio::spawn(async move {
        let mut sequence = 10;
        loop {
            let frame = Frame {
                kind: wire::PRESENCE,
                sequence,
                payload: [
                    identity.player.to_be_bytes().to_vec(),
                    identity.generation.to_be_bytes().to_vec(),
                    wire::Snapshot {
                        x: 100,
                        epoch: 1,
                        ..Default::default()
                    }
                    .encode(),
                ]
                .concat(),
            };
            if writer.write_all(&frame.encode().unwrap()).await.is_err() {
                break;
            }
            sequence += 1;
            tokio::time::sleep(Duration::from_millis(50)).await;
        }
    });
    tokio::time::timeout(Duration::from_secs(4), async {
        while let Ok(frame) = mobile_server::read_frame(&mut reader).await {
            assert!(matches!(frame.kind, wire::ROSTER | wire::DELTA));
        }
    })
    .await
    .expect("Local reads must expire without fresh room authority despite an active source");
    producer.abort();
    bridge.finish().await;
    silent.abort();
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn maximum_delay_keeps_four_active_players_and_isolates_a_bad_local_stream() {
    let host = Service::host(1000).await;
    let origin = format!("ws://{}", host.address);
    let first = Service::bridge(Mode::Internet {
        origin: origin.clone(),
    })
    .await;
    let second = Service::bridge(Mode::Internet { origin }).await;
    // Simultaneous joins also cover the delayed welcome/initial-roster queue.
    let (a, b, c, d) = tokio::join!(
        Client::join(first.address),
        Client::join(first.address),
        Client::join(second.address),
        Client::join(second.address)
    );
    let mut tasks = Vec::new();
    for (index, client) in [a, b, c, d].into_iter().enumerate() {
        tasks.push(tokio::spawn(async move {
            let identity = client.welcome;
            let (mut reader, mut writer) = client.stream.into_split();
            let producer = tokio::spawn(async move {
                let mut tick = tokio::time::interval(Duration::from_millis(50));
                tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
                for sequence in 1..=160 {
                    tick.tick().await;
                    let payload = [
                        identity.player.to_be_bytes().to_vec(),
                        identity.generation.to_be_bytes().to_vec(),
                        wire::Snapshot {
                            x: 1000 * index as u16 + sequence.min(99),
                            epoch: 1,
                            ..Default::default()
                        }
                        .encode(),
                    ]
                    .concat();
                    writer
                        .write_all(
                            &Frame {
                                kind: wire::PRESENCE,
                                sequence,
                                payload,
                            }
                            .encode()
                            .unwrap(),
                        )
                        .await
                        .unwrap();
                }
                // Keep the connection alive until the delayed final positions arrive.
                tokio::time::sleep(Duration::from_secs(3)).await;
                writer
            });
            let mut peers = Vec::<wire::Peer>::new();
            tokio::time::timeout(Duration::from_secs(8), async {
                loop {
                    let frame = mobile_server::read_frame(&mut reader).await.unwrap();
                    if frame.kind == wire::RECEIPT {
                        continue;
                    }
                    assert!(matches!(frame.kind, wire::ROSTER | wire::DELTA));
                    let updates: Vec<_> = frame.payload[5..]
                        .chunks_exact(wire::PEER_LEN)
                        .map(|bytes| wire::Peer::decode(bytes).unwrap())
                        .collect();
                    if frame.kind == wire::ROSTER {
                        peers = updates;
                    } else {
                        for peer in updates {
                            let previous = peers
                                .iter_mut()
                                .find(|old| {
                                    old.player == peer.player && old.generation == peer.generation
                                })
                                .unwrap();
                            *previous = peer;
                        }
                    }
                    if peers.len() == 3 && peers.iter().all(|peer| peer.snapshot.x % 1000 >= 80) {
                        break;
                    }
                }
            })
            .await
            .expect("All players must receive fresh positions at the maximum supported delay");
            producer.abort();
        }));
    }
    let mut bad = TcpStream::connect(first.address).await.unwrap();
    bad.write_all(
        &Frame {
            kind: wire::HELLO,
            sequence: 1,
            payload: vec![255],
        }
        .encode()
        .unwrap(),
    )
    .await
    .unwrap();
    assert!(
        tokio::time::timeout(Duration::from_secs(1), mobile_server::read_frame(&mut bad))
            .await
            .unwrap()
            .is_err()
    );
    for task in tasks {
        task.await.unwrap();
    }
    first.finish().await;
    second.finish().await;
    host.finish().await;
}
