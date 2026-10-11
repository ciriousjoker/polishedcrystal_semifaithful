//! Native Bridge engine. Flutter and the CLI share this implementation.
use futures_util::{SinkExt, StreamExt};
use mobile_lobby_core::group::{Envelope, Owner, MAX_MESSAGE, MAX_STREAM};
use mobile_protocol::{lobby as wire, Frame};
use serde::Serialize;
use std::{
    collections::BTreeMap,
    io,
    net::SocketAddr,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};
use tokio::{
    io::AsyncWriteExt,
    net::{TcpListener, TcpStream},
    sync::{mpsc, watch},
};
use tokio_tungstenite::{
    connect_async_with_config,
    tungstenite::{protocol::WebSocketConfig, Message},
    MaybeTlsStream, WebSocketStream,
};

pub mod ffi;
pub mod hosted;

fn invalid(message: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message)
}
fn network(error: impl std::fmt::Debug) -> io::Error {
    io::Error::other(format!("{error:?}"))
}

#[derive(Clone)]
pub enum Mode {
    Local,
    Internet { origin: String },
}
#[derive(Clone)]
pub struct Config {
    pub bind: SocketAddr,
    pub lobby: u16,
    pub mode: Mode,
}

#[derive(Clone, Default, Serialize)]
pub struct Status {
    pub endpoint: String,
    pub mode: String,
    pub connected: bool,
    pub players: Vec<PlayerStatus>,
    pub error: Option<String>,
}
#[derive(Clone, Serialize)]
pub struct PlayerStatus {
    pub stream: u16,
    pub player: Option<u16>,
    pub joined: bool,
}
pub type SharedStatus = Arc<Mutex<Status>>;

/// Plaintext is limited to explicit loopback development origins.
pub fn validate_origin(origin: &str) -> io::Result<()> {
    let url = tokio_tungstenite::tungstenite::http::Uri::try_from(origin).map_err(network)?;
    let scheme = url.scheme_str().ok_or_else(|| invalid("Missing scheme"))?;
    let host = url.host().ok_or_else(|| invalid("Missing host"))?;
    if origin.len() > 2048
        || origin.contains(['?', '#', '@'])
        || url.path() != "/"
        || url.port_u16() == Some(0)
        || !(scheme == "wss" || (scheme == "ws" && matches!(host, "127.0.0.1" | "localhost")))
    {
        return Err(invalid("Use a WSS origin or explicit loopback WS origin"));
    }
    Ok(())
}

pub fn websocket_config() -> WebSocketConfig {
    let mut config = WebSocketConfig::default();
    config.max_message_size = Some(MAX_MESSAGE);
    config.max_frame_size = Some(MAX_MESSAGE);
    config.write_buffer_size = 0;
    config.max_write_buffer_size = MAX_MESSAGE * 8;
    config
}

pub(crate) async fn send_ws<S>(socket: &mut WebSocketStream<S>, message: Message) -> io::Result<()>
where
    S: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
{
    tokio::time::timeout(Duration::from_secs(2), socket.send(message))
        .await
        .map_err(|_| io::Error::new(io::ErrorKind::TimedOut, "WebSocket write deadline"))?
        .map_err(network)
}

type RemoteSocket = WebSocketStream<MaybeTlsStream<TcpStream>>;
enum Backend {
    Local(Owner),
    Remote {
        origin: String,
        socket: Option<RemoteSocket>,
    },
}
impl Backend {
    async fn input(&mut self, envelope: Envelope, now: u64) -> io::Result<Vec<Envelope>> {
        match self {
            Self::Local(owner) => {
                let mut token = [0; 8];
                getrandom::fill(&mut token).map_err(network)?;
                let mut effects = owner.receive(1, envelope, token, now);
                effects.extend(owner.tick_local(now));
                Ok(effects.into_iter().map(|e| e.envelope).collect())
            }
            Self::Remote { origin, socket } => {
                if envelope.frame.is_none() && socket.is_none() {
                    return Ok(Vec::new());
                }
                if socket.is_none() {
                    let (ws, _) = tokio::time::timeout(
                        Duration::from_secs(3),
                        connect_async_with_config(origin.as_str(), Some(websocket_config()), true),
                    )
                    .await
                    .map_err(|_| invalid("Upstream connection deadline"))?
                    .map_err(network)?;
                    *socket = Some(ws);
                    println!("BRIDGE_UPSTREAM connected=1");
                }
                send_ws(
                    socket.as_mut().ok_or_else(|| invalid("No upstream"))?,
                    Message::Binary(envelope.encode().map_err(network)?.into()),
                )
                .await?;
                Ok(Vec::new())
            }
        }
    }
    async fn next(&mut self) -> io::Result<Envelope> {
        match self {
            Self::Remote {
                socket: Some(socket),
                ..
            } => loop {
                match socket
                    .next()
                    .await
                    .ok_or_else(|| invalid("Upstream disconnected"))?
                    .map_err(network)?
                {
                    Message::Binary(bytes) => return Envelope::decode(&bytes).map_err(network),
                    Message::Ping(_) | Message::Pong(_) => {}
                    _ => return Err(invalid("Upstream closed or sent nonbinary data")),
                }
            },
            _ => std::future::pending().await,
        }
    }
    fn reset(&mut self) {
        if let Self::Remote { socket, .. } = self {
            *socket = None;
        }
    }
}

struct Client {
    writer: Option<mpsc::Sender<Frame>>,
    local_lobby: u16,
    presence: Option<Frame>,
    next_presence_at: u64,
    joined: bool,
    player: Option<u16>,
    generation: u16,
    authority_at: u64,
    roster: Option<Frame>,
    local_credit: bool,
    delivered: Vec<u8>,
}
impl Client {
    fn identity(&self) -> Vec<u8> {
        let mut bytes = self.player.unwrap_or(0).to_be_bytes().to_vec();
        bytes.extend(self.generation.to_be_bytes());
        bytes
    }
    fn publish_cached(&mut self) {
        self.local_credit = true;
        if self.roster.is_none() {
            return;
        }
        self.local_credit = false;
        if let Some(writer) = &self.writer {
            let reply = match self.roster.take() {
                Some(mut frame) => {
                    let current = frame.payload.clone();
                    let old = &self.delivered;
                    if current.len() == old.len()
                        && current[..5] == old[..5]
                        && current[5..]
                            .chunks_exact(wire::PEER_LEN)
                            .zip(old[5..].chunks_exact(wire::PEER_LEN))
                            .all(|(next, prev)| next[..4] == prev[..4])
                    {
                        let changed: Vec<_> = current[5..]
                            .chunks_exact(wire::PEER_LEN)
                            .zip(old[5..].chunks_exact(wire::PEER_LEN))
                            .filter(|(next, prev)| next[6..] != prev[6..])
                            .map(|(next, _)| next)
                            .collect();
                        if !changed.is_empty() && changed.len() < usize::from(current[4]) {
                            let mut delta = current[..4].to_vec();
                            delta.push(changed.len() as u8);
                            for peer in changed {
                                delta.extend(peer);
                            }
                            frame.kind = wire::DELTA;
                            frame.payload = delta;
                        }
                    }
                    self.delivered = current;
                    frame
                }
                None => return,
            };
            if writer.try_send(reply).is_err() {
                self.writer = None;
            }
        }
    }
}
enum Command {
    Attach(u16, mpsc::Sender<Frame>),
    Input(u16, Frame),
    Close(u16),
}

async fn tcp_client(stream: TcpStream, id: u16, tx: mpsc::Sender<Command>) -> io::Result<()> {
    stream.set_nodelay(true)?;
    let (mut reader, mut writer) = stream.into_split();
    let (out, mut rx) = mpsc::channel::<Frame>(8);
    tx.send(Command::Attach(id, out)).await.map_err(network)?;
    let read = async {
        let mut budget = 512u16;
        let mut budget_at = Instant::now();
        loop {
            let frame = tokio::time::timeout(
                Duration::from_secs(3),
                mobile_server::read_frame(&mut reader),
            )
            .await
            .map_err(|_| invalid("Local read deadline"))??;
            if budget_at.elapsed() >= Duration::from_secs(1) {
                budget = 512;
                budget_at = Instant::now();
            }
            budget = budget
                .checked_sub(1)
                .ok_or_else(|| invalid("Local message budget exceeded"))?;
            tx.send(Command::Input(id, frame)).await.map_err(network)?;
        }
        #[allow(unreachable_code)]
        Ok::<(), io::Error>(())
    };
    let write = async {
        while let Some(frame) = rx.recv().await {
            let bytes = frame.encode().map_err(network)?;
            tokio::time::timeout(Duration::from_secs(2), writer.write_all(&bytes))
                .await
                .map_err(|_| invalid("Local write deadline"))??;
            if matches!(frame.kind, wire::LEFT | wire::ERROR) {
                return Ok(());
            }
        }
        Ok::<(), io::Error>(())
    };
    let result = tokio::select! { result = read => result, result = write => result };
    let _ = tx.send(Command::Close(id)).await;
    println!("BRIDGE_DISCONNECTED stream={id} result={result:?}");
    result
}

fn dispatch(
    envelopes: Vec<Envelope>,
    clients: &mut BTreeMap<u16, Client>,
    remote_lobby: u16,
    now: u64,
) -> io::Result<()> {
    for envelope in envelopes {
        let Some(mut frame) = envelope.frame else {
            if let Some(c) = clients.get_mut(&envelope.stream) {
                c.writer = None;
            }
            continue;
        };
        let Some(client) = clients.get_mut(&envelope.stream) else {
            continue;
        };
        match frame.kind {
            wire::WELCOME if !client.joined => {
                let mut welcome = wire::Welcome::decode(&frame.payload).map_err(network)?;
                if welcome.lobby != remote_lobby {
                    return Err(invalid("Wrong upstream lobby"));
                }
                welcome.lobby = client.local_lobby;
                frame.payload = welcome.encode();
                client.joined = true;
                client.player = Some(welcome.player);
                client.generation = welcome.generation;
                client.authority_at = now;
                println!(
                    "BRIDGE_JOINED stream={} player={} generation={}",
                    envelope.stream, welcome.player, welcome.generation
                );
            }
            wire::ROSTER if client.joined => {
                if frame.payload.len() < 5
                    || frame.payload[..4] != client.identity()
                    || frame.payload[4] >= wire::CAPACITY
                    || frame.payload.len() != 5 + usize::from(frame.payload[4]) * wire::PEER_LEN
                {
                    return Err(invalid("Invalid roster identity or size"));
                }
                let mut ids = std::collections::BTreeSet::new();
                ids.insert(client.player.unwrap());
                for bytes in frame.payload[5..].chunks_exact(wire::PEER_LEN) {
                    let peer = wire::Peer::decode(bytes).map_err(network)?;
                    if !ids.insert(peer.player) {
                        return Err(invalid("Duplicate roster identity"));
                    }
                }
                client.authority_at = now;
                client.roster = Some(frame);
                if client.local_credit {
                    client.publish_cached();
                }
                continue;
            }
            wire::RECEIPT if client.joined && frame.payload == client.identity() => {
                client.authority_at = now;
                // The ROM already received a bounded local transfer receipt.
                continue;
            }
            wire::LEFT if client.joined && frame.payload == client.identity() => {}
            wire::ERROR if frame.payload.len() == 1 => {
                println!(
                    "BRIDGE_PLAYER_ERROR stream={} code={}",
                    envelope.stream, frame.payload[0]
                );
            }
            _ => return Err(invalid("Invalid upstream effect")),
        }
        if let Some(writer) = &client.writer {
            if writer.try_send(frame).is_err() {
                client.writer = None;
            }
        }
    }
    Ok(())
}

pub async fn serve(
    listener: TcpListener,
    config: Config,
    status: SharedStatus,
    mut stop: watch::Receiver<bool>,
) -> io::Result<()> {
    // The app binds loopback. An explicit CLI bind also permits a household
    // LAN listener for the existing Mac/Android development command.
    if config.bind.ip().is_multicast() || config.lobby == 0 {
        return Err(invalid("Invalid Bridge config"));
    }
    let mut backend = match &config.mode {
        Mode::Local => Backend::Local(Owner::new(config.lobby).map_err(network)?),
        Mode::Internet { origin } => {
            validate_origin(origin)?;
            Backend::Remote {
                origin: format!("{origin}/lobbies/{}/ws", config.lobby),
                socket: None,
            }
        }
    };
    let (tx, mut rx) = mpsc::channel(32);
    let mut clients = BTreeMap::<u16, Client>::new();
    let started = Instant::now();
    let mut tick = tokio::time::interval(Duration::from_millis(5));
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
    println!(
        "BRIDGE_READY {} mode={}",
        listener.local_addr()?,
        if matches!(config.mode, Mode::Local) {
            "local"
        } else {
            "internet"
        }
    );
    loop {
        let now = started.elapsed().as_millis() as u64;
        let result: io::Result<Vec<Envelope>> = tokio::select! {
            changed = stop.changed() => { if changed.is_err() || *stop.borrow() { break; } Ok(Vec::new()) },
            accepted = listener.accept() => {
                let (stream, _) = accepted?;
                // Stream IDs are not reused until Close is processed. Reserve now,
                // before spawning, so concurrent accepts cannot select one ID.
                if let Some(id) = (1..=MAX_STREAM).find(|id| !clients.contains_key(id)) {
                    println!("BRIDGE_EMULATOR_CONNECTED stream={id} peer={}", stream.peer_addr()?);
                    let (placeholder, _) = mpsc::channel(1);
                    clients.insert(id, Client { writer: Some(placeholder), local_lobby: 0,
                        presence: None, next_presence_at: 0, joined: false, player: None,
                        generation: 0, authority_at: now, roster: None, local_credit: false, delivered: Vec::new() });
                    let tx = tx.clone();
                    tokio::spawn(async move { let _ = tcp_client(stream, id, tx).await; });
                } else { println!("BRIDGE_REJECTED reason=transport_capacity"); }
                Ok(Vec::new())
            },
            command = rx.recv() => {
                match command.ok_or_else(|| invalid("Bridge command channel ended"))? {
                    Command::Attach(id, writer) => { if let Some(c) = clients.get_mut(&id) { c.writer = Some(writer); } Ok(Vec::new()) },
                    Command::Close(id) => {
                        clients.remove(&id);
                        backend.input(Envelope { stream: id, frame: None }, now).await
                    },
                    Command::Input(id, mut frame) => {
                        if let Some(c) = clients.get_mut(&id).filter(|c| c.writer.is_some()) {
                            match frame.kind {
                                wire::HELLO if c.local_lobby == 0 => {
                                    match wire::Hello::decode(&frame.payload) {
                                        Ok(mut hello) => {
                                            c.local_lobby = hello.lobby;
                                            hello.lobby = config.lobby;
                                            frame.payload = hello.encode();
                                            backend.input(Envelope { stream: id, frame: Some(frame) }, now).await
                                        },
                                        Err(_) => { c.writer = None; Ok(Vec::new()) }
                                    }
                                },
                                wire::PRESENCE if c.joined => {
                                    if frame.payload.len() != wire::PRESENCE_LEN
                                        || frame.payload[..4] != c.identity()
                                        || wire::Snapshot::decode(&frame.payload[4..]).is_err() {
                                        c.writer = None;
                                        backend.input(Envelope { stream: id, frame: None }, now).await
                                    } else {
                                        c.publish_cached();
                                        c.presence = Some(frame);
                                        Ok(Vec::new())
                                    }
                                },
                                wire::POLL if c.joined && frame.sequence == 0 && frame.payload.is_empty() => {
                                    c.publish_cached();
                                    Ok(Vec::new())
                                },
                                wire::POLL if c.local_lobby != 0 && !c.joined
                                    && frame.sequence == 0 && frame.payload.is_empty() => {
                                    // libmobile may issue its receive poll before the
                                    // delayed upstream WELCOME reaches the Bridge.
                                    // Keep waiting for authority; never fabricate it.
                                    Ok(Vec::new())
                                },
                                wire::LEAVE if c.joined => {
                                    if frame.kind == wire::LEAVE { c.presence = None; }
                                    backend.input(Envelope { stream: id, frame: Some(frame) }, now).await
                                },
                                _ => { c.writer = None; backend.input(Envelope { stream: id, frame: None }, now).await }
                            }
                        } else { Ok(Vec::new()) }
                    }
                }
            },
            incoming = backend.next() => incoming.map(|envelope| vec![envelope]),
            _ = tick.tick() => {
                // Cloud-bound presence is coalesced to 20Hz. An embedded owner
                // needs no second network throttle ahead of its own bounded
                // dirty-roster cadence. Keep the same latest-only queue.
                let presence_interval = if matches!(backend, Backend::Local(_)) { 0 } else { 50 };
                let mut pending = Vec::new();
                for (&id, client) in &mut clients {
                    if client.writer.is_none() { continue; }
                    if client.joined && now.saturating_sub(client.authority_at) >= 3000 {
                        client.writer = None;
                        pending.push(Envelope { stream: id, frame: None });
                        continue;
                    }
                    if now >= client.next_presence_at {
                        if let Some(frame) = client.presence.take() {
                            client.next_presence_at = now.saturating_add(presence_interval);
                            pending.push(Envelope { stream: id, frame: Some(frame) });
                        }
                    }
                }
                let mut effects = Vec::new();
                let mut failure = None;
                for envelope in pending {
                    match backend.input(envelope, now).await {
                        Ok(new) => effects.extend(new), Err(e) => { failure = Some(e); break; }
                    }
                }
                if let Backend::Local(owner) = &mut backend {
                    effects.extend(owner.tick_local(now).into_iter().map(|e| e.envelope));
                }
                failure.map_or(Ok(effects), Err)
            }
        };
        if let Err(error) = result.and_then(|effects| {
            dispatch(
                effects,
                &mut clients,
                config.lobby,
                started.elapsed().as_millis() as u64,
            )
        }) {
            println!("BRIDGE_UPSTREAM_LOST error={error}");
            for c in clients.values_mut() {
                c.writer = None;
                c.presence = None;
            }
            backend.reset();
            status.lock().map_err(network)?.error = Some(error.to_string());
        }
        let mut view = status.lock().map_err(network)?;
        view.endpoint = listener.local_addr()?.to_string();
        view.mode = if matches!(config.mode, Mode::Local) {
            "local"
        } else {
            "internet"
        }
        .into();
        view.connected = matches!(
            backend,
            Backend::Local(_)
                | Backend::Remote {
                    socket: Some(_),
                    ..
                }
        );
        view.players = clients
            .iter()
            .filter(|(_, c)| c.writer.is_some())
            .map(|(&stream, c)| PlayerStatus {
                stream,
                player: c.player,
                joined: c.joined,
            })
            .collect();
        if view.connected && view.players.iter().any(|p| p.joined) {
            view.error = None;
        }
    }
    clients.clear();
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn local_delivery_coalesces_positions_and_deltas_preserve_membership() {
        let (tx, mut rx) = mpsc::channel(8);
        let mut client = Client {
            writer: Some(tx),
            local_lobby: 1,
            presence: None,
            next_presence_at: 0,
            joined: true,
            player: Some(1),
            generation: 1,
            authority_at: 0,
            roster: None,
            local_credit: false,
            delivered: Vec::new(),
        };
        let mut peers: Vec<_> = (2..=4)
            .map(|player| wire::Peer {
                player,
                generation: 1,
                revision: 1,
                snapshot: wire::Snapshot::default(),
            })
            .collect();
        let roster = |peers: &[wire::Peer]| Frame {
            kind: wire::ROSTER,
            sequence: 0,
            payload: wire::roster(1, 1, peers),
        };
        client.roster = Some(roster(&peers));
        client.publish_cached();
        assert_eq!(rx.try_recv().unwrap().kind, wire::ROSTER);
        for x in 1..=100 {
            peers[0].snapshot.x = x;
            client.roster = Some(roster(&peers));
        }
        assert!(
            rx.try_recv().is_err(),
            "Positions wait for the local read credit"
        );
        client.publish_cached();
        let delta = rx.try_recv().unwrap();
        assert_eq!(delta.kind, wire::DELTA);
        assert_eq!(delta.payload.len(), 5 + wire::PEER_LEN);
        assert_eq!(
            wire::Peer::decode(&delta.payload[5..]).unwrap().snapshot.x,
            100
        );
        peers.pop();
        client.roster = Some(roster(&peers));
        client.publish_cached();
        let full = rx.try_recv().unwrap();
        assert_eq!((full.kind, full.payload[4]), (wire::ROSTER, 2));
        client.publish_cached();
        assert!(
            rx.try_recv().is_err(),
            "Empty reads must not enqueue identity receipts"
        );
        assert!(client.local_credit);
        let mut clients = BTreeMap::from([(1, client)]);
        dispatch(
            vec![Envelope {
                stream: 1,
                frame: Some(roster(&peers)),
            }],
            &mut clients,
            1,
            100,
        )
        .unwrap();
        assert_eq!(rx.try_recv().unwrap().kind, wire::ROSTER);
        assert!(!clients[&1].local_credit);
        dispatch(
            vec![Envelope {
                stream: 1,
                frame: Some(roster(&peers)),
            }],
            &mut clients,
            1,
            101,
        )
        .unwrap();
        assert!(
            rx.try_recv().is_err(),
            "Upstream pushes cannot bypass local read admission"
        );
    }
}
