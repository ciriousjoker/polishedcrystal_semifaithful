//! Local public routing plus one serialized Rust owner per lobby.
//! This adapter models the Worker/DO boundary without cloud credentials.
use crate::{invalid, network, send_ws, websocket_config};
use futures_util::StreamExt;
use mobile_lobby_core::budget::TrafficBudget;
use mobile_lobby_core::group::{Envelope, Owner, MAX_GROUPS};
use std::{
    collections::{BTreeMap, VecDeque},
    io,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};
use tokio::{
    net::{TcpListener, TcpStream},
    sync::{mpsc, watch},
};
use tokio_tungstenite::{
    accept_hdr_async_with_config,
    tungstenite::{
        handshake::server::{Request, Response},
        Message,
    },
};

pub fn room_path(path: &str) -> Option<u16> {
    let value = path.strip_prefix("/lobbies/")?.strip_suffix("/ws")?;
    let room: u16 = value.parse().ok()?;
    (room != 0 && room.to_string() == value).then_some(room)
}

struct RoomService {
    owner: Owner,
    groups: BTreeMap<u64, mpsc::Sender<Envelope>>,
}
impl RoomService {
    fn deliver(&mut self, effects: Vec<mobile_lobby_core::group::Effect>) {
        let mut failed = Vec::new();
        for effect in effects {
            if let Some(sender) = self.groups.get(&effect.group) {
                if sender.try_send(effect.envelope).is_err() {
                    failed.push(effect.group);
                }
            }
        }
        // A saturated transport only loses its own members. No unbounded retry.
        for group in failed {
            self.groups.remove(&group);
        }
    }
}
type Rooms = Arc<Mutex<BTreeMap<u16, Arc<Mutex<RoomService>>>>>;

async fn connection(
    stream: TcpStream,
    group: u64,
    rooms: Rooms,
    started: Instant,
    one_way_delay: Duration,
) -> io::Result<()> {
    stream.set_nodelay(true)?;
    let selected = Arc::new(Mutex::new(None));
    let selection = selected.clone();
    let callback = move |request: &Request, response: Response| {
        let room = room_path(request.uri().path());
        if request.uri().query().is_some() || room.is_none() {
            return Err(tokio_tungstenite::tungstenite::http::Response::builder()
                .status(404)
                .body(Some("Unknown lobby route".into()))
                .expect("Constant HTTP response"));
        }
        *selection.lock().expect("Selection lock") = room;
        Ok(response)
    };
    let mut socket = tokio::time::timeout(
        Duration::from_secs(3),
        accept_hdr_async_with_config(stream, callback, Some(websocket_config())),
    )
    .await
    .map_err(|_| invalid("Upgrade deadline"))?
    .map_err(network)?;
    let room_id = selected
        .lock()
        .map_err(network)?
        .ok_or_else(|| invalid("No room"))?;
    let room = {
        let mut rooms = rooms.lock().map_err(network)?;
        // Bound active rooms; idle rooms are removed when their last group leaves.
        if !rooms.contains_key(&room_id) && rooms.len() >= 128 {
            return Err(invalid("Room capacity"));
        }
        let room = rooms
            .entry(room_id)
            .or_insert_with(|| {
                Arc::new(Mutex::new(RoomService {
                    owner: Owner::new(room_id).expect("Validated room"),
                    groups: BTreeMap::new(),
                }))
            })
            .clone();
        let (sender, replies) = mpsc::channel(16);
        {
            let mut service = room.lock().map_err(network)?;
            if service.groups.len() >= MAX_GROUPS {
                return Err(invalid("Group capacity"));
            }
            service.groups.insert(group, sender);
        }
        (room, replies)
    };
    let (room, mut replies) = room;
    println!("LOBBY_GROUP_CONNECTED room={room_id} group={group}");
    let result = async {
        let mut budget = TrafficBudget::new(started.elapsed().as_millis() as u64);
        let mut incoming_delay = VecDeque::<(Instant, Envelope)>::new();
        let mut outgoing_delay = VecDeque::<(Instant, Envelope)>::new();
        let mut housekeeping = Instant::now();
        loop {
            // Use the same pending-flush deadline as the Durable Object adapter.
            // A periodic poll adds delay to both propagation directions and
            // needlessly wakes an otherwise idle lobby.
            let mut wake = housekeeping + Duration::from_millis(500);
            for due in [incoming_delay.front().map(|e| e.0), outgoing_delay.front().map(|e| e.0)]
                .into_iter().flatten() {
                wake = wake.min(due);
            }
            if let Some(delay) = room.lock().map_err(network)?.owner
                .flush_after(started.elapsed().as_millis() as u64) {
                wake = wake.min(Instant::now() + Duration::from_millis(delay));
            }
            tokio::select! {
                incoming = socket.next() => {
                    let message = incoming.ok_or_else(|| invalid("Bridge disconnected"))?.map_err(network)?;
                    if !budget.admit(started.elapsed().as_millis() as u64) { return Err(invalid("Message budget exceeded")); }
                    let Message::Binary(bytes) = message else {
                        if matches!(message, Message::Ping(_) | Message::Pong(_)) { continue; }
                        return Err(invalid("Binary messages required"));
                    };
                    let envelope = Envelope::decode(&bytes).map_err(network)?;
                    if incoming_delay.len() >= 128 { return Err(invalid("Delayed input queue capacity")); }
                    incoming_delay.push_back((Instant::now() + one_way_delay, envelope));
                },
                envelope = replies.recv() => {
                    let envelope = envelope.ok_or_else(|| invalid("Group delivery closed"))?;
                    if outgoing_delay.len() >= 256 { return Err(invalid("Delayed output queue capacity")); }
                    outgoing_delay.push_back((Instant::now() + one_way_delay, envelope));
                },
                _ = tokio::time::sleep_until(tokio::time::Instant::from_std(wake)) => {
                    // Due-time queues model propagation delay, rather than
                    // sleeping once per packet and accidentally throttling flow.
                    let due = Instant::now();
                    while incoming_delay.front().is_some_and(|e| e.0 <= due) {
                        let (_, envelope) = incoming_delay.pop_front().ok_or_else(|| invalid("Delay queue"))?;
                        let mut token = [0; 8];
                        getrandom::fill(&mut token).map_err(network)?;
                        let mut service = room.lock().map_err(network)?;
                        let effects = service.owner.receive(group, envelope, token, started.elapsed().as_millis() as u64);
                        service.deliver(effects);
                    }
                    while outgoing_delay.front().is_some_and(|e| e.0 <= due) {
                        let (_, envelope) = outgoing_delay.pop_front().ok_or_else(|| invalid("Delay queue"))?;
                        send_ws(&mut socket, Message::Binary(envelope.encode().map_err(network)?.into())).await?;
                    }
                    {
                        let mut service = room.lock().map_err(network)?;
                        let effects = service.owner.tick(started.elapsed().as_millis() as u64);
                        service.deliver(effects);
                    }
                    if housekeeping.elapsed() >= Duration::from_millis(500) {
                        housekeeping = Instant::now();
                        if budget.idle(started.elapsed().as_millis() as u64) { return Err(invalid("Idle group deadline")); }
                        let mut service = room.lock().map_err(network)?;
                        if !service.groups.contains_key(&group) { return Err(invalid("Group evicted")); }
                        let effects = service.owner.tick(started.elapsed().as_millis() as u64);
                        service.deliver(effects);
                    }
                }
            }
        }
        #[allow(unreachable_code)] Ok::<(), io::Error>(())
    }.await;
    {
        let mut rooms = rooms.lock().map_err(network)?;
        let mut service = room.lock().map_err(network)?;
        service.groups.remove(&group);
        let effects = service
            .owner
            .disconnect_group(group, started.elapsed().as_millis() as u64);
        service.deliver(effects);
        if service.groups.is_empty() {
            rooms.remove(&room_id);
        }
    }
    println!("LOBBY_GROUP_DISCONNECTED room={room_id} group={group} result={result:?}");
    result
}

pub async fn serve(
    listener: TcpListener,
    one_way_delay: Duration,
    mut stop: watch::Receiver<bool>,
) -> io::Result<()> {
    if one_way_delay > Duration::from_secs(1) {
        return Err(invalid("Test delay maximum is 1000ms"));
    }
    let rooms: Rooms = Arc::new(Mutex::new(BTreeMap::new()));
    let permits = Arc::new(tokio::sync::Semaphore::new(256));
    let started = Instant::now();
    let mut group = 0u64;
    let mut tasks = tokio::task::JoinSet::new();
    println!(
        "MOBILE_HOSTED_READY {} test_one_way_delay_ms={}",
        listener.local_addr()?,
        one_way_delay.as_millis()
    );
    loop {
        tokio::select! {
            changed = stop.changed() => { if changed.is_err() || *stop.borrow() { break; } },
            _ = tasks.join_next(), if !tasks.is_empty() => {},
            accepted = listener.accept() => {
                let (stream, _) = accepted?;
                let Ok(permit) = permits.clone().try_acquire_owned() else { continue; };
                group = group.checked_add(1).ok_or_else(|| invalid("Group capacity"))?;
                let id = group;
                let rooms = rooms.clone();
                tasks.spawn(async move {
                    let _permit = permit;
                    let _ = connection(stream, id, rooms, started, one_way_delay).await;
                });
            }
        }
    }
    Ok(())
}
