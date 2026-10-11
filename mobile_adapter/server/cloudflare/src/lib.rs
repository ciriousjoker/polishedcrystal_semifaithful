//! One public routing handler and one hibernatable owner per lobby.
use mobile_lobby_core::budget::TrafficBudget;
use mobile_lobby_core::group::{Effect, Envelope, Owner, MAX_GROUPS};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::BTreeMap;
use worker::*;

fn room_path(path: &str) -> Option<u16> {
    let value = path.strip_prefix("/lobbies/")?.strip_suffix("/ws")?;
    let room: u16 = value.parse().ok()?;
    (room != 0 && room.to_string() == value).then_some(room)
}

#[event(fetch)]
pub async fn route(req: Request, env: Env, _ctx: Context) -> Result<Response> {
    if req.method() == Method::Get && req.path() == "/health" {
        return Response::ok("mobile-cloudflare local foundation");
    }
    let Some(room) = room_path(&req.path()) else {
        return Response::error("Unknown route", 404);
    };
    if req.method() != Method::Get {
        return Response::error("Method not allowed", 405);
    }
    if !req
        .headers()
        .get("Upgrade")?
        .is_some_and(|v| v.eq_ignore_ascii_case("websocket"))
    {
        return Response::error("WebSocket upgrade required", 426);
    }
    // Pass through the accepted upgrade. No per-message Worker relay.
    env.durable_object("LOBBIES")?
        .get_by_name(&format!("lobby:{room}"))?
        .fetch_with_request(req)
        .await
}

#[derive(Serialize, Deserialize)]
struct Attachment {
    version: u8,
    group: u64,
    next_group: u64,
    owner: Owner,
    budget: TrafficBudget,
}
#[derive(Default)]
struct Runtime {
    owner: Option<Owner>,
    next_group: u64,
    budgets: BTreeMap<u64, TrafficBudget>,
}

#[durable_object]
pub struct LobbyObject {
    state: State,
    runtime: RefCell<Runtime>,
}
impl LobbyObject {
    fn group(&self, ws: &WebSocket) -> Option<u64> {
        self.state.get_tags(ws).first()?.parse().ok()
    }
    fn persist(&self) -> Result<()> {
        let runtime = self.runtime.borrow();
        let Some(owner) = &runtime.owner else {
            return Ok(());
        };
        for ws in self.state.get_websockets() {
            let Some(group) = self.group(&ws) else {
                continue;
            };
            let Some(budget) = runtime.budgets.get(&group) else {
                continue;
            };
            ws.serialize_attachment(Attachment {
                version: 4,
                group,
                next_group: runtime.next_group,
                owner: owner.checkpoint(),
                budget: budget.clone(),
            })?;
        }
        Ok(())
    }
    fn deliver(&self, effects: Vec<Effect>) -> Result<()> {
        let sockets = self.state.get_websockets();
        for effect in effects {
            if let Some(ws) = sockets
                .iter()
                .find(|ws| self.group(ws) == Some(effect.group))
            {
                if let Ok(bytes) = effect.envelope.encode() {
                    if ws.send_with_bytes(bytes).is_err() {
                        let _ = ws.close(Some(1011), Some("Transport failure"));
                    }
                }
            }
        }
        self.persist()
    }
    async fn schedule_flush(&self) -> Result<()> {
        let now = Date::now().as_millis();
        let delay = {
            let mut runtime = self.runtime.borrow_mut();
            if runtime.budgets.is_empty() {
                return Ok(());
            }
            runtime
                .owner
                .as_mut()
                .and_then(|o| o.flush_after(now))
                .unwrap_or(1000)
                .max(1)
        };
        let storage = self.state.storage();
        let existing = storage.get_alarm().await?;
        // A later message must not postpone an already scheduled dirty-state flush.
        if existing.is_none_or(|at| at > now.saturating_add(delay) as i64) {
            storage.set_alarm(delay as i64).await?;
        }
        Ok(())
    }
    fn remove(&self, ws: &WebSocket) -> Result<()> {
        let effects = if let Some(group) = self.group(ws) {
            {
                let mut runtime = self.runtime.borrow_mut();
                runtime.budgets.remove(&group);
                runtime
                    .owner
                    .as_mut()
                    .map(|owner| owner.disconnect_group(group, Date::now().as_millis()))
                    .unwrap_or_default()
            }
        } else {
            Vec::new()
        };
        self.deliver(effects)
    }
    fn reject(&self, ws: &WebSocket) -> Result<()> {
        let _ = ws.close(Some(1008), Some("Invalid Bridge envelope"));
        self.remove(ws)
    }
}
impl DurableObject for LobbyObject {
    fn new(state: State, _env: Env) -> Self {
        let mut runtime = Runtime::default();
        let sockets = state.get_websockets();
        let mut valid = sockets.len() <= MAX_GROUPS;
        let mut groups = std::collections::BTreeSet::new();
        for ws in &sockets {
            match ws.deserialize_attachment::<Attachment>() {
                Ok(Some(a))
                    if a.version == 4
                        && a.budget.validate_checkpoint()
                        && a.group != 0
                        && a.next_group >= a.group
                        && a.owner.validate_checkpoint()
                        && groups.insert(a.group)
                        && state.get_tags(ws) == [a.group.to_string()]
                        && runtime
                            .owner
                            .as_ref()
                            .is_none_or(|o| o.room_id() == a.owner.room_id()) =>
                {
                    runtime.budgets.insert(a.group, a.budget);
                    runtime.owner = Some(a.owner);
                    runtime.next_group = runtime.next_group.max(a.next_group);
                }
                _ => valid = false,
            }
        }
        if !valid {
            for ws in sockets {
                let _ = ws.close(Some(1012), Some("Rejoin required"));
            }
            runtime = Runtime::default();
        }
        Self {
            state,
            runtime: RefCell::new(runtime),
        }
    }
    async fn fetch(&self, req: Request) -> Result<Response> {
        let Some(room) = room_path(&req.path()) else {
            return Response::error("Unknown route", 404);
        };
        if self.state.get_websockets().len() >= MAX_GROUPS {
            return Response::error("Group capacity", 429);
        }
        let pair = WebSocketPair::new()?;
        let mut runtime = self.runtime.borrow_mut();
        if runtime.owner.as_ref().is_some_and(|o| o.room_id() != room) {
            return Response::error("Routing mismatch", 400);
        }
        if runtime.owner.is_none() {
            runtime.owner = Some(
                Owner::new(room).map_err(|_| worker::Error::RustError("Invalid room".into()))?,
            );
        }
        let Some(group) = runtime.next_group.checked_add(1) else {
            return Response::error("Group capacity", 503);
        };
        runtime.next_group = group;
        runtime
            .budgets
            .insert(group, TrafficBudget::new(Date::now().as_millis()));
        drop(runtime);
        self.state
            .accept_websocket_with_tags(&pair.server, &[&group.to_string()]);
        self.persist()?;
        self.schedule_flush().await?;
        console_log!("LOBBY_GROUP_CONNECTED room={room} group={group}");
        Response::from_websocket(pair.client)
    }
    async fn websocket_message(
        &self,
        ws: WebSocket,
        message: WebSocketIncomingMessage,
    ) -> Result<()> {
        let Some(group) = self.group(&ws) else {
            return self.reject(&ws);
        };
        let admitted = self
            .runtime
            .borrow_mut()
            .budgets
            .get_mut(&group)
            .is_some_and(|b| b.admit(Date::now().as_millis()));
        if !admitted {
            return self.reject(&ws);
        }
        let WebSocketIncomingMessage::Binary(bytes) = message else {
            return self.reject(&ws);
        };
        let Ok(envelope) = Envelope::decode(&bytes) else {
            return self.reject(&ws);
        };
        let mut token = [0; 8];
        if envelope
            .frame
            .as_ref()
            .is_some_and(|f| f.kind == mobile_protocol::lobby::HELLO)
        {
            let crypto = worker::js_sys::Reflect::get(&worker::js_sys::global(), &"crypto".into())?;
            let function = worker::js_sys::Reflect::get(&crypto, &"getRandomValues".into())?;
            use worker::wasm_bindgen::JsCast;
            let bytes = worker::js_sys::Uint8Array::new_with_length(8);
            function
                .dyn_into::<worker::js_sys::Function>()?
                .call1(&crypto, &bytes)?;
            bytes.copy_to(&mut token);
        }
        let effects = self
            .runtime
            .borrow_mut()
            .owner
            .as_mut()
            .map(|o| o.receive(group, envelope, token, Date::now().as_millis()))
            .unwrap_or_default();
        self.deliver(effects)?;
        self.schedule_flush().await
    }
    async fn alarm(&self) -> Result<Response> {
        let now = Date::now().as_millis();
        for ws in self.state.get_websockets() {
            let idle = self
                .group(&ws)
                .and_then(|g| self.runtime.borrow().budgets.get(&g).map(|b| b.idle(now)))
                .unwrap_or(true);
            if idle {
                let _ = ws.close(Some(1001), Some("Idle group deadline"));
                self.remove(&ws)?;
            }
        }
        let effects = self
            .runtime
            .borrow_mut()
            .owner
            .as_mut()
            .map(|o| o.tick(now))
            .unwrap_or_default();
        self.deliver(effects)?;
        self.schedule_flush().await?;
        Response::ok("Lobby expiry checked")
    }
    async fn websocket_close(
        &self,
        ws: WebSocket,
        _code: usize,
        _reason: String,
        _clean: bool,
    ) -> Result<()> {
        self.remove(&ws)
    }
    async fn websocket_error(&self, ws: WebSocket, _error: worker::Error) -> Result<()> {
        let _ = ws.close(Some(1011), Some("Transport failure"));
        self.remove(&ws)
    }
}
