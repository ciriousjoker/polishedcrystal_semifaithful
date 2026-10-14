//! Bounded session model, independent of sockets and wall-clock sleeps.
use mobile_protocol::lobby::{newer, Error, Hello, Peer, Snapshot, Welcome, CAPACITY};
use std::collections::BTreeMap;
pub mod budget;
pub mod group;

const MAX_IDENTITIES: usize = 128;
const RESUME_MS: u64 = 30_000;
const LIVE_MS: u64 = 3_000;

#[derive(Clone)]
#[cfg_attr(feature = "checkpoint", derive(serde::Serialize, serde::Deserialize))]
struct Player {
    welcome: Welcome,
    mechanics: u8,
    counterpart: [u8; 32],
    session: Option<u64>,
    last_seen: u64,
    revision: Option<u16>,
    snapshot: Option<Snapshot>,
}

#[derive(Default, Clone)]
#[cfg_attr(feature = "checkpoint", derive(serde::Serialize, serde::Deserialize))]
pub struct Lobbies {
    players: BTreeMap<u16, Player>,
    next_player: u16,
}

/// A Durable Object owns exactly one room. Time and entropy come from its adapter.
#[derive(Clone)]
#[cfg_attr(feature = "checkpoint", derive(serde::Serialize, serde::Deserialize))]
pub struct Room {
    id: u16,
    model: Lobbies,
}

impl Room {
    pub fn new(id: u16) -> Result<Self, Error> {
        if id == 0 {
            return Err(Error::Malformed);
        }
        Ok(Self {
            id,
            model: Lobbies::default(),
        })
    }

    pub fn id(&self) -> u16 {
        self.id
    }
    pub fn has_session(&self, session: u64) -> bool {
        self.model
            .players
            .values()
            .any(|p| p.session == Some(session))
    }
    pub fn session_count(&self) -> usize {
        self.model
            .players
            .values()
            .filter(|p| p.session.is_some())
            .count()
    }

    pub fn reply(
        &mut self,
        session: u64,
        frame: &mobile_protocol::Frame,
        token: [u8; 8],
        now: u64,
    ) -> Result<mobile_protocol::Frame, Error> {
        use mobile_protocol::lobby as wire;
        let (kind, payload) = match frame.kind {
            wire::HELLO => {
                let hello = Hello::decode(&frame.payload)?;
                if hello.lobby != self.id {
                    return Err(Error::Malformed);
                }
                (
                    wire::WELCOME,
                    self.model.join(session, hello, token, now)?.encode(),
                )
            }
            wire::PRESENCE | wire::LEAVE => {
                let size = if frame.kind == wire::PRESENCE {
                    wire::PRESENCE_LEN
                } else {
                    4
                };
                if frame.payload.len() != size {
                    return Err(Error::Malformed);
                }
                let player = u16::from_be_bytes([frame.payload[0], frame.payload[1]]);
                let generation = u16::from_be_bytes([frame.payload[2], frame.payload[3]]);
                if frame.kind == wire::LEAVE {
                    self.model.leave(session, player, generation)?;
                    (wire::LEFT, frame.payload.clone())
                } else {
                    let snapshot = Snapshot::decode(&frame.payload[4..])?;
                    let peers = self.model.presence(
                        session,
                        player,
                        generation,
                        frame.sequence,
                        snapshot,
                        now,
                    )?;
                    (wire::ROSTER, wire::roster(player, generation, &peers))
                }
            }
            _ => return Err(Error::Malformed),
        };
        Ok(mobile_protocol::Frame {
            kind,
            sequence: frame.sequence,
            payload,
        })
    }

    pub fn roster(&mut self, session: u64, now: u64) -> Option<mobile_protocol::Frame> {
        use mobile_protocol::lobby as wire;
        let (player, generation, peers) = self.model.roster(session, now)?;
        Some(mobile_protocol::Frame {
            kind: wire::ROSTER,
            sequence: 0,
            payload: wire::roster(player, generation, &peers),
        })
    }

    pub fn disconnect(&mut self, session: u64, now: u64) {
        self.model.disconnect(session, now);
    }

    /// Only live members enter a bounded WebSocket attachment. Offline resume
    /// records are disposable when the hosting runtime reconstructs the room.
    pub fn checkpoint(&self) -> Self {
        let mut copy = self.clone();
        copy.model.players.retain(|_, p| p.session.is_some());
        copy
    }

    pub fn validate_checkpoint(&self) -> bool {
        let players = &self.model.players;
        self.id != 0
            && players.len() <= usize::from(CAPACITY)
            && players.iter().all(|(id, p)| {
                *id != 0
                    && *id == p.welcome.player
                    && p.welcome.generation != 0
                    && p.welcome.lobby == self.id
                    && p.mechanics <= 1
                    && p.session.is_some_and(|s| s != 0)
                    && p.snapshot.is_some() == p.revision.is_some()
                    && p.snapshot.is_none_or(|s| s.is_valid())
            })
            && players.values().all(|p| {
                players.values().all(|q| {
                    p.mechanics == q.mechanics
                        && p.counterpart == q.counterpart
                        && (p.welcome.player == q.welcome.player || p.session != q.session)
                })
            })
    }
}

impl Lobbies {
    pub fn snapshot(&self, session: u64) -> Option<Snapshot> {
        self.players
            .values()
            .find(|p| p.session == Some(session))?
            .snapshot
    }
    fn expire(&mut self, now: u64) {
        for player in self.players.values_mut() {
            if now.saturating_sub(player.last_seen) > LIVE_MS {
                player.session = None;
                player.snapshot = None;
            }
        }
        self.players.retain(|_, player| {
            player.session.is_some() || now.saturating_sub(player.last_seen) <= RESUME_MS
        });
    }

    pub fn join(
        &mut self,
        session: u64,
        hello: Hello,
        token: [u8; 8],
        now: u64,
    ) -> Result<Welcome, Error> {
        if session == 0 || hello.lobby == 0 || hello.mechanics > 1 {
            return Err(Error::Malformed);
        }
        self.expire(now);
        if self
            .players
            .values()
            .any(|player| player.session == Some(session))
        {
            return Err(Error::Stale);
        }
        let occupied = self
            .players
            .values()
            .filter(|player| {
                player.session.is_some()
                    && player.welcome.lobby == hello.lobby
                    && player.welcome.player != hello.resume
            })
            .count();
        if occupied >= usize::from(CAPACITY) {
            return Err(Error::Full);
        }
        if self.players.values().any(|player| {
            player.session.is_some()
                && player.welcome.lobby == hello.lobby
                && (player.mechanics != hello.mechanics || player.counterpart != hello.counterpart)
        }) {
            return Err(Error::Incompatible);
        }
        if hello.resume != 0 {
            let player = self.players.get_mut(&hello.resume).ok_or(Error::Resume)?;
            if player.counterpart != hello.counterpart {
                return Err(Error::Incompatible);
            }
            if player.welcome.token != hello.token
                || player.welcome.lobby != hello.lobby
                || player.mechanics != hello.mechanics
                || player.welcome.generation == u16::MAX
            {
                return Err(Error::Resume);
            }
            player.welcome.generation += 1;
            player.session = Some(session);
            player.last_seen = now;
            player.revision = None;
            player.snapshot = None;
            return Ok(player.welcome.clone());
        }
        if self.players.len() >= MAX_IDENTITIES {
            // Connection churn cannot exhaust capacity for healthy members.
            // Resume records are disposable; evict only the oldest offline one.
            let oldest = self
                .players
                .iter()
                .filter(|(_, p)| p.session.is_none())
                .min_by_key(|(_, p)| p.last_seen)
                .map(|(&id, _)| id)
                .ok_or(Error::Capacity)?;
            self.players.remove(&oldest);
        }
        // Reuse only unoccupied identifiers after wrap. Authentication still
        // requires the new unpredictable token and transport session.
        loop {
            self.next_player = self.next_player.wrapping_add(1).max(1);
            if !self.players.contains_key(&self.next_player) {
                break;
            }
        }
        let welcome = Welcome {
            player: self.next_player,
            generation: 1,
            lobby: hello.lobby,
            token,
        };
        self.players.insert(
            welcome.player,
            Player {
                welcome: welcome.clone(),
                mechanics: hello.mechanics,
                counterpart: hello.counterpart,
                session: Some(session),
                last_seen: now,
                revision: None,
                snapshot: None,
            },
        );
        Ok(welcome)
    }

    pub fn presence(
        &mut self,
        session: u64,
        player: u16,
        generation: u16,
        revision: u16,
        snapshot: Snapshot,
        now: u64,
    ) -> Result<Vec<Peer>, Error> {
        if !snapshot.is_valid() {
            return Err(Error::Malformed);
        }
        self.expire(now);
        let current = self.players.get_mut(&player).ok_or(Error::Stale)?;
        if current.session != Some(session)
            || current.welcome.generation != generation
            || current
                .revision
                .is_some_and(|previous| !newer(revision, previous))
        {
            return Err(Error::Stale);
        }
        if let Some(previous) = current.snapshot {
            let changed_map = previous.map_group != snapshot.map_group
                || previous.map_number != snapshot.map_number
                || previous.instance != snapshot.instance;
            if (!newer(snapshot.epoch, previous.epoch) && snapshot.epoch != previous.epoch)
                || (changed_map && !newer(snapshot.epoch, previous.epoch))
            {
                return Err(Error::Stale);
            }
        }
        current.snapshot = Some(snapshot);
        current.revision = Some(revision);
        current.last_seen = now;
        let lobby = current.welcome.lobby;
        Ok(self
            .players
            .values()
            .filter(|other| {
                other.welcome.player != player
                    && other.welcome.lobby == lobby
                    && other.session.is_some()
            })
            .filter_map(|other| {
                Some(Peer {
                    player: other.welcome.player,
                    generation: other.welcome.generation,
                    revision: other.revision?,
                    snapshot: other.snapshot?,
                })
            })
            .collect())
    }

    pub fn leave(&mut self, session: u64, player: u16, generation: u16) -> Result<(), Error> {
        let current = self.players.get(&player).ok_or(Error::Stale)?;
        if current.session != Some(session) || current.welcome.generation != generation {
            return Err(Error::Stale);
        }
        self.players.remove(&player);
        Ok(())
    }

    /// A full latest roster for a live connection, including removal of peers.
    pub fn roster(&mut self, session: u64, now: u64) -> Option<(u16, u16, Vec<Peer>)> {
        self.expire(now);
        let current = self.players.values().find(|p| p.session == Some(session))?;
        let identity = (current.welcome.player, current.welcome.generation);
        let room = current.welcome.lobby;
        let peers = self
            .players
            .values()
            .filter(|p| {
                p.session.is_some() && p.welcome.lobby == room && p.welcome.player != identity.0
            })
            .filter_map(|p| {
                Some(Peer {
                    player: p.welcome.player,
                    generation: p.welcome.generation,
                    revision: p.revision?,
                    snapshot: p.snapshot?,
                })
            })
            .collect();
        Some((identity.0, identity.1, peers))
    }

    pub fn disconnect(&mut self, session: u64, now: u64) -> Option<u16> {
        let current = self
            .players
            .values_mut()
            .find(|player| player.session == Some(session))?;
        current.session = None;
        current.snapshot = None;
        current.last_seen = now;
        Some(current.welcome.player)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn hello() -> Hello {
        Hello {
            mechanics: 0,
            lobby: 1,
            resume: 0,
            token: [0; 8],
            counterpart: [0x42; 32],
        }
    }
    fn snapshot() -> Snapshot {
        Snapshot {
            map_group: 24,
            map_number: 5,
            epoch: 1,
            x: 48,
            y: 48,
            status: 1,
            ..Snapshot::default()
        }
    }

    #[test]
    fn counterpart_admission_resume_and_empty_room_lifetime() {
        let mut model = Lobbies::default();
        let first = model.join(1, hello(), [7; 8], 0).unwrap();
        let mut changed = hello();
        changed.counterpart[31] ^= 1;
        assert_eq!(
            model.join(2, changed.clone(), [2; 8], 1),
            Err(Error::Incompatible)
        );
        // Same trainer identity is irrelevant: no trainer ID participates in HELLO.
        let healthy = model.join(2, hello(), [2; 8], 1).unwrap();
        assert_ne!(first.player, healthy.player);
        let mut other_room = changed.clone();
        other_room.lobby = 2;
        assert!(model.join(3, other_room, [3; 8], 1).is_ok());
        model.disconnect(1, 2);
        model.disconnect(2, 2);
        changed.resume = first.player;
        changed.token = first.token;
        assert_eq!(
            model.join(4, changed.clone(), [4; 8], 3),
            Err(Error::Incompatible)
        );
        // A rejected mismatch cannot mutate the retained identity or generation.
        let mut resume = hello();
        resume.resume = first.player;
        resume.token = first.token;
        let resumed = model.join(4, resume.clone(), [4; 8], 3).unwrap();
        assert_eq!(resumed.generation, 2);
        model.disconnect(4, 4);
        changed.resume = 0;
        changed.token = [0; 8];
        // With no live members, a fresh join may establish a new room identity.
        let replacement = model.join(5, changed, [5; 8], 5).unwrap();
        assert_eq!(
            model.join(6, resume.clone(), [6; 8], 6),
            Err(Error::Incompatible)
        );
        model.disconnect(5, 7);
        assert_eq!(model.join(6, resume, [6; 8], 8).unwrap().generation, 3);
        assert_ne!(replacement.player, first.player);
    }

    #[test]
    fn typed_invalid_identity_attempt_does_not_change_membership() {
        let mut model = Lobbies::default();
        let mut invalid = hello();
        invalid.mechanics = 2;
        assert_eq!(model.join(1, invalid, [1; 8], 0), Err(Error::Malformed));
        let mut invalid = hello();
        invalid.lobby = 0;
        assert_eq!(model.join(1, invalid, [1; 8], 0), Err(Error::Malformed));
        assert_eq!(model.join(0, hello(), [1; 8], 0), Err(Error::Malformed));
        for session in 1..=4 {
            assert!(model.join(session, hello(), [session as u8; 8], 1).is_ok());
        }
        assert_eq!(model.join(5, hello(), [5; 8], 1), Err(Error::Full));
    }

    #[test]
    fn four_players_see_distinct_peers_and_fifth_is_rejected() {
        let mut model = Lobbies::default();
        for id in 1..=4 {
            let welcome = model.join(id, hello(), [id as u8; 8], 0).unwrap();
            model
                .presence(id, welcome.player, welcome.generation, 1, snapshot(), 1)
                .unwrap();
        }
        assert_eq!(model.join(5, hello(), [5; 8], 2), Err(Error::Full));
        let peers = model.presence(1, 1, 1, 2, snapshot(), 3).unwrap();
        assert_eq!(
            peers.iter().map(|p| p.player).collect::<Vec<_>>(),
            [2, 3, 4]
        );
        model.leave(2, 2, 1).unwrap();
        assert_eq!(model.presence(1, 1, 1, 3, snapshot(), 4).unwrap().len(), 2);
    }

    #[test]
    fn resume_invalidates_old_events_and_old_disconnect_without_losing_identity() {
        let mut model = Lobbies::default();
        let first = model.join(1, hello(), [7; 8], 0).unwrap();
        let mut resume = hello();
        resume.resume = first.player;
        resume.token = first.token;
        let second = model.join(2, resume, [8; 8], 1).unwrap();
        assert_eq!(second.player, first.player);
        assert_eq!(second.generation, 2);
        assert_eq!(model.disconnect(1, 2), None);
        assert_eq!(
            model.presence(1, first.player, 1, 1, snapshot(), 3),
            Err(Error::Stale)
        );
        assert!(model.presence(2, first.player, 2, 1, snapshot(), 4).is_ok());
    }

    #[test]
    fn rooms_and_mechanics_are_isolated_and_resume_requires_correct_token() {
        let mut model = Lobbies::default();
        let player = model.join(1, hello(), [7; 8], 0).unwrap();
        let mut other = hello();
        other.mechanics = 1;
        assert_eq!(
            model.join(2, other.clone(), [0; 8], 0),
            Err(Error::Incompatible)
        );
        other.lobby = 2;
        assert!(model.join(2, other, [9; 8], 0).is_ok());
        let mut resume = hello();
        resume.resume = player.player;
        assert_eq!(model.join(3, resume, [0; 8], 0), Err(Error::Resume));
        assert!(model
            .presence(1, player.player, 1, 1, snapshot(), 1)
            .unwrap()
            .is_empty());
    }

    #[test]
    fn latest_snapshot_coalesces_without_an_event_queue_and_stale_warps_are_rejected() {
        let mut model = Lobbies::default();
        model.join(1, hello(), [1; 8], 0).unwrap();
        model.join(2, hello(), [2; 8], 0).unwrap();
        for sequence in 1..=1000 {
            model
                .presence(
                    2,
                    2,
                    1,
                    sequence,
                    Snapshot {
                        x: sequence,
                        ..snapshot()
                    },
                    1,
                )
                .unwrap();
        }
        assert_eq!(
            model.presence(1, 1, 1, 1, snapshot(), 2).unwrap()[0]
                .snapshot
                .x,
            1000
        );
        let warp = Snapshot {
            map_number: 6,
            epoch: 2,
            ..snapshot()
        };
        model.presence(2, 2, 1, 1001, warp, 3).unwrap();
        assert_eq!(
            model.presence(2, 2, 1, 1002, snapshot(), 4),
            Err(Error::Stale)
        );
        assert_eq!(model.presence(2, 2, 1, 1001, warp, 4), Err(Error::Stale));
    }

    #[test]
    fn inactive_players_disappear_and_resume_retention_is_bounded() {
        let mut model = Lobbies::default();
        let first = model.join(1, hello(), [1; 8], 0).unwrap();
        model.presence(1, 1, 1, 1, snapshot(), 1).unwrap();
        model.join(2, hello(), [2; 8], 3002).unwrap();
        assert!(model
            .presence(2, 2, 1, 1, snapshot(), 3003)
            .unwrap()
            .is_empty());
        let mut resume = hello();
        resume.resume = first.player;
        resume.token = first.token;
        assert_eq!(model.join(3, resume, [3; 8], 30_002), Err(Error::Resume));
        for id in 4..=200 {
            model.join(id, hello(), [1; 8], id * 40_000).unwrap();
        }
        assert!(model.players.len() <= 2);
    }

    #[test]
    fn hostile_identity_churn_never_evicts_live_players_and_identifiers_wrap() {
        let mut model = Lobbies::default();
        let healthy = model.join(1, hello(), [1; 8], 0).unwrap();
        for session in 2..=1000 {
            let visitor = model.join(session, hello(), [2; 8], 1).unwrap();
            model.disconnect(session, 1);
            assert_ne!(visitor.player, healthy.player);
        }
        assert_eq!(model.players.len(), MAX_IDENTITIES);
        model
            .presence(1, healthy.player, 1, 1, snapshot(), 2)
            .unwrap();
        model.next_player = u16::MAX;
        let next = model.join(1001, hello(), [3; 8], 2).unwrap();
        assert_ne!(next.player, 0);
        assert_ne!(next.player, healthy.player);
        assert_eq!(model.players[&healthy.player].session, Some(1));
        let invalid = Snapshot {
            avatar: 255,
            ..snapshot()
        };
        assert_eq!(
            model.presence(1, healthy.player, 1, 2, invalid, 3),
            Err(Error::Malformed)
        );
        assert_eq!(model.snapshot(1), Some(snapshot()));
    }
}
