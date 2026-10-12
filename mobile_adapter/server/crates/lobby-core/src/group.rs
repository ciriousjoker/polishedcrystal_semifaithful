//! Runtime-independent multiplexing and bounded latest-state pushes for Bridges.
use crate::Room;
use mobile_protocol::{lobby as wire, Frame, HEADER_LEN, MAX_PAYLOAD};
use std::collections::{BTreeMap, BTreeSet};

pub const ENVELOPE_LEN: usize = 6;
pub const MAX_MESSAGE: usize = ENVELOPE_LEN + HEADER_LEN + MAX_PAYLOAD;
pub const MAX_STREAM: u16 = 16;
pub const MAX_GROUPS: usize = 8;
/// Latest-state cloud pushes are bounded to approximately thirty per second.
/// Embedded play bypasses this network cadence without adding a second queue.
pub const PRESENCE_INTERVAL_MS: u64 = 33;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Envelope {
    pub stream: u16,
    /// None closes just this emulator stream, never the entire Bridge socket.
    pub frame: Option<Frame>,
}

impl Envelope {
    pub fn encode(&self) -> Result<Vec<u8>, wire::Error> {
        if self.stream == 0 || self.stream > MAX_STREAM {
            return Err(wire::Error::Malformed);
        }
        let mut bytes = vec![b'M', b'B', 1, u8::from(self.frame.is_some())];
        bytes.extend(self.stream.to_be_bytes());
        if let Some(frame) = &self.frame {
            bytes.extend(frame.encode().map_err(|_| wire::Error::Malformed)?);
        }
        Ok(bytes)
    }

    pub fn decode(bytes: &[u8]) -> Result<Self, wire::Error> {
        if bytes.len() < ENVELOPE_LEN
            || bytes.len() > MAX_MESSAGE
            || bytes[..3] != [b'M', b'B', 1]
            || bytes[3] > 1
        {
            return Err(wire::Error::Malformed);
        }
        let stream = u16::from_be_bytes([bytes[4], bytes[5]]);
        if stream == 0 || stream > MAX_STREAM {
            return Err(wire::Error::Malformed);
        }
        let frame = if bytes[3] == 1 {
            Some(Frame::decode(&bytes[6..]).map_err(|_| wire::Error::Malformed)?)
        } else {
            if bytes.len() != ENVELOPE_LEN {
                return Err(wire::Error::Malformed);
            }
            None
        };
        Ok(Self { stream, frame })
    }
}

#[derive(Clone)]
#[cfg_attr(feature = "checkpoint", derive(serde::Serialize, serde::Deserialize))]
struct Member {
    session: u64,
    last_sent_at: u64,
    last: Vec<u8>,
}

#[derive(Clone)]
#[cfg_attr(feature = "checkpoint", derive(serde::Serialize, serde::Deserialize))]
pub struct Owner {
    room: Room,
    next_session: u64,
    members: BTreeMap<(u64, u16), Member>,
}

pub struct Effect {
    pub group: u64,
    pub envelope: Envelope,
}

impl Owner {
    pub fn new(room: u16) -> Result<Self, wire::Error> {
        Ok(Self {
            room: Room::new(room)?,
            next_session: 0,
            members: BTreeMap::new(),
        })
    }
    pub fn room_id(&self) -> u16 {
        self.room.id()
    }
    pub fn member_count(&self) -> usize {
        self.members.len()
    }

    /// Entropy and time are supplied by the adapter. Memory and effects are
    /// bounded by lobby capacity, not the volume of positions a client sends.
    pub fn receive(
        &mut self,
        group: u64,
        envelope: Envelope,
        token: [u8; 8],
        now: u64,
    ) -> Vec<Effect> {
        let key = (group, envelope.stream);
        let mut effects = Vec::new();
        if group == 0 || envelope.stream == 0 || envelope.stream > MAX_STREAM {
            return effects;
        }
        let Some(frame) = envelope.frame else {
            self.remove(key, now);
            self.broadcast(now, &mut effects, false);
            return effects;
        };
        // A due presence replaces the queued position before flushing it. Sending
        // the previous position here would start another entire cadence interval
        // for the newer state arriving at exactly the flush boundary.
        if frame.kind != wire::PRESENCE {
            self.broadcast(now, &mut effects, false);
        }
        let result = if frame.kind == wire::HELLO && !self.members.contains_key(&key) {
            if self.members.len() >= usize::from(wire::CAPACITY) {
                effects.push(Effect {
                    group,
                    envelope: Envelope {
                        stream: key.1,
                        frame: Some(Frame {
                            kind: wire::ERROR,
                            sequence: frame.sequence,
                            payload: vec![wire::Error::Full as u8],
                        }),
                    },
                });
                return effects;
            }
            match self.next_session.checked_add(1) {
                Some(session) => match self.room.reply(session, &frame, token, now) {
                    Ok(reply) => {
                        self.next_session = session;
                        self.members.insert(
                            key,
                            Member {
                                session,
                                last_sent_at: now,
                                last: Vec::new(),
                            },
                        );
                        Ok(Some(reply))
                    }
                    Err(error) => Err(error),
                },
                None => Err(wire::Error::Capacity),
            }
        } else if let Some(member) = self.members.get_mut(&key) {
            if frame.kind == wire::POLL && frame.sequence == 0 && frame.payload.is_empty() {
                Ok(None)
            } else if frame.kind == wire::PRESENCE || frame.kind == wire::LEAVE {
                self.room
                    .reply(member.session, &frame, token, now)
                    .map(|mut reply| {
                        if reply.kind == wire::ROSTER {
                            // Confirm only a presence accepted by the room authority.
                            // A Bridge may use this receipt as a bounded local-read
                            // credit without inventing a join or fresh peer state.
                            reply.kind = wire::RECEIPT;
                            reply.payload.truncate(4);
                        }
                        Some(reply)
                    })
            } else {
                Err(wire::Error::Malformed)
            }
        } else {
            Err(wire::Error::Stale)
        };
        match result {
            Ok(Some(reply)) => {
                let left = reply.kind == wire::LEFT;
                effects.push(Effect {
                    group,
                    envelope: Envelope {
                        stream: key.1,
                        frame: Some(reply),
                    },
                });
                if left {
                    self.remove(key, now);
                }
            }
            Ok(None) => {}
            Err(error) => {
                effects.push(Effect {
                    group,
                    envelope: Envelope {
                        stream: key.1,
                        frame: Some(Frame {
                            kind: wire::ERROR,
                            sequence: frame.sequence,
                            payload: vec![error as u8],
                        }),
                    },
                });
                self.remove(key, now);
            }
        }
        self.broadcast(now, &mut effects, false);
        effects
    }

    fn remove(&mut self, key: (u64, u16), now: u64) {
        if let Some(member) = self.members.remove(&key) {
            self.room.disconnect(member.session, now);
        }
    }

    pub fn disconnect_group(&mut self, group: u64, now: u64) -> Vec<Effect> {
        let keys: Vec<_> = self
            .members
            .keys()
            .filter(|k| k.0 == group)
            .copied()
            .collect();
        for key in keys {
            self.remove(key, now);
        }
        let mut effects = Vec::new();
        self.broadcast(now, &mut effects, false);
        effects
    }

    pub fn tick(&mut self, now: u64) -> Vec<Effect> {
        let mut effects = Vec::new();
        self.broadcast(now, &mut effects, false);
        effects
    }

    /// An embedded owner has no cloud transport cost. Flush the same bounded
    /// latest-state effects immediately; membership and validation are unchanged.
    pub fn tick_local(&mut self, now: u64) -> Vec<Effect> {
        let mut effects = Vec::new();
        self.broadcast(now, &mut effects, true);
        effects
    }

    fn broadcast(&mut self, now: u64, effects: &mut Vec<Effect>, local: bool) {
        let mut expired = Vec::new();
        for (&(group, stream), member) in &mut self.members {
            let Some(frame) = self.room.roster(member.session, now) else {
                expired.push((group, stream));
                effects.push(Effect {
                    group,
                    envelope: Envelope {
                        stream,
                        frame: None,
                    },
                });
                continue;
            };
            let mut key = frame.payload.clone();
            for peer in key[5..].chunks_exact_mut(wire::PEER_LEN) {
                peer[4..6].fill(0);
            }
            if key != member.last
                && (local
                    || member.last.is_empty()
                    || now.saturating_sub(member.last_sent_at) >= PRESENCE_INTERVAL_MS)
            {
                member.last = key;
                member.last_sent_at = now;
                effects.push(Effect {
                    group,
                    envelope: Envelope {
                        stream,
                        frame: Some(frame),
                    },
                });
            }
        }
        for key in expired {
            self.members.remove(&key);
        }
    }

    /// Earliest pending coalesced push. Hosting adapters schedule a short flush
    /// only when needed; an idle room does not require a permanent timer.
    pub fn flush_after(&mut self, now: u64) -> Option<u64> {
        self.members
            .values()
            .filter_map(|member| {
                let frame = self.room.roster(member.session, now)?;
                let mut key = frame.payload;
                for peer in key[5..].chunks_exact_mut(wire::PEER_LEN) {
                    peer[4..6].fill(0);
                }
                (key != member.last).then(|| {
                    PRESENCE_INTERVAL_MS.saturating_sub(now.saturating_sub(member.last_sent_at))
                })
            })
            .min()
    }

    pub fn checkpoint(&self) -> Self {
        let mut copy = self.clone();
        copy.room = copy.room.checkpoint();
        copy
    }

    pub fn validate_checkpoint(&self) -> bool {
        self.room.validate_checkpoint()
            && self.room.session_count() == self.members.len()
            && self.members.len() <= usize::from(wire::CAPACITY)
            && self.members.iter().all(|(&(group, stream), m)| {
                group != 0
                    && stream > 0
                    && stream <= MAX_STREAM
                    && m.session != 0
                    && m.session <= self.next_session
                    && m.last.len() <= 5 + (usize::from(wire::CAPACITY) - 1) * wire::PEER_LEN
            })
            && self
                .members
                .values()
                .map(|m| m.session)
                .collect::<BTreeSet<_>>()
                .len()
                == self.members.len()
            && self
                .members
                .values()
                .all(|m| self.room.has_session(m.session))
    }
}
