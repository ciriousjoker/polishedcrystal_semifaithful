//! Explicit, bounded lobby wire fields. All multibyte fields are big endian.
use crate::DecodeError;

pub const SCHEMA: u8 = 5;
include!(concat!(env!("OUT_DIR"), "/capacity.rs"));
pub const HELLO: u8 = 16;
pub const WELCOME: u8 = 17;
pub const PRESENCE: u8 = 18;
pub const ROSTER: u8 = 19;
pub const LEAVE: u8 = 20;
pub const LEFT: u8 = 21;
pub const ERROR: u8 = 22;
pub const POLL: u8 = 23;
pub const DELTA: u8 = 24;
/// Identity-bound receipt; never changes membership or a peer snapshot.
pub const RECEIPT: u8 = 25;
pub const SNAPSHOT_LEN: usize = 11;
pub const PEER_LEN: usize = 17;
pub const HELLO_LEN: usize = 47;
pub const WELCOME_LEN: usize = 15;
pub const PRESENCE_LEN: usize = 4 + SNAPSHOT_LEN;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Error {
    Malformed = 1,
    Incompatible = 2,
    Full = 3,
    Stale = 4,
    Resume = 5,
    Capacity = 6,
}

fn word(bytes: &[u8], at: usize) -> u16 {
    u16::from_be_bytes([bytes[at], bytes[at + 1]])
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hello {
    /// SHA-256 of the matching clean ordinary cartridge, independent of schema.
    pub counterpart: [u8; 32],
    pub mechanics: u8,
    pub lobby: u16,
    pub resume: u16,
    pub token: [u8; 8],
}

impl Hello {
    pub fn decode(bytes: &[u8]) -> Result<Self, Error> {
        if bytes.len() != HELLO_LEN {
            return Err(Error::Malformed);
        }
        if bytes[0] != SCHEMA || bytes[1] > 1 || bytes[2] != CAPACITY {
            return Err(Error::Incompatible);
        }
        if word(bytes, 3) == 0 {
            return Err(Error::Malformed);
        }
        Ok(Self {
            mechanics: bytes[1],
            lobby: word(bytes, 3),
            resume: word(bytes, 5),
            token: bytes[7..15].try_into().unwrap(),
            counterpart: bytes[15..47].try_into().unwrap(),
        })
    }

    pub fn encode(&self) -> Vec<u8> {
        let mut bytes = vec![SCHEMA, self.mechanics, CAPACITY];
        bytes.extend(self.lobby.to_be_bytes());
        bytes.extend(self.resume.to_be_bytes());
        bytes.extend(self.token);
        bytes.extend(self.counterpart);
        bytes
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "checkpoint", derive(serde::Serialize, serde::Deserialize))]
pub struct Welcome {
    pub player: u16,
    pub generation: u16,
    pub lobby: u16,
    pub token: [u8; 8],
}

impl Welcome {
    pub fn encode(&self) -> Vec<u8> {
        let mut bytes = self.player.to_be_bytes().to_vec();
        bytes.extend(self.generation.to_be_bytes());
        bytes.extend(self.lobby.to_be_bytes());
        bytes.extend(self.token);
        bytes.push(CAPACITY);
        bytes
    }

    pub fn decode(bytes: &[u8]) -> Result<Self, DecodeError> {
        if bytes.len() != WELCOME_LEN
            || bytes[14] != CAPACITY
            || word(bytes, 0) == 0
            || word(bytes, 2) == 0
        {
            return Err(DecodeError::Length);
        }
        Ok(Self {
            player: word(bytes, 0),
            generation: word(bytes, 2),
            lobby: word(bytes, 4),
            token: bytes[6..14].try_into().unwrap(),
        })
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "checkpoint", derive(serde::Serialize, serde::Deserialize))]
pub struct Snapshot {
    pub map_group: u8,
    pub map_number: u8,
    pub instance: u16,
    pub epoch: u16,
    pub x: u16,
    pub y: u16,
    pub facing: u8,
    pub status: u8,
    pub avatar: u8,
    pub movement: u8,
}

impl Snapshot {
    pub fn is_valid(&self) -> bool {
        self.x <= 4095
            && self.y <= 4095
            && self.facing < 16
            && self.status <= 1
            && self.avatar < 4
            && self.movement <= 5
    }
    pub fn encode(&self) -> Vec<u8> {
        assert!(self.is_valid());
        let mut bytes = vec![self.map_group, self.map_number];
        bytes.extend(self.instance.to_be_bytes());
        bytes.extend(self.epoch.to_be_bytes());
        bytes.extend([
            (self.x >> 4) as u8,
            ((self.x << 4) as u8) | (self.y >> 8) as u8,
            self.y as u8,
            (self.facing << 4) | (self.movement << 1) | self.status,
            self.avatar,
        ]);
        bytes
    }

    pub fn decode(bytes: &[u8]) -> Result<Self, Error> {
        if bytes.len() != SNAPSHOT_LEN || (bytes[9] >> 1) & 7 > 5 || bytes[10] >= 4 {
            return Err(Error::Malformed);
        }
        Ok(Self {
            map_group: bytes[0],
            map_number: bytes[1],
            instance: word(bytes, 2),
            epoch: word(bytes, 4),
            x: (u16::from(bytes[6]) << 4) | u16::from(bytes[7] >> 4),
            y: (u16::from(bytes[7] & 15) << 8) | u16::from(bytes[8]),
            facing: bytes[9] >> 4,
            status: bytes[9] & 1,
            avatar: bytes[10],
            movement: (bytes[9] >> 1) & 7,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Peer {
    pub player: u16,
    pub generation: u16,
    pub revision: u16,
    pub snapshot: Snapshot,
}

impl Peer {
    pub fn decode(bytes: &[u8]) -> Result<Self, Error> {
        if bytes.len() != PEER_LEN || word(bytes, 0) == 0 || word(bytes, 2) == 0 {
            return Err(Error::Malformed);
        }
        Ok(Self {
            player: word(bytes, 0),
            generation: word(bytes, 2),
            revision: word(bytes, 4),
            snapshot: Snapshot::decode(&bytes[6..])?,
        })
    }
    pub fn encode(&self) -> Vec<u8> {
        let mut bytes = self.player.to_be_bytes().to_vec();
        bytes.extend(self.generation.to_be_bytes());
        bytes.extend(self.revision.to_be_bytes());
        bytes.extend(self.snapshot.encode());
        bytes
    }
}

pub fn roster(player: u16, generation: u16, peers: &[Peer]) -> Vec<u8> {
    assert!(peers.len() < usize::from(CAPACITY));
    let mut bytes = player.to_be_bytes().to_vec();
    bytes.extend(generation.to_be_bytes());
    bytes.push(peers.len() as u8);
    for peer in peers {
        bytes.extend(peer.encode());
    }
    bytes
}

/// Serial-number arithmetic, including wrap from 65535 to zero.
pub fn newer(next: u16, previous: u16) -> bool {
    let distance = next.wrapping_sub(previous);
    distance != 0 && distance < 0x8000
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hello_and_welcome_have_fixed_explicit_wire_layouts() {
        let hello = Hello {
            mechanics: 1,
            lobby: 0x1234,
            resume: 7,
            token: [0xa5; 8],
            counterpart: [0x42; 32],
        };
        assert_eq!(&hello.encode()[..7], &[5, 1, 4, 0x12, 0x34, 0, 7]);
        let encoded = hello.encode();
        assert_eq!(encoded.len(), HELLO_LEN);
        assert_eq!(&encoded[7..15], &[0xa5; 8]);
        assert_eq!(&encoded[15..47], &[0x42; 32]);
        let frame = crate::Frame {
            kind: HELLO,
            sequence: 1,
            payload: encoded.clone(),
        };
        assert_eq!(frame.encode().unwrap().len(), 55);
        assert_eq!(Hello::decode(&encoded), Ok(hello));
        let mut oversized = encoded.clone();
        oversized.push(0);
        assert_eq!(Hello::decode(&oversized), Err(Error::Malformed));
        let mut old_schema = encoded;
        old_schema[0] = 4;
        assert_eq!(Hello::decode(&old_schema), Err(Error::Incompatible));
        let welcome = Welcome {
            player: 0x1234,
            generation: 2,
            lobby: 1,
            token: [0xa5; 8],
        };
        assert_eq!(Welcome::decode(&welcome.encode()), Ok(welcome));
        for length in 0..HELLO_LEN {
            assert_eq!(Hello::decode(&vec![0; length]), Err(Error::Malformed));
        }
    }

    #[test]
    fn incompatible_rom_and_capacity_are_rejected_before_join() {
        let mut bytes = Hello {
            mechanics: 0,
            lobby: 1,
            resume: 0,
            token: [0; 8],
            counterpart: [0x42; 32],
        }
        .encode();
        for at in 0..3 {
            bytes[at] = 255;
            assert_eq!(Hello::decode(&bytes), Err(Error::Incompatible));
            bytes[at] = [SCHEMA, 0, CAPACITY][at];
        }
    }

    #[test]
    fn roster_is_bounded_and_snapshots_do_not_copy_host_structs() {
        let snapshot = Snapshot {
            map_group: 24,
            map_number: 5,
            instance: 0x1234,
            epoch: 2,
            x: 48,
            y: 64,
            facing: 12,
            status: 1,
            avatar: 0,
            movement: 3,
        };
        assert_eq!(
            snapshot.encode(),
            [24, 5, 0x12, 0x34, 0, 2, 3, 0, 64, 0xc7, 0]
        );
        assert_eq!(Snapshot::decode(&snapshot.encode()), Ok(snapshot));
        let peers = [
            Peer {
                player: 2,
                generation: 1,
                revision: 7,
                snapshot,
            },
            Peer {
                player: 3,
                generation: 2,
                revision: 9,
                snapshot,
            },
        ];
        assert_eq!(roster(1, 1, &peers).len(), 39);
        assert!(roster(1, 1, &peers).len() < crate::MAX_PAYLOAD);
        let mut invalid = snapshot.encode();
        invalid[9] = 255;
        assert_eq!(Snapshot::decode(&invalid), Err(Error::Malformed));
        let mut invalid_avatar = snapshot.encode();
        invalid_avatar[10] = 4;
        assert_eq!(Snapshot::decode(&invalid_avatar), Err(Error::Malformed));
        assert!(newer(0, 65535));
        assert!(!newer(65535, 0));
        assert!(!newer(7, 7));
    }

    #[test]
    fn packed_positions_and_states_preserve_every_coordinate_bit() {
        for x in 0..=4095 {
            for movement in 0..=5 {
                let snapshot = Snapshot {
                    x,
                    y: 4095 - x,
                    facing: (x & 15) as u8,
                    status: (x & 1) as u8,
                    movement,
                    avatar: (x & 3) as u8,
                    ..Snapshot::default()
                };
                assert_eq!(Snapshot::decode(&snapshot.encode()), Ok(snapshot));
            }
        }
    }
}
