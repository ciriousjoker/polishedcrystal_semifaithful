//! Pure finite EMPTY projection. This module is not a live INT1 adapter.
//! Local scope and raw step-flag authority are explicit external proof obligations.
use crate::native_appearance::{Key, ReceiverLighting, SealedReferences};
use crate::static_ordinary_intent::{Figure, Part};
#[derive(Clone, Debug)]
pub struct Source {
    pub id: u64,
    pub entry: u64,
    pub returned: u64,
    pub closed: u64,
    pub original: [[u8; 444]; 3],
    /// Entry, return, close; X, Y, step flags, step vector X/Y, player state.
    pub operands: [[u8; 6]; 3],
}
#[derive(Clone, Debug)]
pub struct Accepted {
    pub id: u64,
    pub lifetime: u64,
    pub transaction: u64,
    pub captured: u64,
    pub committed: u64,
    pub receiver_id: u16,
    pub receiver_generation: u16,
    /// Exact expanded accepted definition, including unavailable records.
    pub peers: Vec<[u8; 20]>,
}
/// The 444 bytes and six operands do not establish local map/instance.
/// A live adapter must independently authenticate the frozen EMPTY native map
/// preimage (width/height/tileset/interior blocks), its B/C hash and applicability
/// to this completed source. No live private instance or delayed outgoing upload
/// can supply expected eligibility. Local epoch equality is not a visibility rule.
#[derive(Clone, Debug)]
pub struct IndependentLocalScope {
    pub source: u64,
    pub observed: u64,
    pub map: [u8; 2],
    pub instance: u16,
    pub evidence: LocalScopeEvidence,
}
#[derive(Clone, Debug)]
pub enum LocalScopeEvidence {
    FrozenMapPreimage { preimage_sha256: [u8; 32] },
}
/// Deliberately unresolved until an actual bounded native sample validates raw
/// flags. Numeric zero is not inferred from names, vectors or stationary walking.
#[derive(Clone, Debug)]
pub struct ValidatedStationaryEvidence {
    pub raw_step_flags: u8,
    pub native_sample_sha256: [u8; 32],
}
#[derive(Clone, Debug)]
pub struct Input {
    pub source: Source,
    /// None is genuine prejoin, with no manufactured accepted definition.
    pub accepted: Option<Accepted>,
    /// Experiment lifetime and local identity, independent of roster presence.
    pub local_identity: (u64, u16, u16),
    pub opportunity: u64,
    pub opportunity_instruction: u64,
    pub receiver_role: u8,
    pub local_avatar: u8,
    pub local_scope: Option<IndependentLocalScope>,
    pub stationary: Option<ValidatedStationaryEvidence>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    MissingLocalScope,
    MissingStationaryEvidence,
    Chronology,
    Identity,
    Unstable,
    UnsupportedScene,
    Pose,
    Scope,
    Clipped,
    Dictionary,
}
#[derive(Debug)]
pub struct Projection {
    /// Retain exact input, revisions, availability and chronology even if ineligible.
    pub provenance: Input,
    pub figures: Vec<Figure>,
}
fn be(b: &[u8], n: usize) -> u16 {
    u16::from_be_bytes([b[n], b[n + 1]])
}
fn nonzero_digest(h: &[u8; 32]) -> bool {
    h.iter().any(|&b| b != 0)
}
/// Trusted typed-input API only. Evidence digests are provenance, not proof
/// authentication. A live adapter MUST validate those external proof obligations
/// before constructing Input. No such adapter is implemented or authorized here.
pub fn project(sealed: &SealedReferences, input: Input) -> Result<Projection, Error> {
    let stationary = input
        .stationary
        .as_ref()
        .ok_or(Error::MissingStationaryEvidence)?;
    let s = &input.source;
    let accepted = input.accepted.as_ref();
    let (lifetime, local_id, local_generation) = input.local_identity;
    if s.id == 0
        || s.entry == 0
        || !(s.entry < s.returned
            && s.returned < s.closed
            && s.closed < input.opportunity_instruction)
        || input.opportunity == 0
    {
        return Err(Error::Chronology);
    }
    if !nonzero_digest(&stationary.native_sample_sha256) {
        return Err(Error::Scope);
    }
    if lifetime == 0 {
        return Err(Error::Identity);
    }
    if let Some(a) = accepted {
        if a.id == 0
            || a.transaction == 0
            || a.captured == 0
            || !(a.captured < a.committed && a.committed < input.opportunity_instruction)
        {
            return Err(Error::Chronology);
        }
        if a.lifetime != lifetime
            || a.receiver_id != local_id
            || a.receiver_generation != local_generation
            || a.receiver_id == 0
            || a.receiver_generation == 0
            || a.peers.len() > 3
        {
            return Err(Error::Identity);
        }
    }
    if s.original[0] != s.original[1]
        || s.original[0] != s.original[2]
        || s.operands[0] != s.operands[1]
        || s.operands[0] != s.operands[2]
    {
        return Err(Error::Unstable);
    }
    let raw = &s.original[0];
    let c = s.operands[0];
    let p = &raw[..34];
    if (1..13).any(|n| raw[n * 34] != 0) {
        return Err(Error::UnsupportedScene);
    }
    // E013: walking255 is stationary, duration at10 is intentionally unrestricted.
    if p[7] != 255
        || p[8] != 0
        || p[13] != 0
        || p[25] != 0
        || p[26] != 0
        || p[4] & 1 != 0
        || p[5] & 0xd8 != 0
        || p[6] & 0x10 != 0
        || p[16..18] != p[18..20]
        || c[2] != stationary.raw_step_flags
        || c[3..6] != [0, 0, 0]
        || u16::from(p[16]) != u16::from(c[0]) + 4
        || u16::from(p[17]) != u16::from(c[1]) + 4
    {
        return Err(Error::Pose);
    }
    let key = Key::stationary(
        input.receiver_role,
        input.local_avatar,
        ReceiverLighting::GameCornerDay12,
    )
    .map_err(|_| Error::Dictionary)?;
    if p[0] != sealed.get(key).sprite() || p[33] != input.local_avatar {
        return Err(Error::Dictionary);
    }
    let x = p[23]
        .wrapping_add(p[25])
        .wrapping_add(raw[442])
        .wrapping_add(8) as i32;
    let y = p[24]
        .wrapping_add(p[26])
        .wrapping_add(raw[443])
        .wrapping_add(12) as i32;
    if x != 72 || y != 76 {
        return Err(Error::Pose);
    } // independent stationary world crosscheck
    let mut figures = vec![figure(
        sealed,
        key,
        lifetime,
        local_id,
        local_generation,
        x,
        y,
    )?];
    let peers = accepted.map(|a| a.peers.as_slice()).unwrap_or(&[]);
    for (n, peer) in peers.iter().enumerate() {
        let id = be(peer, 0);
        let generation = be(peer, 2);
        if id == 0 || generation == 0 || id == local_id || peers[..n].iter().any(|p| be(p, 0) == id)
        {
            return Err(Error::Identity);
        }
        if peer[17] > 1
            || peer[18] > 3
            || peer[19] > 5
            || be(peer, 12) > 4095
            || be(peer, 14) > 4095
        {
            return Err(Error::UnsupportedScene);
        }
        if peer[17] == 0 {
            continue;
        }
        let scope = input.local_scope.as_ref().ok_or(Error::MissingLocalScope)?;
        if scope.source != s.id || scope.observed == 0 || scope.observed > s.closed {
            return Err(Error::Chronology);
        }
        let valid = match &scope.evidence {
            LocalScopeEvidence::FrozenMapPreimage { preimage_sha256 } => {
                nonzero_digest(preimage_sha256)
            }
        };
        // Outgoing readiness may lag demand, so it cannot authorize eligibility.
        if !valid {
            return Err(Error::Scope);
        }
        // Peer epoch is retained freshness input, not receiver-epoch equality.
        if peer[6..8] != scope.map || be(peer, 8) != scope.instance {
            continue;
        }
        if peer[16] != 0 || peer[19] != 0 {
            return Err(Error::Pose);
        }
        let k = Key::stationary(
            input.receiver_role,
            peer[18],
            ReceiverLighting::GameCornerDay12,
        )
        .map_err(|_| Error::Dictionary)?;
        let px = i32::from(be(peer, 12)) - 16 * (i32::from(c[0]) - 4) + 8;
        let py = i32::from(be(peer, 14)) - 16 * (i32::from(c[1]) - 4) + 12;
        figures.push(figure(sealed, k, lifetime, id, generation, px, py)?);
    }
    Ok(Projection {
        provenance: input,
        figures,
    })
}
fn figure(
    s: &SealedReferences,
    k: Key,
    lifetime: u64,
    id: u16,
    generation: u16,
    x: i32,
    y: i32,
) -> Result<Figure, Error> {
    if !(8..=152).contains(&x) || !(16..=144).contains(&y) {
        return Err(Error::Clipped);
    }
    let parts = s.get(k).parts().map(|p| Part {
        x: (x + i32::from(p.dx)) as i16,
        y: (y + i32::from(p.dy)) as i16,
        flags: p.flags,
        tile: p.tile16,
        rgb: p.rgb8,
    });
    Ok(Figure {
        lifetime,
        id,
        generation,
        parts,
    })
}
