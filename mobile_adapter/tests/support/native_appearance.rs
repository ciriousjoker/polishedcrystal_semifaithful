//! Finite NRFv2 normalization of the sealed 114 disabled controls only.
//! No enabled input, camera projection, raster selection or asset interpretation.
use std::array;

pub const MANIFEST_SHA256: &str =
    "438605687f76cd63028994542f83909cf7949e00128e061b240841fb058a5229";
pub const REFERENCE_SHA256: [&str; 4] = [
    "ecf683747893d2ce4e2b24f77967e032e5746cb6ad2b255ea292ef67afe05c99",
    "a5dca2fab99da3afef7a0f9dbb84dbfb4614812246cf6eb8341ebd877394d589",
    "56dd8245cfb3238e8be84e4823295fb127aefe43c197799d2577b794af0ae968",
    "7839483c6ac9ac90a285131b6ed15301b07eba59e4e9c832ad9cfa2619b62cb0",
];
const ORDERS: [[usize; 4]; 4] = [[0, 1, 2, 3], [1, 0, 2, 3], [2, 0, 1, 3], [3, 0, 1, 2]];
const SPRITES: [u8; 4] = [1, 4, 199, 209];
const DELTAS: [(i8, i8); 4] = [(0, 0), (8, 0), (0, 8), (8, 8)];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    Version,
    Length,
    Manifest,
    Reference,
    Key,
    Pose,
    Geometry,
    MissingActor,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReceiverLighting {
    GameCornerDay12,
}
/// Role and avatar remain separate, even where authenticated appearance bytes agree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Key {
    receiver: u8,
    avatar: u8,
    lighting: ReceiverLighting,
}
impl Key {
    pub fn stationary(receiver: u8, avatar: u8, lighting: ReceiverLighting) -> Result<Self, Error> {
        if receiver > 3 || avatar > 3 {
            return Err(Error::Key);
        }
        Ok(Self {
            receiver,
            avatar,
            lighting,
        })
    }
    pub fn receiver(self) -> u8 {
        self.receiver
    }
    pub fn avatar(self) -> u8 {
        self.avatar
    }
    pub fn lighting(self) -> ReceiverLighting {
        self.lighting
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Part {
    pub dx: i8,
    pub dy: i8,
    /// All flags except physical VRAM bank and palette slot selectors.
    pub flags: u8,
    pub tile16: [u8; 16],
    pub rgb8: [u8; 8],
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OriginalPart {
    pub tuple: [u8; 4],
    pub oam_index: u8,
}
#[derive(Debug)]
pub struct Appearance {
    key: Key,
    parts: [Part; 4],
    original: [OriginalPart; 4],
    actor: u8,
    sprite: u8,
    palette: u8,
}
impl Appearance {
    pub fn key(&self) -> Key {
        self.key
    }
    pub fn parts(&self) -> &[Part; 4] {
        &self.parts
    }
    pub fn original(&self) -> &[OriginalPart; 4] {
        &self.original
    }
    pub fn actor(&self) -> u8 {
        self.actor
    }
    pub fn sprite(&self) -> u8 {
        self.sprite
    }
    pub fn canonical_palette(&self) -> u8 {
        self.palette
    }
    pub fn reference_sha256(&self) -> &'static str {
        REFERENCE_SHA256[self.key.receiver as usize]
    }
    pub fn manifest_sha256(&self) -> &'static str {
        MANIFEST_SHA256
    }
    /// Candidate selectors may move, but every byte and nonselector flag must match.
    /// This comparison grants no dictionary authority and proves no selected mask.
    pub fn matches(&self, key: Key, parts: &[Part]) -> bool {
        key == self.key
            && parts.len() == 4
            && parts.iter().zip(&self.parts).all(|(a, b)| {
                a.dx == b.dx
                    && a.dy == b.dy
                    && a.flags & 0xf0 == b.flags
                    && a.tile16 == b.tile16
                    && a.rgb8 == b.rgb8
            })
    }
}
/// Construction requires all four exact exports plus their immutable external proof.
/// The pinned manifest binds original ROM/SYM, reader, core, runner, observer,
/// role order and sixteen successful native pairs per role. No JSON parser needed.
#[derive(Debug)]
pub struct SealedReferences {
    appearances: [[Appearance; 4]; 4],
}
impl SealedReferences {
    pub fn from_114(version: u32, manifest: &[u8], references: [&[u8]; 4]) -> Result<Self, Error> {
        if version != 2 {
            return Err(Error::Version);
        }
        if manifest.len() > 16384 {
            return Err(Error::Length);
        }
        if sha256(manifest) != MANIFEST_SHA256 {
            return Err(Error::Manifest);
        }
        for (role, reference) in references.iter().enumerate() {
            if reference.len() != 1016 {
                return Err(Error::Length);
            }
            if sha256(reference) != REFERENCE_SHA256[role] {
                return Err(Error::Reference);
            }
        }
        let mut rows = Vec::with_capacity(4);
        for (role, reference) in references.iter().enumerate() {
            rows.push(normalize(role, reference)?);
        }
        Ok(Self {
            appearances: rows.try_into().unwrap(),
        })
    }
    pub fn get(&self, key: Key) -> &Appearance {
        &self.appearances[key.receiver as usize][key.avatar as usize]
    }
}
fn relative(value: u8, base: u8) -> i8 {
    value.wrapping_sub(base) as i8
}
fn normalize(role: usize, r: &[u8]) -> Result<[Appearance; 4], Error> {
    if role >= 4 {
        return Err(Error::Key);
    }
    if r.len() != 1016 {
        return Err(Error::Length);
    }
    let source = &r[44..488];
    let lighting = &r[936..];
    if lighting[52..57] != [1, 1, 1, 12, 0]
        || lighting[57] & 4 != 0
        || lighting[58..70] != [0, 0, 0, 1, 0, 1, 0x55, 11, 23, 3, 255, 0]
        || lighting[70..].iter().any(|&v| v != 0)
    {
        return Err(Error::Key);
    }
    if (4..13).any(|a| source[a * 34] != 0) {
        return Err(Error::MissingActor);
    }
    let mut appearances: [Option<Appearance>; 4] = array::from_fn(|_| None);
    for actor in 0..4 {
        let avatar = ORDERS[role][actor];
        let s = &source[actor * 34..actor * 34 + 34];
        if s[0] != SPRITES[avatar] || s[33] as usize != avatar {
            return Err(Error::Key);
        }
        if s[0] == 0 {
            return Err(Error::MissingActor);
        }
        if s[6] & 0x10 != 0
            || s[5] & 0xd8 != 0
            || s[4] & 1 != 0
            || s[7] != 255
            || s[13] != 0
            || s[25] != 0
            || s[26] != 0
        {
            return Err(Error::Pose);
        }
        let bx = s[23]
            .wrapping_add(s[25])
            .wrapping_add(source[442])
            .wrapping_add(8);
        let by = s[24]
            .wrapping_add(s[26])
            .wrapping_add(source[443])
            .wrapping_add(12);
        let original = array::from_fn(|n| {
            let at = 488 + (actor * 4 + n) * 28;
            OriginalPart {
                tuple: r[at..at + 4].try_into().unwrap(),
                oam_index: (39 - actor * 4 - n) as u8,
            }
        });
        let parts = array::from_fn(|n| {
            let at = 488 + (actor * 4 + n) * 28;
            let t = original[n].tuple;
            Part {
                dx: relative(t[1], bx),
                dy: relative(t[0], by),
                flags: t[3] & 0xf0,
                tile16: r[at + 4..at + 20].try_into().unwrap(),
                rgb8: r[at + 20..at + 28].try_into().unwrap(),
            }
        });
        for n in 0..4 {
            let t = original[n].tuple;
            if (parts[n].dx, parts[n].dy) != DELTAS[n]
                || !(8..=160).contains(&t[1])
                || !(16..=152).contains(&t[0])
            {
                return Err(Error::Geometry);
            }
        }
        appearances[avatar] = Some(Appearance {
            key: Key::stationary(role as u8, avatar as u8, ReceiverLighting::GameCornerDay12)?,
            parts,
            original,
            actor: actor as u8,
            sprite: s[0],
            palette: s[33],
        });
    }
    Ok(appearances.map(Option::unwrap))
}

// Standalone SHA-256 avoids a caller-supplied digest becoming authority.
fn sha256(input: &[u8]) -> String {
    const K: [u32; 64] = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
        0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
        0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
        0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
        0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
        0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
        0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
        0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
        0xc67178f2,
    ];
    let mut h: [u32; 8] = [
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
        0x5be0cd19,
    ];
    let mut bytes = input.to_vec();
    bytes.push(0x80);
    while bytes.len() % 64 != 56 {
        bytes.push(0);
    }
    bytes.extend_from_slice(&((input.len() as u64) * 8).to_be_bytes());
    for block in bytes.chunks_exact(64) {
        let mut w = [0u32; 64];
        for i in 0..16 {
            w[i] = u32::from_be_bytes(block[i * 4..i * 4 + 4].try_into().unwrap());
        }
        for i in 16..64 {
            let a = w[i - 15];
            let b = w[i - 2];
            w[i] = w[i - 16]
                .wrapping_add(a.rotate_right(7) ^ a.rotate_right(18) ^ (a >> 3))
                .wrapping_add(w[i - 7])
                .wrapping_add(b.rotate_right(17) ^ b.rotate_right(19) ^ (b >> 10));
        }
        let [mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut z] = h;
        for i in 0..64 {
            let t = z
                .wrapping_add(e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25))
                .wrapping_add((e & f) ^ (!e & g))
                .wrapping_add(K[i])
                .wrapping_add(w[i]);
            let u = (a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22))
                .wrapping_add((a & b) ^ (a & c) ^ (b & c));
            z = g;
            g = f;
            f = e;
            e = d.wrapping_add(t);
            d = c;
            c = b;
            b = a;
            a = t.wrapping_add(u);
        }
        for (v, n) in h.iter_mut().zip([a, b, c, d, e, f, g, z]) {
            *v = v.wrapping_add(n);
        }
    }
    h.iter().map(|v| format!("{v:08x}")).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    fn fixture() -> Vec<u8> {
        let mut r = vec![0; 1016];
        r[988..993].copy_from_slice(&[1, 1, 1, 12, 0]);
        r[994..1006].copy_from_slice(&[0, 0, 0, 1, 0, 1, 0x55, 11, 23, 3, 255, 0]);
        for a in 0..4 {
            let at = 44 + a * 34;
            r[at] = SPRITES[a];
            r[at + 33] = a as u8;
            r[at + 7] = 255;
            r[at + 23] = 64 + a as u8 * 16;
            r[at + 24] = 64;
            for n in 0..4 {
                let p = 488 + (a * 4 + n) * 28;
                r[p] = 76 + DELTAS[n].1 as u8;
                r[p + 1] = 72 + a as u8 * 16 + DELTAS[n].0 as u8;
                r[p + 2] = n as u8;
                r[p + 3] = a as u8;
                // Include a fully transparent quadrant rather than omit it.
                if n != 3 {
                    r[p + 4..p + 20].fill((a * 4 + n + 1) as u8);
                }
                r[p + 20..p + 28].copy_from_slice(&[0, 0, 1, 2, 3, 4, 5, 6]);
            }
        }
        r
    }
    #[test]
    fn sha256_independent_vectors() {
        assert_eq!(
            sha256(b""),
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        );
        assert_eq!(
            sha256(b"abc"),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );
        assert_eq!(
            sha256(&vec![b'a'; 1000]),
            "41edece42d63e8d9bf515a9ba6932e1c20cbc9f5a5d134645adb5db1b9737ea3"
        );
    }
    #[test]
    fn exact_parts_transparency_order_flags_and_selectors() {
        let rows = normalize(0, &fixture()).unwrap();
        let a = &rows[2];
        assert_eq!(a.sprite(), 199);
        assert_eq!(a.canonical_palette(), 2);
        assert_eq!(a.original().map(|p| p.oam_index), [31, 30, 29, 28]);
        assert_eq!(a.parts()[3].tile16, [0; 16]);
        assert!(a.matches(a.key(), a.parts()));
        for n in 0..4 {
            let mut relocation = *a.parts();
            relocation[n].flags |= 15;
            assert!(a.matches(a.key(), &relocation));
            for bit in [0x10, 0x20, 0x40, 0x80] {
                let mut v = *a.parts();
                v[n].flags ^= bit;
                assert!(!a.matches(a.key(), &v));
            }
            for byte in 0..16 {
                let mut v = *a.parts();
                v[n].tile16[byte] ^= 1;
                assert!(!a.matches(a.key(), &v));
            }
            for byte in 0..8 {
                let mut v = *a.parts();
                v[n].rgb8[byte] ^= 1;
                assert!(!a.matches(a.key(), &v));
            }
        }
        assert!(!a.matches(a.key(), &a.parts()[..3]));
        let mut v = *a.parts();
        v[3] = v[2];
        assert!(!a.matches(a.key(), &v));
        let mut v = *a.parts();
        v.swap(0, 1);
        assert!(!a.matches(a.key(), &v));
        for (role, avatar) in [(1, 2), (0, 1)] {
            assert!(!a.matches(
                Key::stationary(role, avatar, ReceiverLighting::GameCornerDay12).unwrap(),
                a.parts()
            ));
        }
        for (role, avatar) in [(4, 0), (0, 4), (255, 255)] {
            assert_eq!(
                Key::stationary(role, avatar, ReceiverLighting::GameCornerDay12),
                Err(Error::Key)
            );
        }
    }
    #[test]
    fn finite_schema_rejects_unsupported_and_ambiguous_figures() {
        let r = fixture();
        for at in [
            44,
            44 + 33,
            44 + 7,
            44 + 13,
            44 + 25,
            44 + 26,
            488,
            489,
            988,
            993,
            1006,
        ] {
            let mut v = r.clone();
            v[at] ^= if at == 993 { 4 } else { 1 };
            assert!(normalize(0, &v).is_err(), "offset {at}");
        }
        for (at, bit) in [
            (44 + 6, 16),
            (44 + 5, 16),
            (44 + 5, 8),
            (44 + 4, 1),
            (44 + 4 * 34, 1),
        ] {
            let mut v = r.clone();
            v[at] ^= bit;
            assert!(normalize(0, &v).is_err());
        }
        let mut v = r.clone();
        v[516..544].copy_from_slice(&r[488..516]);
        assert_eq!(normalize(0, &v).unwrap_err(), Error::Geometry);
        for n in [0, 1015, 1017] {
            assert_eq!(normalize(0, &vec![0; n]).unwrap_err(), Error::Length);
        }
        assert_eq!(normalize(4, &r).unwrap_err(), Error::Key);
        // Neither structurally plausible synthetic bytes nor an arbitrary manifest seal.
        assert_eq!(
            SealedReferences::from_114(1, b"", [&r; 4]).unwrap_err(),
            Error::Version
        );
        assert_eq!(
            SealedReferences::from_114(2, b"{}", [&r; 4]).unwrap_err(),
            Error::Manifest
        );
    }
    #[test]
    fn signed_modulo_and_wrapped_original_base() {
        for base in 0..=255u8 {
            for value in 0..=255u8 {
                let expected =
                    ((i16::from(value) - i16::from(base) + 128).rem_euclid(256) - 128) as i8;
                assert_eq!(relative(value, base), expected);
            }
        }
        // Native SX + BX wraps before forming the same visible base X72.
        let mut r = fixture();
        for a in 0..4 {
            r[44 + a * 34 + 23] = r[44 + a * 34 + 23].wrapping_sub(70);
        }
        r[44 + 442] = 70;
        let a = normalize(0, &r).unwrap();
        assert_eq!(a[0].parts()[0].dx, 0);
        // A wrapped raw OAM constituent is still clipped and unsupported.
        r[489] = 0;
        assert_eq!(normalize(0, &r).unwrap_err(), Error::Geometry);
    }
}
