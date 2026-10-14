// Static complete-figure matching against a sealed disabled-native dictionary.
// This module never derives expected appearance from enabled OAM or feature RAM.
use std::io::Read;
pub const MAX_PAYLOAD: usize = 65536;
pub const MAGIC: u32 = 0x31544e49; // INT1
#[derive(Clone, Debug)]
pub struct Part {
    pub y: i16,
    pub x: i16,
    pub flags: u8,
    pub tile: [u8; 16],
    pub rgb: [u8; 8],
}
#[derive(Clone, Debug)]
pub struct Figure {
    pub lifetime: u64,
    pub id: u16,
    pub generation: u16,
    // Sealed geometric constituent order, independent of physical OAM placement.
    pub parts: [Part; 4],
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Failure {
    Format,
    Pair,
    Crc,
    Identity,
    MissingPart { figure: usize, part: usize, row: usize },
    RowLimit { figure: usize, part: usize, row: usize },
    Order { figure: usize, row: usize },
    UnknownObject { object: usize, row: usize },
}
fn word(p: &[u8], at: usize) -> u32 {
    u32::from_le_bytes(p[at..at + 4].try_into().unwrap())
}
pub fn crc32(p: &[u8]) -> u32 {
    let mut c = !0u32;
    for &v in p {
        c ^= u32::from(v);
        for _ in 0..8 { c = (c >> 1) ^ (0xedb88320 & 0u32.wrapping_sub(c & 1)); }
    }
    !c
}
// Payload starts with actual display/raster IDs. Dictionary authentication and
// source/input definitions are a separate prerequisite, not asserted by framing.
pub fn read_pair(input: &mut impl Read, ordinal: u32, display: u32, raster: u32)
    -> Result<Vec<u8>, Failure> {
    let mut h = [0; 20];
    input.read_exact(&mut h).map_err(|_| Failure::Format)?;
    if word(&h, 0) != MAGIC || word(&h, 4) != 1 || word(&h, 8) != ordinal {
        return Err(Failure::Pair);
    }
    let n = word(&h, 12) as usize;
    if !(8..=MAX_PAYLOAD).contains(&n) { return Err(Failure::Format); }
    let mut payload = vec![0; n];
    input.read_exact(&mut payload).map_err(|_| Failure::Format)?;
    if crc32(&payload) != word(&h, 16) { return Err(Failure::Crc); }
    if word(&payload, 0) != display || word(&payload, 4) != raster { return Err(Failure::Pair); }
    Ok(payload)
}
pub fn identities(figures: &[Figure]) -> Result<(), Failure> {
    if figures.is_empty() || figures.len() > 4 { return Err(Failure::Identity); }
    for (i, f) in figures.iter().enumerate() {
        if f.lifetime == 0 || figures[..i].iter().any(|g|
            (f.lifetime, f.id, f.generation) == (g.lifetime, g.id, g.generation)) {
            return Err(Failure::Identity);
        }
    }
    Ok(())
}
// Check every constituent even when its tile is fully transparent or hidden by
// the BG. Hidden-X blockers consume row slots exactly as they do on hardware.
pub fn check_row(h: &[u8], figures: &[Figure], row: usize) -> Result<(), Failure> {
    if h.len() != 16966 || row >= 144 || h[16800] & 0x86 != 0x82 {
        return Err(Failure::Format); // enabled height8 OBJ mode only
    }
    identities(figures)?;
    let selected: Vec<usize> = (0..40).filter(|q| {
        let y = i32::from(h[q * 4]) - 16;
        (y..y + 8).contains(&(row as i32))
    }).take(10).collect();
    let mut used = [false; 40];
    for (fi, f) in figures.iter().enumerate() {
        for (pi, part) in f.parts.iter().enumerate() {
            if !(0..=255).contains(&part.y) || !(0..=255).contains(&part.x) || part.flags & 0x0f != 0 {
                return Err(Failure::Format);
            }
            if !(i32::from(part.y) - 16..i32::from(part.y) - 8).contains(&(row as i32)) { continue; }
            let q = (0..40).find(|&q| {
                let at = q * 4;
                let flags = h[at + 3];
                let tile = 160 + usize::from((flags >> 3) & 1) * 0x2000 + usize::from(h[at + 2]) * 16;
                let rgb = 16544 + (8 + usize::from(flags & 7)) * 8;
                !used[q] && i16::from(h[at]) == part.y && i16::from(h[at + 1]) == part.x &&
                    flags & 0xf0 == part.flags && h[tile..tile + 16] == part.tile &&
                    h[rgb..rgb + 8] == part.rgb
            }).ok_or(Failure::MissingPart { figure: fi, part: pi, row })?;
            if !selected.contains(&q) { return Err(Failure::RowLimit { figure: fi, part: pi, row }); }
            // Sealed114 parts are disjoint 8x8 quadrants. Native InitSprite
            // emits them at descending indices, while the private peer emitter
            // increments its output pointer. Either placement has the same
            // intra-figure pixels once every constituent survives the first ten.
            // The finite intent has no priority input for overlapping parts:
            // reject that unsupported case rather than infer it from actual OAM.
            if f.parts[..pi].iter().any(|other| {
                (i32::from(other.y) - 16..i32::from(other.y) - 8).contains(&(row as i32)) &&
                    i32::from(other.x) < i32::from(part.x) + 8 &&
                    i32::from(part.x) < i32::from(other.x) + 8
            }) { return Err(Failure::Order { figure: fi, row }); }
            used[q] = true;
        }
    }
    // Static EMPTY allows exactly the declared avatars. An unknown active tuple
    // cannot silently supply a missing quadrant or introduce unaccounted pressure.
    for &q in &selected {
        if h[q * 4 + 1] > 0 && h[q * 4 + 1] < 168 && !used[q] {
            return Err(Failure::UnknownObject { object: q, row });
        }
    }
    Ok(())
}
#[cfg(test)]
mod tests {
    use super::*;
    fn fixture() -> (Vec<u8>, Vec<Figure>) {
        let mut h = vec![0; 16966]; h[16800] = 0x82;
        let parts = std::array::from_fn(|p| {
            let y = 40 + (p / 2) as i16 * 8;
            let x = 40 + (p % 2) as i16 * 8;
            let q = 39 - p;
            h[q * 4] = y as u8; h[q * 4 + 1] = x as u8; h[q * 4 + 2] = p as u8;
            let tile = [p as u8; 16]; h[160 + p * 16..176 + p * 16].copy_from_slice(&tile);
            Part { y, x, flags: 0, tile, rgb: [0; 8] }
        });
        (h, vec![Figure { lifetime: 1, id: 0, generation: 1, parts }])
    }
    #[test] fn complete_and_repeat() {
        let (h, f) = fixture();
        for _ in 0..2 { for row in 24..40 { assert_eq!(check_row(&h, &f, row), Ok(())); } }
    }
    #[test] fn wrong_tile_rgb_flip_and_transparent_omission() {
        let (h, f) = fixture();
        for (at, value) in [(160, 1), (16608, 1), (159, 32), (156, 0)] {
            let mut bad = h.clone(); bad[at] = value;
            assert!(matches!(check_row(&bad, &f, 24), Err(Failure::MissingPart { .. })));
        }
    }
    #[test] fn relocation_with_exact_bytes_passes() {
        let (mut h, f) = fixture();
        h[158] = 20; h[159] = 11;
        h[160 + 0x2000 + 20 * 16..176 + 0x2000 + 20 * 16].copy_from_slice(&f[0].parts[0].tile);
        assert_eq!(check_row(&h, &f, 24), Ok(()));
    }
    #[test] fn hidden_x_blockers_fail_survival() {
        let (mut h, f) = fixture();
        for q in 0..10 { h[q * 4] = 40; h[q * 4 + 1] = 0; }
        assert!(matches!(check_row(&h, &f, 24), Err(Failure::RowLimit { .. })));
    }
    #[test] fn disjoint_parts_allow_native_and_private_oam_placement() {
        let (h, f) = fixture();
        let mut ascending = h.clone();
        ascending[..160].fill(0);
        for p in 0..4 { ascending[p * 4..p * 4 + 4].copy_from_slice(&h[(39 - p) * 4..(40 - p) * 4]); }
        for row in 24..40 { assert_eq!(check_row(&ascending, &f, row), Ok(())); }
        for (a, b, row) in [(39, 38, 24), (37, 36, 32)] {
            let mut swapped = h.clone();
            for n in 0..4 { swapped.swap(a * 4 + n, b * 4 + n); }
            assert_eq!(check_row(&swapped, &f, row), Ok(()));
        }
    }
    #[test] fn overlapping_parts_require_independent_priority_intent() {
        let (mut h, mut f) = fixture();
        f[0].parts[1].x = 44; h[38 * 4 + 1] = 44;
        assert_eq!(check_row(&h, &f, 24), Err(Failure::Order { figure: 0, row: 24 }));
    }
    #[test] fn unknown_object_and_each_partial_figure_fail() {
        let (h, f) = fixture();
        let mut unknown = h.clone(); unknown[0] = 40; unknown[1] = 80;
        assert_eq!(check_row(&unknown, &f, 24), Err(Failure::UnknownObject { object: 0, row: 24 }));
        for part in 0..4 {
            let mut partial = h.clone(); partial[(39 - part) * 4] = 0;
            let row = 24 + (part / 2) * 8;
            assert_eq!(check_row(&partial, &f, row), Err(Failure::MissingPart { figure: 0, part, row }));
        }
    }
    #[test] fn duplicate_identity_fails_even_with_distinct_positions() {
        let (_, mut f) = fixture(); f.push(f[0].clone()); assert_eq!(identities(&f), Err(Failure::Identity));
    }
    #[test] fn strict_pair_crc_size_ordinal_and_binding() {
        assert_eq!(crc32(b"123456789"), 0xcbf43926);
        let payload = [7u32.to_le_bytes(), 9u32.to_le_bytes()].concat();
        let mut record: Vec<u8> = [MAGIC, 1, 1, 8, crc32(&payload)].into_iter().flat_map(u32::to_le_bytes).collect();
        record.extend(&payload);
        assert_eq!(read_pair(&mut &record[..], 1, 7, 9), Ok(payload));
        assert_eq!(read_pair(&mut &record[..], 2, 7, 9), Err(Failure::Pair));
        assert_eq!(read_pair(&mut &record[..], 1, 8, 9), Err(Failure::Pair));
        record[20] ^= 1; assert_eq!(read_pair(&mut &record[..], 1, 7, 9), Err(Failure::Crc));
        record[12..16].copy_from_slice(&65537u32.to_le_bytes());
        assert_eq!(read_pair(&mut &record[..], 1, 7, 9), Err(Failure::Format));
    }
}
