// Bounded OUTF v1 physical-picture consumer. No owner intent/cadence inference.
mod actual_picture;
use std::io::{self, Read};
const HARDWARE: usize = 16966;
const ROWS: usize = 144 * 16 * 4;
const FIXED: usize = 160 + HARDWARE + ROWS + 46080;
fn word(p: &[u8], at: usize) -> u32 {
    u32::from_le_bytes(p[at..at + 4].try_into().unwrap())
}
fn read(input: &mut impl Read, count: usize) -> Vec<u8> {
    let mut p = vec![0; count];
    input.read_exact(&mut p).expect("truncated record/footer");
    p
}
fn crc32(p: &[u8]) -> u32 {
    let mut c = !0u32;
    for &v in p {
        c ^= u32::from(v);
        for _ in 0..8 {
            c = (c >> 1) ^ (0xedb88320u32 & 0u32.wrapping_sub(c & 1));
        }
    }
    !c
}
fn display(d: &[u64], m: &[u64]) {
    assert_eq!(d.len(), 24);
    assert_eq!(d[0], 1);
    assert_eq!(d[23], 24);
    assert!(d[1] > 0);
    assert_eq!(d[2], m[1]);
    assert_eq!(d[3], m[2]);
    assert_eq!(d[4], 144);
    assert_eq!(&d[5..9], &[160, 144, 16, 320]);
    assert!(d[9] <= 1 && d[10] <= 1 && d[12] <= 1);
    assert_eq!(&d[13..17], &[1, 0, 1, 0]);
    assert_eq!(d[22], 0);
    let f = f32::from_bits(u32::try_from(d[11]).unwrap());
    assert!(f.is_finite() && (1.0..=2.0).contains(&f));
    if d[9] != 0 {
        assert_eq!(d[10], 1);
    }
    actual_picture::effect_logical_color(0, d);
}
fn consume(input: &mut impl Read, expected: u32) {
    assert!((1..=800).contains(&expected));
    let (mut prior_display, mut prior_render) = (0, 0);
    for index in 1..=expected {
        let head = read(input, 20);
        assert_eq!(word(&head, 0), 0x4654554f);
        assert_eq!(word(&head, 4), 1);
        assert_eq!(word(&head, 8), index);
        let size = word(&head, 12) as usize;
        assert!((FIXED..=596710).contains(&size));
        let p = read(input, size);
        assert_eq!(crc32(&p), word(&head, 16), "CRC mismatch");
        let d: Vec<u64> = (0..24).map(|i| u64::from(word(&p, 4 + i * 4))).collect();
        let m: Vec<u64> = (0..14).map(|i| u64::from(word(&p, 100 + i * 4))).collect();
        let changes = word(&p, 156) as usize;
        assert!(changes <= 65536);
        assert_eq!(size, FIXED + changes * 8);
        display(&d, &m);
        assert!(m[1] > 0);
        assert_eq!(m[0], 1);
        assert_eq!(word(&p, 96), 24);
        assert_eq!(m[5], 144);
        assert_eq!(m[6], 0);
        assert_eq!(m[7], 143);
        if prior_display != 0 {
            assert_eq!(d[1], prior_display + 1);
            assert_eq!(m[1], prior_render + 1);
        }
        prior_display = d[1];
        prior_render = m[1];
        let mut h = p[160..160 + HARDWARE].to_vec();
        let delta = 160 + HARDWARE + ROWS;
        let changes: Vec<_> = (0..changes)
            .map(|i| {
                (
                    word(&p, delta + i * 8) as usize,
                    u8::try_from(word(&p, delta + i * 8 + 4)).unwrap(),
                )
            })
            .collect();
        let mut cursor = 0;
        let pixel_base = delta + changes.len() * 8;
        for y in 0..144 {
            let at = 160 + HARDWARE + y * 64;
            let row: Vec<u64> = (0..16).map(|i| u64::from(word(&p, at + i * 4))).collect();
            assert!(
                row[8] <= i32::MAX as u64,
                "signed window counter unsupported"
            );
            actual_picture::apply_changes(&mut h, &changes, row[9] as usize, &mut cursor);
            assert_eq!(
                &h[..160],
                &h[16806..],
                "DMA source differs from rendered OAM"
            );
            assert!(row[4] > 0, "missing actual DMA provenance");
            let page = h[16805];
            let bank = if (0xd0..0xe0).contains(&page) {
                let raw = row[5] & 7;
                if raw == 0 {
                    1
                } else {
                    raw
                }
            } else {
                0
            };
            assert_eq!(row[6], bank, "actual DMA bank/page mismatch");
            for x in 0..160 {
                let v = actual_picture::pixel(&h, row[8], row[7], &d, x, y as i32);
                let at = pixel_base + (y * 160 + x as usize) * 2;
                let actual = u16::from_le_bytes([p[at], p[at + 1]]);
                assert_eq!(
                    actual, v.physical,
                    "physical mismatch output {index} at {x},{y}"
                );
            }
        }
        assert_eq!(cursor, changes.len());
        println!(
            "{index} {} {} {:08x} pixels=23040 physical=PASS intent=UNPROVEN",
            d[1],
            m[1],
            word(&head, 16)
        );
    }
    let end = read(input, 8);
    assert_eq!(word(&end, 0), 0x31444e45);
    assert_eq!(word(&end, 4), expected);
    let mut extra = [0];
    assert_eq!(input.read(&mut extra).unwrap(), 0);
}
fn main() {
    let expected = std::env::args()
        .nth(1)
        .unwrap_or("800".into())
        .parse()
        .unwrap();
    consume(&mut io::stdin().lock(), expected);
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn known_crc() {
        assert_eq!(crc32(b"123456789"), 0xcbf43926);
    }
}
