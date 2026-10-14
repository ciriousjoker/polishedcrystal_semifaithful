// Disabled native NRF1/OUTF reference consumer. Enabled intent remains unproven.
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
fn reference(input: &mut impl Read, index: u32) -> Vec<u8> {
    let head = read(input, 20);
    assert_eq!(word(&head, 0), 0x3146524e);
    assert_eq!(word(&head, 4), 2);
    assert_eq!(word(&head, 8), index);
    assert_eq!(word(&head, 12), 1016);
    let r = read(input, 1016);
    assert_eq!(crc32(&r), word(&head, 16));
    assert_eq!(word(&r, 0), index);
    let u64at = |at| u64::from_le_bytes(r[at..at+8].try_into().unwrap());
    assert!(u64at(12) > 0);
    assert!(u64at(20) < u64at(28) && u64at(28) < u64at(36));
    lighting(&r[44 + 892..]);
    r
}
// Exact layout mirrors the producer metadata, independently checked before adoption.
fn lighting(k: &[u8]) {
    assert_eq!(k.len(), 80);
    assert_eq!(&k[52..57], &[1, 1, 1, 12, 0], "receiver time/endpoint/fade changed");
    assert_eq!(k[57] & 4, 0, "forced daytime palette operand");
    assert_eq!(&k[58..70], &[0, 0, 0, 1, 0, 1, 0x55, 11, 23, 3, 255, 0],
        "receiver endpoint or original map metadata changed");
    assert!(k[70..].iter().all(|v| *v == 0), "nonzero reserved reference bytes");
}
fn parts(r: &[u8], h: &[u8], y: usize) {
    assert_eq!(h[16800] & 4, 0, "native reference requires height8");
    let mut owned = [false; 40];
    for actor in 0..4 {
        let mut prior = 40;
        for part in 0..4 {
            let at = 44 + 444 + (actor * 4 + part) * 28;
            let tuple = &r[at..at+4];
            let candidates: Vec<_> = (0..40).filter(|q| h[q*4..q*4+4] == *tuple).collect();
            assert_eq!(candidates.len(), 1, "missing/ambiguous original native constituent");
            let q = candidates[0];
            assert!(!owned[q]); owned[q] = true;
            assert!(q < prior, "original native tuple order changed"); prior = q;
            assert!((8..=160).contains(&tuple[1]) && (16..=152).contains(&tuple[0]), "reference figure clipped");
            let tile = 160 + ((tuple[3] >> 3) & 1) as usize * 8192 + tuple[2] as usize * 16;
            assert_eq!(&h[tile..tile+16], &r[at+4..at+20], "source-to-row native tile mismatch");
            let pal = 16544 + (8 + (tuple[3] & 7) as usize) * 8;
            assert_eq!(&h[pal..pal+8], &r[at+20..at+28], "source-to-row native RGB mismatch");
            let top = i32::from(tuple[0]) - 16;
            if (top..top+8).contains(&(y as i32)) {
                let selected: Vec<_> = (0..40).filter(|p| {
                    let t = i32::from(h[p*4])-16; (t..t+8).contains(&(y as i32))
                }).take(10).collect();
                assert!(selected.contains(&q), "native constituent lost in actual first-ten list");
            }
        }
    }
    for q in 0..40 {
        if !owned[q] {
            // ClearSprites uses Y0; _UpdateSprites.hide_unused_sprites writes
            // OAM_YCOORD_HIDDEN (160). At height8 both occupy no LCD row.
            // Do not filter by X: hidden-X objects can consume first-ten slots.
            assert!(matches!(h[q*4], 0 | 160), "unknown disabled native OAM constituent");
        }
    }
}
fn consume(input: &mut impl Read, expected: u32) {
    assert!((1..=800).contains(&expected));
    let (mut prior_display, mut prior_render) = (0, 0);
    let mut dictionary: Option<Vec<u8>> = None;
    for index in 1..=expected {
        let reference = reference(input, index);
        if let Some(ref first) = dictionary {
            // Retain raw flags as diagnostics; only bit2 selects LookupOBPalette's
            // lighting path and lighting() authenticates that bit on every pair.
            const FLAGS: usize = 44 + 892 + 57;
            assert_eq!(&reference[44..FLAGS], &first[44..FLAGS], "standing reference changed after first output");
            assert_eq!(&reference[FLAGS+1..], &first[FLAGS+1..], "standing reference changed after first output");
        } else { dictionary = Some(reference.clone()); }
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
        assert_eq!(word(&reference, 4) as u64, d[1]);
        assert_eq!(word(&reference, 8) as u64, d[2]);
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
            parts(&reference, &h, y);
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
            "{index} {} {} {:08x} pixels=23040 physical=PASS disabled_reference_binding=PASS enabled_intent=UNPROVEN",
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
    if let (Some(first), Some(path)) = (dictionary, std::env::args().nth(2)) {
        std::fs::write(path, &first).expect("cannot export disabled reference evidence");
    }
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

#[cfg(test)]
mod reference_tests {
    use super::*;
    fn fixture() -> (Vec<u8>, Vec<u8>) {
        let mut r = vec![0;1016]; let mut h=vec![0;HARDWARE];
        for a in 0..4 { for n in 0..4 {
            let q=39-a*4-n; let at=44+444+(a*4+n)*28;
            let tuple=[(32+a*24) as u8,(24+n*8) as u8,(a*4+n) as u8,a as u8];
            r[at..at+4].copy_from_slice(&tuple); h[q*4..q*4+4].copy_from_slice(&tuple);
            for i in 0..16 { r[at+4+i]=(i+1) as u8; h[160+(a*4+n)*16+i]=(i+1) as u8; }
            for i in 0..8 { r[at+20+i]=(i+a) as u8; h[16544+(8+a)*8+i]=(i+a) as u8; }
        }}
        (r,h)
    }
    #[test]
    fn exact_native_rows_and_mutations() {
        let (r,h)=fixture();
        for y in 0..144 { parts(&r,&h,y); }
        for at in [160+15*16,16544+11*8,39*4,39*4+3] {
            let mut changed=h.clone(); changed[at]^=1;
            assert!(std::panic::catch_unwind(||parts(&r,&changed,16)).is_err());
        }
        let mut hidden=h.clone(); hidden[0]=32;
        assert!(std::panic::catch_unwind(||parts(&r,&hidden,16)).is_err());
        let mut lost=h.clone(); lost[39*4]=0;
        assert!(std::panic::catch_unwind(||parts(&r,&lost,16)).is_err());
        let mut order=h.clone(); let left=order[39*4..40*4].to_vec(); let right=order[38*4..39*4].to_vec();
        order[39*4..40*4].copy_from_slice(&right); order[38*4..39*4].copy_from_slice(&left);
        assert!(std::panic::catch_unwind(||parts(&r,&order,16)).is_err());
    }
    #[test]
    fn original_unused_tail_and_occupied_row_counterexamples() {
        let (r,mut h)=fixture();
        for q in 0..24 { h[q*4]=160; }
        for y in 0..144 { parts(&r,&h,y); }
        // Exhaust all Y bytes and both invisible-X boundaries. Only the two
        // original inactive sentinels are admitted, even for other hidden Y.
        for x in [0, 168, 255] { for y in 0..=255u8 {
            if matches!(y, 0 | 160) { continue; }
            let mut changed=h.clone(); changed[0]=y; changed[1]=x;
            assert!(std::panic::catch_unwind(||parts(&r,&changed,143)).is_err());
        }}
        // Ten X0 blockers on an actor's row must still lose that constituent
        // in the actual first-ten list before any unknown-slot filtering.
        let mut blocked=h.clone();
        for q in 0..10 { blocked[q*4]=32; }
        let failure=std::panic::catch_unwind(||parts(&r,&blocked,16)).unwrap_err();
        let message=failure.downcast_ref::<&str>().copied()
            .or_else(||failure.downcast_ref::<String>().map(String::as_str)).unwrap();
        assert!(message.contains("native constituent lost in actual first-ten list"));
        let mut tall=h; tall[16800]|=4;
        assert!(std::panic::catch_unwind(||parts(&r,&tall,0)).is_err());
    }
    #[test]
    fn rejects_missing_truncated_and_wrong_pair_reference() {
        for bytes in [vec![],vec![0;20],vec![0;1000]] {
            assert!(std::panic::catch_unwind(||reference(&mut &bytes[..],1)).is_err());
        }
    }
}

#[cfg(test)]
mod lighting_tests {
    use super::*;
    #[test]
    fn rejects_each_wrong_initial_key_before_reference_return() {
        let mut r = vec![0u8;1016];
        r[0..4].copy_from_slice(&1u32.to_le_bytes());
        for (at,v) in [(12,1u64),(20,1),(28,2),(36,3)] { r[at..at+8].copy_from_slice(&v.to_le_bytes()); }
        let key=44+892;
        r[key+52..key+57].copy_from_slice(&[1,1,1,12,0]);
        r[key+58..key+70].copy_from_slice(&[0,0,0,1,0,1,0x55,11,23,3,255,0]);
        let framed = |r: &[u8], version:u32, length:u32| {
            let mut stream=Vec::new();
            for v in [0x3146524eu32,version,1,length,crc32(r)] { stream.extend(v.to_le_bytes()); }
            stream.extend(r); stream
        };
        reference(&mut &framed(&r,2,1016)[..],1);
        let mut scheduling=r.clone(); scheduling[key+57]=0xfb;
        reference(&mut &framed(&scheduling,2,1016)[..],1);
        for at in 52..70 {
            let mut wrong=r.clone(); wrong[key+at]^=if at==57 {4} else {1};
            assert!(std::panic::catch_unwind(||reference(&mut &framed(&wrong,2,1016)[..],1)).is_err());
        }
        for (version,length) in [(1,1000),(1,1016),(2,1000),(2,1017)] {
            assert!(std::panic::catch_unwind(||reference(&mut &framed(&r,version,length)[..],1)).is_err());
        }
    }
}
