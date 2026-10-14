// Exact physical PPU decoder shared by Head semantic checks and OUTF consumer.
// Physical equality alone does not prove owner intent, eligibility or cadence.
pub fn effect_logical_color(raw: u16, display: &[u64]) -> u16 {
    assert_eq!(
        display[17], 1,
        "Missing actual palette conversion observation"
    );
    assert_eq!(
        display[18], 11,
        "Only actual GBC RGB565 correction without GBA is supported"
    );
    assert_eq!(
        display[20], 0,
        "Non-neutral actual light temperature is unsupported"
    );
    assert_eq!(
        display[21], 0,
        "Changed cached palette configuration is unsupported"
    );
    let r = u32::from(raw & 31);
    let g = u32::from((raw >> 5) & 31);
    let b = u32::from((raw >> 10) & 31);
    let (r, g, b) = match display[19] {
        1 => (
            ((r * 13 + g * 2 + b) >> 4),
            ((g * 3 + b) >> 2),
            ((r * 3 + g * 2 + b * 11) >> 4),
        ),
        2 => {
            let inv = 1.0f32 / 31.0f32;
            let gamma = 2.2f32 - 0.5f32;
            let rr = (r as f32 * inv).powf(gamma);
            let gg = (g as f32 * inv).powf(gamma);
            let bb = (b as f32 * inv).powf(gamma);
            let channel = |v: f32| -> u32 {
                ((v.clamp(0.0, 1.0).powf(1.0f32 / 2.2f32).clamp(0.0, 1.0) * 31.0 + 0.5) as u32) & 31
            };
            (
                channel(0.94f32 * ((0.82f32 * rr) + (0.24f32 * gg) + (-0.06f32 * bb))),
                channel(0.94f32 * ((0.125f32 * rr) + (0.665f32 * gg) + (0.21f32 * bb))),
                channel(0.94f32 * ((0.195f32 * rr) + (0.075f32 * gg) + (0.73f32 * bb))),
            )
        }
        _ => panic!("Unsupported actual palette correction mode"),
    };
    ((r << 11) | (g << 6) | b) as u16
}
pub fn effect_physical_color(logical: u16, y: usize, display: &[u64]) -> u16 {
    if display[9] == 0 || y % 2 != display[12] as usize {
        return logical;
    }
    let factor = f32::from_bits(u32::try_from(display[11]).unwrap());
    let channel = |v: u16, max: u16| ((v as f32 * factor) as u16).min(max);
    (channel((logical >> 11) & 31, 31) << 11)
        | (channel((logical >> 5) & 63, 63) << 5)
        | channel(logical & 31, 31)
}
pub fn effect_tile_pixel(vram: &[u8], tile: usize, bank: usize, x: usize, y: usize) -> u8 {
    let at = bank * 0x2000 + tile * 16 + y * 2;
    ((vram[at] >> (7 - x)) & 1) | (((vram[at + 1] >> (7 - x)) & 1) << 1)
}
pub fn effect_obj_pixel(oam: &[u8], vram: &[u8], q: usize, x: i32, y: i32, height: i32) -> u8 {
    let mut dx = x - (i32::from(oam[q * 4 + 1]) - 8);
    let mut dy = y - (i32::from(oam[q * 4]) - 16);
    if !(0..8).contains(&dx) || !(0..height).contains(&dy) {
        return 0;
    }
    let flags = oam[q * 4 + 3];
    if flags & 32 != 0 {
        dx = 7 - dx;
    }
    if flags & 64 != 0 {
        dy = height - 1 - dy;
    }
    let tile = if height == 16 {
        (oam[q * 4 + 2] & 254) as usize + dy as usize / 8
    } else {
        oam[q * 4 + 2] as usize
    };
    effect_tile_pixel(
        vram,
        tile,
        ((flags >> 3) & 1) as usize,
        dx as usize,
        dy as usize % 8,
    )
}
#[derive(Debug, Clone, Copy)]
pub struct Pixel {
    pub physical: u16,
    pub object: Option<usize>,
}
// window_line is raw row[8], layers is raw row[7] (translated Head meta[13]).
// The object result is only the final winner after BG/window priority.
pub fn pixel(h: &[u8], window_line: u64, layers: u64, display: &[u64], x: i32, y: i32) -> Pixel {
    assert_eq!(h.len(), 16966);
    let oam = &h[..160];
    let vram = &h[160..16544];
    let mapped = &h[16672..16800];
    let r = &h[16800..16806];
    let height = if r[0] & 4 != 0 { 16 } else { 8 };
    // Hidden-X objects consume the actual first-ten scanline list.
    let mut slots = [0usize; 10];
    let mut count = 0;
    for q in 0..40 {
        let top = i32::from(oam[q * 4]) - 16;
        if (top..top + height).contains(&y) {
            slots[count] = q;
            count += 1;
            if count == 10 {
                break;
            }
        }
    }
    let selected = &slots[..count];
    let winner = selected
        .iter()
        .find(|q| effect_obj_pixel(oam, vram, **q, x, y, height) != 0);
    assert_eq!(layers, 7, "Full rendered row requires all LCD layers");
    assert_ne!(r[0] & 128, 0, "Captured native LCD must be enabled");
    let window =
        r[0] & 32 != 0 && i32::from(r[3]) < y + 1 && r[4] <= 166 && x >= i32::from(r[4]) - 7;
    let (bx, by, base, map_row) = if window {
        let entry = i32::try_from(window_line).expect("Actual window counter outside signed range");
        let wy = entry.checked_sub(1).expect("Actual window row underflow");
        // Pinned lcd::win_render_color starts its map pointer
        // one tile row before y>>3. Reset entry0 means y=-1;
        // clear_win_count entry9 means y=8, map row0.
        (
            (x - (i32::from(r[4]) - 7)) as usize,
            (wy & 7) as usize,
            if r[0] & 64 != 0 { 0x1c00i64 } else { 0x1800i64 },
            i64::from((wy >> 3) - 1),
        )
    } else {
        let by = ((y + i32::from(r[1])) & 255) as usize;
        (
            ((x + i32::from(r[2])) & 255) as usize,
            by,
            if r[0] & 8 != 0 { 0x1c00i64 } else { 0x1800i64 },
            (by / 8) as i64,
        )
    };
    let address = base + map_row * 32 + (bx / 8) as i64;
    assert!(
        (0..0x2000).contains(&address),
        "Actual window/BG map address outside captured VRAM bank"
    );
    let at = address as usize;
    let attr = vram[0x2000 + at];
    let tile = if r[0] & 16 != 0 {
        vram[at] as usize
    } else {
        (256i32 + i32::from(vram[at] as i8)) as usize
    };
    let px = if attr & 32 != 0 { 7 - bx % 8 } else { bx % 8 };
    let py = if attr & 64 != 0 { 7 - by % 8 } else { by % 8 };
    let bg = effect_tile_pixel(vram, tile, ((attr >> 3) & 1) as usize, px, py);
    let mut palette = usize::from(attr & 7);
    let mut color = usize::from(bg);
    let mut object = None;
    if r[0] & 2 != 0 {
        if let Some(&q) = winner {
            let flags = oam[q * 4 + 3];
            if r[0] & 1 == 0 || bg == 0 || (attr & 128 == 0 && flags & 128 == 0) {
                palette = 8 + usize::from(flags & 7);
                color = usize::from(effect_obj_pixel(oam, vram, q, x, y, height));
                object = Some(q);
            }
        }
    }
    let at = (palette * 4 + color) * 2;
    let expected = u16::from_le_bytes([mapped[at], mapped[at + 1]]);
    let raw = u16::from_le_bytes([h[16544 + at], h[16544 + at + 1]]);
    assert_eq!(
        expected,
        effect_logical_color(raw, &display),
        "Raw selected logical color at {x},{y}"
    );
    let expected = effect_physical_color(expected, y as usize, &display);
    Pixel {
        physical: expected,
        object,
    }
}
pub fn apply_changes(hardware: &mut [u8], changes: &[(usize, u8)], end: usize, cursor: &mut usize) {
    apply_changes_by(hardware, changes.len(), end, cursor, |i| changes[i]);
}
pub fn apply_changes_by(
    hardware: &mut [u8],
    count: usize,
    end: usize,
    cursor: &mut usize,
    change: impl Fn(usize) -> (usize, u8),
) {
    assert!(end >= *cursor && end <= count, "Row change order/bounds");
    let mut previous = None;
    while *cursor < end {
        let (at, value) = change(*cursor);
        assert!(at < hardware.len());
        assert!(
            previous.map_or(true, |p| at > p),
            "Duplicate/unordered row byte mutation"
        );
        assert_ne!(
            hardware[at], value,
            "Sparse change must change actual row hardware"
        );
        hardware[at] = value;
        previous = Some(at);
        *cursor += 1;
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    fn display() -> Vec<u64> {
        vec![
            1, 9, 7, 12, 144, 160, 144, 16, 320, 1, 1, 1065520988, 0, 1, 0, 1, 0, 1, 11, 2, 0, 0,
            0, 24,
        ]
    }
    fn fixture() -> (Vec<u8>, Vec<u64>) {
        let mut h = vec![0; 16966];
        h[16800] = 0x93;
        for palette in 0..16 {
            let at = palette * 8 + 2;
            h[16544 + at..16544 + at + 2].copy_from_slice(&32767u16.to_le_bytes());
            h[16672 + at..16672 + at + 2].copy_from_slice(&65503u16.to_le_bytes());
        }
        let mut d = display();
        d[9] = 0;
        d[19] = 1;
        (h, d)
    }
    fn white_tile(h: &mut [u8], bank: usize, tile: usize) {
        for y in 0..8 {
            h[160 + bank * 8192 + tile * 16 + y * 2] = 255;
        }
    }
    #[test]
    fn independent_raw_mapped_physical_vectors() {
        let d = display();
        assert_eq!(effect_logical_color(11254, &d), 52945);
        assert_eq!(effect_physical_color(52945, 0, &d), 52977); // Actual fast1.02 green54 ->55.
        assert_eq!(effect_physical_color(52945, 1, &d), 52945);
        let mut other = d.clone();
        other[12] = 1;
        assert_eq!(effect_physical_color(52945, 0, &other), 52945);
        other[11] = 1f32.to_bits().into();
        assert_eq!(effect_physical_color(52945, 1, &other), 52945);
        other[19] = 1;
        assert_eq!(effect_logical_color(31, &other), 51205);
    }
    #[test]
    fn background_window_reset_signed_address_banks_flips() {
        let (mut h, d) = fixture();
        white_tile(&mut h, 1, 1);
        h[160 + 0x1800] = 1;
        h[160 + 0x3800] = 8 | 32 | 64;
        assert_eq!(pixel(&h, 9, 7, &d, 0, 0).physical, 65503);
        h[16800] |= 32;
        h[16804] = 7;
        h[160 + 0x17c0 + 31] = 1;
        h[160 + 0x37c0 + 31] = 8;
        // Reset window entry0 uses map row -2, tile row7.
        h[160 + 0x17c0] = 1;
        h[160 + 0x37c0] = 8;
        assert_eq!(pixel(&h, 0, 7, &d, 0, 0).physical, 65503);
        assert_eq!(pixel(&h, 9, 7, &d, 0, 0).physical, 65503); // entry9 uses map row0.
    }
    #[test]
    fn object_priority_transparency_first_ten_and_height() {
        let (mut h, d) = fixture();
        white_tile(&mut h, 0, 1);
        h[..4].copy_from_slice(&[16, 8, 1, 0]);
        assert_eq!(pixel(&h, 9, 7, &d, 0, 0).object, Some(0));
        h[160 + 16] = 0;
        assert_eq!(pixel(&h, 9, 7, &d, 0, 0).object, None); // transparent
        white_tile(&mut h, 0, 1);
        h[160 + 0x1800] = 1;
        h[3] = 128;
        assert_eq!(pixel(&h, 9, 7, &d, 0, 0).object, None);
        h[16800] &= !1;
        assert_eq!(pixel(&h, 9, 7, &d, 0, 0).object, Some(0));
        h[16800] |= 1;
        h[3] = 0;
        h[160 + 0x3800] = 128;
        assert_eq!(pixel(&h, 9, 7, &d, 0, 0).object, None);
        h[160 + 0x1800] = 0;
        h[160 + 0x3800] = 0;
        for q in 0..10 {
            h[q * 4..q * 4 + 4].copy_from_slice(&[16, 0, 1, 0]);
        }
        h[40..44].copy_from_slice(&[16, 8, 1, 0]);
        assert_eq!(pixel(&h, 9, 7, &d, 0, 0).object, None);
        h[..160].fill(0);
        h[..4].copy_from_slice(&[16, 8, 1, 0]);
        assert_eq!(pixel(&h, 9, 7, &d, 0, 8).object, None);
        h[16800] |= 4;
        assert_eq!(pixel(&h, 9, 7, &d, 0, 8).object, Some(0));
    }
    #[test]
    fn asymmetric_bank_flip_and_signed_tile_addressing() {
        let (mut h,d)=fixture();
        h[160+8192+16+14]=1; // bank1 tile1 bottom-right only.
        h[160+0x1800]=1; h[160+0x3800]=8|32|64;
        assert_eq!(pixel(&h,9,7,&d,0,0).physical,65503);
        assert_eq!(pixel(&h,9,7,&d,1,0).physical,0);
        h[16800]&=!16; h[160+0x1800]=255; // signed -1 -> tile255.
        h[160+8192+255*16+14]=1;
        assert_eq!(pixel(&h,9,7,&d,0,0).physical,65503);
        h[16800]|=16; h[160+0x1800]=0;h[160+0x3800]=0;
        h[..4].copy_from_slice(&[16,8,1,8|32|64]);
        assert_eq!(pixel(&h,9,7,&d,0,0).object,Some(0));
        assert_eq!(pixel(&h,9,7,&d,1,0).object,None);
    }
    #[test]
    fn sparse_changes_strict() {
        let mut h = [0; 8];
        let mut c = 0;
        apply_changes(&mut h, &[(1, 2), (4, 3)], 2, &mut c);
        assert_eq!(c, 2);
        assert_eq!(h[4], 3);
        for changes in [
            vec![(1, 2), (1, 3)],
            vec![(2, 2), (1, 3)],
            vec![(1, 0)],
            vec![(8, 1)],
        ] {
            assert!(std::panic::catch_unwind(|| {
                let mut h = [0; 8];
                apply_changes(&mut h, &changes, changes.len(), &mut 0);
            })
            .is_err());
        }
    }
    #[test]
    fn strict_unsupported_and_intent_boundary() {
        let (mut h, d) = fixture();
        assert_eq!(pixel(&h, 9, 7, &d, 0, 0).physical, 0);
        // A different, internally consistent physical picture is accepted.
        // Owner glyph/palette/eligibility/temporal intent must be checked elsewhere.
        white_tile(&mut h, 0, 1);
        h[160 + 0x1800] = 1;
        assert_eq!(pixel(&h, 9, 7, &d, 0, 0).physical, 65503);
        for mutate in 0..3 {
            let mut bad = h.clone();
            if mutate == 0 {
                bad[16674] ^= 1;
            } else if mutate == 1 {
                bad[16546] ^= 1;
            } else {
                bad[16800] &= 127;
            }
            assert!(std::panic::catch_unwind(|| pixel(&bad, 9, 7, &d, 0, 0)).is_err());
        }
        let mut window = h.clone();
        window[16800] |= 32;
        window[16804] = 7;
        assert!(std::panic::catch_unwind(|| pixel(&window, i32::MAX as u64, 7, &d, 0, 0)).is_err());
    }
}
