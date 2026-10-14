//! Independent fixed raster fixtures execute the durable acceptance oracle.
use super::{headbutt_pixels, Value};
use serde_json::json;
fn event(sequence: usize, span: usize, source: &[u8]) -> Value {
    let mut fields = vec![0; 22];
    fields[0] = sequence; fields[1] = sequence; fields[2] = span; fields[3] = 2;
    json!({"fields": fields, "watches": {"0": source, "4": vec![0; 160], "1": [0], "2": [0], "5": [0]}})
}
fn rows(index: usize, sequence: usize, phase: usize) -> Vec<usize> {
    (0..144).flat_map(|_| [1,index,sequence,sequence,sequence,0,0,7,1,0,sequence,sequence,phase,0,0,0]).collect()
}
fn fixture() -> Value {
    let mut source = vec![0u8; 160];
    source[..8].copy_from_slice(&[32, 24, 16, 7, 32, 48, 16, 7]);
    let mut hardware = vec![0u8; 16966];
    hardware[..160].copy_from_slice(&source); hardware[16806..].copy_from_slice(&source);
    // Tile 16 is a solid color-1 square. Paint two independent 8x8 rectangles,
    // without calling any oracle pixel, priority or tile-decoding helper.
    for row in 0..8 { hardware[160 + 256 + row * 2] = 255; }
    hardware[16800..16806].copy_from_slice(&[0x93, 0, 0, 0, 0, 0xc0]);
    // Simple correction preserves grayscale RGB5(2,2,2): raw0x0842 -> RGB5650x1082.
    hardware[16544 + 15 * 8 + 2..16544 + 15 * 8 + 4].copy_from_slice(&[0x42, 0x08]);
    hardware[16672 + 15 * 8 + 2..16672 + 15 * 8 + 4].copy_from_slice(&[0x82, 0x10]);
    let mut pixels = vec![0u8; 160 * 144 * 2];
    for y in 16..24 { for x in (16..24).chain(40..48) {
        pixels[(y * 160 + x) * 2..(y * 160 + x) * 2 + 2].copy_from_slice(&[0x82, 0x10]);
    }}
    let video: Vec<_> = (1..=2).map(|step| json!({
        "render_metadata": [1, step, step, 1, step, 144, 0, 143, step, step, step, 0, 0, 7],
        "display_metadata": [1, step+1, step, step, 144, 160, 144, 16, 320, 0, 1, 1065353216u32, 0, 1, 0, 1, 0, 1, 11, 1, 0, 0, 0, 24],
        "hardware": hardware, "pixels": pixels, "rows": rows(step,step,1), "changes": [], "private_oam_page": 0xd8, "private_oam_bank": 7, "host_frame": step, "width": 160, "height": 144, "format": 2
    })).collect();
    let run = json!({"state": {"vAcceptanceHeadbuttTileStart": 0x8100, "vAcceptanceHeadbuttTileEnd": 0x8110},
        "profile": {"native_effect_event_overflow": 0, "native_effect_video_overflow": 0,
            "native_effect_events": [event(1, 5, &source), event(2, 5, &source)], "native_effect_video_frames": video}});
    json!({"clients": [{"mobile": run, "single": run}]})
}
fn rejects(e: &Value, expected: &str) {
    let panic = std::panic::catch_unwind(|| headbutt_pixels(e)).expect_err("Corrupted demand passed actual pixel oracle");
    let message = panic.downcast_ref::<String>().map(String::as_str)
        .or_else(|| panic.downcast_ref::<&str>().copied()).unwrap_or("");
    assert!(message.contains(expected), "Wrong rejection: {message}");
}
#[test]
fn headbutt_pixel_control_accepts_complete_native_demand() { headbutt_pixels(&fixture()); }
#[test]
fn headbutt_pixel_control_rejects_quad_missing_from_source_and_dma() {
    let mut e = fixture();
    for event in e["clients"][0]["mobile"]["profile"]["native_effect_events"].as_array_mut().unwrap() {
        for i in 4..8 { event["watches"]["0"][i] = json!(0); }
    }
    for frame in e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
        for i in 4..8 { frame["hardware"][i] = json!(0); frame["hardware"][16806 + i] = json!(0); }
        for y in 16..24 { for x in 40..48 {
            frame["pixels"][(y * 160 + x) * 2] = json!(0); frame["pixels"][(y * 160 + x) * 2 + 1] = json!(0);
        }}
    }
    rejects(&e, "Complete disabled native effect source");
}
#[test]
fn headbutt_pixel_control_rejects_changed_selector_with_self_consistent_colors() {
    let mut e = fixture();
    for event in e["clients"][0]["mobile"]["profile"]["native_effect_events"].as_array_mut().unwrap() {
        event["watches"]["0"][3] = json!(6);
    }
    for frame in e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
        frame["hardware"][3] = json!(6); frame["hardware"][16809] = json!(6);
        frame["hardware"][16672 + 14 * 8 + 2] = json!(0x78); frame["hardware"][16672 + 14 * 8 + 3] = json!(0x56);
        for y in 16..24 { for x in 16..24 {
            frame["pixels"][(y * 160 + x) * 2] = json!(0x78); frame["pixels"][(y * 160 + x) * 2 + 1] = json!(0x56);
        }}
    }
    rejects(&e, "Complete disabled native effect source");
}
#[test]
fn headbutt_pixel_control_rejects_intended_slot_rgb_changes() {
    let mut e = fixture();
    // Intend slot 6, while the former fixed slot 7 check remains unchanged.
    for name in ["mobile", "single"] {
        let run = &mut e["clients"][0][name];
        for event in run["profile"]["native_effect_events"].as_array_mut().unwrap() { event["watches"]["0"][3] = json!(6); }
        for frame in run["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
            frame["hardware"][3] = json!(6); frame["hardware"][16809] = json!(6);
            frame["hardware"][16544 + 14 * 8 + 2] = json!(0x42); frame["hardware"][16544 + 14 * 8 + 3] = json!(0x08);
            frame["hardware"][16672 + 14 * 8 + 2] = json!(0x82); frame["hardware"][16672 + 14 * 8 + 3] = json!(0x10);
        }
    }
    headbutt_pixels(&e);
    for frame in e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
        frame["hardware"][16544 + 14 * 8 + 2] = json!(1);
    }
    rejects(&e, "Actual intended OBJ palette RGB differs at slot 14");
}
#[test]
fn headbutt_pixel_control_allows_tile_reuse_only_after_actual_release_and_return() {
    let mut e = fixture(); let mut peer = vec![0u8; 160]; peer[..4].copy_from_slice(&[32, 24, 16, 6]);
    for name in ["mobile", "single"] {
        let run = &mut e["clients"][0][name];
        run["profile"]["native_effect_events"].as_array_mut().unwrap().extend([event(3, 0, &peer), event(4, 3, &peer), event(5, 5, &peer)]);
        let mut frame = run["profile"]["native_effect_video_frames"][1].clone();
        frame["render_metadata"] = json!([1, 3, 5, 1, 5, 144, 0, 143, 5, 5, 5, 0, 0, 7]);
        frame["rows"] = json!(rows(5,5,2));
        frame["display_metadata"][1]=json!(4); frame["display_metadata"][2]=json!(3); frame["display_metadata"][3]=json!(5);
        if name == "mobile" { frame["hardware"][16805] = json!(0xd8); for y in 0..144 {frame["rows"][y*16+5]=json!(7);frame["rows"][y*16+6]=json!(7);} }
        // Reused peer tile has palette6 and one actual rectangle.
        for y in 16..24 { for x in (16..24).chain(40..48) {frame["pixels"][(y*160+x)*2]=json!(0);frame["pixels"][(y*160+x)*2+1]=json!(0);} }
        for (i, byte) in peer.iter().enumerate() { frame["hardware"][i] = json!(byte); frame["hardware"][16806 + i] = json!(byte); }
        run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().push(frame);
    }
    // Disabled ABI has no addon graphics-release span. Its actual native
    // Shake return and cleared animation/source are the completion proof.
    e["clients"][0]["single"]["profile"]["native_effect_events"].as_array_mut().unwrap().remove(3);
    e["clients"][0]["single"]["profile"]["native_effect_video_frames"][2]["render_metadata"][4] = json!(4);
    e["clients"][0]["single"]["profile"]["native_effect_video_frames"][2]["rows"] = json!(rows(4,5,2));
    headbutt_pixels(&e);
    let mut absent_release = e.clone();
    absent_release["clients"][0]["mobile"]["profile"]["native_effect_events"].as_array_mut().unwrap().remove(3);
    absent_release["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][2]["render_metadata"][4] = json!(4);
    for y in 0..144 {absent_release["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][2]["rows"][y*16+1]=json!(4);}
    rejects(&absent_release, "Completed effect has no preceding actual native graphics release");
    // A release recorded later cannot authorize an earlier actual DMA.
    let mut late_release = absent_release;
    late_release["clients"][0]["mobile"]["profile"]["native_effect_events"].as_array_mut().unwrap().push(event(6, 3, &peer));
    rejects(&late_release, "Completed effect has no preceding actual native graphics release");
    e["clients"][0]["mobile"]["profile"]["native_effect_events"][3]["watches"]["5"] = json!([1]);
    rejects(&e, "Completed effect has no preceding actual native graphics release");
}
#[test]
fn headbutt_pixel_control_keeps_hidden_and_duplicate_demand() {
    for second in [[32, 0, 16, 7], [32, 24, 16, 7]] {
        let mut e = fixture();
        for name in ["mobile", "single"] {
            let run = &mut e["clients"][0][name];
            for event in run["profile"]["native_effect_events"].as_array_mut().unwrap() {
                for i in 0..4 { event["watches"]["0"][4 + i] = json!(second[i]); }
            }
            for frame in run["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
                for i in 0..4 { frame["hardware"][4 + i] = json!(second[i]); frame["hardware"][16810 + i] = json!(second[i]); }
                for y in 16..24 {for x in 40..48 {frame["pixels"][(y*160+x)*2]=json!(0);frame["pixels"][(y*160+x)*2+1]=json!(0);}}
            }
        }
        headbutt_pixels(&e);
        let run = &mut e["clients"][0]["mobile"];
        for event in run["profile"]["native_effect_events"].as_array_mut().unwrap() {
            for i in 4..8 { event["watches"]["0"][i] = json!(0); }
        }
        for frame in run["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
            for i in 4..8 { frame["hardware"][i] = json!(0); frame["hardware"][16806 + i] = json!(0); }
        }
        rejects(&e, "Complete disabled native effect source");
    }
}
#[test]
fn headbutt_pixel_control_rejects_self_consistent_mapped_rgb_changes() {
    let mut e = fixture();
    for frame in e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
        frame["hardware"][16672 + 15 * 8 + 2] = json!(0x78);
        frame["hardware"][16672 + 15 * 8 + 3] = json!(0x56);
        for y in 16..24 { for x in (16..24).chain(40..48) {
            frame["pixels"][(y * 160 + x) * 2] = json!(0x78); frame["pixels"][(y * 160 + x) * 2 + 1] = json!(0x56);
        }}
    }
    rejects(&e, "Actual intended mapped OBJ colors differ at slot 15");
}

#[test]
fn headbutt_pixel_rows_accept_midline_palette_change_and_reject_stale_pixels() {
    let mut e = fixture();
    for name in ["mobile", "single"] {
        for frame in e["clients"][0][name]["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
            // Grayscale RGB5(4,4,4): raw0x1084 -> mapped0x2104.
            frame["changes"] = json!([16544 + 15*8 + 2, 0x84, 16544 + 15*8 + 3, 0x10, 16672 + 15*8 + 2, 0x04, 16672 + 15*8 + 3, 0x21]);
            for y in 20..144 {frame["rows"][y*16+9] = json!(4);}
            for y in 20..24 {for x in (16..24).chain(40..48) {
                frame["pixels"][(y*160+x)*2]=json!(0x04);frame["pixels"][(y*160+x)*2+1]=json!(0x21);
            }}
        }
    }
    headbutt_pixels(&e);
    e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][0]["pixels"][(20*160+16)*2] = json!(0x42);
    rejects(&e, "Intended selected effect pixel");
}
#[test]
fn headbutt_pixel_rows_reject_missing_unordered_and_late_source_provenance() {
    let mut missing = fixture();
    missing["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][0]["rows"].as_array_mut().unwrap().pop();
    rejects(&missing, "left == right");
    let mut unordered = fixture();
    let frame = &mut unordered["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][0];
    frame["changes"] = json!([4,1,3,1]); for y in 17..144 {frame["rows"][y*16+9]=json!(2);}
    rejects(&unordered, "Duplicate/unordered row byte mutation");
    let mut late = fixture();
    late["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][0]["rows"][16*16+2]=json!(0);
    rejects(&late, "Latest native animation return before actual DMA required");
}
#[test]
fn headbutt_pixel_rows_cover_pre_animation_previous_oam_and_native_owned_tail() {
    let mut e = fixture();
    for name in ["mobile", "single"] {
        let run = &mut e["clients"][0][name];
        let mut before = run["profile"]["native_effect_video_frames"][0].clone();
        before["rows"] = json!(rows(0,0,0)); before["render_metadata"][1]=json!(0);
        before["display_metadata"][1]=json!(1); before["display_metadata"][2]=json!(0);
        for y in 0..144 {before["rows"][y*16+4]=json!(1);}
        run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().insert(0,before);
        let source = super::effect_bytes(&run["profile"]["native_effect_events"][0]["watches"]["0"]);
        run["profile"]["native_effect_events"].as_array_mut().unwrap().extend([event(3,0,&source),event(4,5,&source)]);
        let mut tail = run["profile"]["native_effect_video_frames"][2].clone();
        tail["rows"] = json!(rows(4,4,2)); tail["render_metadata"][1]=json!(3);
        tail["display_metadata"][1]=json!(4); tail["display_metadata"][2]=json!(3);
        run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().push(tail);
    }
    // Normal previous OAM and native-owned post-effect graphics remain fully
    // decoded, even when their tile numbers overlap the animation range.
    headbutt_pixels(&e);
    let frame = &mut e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][3];
    frame["hardware"][16805]=json!(0xd8);
    for y in 0..144 {frame["rows"][y*16+5]=json!(7);frame["rows"][y*16+6]=json!(7);}
    rejects(&e, "Completed effect has no preceding actual native graphics release");
}

fn window_fixture(reset: bool) -> Value {
    let mut e = fixture();
    for name in ["mobile", "single"] {
        for frame in e["clients"][0][name]["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
            let first_y = if reset {143} else {64};
            frame["hardware"][16800] = json!(0xf3);
            frame["hardware"][16803] = json!(first_y); frame["hardware"][16804] = json!(7);
            for tile_row in 0..8 {
                frame["hardware"][160+16+tile_row*2]=json!(255); // solid color1
                frame["hardware"][160+32+tile_row*2+1]=json!(255); // solid color2
            }
            // Independently fixed grayscale colors1 and2.
            for (color,bytes,raw) in [(1,[0x41,0x08],[0x21,0x04]),(2,[0x82,0x10],[0x42,0x08])] {
                frame["hardware"][16544+color*2]=json!(raw[0]); frame["hardware"][16544+color*2+1]=json!(raw[1]);
                frame["hardware"][16672+color*2]=json!(bytes[0]); frame["hardware"][16672+color*2+1]=json!(bytes[1]);
            }
            // Independent fixed stripes: row0 uses tile1, row1 tile2.
            for map_row in 0..11 { for col in 0..32 {frame["hardware"][160+0x1c00+map_row*32+col]=json!(1+map_row%2);} }
            if reset {for col in 0..32 {frame["hardware"][160+0x1bc0+col]=json!(1);}}
            for y in first_y..144 {
                frame["rows"][y*16+8]=json!(if reset {0} else {9+y-64});
                let bytes = if reset || (y-64)/8%2==0 {[0x41,0x08]} else {[0x82,0x10]};
                for x in 0..160 {frame["pixels"][(y*160+x)*2]=json!(bytes[0]);frame["pixels"][(y*160+x)*2+1]=json!(bytes[1]);}
            }
        }
    }
    e
}
#[test]
fn headbutt_pixel_window_accepts_pinned_rows_and_rejects_unbiased_map() {
    let mut e = window_fixture(false); headbutt_pixels(&e);
    // The former decoder accepted these next-row stripes with unchanged
    // hardware and entry9. They do not match the pinned core's window output.
    for name in ["mobile","single"] {
        for frame in e["clients"][0][name]["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
            for y in 64..144 {let bytes=if (y-64)/8%2==0 {[0x82,0x10]} else {[0x41,0x08]};for x in 0..160 {
                frame["pixels"][(y*160+x)*2]=json!(bytes[0]);frame["pixels"][(y*160+x)*2+1]=json!(bytes[1]);
            }}
        }
    }
    rejects(&e,"independently decoded BG/OBJ/window pixel at 0,64");
}
#[test]
fn headbutt_pixel_window_accepts_signed_reset_row_and_rejects_address_overflow() {
    let mut e = window_fixture(true); headbutt_pixels(&e);
    for frame in e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
        frame["rows"][143*16+8]=json!(1000);
    }
    rejects(&e,"Actual window/BG map address outside captured VRAM bank");
}
