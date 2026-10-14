//! Independent fixed raster fixtures execute the durable acceptance oracle.
use super::{headbutt_pixels, Value};
use serde_json::json;
fn event(sequence: usize, span: usize, source: &[u8]) -> Value {
    let mut fields = vec![0; 22];
    fields[0] = sequence * 10; fields[1] = sequence; fields[2] = span; fields[3] = 2;
    json!({"fields": fields, "watches": {"0": source, "4": vec![0; 160], "1": [0], "2": [0], "5": [0]}})
}
fn rows(index: usize, sequence: usize, phase: usize) -> Vec<usize> {
    (0..144).flat_map(|_| [1,index,sequence*10+1,sequence,sequence,0,0,7,1,0,sequence,sequence*10+1,phase,0,0,0]).collect()
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
fn teardown_fixture() -> Value {
    let mut e = fixture();
    for name in ["mobile", "single"] {
        let run = &mut e["clients"][0][name];
        let mut restored = vec![0u8; 160];
        restored[144..148].copy_from_slice(&[32,24,1,0]);
        let mut teardown = event(3,8,&restored);
        teardown["fields"][14] = json!(0x4861); teardown["fields"][15] = json!(35); teardown["fields"][16] = json!(1);
        run["profile"]["native_effect_source_bindings"] = json!({"teardown_return": [35,0x4861]});
        run["profile"]["native_effect_events"].as_array_mut().unwrap().push(teardown);
        let mut frame = run["profile"]["native_effect_video_frames"][1].clone();
        frame["render_metadata"][1]=json!(3); frame["render_metadata"][2]=json!(3); frame["rows"]=json!(rows(3,3,1));
        frame["display_metadata"][1]=json!(4);frame["display_metadata"][2]=json!(3);frame["display_metadata"][3]=json!(3);
        frame["host_frame"]=json!(3);frame["pixels"]=json!(vec![0;160*144*2]);
        frame["hardware"][16805]=json!(0xc1);
        for (i,b) in restored.iter().enumerate() {frame["hardware"][i]=json!(b);frame["hardware"][16806+i]=json!(b);}
        run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().push(frame);
    }
    e
}
#[test]
fn headbutt_pixel_teardown_restores_player_and_matches_same_step_control_semantically() {
    let e=teardown_fixture(); headbutt_pixels(&e);
    // The same animation generation has both live and teardown controls.
    let run=&e["clients"][0]["single"];
    let key=|i| {let f=&run["profile"]["native_effect_video_frames"][i]; let mut h=super::effect_bytes(&f["hardware"]);let mut c=0;super::headbutt_source_key(run,&super::headbutt_row(f,0,&mut h,&mut c)).unwrap()};
    assert_eq!(key(1).1,key(2).1); assert_ne!(key(1).0,key(2).0);
    let forward:std::collections::BTreeMap<_,_>=[(key(1),1),(key(2),2)].into_iter().collect();
    let reverse:std::collections::BTreeMap<_,_>=[(key(2),2),(key(1),1)].into_iter().collect();
    assert_eq!(forward,reverse);assert_eq!(reverse.get(&key(2)),Some(&2));
}
#[test]
fn headbutt_pixel_native_initial_source_is_bound_before_animation() {
    let mut e=teardown_fixture();
    for name in ["mobile","single"] {
        let run=&mut e["clients"][0][name];
        let restored=super::effect_bytes(&run["profile"]["native_effect_events"][2]["watches"]["0"]);
        let mut initial=event(0,8,&restored);initial["fields"][14]=json!(0x47c1);initial["fields"][15]=json!(35);initial["fields"][16]=json!(2);
        run["profile"]["native_effect_source_bindings"]["initial_return"]=json!([35,0x47c1]);
        run["profile"]["native_effect_events"].as_array_mut().unwrap().insert(0,initial);
        for (i,f) in run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().iter_mut().enumerate(){for y in 0..144{f["rows"][y*16+1]=json!(i+2);}}
        let mut frame=run["profile"]["native_effect_video_frames"][2].clone();
        frame["host_frame"]=json!(0);frame["rows"]=json!(rows(1,0,0));frame["render_metadata"][1]=json!(0);frame["render_metadata"][2]=json!(0);
        frame["display_metadata"][1]=json!(1);frame["display_metadata"][2]=json!(0);frame["display_metadata"][3]=json!(0);
        for y in 0..144{frame["rows"][y*16+4]=json!(1);}
        run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().insert(0,frame);
    }
    headbutt_pixels(&e);
}
#[test]
fn headbutt_pixel_teardown_rejects_missing_late_wrong_kind_and_leftovers() {
    for mode in 0..5 {
        let mut e=teardown_fixture();let run=&mut e["clients"][0]["mobile"];
        match mode {
            0 => {run["profile"]["native_effect_events"][2]["fields"][0]=json!(32);},
            1 => {run["profile"]["native_effect_events"][2]["fields"][2]=json!(0);},
            2 => {run["profile"]["native_effect_events"][2]["fields"][14]=json!(0x4862);},
            3 => {for (i,b) in [32,24,16,7].iter().enumerate(){run["profile"]["native_effect_video_frames"][2]["hardware"][i]=json!(b);run["profile"]["native_effect_video_frames"][2]["hardware"][16806+i]=json!(b);}},
            _ => {run["profile"]["native_effect_events"][2]["watches"]["0"]=json!([0]);}
        }
        assert!(std::panic::catch_unwind(||headbutt_pixels(&e)).is_err());
    }
}
#[test]
fn headbutt_pixel_later_teardown_and_shake_return_cannot_excuse_earlier_disappearance() {
    let mut e=teardown_fixture();let run=&mut e["clients"][0]["mobile"];
    run["profile"]["native_effect_events"][2]["fields"][0]=json!(32);
    run["profile"]["native_effect_events"].as_array_mut().unwrap().push(event(4,0,&vec![0;160]));
    for y in 0..144{run["profile"]["native_effect_video_frames"][2]["rows"][y*16+1]=json!(2);run["profile"]["native_effect_video_frames"][2]["rows"][y*16+3]=json!(2);}
    // Supply a separate earlier live control occurrence so this corruption
    // reaches complete source demand, rather than a missing counterpart gate.
    let video = e["clients"][0]["single"]["profile"]["native_effect_video_frames"].as_array_mut().unwrap();
    let live = video[1].clone(); video.insert(2, live);
    for (i, frame) in video.iter_mut().enumerate() {
        frame["render_metadata"][1] = json!(i+1);
        frame["display_metadata"][1] = json!(i+2);
        frame["display_metadata"][2] = json!(i+1);
    }
    rejects(&e,"Every source-bound native effect quad");
    let mut stale=teardown_fixture();
    stale["clients"][0]["single"]["profile"]["native_effect_events"][2]["watches"]["4"][0]=json!(1);
    assert!(std::panic::catch_unwind(||headbutt_pixels(&stale)).is_err());
    let mut written=teardown_fixture();
    for at in [144,16806+144]{written["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][2]["hardware"][at]=json!(33);}
    rejects(&written,"Native ShadowOAM changed after producer binding");
}
#[test]
fn headbutt_pixel_control_allows_tile_reuse_only_after_actual_release_and_return() {
    let mut e=teardown_fixture();
    for name in ["mobile","single"] {
        let run=&mut e["clients"][0][name];
        let mut peer=vec![0u8;160];peer[..4].copy_from_slice(&[32,24,16,6]);
        run["profile"]["native_effect_events"].as_array_mut().unwrap().extend([event(4,0,&peer),event(5,3,&peer),event(6,5,&peer)]);
        let mut frame=run["profile"]["native_effect_video_frames"][2].clone();
        frame["rows"]=json!(rows(6,6,2));frame["render_metadata"][1]=json!(4);frame["render_metadata"][2]=json!(6);
        frame["display_metadata"][1]=json!(5);frame["display_metadata"][2]=json!(4);frame["display_metadata"][3]=json!(6);
        if name=="mobile" {frame["hardware"][16805]=json!(0xd8);for y in 0..144{frame["rows"][y*16+5]=json!(7);frame["rows"][y*16+6]=json!(7);}}
        for(i,b)in peer.iter().enumerate(){frame["hardware"][i]=json!(b);frame["hardware"][16806+i]=json!(b);}
        run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().push(frame);
    }
    headbutt_pixels(&e);
    e["clients"][0]["mobile"]["profile"]["native_effect_events"][4]["watches"]["5"]=json!([1]);
    rejects(&e,"Completed effect has no preceding actual native graphics release");
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
    let mut e=teardown_fixture();
    for name in ["mobile","single"] {
        let run=&mut e["clients"][0][name];let mut before=run["profile"]["native_effect_video_frames"][0].clone();
        before["rows"]=json!(rows(0,0,0));before["render_metadata"][1]=json!(0);
        before["display_metadata"][1]=json!(1);before["display_metadata"][2]=json!(0);
        for y in 0..144 {before["rows"][y*16+4]=json!(1);before["rows"][y*16+2]=json!(0);before["rows"][y*16+11]=json!(0);}
        run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().insert(0,before);
    }
    headbutt_pixels(&e);
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
    for name in ["mobile", "single"] {
        for frame in e["clients"][0][name]["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
            frame["rows"][143*16+8]=json!(1000);
        }
    }
    rejects(&e,"Actual window/BG map address outside captured VRAM bank");
}

#[test]
fn headbutt_pixel_late_initializer_cannot_end_active_demand() {
    // The independently found defect retains both matched controls, two live
    // generations, complete restored OAM and exact pixels, without teardown.
    let mut e = teardown_fixture();
    for name in ["mobile", "single"] {
        let run = &mut e["clients"][0][name];
        run["profile"]["native_effect_source_bindings"]["initial_return"] = json!([35, 0x47c1]);
        run["profile"]["native_effect_events"][2]["fields"][14] = json!(0x47c1);
        run["profile"]["native_effect_events"][2]["fields"][16] = json!(2);
        for y in 0..144 { run["profile"]["native_effect_video_frames"][2]["rows"][y*16+12] = json!(0); }
    }
    rejects(&e, "Native initialization cannot waive live effect demand");
}

#[test]
fn headbutt_pixel_unrelated_clear_preserves_pre_effect_previous_native_baseline() {
    let mut e = fixture();
    for name in ["mobile", "single"] {
        let run = &mut e["clients"][0][name];
        let source = super::effect_bytes(&run["profile"]["native_effect_events"][0]["watches"]["0"]);
        run["profile"]["native_effect_events"].as_array_mut().unwrap().insert(0, event(0, 8, &source));
        for (i, frame) in run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().iter_mut().enumerate() {
            for y in 0..144 { frame["rows"][y*16+1] = json!(i+2); }
        }
        let mut before = run["profile"]["native_effect_video_frames"][0].clone();
        before["rows"] = json!(rows(0, 0, 0)); before["render_metadata"][1] = json!(0);
        before["display_metadata"][1] = json!(1); before["display_metadata"][2] = json!(0);
        before["hardware"][16805] = json!(0xc1);
        // Normal native writes after unrelated Clear are an honest previous
        // OAM baseline, not a completed effect publication. Tile1 is blank.
        for (i, b) in [33, 24, 1, 0].iter().enumerate() {
            before["hardware"][144+i] = json!(b); before["hardware"][16806+144+i] = json!(b);
        }
        for y in 0..144 { before["rows"][y*16+4] = json!(1); }
        run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().insert(0, before);
    }
    headbutt_pixels(&e);
    // Explicitly labelling this unrelated snapshot as intent must reject.
    for y in 0..144 { e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][0]["rows"][y*16+1] = json!(1); }
    rejects(&e, "Latest native animation return before actual DMA required");
}

#[test]
fn headbutt_pixel_unrelated_clear_cannot_waive_active_source_or_later_writes() {
    let mut e = teardown_fixture();
    for name in ["mobile", "single"] {
        let run = &mut e["clients"][0][name];
        run["profile"]["native_effect_events"][2]["fields"][16] = json!(0);
        for y in 0..144 { run["profile"]["native_effect_video_frames"][2]["rows"][y*16+1] = json!(2); }
    }
    rejects(&e, "Every source-bound native effect quad");
    let mut written = fixture();
    let run = &mut written["clients"][0]["mobile"];
    let source = super::effect_bytes(&run["profile"]["native_effect_events"][0]["watches"]["0"]);
    let mut clear = event(1, 8, &source); clear["fields"][0] = json!(15);
    run["profile"]["native_effect_events"].as_array_mut().unwrap().insert(1, clear);
    for y in 0..144 { run["profile"]["native_effect_video_frames"][1]["rows"][y*16+1] = json!(3); }
    let frame = &mut run["profile"]["native_effect_video_frames"][0];
    frame["hardware"][16805] = json!(0xc1);
    // First DMA now follows the unrelated Clear, while retaining producer1.
    for y in 0..144 { frame["rows"][y*16+2] = json!(16); frame["rows"][y*16+11] = json!(16); }
    frame["hardware"][159] = json!(1); frame["hardware"][16965] = json!(1);
    rejects(&written, "Native ShadowOAM changed after producer binding");
}

fn presentation_fixture() -> Value {
    let mut e = teardown_fixture();
    for name in ["mobile", "single"] {
        let run = &mut e["clients"][0][name];
        let base = run["profile"]["native_effect_video_frames"][2].clone();
        run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().truncate(2);
        for (ordinal, age) in [0,1,3,3,5,6,7].into_iter().enumerate() {
            let mut f = base.clone();
            let output = ordinal + 3;
            f["render_metadata"][1] = json!(output);
            f["render_metadata"][2] = json!(3 + age);
            f["display_metadata"][1] = json!(output + 1);
            f["display_metadata"][2] = json!(output);
            f["display_metadata"][3] = json!(3 + age);
            f["host_frame"] = json!(3 + ordinal);
            f["rows"] = json!(rows(3, 3 + age, 1));
            f["hardware"][16800] = json!(0xb3);
            f["hardware"][16803] = json!(if age == 5 || age == 6 {0} else {144});
            f["hardware"][16804] = json!(7);
            for y in 0..144 { f["rows"][y*16+8] = json!(if age == 5 || age == 6 {9+y} else {9}); }
            run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().push(f);
        }
    }
    e
}
#[test]
fn headbutt_pixel_retained_source_matches_dma_progress_and_repeated_output() {
    headbutt_pixels(&presentation_fixture());
}
#[test]
fn headbutt_pixel_retained_source_rejects_stale_stage_and_wrong_same_stage_view() {
    let mut e = presentation_fixture();
    for y in 0..144 {
        e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][6]["rows"][y*16+3] = json!(20);
    }
    rejects(&e, "No same-source control");
    let mut stale = presentation_fixture();
    // A stale delta0 candidate cannot substitute for the missing delta5 control.
    stale["clients"][0]["single"]["profile"]["native_effect_video_frames"].as_array_mut().unwrap().remove(6);
    rejects(&stale, "No same-source control");
    for index in [6,7,8] {
        let mut e = presentation_fixture();
        e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][index]["hardware"][16803] = json!(if index == 8 {0} else {144});
        rejects(&e, "Actual native view geometry differs");
    }
    let mut e = presentation_fixture();
    e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][6]["rows"][8] = json!(10);
    rejects(&e, "Actual native window line differs");
}
#[test]
fn headbutt_pixel_retained_source_rejects_extra_or_missing_actual_output() {
    for extra in [false,true] {
        let mut e = presentation_fixture();
        let video = e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"].as_array_mut().unwrap();
        if extra { let f = video[5].clone(); video.insert(6,f); } else { video.remove(5); }
        for (i,f) in video.iter_mut().enumerate() {
            f["render_metadata"][1] = json!(i+1);
            f["display_metadata"][1] = json!(i+2);
            f["display_metadata"][2] = json!(i+1);
        }
        rejects(&e, if extra {"No same-source control occurrence"} else {"Actual output source-stage progression or repeated-DMA multiplicity differs"});
    }
    let mut e = presentation_fixture();
    e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][8]["display_metadata"][1] = json!(100);
    rejects(&e, "Missing/discontinuous actual output invocation");
}
#[test]
fn headbutt_pixel_retained_source_rejects_invalid_age_and_ambiguous_control() {
    let mut e = presentation_fixture();
    e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][2]["rows"][3] = json!(2);
    rejects(&e, "Actual DMA precedes its native source frame");
    let mut e = presentation_fixture();
    e["clients"][0]["single"]["profile"]["native_effect_video_frames"][5]["hardware"][16803] = json!(0);
    rejects(&e, "Actual native view geometry differs");
}
#[test]
fn headbutt_pixel_progression_uses_separate_source_and_output_counter_domains() {
    let mut e = presentation_fixture();
    let run = &mut e["clients"][0]["single"];
    for event in run["profile"]["native_effect_events"].as_array_mut().unwrap() {
        event["fields"][1] = json!(event["fields"][1].as_u64().unwrap()+32);
    }
    for f in run["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
        f["host_frame"] = json!(f["host_frame"].as_u64().unwrap()+32);
        f["render_metadata"][1] = json!(f["render_metadata"][1].as_u64().unwrap()+900);
        f["render_metadata"][2] = json!(f["render_metadata"][2].as_u64().unwrap()+32);
        f["display_metadata"][1] = json!(f["display_metadata"][1].as_u64().unwrap()+500);
        f["display_metadata"][2] = f["render_metadata"][1].clone();
        f["display_metadata"][3] = f["render_metadata"][2].clone();
        for y in 0..144 {for field in [3,10] {
            f["rows"][y*16+field] = json!(f["rows"][y*16+field].as_u64().unwrap()+32);
        }}
    }
    headbutt_pixels(&e);
}

// Two actual outputs share the same native source and actual DMA identity.
// Their independent intended inputs may evolve between output invocations.
fn repeated_live_fixture() -> Value {
    let mut e = fixture();
    for name in ["mobile", "single"] {
        let video = e["clients"][0][name]["profile"]["native_effect_video_frames"].as_array_mut().unwrap();
        let first = video[0].clone(); video.insert(1, first);
        for (i, frame) in video.iter_mut().enumerate() {
            frame["render_metadata"][1] = json!(i+1);
            frame["display_metadata"][1] = json!(i+2);
            frame["display_metadata"][2] = json!(i+1);
        }
    }
    e
}
fn earlier_live_color(frame: &mut Value) {
    // Fixed independent RGB5(4,4,4) -> RGB565(4,8,4), plus both full rectangles.
    frame["hardware"][16544+15*8+2] = json!(0x84);
    frame["hardware"][16544+15*8+3] = json!(0x10);
    frame["hardware"][16672+15*8+2] = json!(0x04);
    frame["hardware"][16672+15*8+3] = json!(0x21);
    for y in 16..24 { for x in (16..24).chain(40..48) {
        frame["pixels"][(y*160+x)*2] = json!(0x04);
        frame["pixels"][(y*160+x)*2+1] = json!(0x21);
    }}
}
#[test]
fn headbutt_pixel_each_duplicate_occurrence_retains_intended_tile() {
    let mut e = presentation_fixture();
    // Earlier inactive effect tile differs, while its own pixels stay exact.
    e["clients"][0]["single"]["profile"]["native_effect_video_frames"][4]["hardware"][160+256] = json!(0);
    rejects(&e, "Actual native animation tiles differ");
}
#[test]
fn headbutt_pixel_each_duplicate_occurrence_retains_live_rgb() {
    let mut e = repeated_live_fixture();
    earlier_live_color(&mut e["clients"][0]["single"]["profile"]["native_effect_video_frames"][0]);
    rejects(&e, "Actual intended OBJ palette RGB differs at slot 15");
}
#[test]
fn headbutt_pixel_duplicate_occurrences_allow_corresponding_native_input_changes() {
    let mut e = repeated_live_fixture();
    for name in ["mobile", "single"] {
        let first = &mut e["clients"][0][name]["profile"]["native_effect_video_frames"][0];
        earlier_live_color(first);
        // Background is blank, so native SCX may change without repainting it.
        first["hardware"][16802] = json!(8);
    }
    headbutt_pixels(&e);
    let mut e = presentation_fixture();
    for name in ["mobile", "single"] {
        e["clients"][0][name]["profile"]["native_effect_video_frames"][4]["hardware"][160+256] = json!(0);
    }
    headbutt_pixels(&e);
}
#[test]
fn headbutt_pixel_duplicate_occurrences_reject_wrong_first_or_second_pair() {
    for wrong in [0,1] {
        let mut e = repeated_live_fixture();
        for name in ["mobile", "single"] {
            earlier_live_color(&mut e["clients"][0][name]["profile"]["native_effect_video_frames"][0]);
        }
        let video = e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"].as_array_mut().unwrap();
        let replacement = video[1-wrong].clone();
        for field in ["hardware", "pixels"] { video[wrong][field] = replacement[field].clone(); }
        rejects(&e, "Actual intended OBJ palette RGB differs at slot 15");
    }
    let mut e = presentation_fixture();
    e["clients"][0]["single"]["profile"]["native_effect_video_frames"][4]["rows"][77*16+8] = json!(10);
    rejects(&e, "Actual native window line differs");
}
#[test]
fn headbutt_pixel_repeated_output_dma_identity_and_source_sequence_stay_strict() {
    let mut e = presentation_fixture();
    e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][5]["rows"][77*16+4] = json!(100);
    rejects(&e, "Actual output source-stage progression or repeated-DMA multiplicity differs");
    let mut e = presentation_fixture();
    e["clients"][0]["mobile"]["profile"]["native_effect_video_frames"][6]["rows"][77*16+2] = json!(29);
    rejects(&e, "Latest native animation return before actual DMA required");
}
#[test]
fn headbutt_pixel_unbound_first_output_cannot_match_genuine_native_initializer() {
    let mut e = teardown_fixture();
    for name in ["mobile", "single"] {
        let run = &mut e["clients"][0][name];
        let restored = super::effect_bytes(&run["profile"]["native_effect_events"][2]["watches"]["0"]);
        let mut baseline = run["profile"]["native_effect_video_frames"][2].clone();
        baseline["host_frame"] = json!(0);
        baseline["render_metadata"][1] = json!(0);
        baseline["render_metadata"][2] = json!(0);
        baseline["display_metadata"][1] = json!(1);
        baseline["display_metadata"][2] = json!(0);
        baseline["display_metadata"][3] = json!(0);
        baseline["rows"] = json!(rows(if name == "single" {1} else {0}, 0, 0));
        for y in 0..144 { baseline["rows"][y*16+4] = json!(1); }
        if name == "single" {
            let mut initial = event(0, 8, &restored);
            initial["fields"][14] = json!(0x47c1);
            initial["fields"][15] = json!(35);
            initial["fields"][16] = json!(2);
            run["profile"]["native_effect_source_bindings"]["initial_return"] = json!([35,0x47c1]);
            run["profile"]["native_effect_events"].as_array_mut().unwrap().insert(0, initial);
            for frame in run["profile"]["native_effect_video_frames"].as_array_mut().unwrap() {
                for y in 0..144 {
                    frame["rows"][y*16+1] = json!(frame["rows"][y*16+1].as_u64().unwrap()+1);
                }
            }
        }
        run["profile"]["native_effect_video_frames"].as_array_mut().unwrap().insert(0, baseline);
    }
    rejects(&e, "Actual output source-stage progression or repeated-DMA multiplicity differs");
}
