use mobile_acceptance::evidence;
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
fn limit(key: &str) -> u64 {
    mobile_acceptance::assumptions()["acceptance_limits"][key]
        .as_u64()
        .expect("Declared acceptance limit")
}

fn clients(e: &Value) -> &[Value] {
    e["clients"].as_array().expect("Four client records")
}
fn frames(run: &Value) -> &[Value] {
    run["frames"].as_array().expect("Frame trace")
}
fn n(state: &Value, key: &str) -> u64 {
    state[key]
        .as_u64()
        .unwrap_or_else(|| panic!("Missing observer: {key}"))
}
fn steady(run: &Value) -> impl Iterator<Item = &Value> {
    let first = n(&run["profile"], "world_window_start");
    frames(run)
        .iter()
        .filter(move |f| n(f, "frame") >= first && n(f, "frame") < first + limit("profile_frames"))
        .map(|f| &f["state"])
}
fn connected(e: &Value) {
    assert_eq!(clients(e).len(), 4);
    let mut ids = BTreeSet::new();
    for (index, client) in clients(e).iter().enumerate() {
        let state = &client["mobile"]["state"];
        assert_eq!(
            n(state, "wMobileOnline"),
            1,
            "Never established four-player fixture"
        );
        assert_eq!(
            n(state, "wMobilePeerCount"),
            3,
            "Missing actual emulator peers"
        );
        assert_eq!(n(state, "hCrashCode"), 0);
        if e["action"] != "warp" {
            let expected = if index == 0 {
                &e["fixture"]["primary"]
            } else {
                &e["fixture"]["peers"][index - 1]
            };
            if e["action"] != "clear-conflict" || index == 0 {
                assert_eq!(
                    state["wXCoord"], expected[0],
                    "Requested scenario X coordinate was not established"
                );
                assert_eq!(
                    state["wYCoord"], expected[1],
                    "Requested scenario Y coordinate was not established"
                );
            }
        }
        ids.insert(n(state, "wMobilePlayerId"));
    }
    assert_eq!(ids.len(), 4, "Four distinct network identities required");
}
fn cadence(e: &Value) {
    connected(e);
    for (i, client) in clients(e).iter().enumerate() {
        let m = &client["mobile"]["profile"];
        let s = &client["single"]["profile"];
        assert_eq!(m["world_window_start"], s["world_window_start"]);
        assert!(
            n(m, "world_frames_with_update") >= n(s, "world_frames_with_update"),
            "Client {i}: additional dropped world frames, native={s}, mobile={m}"
        );
        assert!(
            n(m, "world_max_frame_gap") <= n(s, "world_max_frame_gap"),
            "Client {i}: longer world-update gap"
        );
        assert!(
            n(m, "world_duplicate_updates") <= n(s, "world_duplicate_updates"),
            "Client {i}: duplicate world updates"
        );
        let updates = |run: &Value| {
            let start = n(&run["profile"], "world_window_start");
            let previous = frames(run)
                .iter()
                .find(|row| n(row, "frame") + 1 == start)
                .expect("Missing world counter immediately before measurement");
            let cumulative: Vec<_> = steady(run).map(|s| n(s, "vProfileWorldUpdates")).collect();
            assert_eq!(cumulative.len() as u64, limit("profile_frames"));
            mobile_acceptance::contracts::cumulative_deltas(
                n(&previous["state"], "vProfileWorldUpdates"),
                &cumulative,
            )
        };
        if let Some(error) = mobile_acceptance::contracts::cadence_regression(
            &updates(&client["single"]),
            &updates(&client["mobile"]),
        ) {
            panic!("Client {i}: {error}");
        }
    }
}
fn safety(e: &Value) {
    for client in clients(e) {
        let rows: Vec<_> = frames(&client["mobile"])
            .iter()
            .filter(|r| n(&r["state"], "vAcceptanceReady") > 0)
            .collect();
        assert!(
            rows.len() as u64 >= limit("profile_frames"),
            "No initialized observation window"
        );
        for row in rows {
            assert_eq!(
                n(&row["state"], "hCrashCode"),
                0,
                "Crash at frame {}",
                row["frame"]
            );
        }
        let p = &client["mobile"]["profile"];
        assert_eq!(
            n(p, "serial_failures"),
            0,
            "Serial interrupt corrupted native registers/banks"
        );
        assert_eq!(
            n(p, "tick_failures"),
            0,
            "Multiplayer hook corrupted native registers/banks"
        );
        assert_eq!(
            n(p, "save_copy_register_failures"),
            0,
            "Native serialization changed data during copy"
        );
        assert_eq!(
            n(p, "save_copy_byte_failures"),
            0,
            "Saved native payload differs from its source"
        );
    }
}
fn native_preserved(e: &Value) {
    for client in clients(e) {
        for key in [
            "vAcceptancePartyHash",
            "vAcceptanceInventoryHash",
            "vStoryFlagsHash",
        ] {
            let m: BTreeSet<_> = steady(&client["mobile"]).map(|s| n(s, key)).collect();
            let s: BTreeSet<_> = steady(&client["single"]).map(|s| n(s, key)).collect();
            assert_eq!(m, s, "Multiplayer modified {key}");
        }
    }
}
fn same_save(client: &Value, context: &str) {
    let mobile = client["mobile"]["save"].as_array().expect("Mobile SRAM");
    let native = client["single"]["save"].as_array().expect("Native SRAM");
    assert_eq!(mobile.len(), native.len(), "{context}: SRAM length");
    if let Some(at) = mobile.iter().zip(native).position(|(m, s)| m != s) {
        panic!("{context}: first SRAM difference at {at:#x}, mobile={}, native={}; hashes mobile={}, native={}. Complete dumps: {}",mobile[at],native[at],client["mobile"]["save_sha256"],client["single"]["save_sha256"],mobile_acceptance::output().display());
    }
}

fn emergency(e: &Value) {
    let m = &clients(e)[0]["mobile"];
    let event = m["events"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|e| n(e, "button") == 2)
        .last()
        .expect("Exit Select input absent");
    let first = n(event, "frame") + limit("exit_frames");
    let after: Vec<_> = frames(m)
        .iter()
        .filter(|r| n(r, "frame") >= first)
        .collect();
    assert!(!after.is_empty());
    for row in after {
        for key in [
            "wMobileOwner",
            "wMobileOnline",
            "wMobileWanted",
            "wMobilePeerCount",
        ] {
            assert_eq!(
                n(&row["state"], key),
                0,
                "Local exit overwritten at frame {}, {key}",
                row["frame"]
            );
        }
    }
    assert_eq!(n(&m["state"], "hCrashCode"), 0);
    assert_ne!(
        m["state"]["wXCoord"], e["fixture"]["primary"][0],
        "Normal movement did not recover"
    );
}

fn before_exit(e: &Value) -> &Value {
    let run = &clients(e)[0]["mobile"];
    let event = run["events"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|e| n(e, "button") == 2)
        .last()
        .expect("Exit input absent");
    let at = n(event, "frame");
    &frames(run)
        .iter()
        .find(|f| n(f, "frame") + 1 == at)
        .expect("Exit context absent")["state"]
}

#[test]
fn four_player_presence_and_private_records() {
    let e = evidence("empty");
    connected(&e);
    for c in clients(&e) {
        for state in steady(&c["mobile"]) {
            assert_eq!(n(state, "vNPCOccupiedMask"), 1);
        }
    }
}
#[test]
fn equal_actor_load_frame_timing_matches_singleplayer() {
    cadence(&evidence("empty"));
}
#[test]
fn background_multiplayer_work_never_crosses_native_frame_deadlines() {
    let e = evidence("empty");
    connected(&e);
    for c in clients(&e) {
        let p = &c["mobile"]["profile"];
        assert!(
            n(p, "tick_count") > 0 && n(p, "native_idle_count") > 0,
            "No actual background work was observed"
        );
        for key in [
            "native_upload_vblank_crossings",
            "native_idle_vblank_crossings",
            "compose_vblank_crossings",
            "native_group_vblank_crossings",
            "peer_group_vblank_crossings",
            "merge_palette_vblank_crossings",
            "pose_commit_vblank_crossings",
            "command_prepare_vblank_crossings",
            "application_parse_vblank_crossings",
            "response_checksum_vblank_crossings",
        ] {
            assert_eq!(
                n(p, key),
                0,
                "Multiplayer job crossed a native frame deadline: {key}"
            );
        }
    }
}
#[test]
fn crowded_frame_timing_retains_native_baseline_gaps() {
    cadence(&evidence("pool"));
}
#[test]
fn mixed_100_200_300_400_percent_speeds() {
    cadence(&evidence("speeds"));
}
#[test]
fn identical_trainer_ids_have_distinct_sessions() {
    let e = evidence("duplicate-ids");
    connected(&e);
    for c in clients(&e) {
        assert_eq!(
            n(&c["mobile"]["state"], "vAcceptanceTrainerID"),
            12345,
            "Requested duplicate native trainer ID was not established"
        );
    }
}
#[test]
fn local_state_and_story_flags_are_independent() {
    let e = evidence("empty");
    connected(&e);
    native_preserved(&e);
    safety(&e);
}
#[test]
fn all_twelve_native_objects_remain_simulated() {
    let e = evidence("pool");
    for state in steady(&clients(&e)[0]["single"]) {
        assert_eq!(
            n(state, "vNPCOccupiedMask"),
            8191,
            "The primary native control did not establish twelve ordinary objects"
        );
    }
    connected(&e);
    for c in clients(&e) {
        for (mobile, native) in steady(&c["mobile"]).zip(steady(&c["single"])) {
            assert_eq!(
                mobile["vNPCOccupiedMask"], native["vNPCOccupiedMask"],
                "Native activation/culling must be preserved for each receiver"
            );
            assert_eq!(
                mobile["vAcceptanceNativeHash"], native["vAcceptanceNativeHash"],
                "Ordinary object simulation changed"
            );
        }
    }
    native_preserved(&e);
}
#[test]
fn local_player_remains_visible_whenever_native_control_displays_it() {
    let e = evidence("scanline");
    connected(&e);
    for c in clients(&e) {
        for (m, s) in steady(&c["mobile"]).zip(steady(&c["single"])) {
            if n(s, "vAcceptanceLocalComplete") == 1 {
                assert_eq!(n(m, "vAcceptanceLocalComplete"), 1);
            }
        }
    }
}
#[test]
fn four_npcs_player_three_remotes_on_one_scanline_receive_presentation() {
    let e = evidence("scanline");
    connected(&e);
    presentation(&e, true);
}
#[test]
fn twelve_objects_and_three_remotes_receive_fair_presentation() {
    let e = evidence("pool");
    connected(&e);
    presentation(&e, true);
}
fn presentation(e: &Value, native: bool) {
    for c in clients(e) {
        let mut last = BTreeMap::new();
        let mut counts = BTreeMap::<String, u64>::new();
        let ordinary = n(&e["fixture"], "ordinary_objects");
        let mut expected = BTreeSet::<u64>::new();
        for (state, baseline) in steady(&c["mobile"]).zip(steady(&c["single"])) {
            let mut eligible = Vec::new();
            if native {
                let wanted = ordinary_demands(baseline, ordinary);
                let shown = ordinary_presentations(state, ordinary);
                expected.extend(&wanted);
                for map in wanted {
                    eligible.push((format!("native{map}"), shown.contains(&map)));
                }
            }
            for peer in 1..=3 {
                if n(state, &format!("wMobilePeer{peer}Visible")) == 1 {
                    eligible.push((
                        format!("remote{peer}"),
                        n(state, &format!("vAcceptancePeer{peer}Complete")) == 1,
                    ));
                }
            }
            for (id, visible) in eligible {
                let gap = last.entry(id.clone()).or_insert(0u64);
                if visible {
                    *gap = 0;
                    *counts.entry(id.clone()).or_default() += 1;
                } else {
                    *gap += 1;
                }
                assert!(
                    *gap <= limit("presentation_gap_frames"),
                    "{id} starved for {gap} eligible frames"
                );
            }
        }
        assert!(
            counts.keys().filter(|s| s.starts_with("remote")).count() == 3,
            "Not all remote figures were actually presented"
        );
        let remote: Vec<_> = counts
            .iter()
            .filter(|(s, _)| s.starts_with("remote"))
            .map(|(_, n)| *n)
            .collect();
        assert!(
            remote.iter().max().unwrap() - remote.iter().min().unwrap()
                <= limit("fairness_count_difference"),
            "Unequal same-class presentation: {counts:?}"
        );
        for slot in &expected {
            assert!(
                counts.contains_key(&format!("native{slot}")),
                "Required visible ordinary figure {slot} was never presented"
            );
        }
        let classes: Vec<Vec<_>> = if e["fixture"]["scene"] == "pool" {
            vec![
                expected.iter().copied().filter(|map| *map <= 6).collect(),
                expected.iter().copied().filter(|map| *map > 6).collect(),
            ]
        } else {
            vec![expected.into_iter().collect()]
        };
        for class in classes {
            let shares: Vec<_> = class
                .iter()
                .map(|slot| counts[&format!("native{slot}")])
                .collect();
            if !shares.is_empty() {
                assert!(
                    shares.iter().max().unwrap() - shares.iter().min().unwrap()
                        <= limit("fairness_count_difference"),
                    "Unequal native same-class presentation: {counts:?}"
                );
            }
        }
    }
}

fn ordinary_demands(state: &Value, ordinary: u64) -> BTreeSet<u64> {
    (1..13)
        .filter_map(|slot| {
            let map = n(state, &format!("vNativeSource{slot}Map"));
            (map > 0 && map <= ordinary && n(state, &format!("vNativeSource{slot}Count")) > 0)
                .then_some(map)
        })
        .collect()
}
fn ordinary_presentations(state: &Value, ordinary: u64) -> BTreeSet<u64> {
    (1..13)
        .filter_map(|slot| {
            let map = n(state, &format!("vNativeSource{slot}Map"));
            (map > 0 && map <= ordinary && n(state, &format!("vNativeSource{slot}Complete")) == 1)
                .then_some(map)
        })
        .collect()
}
#[test]
fn native_presentation_oracle_excludes_doubles_and_retains_missing_demand() {
    let mut native = serde_json::json!({});
    let mut mobile = serde_json::json!({});
    for slot in 1..13 {
        for state in [&mut native, &mut mobile] {
            state[format!("vNativeSource{slot}Map")] = serde_json::json!(slot);
            state[format!("vNativeSource{slot}Count")] = serde_json::json!(u64::from(slot <= 7));
            state[format!("vNativeSource{slot}Complete")] = serde_json::json!(1);
        }
        if slot <= 4 {
            mobile[format!("vNativeSource{slot}Count")] = serde_json::json!(0);
            mobile[format!("vNativeSource{slot}Complete")] = serde_json::json!(0);
        }
    }
    assert_eq!(ordinary_demands(&native, 4), BTreeSet::from([1, 2, 3, 4]));
    assert!(ordinary_demands(&mobile, 4).is_empty());
    assert!(
        ordinary_presentations(&mobile, 4).is_empty(),
        "Avatar doubles must not satisfy ordinary NPC demand"
    );
    assert_eq!(
        ordinary_demands(&native, 4)
            .difference(&ordinary_presentations(&mobile, 4))
            .count(),
        4,
        "Mobile hiding or zeroing demand must not waive four native-visible NPCs"
    );
}
#[test]
fn no_temporal_rendering_without_actual_resource_conflict() {
    let e = evidence("empty");
    connected(&e);
    for c in clients(&e) {
        for state in steady(&c["mobile"]) {
            for p in 1..=3 {
                assert!(
                    n(state, &format!("vAcceptancePeer{p}Complete")) == 1,
                    "A conflict-free ready remote figure was omitted"
                );
            }
        }
    }
}
#[test]
fn seven_native_and_three_remote_palettes_never_substitute_colors() {
    let e = evidence("palettes");
    connected(&e);
    let mut requested: BTreeSet<_> = e["fixture"]["palettes"]
        .as_array()
        .unwrap()
        .iter()
        .map(|palette| {
            palette["encoded_day_colors"]
                .as_array()
                .unwrap()
                .iter()
                .fold(2166136261u32, |hash, byte| {
                    (hash ^ byte.as_u64().unwrap() as u32).wrapping_mul(16777619)
                })
        })
        .map(u64::from)
        .collect();
    for c in clients(&e) {
        requested.extend(
            steady(&c["single"])
                .filter(|s| n(s, "vAcceptanceLocalComplete") == 1)
                .map(|s| n(s, "vAcceptanceLocalPaletteHash")),
        );
    }
    assert_eq!(requested.len(),11,
        "The palette stress fixture did not establish seven distinct ordinary colors plus four distinct avatar colors");
    colors(&e);
    for c in &clients(&e)[..1] {
        for state in steady(&c["mobile"]) {
            for slot in 1..13 {
                if n(state, &format!("vNativeSource{slot}Complete")) == 0 {
                    continue;
                }
                let map = n(state, &format!("vNativeSource{slot}Map")) as usize;
                assert!((1..=12).contains(&map), "Unexpected native fixture object");
                let encoded = e["fixture"]["palettes"][(map - 1) % 7]["encoded_day_colors"]
                    .as_array()
                    .unwrap();
                let expected = encoded.iter().fold(2166136261u32, |hash, byte| {
                    (hash ^ byte.as_u64().unwrap() as u32).wrapping_mul(16777619)
                });
                assert_eq!(
                    n(state, &format!("vNativeSource{slot}PaletteHash")),
                    u64::from(expected),
                    "NPC {map}: hardware palette substitutes its current compiled day colors"
                );
            }
        }
        let native_colors: BTreeSet<_> = steady(&c["mobile"])
            .flat_map(|s| {
                (1..13)
                    .filter_map(move |slot| s[format!("vNativeSource{slot}PaletteHash")].as_u64())
            })
            .collect();
        assert!(native_colors.len()>=7,"The required seven unique ordinary colors were not actually presented: {native_colors:?}");
    }
}
#[test]
fn remote_appearance_matches_native_local_appearance() {
    let e = evidence("empty");
    connected(&e);
    colors(&e);
}
#[test]
fn remote_palette_matches_receiver_night_lighting() {
    let e = evidence("night");
    connected(&e);
    colors(&e);
}
#[test]
fn remote_palette_matches_receiver_forced_dark_lighting() {
    let e = evidence("dark");
    connected(&e);
    colors(&e);
}
#[test]
fn continuous_rendering_returns_when_scanline_conflict_disappears() {
    let e = evidence("clear-conflict");
    connected(&e);
    for c in clients(&e) {
        let states: Vec<_> = steady(&c["mobile"]).collect();
        assert!(
            states
                .iter()
                .any(|s| (1..=3).any(|p| n(s, &format!("vMobilePeer{p}OAM0")) >= 160)),
            "Initial scanline conflict was not exercised"
        );
        for state in states.iter().rev().take(100) {
            for p in 1..=3 {
                assert!(
                    n(state, &format!("vAcceptancePeer{p}Complete")) == 1,
                    "Conflict-free remote presentation did not become continuous"
                );
            }
        }
    }
}
#[test]
fn temporal_priority_is_npcs_then_ground_objects_then_remote_players() {
    let e = evidence("pool");
    connected(&e);
    for c in clients(&e) {
        let mut groups = [(0u64, 0u64); 3];
        for (state, baseline) in steady(&c["mobile"]).zip(steady(&c["single"])) {
            let ordinary = n(&e["fixture"], "ordinary_objects");
            let shown = ordinary_presentations(state, ordinary);
            for map in ordinary_demands(baseline, ordinary) {
                let group = if map <= 6 { 0 } else { 1 };
                groups[group].0 += u64::from(shown.contains(&map));
                groups[group].1 += 1;
            }
            for p in 1..=3 {
                if n(state, &format!("wMobilePeer{p}Visible")) == 1 {
                    groups[2].0 += u64::from(n(state, &format!("vAcceptancePeer{p}Complete")) == 1);
                    groups[2].1 += 1;
                }
            }
        }
        assert!(
            groups.iter().all(|(_, eligible)| *eligible > 0),
            "A required priority class was not exercised"
        );
        assert!(
            groups[0].0 * groups[1].1 >= groups[1].0 * groups[0].1,
            "Ground objects received a higher share than NPCs: {groups:?}"
        );
        assert!(
            groups[1].0 * groups[2].1 >= groups[2].0 * groups[1].1,
            "Remote players received a higher share than ground objects: {groups:?}"
        );
    }
}
fn colors(e: &Value) {
    let refs: BTreeMap<_, _> = clients(e)
        .iter()
        .enumerate()
        .map(|(i, c)| {
            (
                i as u64,
                steady(&c["single"])
                    .map(|s| n(s, "vAcceptanceLocalPaletteHash"))
                    .collect::<BTreeSet<_>>(),
            )
        })
        .collect();
    for c in clients(e) {
        let mut presented = BTreeSet::new();
        for state in steady(&c["mobile"]) {
            for p in 1..=3 {
                if n(state, &format!("vAcceptancePeer{p}Complete")) == 0 {
                    continue;
                }
                presented.insert(p);
                let avatar = n(state, &format!("wMobilePeer{p}Avatar"));
                let color = n(state, &format!("vMobilePeer{p}PaletteHash"));
                assert!(
                    refs[&avatar].contains(&color),
                    "Remote {p}, avatar {avatar}: substituted color {color}, expected {:?}",
                    refs[&avatar]
                );
            }
        }
        assert_eq!(
            presented,
            BTreeSet::from([1, 2, 3]),
            "Palette correctness requires actual presentation of every remote figure"
        );
    }
}
#[test]
fn palette_oracle_rejects_unpresented_peers() {
    let mut e = serde_json::json!({"clients": []});
    for avatar in 0..4 {
        let mut state = serde_json::json!({});
        for peer in 1..=3 {
            let other = (avatar + peer) % 4;
            state[format!("vAcceptancePeer{peer}Complete")] = serde_json::json!(0);
            state[format!("wMobilePeer{peer}Avatar")] = serde_json::json!(other);
            state[format!("vMobilePeer{peer}PaletteHash")] = serde_json::json!(100 + other);
        }
        e["clients"]
            .as_array_mut()
            .unwrap()
            .push(serde_json::json!({
                "single": {"profile": {"world_window_start": 0}, "frames": [
                    {"frame": 0, "state": {"vAcceptanceLocalPaletteHash": 100 + avatar}}
                ]},
                "mobile": {"profile": {"world_window_start": 0}, "frames": [
                    {"frame": 0, "state": state}
                ]}
            }));
    }
    assert!(
        std::panic::catch_unwind(|| colors(&e)).is_err(),
        "Zero presentation must not establish correct colors"
    );
    for client in e["clients"].as_array_mut().unwrap() {
        for peer in 1..=3 {
            client["mobile"]["frames"][0]["state"][format!("vAcceptancePeer{peer}Complete")] =
                serde_json::json!(1);
        }
    }
    colors(&e);
}
#[test]
fn remote_sprite_pixels_match_current_native_appearance() {
    let e = evidence("empty");
    connected(&e);
    let refs: BTreeMap<_, _> = clients(&e)
        .iter()
        .enumerate()
        .map(|(i, c)| {
            (
                i as u64,
                steady(&c["single"])
                    .filter(|s| n(s, "vAcceptanceLocalComplete") == 1)
                    .map(|s| n(s, "vAcceptanceLocalTileHash"))
                    .collect::<BTreeSet<_>>(),
            )
        })
        .collect();
    for c in clients(&e) {
        for s in steady(&c["mobile"]) {
            for p in 1..=3 {
                if n(s, &format!("vAcceptancePeer{p}Complete")) == 0 {
                    continue;
                }
                let avatar = n(s, &format!("wMobilePeer{p}Avatar"));
                assert!(
                    refs[&avatar].contains(&n(s, &format!("vAcceptancePeer{p}TileHash"))),
                    "Remote avatar pixels differ from its current native rendering"
                );
            }
        }
    }
}
#[test]
fn npc_conversation_and_native_event_execute_with_remote_players() {
    let e = evidence("interaction");
    connected(&e);
    assert_eq!(
        clients(&e)[0]["mobile"]["state"]["vAcceptanceDialogDone"],
        1
    );
    assert_eq!(
        clients(&e)[0]["single"]["state"]["vAcceptanceDialogDone"],
        1
    );
    assert_eq!(
        clients(&e)[0]["mobile"]["state"]["vStoryFlagsHash"],
        clients(&e)[0]["single"]["state"]["vStoryFlagsHash"]
    );
}
#[test]
fn scripted_native_npc_performance_matches_equal_native_actor_load() {
    let e = evidence("npc-movement");
    connected(&e);
    assert_eq!(e["fixture"]["equal_actor_load"], true);
    let completed_counts = |run: &Value| -> Vec<u64> {
        let first = n(&run["profile"], "world_window_start");
        let mut counts = vec![0; limit("profile_frames") as usize];
        assert_eq!(n(&run["profile"], "completed_native_update_overflow"), 0);
        for row in run["profile"]["completed_native_updates"]
            .as_array()
            .expect("Missing completed world-update evidence")
        {
            let frame = row[0].as_u64().unwrap();
            if frame >= first && frame < first + counts.len() as u64 {
                counts[(frame - first) as usize] += 1;
            }
        }
        counts
    };
    // Compare completed updates, rather than entries into a routine that can
    // straddle a frontend frame boundary. Keep equal actor load for performance.
    for (i, c) in clients(&e).iter().enumerate() {
        let mobile = completed_counts(&c["mobile"]);
        let native = completed_counts(&c["single"]);
        let (mut m, mut s) = (0, 0);
        for (frame, (&a, &b)) in mobile.iter().zip(&native).enumerate() {
            m += a;
            s += b;
            assert!(
                m >= s,
                "Client {i}: additional equal-load delay at frame {frame}"
            );
        }
        let occupied = |rows: &[u64]| rows.iter().filter(|&&count| count > 0).count();
        let max_gap = |rows: &[u64]| {
            let positions: Vec<_> = rows
                .iter()
                .enumerate()
                .filter(|(_, count)| **count > 0)
                .map(|(frame, _)| frame)
                .collect();
            positions
                .windows(2)
                .map(|pair| pair[1] - pair[0])
                .max()
                .unwrap_or(rows.len())
        };
        assert!(
            occupied(&mobile) >= occupied(&native),
            "Client {i}: additional dropped frames"
        );
        assert!(
            max_gap(&mobile) <= max_gap(&native),
            "Client {i}: longer native update gap"
        );
        // Dialogue closure in the equal-load control reloads three extra native
        // avatars and changes the engine's normal busy-VBlank credit. Preserve
        // the strict duplicate/cadence gate against the original NPC path.
        let reference = if i == 0 {
            &e["original_npc_control"]
        } else {
            &c["single"]
        };
        if let Some(error) =
            mobile_acceptance::contracts::cadence_regression(&completed_counts(reference), &mobile)
        {
            panic!("Client {i}: {error}");
        }
    }
}
#[test]
fn scripted_native_npc_simulation_matches_after_completed_world_updates() {
    let e = evidence("npc-movement");
    connected(&e);
    let original = &e["original_npc_control"];
    assert!(
        original.is_object(),
        "Missing independent original-NPC control"
    );
    let completed = |run: &Value| -> Vec<u64> {
        assert_eq!(n(&run["profile"], "completed_native_update_overflow"), 0);
        run["profile"]["completed_native_updates"]
            .as_array()
            .expect("Completed native object updates")
            .iter()
            .map(|row| row[1].as_u64().expect("Native simulation hash"))
            .collect()
    };
    let mobile = completed(&clients(&e)[0]["mobile"]);
    let native = completed(original);
    assert!(
        native.iter().collect::<BTreeSet<_>>().len() > 8,
        "The original native movement scenario did not actually move an NPC"
    );
    assert_eq!(
        mobile.len(),
        native.len(),
        "Original NPC update count changed"
    );
    let common = native.len();
    assert!(
        common as u64 >= limit("profile_frames"),
        "Insufficient completed simulation history"
    );
    assert_eq!(
        &mobile[..common],
        &native[..common],
        "Remote presentation changed original NPC simulation after a completed world update"
    );
}
#[test]
fn remote_figures_are_never_partially_selected_by_hardware() {
    for scene in ["scanline", "pool", "palettes"] {
        let e = evidence(scene);
        connected(&e);
        for c in clients(&e) {
            for state in steady(&c["mobile"]) {
                for p in 1..=3 {
                    assert_eq!(
                        n(state, &format!("vAcceptancePeer{p}Partial")),
                        0,
                        "A remote figure was partially drawn under {scene} pressure"
                    );
                }
            }
        }
    }
}
#[test]
fn native_map_transition_does_not_revive_stale_remote_records() {
    let e = evidence("warp");
    connected(&e);
    let state = &clients(&e)[0]["mobile"]["state"];
    assert_ne!(
        (state["wMapGroup"].clone(), state["wMapNumber"].clone()),
        (
            e["fixture"]["map"]["group"].clone(),
            e["fixture"]["map"]["number"].clone()
        )
    );
    for p in 1..=3 {
        assert_eq!(
            clients(&e)[0]["mobile"]["state"][format!("wMobilePeer{p}Visible")],
            0
        );
    }
}
#[test]
fn emergency_exit_in_overworld() {
    emergency(&evidence("exit"));
}
#[test]
fn emergency_exit_in_native_menu() {
    let e = evidence("exit-menu");
    assert_eq!(
        n(before_exit(&e), "vAcceptanceMenuActive"),
        1,
        "Exit input did not overlap the actual native StartMenu routine"
    );
    emergency(&e);
}
#[test]
fn emergency_exit_during_script_and_input_lock() {
    let e = evidence("exit-script");
    assert_eq!(
        n(before_exit(&e), "wInputFlags"),
        255,
        "The scripted native input lock was not active at exit"
    );
    emergency(&e);
}
#[test]
fn emergency_exit_while_native_save_runs() {
    let e = evidence("exit-save");
    emergency(&e);
    safety(&e);
    assert!(
        n(&clients(&e)[0]["mobile"]["profile"], "save_copy_count") >= 4,
        "No second native save was observed"
    );
    let run = &clients(&e)[0]["mobile"];
    let event = run["events"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|e| n(e, "button") == 2)
        .last()
        .unwrap();
    let at = n(event, "frame");
    assert!(
        frames(run)
            .iter()
            .any(|f| n(f, "frame") + 1 == at && n(&f["state"], "vAcceptanceSaveActive") == 1),
        "Exit input did not overlap active native serialization"
    );
}
#[test]
fn online_emulator_state_restored_without_adapter_remains_escapable() {
    let e = evidence("resume");
    connected(&e);
    let r = &e["recovery"];
    assert!(!r.is_null(), "Actual online core state was not restored");
    for row in frames(r)
        .iter()
        .filter(|r| n(r, "frame") >= 40 + limit("exit_frames"))
    {
        for key in [
            "wMobileOwner",
            "wMobileOnline",
            "wMobileWanted",
            "wMobilePeerCount",
        ] {
            assert_eq!(
                n(&row["state"], key),
                0,
                "Restored state cannot exit locally"
            );
        }
    }
    assert_ne!(r["state"]["wXCoord"], e["fixture"]["primary"][0]);
}
#[test]
fn link_cable_entry_disconnects_until_explicit_join() {
    let e = evidence("link");
    let run = &clients(&e)[0]["mobile"];
    assert!(
        frames(run)
            .iter()
            .any(|f| n(&f["state"], "wMobileOnline") == 1),
        "No multiplayer session before link-cable entry"
    );
    let after: Vec<_> = frames(run)
        .iter()
        .filter(|f| n(&f["state"], "vAcceptanceLinkEntered") > 0)
        .collect();
    assert!(
        !after.is_empty(),
        "The real LinkCommunications path was not entered"
    );
    for row in after {
        for key in ["wMobileOnline", "wMobileWanted", "wMobileOwner"] {
            assert_eq!(
                n(&row["state"], key),
                0,
                "Link cable did not retain serial ownership at frame {}",
                row["frame"]
            );
        }
    }
}
#[test]
fn save_bytes_match_native_and_serialization_preserves_both_copies() {
    let e = evidence("save");
    connected(&e);
    safety(&e);
    same_save(&clients(&e)[0], "Native save equality");
}
#[test]
fn ordinary_saves_load_in_both_counterpart_directions() {
    let e = evidence("save");
    let reloads = e["save_reload"].as_array().unwrap();
    assert_eq!(reloads.len(), 2);
    for reload in reloads {
        let state = &reload["result"]["state"];
        assert_eq!(
            state["vAcceptanceLoaded"], 1,
            "{} into {} failed native loading",
            reload["producer"], reload["consumer"]
        );
        assert_eq!(state["vAcceptanceLoadFailed"], 0);
        assert_eq!(state["wMapGroup"], e["fixture"]["map"]["group"]);
        assert_eq!(state["wXCoord"], e["fixture"]["primary"][0]);
    }
}
macro_rules! fault_tests { ($($name:ident => $mode:literal),*) => { $(#[test] fn $name() {
    let e=evidence(concat!("fault-",$mode));
    assert!(n(&e,"injected")>0,"Fault was never delivered to real ROM");
    safety(&e); native_preserved(&e);
})* }; }
fault_tests!(invalid_magic_is_safe=>"magic",invalid_version_is_safe=>"version",oversized_payload_is_safe=>"length",
    unknown_packet_type_is_safe=>"kind",malformed_roster_is_safe=>"roster",partial_packet_is_safe=>"partial",
    silent_server_is_safe=>"silence",sustained_packet_flood_preserves_native_state=>"flood");
macro_rules! fault_recovery_tests { ($($name:ident => $fault:literal),*) => { $(#[test] fn $name() {
        let e = evidence(concat!("fault-",$fault));
        assert!(n(&e, "injected") > 0);
        emergency(&e);
        safety(&e);
        let p = &clients(&e)[0]["mobile"]["profile"];
        assert!(
            n(p, "save_copy_count") >= 4,
            "No post-fault native main/backup save was actually executed"
        );
        same_save(&clients(&e)[0], concat!("Transport fault ",$fault));
})* }; }
fault_recovery_tests!(magic_fault_preserves_save_and_exit=>"magic",version_fault_preserves_save_and_exit=>"version",
    length_fault_preserves_save_and_exit=>"length",unknown_kind_preserves_save_and_exit=>"kind",roster_fault_preserves_save_and_exit=>"roster",
    partial_packet_preserves_save_and_exit=>"partial",silent_server_preserves_save_and_exit=>"silence",packet_flood_preserves_save_and_exit=>"flood");
macro_rules! private_fault_tests { ($($name:ident => $value:literal),*) => { $(#[test] fn $name() {
    let e=evidence(concat!("private-",$value)); safety(&e);
    assert_eq!(n(before_exit(&e),"wMobileWanted"), $value.parse::<u64>().unwrap(),
        "The private fault was already cleared before the escape request");
    assert_eq!(n(before_exit(&e),"vAcceptanceSaveActive"),1,
        "The malformed private state did not overlap actual native serialization");
    emergency(&e);
    assert!(n(&clients(&e)[0]["mobile"]["profile"],"save_copy_count")>=4,"The invalid-private-state fixture did not reach the native save");
    same_save(&clients(&e)[0],concat!("Private error state ",$value));
})* }; }
private_fault_tests!(unknown_private_state_2_remains_escapable=>"2",unknown_private_state_7_remains_escapable=>"7",unknown_private_state_255_remains_escapable=>"255");
#[test]
fn temporary_server_disconnect_reconnects_automatically() {
    let e = evidence("fault-reconnect");
    assert!(n(&e, "injected") > 0);
    connected(&e);
    assert!(
        n(&e, "recovered") > 0,
        "No server response crossed a new connection after the controlled blackout"
    );
    safety(&e);
    native_preserved(&e);
}
#[test]
fn single_client_transport_reset_reconnects_without_another_join_input() {
    let e = evidence("fault-reconnect-one");
    assert_eq!(
        n(&e, "injected"),
        1,
        "Exactly one established connection must be reset"
    );
    connected(&e);
    assert!(
        n(&e, "recovered") > 0,
        "The reset client sent no successful request through a new connection"
    );
    safety(&e);
    native_preserved(&e);
}
