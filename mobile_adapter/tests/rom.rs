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
fn normal_speed_receiver_presentation(e: &Value, speeds: &[u64]) {
    assert_eq!(e["speeds"], serde_json::json!(speeds));
    connected(e);
    colors(e);
    let references: BTreeMap<_, _> = clients(e)
        .iter()
        .enumerate()
        .map(|(avatar, client)| {
            let native: Vec<_> = steady(&client["single"])
                .filter(|s| n(s, "vAcceptanceLocalComplete") == 1)
                .collect();
            let tiles: BTreeSet<_> = native
                .iter()
                .map(|s| n(s, "vAcceptanceLocalTileHash"))
                .collect();
            let palettes: BTreeSet<_> = native
                .iter()
                .map(|s| n(s, "vAcceptanceLocalPaletteHash"))
                .collect();
            assert!(
                !tiles.is_empty() && !palettes.is_empty(),
                "Avatar {avatar}: no complete native appearance/color reference"
            );
            (avatar as u64, (tiles, palettes))
        })
        .collect();
    let receiver: Vec<_> = steady(&clients(e)[0]["mobile"]).collect();
    assert_eq!(receiver.len() as u64, limit("profile_frames"));
    for (sender_index, sender) in clients(e).iter().enumerate().skip(1) {
        let run = &sender["mobile"];
        let first = n(&run["profile"], "world_window_start");
        assert!(
            run["events"]
                .as_array()
                .expect("Physical input events")
                .iter()
                .any(|event| {
                    // These are the declared libretro UP/DOWN physical button IDs.
                    matches!(n(event, "button"), 4 | 5)
                        && n(event, "duration") > 0
                        && n(event, "frame") >= first
                        && n(event, "frame") < first + limit("profile_frames")
                }),
            "Sender {sender_index}: no measured physical movement input"
        );
        let motion: BTreeSet<_> = steady(run)
            .map(|s| (n(s, "wXCoord"), n(s, "wYCoord")))
            .collect();
        assert!(
            motion.len() > 1,
            "Sender {sender_index}: physical input did not move the native player"
        );
        let id = n(&run["state"], "wMobilePlayerId");
        let generation = n(&run["state"], "wMobileGeneration");
        let mut presented_positions = BTreeSet::new();
        let mut peer_slots = BTreeSet::new();
        for state in &receiver {
            assert_eq!(n(state, "vAcceptanceReady"), 1);
            assert_eq!(n(state, "wMobileOnline"), 1);
            assert_eq!(n(state, "wMobilePeerCount"), 3);
            let peers: Vec<_> = (1..=3)
                .filter(|p| n(state, &format!("wMobilePeer{p}Id")) == id)
                .collect();
            assert_eq!(
                peers.len(),
                1,
                "Sender {sender_index}: missing/duplicate receiver identity"
            );
            let p = peers[0];
            peer_slots.insert(p);
            assert_eq!(n(state, &format!("wMobilePeer{p}Generation")), generation);
            assert_eq!(
                n(state, &format!("wMobilePeer{p}Avatar")),
                sender_index as u64
            );
            assert_eq!(n(state, &format!("wMobilePeer{p}Visible")), 1);
            assert_eq!(
                n(state, &format!("vAcceptancePeer{p}Complete")),
                1,
                "Sender {sender_index}: normal-speed receiver omitted a complete figure"
            );
            assert_eq!(n(state, &format!("vAcceptancePeer{p}Partial")), 0);
            let (tiles, palettes) = &references[&(sender_index as u64)];
            assert!(
                tiles.contains(&n(state, &format!("vAcceptancePeer{p}TileHash"))),
                "Sender {sender_index}: presented pixels differ from current native appearance"
            );
            assert!(
                palettes.contains(&n(state, &format!("vMobilePeer{p}PaletteHash"))),
                "Sender {sender_index}: presented colors differ from current native palette"
            );
            // The existing observer reads these coordinates from actual video OAM.
            // Complete selection above prevents a cached/hidden figure proving motion.
            presented_positions.insert((
                n(state, &format!("vMobilePeer{p}OAM0")),
                n(state, &format!("vMobilePeer{p}OAM1")),
            ));
        }
        assert_eq!(
            peer_slots.len(),
            1,
            "Sender {sender_index}: receiver record changed identity"
        );
        assert!(
            presented_positions.len() > 1,
            "Sender {sender_index}: no actual presented coordinate change at normal speed"
        );
    }
}
#[test]
fn normal_speed_receiver_presents_mixed_speed_remote_figures() {
    normal_speed_receiver_presentation(&evidence("speeds"), &[1, 2, 3, 4]);
}
#[test]
fn normal_speed_receiver_presents_normal_speed_remote_figures() {
    normal_speed_receiver_presentation(&evidence("speeds-100"), &[1, 1, 1, 1]);
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
fn mobile_menu_has_text(e: &Value, state: &Value, text: &str) -> bool {
    let expected: Vec<_> = text
        .chars()
        .map(|c| n(&e["menu_charmap"], &c.to_string()))
        .collect();
    (0..18).any(|row| {
        let tiles: Vec<_> = (0..20)
            .map(|column| {
                state[format!("vMobileMenuTile{}", row * 20 + column)]
                    .as_u64()
                    .unwrap_or(0)
            })
            .collect();
        tiles.windows(expected.len()).any(|span| span == expected)
    })
}

// Require a complete decimal token. A label plus a numeric prefix of a longer
// ID is insufficient; native roster entries end at a blank tile or row edge.
// Native TextboxBorder uses the left middle tile "│" beside first-column labels.
fn mobile_menu_has_id(e: &Value, state: &Value, text: &str) -> bool {
    let expected: Vec<_> = text
        .chars()
        .map(|c| n(&e["menu_charmap"], &c.to_string()))
        .collect();
    let blank = n(&e["menu_charmap"], " ");
    let left_border = n(&e["menu_charmap"], "│");
    (0..18).any(|row| {
        let tiles: Vec<_> = (0..20)
            .map(|column| n(state, &format!("vMobileMenuTile{}", row * 20 + column)))
            .collect();
        tiles
            .windows(expected.len())
            .enumerate()
            .any(|(column, span)| {
                span == expected
                    && (column == 0
                        || tiles[column - 1] == blank
                        || tiles[column - 1] == left_border)
                    && (column + expected.len() == 20 || tiles[column + expected.len()] == blank)
            })
    })
}

fn actual_mobile_menu(e: &Value) {
    let run = &clients(e)[0]["mobile"];
    let active: Vec<_> = frames(run)
        .iter()
        .map(|f| &f["state"])
        .filter(|s| s["wMobileMenuState"] == 1)
        .collect();
    assert!(
        !active.is_empty(),
        "Select+A never entered the actual Mobile menu"
    );
    assert!(
        active.iter().all(|s| n(s, "vMobileMenuSourceBank") == 134),
        "Menu source did not remain in relocated addon bank 134"
    );
    assert!(
        active.iter().any(|s| {
            n(s, "vMobileMenuInstructionActive") == 1
                && n(s, "vAcceptanceMenuSourceBank") == 134
                && n(s, "vAcceptanceMenuSourcePC") == n(s, "vMobileMenuSourcePC")
                && n(s, "vMobileMenuSourcePC") >= 0x4000
                && n(s, "vMobileMenuSourcePC") < 0x8000
        }),
        "No actual execution in the relocated Mobile menu bank was observed"
    );
    assert!(
        active
            .iter()
            .any(|s| n(s, "hInMenu") != 0 && mobile_menu_has_text(e, s, "PLAYERS")),
        "Native menu ownership and rendered options were not observed together"
    );
    let final_state = &run["state"];
    for key in [
        "wMobileMenuState",
        "hInMenu",
        "hJoypadDown",
        "hJoyPressed",
        "hCrashCode",
    ] {
        assert_eq!(
            n(final_state, key),
            0,
            "Native menu/joypad recovery failed: {key}"
        );
    }
    assert!(
        n(final_state, "hROMBank") < 128,
        "Addon ROM bank leaked after menu close"
    );
    assert_ne!(
        final_state["wXCoord"], e["fixture"]["primary"][0],
        "Native movement did not resume"
    );

    // Recovery must really present all three independent peers, with current
    // native pixel and palette references, after the final physical movement.
    let last_input = run["events"]
        .as_array()
        .unwrap()
        .iter()
        .map(|event| n(event, "frame") + n(event, "duration"))
        .max()
        .unwrap();
    let recovery: Vec<_> = frames(run)
        .iter()
        .filter(|f| n(f, "frame") > last_input + 30)
        .collect();
    assert!(recovery.len() > 1, "No post-menu native recovery window");
    assert!(
        n(&recovery.last().unwrap()["state"], "vProfileWorldUpdates")
            > n(&recovery[0]["state"], "vProfileWorldUpdates"),
        "Native world simulation did not resume"
    );
    let mut presented = BTreeSet::new();
    for state in frames(run)
        .iter()
        .filter(|f| n(f, "frame") > last_input + 30)
        .map(|f| &f["state"])
    {
        for peer in 1..=3 {
            if n(state, &format!("vAcceptancePeer{peer}Complete")) == 0 {
                continue;
            }
            presented.insert(peer);
            let avatar = n(state, &format!("wMobilePeer{peer}Avatar")) as usize;
            let reference = &clients(e)[avatar]["single"];
            for (actual, expected) in [
                (
                    format!("vAcceptancePeer{peer}TileHash"),
                    "vAcceptanceLocalTileHash",
                ),
                (
                    format!("vMobilePeer{peer}PaletteHash"),
                    "vAcceptanceLocalPaletteHash",
                ),
            ] {
                assert!(
                    frames(reference)
                        .iter()
                        .any(|f| n(&f["state"], "vAcceptanceLocalComplete") == 1
                            && n(&f["state"], expected) == n(state, &actual)),
                    "Post-menu remote pixels/colors changed: {actual}"
                );
            }
        }
    }
    assert_eq!(
        presented,
        BTreeSet::from([1, 2, 3]),
        "Post-menu remote recovery was never actually presented"
    );
}

#[test]
fn mobile_menu_offline_connect_enters_four_player_lobby() {
    let e = evidence("mobile-menu-connect");
    actual_mobile_menu(&e);
    let ids: BTreeSet<_> = clients(&e)
        .iter()
        .map(|c| {
            assert_eq!(n(&c["mobile"]["state"], "wMobileOnline"), 1);
            assert_eq!(n(&c["mobile"]["state"], "wMobilePeerCount"), 3);
            n(&c["mobile"]["state"], "wMobilePlayerId")
        })
        .collect();
    assert_eq!(ids.len(), 4);
    assert!(
        frames(&clients(&e)[0]["mobile"])
            .iter()
            .any(|f| f["state"]["wMobileMenuState"] == 1
                && f["state"]["wMobileOnline"] == 0
                && mobile_menu_has_text(
                    &e,
                    &f["state"],
                    e["menu_text"]["OfflineText"].as_str().unwrap()
                )),
        "Offline status was not rendered before CONNECT"
    );
}

#[test]
fn mobile_menu_players_renders_all_four_independent_session_ids() {
    let e = evidence("mobile-menu-players");
    actual_mobile_menu(&e);
    let run = &clients(&e)[0]["mobile"];
    let roster = frames(run)
        .iter()
        .map(|f| &f["state"])
        .find(|s| {
            s["wMobileMenuState"] == 1
                && mobile_menu_has_text(&e, s, e["menu_text"]["RosterText"].as_str().unwrap())
        })
        .expect("Actual native roster text never appeared");
    let ids: BTreeSet<_> = clients(&e)
        .iter()
        .map(|c| n(&c["mobile"]["state"], "wMobilePlayerId"))
        .collect();
    assert_eq!(ids.len(), 4);
    let staged: BTreeSet<_> = (0..4)
        .map(|i| n(roster, &format!("vMobileMenuRoster{i}")))
        .collect();
    assert_eq!(
        staged, ids,
        "Roster staging did not contain local plus three independent 16-bit IDs"
    );
    for i in 0..4 {
        let id = n(roster, &format!("vMobileMenuRoster{i}"));
        let label = if i == 0 {
            "YOU: ".to_string()
        } else {
            format!("{i}: ")
        };
        // TextCommand_DECIMAL sets PRINTNUM_LEFTALIGN, so PrintNum skips
        // leading spaces even though text_decimal allows five digits.
        assert!((1..=u16::MAX as u64).contains(&id));
        let text = format!("{label}{id}");
        assert!(
            frames(run)
                .iter()
                .any(|f| f["state"]["wMobileMenuState"] == 1
                    && mobile_menu_has_id(&e, &f["state"], &text)),
            "Actual roster never rendered {text}"
        );
    }
    assert!(
        frames(run).iter().any(|f| mobile_menu_has_text(
            &e,
            &f["state"],
            e["menu_text"]["OnlineText"].as_str().unwrap()
        )),
        "Online status was not rendered"
    );
}

#[test]
fn mobile_menu_cancel_restores_native_world_and_remote_graphics() {
    actual_mobile_menu(&evidence("mobile-menu-cancel"));
}
#[test]
fn mobile_menu_b_restores_native_world_and_remote_graphics() {
    actual_mobile_menu(&evidence("mobile-menu-b"));
}

fn mobile_menu_disconnect_suppression(case: &str, roster: bool) {
    let e = evidence(case);
    actual_mobile_menu(&e);
    let run = &clients(&e)[0]["mobile"];
    let selects: Vec<_> = run["events"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|event| n(event, "button") == 2)
        .collect();
    let rejoin = n(selects.last().unwrap(), "frame");
    let first = if case == "mobile-menu-disconnect" {
        frames(run)
            .iter()
            .find(|f| n(f, "frame") > n(selects[1], "frame") && f["state"]["wMobileWanted"] == 0)
            .map(|f| n(f, "frame") + limit("exit_frames"))
            .expect("DISCONNECT never cleared connection intent")
    } else {
        let held = selects
            .iter()
            .find(|event| n(event, "duration") == 160)
            .expect("Held emergency chord absent");
        let before = &frames(run)
            .iter()
            .find(|f| n(f, "frame") + 1 == n(held, "frame"))
            .unwrap()["state"];
        assert_eq!(
            n(before, "wMobileMenuState"),
            1,
            "Emergency chord missed actual Mobile menu"
        );
        if roster {
            assert!(
                mobile_menu_has_text(&e, before, e["menu_text"]["RosterText"].as_str().unwrap()),
                "Emergency chord missed actual roster"
            );
        }
        n(held, "frame") + limit("exit_frames")
    };
    let suppressed: Vec<_> = frames(run)
        .iter()
        .filter(|f| n(f, "frame") >= first && n(f, "frame") < rejoin)
        .collect();
    assert!(
        !suppressed.is_empty(),
        "No suppression interval before new explicit join"
    );
    for f in suppressed {
        for key in [
            "wMobileWanted",
            "wMobileOnline",
            "wMobilePeerCount",
            "wMobileOwner",
        ] {
            assert_eq!(
                n(&f["state"], key),
                0,
                "Held chord or released teardown revived {key}"
            );
        }
    }
    assert_eq!(
        n(&run["state"], "wMobileOnline"),
        1,
        "New explicit join did not reconnect"
    );
}
#[test]
fn mobile_menu_disconnect_requires_new_explicit_join() {
    mobile_menu_disconnect_suppression("mobile-menu-disconnect", false);
}
#[test]
fn mobile_menu_priority_exit_suppresses_held_chord_rejoin() {
    mobile_menu_disconnect_suppression("mobile-menu-exit", false);
}
#[test]
fn mobile_menu_roster_priority_exit_suppresses_held_chord_rejoin() {
    mobile_menu_disconnect_suppression("mobile-menu-exit-roster", true);
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

fn identity_bytes(text: &str) -> Vec<u8> {
    assert_eq!(text.len() % 2, 0, "Incomplete wire hex");
    (0..text.len())
        .step_by(2)
        .map(|at| u8::from_str_radix(&text[at..at + 2], 16).expect("Actual wire hex"))
        .collect()
}

fn native_identity_wire(e: &Value, mismatch: Option<&str>) {
    use mobile_protocol::lobby::{ERROR, HELLO, SCHEMA, WELCOME};
    let expected = identity_bytes(e["clean_counterpart_sha256"].as_str().unwrap());
    assert_eq!(expected.len(), 32);
    let wire = e["identity_wire"]
        .as_array()
        .expect("Actual proxy wire evidence");
    assert!(!wire.is_empty());
    let mut healthy = BTreeSet::new();
    let mut rejected = BTreeSet::new();
    for item in wire {
        let frame = identity_bytes(item["frame"].as_str().unwrap());
        assert!(frame.len() >= 8);
        assert_eq!(&frame[..3], b"PC\x01");
        assert_eq!(
            frame.len(),
            8 + u16::from_be_bytes([frame[6], frame[7]]) as usize
        );
        if item["direction"] == "native-to-bridge" && frame[3] == HELLO {
            assert_eq!(frame.len(), 55, "Actual outgoing HELLO length");
            let connection = n(item, "connection");
            if &frame[23..55] == expected.as_slice() && frame[8] == SCHEMA {
                healthy.insert(connection);
            } else {
                match mismatch {
                    Some("digest") => {
                        assert_eq!(frame[8], SCHEMA, "Digest negative changed schema");
                        let mut declared = expected.clone();
                        declared[0] ^= 1;
                        assert_eq!(&frame[23..55], declared.as_slice());
                    }
                    Some("schema") => {
                        assert_eq!(
                            &frame[23..55],
                            expected.as_slice(),
                            "Schema negative changed digest"
                        );
                        assert_eq!(frame[8], SCHEMA ^ 0xff);
                    }
                    _ => panic!("Actual native HELLO did not match clean counterpart"),
                }
                rejected.insert(connection);
            }
        }
    }
    assert_eq!(healthy.len(), if mismatch.is_some() { 3 } else { 4 });
    assert_eq!(rejected.len(), usize::from(mismatch.is_some()));
    for connection in &healthy {
        assert!(
            wire.iter().any(|item| n(item, "connection") == *connection
                && item["direction"] == "bridge-to-native"
                && identity_bytes(item["frame"].as_str().unwrap())[3] == WELCOME),
            "Matching native stream never received authoritative WELCOME"
        );
    }
    for connection in &rejected {
        let replies: Vec<_> = wire
            .iter()
            .filter(|item| {
                n(item, "connection") == *connection && item["direction"] == "bridge-to-native"
            })
            .map(|item| identity_bytes(item["frame"].as_str().unwrap()))
            .collect();
        assert!(
            replies
                .iter()
                .any(|frame| frame[3] == ERROR && frame.len() == 9 && frame[8] == 2),
            "Declared mismatch did not receive authoritative incompatible rejection"
        );
        assert!(
            !replies.iter().any(|frame| frame[3] == WELCOME),
            "Rejected candidate received successful admission"
        );
    }
}

#[test]
fn native_identity_hello_matches_clean_counterpart() {
    let e = evidence("identity-match");
    native_identity_wire(&e, None);
    connected(&e);
    safety(&e);
}

fn native_identity_rejection(kind: &str) {
    let e = evidence(&format!("identity-{kind}"));
    native_identity_wire(&e, Some(kind.trim_end_matches("-held")));
    safety(&e);
    native_preserved(&e);
    let candidate = &clients(&e)[0]["mobile"];
    for row in frames(candidate) {
        assert_eq!(
            n(&row["state"], "wMobileOnline"),
            0,
            "Rejected candidate participated"
        );
        assert_eq!(
            n(&row["state"], "wMobilePeerCount"),
            0,
            "Rejected candidate acquired peers"
        );
    }
    if kind == "digest-held" {
        assert_eq!(
            n(before_exit(&e), "wMobileWanted"),
            1,
            "Held rejection did not retain a live native join for physical escape"
        );
        emergency(&e);
    } else {
        assert!(
            frames(candidate)
                .iter()
                .any(|row| n(&row["state"], "wMobileServerError") == 2),
            "Native ROM never consumed the incompatible ERROR payload"
        );
        for state in steady(candidate) {
            for key in [
                "wMobileWanted",
                "wMobileOwner",
                "wMobileOnline",
                "wMobilePeerCount",
            ] {
                assert_eq!(n(state, key), 0, "Native ERROR did not tear down {key}");
            }
        }
        assert_ne!(
            candidate["state"]["wXCoord"], e["fixture"]["primary"][0],
            "Rejected native client did not resume ordinary movement"
        );
    }
    let mut ids = BTreeSet::new();
    for client in &clients(&e)[1..] {
        for state in steady(&client["mobile"]) {
            assert_eq!(
                n(state, "wMobileOnline"),
                1,
                "Healthy sibling lost admission"
            );
            assert_eq!(
                n(state, "wMobilePeerCount"),
                2,
                "Rejected candidate changed healthy membership"
            );
        }
        ids.insert(n(&client["mobile"]["state"], "wMobilePlayerId"));
    }
    assert_eq!(ids.len(), 3, "Healthy native identities were merged");
}

#[test]
fn native_identity_declared_digest_rejected_before_participation() {
    native_identity_rejection("digest");
}

#[test]
fn native_identity_schema_rejected_independently() {
    native_identity_rejection("schema");
}

#[test]
fn native_identity_held_rejection_remains_physically_escapable() {
    native_identity_rejection("digest-held");
}

fn effect_events(run: &Value) -> &[Value] {
    assert_eq!(n(&run["profile"], "native_effect_event_overflow"), 0);
    run["profile"]["native_effect_events"].as_array().expect("Native instruction events")
}
fn effect_field(event: &Value, field: usize) -> u64 {
    event["fields"][field].as_u64().expect("Native event field")
}
fn effect_calls(run: &Value, span: u64, kind: u64) -> Vec<&Value> {
    effect_events(run).iter().filter(|e| effect_field(e, 2) == span && effect_field(e, 3) == kind).collect()
}
fn effect_bytes(value: &Value) -> Vec<u8> {
    value.as_array().expect("Captured bytes").iter().map(|b| u8::try_from(b.as_u64().unwrap()).unwrap()).collect()
}
fn headbutt_lifecycle(e: &Value) {
    assert_eq!(clients(e).len(), 4);
    let mut ids = BTreeSet::new();
    for (i, client) in clients(e).iter().enumerate() {
        let m = &client["mobile"];
        ids.insert(n(&m["state"], "wMobilePlayerId"));
        assert_eq!(n(&m["state"], "hCrashCode"), 0);
        assert_eq!(n(&m["state"], "wMobileOnline"), 1);
        assert_eq!(n(&m["state"], "wMobilePeerCount"), 3);
        assert_eq!(n(&m["state"], "vAcceptanceNativeSceneReady"), 1);
        assert!(n(&m["state"], "vProfileWorldUpdates") > 0);
        for run in [&client["mobile"], &client["single"]] {
            assert_eq!(n(&run["state"], "vAcceptancePartyCount"), 1);
            assert_eq!(n(&run["state"], "vAcceptancePartyMove"), 29);
            assert_eq!(effect_calls(run, 0, 1).len(), usize::from(i == 0));
            assert_eq!(effect_calls(run, 0, 2).len(), usize::from(i == 0));
            assert_eq!(n(&run["profile"], "completed_native_update_overflow"), 0);
            assert!(!run["profile"]["completed_native_updates"].as_array().unwrap().is_empty());
        }
    }
    assert_eq!(ids.len(), 4, "Four genuine independent native clients required");
    for run in [&clients(e)[0]["mobile"], &clients(e)[0]["single"]] {
        assert_eq!(effect_calls(run, 6, 1).len(), 1, "Physical native TryHeadbuttOW path");
        for path in ["AskHeadbuttScript", "AutoHeadbuttScript"] {
            let point = &run["profile"]["native_effect_script_bindings"][path];
            assert!(effect_calls(run, 7, 1).iter().any(|event| {
                let watch = effect_bytes(&event["watches"]["6"]);
                u64::from(watch[0]) == point[0].as_u64().unwrap()
                    && u64::from(u16::from_le_bytes([watch[1], watch[2]])) == point[1].as_u64().unwrap()
            }), "Interpreter did not execute {path}");
        }
    }
    let run = &clients(e)[0]["mobile"];
    let start = n(&run["state"], "vAcceptanceHeadbuttTileStart");
    let count = n(&run["state"], "vAcceptanceHeadbuttTileCount");
    let requests: Vec<_> = effect_calls(run, 2, 1).into_iter().filter(|event| effect_field(event, 10) == start).collect();
    assert_eq!(requests.len(), 1, "One native effect graphics transfer required");
    for entry in requests {
        assert_eq!(effect_field(entry, 8) & 255, count);
        assert_eq!(effect_field(entry, 11) & 1, 0);
        let ret = effect_calls(run, 2, 2).into_iter().find(|r| effect_field(r, 0) > effect_field(entry, 0)).unwrap();
        for field in 7..13 { assert_eq!(effect_field(entry, field), effect_field(ret, field), "Native return register/selector {field}"); }
        assert_eq!(effect_bytes(&ret["watches"]["5"]), [1]);
    }
    for event in effect_events(run) {
        if effect_field(event, 2) == 4 && effect_field(event, 3) == 3
            && effect_field(event, 16) & 128 == 0 && effect_field(event, 21) & 128 != 0 {
            assert_eq!(effect_bytes(&event["watches"]["5"]), [0], "Private GDMA during native graphics ownership");
        }
    }
    assert!(effect_calls(run, 3, 2).iter().any(|event| {
        effect_bytes(&event["watches"]["5"]) == [0]
            && effect_bytes(&event["watches"]["1"]) == [0]
            && effect_bytes(&event["watches"]["2"]) == [0]
            && effect_bytes(&event["watches"]["4"]).iter().all(|b| *b == 0)
    }), "Native graphics release did not complete after the actual effect");
}

// The supported palette conversion is independently evaluated from raw RGB15.
// Configuration comes from actual map_color calls, never option defaults.
fn effect_logical_color(raw: u16, display: &[u64]) -> u16 {
    assert_eq!(display[17], 1, "Missing actual palette conversion observation");
    assert_eq!(display[18], 11, "Only actual GBC RGB565 correction without GBA is supported");
    assert_eq!(display[20], 0, "Non-neutral actual light temperature is unsupported");
    assert_eq!(display[21], 0, "Changed cached palette configuration is unsupported");
    let r = u32::from(raw & 31); let g = u32::from((raw >> 5) & 31); let b = u32::from((raw >> 10) & 31);
    let (r, g, b) = match display[19] {
        1 => (((r * 13 + g * 2 + b) >> 4), ((g * 3 + b) >> 2), ((r * 3 + g * 2 + b * 11) >> 4)),
        2 => {
            let inv = 1.0f32 / 31.0f32;
            let gamma = 2.2f32 - 0.5f32;
            let rr = (r as f32 * inv).powf(gamma);
            let gg = (g as f32 * inv).powf(gamma);
            let bb = (b as f32 * inv).powf(gamma);
            let channel = |v: f32| -> u32 {
                ((v.clamp(0.0, 1.0).powf(1.0f32 / 2.2f32).clamp(0.0, 1.0) * 31.0 + 0.5) as u32) & 31
            };
            (channel(0.94f32 * ((0.82f32 * rr) + (0.24f32 * gg) + (-0.06f32 * bb))),
             channel(0.94f32 * ((0.125f32 * rr) + (0.665f32 * gg) + (0.21f32 * bb))),
             channel(0.94f32 * ((0.195f32 * rr) + (0.075f32 * gg) + (0.73f32 * bb))))
        },
        _ => panic!("Unsupported actual palette correction mode"),
    };
    ((r << 11) | (g << 6) | b) as u16
}
fn effect_display_metadata(frame: &Value) -> Vec<u64> {
    let d: Vec<_> = frame["display_metadata"].as_array().expect("Missing actual display metadata")
        .iter().map(|v| v.as_u64().expect("Invalid display metadata integer")).collect();
    assert_eq!(d.len(), 24, "Display ABI field count"); assert_eq!(d[0], 1); assert_eq!(d[23], 24);
    assert!(d[1] > 0, "Missing actual output invocation");
    assert_eq!(d[2], frame["render_metadata"][1].as_u64().unwrap(), "Stale output/render binding");
    assert_eq!(d[3], frame["render_metadata"][2].as_u64().unwrap(), "Stale hardware source binding");
    assert_eq!(d[4], 144, "Display call cannot invent rendered rows");
    assert_eq!(&d[5..9], &[160, 144, 16, 320]);
    assert!(d[9] <= 1 && d[10] <= 1 && d[12] <= 1);
    assert_eq!(&d[13..17], &[1, 0, 1, 0], "Unsupported actual display pipeline");
    assert_eq!(d[22], 0, "Invalid actual row observation");
    let factor = f32::from_bits(u32::try_from(d[11]).unwrap());
    assert!(factor.is_finite() && (1.0..=2.0).contains(&factor), "Unsupported interlacing factor");
    if d[9] != 0 { assert_eq!(d[10], 1, "Linear interlacing is unsupported"); }
    d
}
fn effect_physical_color(logical: u16, y: usize, display: &[u64]) -> u16 {
    if display[9] == 0 || y % 2 != display[12] as usize { return logical; }
    let factor = f32::from_bits(u32::try_from(display[11]).unwrap());
    let channel = |v: u16, max: u16| ((v as f32 * factor) as u16).min(max);
    (channel((logical >> 11) & 31, 31) << 11) |
        (channel((logical >> 5) & 63, 63) << 5) | channel(logical & 31, 31)
}
fn effect_exact_pixel(raw: u16, mapped: u16, physical: u16, y: usize, display: &[u64]) {
    assert_eq!(mapped, effect_logical_color(raw, display), "Raw intended logical color differs from actual mapped palette");
    assert_eq!(physical, effect_physical_color(mapped, y, display), "Exact final physical RGB565 differs");
}

#[test]
fn actual_display_exact_full_background_and_corruption_regressions() {
    use serde_json::json;
    // Independently specified source bytes and final pixels, not oracle outputs.
    let frame = json!({"render_metadata": [1, 7, 12], "display_metadata":
        [1, 9, 7, 12, 144, 160, 144, 16, 320, 1, 1, 1065520988u32, 0, 1, 0, 1, 0, 1, 11, 2, 0, 0, 0, 24]});
    let display = effect_display_metadata(&frame);
    let mut vram = vec![0u8; 0x4000];
    vram[0x1800..0x1c00].fill(1); vram[0x3800..0x3c00].fill(2);
    let palette_raw = [246u8, 43]; let palette_mapped = [209u8, 206];
    let mut checked = 0;
    for y in 0..144 { for x in 0..160 {
        let address = 0x1800 + y / 8 * 32 + x / 8;
        assert_eq!(vram[0x2000 + address], 2);
        let color = effect_tile_pixel(&vram, 256 + vram[address] as usize, 0, x % 8, y % 8);
        assert_eq!(color, 0);
        let physical = if y % 2 == 0 { 52977 } else { 52945 };
        effect_exact_pixel(u16::from_le_bytes(palette_raw), u16::from_le_bytes(palette_mapped), physical, y, &display);
        checked += 1;
    }}
    assert_eq!(checked, 23040);
    let rejected = |raw, mapped, pixel, y, d: &[u64]| assert!(std::panic::catch_unwind(|| effect_exact_pixel(raw, mapped, pixel, y, d)).is_err());
    rejected(0, 0, 52977, 0, &display); // Changed raw and matching mapped, original pixels.
    rejected(0, 52945, 52977, 0, &display); // Raw-only corruption.
    rejected(11254, 47050, 47050, 0, &display); // Stale startup fallback cache, final Accurate configuration.
    let mut parity = display.clone(); parity[12] = 1; rejected(11254, 52945, 52977, 0, &parity);
    let mut factor = display.clone(); factor[11] = u64::from(1.0f32.to_bits()); rejected(11254, 52945, 52977, 0, &factor);
    rejected(11254, 52945, 52976, 0, &display);
    let mut missing = frame.clone(); missing.as_object_mut().unwrap().remove("display_metadata");
    assert!(std::panic::catch_unwind(|| effect_display_metadata(&missing)).is_err());
    let mut stale = frame.clone(); stale["display_metadata"][2] = json!(8);
    assert!(std::panic::catch_unwind(|| effect_display_metadata(&stale)).is_err());
}
fn effect_tile_pixel(vram: &[u8], tile: usize, bank: usize, x: usize, y: usize) -> u8 {
    let at = bank * 0x2000 + tile * 16 + y * 2;
    ((vram[at] >> (7 - x)) & 1) | (((vram[at + 1] >> (7 - x)) & 1) << 1)
}
fn effect_obj_pixel(oam: &[u8], vram: &[u8], q: usize, x: i32, y: i32, height: i32) -> u8 {
    let mut dx = x - (i32::from(oam[q * 4 + 1]) - 8);
    let mut dy = y - (i32::from(oam[q * 4]) - 16);
    if !(0..8).contains(&dx) || !(0..height).contains(&dy) { return 0; }
    let flags = oam[q * 4 + 3];
    if flags & 32 != 0 { dx = 7 - dx; }
    if flags & 64 != 0 { dy = height - 1 - dy; }
    let tile = if height == 16 { (oam[q * 4 + 2] & 254) as usize + dy as usize / 8 } else { oam[q * 4 + 2] as usize };
    effect_tile_pixel(vram, tile, ((flags >> 3) & 1) as usize, dx as usize, dy as usize % 8)
}
fn headbutt_row(frame: &Value, y: usize, hardware: &mut [u8], cursor: &mut usize) -> Value {
    let rows = frame["rows"].as_array().expect("Actual per-line provenance");
    let changes = frame["changes"].as_array().expect("Ordered sparse hardware changes");
    assert_eq!(rows.len(), 144 * 16); assert_eq!(changes.len() % 2, 0);
    assert!(changes.len() <= 65536 * 2);
    let row = &rows[y * 16..(y + 1) * 16];
    let end = row[9].as_u64().unwrap() as usize;
    assert!(end >= *cursor && end <= changes.len() / 2, "Row change order/bounds");
    let mut previous = None;
    while *cursor < end {
        let at = changes[*cursor * 2].as_u64().unwrap() as usize;
        let value = u8::try_from(changes[*cursor * 2 + 1].as_u64().unwrap()).unwrap();
        assert!(at < hardware.len());
        assert!(previous.map_or(true, |p| at > p), "Duplicate/unordered row byte mutation");
        assert_ne!(hardware[at], value, "Sparse change must change actual row hardware");
        hardware[at] = value; previous = Some(at); *cursor += 1;
    }
    if y == 143 { assert_eq!(end, changes.len() / 2, "Unconsumed row changes"); }
    let mut meta = frame["render_metadata"].clone();
    for (dest, src) in [(3,0),(4,1),(8,2),(9,3),(10,4),(11,5),(12,6),(13,7),(2,10)] { meta[dest] = row[src].clone(); }
    serde_json::json!({"render_metadata":meta,"window_line":row[8],"phase":row[12]})
}
fn headbutt_video_step(run: &Value, frame: &Value) -> Option<usize> {
    let index = frame["render_metadata"][4].as_u64().unwrap() as usize;
    if index == 0 { return None; }
    let events = effect_events(run);
    assert!(index <= events.len());
    let source = &events[index - 1];
    assert_eq!(effect_field(source, 2), 5);
    assert!(effect_field(source, 0) <= frame["render_metadata"][8].as_u64().unwrap(), "Animation source must precede the actual DMA");
    assert_eq!(effect_field(source, 3), 2);
    assert!(effect_field(source, 1) <= frame["render_metadata"][2].as_u64().unwrap());
    if effect_calls(run, 0, 2).iter().any(|completed|
        effect_field(completed, 0) <= frame["render_metadata"][8].as_u64().unwrap()) { return None; }
    Some(events[..index].iter().filter(|e| effect_field(e, 2) == 5 && effect_field(e, 3) == 2).count())
}
fn headbutt_pixels(e: &Value) {
    let mobile = &clients(e)[0]["mobile"];
    let native = &clients(e)[0]["single"];
    let first = (n(&mobile["state"], "vAcceptanceHeadbuttTileStart") - 0x8000) as usize / 16;
    let end = (n(&mobile["state"], "vAcceptanceHeadbuttTileEnd") - 0x8000) as usize / 16;
    let mut controls = BTreeMap::new();
    for frame in native["profile"]["native_effect_video_frames"].as_array().unwrap() {
        let mut hardware = effect_bytes(&frame["hardware"]); let mut cursor = 0;
        for y in 0..144 { let row = headbutt_row(frame, y, &mut hardware, &mut cursor);
            if let Some(step) = headbutt_video_step(native, &row) { controls.entry((step,y)).or_insert(frame); }
        }
    }
    let mut visible = 0usize;
    let mut steps = BTreeSet::new();
    for run in [mobile, native] {
        assert_eq!(n(&run["profile"], "native_effect_video_overflow"), 0);
        let video = run["profile"]["native_effect_video_frames"].as_array().expect("Bounded actual framebuffer capture");
        assert!(!video.is_empty());
        let mut previous_frame = None;
        let mut previous_host = None;
        let mut previous_output = None;
        for frame in video {
            let frame_meta = frame["render_metadata"].as_array().unwrap();
            let number = frame_meta[1].as_u64().unwrap();
            assert!(previous_frame.map_or(true, |p| number == p + 1), "Missing/reordered actual rendered frame");
            assert!(previous_host.map_or(true, |p| n(frame,"host_frame") >= p), "Reordered host capture");
            previous_frame = Some(number); previous_host = Some(n(frame,"host_frame"));
            assert_eq!(frame_meta[5], 144, "Every actual visible line required");
            assert_eq!(frame_meta[6], 0, "Actual per-line capture continuity/overflow failed");
            assert_eq!(n(frame, "width"), 160); assert_eq!(n(frame, "height"), 144); assert_eq!(n(frame, "format"), 2);
            let display = effect_display_metadata(frame);
            assert!(previous_output.map_or(true, |p| display[1] > p), "Repeated/stale actual output invocation");
            previous_output = Some(display[1]);
            let mut h = effect_bytes(&frame["hardware"]);
            let pixels = effect_bytes(&frame["pixels"]);
            assert_eq!(h.len(), 16966); assert_eq!(pixels.len(), 160 * 144 * 2);
            let mut cursor = 0;
            for y in 0..144i32 {
            let row = headbutt_row(frame, y as usize, &mut h, &mut cursor);
            let meta = row["render_metadata"].as_array().unwrap();
            let oam = &h[..160]; let vram = &h[160..16544]; let mapped = &h[16672..16800]; let r = &h[16800..16806];
            assert_eq!(oam, &h[16806..], "Actual DMA source differs from rendered OAM");
            assert!(meta[10].as_u64().unwrap() > 0, "Missing actual OAM DMA provenance");
            let page = r[5];
            if (0xd0..0xe0).contains(&page) {
                let raw = meta[11].as_u64().unwrap() & 7;
                assert_eq!(meta[12].as_u64().unwrap(), if raw == 0 { 1 } else { raw });
            }
            let source_index = meta[4].as_u64().unwrap() as usize;
            let latest = effect_events(run).iter().enumerate().rev().find(|(_,event)|
                effect_field(event,2)==5 && effect_field(event,3)==2 && effect_field(event,0)<=meta[8].as_u64().unwrap()).map_or(0,|(i,_)|i+1);
            assert_eq!(source_index,latest,"Latest native animation return before actual DMA required");
            let sequence = frame["rows"][y as usize*16+11].as_u64().unwrap();
            let phase = if effect_calls(run,0,2).iter().any(|event|effect_field(event,0)<=sequence) {2} else if source_index>0 {1} else {0};
            assert_eq!(n(&row,"phase"),phase,"Actual native phase provenance");
            let step = headbutt_video_step(run, &row);
            let source = if source_index > 0 {
                effect_bytes(&effect_events(run)[source_index - 1]["watches"]["0"])
            } else { vec![0; 160] };
            assert_eq!(source.len(), 160, "Complete returned native ShadowOAM required");
            // Establish demand from the disabled native source before filtering
            // visible hardware targets. Hidden demand and duplicate tuples count.
            let demand = |bytes: &[u8]| {
                let mut quads: Vec<_> = bytes.chunks_exact(4)
                    .filter(|q| (first..end).contains(&(q[2] as usize)) && q[3] & 8 == 0)
                    .map(|q| q.to_vec()).collect();
                quads.sort(); quads
            };
            let mut intended = demand(&source);
            if let Some(step) = step {
                if std::ptr::eq(run, mobile) {
                    let control = controls.get(&(step,y as usize)).expect("No same-source control at matched actual animation step");
                    let mut c = effect_bytes(&control["hardware"]); let mut cc = 0;
                    let mut cr = Value::Null;
                    for cy in 0..=y as usize { cr = headbutt_row(control, cy, &mut c, &mut cc); }
                    let ci = cr["render_metadata"][4].as_u64().unwrap() as usize - 1;
                    let control_source = effect_bytes(&effect_events(native)[ci]["watches"]["0"]);
                    assert_eq!(control_source.len(), 160);
                    assert_eq!(effect_events(run)[source_index - 1]["watches"]["4"], effect_events(native)[ci]["watches"]["4"], "Control animation state is not the same native step");
                    // Identical Route29/player-pose inputs require identical
                    // screen geometry. No tuple-derived coordinate realignment.
                    assert_eq!(&r[..5], &c[16800..16805], "Actual native view geometry differs");
                    let native_demand = demand(&control_source);
                    assert_eq!(intended, native_demand, "Complete disabled native effect source geometry/tile/attributes/palette/multiplicity differs");
                    assert_eq!(&h[160 + first * 16..160 + end * 16], &c[160 + first * 16..160 + end * 16], "Actual native animation tiles differ");
                    for quad in &native_demand {
                        let slot = 8 + usize::from(quad[3] & 7);
                        assert_eq!(&h[16544 + slot * 8..16544 + (slot + 1) * 8], &c[16544 + slot * 8..16544 + (slot + 1) * 8], "Actual intended OBJ palette RGB differs at slot {slot}");
                        assert_eq!(&h[16672 + slot * 8..16672 + (slot + 1) * 8], &c[16672 + slot * 8..16672 + (slot + 1) * 8], "Actual intended mapped OBJ colors differ at slot {slot}");
                    }
                    if !native_demand.is_empty() { steps.insert(step); }
                }
            } else if source_index > 0 {
                // Native Shake return and cleared animation end native demand.
                // The enabled addon must additionally release graphics ownership
                // before private peers may reuse those native effect tiles.
                let completed = effect_calls(run, 0, 2).into_iter().rev().find(|event|
                    effect_field(event, 0) <= meta[8].as_u64().unwrap())
                    .expect("Idle source has no actual completed native effect before DMA");
                assert!(effect_bytes(&completed["watches"]["4"]).iter().all(|b| *b == 0), "Effect returned with live animation demand");
                assert!(effect_bytes(&effect_events(run)[source_index - 1]["watches"]["4"]).iter().all(|b| *b == 0), "Post-effect source has unsupported live native animation");
                if std::ptr::eq(run, mobile) && u64::from(page) == n(frame, "private_oam_page") && meta[12].as_u64().unwrap() == n(frame, "private_oam_bank") {
                    assert!(effect_calls(run, 3, 2).iter().any(|release| {
                        effect_field(release, 0) <= meta[8].as_u64().unwrap()
                            && effect_bytes(&release["watches"]["5"]) == [0]
                            && effect_bytes(&release["watches"]["1"]) == [0]
                            && effect_bytes(&release["watches"]["2"]) == [0]
                            && effect_bytes(&release["watches"]["4"]).iter().all(|b| *b == 0)
                    }), "Completed effect has no preceding actual native graphics release");
                }
                intended.clear();
            } else {
                assert_eq!(row["phase"], 0, "Missing source is only valid before actual animation");
                assert_eq!(source_index, 0);
                // Pre-animation hardware is its actual previous native/private
                // OAM baseline. Tile numbers alone cannot invent effect demand.
            }
            if step.is_some() {
                assert_eq!(demand(oam), intended, "Every source-bound native effect quad, preserving multiplicity");
            }
            let targets: Vec<_> = (0..40).filter(|q| step.is_some() && (first..end).contains(&(oam[q * 4 + 2] as usize)) && oam[q * 4 + 3] & 8 == 0 && (1..160).contains(&oam[q * 4])).collect();
            if !targets.is_empty() {
                assert_eq!(meta[13], 7, "All actual native LCD layers must be enabled");
                assert_eq!(r[0] & 0x82, 0x82, "LCD and OBJ must be enabled for intended pixels");
                assert!(step.is_some(), "Animation has no native return source binding");
            }
            let height = if r[0] & 4 != 0 { 16 } else { 8 };
                // X-hidden sprites still consume the hardware's first-ten list.
                let selected: Vec<_> = (0..40).filter(|q| {
                    let top = i32::from(oam[q * 4]) - 16; (top..top + height).contains(&y)
                }).take(10).collect();
                for x in 0..160i32 {
                    let winner = selected.iter().find(|q| effect_obj_pixel(oam, vram, **q, x, y, height) != 0);
                    assert_eq!(meta[13], 7, "Full rendered row requires all LCD layers");
                    assert_ne!(r[0] & 128, 0, "Captured native LCD must be enabled");
                    let window = r[0] & 32 != 0 && i32::from(r[3]) < y + 1 && r[4] <= 166 && x >= i32::from(r[4]) - 7;
                    let (bx, by, base, map_row) = if window {
                        let entry = i32::try_from(n(&row,"window_line")).expect("Actual window counter outside signed range");
                        let wy = entry.checked_sub(1).expect("Actual window row underflow");
                        // Pinned lcd::win_render_color starts its map pointer
                        // one tile row before y>>3. Reset entry0 means y=-1;
                        // clear_win_count entry9 means y=8, map row0.
                        ((x - (i32::from(r[4]) - 7)) as usize, (wy & 7) as usize,
                            if r[0] & 64 != 0 {0x1c00i64} else {0x1800i64}, i64::from((wy >> 3) - 1))
                    } else {
                        let by = ((y + i32::from(r[1])) & 255) as usize;
                        (((x + i32::from(r[2])) & 255) as usize, by,
                            if r[0] & 8 != 0 {0x1c00i64} else {0x1800i64}, (by / 8) as i64)
                    };
                    let address = base + map_row * 32 + (bx / 8) as i64;
                    assert!((0..0x2000).contains(&address), "Actual window/BG map address outside captured VRAM bank");
                    let at = address as usize; let attr = vram[0x2000 + at];
                    let tile = if r[0] & 16 != 0 { vram[at] as usize } else { (256i32 + i32::from(vram[at] as i8)) as usize };
                    let px = if attr & 32 != 0 { 7 - bx % 8 } else { bx % 8 }; let py = if attr & 64 != 0 { 7 - by % 8 } else { by % 8 };
                    let bg = effect_tile_pixel(vram, tile, ((attr >> 3) & 1) as usize, px, py);
                    let mut palette = usize::from(attr & 7); let mut color = usize::from(bg);
                    let mut proved_effect = false;
                    if r[0] & 2 != 0 { if let Some(&q) = winner {
                        let flags = oam[q * 4 + 3];
                        if r[0] & 1 == 0 || bg == 0 || (attr & 128 == 0 && flags & 128 == 0) {
                            palette = 8 + usize::from(flags & 7);
                            color = usize::from(effect_obj_pixel(oam, vram, q, x, y, height));
                            proved_effect = targets.contains(&q);
                        }
                    }}
                    let at = (palette * 4 + color) * 2;
                    let expected = u16::from_le_bytes([mapped[at], mapped[at + 1]]);
                    let raw = u16::from_le_bytes([h[16544 + at], h[16544 + at + 1]]);
                    assert_eq!(expected, effect_logical_color(raw, &display), "Raw selected logical color at {x},{y}");
                    let expected = effect_physical_color(expected, y as usize, &display);
                    let at = (y as usize * 160 + x as usize) * 2;
                    assert_eq!(u16::from_le_bytes([pixels[at], pixels[at + 1]]), expected, "Intended selected effect pixel or independently decoded BG/OBJ/window pixel at {x},{y}, host {}", frame["host_frame"]);
                    if std::ptr::eq(run, mobile) && proved_effect { visible += 1; }
                }
            }
        }
    }
    assert!(steps.len() >= 2, "No actual animation progression");
    assert!(visible > 0, "No nontransparent intended framebuffer pixels proved");
}
fn headbutt_movement(run: &Value) {
    let event = run["events"].as_array().unwrap().iter().find(|e| n(e, "button") == 6).expect("Physical post-effect movement absent");
    let before = &frames(run).iter().find(|r| n(r, "frame") + 1 == n(event, "frame")).unwrap()["state"];
    assert_eq!(n(before, "vAcceptanceHeadbuttFinished"), 1);
    assert_eq!(n(before, "wScriptRunning"), 0);
    assert!(frames(run).iter().filter(|r| n(r, "frame") >= n(event, "frame")).any(|r| r["state"]["wXCoord"] != before["wXCoord"] || r["state"]["wYCoord"] != before["wYCoord"]), "Physical movement did not change native coordinates");
}
#[test]
fn native_headbutt_lifecycle_and_selected_pixels() {
    let e = evidence("native-headbutt");
    headbutt_lifecycle(&e); headbutt_pixels(&e); headbutt_movement(&clients(&e)[0]["mobile"]);
}
#[test]
fn native_headbutt_held_exit_and_movement() {
    let e = evidence("native-headbutt-held");
    headbutt_lifecycle(&e); headbutt_pixels(&e);
    let run = &clients(&e)[0]["mobile"];
    let selects: Vec<_> = run["events"].as_array().unwrap().iter().filter(|e| n(e, "button") == 2).collect();
    let held = selects.iter().find(|e| n(e, "duration") == 160).expect("Actual held Select+B160 absent");
    assert!(run["events"].as_array().unwrap().iter().any(|b| n(b, "button") == 0 && n(b, "frame") == n(held, "frame") && n(b, "duration") == 160));
    let before = &frames(run).iter().find(|f| n(f, "frame") + 1 == n(held, "frame")).unwrap()["state"];
    assert_eq!(n(before, "vAcceptanceEffectActive0"), 1, "Held exit missed actual active ShakeHeadbuttTree");
    let rejoin = n(selects.last().unwrap(), "frame");
    assert!(rejoin > n(held, "frame") + 160, "Later explicit join must follow held release");
    let suppressed: Vec<_> = frames(run).iter().filter(|f| n(f, "frame") >= n(held, "frame") + limit("exit_frames") && n(f, "frame") < rejoin).collect();
    assert!(!suppressed.is_empty());
    for row in suppressed {
        assert_eq!(n(&row["state"], "vAcceptanceSerialSC") & 128, 0, "Actual serial transfer remained active after local exit");
        assert_eq!(n(&row["state"], "vAcceptanceSerialIE") & 8, n(before, "wMobileSavedSerialIE") & 8, "Serial interrupt ownership was not restored after local exit");
        assert_eq!(n(&row["state"], "wMobileTransferActive"), 0);
        for key in ["wMobileWanted", "wMobileOwner", "wMobileOnline", "wMobilePeerCount"] { assert_eq!(n(&row["state"], key), 0, "Held exit suppression {key}, frame {}", row["frame"]); } }
    for client in &clients(&e)[1..] {
        assert!(frames(&client["mobile"]).iter().filter(|f| n(f, "frame") >= n(held, "frame")).all(|f| n(&f["state"], "wMobileOnline") == 1 && n(&f["state"], "hCrashCode") == 0), "Sibling lost its native session");
    }
    headbutt_movement(run);
}

#[path = "support/headbutt_pixel_regressions.rs"]
mod headbutt_pixel_regressions;
