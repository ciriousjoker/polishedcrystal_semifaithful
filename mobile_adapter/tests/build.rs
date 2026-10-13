use mobile_acceptance::evidence;
use std::{fs, path::PathBuf, process::Command};

#[test]
fn disabled_feature_is_byte_identical_to_pristine_counterpart() {
    let e = evidence("build");
    assert_eq!(
        e["manifest"]["baseline_sha256"], e["manifest"]["pristine_sha256"],
        "Disabled ROM changed relative to the declared pristine source revision"
    );
}

#[test]
fn original_map_events_and_native_movement_are_preserved() {
    let e = evidence("build");
    assert_eq!(e["maps"]["passed"], true);
    assert!(e["maps"]["map_count"].as_u64().unwrap() > 0);
    for map in e["maps"]["maps"].as_array().unwrap() {
        assert_eq!(map["required_remote_object_slots"], 0);
        assert_eq!(map["npc_permissions_required"], false);
    }
}

#[test]
fn rom_budget_and_bank_isolation_remain_bounded() {
    let e = evidence("build");
    assert_eq!(
        e["budget"]["within_budget"], true,
        "ROM budget: {}",
        e["budget"]
    );
}

#[test]
fn feature_assembly_stays_in_its_directory_with_reviewable_integration() {
    let e = evidence("build");
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let base = e["manifest"]["pristine_ref"].as_str().unwrap();
    let output = Command::new("git")
        .args(["diff", "--name-only", base, "--", "*.asm"])
        .current_dir(&root)
        .output()
        .unwrap();
    assert!(output.status.success());
    let allowed = [
        "main.asm",
        "includes.asm",
        "ram.asm", // Conditional include of the feature-owned RAM declarations.
        "ram/wram0.asm",
        "ram/wramx.asm",
        "home/delay.asm",
        "home/map.asm", // Optional declarative test scenario selection.
        "engine/menus/intro_menu.asm",
        "engine/menus/main_menu.asm",
        "engine/overworld/spawn_points.asm",
        "home/palettes.asm",
        "home/gfx.asm", // Conditional private tile ownership fence for native uploads.
        "home/serial.asm",
        "home/vblank.asm",
        "home/video.asm", // Conditional private OAM DMA entry, reviewed on this branch.
        "engine/link/link.asm",
        "engine/movie/title.asm",
        "engine/overworld/events.asm",
        "engine/overworld/map_objects.asm",
        "engine/printer/printer_serial.asm",
        "engine/gfx/dynamic_pals.asm",
    ];
    let files = String::from_utf8(output.stdout).unwrap();
    for path in files.lines().filter(|p| !p.starts_with("mobile_adapter/")) {
        assert!(
            allowed.contains(&path),
            "Unreviewed assembly integration outside feature directory: {path}"
        );
    }
    fs::write(
        mobile_acceptance::output().join("integration-review.txt"),
        files,
    )
    .unwrap();
    // This is a structural guard. Human approval of minimality is still required.
}

#[test]
fn build_emits_rom_compatibility_identity() {
    let e = evidence("build");
    let symbols = e["symbols"].as_array().unwrap();
    assert!(symbols.iter().any(|s| s == "MobileCompatibilityHash"),
        "No build-generated compatibility identity is present in the ROM. Schema/mechanics/capacity alone cannot establish cross-revision compatibility.");
    assert_eq!(e["compatibility_hash"], e["manifest"]["baseline_sha256"],
        "ROM compatibility identity must contain the SHA-256 of its current clean counterpart, not just a constant label");
}

#[test]
fn optional_test_pair_contains_all_scenarios_without_changing_release_roms() {
    let e = evidence("build");
    let pair = &e["test_pair"];
    assert_eq!(pair["build_flag"], "MOBILE_TEST_SCENARIOS");
    assert_eq!(pair["production_has_test_entry"], false);
    assert_eq!(pair["scenario_headers_match"], true);
    assert_eq!(pair["save_event_addresses_match"], true);
    let scenarios = pair["scenarios"].as_object().unwrap();
    assert_eq!(scenarios.len(), 13);
    let ids: std::collections::BTreeSet<_> = scenarios
        .values()
        .map(|s| s["scene_id"].as_u64().unwrap())
        .collect();
    assert_eq!(ids, (0..13).collect());
    for key in ["single_rom", "mobile_rom"] {
        let bytes = fs::read(pair[key].as_str().unwrap()).unwrap();
        assert_eq!(bytes.len(), 4 * 1024 * 1024);
        assert_eq!(&bytes[0x147..0x14a], &[0x10, 7, 3]);
    }
    assert_eq!(
        e["manifest"]["baseline_sha256"],
        e["manifest"]["pristine_sha256"]
    );
}
