#!/usr/bin/env python3
"""Run four real native ROMs next to Route 37's protected twin trainers.

Creates ordinary New Game/Save batteries, with no state or game-RAM injection.
The current known conflict fails acceptance. --diagnostic records that failure
without treating it as renderer success. User listeners and saves are untouched.
"""
import argparse
import json
from pathlib import Path
import subprocess
from probe_emulator import digest, require

REPO = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cache", type=Path, default=Path("/Volumes/Storage/Cache/mobileadapter"))
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--port", type=int, default=17640)
    parser.add_argument("--diagnostic", action="store_true")
    args = parser.parse_args()
    args.output = args.output.resolve()
    args.cache = args.cache.resolve()
    require(1024 <= args.port <= 65534 and not args.port <= 17423 <= args.port + 1,
            "Use an isolated test port range")
    require(not args.output.exists(), "Choose a fresh fixture output directory")
    args.output.mkdir(parents=True)
    runner = args.output / "smoke-libretro"
    subprocess.run(["clang++", "-std=c++17", "-O2", "-I",
        str(args.cache / "sources/doublecherry/libretro/DoubleCherryEngine"),
        str(REPO / "mobile_adapter/tools/smoke_libretro.cpp"), "-lz", "-o", str(runner)], check=True)
    core = args.cache / "build/doublecherry/DoubleCherryGB_libretro.dylib"
    batteries = []
    for figure in ("chris", "kris", "crys", "beta"):
        directory = args.output / figure
        with (args.output / (figure + "-producer.log")).open("w") as log:
            subprocess.run(["python3", str(REPO / "mobile_adapter/tools/create_scene_save.py"),
                "--runner", str(runner), "--core", str(core), "--output", str(directory),
                "--map", "Route37", "--x", "8", "--y", "12", "--avatar", figure],
                stdout=log, stderr=subprocess.STDOUT, check=True)
        batteries.append(directory / "ingame.sav")
    command = ["python3", str(REPO / "mobile_adapter/tools/probe_lobby.py"),
        "--runner", str(runner), "--core", str(core), "--server",
        str(REPO / "mobile_adapter/server/target/debug/mobile-bridge"), "--players", "4",
        "--native-actor-probe", "--scenario", "scanline", "--port", str(args.port),
        "--output", str(args.output / "runtime"), "--battery", str(batteries[0]),
        "--second-battery", str(batteries[1]), "--different-battery", str(batteries[2]),
        "--fourth-battery", str(batteries[3])]
    with (args.output / "probe.log").open("w") as log:
        process = subprocess.run(command, stdout=log, stderr=subprocess.STDOUT)
    report = args.output / "runtime/native-scanlines.json"
    require(report.exists(), "Fixture failed before scanline observation; inspect producer/probe logs")
    result = json.loads(report.read_text())
    require((process.returncode == 0) == result["passed"], "Unexpected fixture failure after scanline checks")
    if process.returncode:
        require("Native scanline/visibility acceptance failed" in (args.output / "probe.log").read_text(),
                "Diagnostic mode cannot absorb unrelated fixture failures")
    evidence = {"acceptance_passed": result["passed"], "rom_sha256": digest(REPO / "polishedcrystal-mobile-3.2.3.gbc"),
        "core_sha256": digest(core), "map": "Route37", "protected_map_indices": [2, 3],
        "player_positions": [[8, 12]] * 4, "battery_sha256": [digest(path) for path in batteries],
        "note": "Distinct live players may overlap because remote hitboxes are disabled. X overlap does not reduce OAM selection demand.",
        "scanlines": result}
    (args.output / "evidence.json").write_text(json.dumps(evidence, indent=2) + "\n")
    print(json.dumps(evidence, indent=2))
    require(args.diagnostic or result["passed"], "Protected Route37 scanline conflict remains unresolved")


if __name__ == "__main__":
    main()
