#!/usr/bin/env python3
"""Repeat the four-player ROM/Bridge acceptance suite with real headless cores.

Run host tests separately. Use dedicated ports and a fresh artifact directory.
Native runtime and matched-control paths are explicit dependencies. Tests never
patch game RAM or battery saves. Failures retain every diagnostic artifact.
"""
import argparse
import json
from pathlib import Path
import socket
import subprocess
import time
from probe_emulator import BASELINE_SHA256, require
from prepare_native_controls import compare_reference

REPO = Path(__file__).resolve().parents[2]
CACHE = Path("/Volumes/Storage/Cache/mobileadapter")
CASES = ("cadence", "local-latency", "hosted-latency", "priority", "saves",
         "running", "start-menu", "warp", "rejoin", "save-caches")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cache", type=Path, default=CACHE)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--rom", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.gbc")
    parser.add_argument("--symbols", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.sym")
    parser.add_argument("--control", type=Path, help="Optional validated controls for this exact reference; otherwise generate them after cadence")
    parser.add_argument("--port", type=int, default=17584)
    parser.add_argument("--cases", default=",".join(CASES))
    args = parser.parse_args()
    for name in ("cache", "output", "rom", "symbols", "control"):
        value = getattr(args, name)
        if value is not None:
            setattr(args, name, value.resolve())
    cases = args.cases.split(",")
    require(cases and len(set(cases)) == len(cases) and all(c in CASES for c in cases), "Unknown or repeated case")
    require(1024 <= args.port <= 65525 and not args.port <= 17423 < args.port + 10,
            "Use an isolated test port range")
    require(not args.output.exists(), "Choose a fresh output directory")
    for port in range(args.port, args.port + 10):
        with socket.socket() as probe:
            probe.bind(("127.0.0.1", port))
    args.output.mkdir(parents=True)
    runner = args.cache / "smoke-libretro-m2"
    core = args.cache / "build/doublecherry/DoubleCherryGB_libretro.dylib"
    profile = args.cache / "build/doublecherry-profile-cadence/DoubleCherryGB_libretro.dylib"
    binaries = REPO / "mobile_adapter/server/target/debug"
    shared = ["--runner", runner, "--core", core, "--rom", args.rom, "--symbols", args.symbols]
    saves = REPO / "mobile_adapter/tests/scenarios"

    def run(tool, values):
        subprocess.run(["python3", str(REPO / "mobile_adapter/tools" / tool), *map(str, values)],
                       cwd=REPO, check=True)

    for case in cases:
        print("ACCEPTANCE " + case, flush=True)
        scene = "bedroom" if case in ("local-latency", "hosted-latency", "warp", "rejoin", "save-caches") else "game-corner"
        batteries = [saves / scene / (name + ".sav") for name in ("chris", "kris", "crys", "beta")]
        tail = ["--port", args.port, "--output", args.output / case]
        if case == "priority":
            peers = [item for path in batteries[1:] for item in ("--peer-battery", path)]
            run("probe_priority.py", shared + ["--bridge", binaries / "mobile-bridge", "--battery", batteries[0]] + peers + tail)
            continue
        if case in ("saves", "save-caches"):
            run("probe_save_safety.py", shared + ["--server", binaries / "mobile-bridge", "--battery", batteries[0],
                "--profile-core", profile,
                "--singleplayer-rom", REPO / 'polishedcrystal-3.2.3.gbc',
                "--singleplayer-symbols", REPO / 'polishedcrystal-3.2.3.sym',
                "--expected-singleplayer-sha256", BASELINE_SHA256,
                "--peer-battery", batteries[1], "--extra-peer-battery", batteries[2],
                "--extra-peer-battery", batteries[3]] + (["--cases", "connected"] if case == "save-caches" else []) + tail)
            continue
        values = shared + ["--server", binaries / "mobile-bridge", "--battery", batteries[0],
            "--second-battery", batteries[1], "--different-battery", batteries[2],
            "--fourth-battery", batteries[3], "--players", 4, "--native-actor-probe"] + tail
        if case.endswith("latency"):
            values += ["--latency"]
        elif case == "cadence":
            values[values.index("--core") + 1] = profile
            values += ["--speeds", "1,2,3,4", "--profile", "--profile-window-start", 3300,
                       "--require-npc-motion", "--observation-frames", 5500]
        else:
            values += ["--scenario", case]
        if case not in ("cadence", "hosted-latency"):
            run("probe_lobby.py", values)
            continue
        host_port = args.port + 4
        log_path = args.output / (case + "-host.log")
        with log_path.open("w") as log:
            host = subprocess.Popen([str(binaries / "mobile-hosted"), "--bind", f"127.0.0.1:{host_port}",
                "--test-one-way-delay-ms", "50"], stdout=log, stderr=subprocess.STDOUT)
            try:
                deadline = time.monotonic() + 5
                while "MOBILE_HOSTED_READY" not in log_path.read_text():
                    require(host.poll() is None and time.monotonic() < deadline, "Hosted router did not start")
                    time.sleep(.02)
                run("probe_lobby.py", values + ["--households", 2, "--bridge-upstream", f"ws://127.0.0.1:{host_port}",
                    "--declared-upstream-one-way-delay-ms", 50])
            finally:
                host.terminate()
                host.wait(timeout=5)
    if "cadence" in cases:
        control = args.control or args.output / "native-controls"
        if args.control is None:
            run("prepare_native_controls.py", ["--cache", args.cache,
                "--reference", args.output / "cadence", "--output", control])
        comparison = compare_reference(args.output / "cadence", control)
        (args.output / "cadence-comparison.json").write_text(json.dumps(
            {"passed": True, "controls": str(control), "comparisons": comparison}, indent=2) + "\n")
    print("Milestone-2 selected acceptance cases passed", flush=True)


if __name__ == "__main__":
    main()
