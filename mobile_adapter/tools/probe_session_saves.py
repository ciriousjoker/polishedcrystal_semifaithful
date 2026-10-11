#!/usr/bin/env python3
"""Boot every normalized launcher save and compare its native appearance."""
import argparse
import json
from pathlib import Path
import subprocess

from core_runtime import OBSERVED_CORE
from dev_session import CACHE, REPO, TOOLS, verify_idle
from probe_emulator import read_symbols, require
from save_scenarios import AVATARS, battery, catalog, expected, session_battery


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--runner", type=Path, default=CACHE / "smoke-libretro")
    parser.add_argument("--core", type=Path, default=OBSERVED_CORE)
    parser.add_argument("--rom", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.gbc")
    parser.add_argument("--symbols", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.sym")
    args = parser.parse_args()
    require(not args.output.exists(), "Choose a fresh output directory")
    args.output.mkdir(parents=True)
    symbols = read_symbols(args.symbols)
    results = []
    for entry in catalog():
        for index, avatar in enumerate(AVATARS):
            root = args.output / entry["id"] / avatar
            root.mkdir(parents=True)
            fixture = session_battery(entry, avatar, symbols, root / "session.sav", 10001 + index)
            views = []
            for name, save in (("original", battery(entry, avatar, symbols)), ("session", fixture)):
                target = root / name
                target.mkdir()
                with (target / "process.log").open("w") as log:
                    subprocess.run(list(map(str, [args.runner, args.core, args.rom,
                        target / "screen.ppm", target, "--frames", 3500, "--adapter", 0,
                        "--save-in", save, "--input", TOOLS / "fixtures/continue.input",
                        "--symbols", args.symbols, "--state-out", target / "state.json"])),
                        check=True, timeout=60, stdout=log, stderr=subprocess.STDOUT)
                verify_idle(target / "state.json", expected(entry, avatar))
                state = json.loads((target / "state.json").read_text())["symbols"]
                require("vLocalPaletteHash" in state, "Use the native palette observation core")
                views.append(state)
            require(views[0]["wPlayerSprite"] == views[1]["wPlayerSprite"], "Session player sprite differs from the ordinary character-choice save")
            require(views[0]["vLocalPaletteHash"] == views[1]["vLocalPaletteHash"], "Session native hardware colors differ from the ordinary character-choice save")
            results.append({"scenario": entry["id"], "avatar": avatar,
                "native_sprite": views[1]["wPlayerSprite"],
                "native_palette_hash": views[1]["vLocalPaletteHash"], "passed": True})
            print(f"PASS {entry['id']} / {avatar}: ordinary Continue, sprite and palette", flush=True)
    (args.output / "report.json").write_text(json.dumps(results, indent=2) + "\n")


if __name__ == "__main__":
    main()
