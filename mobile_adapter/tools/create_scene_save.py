#!/usr/bin/env python3
"""Produce an ordinary save through New Game and the in-game Save menu.

A disposable producer ROM changes the initial spawn and clock choice, plus
explicit initial event substitutions when requested for a progress fixture.
The battery is then booted by the unchanged feature ROM for acceptance.
No game RAM, SRAM editing or emulator state injection is used.
"""
import argparse
import json
from pathlib import Path
import subprocess
from audit_render_policy import generate
from probe_emulator import convert_png, digest, read_symbols, require, validate_battery

REPO = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("runner", "core", "output"):
        parser.add_argument("--" + name, type=Path, required=True)
    parser.add_argument("--rom", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.gbc")
    parser.add_argument("--symbols", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.sym")
    parser.add_argument("--map", required=True)
    parser.add_argument("--x", type=int, required=True)
    parser.add_argument("--y", type=int, required=True)
    parser.add_argument("--hour", type=int, default=10)
    parser.add_argument("--reveal-initial-event", action="append", default=[],
                        help="Stress fixture: omit an initial hide flag before ordinary New Game/Save")
    parser.add_argument("--set-initial-event", action="append", default=[],
                        help="Progress fixture: substitute an initial event before ordinary New Game/Save")
    parser.add_argument("--rgbds", type=Path, default=Path("/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4"))
    args = parser.parse_args()
    for name in ("runner", "core", "output", "rom", "symbols"):
        setattr(args, name, getattr(args, name).resolve())
    require(not args.output.exists(), "Choose a fresh output directory")
    require(0 <= args.hour < 24 and 0 <= args.x < 128 and 0 <= args.y < 128, "Invalid fixture choices")
    args.output.mkdir(parents=True)
    _, rows = generate(args.rom, args.symbols)
    record = next(row for row in rows if row["map"] == args.map)
    symbols = read_symbols(args.symbols)

    def offset(name):
        bank, address = symbols[name]
        return bank * 0x4000 + address - (0x4000 if bank else 0)

    data = bytearray(args.rom.read_bytes())
    spawn = offset("SpawnPoints")
    data[spawn:spawn + 4] = bytes([record["group"], record["number"], args.x, args.y])
    clock = offset("InitClock")
    hour_address = symbols["wInitHourBuffer"][1]
    pattern = bytes([0x3e, 10, 0xea]) + hour_address.to_bytes(2, "little")
    matches = [clock + index for index in range(300) if data[clock + index:clock + index + 5] == pattern]
    require(len(matches) == 1, "Clock initialization changed")
    data[matches[0] + 1] = args.hour
    if args.reveal_initial_event:
        import re
        names = re.findall(r"^\s*dw (EVENT_\w+)\s*$",
                           (REPO / "data/events/initialize_events.asm").read_text(), re.M)
        require(set(args.reveal_initial_event) <= set(names), "Unknown initial event flag")
        start = offset("InitialEvents")
        end = start + 2 * len(names)
        require(data[end:end + 2] == b"\xff\xff", "Initial event table format changed")
        entries = [data[start + index * 2:start + index * 2 + 2]
                   for index, name in enumerate(names) if name not in args.reveal_initial_event]
        replacement = b"".join(entries) + b"\xff\xff"
        data[start:end + 2] = replacement.ljust(end + 2 - start, b"\xff")
    if args.set_initial_event:
        require(not args.reveal_initial_event, "Choose event removal or substitution, not both")
        import re
        allowed = set(re.findall(r"^\s*const (EVENT_\w+)\s*(?:;.*)?$",
                                 (REPO / "constants/event_flags.asm").read_text(), re.M))
        require(set(args.set_initial_event) <= allowed, "Unknown event substitution")
        source = args.output / "events.asm"
        source.write_text('SECTION "Fixture events", ROM0[$100]\n' +
                          "\n".join("dw " + name for name in args.set_initial_event) + "\n")
        subprocess.run([str(args.rgbds / "rgbasm"), "-P", "includes.asm", "-o",
                        str(args.output / "events.o"), str(source)], cwd=REPO, check=True)
        subprocess.run([str(args.rgbds / "rgblink"), "-o", str(args.output / "events.bin"),
                        str(args.output / "events.o")], cwd=REPO, check=True)
        events = (args.output / "events.bin").read_bytes()[0x100:0x100 + 2 * len(args.set_initial_event)]
        start = offset("InitialEvents")
        data[start:start + len(events)] = events
    data[0x14e:0x150] = ((sum(data[:0x14e]) + sum(data[0x150:])) & 65535).to_bytes(2, "big")
    rom = args.output / "producer.gbc"
    rom.write_bytes(data)
    command = [str(args.runner), str(args.core), str(rom), str(args.output / "screen.ppm"), str(args.output),
               "--frames", "12000", "--input", str(REPO / "mobile_adapter/tools/fixtures/new-game-save.input"),
               "--symbols", str(args.symbols), "--state-out", str(args.output / "state.json"),
               "--save-out", str(args.output / "ingame.sav"), "--adapter", "0"]
    with (args.output / "process.log").open("w") as log:
        subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=90)
    state = json.loads((args.output / "state.json").read_text())["symbols"]
    for key, expected in {"hCrashCode": 0, "hInMenu": 0, "wMapGroup": record["group"],
                          "wMapNumber": record["number"], "wXCoord": args.x, "wYCoord": args.y,
                          "wSaveFileExists": 1}.items():
        require(state[key] == expected, f"Scene producer: {key}={state[key]}, expected {expected}")
    metadata = validate_battery(args.output / "ingame.sav", symbols)
    metadata.update({"map": args.map, "x": args.x, "y": args.y, "hour": args.hour,
                     "revealed_initial_events": args.reveal_initial_event,
                     "substituted_initial_events": args.set_initial_event,
                     "production_rom_sha256": digest(args.rom), "producer_rom_sha256": digest(rom),
                     "changed_rom_bytes": [index for index, (old, new) in enumerate(zip(args.rom.read_bytes(), data)) if old != new],
                     "source": "ordinary New Game and in-game Save menu"})
    (args.output / "fixture.json").write_text(json.dumps(metadata, indent=2) + "\n")
    convert_png(args.output / "screen.ppm", args.output / "screen.png")
    print(f"Created ordinary {args.map} battery: {args.output / 'ingame.sav'}")


if __name__ == "__main__":
    main()
