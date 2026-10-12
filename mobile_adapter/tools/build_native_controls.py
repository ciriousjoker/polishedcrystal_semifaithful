#!/usr/bin/env python3
"""Build a disposable ordinary-NPC control, restore native source and baseline.

The fixture uses existing object events and native sprite loading. No feature
assembly, RAM injection or save modification is involved in the control ROM.
"""
import argparse
from pathlib import Path
import shutil
import subprocess
from probe_emulator import BASELINE_SHA256, digest, require

REPO = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rgbds", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--scene", choices=("bedroom", "game-corner"), default="bedroom")
    parser.add_argument("--avatar", choices=("chris", "kris", "crys", "beta"), default="chris",
                        help="Local figure; the corner control contains the other three native figures")
    parser.add_argument("--peer-position", action="append", default=[],
                        help="Override an ordinary peer position as figure=x,y from observed reference state")
    parser.add_argument("--replace-object", action="append", type=int, default=[],
                        help="Map-object index actually reclaimed in the measured reference")
    args = parser.parse_args()
    replacements = args.replace_object or [10, 11]
    require(len(replacements) == 2 and len(set(replacements)) == 2 and
            all(1 <= index <= 12 for index in replacements), 'Expected two distinct corner leases')
    positions = {}
    for value in args.peer_position:
        name, coords = value.split('=', 1)
        x, y = map(int, coords.split(','))
        require(name in ('chris', 'kris', 'crys', 'beta') and name != args.avatar and
                name not in positions and 0 <= x < 32 and 0 <= y < 32,
                'Invalid or repeated peer position')
        positions[name] = (x, y)
    require(not args.output.exists(), "Choose a new artifact directory")
    args.output.mkdir(parents=True)
    source = REPO / ("maps/PlayersHouse2F.asm" if args.scene == "bedroom" else "maps/GoldenrodGameCorner.asm")
    original = source.read_bytes()
    text = original.decode()
    if args.scene == "bedroom":
        needle = "\nPlayersHouse2FInitializeRoom:"
        require(text.count(needle) == 1, "Fixture insertion point changed")
        fixture_text = text.replace(needle, '\nINCLUDE "mobile_adapter/rom/fixtures/native_control_objects.asm"\n' + needle)
    else:
        lines = text.splitlines(keepends=True)
        objects = [i for i, line in enumerate(lines) if line.lstrip().startswith("object_event ")]
        require(len(objects) == 12, "Game Corner object inventory changed")
        for number, name in zip(replacements, ("crys", "beta")):
            lines[objects[number - 1]] = f'INCLUDE "mobile_adapter/rom/fixtures/native_control_corner_{name}.asm"\n'
        lines[objects[-1]] += 'INCLUDE "mobile_adapter/rom/fixtures/native_control_corner_extra.asm"\n'
        fixture_text = "".join(lines)
    # Force the map dependency on both builds. macOS make can otherwise miss
    # a fast fixture restoration whose timestamp falls in the same second.
    command = ["make", "-j4", "-W", str(source.relative_to(REPO)),
               "crystal", f"RGBDS={args.rgbds}/"]
    fixture_originals = {}
    try:
        if args.scene == "bedroom":
            fixture = REPO / "mobile_adapter/rom/fixtures/native_control_objects.asm"
            fixture_originals[fixture] = fixture.read_bytes()
            rows = []
            for name, x, y, facing in (("chris", 4, 3, "RIGHT"), ("kris", 3, 4, "DOWN"),
                                       ("crys", 2, 3, "LEFT"), ("beta", 3, 2, "UP")):
                if name == args.avatar:
                    continue
                x, y = positions.get(name, (x, y))
                rows.append(f"\tobject_event {x}, {y}, SPRITE_{name.upper()}, SPRITEMOVEDATA_STANDING_{facing}, "
                            "0, 0, -1, 0, OBJECTTYPE_SCRIPT, 0, DoNothingScript, -1\n")
            fixture.write_text('; Disposable ordinary figures for a matched avatar-load control.\n' + ''.join(rows))
        if args.scene == "game-corner":
            # Each control must contain the corresponding OTHER figures, not
            # duplicate its local figure. All changes are disposable source data.
            figures = (("chris", 15, 5, "RIGHT"), ("kris", 14, 6, "DOWN"),
                       ("crys", 13, 5, "LEFT"), ("beta", 14, 4, "UP"))
            others = [figure for figure in figures if figure[0] != args.avatar]
            for suffix, (name, x, y, facing) in zip(("crys", "beta", "extra"), others):
                x, y = positions.get(name, (x, y))
                fixture = REPO / f"mobile_adapter/rom/fixtures/native_control_corner_{suffix}.asm"
                fixture_originals[fixture] = fixture.read_bytes()
                fixture.write_text(f"; Disposable ordinary {name} peer for local {args.avatar}.\n"
                    f"\tobject_event {x}, {y}, SPRITE_{name.upper()}, SPRITEMOVEDATA_STANDING_{facing}, "
                    "0, 0, -1, 0, OBJECTTYPE_SCRIPT, 0, DoNothingScript, -1\n")
        source.write_text(fixture_text)
        with (args.output / "build.log").open("w") as log:
            subprocess.run(command, cwd=REPO, stdout=log, stderr=subprocess.STDOUT, check=True)
        for suffix in ("gbc", "sym", "map"):
            shutil.copyfile(REPO / f"polishedcrystal-3.2.3.{suffix}", args.output / f"control.{suffix}")
    finally:
        source.write_bytes(original)
        for fixture, contents in fixture_originals.items():
            fixture.write_bytes(contents)
        with (args.output / "restore.log").open("w") as log:
            subprocess.run(command, cwd=REPO, stdout=log, stderr=subprocess.STDOUT, check=True)
        require(digest(REPO / "polishedcrystal-3.2.3.gbc") == BASELINE_SHA256,
                "Native source restoration did not restore the exact baseline")
    print(f"Native control built; source and baseline restored: {args.output}")


if __name__ == "__main__":
    main()
