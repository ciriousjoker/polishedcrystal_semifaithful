#!/usr/bin/env python3
"""Audit assembled map objects without starting an emulator.

Reads ROM records rather than counting only object_event source lines. Snapshot
density is diagnostic, not a proof over callbacks, story states, or NPC movement.
"""

import argparse
import csv
import hashlib
import json
import re
from collections import Counter
from pathlib import Path


def read_symbols(path):
    symbols = {}
    for line in path.read_text().splitlines():
        match = re.fullmatch(r"([0-9a-fA-F]+):([0-9a-fA-F]+) (\S+)", line)
        if match:
            bank, address = int(match[1], 16), int(match[2], 16)
            symbols[match[3]] = bank * 0x4000 + address - (0x4000 if bank else 0)
    return symbols


def max_snapshot(objects, width, height, unflagged_only=False, movement_envelope=False,
                 retained_movements=frozenset(), positions=None):
    """Match InitializeVisibleSprites' unsigned initial-coordinate rectangle."""
    best = (0, 0, 0, 0)
    for time in range(4):
        active = [obj for obj in objects if obj[6] & (1 << time)]
        if unflagged_only:
            active = [obj for obj in active if obj[11:13] == b"\xff\xff"]
        # Only anchor coordinates can change a rectangle's membership. The
        # extrema below cover every possible occupancy change, including maps
        # where callbacks later reposition objects outside nominal dimensions.
        xs = {0, width * 2 - 1}
        ys = {0, height * 2 - 1}
        rectangles = []
        for obj in active:
            rx = ry = 0
            if movement_envelope and obj[3] in (2, 4, 5):
                # WANDER, WALK_UP_DOWN, WALK_LEFT_RIGHT. Runtime adds one
                # to each source radius. Include the full enlarged radius
                # conservatively, even when collision prevents reaching it.
                if obj[3] in (2, 5):
                    rx = (obj[4] & 15) + 1
                if obj[3] in (2, 4):
                    ry = (obj[4] >> 4) + 1
            if obj[3] in retained_movements:
                left, right, top, bottom = 0, width * 2 - 1, 0, height * 2 - 1
            else:
                left, right = max(0, obj[2] - 10 - rx), min(width * 2 - 1, obj[2] + 1 + rx)
                top, bottom = max(0, obj[1] - 9 - ry), min(height * 2 - 1, obj[1] + 1 + ry)
            rectangles.append((left, right, top, bottom))
            xs.update((left, right))
            ys.update((top, bottom))
        candidates = positions if positions is not None else (
            (x, y) for x in sorted(v for v in xs if 0 <= v < width * 2)
            for y in sorted(v for v in ys if 0 <= v < height * 2))
        for x, y in candidates:
            if movement_envelope or retained_movements:
                count = sum(left <= x <= right and top <= y <= bottom
                            for left, right, top, bottom in rectangles)
            else:
                count = sum((obj[2] + 1 - x) % 256 < 12
                            and (obj[1] + 1 - y) % 256 < 11 for obj in active)
            if count > best[0]:
                best = (count, x, y, time)
    return best


def audit(repo, rom_path, sym_path):
    rom = rom_path.read_bytes()
    symbols = read_symbols(sym_path)
    names = re.findall(r"^\s*map_attributes (\w+),", (repo / "data/maps/attributes.asm").read_text(), re.M)
    assert len(names) == len(set(names)), "Duplicate map attributes"
    movement = symbols["SpriteMovementData"]
    rows = []
    for name in names:
        cursor = symbols[name + "_MapScriptHeader"]
        for record_size in (2, 3, 5, 5, 5):
            count = rom[cursor]
            cursor += 1 + count * record_size
        count = rom[cursor]
        assert count <= 20, (name, count, "Unexpected object count or changed format")
        objects = [rom[cursor + 1 + index * 13:cursor + 1 + (index + 1) * 13]
                   for index in range(count)]
        assert all(len(obj) == 13 for obj in objects)
        attribute = symbols[name + "_MapAttributes"]
        height, width = rom[attribute + 1:attribute + 3]
        assert width and height, name
        visible, x, y, time = max_snapshot(objects, width, height)
        unflagged = max_snapshot(objects, width, height, True)[0]
        envelope = max_snapshot(objects, width, height, movement_envelope=True)[0]
        retained = frozenset(obj[3] for obj in objects if rom[movement + obj[3] * 6 + 3] & 2)
        persistent = sum(obj[3] in retained for obj in objects)
        retained_envelope = max_snapshot(objects, width, height, movement_envelope=True,
                                         retained_movements=retained)[0]
        rows.append({
            "map": name,
            "declared_objects": count,
            "unused_map_records": 20 - count,
            "whole_map_runtime_spare_lower_bound_no_effects": max(0, 12 - count),
            "initial_window_max_ignoring_story_flags": visible,
            "initial_window_max_unflagged": unflagged,
            "random_movement_envelope_max_ignoring_story_flags": envelope,
            "retained_movement_envelope_max_ignoring_story_flags": retained_envelope,
            "wont_delete_records": persistent,
            "witness_player_x": x,
            "witness_player_y": y,
            "witness_time_index": time,
        })
    summary = {
        "rom_sha256": hashlib.sha256(rom).hexdigest(),
        "map_count": len(rows),
        "runtime_nonplayer_slots": 12,
        "map_nonplayer_records": 20,
        "declared_object_histogram": dict(sorted(Counter(row["declared_objects"] for row in rows).items())),
        "maps_with_at_least_n_spare_by_whole_map_count_no_effects": {
            str(n): sum(row["declared_objects"] <= 12 - n for row in rows) for n in (1, 2, 3)
        },
        "initial_window_histogram_ignoring_story_flags": dict(sorted(Counter(row["initial_window_max_ignoring_story_flags"] for row in rows).items())),
        "initial_windows_with_at_least_10_objects": [row for row in rows if row["initial_window_max_ignoring_story_flags"] >= 10],
        "movement_envelopes_with_at_least_10_objects": [row for row in rows if row["random_movement_envelope_max_ignoring_story_flags"] >= 10],
        "retained_envelopes_with_at_least_10_objects": [row for row in rows if row["retained_movement_envelope_max_ignoring_story_flags"] >= 10],
        "limitations": [
            "Whole-map counts exclude temporary effect objects and script-created objects.",
            "Spatial snapshots use initial anchors and time masks, ignore story flags, and do not model movement or callbacks.",
            "Witness coordinates are geometry only; collision/reachability is not tested.",
            "Persistent objects can retain slots while offscreen; no whole-playthrough density proof is claimed.",
            "Retained envelopes count all current-time WONT_DELETE records regardless of reachability or story flags, plus ordinary movement envelopes; they do not model older time masks or changed runtime flags.",
            "Movement envelopes cover three ordinary random movement types only, conservatively adding one to source radii; scripts, collision, and route transitions are not simulated.",
            "The parser targets the current 13-byte object record and 6-byte movement table format.",
        ],
    }
    return rows, summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--rom", type=Path, required=True)
    parser.add_argument("--sym", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    rows, summary = audit(args.repo, args.rom, args.sym)
    args.output.mkdir(parents=True, exist_ok=True)
    with (args.output / "map-slots.csv").open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(sorted(rows, key=lambda row: row["map"]))
    (args.output / "map-slots-summary.json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
