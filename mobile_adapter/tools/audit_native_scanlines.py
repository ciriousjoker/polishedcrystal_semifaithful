#!/usr/bin/env python3
"""Flag compiled map pairs that can exhaust scanlines beside four players.

This is a conservative conflict finder, not a story/reachability proof. Optional
actors need whole-figure admission; protected pairs require explicit review.
"""
import argparse
import csv
import hashlib
import json
from pathlib import Path
import re
from audit_map_slots import read_symbols
from probe_emulator import require

REPO = Path(__file__).resolve().parents[2]


def envelope(obj):
    x, y = obj[2] - 4, obj[1] - 4
    rx = (obj[4] & 15) + 1 if obj[3] in (2, 5) else 0
    ry = (obj[4] >> 4) + 1 if obj[3] in (2, 4) else 0
    return x - rx, x + rx, y - ry, y + ry


def pair_overlap(first, second):
    """Ordinary movement envelopes; ten map tiles fit a native viewport."""
    if not first[6] & second[6] & 15:
        return None
    ax0, ax1, ay0, ay1 = envelope(first)
    bx0, bx1, by0, by1 = envelope(second)
    if max(ay0, by0) > min(ay1, by1) or max(ax0 - bx1, bx0 - ax1, 0) > 9:
        return None
    return "initial-row" if first[1] == second[1] and abs(first[2] - second[2]) <= 9 else "movement-envelope"


def classify(obj, index, approved, commands):
    if index in approved:
        return "replaceable"
    kind = obj[7] & 15
    if kind in (3, 7):
        return "required-trainer"
    if kind == 5 and obj[8] < len(commands):
        command = commands[obj[8]]
        if command == "pokemart":
            return "required-shop"
        if command == "jumpstd":
            return "protected-standard-service"
    return "protected-needs-review"


def audit(rom_path, sym_path, policy_path):
    data, symbols = rom_path.read_bytes(), read_symbols(sym_path)
    policy = json.loads(policy_path.read_text())
    commands = re.findall(r"^\s*const (\w+)_command\s*$",
                          (REPO / "macros/scripts/events.asm").read_text(), re.M)
    headers = re.findall(r"^\s*overworld_sprite (\w+), (\w+),",
                         (REPO / "data/sprites/sprites.asm").read_text(), re.M)
    names = re.findall(r"^\s*map (\w+),", (REPO / "data/maps/maps.asm").read_text(), re.M)
    require(names == [row["map"] for row in policy["maps"]], "Lease policy map inventory is stale")
    pointers = {}
    for name, pointer in symbols.items():
        if "." not in name:
            pointers.setdefault(pointer, []).append(name)
    rows = []
    for item in policy["maps"]:
        name = item["map"]
        cursor = symbols[name + "_MapScriptHeader"]
        for size in (2, 3, 5, 5, 5):
            cursor += 1 + data[cursor] * size
        count = data[cursor]
        require(count == item["objects"] <= 20, "Map object inventory differs from reviewed leases: " + name)
        objects = [data[cursor + 1 + i * 13:cursor + 1 + (i + 1) * 13] for i in range(count)]
        actors, unknown = [], []
        for index, obj in enumerate(objects, 1):
            if not 1 <= obj[0] <= len(headers) or headers[obj[0] - 1][1] not in ("WALKING_SPRITE", "STANDING_SPRITE"):
                unknown.append(index)
                continue
            actors.append((index, obj))
        pairs = []
        for number, (index, first) in enumerate(actors):
            for other_index, second in actors[number + 1:]:
                overlap = pair_overlap(first, second)
                if overlap is None:
                    continue
                entries = []
                for actor_index, obj in ((index, first), (other_index, second)):
                    pointer = int.from_bytes(obj[9:11], "little")
                    address = cursor // 0x4000 * 0x4000 + pointer - 0x4000 if pointer >= 0x4000 else pointer
                    entries.append({"index": actor_index, "x": obj[2] - 4, "y": obj[1] - 4,
                        "sprite": headers[obj[0] - 1][0],
                        "policy": classify(obj, actor_index, item["approved_indices"], commands),
                        "event_flag": None if obj[11:13] == b"\xff\xff" else int.from_bytes(obj[11:13], "little"),
                        "script": sorted(pointers.get(address, []))[:3] if obj[7] & 15 != 5 else []})
                protected = all(entry["policy"] != "replaceable" for entry in entries)
                pairs.append({"id": f"{name}:{index}:{other_index}", "model": overlap,
                    "shared_time_mask": first[6] & second[6] & 15,
                    "actors": entries, "both_protected": protected,
                    "status": "protected-conflict-needs-review" if protected else "optional-admission-pending"})
        code = (REPO / "maps" / (name + ".asm")).read_text()
        rows.append({"map": name, "pairs": pairs, "unmodelled_asset_indices": unknown,
                     "has_scripted_object_motion": bool(re.search(r"^\s*(?:moveobject|applymovement|applyonemovement)\b", code, re.M))})
    pairs = [pair for row in rows for pair in row["pairs"]]
    return {"rom_sha256": hashlib.sha256(data).hexdigest(), "map_count": len(rows),
        "flagged_maps": sum(bool(row["pairs"]) for row in rows), "flagged_pairs": len(pairs),
        "protected_pairs": sum(pair["both_protected"] for pair in pairs),
        "unconditional_initial_protected_pairs": sum(pair["both_protected"] and pair["model"] == "initial-row" and
            all(actor["event_flag"] is None for actor in pair["actors"]) for pair in pairs),
        "limitations": ["Candidates ignore collision/reachability and mutually exclusive story states; they require review.",
            "Ordinary wandering envelopes include continuous movement; script-repositioned actors need runtime fixtures.",
            "Special/variable assets, partial pixel-row overlaps and effects need separate geometry/lifecycle tests.",
            "Protected is a safety default, not a claim that every protected actor is indispensable.",
            "Optional admission is approved but not implemented; flagged pairs remain unresolved."], "maps": rows}


def write_pairs_csv(result, output):
    with output.open("w", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(("map", "pair", "model", "both_protected", "status", "first_policy",
                         "second_policy", "first_xy", "second_xy", "first_flag", "second_flag"))
        for row in result["maps"]:
            for pair in row["pairs"]:
                first, second = pair["actors"]
                writer.writerow((row["map"], pair["id"], pair["model"], pair["both_protected"], pair["status"],
                    first["policy"], second["policy"], f"{first['x']},{first['y']}", f"{second['x']},{second['y']}",
                    first["event_flag"], second["event_flag"]))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rom", type=Path, required=True)
    parser.add_argument("--sym", type=Path, required=True)
    parser.add_argument("--policy", type=Path, default=REPO / "mobile_adapter/research/native-lease-policy.json")
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--csv", type=Path, help="Also write a fresh reviewable pair inventory")
    parser.add_argument("--strict", action="store_true", help="Fail on unresolved conflicts instead of producing only a diagnostic")
    args = parser.parse_args()
    require(not args.output.exists(), "Choose a fresh scanline report path")
    require(args.csv is None or not args.csv.exists(), "Choose a fresh CSV report path")
    result = audit(args.rom, args.sym, args.policy)
    args.output.write_text(json.dumps(result, indent=2) + "\n")
    if args.csv:
        write_pairs_csv(result, args.csv)
    print(json.dumps({key: value for key, value in result.items() if key != "maps"}, indent=2))
    require(not args.strict or not result["flagged_pairs"], "Unresolved map scanline conflicts; see " + str(args.output))


if __name__ == "__main__":
    main()
