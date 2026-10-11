#!/usr/bin/env python3
"""Reviewable protected-default object leasing, separate from OAM suppression.

Compiled complete dialogue commands are necessary but not sufficient: automatic
candidates have no scripted object references, a normal movement and ordinary
fixed sprite graphics or a reviewed plain-text Pokemon icon. Conditional
dialogue may read flags but never writes them. Trainers and special actors stay protected.
Source fingerprints make upstream changes require a fresh review in mobile builds.
"""
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import re
from audit_map_slots import read_symbols
from probe_emulator import require

REPO = Path(__file__).resolve().parents[2]
MANIFEST = REPO / "mobile_adapter/research/native-lease-policy.json"
TABLE = REPO / "mobile_adapter/rom/native_lease_policy.asm"


def plain_icon_text(code, labels):
    """Only local literal dialogue, ending without any executable text command."""
    for label in labels:
        match = re.search(r"^" + re.escape(label) + r":\s*\n((?:[ \t]+[^\n]*\n)+)", code, re.M)
        if not match:
            continue
        lines = [line.strip() for line in match[1].splitlines() if line.strip()]
        if (len(lines) >= 2 and lines[-1] == "done" and lines[0].startswith('text "') and
                all(re.fullmatch(r'(?:text|line|para|cont) "[^"\n]*"', line) for line in lines[:-1])):
            return True
    return False


def dialogue_only(data, commands, pointer, bank_start, remaining=32, visited=frozenset()):
    """Accept only a finite graph of flag reads, facing and complete dialogue.

    Every branch must terminate without a state-writing script command. Reject
    loops, bank escapes, truncation and unknown commands rather than guessing.
    """
    if (remaining <= 0 or pointer in visited or not bank_start <= pointer < min(len(data), bank_start + 0x4000)):
        return False
    visited = visited | {pointer}
    command = commands[data[pointer]] if data[pointer] < len(commands) else None
    if command in ("jumptext", "jumptextfaceplayer"):
        return pointer + 3 <= min(len(data), bank_start + 0x4000)
    if command in ("jumpthistext", "jumpthistextfaceplayer"):
        return pointer + 1 < min(len(data), bank_start + 0x4000)
    if command == "end":
        return True
    def next_at(address):
        return dialogue_only(data, commands, address, bank_start, remaining - 1, visited)
    if command in ("checkevent", "checkflag", "showtext", "showtextfaceplayer", "writetext"):
        return pointer + 3 <= bank_start + 0x4000 and next_at(pointer + 3)
    if command in ("faceplayer", "opentext", "closetext", "promptbutton", "waitbutton"):
        return next_at(pointer + 1)
    if command == "turnobject":
        return (pointer + 3 <= bank_start + 0x4000 and data[pointer + 1] == 254 and
                data[pointer + 2] < 4 and next_at(pointer + 3))
    if command in ("iftrue_jumptext", "iffalse_jumptext", "iftrue_jumptextfaceplayer", "iffalse_jumptextfaceplayer"):
        return pointer + 3 <= bank_start + 0x4000 and next_at(pointer + 3)
    if command in ("iftruefwd", "iffalsefwd"):
        return (pointer + 2 <= bank_start + 0x4000 and next_at(pointer + 2) and
                next_at(pointer + 2 + data[pointer + 1]))
    if command in ("iftrue", "iffalse"):
        if pointer + 3 > bank_start + 0x4000:
            return False
        target = int.from_bytes(data[pointer + 1:pointer + 3], "little")
        return 0x4000 <= target < 0x8000 and next_at(pointer + 3) and next_at(bank_start + target - 0x4000)
    return False

def generate(rom_path, sym_path):
    data, symbols = rom_path.read_bytes(), read_symbols(sym_path)
    commands = re.findall(r"^\s*const (\w+)_command\s*$",
                          (REPO / "macros/scripts/events.asm").read_text(), re.M)
    text_commands = {commands.index(name) for name in ("jumptext", "jumptextfaceplayer")}
    show_commands = {commands.index(name) for name in ("showtext", "showtextfaceplayer")}
    names = re.findall(r"^\s*map (\w+),", (REPO / "data/maps/maps.asm").read_text(), re.M)
    map_ids, group, number = [], 0, 0
    for line in (REPO / "constants/map_constants.asm").read_text().splitlines():
        if re.match(r"\s*newgroup\s*(;.*)?$", line):
            group, number = group + 1, 0
        if re.match(r"\s*map_const ", line):
            number += 1
            map_ids.append((group, number))
    require(len(names) == len(map_ids) == 611, "Map inventory changed")
    # Exclude script-targeted actors even when their own conversation is benign.
    texts = {p: p.read_text() for p in (REPO / "maps").glob("*.asm")}
    native_sources = {p: p.read_text() for p in REPO.rglob("*.asm")
                      if "mobile_adapter" not in p.parts}
    all_code = "\n".join(re.sub(r";[^\n]*", "", text) for text in native_sources.values())
    tokens = Counter(re.findall(r"\b[A-Za-z_][A-Za-z_0-9]*\b", all_code))
    headers = re.findall(r"^\s*overworld_sprite (\w+), (\w+),",
                         (REPO / "data/sprites/sprites.asm").read_text(), re.M)
    labels_by_pointer = {}
    for label, pointer in symbols.items():
        labels_by_pointer.setdefault(pointer, []).append(label)
    dependencies = ('data/sprites/map_objects.asm', 'constants/map_object_constants.asm',
                    'constants/sprite_constants.asm', 'macros/scripts/maps.asm',
                    'macros/scripts/events.asm', 'data/sprites/sprites.asm',
                    'home/map_objects.asm', 'engine/overworld/overworld.asm')
    sources = {path: hashlib.sha256((REPO / path).read_bytes()).hexdigest() for path in dependencies}
    rows, masks, indices, offsets, unreferenced = [], [0], [], [], []
    for name, (group, number) in zip(names, map_ids):
        source = REPO / "maps" / (name + ".asm")
        code = re.sub(r";[^\n]*", "", texts[source])
        constants = []
        active = False
        for line in code.splitlines():
            if line.strip() == "object_const_def":
                active = True
            match = re.match(r"\s*const (\w+)\s*$", line)
            if active and match:
                constants.append(match[1])
        cursor = symbols[name + "_MapScriptHeader"]
        for size in (2, 3, 5, 5, 5):
            cursor += 1 + data[cursor] * size
        count = data[cursor]
        require(count <= 20, f"Object format changed: {name}")
        approved = []
        for index in range(1, count + 1):
            obj = data[cursor + 1 + 13 * (index - 1):cursor + 1 + 13 * index]
            kind = obj[7] & 15
            script = int.from_bytes(obj[9:11], "little")
            pointer = cursor // 0x4000 * 0x4000 + script - 0x4000 if script >= 0x4000 else script
            fixed_sprite = 1 <= obj[0] <= len(headers) and headers[obj[0] - 1][1] in (
                "WALKING_SPRITE", "STANDING_SPRITE")
            ordinary_movement = obj[3] in (1, 2, 4, 5, 6, 7, 8, 9, 10, 24, 25, 35, 36, 37, 38)
            icon = (obj[0] == 0xee and obj[3] == 16 and kind == 4 and
                    plain_icon_text(code, labels_by_pointer.get(pointer, [])))
            # These shared-tile decorations are leased only while OFF_SCREEN
            # and outside the protected camera sequence. Their atlas is never
            # overwritten, and all original object fields are restored.
            window = (name == 'SnowtopMountainOutside' and obj[0] == 0xbf and obj[3] == 0x2d and
                      kind == 0 and script == symbols['DoNothingScript'] & 0xffff and
                      index in range(2, 13))
            pure = ((kind == 5 and obj[8] in text_commands) or
                    (kind in (0, 6) and dialogue_only(data, commands, pointer, cursor // 0x4000 * 0x4000)))
            body = data[pointer:pointer + 7]
            cosmetic = (kind in (0, 6) and len(body) == 7 and body[0] in show_commands and
                        body[3] == commands.index("turnobject") and body[4] == 254 and
                        body[5] < 4 and body[6] == commands.index("end"))
            noop = kind in (0, 6) and script == symbols["DoNothingScript"] & 0xffff
            referenced = index <= len(constants) and tokens[constants[index - 1]] != 1
            # Anonymous numeric object references cannot be traced by symbol.
            numeric_reference = re.search(
                r"^\s*(?:appear|disappear|turnobject|moveobject|applymovement|"
                r"applyonemovement|showemote|follow|faceobject)\s+(?:\d+|\$[0-9a-fA-F]+)\b",
                code, re.M) is not None
            # Keep this wandering actor to retain the audited native-motion scene.
            fixture_protected = name == "GoldenrodGameCorner" and index in (5, 12)
            if (((fixed_sprite and ordinary_movement and (pure or cosmetic or noop)) or icon or window) and
                    "text_asm" not in code and "tx_asm" not in code and
                    not referenced and not numeric_reference and not fixture_protected):
                approved.append(index)
                if index <= len(constants):
                    unreferenced.append(constants[index - 1])
        mask = sum(1 << index for index in approved)
        if mask not in masks:
            masks.append(mask)
        if number == 1:
            offsets.append(len(indices))
        indices.append((name, masks.index(mask)))
        # Every map can introduce protected scanline or slot pressure, including
        # maps with no currently replaceable actors. Guard its geometry too.
        sources[str(source.relative_to(REPO))] = hashlib.sha256(source.read_bytes()).hexdigest()
        rows.append({"map": name, "group": group, "number": number,
                     "objects": count, "approved_indices": approved})
    for path in ("macros/scripts/events.asm", "macros/scripts/maps.asm", "macros/data.asm",
                 "data/maps/maps.asm", "constants/map_constants.asm",
                 "data/sprites/sprites.asm", "constants/map_object_constants.asm",
                 "home/header.asm", "data/maps/blocks.asm", "data/maps/attributes.asm",
                 "data/sprites/facings.asm", "engine/overworld/map_objects.asm"):
        source = REPO / path
        sources[path] = hashlib.sha256(source.read_bytes()).hexdigest()
    # Several maps share block data. Guard original inputs and alias definitions,
    # not the timestamp-dependent generated compressed files.
    for source in sorted((REPO / "maps").glob("*.ablk")) + sorted((REPO / "data/tilesets").glob("*_collision.asm")):
        sources[str(source.relative_to(REPO))] = hashlib.sha256(source.read_bytes()).hexdigest()
    require(len(masks) <= 256, "Lease mask dictionary exceeds one-byte indices")
    assembly = ["; Audited native leases. Generated separately from render suppression.",
                "PUSHS", 'SECTION "Mobile Native Lease Policy", ROMX', "MobileNativePolicyStart::", "MobileNativeLoadLeasePolicy:",
                "; Bank 1 on entry/exit. Fetch the protected-default map mask once.",
                "\tld a, [wMapGroup]", "\tdec a", "\tadd a", "\tld e, a", "\tld d, 0",
                "\tld hl, MobileLeaseGroupOffsets", "\tadd hl, de",
                "\tld a, [hli]", "\tld h, [hl]", "\tld l, a",
                "\tld a, [wMapNumber]", "\tdec a", "\tld e, a", "\tadd hl, de",
                "\tld a, [hl]", "\tld l, a", "\tld h, 0", "\tld d, h", "\tld e, l",
                "\tadd hl, hl", "\tadd hl, de", "\tld de, MobileLeaseMasks", "\tadd hl, de",
                "\tld a, MOBILE_WRAM_BANK", "\tldh [rWBK], a",
                "\tld de, wMobileLeaseMask", "\tld bc, 3", "\trst CopyBytes",
                "\tld a, 1", "\tldh [rWBK], a", "\tret", "MobileLeaseGroupOffsets:"]
    assembly += [f"\tdw MobileLeaseMapIndices + {offset}" for offset in offsets]
    assembly += ["MobileLeaseMapIndices:"]
    assembly += [f"\tdb {index} ; {name}" for name, index in indices]
    assembly += ["MobileLeaseMasks:"]
    assembly += [f"\tdb ${mask & 255:02x}, ${(mask >> 8) & 255:02x}, ${(mask >> 16) & 255:02x}" for mask in masks]
    assembly += ["MobileNativePolicyEnd::", "ASSERT MobileNativePolicyEnd - MobileNativePolicyStart <= 1280", "POPS"]
    return "\n".join(assembly) + "\n", {"version": 2, "policy": "unreferenced-finite-dialogue-graph-only",
        "generator_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "source_sha256": sources, "unreferenced_constants": sorted(unreferenced), "maps": rows}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rom", type=Path)
    parser.add_argument("--sym", type=Path)
    parser.add_argument("--write", action="store_true")
    parser.add_argument("--check-sources", action="store_true")
    args = parser.parse_args()
    if args.check_sources:
        manifest = json.loads(MANIFEST.read_text())
        require(hashlib.sha256(Path(__file__).read_bytes()).hexdigest() == manifest["generator_sha256"],
                "Lease audit algorithm changed; regenerate and review the policy")
        for path, expected in manifest["source_sha256"].items():
            require(hashlib.sha256((REPO / path).read_bytes()).hexdigest() == expected,
                    f"Native lease source changed; review policy before building mobile: {path}")
        native = "\n".join(re.sub(r";[^\n]*", "", p.read_text()) for p in REPO.rglob("*.asm")
                           if "mobile_adapter" not in p.parts)
        tokens = Counter(re.findall(r"\b[A-Za-z_][A-Za-z_0-9]*\b", native))
        for name in manifest["unreferenced_constants"]:
            require(tokens[name] == 1, f"Leased actor gained a script reference; review policy: {name}")
        print("Audited native lease sources match")
        return
    require(args.rom and args.sym, "Compiled ROM and symbols are required")
    table, manifest = generate(args.rom, args.sym)
    if args.write:
        TABLE.write_text(table)
        MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n")
    else:
        require(TABLE.read_text() == table and json.loads(MANIFEST.read_text()) == manifest,
                "Native lease policy changed; review the candidate list")
    print(f"Checked {len(manifest['maps'])} maps; "
          f"{sum(len(row['approved_indices']) for row in manifest['maps'])} approved native actors")


if __name__ == "__main__":
    main()
