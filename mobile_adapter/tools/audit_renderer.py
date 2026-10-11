#!/usr/bin/env python3
"""Check emitted avatar geometry and the pinned overworld tile allocation."""
import argparse
from pathlib import Path
import re
from probe_emulator import read_symbols, require

REPO = Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--rom", type=Path, required=True)
parser.add_argument("--sym", type=Path, required=True)
args = parser.parse_args()
rom = args.rom.read_bytes()
symbols = read_symbols(args.sym)


def offset(name):
    bank, address = symbols[name]
    return bank * 0x4000 + address - (0x4000 if bank else 0)


def facing(table, index):
    start = offset(table) + index * 2
    pointer = int.from_bytes(rom[start:start + 2], "little")
    bank = symbols[table][0]
    start = bank * 0x4000 + pointer - 0x4000
    return bytearray(rom[start:start + 17])


for index in range(16):
    native, social = facing("Facings", index), facing("MobileSocialFacings", index)
    require(native[0] == social[0] == 4, "Approved avatar must have four quads")
    for attribute in range(3, 17, 4):
        native[attribute] &= 0xe0
    require(native == social, f"Facing {index} differs from native geometry")

# A change in this allocator requires a fresh tile audit, even if it still builds.
allocator = (REPO / "home/map_objects.asm").read_text()
require("cp FIRST_VRAM1_OBJECT_STRUCT" in allocator and
        "sub FIRST_VRAM1_OBJECT_STRUCT" in allocator and
        "add a\n\tadd a\n\tld d, a\n\tadd d\n\tadd d" in allocator,
        "Native 12-tile slot allocation changed")
require("DEF FIRST_VRAM1_OBJECT_STRUCT EQU 8" in
        (REPO / "constants/map_object_constants.asm").read_text(), "VRAM split changed")
reservations = {0: set(range(0x64, 0x6c)) | set(range(0xe0, 0xec)),
                1: set(range(0x7c, 0x80))}
for slot in range(13):
    bank = int(slot >= 8)
    base = (slot - 8 if bank else slot) * 12
    standing = set(range(base, base + 12))
    walking_base = base + (0x40 if bank else 0x80)
    walking = set(range(walking_base, walking_base + 12))
    require(not (standing | walking) & reservations[bank], "Avatar overlaps ordinary NPC tiles")
require(not set(range(0x60, 0x64)) & reservations[0], "Avatar overlaps emotes")
require(not set(range(0x6f, 0x80)) & reservations[0], "Avatar overlaps weather/effect tiles")

names = re.findall(r"^\s*const (SPRITE_\w+)\s*(?:;.*)?$",
                   (REPO / "constants/sprite_constants.asm").read_text(), re.M)
headers = offset("SpriteHeaders")
expected = {"SPRITE_CHRIS": 1, "SPRITE_SAILBOAT": 2, "SPRITE_BIG_HO_OH": 1,
            "SPRITE_BIG_LUGIA": 2, "SPRITE_BIG_GYARADOS": 4, "SPRITE_ALOLAN_EXEGGUTOR": 4}
for name, kind in expected.items():
    index = names.index(name) - 1
    require((rom[headers + index * 4 + 3] >> 6) + 1 == kind, f"Sprite type changed: {name}")
# Fifteen-tile final-slot walking sprites overlap 7c..7e and must defer avatars.
for name in ("SPRITE_BIG_GYARADOS", "SPRITE_ALOLAN_EXEGGUTOR"):
    require("cp " + name in (REPO / "mobile_adapter/rom/overworld.asm").read_text(),
            "Missing protected large sprite")
print("Renderer passed: 16 native facings, 13 tile slots, emotes/effects and six special headers")
