#!/usr/bin/env python3
"""Generate all acceptance event tables for a single optional MBC30 ROM pair."""
import argparse
import json
import math
from pathlib import Path
import re
import sys

SCENES = ("empty", "pool", "palettes", "scanline", "interaction", "script", "link", "save", "private-save", "warp", "npc-movement", "night", "dark")

def symbols(path):
    result = {}
    for line in path.read_text().splitlines():
        match = re.match(r"([\da-fA-F]+):([\da-fA-F]+)\s+(\S+)", line)
        if match:
            result[match[3]] = (int(match[1], 16), int(match[2], 16))
    return result


def rom_offset(binding):
    bank, address = binding
    return bank * 0x4000 + address - (0x4000 if address >= 0x4000 else 0)

def floor_area(source, name):
    # Current map order and collision source are queried, not frozen numeric IDs.
    import sys
    sys.path.insert(0, str(Path(__file__).parent))
    from audit_independent_maps import map_inventory
    record = next(row for row in map_inventory(source) if row["map"] == name)
    tileset = re.search(rf"^\s*map {name}, TILESET_(\w+),", (source / "data/maps/maps.asm").read_text(), re.M)[1].lower()
    collision_rows = re.findall(r"^\s*tilecoll\s+([^;\n]+)",
        (source / f"data/tilesets/{tileset}_collision.asm").read_text(), re.M)
    collisions = [[token.strip() for token in row.split(",")] for row in collision_rows]
    if not collisions or len({len(row) for row in collisions}) != 1:
        raise RuntimeError("Native collision entry format cannot be derived")
    cells = math.isqrt(len(collisions[0]))
    if cells * cells != len(collisions[0]):
        raise RuntimeError("Native collision entries no longer describe a square block")
    blocks = (source / f"maps/{name}.ablk").read_bytes()
    width, height = record["width"] * cells, record["height"] * cells
    record["collision_cells_per_block_axis"] = cells
    def floor(x, y):
        block = blocks[(y // cells) * record["width"] + x // cells]
        return collisions[block][(y % cells) * cells + x % cells] == "FLOOR"
    candidates = []
    for row in range(2, height - 2):
        for x in range(1, width - 7):
            if not all(floor(x + dx, row) for dx in range(8)):
                continue
            if not floor(x + 3, row + 1) or not all(floor(x + dx, row - 1) for dx in (4, 5, 6)):
                continue
            points = [(px, py) for py in range(row - 1, row + 3) for px in range(x, x + 8) if floor(px, py)]
            if len(points) >= 16:
                candidates.append((x, row - 1, points))
    if not candidates:
        raise RuntimeError(f"Declared assumption failed: {name} lacks an eight-cell floor row and sixteen visible floor positions")
    record["origin"] = list(candidates[0][:2])
    record["floor_points"] = candidates[0][2]
    spawn_source = (source / "engine/overworld/spawn_points.asm").read_text().split("EnterMapSpawnPoint:", 1)[1]
    record["spawn_fields"] = re.findall(r"ld \[(wMapGroup|wMapNumber|wXCoord|wYCoord)\], a", spawn_source)[:4]
    if set(record["spawn_fields"]) != {"wMapGroup", "wMapNumber", "wXCoord", "wYCoord"}:
        raise RuntimeError("Native spawn data order cannot be derived from its current loader")
    return record


def palette_choices(meta):
    source = Path(meta["source"])
    declarations = (source / "constants/sprite_data_constants.asm").read_text().split("; MapObjectPals indexes", 1)[1].split("DEF NUM_OW_TIME_OF_DAY_PALS", 1)[0]
    names = re.findall(r"^\s*ow_npc_pal_const\s+(\w+)", declarations, re.M)
    times = (source / "constants/ram_constants.asm").read_text().split("\tconst MORN", 1)[1].split("DEF NUM_DAYTIMES", 1)[0]
    daytime = ["MORN", *re.findall(r"^\s*const\s+(\w+)", times, re.M)].index("DAY")
    rom = source / f"polishedcrystal-{meta['version']}.gbc"
    start = rom_offset(symbols(rom.with_suffix(".sym"))["MapObjectPals"]) + daytime * len(names) * 8
    data = rom.read_bytes()
    player_source = (source / "engine/overworld/player_object.asm").read_text()
    player_palettes = re.findall(r"assert PLAYER_\w+\s*\+\s*1\s*==\s*PAL_NPC_(\w+)", player_source)
    if len(player_palettes) != 4 or not all(name in names for name in player_palettes):
        raise RuntimeError("Declared native avatar palette mapping interface changed")
    avatar_colors = {data[start + names.index(name) * 8:start + (names.index(name) + 1) * 8]
                     for name in player_palettes}
    unique = {}
    for index, name in enumerate(names):
        encoded = bytes(data[start + index * 8:start + (index + 1) * 8])
        # Avoid the four avatar defaults so the crowded scene adds seven colors.
        if encoded in avatar_colors:
            continue
        unique.setdefault(encoded, name)
    if len(unique) < 7:
        raise RuntimeError("Declared palette-table prerequisite failed: fewer than seven distinct non-avatar day palettes")
    return [{"name": name, "encoded_day_colors": list(encoded)} for encoded, name in list(unique.items())[:7]]



def generate(source, baseline, output):
    source, baseline = source.resolve(), baseline.resolve()
    meta = {"source": str(source), "version": re.search(r"^VERSION := (\S+)", (source / "Makefile").read_text(), re.M)[1]}
    if baseline != source / f"polishedcrystal-{meta['version']}.gbc":
        raise RuntimeError("Scenario generation requires the current clean counterpart")
    record = floor_area(source, "GoldenrodGameCorner")
    palette_data = palette_choices(meta)
    x, y = record["origin"]
    primary = (x + 3, y + 1)
    peers = [(x + 4, y + 1), (x + 5, y + 1), (x + 6, y + 1)]
    colors = [row["name"] for row in palette_data]
    text = 'SECTION "Mobile Acceptance Scenarios", ROMX, BANK[$f0]\nAcceptanceScenarioData::\n'
    scenarios = {}
    headers = []
    counts = []
    controls = []
    for scene_id, scene in enumerate(SCENES):
        prefix = "Acceptance_" + scene.replace("-", "_")
        # Native event/script pointers are part of the game's ordinary SRAM.
        # Give each scenario the same declarative addresses in both ROMs, even
        # when the native control adds three extra object-header records.
        text += f"\tds {scene_id} * $200 - (@ - AcceptanceScenarioData), 0\n"
        count = 12 if scene == "pool" else 7 if scene == "palettes" else 4 if scene == "scanline" else 1 if scene in ("interaction", "script", "link", "save", "private-save", "warp", "npc-movement") else 0
        points = [tuple(p) for p in record["floor_points"] if tuple(p) not in [primary, *peers]]
        if scene == "scanline":
            points = [(x, y + 1), (x + 1, y + 1), (x + 2, y + 1), (x + 7, y + 1)]
        if count == 1:
            points = [(primary[0], primary[1] + 1)]
        equal = scene in ("empty", "scanline", "night", "dark", "npc-movement")
        headers.append(prefix + "_Header")
        counts.append(count)
        controls.append(int(equal))
        text += f"{prefix}_Header:\n\tdef_scene_scripts\n\tdef_callbacks\n\tcallback MAPCALLBACK_OBJECTS, {prefix}_Ready\n\tdef_warp_events\n\tdef_coord_events\n\tdef_bg_events\n\tdef_object_events\n"
        for i, (px, py) in enumerate(points[:count]):
            if scene == "pool" and i >= 6:
                text += f"\titemball_event {px}, {py}, POTION, 1, -1\n"
            else:
                text += f"\tobject_event {px}, {py}, SPRITE_POKEFAN_M, SPRITEMOVEDATA_STANDING_DOWN, 0, 0, -1, PAL_NPC_{colors[i % 7]}, OBJECTTYPE_SCRIPT, 0, {prefix}_NPC, -1\n"
        if equal:
            text += "if !DEF(MOBILE_ADAPTER)\n"
            for (px, py), name in zip(peers, ("KRIS", "CRYS", "BETA")):
                text += f"\tobject_event {px}, {py}, SPRITE_{name}, SPRITEMOVEDATA_STANDING_DOWN, 0, 0, -1, 0, OBJECTTYPE_SCRIPT, 0, {prefix}_NPC, -1\n"
            text += "endc\n"
        text += f"{prefix}_Ready:\n\tcallasm AcceptanceApplyObjectConfig\n\tcallasm AcceptanceReadyMark\n"
        if scene in ("save", "private-save"):
            text += "\tcallasm AcceptancePauseGameTimer\n"
        text += f"\tcallasm AcceptanceSave\n\tendcallback\n{prefix}_NPC:\n\topentext\n\twritetext {prefix}_Text\n\twaitbutton\n\tclosetext\n"
        if scene == "private-save":
            text += "\tcallasm AcceptanceApplyInteractionFault\n\tcallasm AcceptanceSave\n"
        elif scene == "save":
            text += "\tcallasm AcceptanceSave\n"
        elif scene == "link":
            text += "\tcallasm AcceptanceLink\n"
        elif scene == "script":
            text += "\tloadmem wInputFlags, 255\n\tpause 120\n\tloadmem wInputFlags, 0\n"
        elif scene == "warp":
            text += "\twarp PLAYERS_HOUSE_2F, 3, 3\n"
        elif scene == "npc-movement":
            text += f"\tapplymovement 1, {prefix}_Movement\n"
        text += f"\tsetevent EVENT_TEMPORARY_UNTIL_MAP_RELOAD_1\n\tcallasm AcceptanceDialogMark\n\tend\n{prefix}_Text:\n\ttext \"TEST\"\n\tdone\n"
        if scene == "npc-movement":
            text += f"{prefix}_Movement:\nrept 64\n\tstep_right\n\tstep_left\nendr\n\tstep_end\n"
        scenarios[scene] = {"scene_id": scene_id, "scene": scene, "map": record, "primary": primary,
            "peers": peers, "ordinary_objects": count, "equal_actor_load": equal,
            "native_object_positions": points[:count], "palettes": palette_data,
            "limitation": None if equal else "Native control retains original actors; saturated native hardware cannot represent every extra remote as a correct native avatar.",
            "clock_input": "fixed clock for saves" if scene in ("save", "private-save") else "native clock"}
    text += f"\tds {len(SCENES)} * $200 - (@ - AcceptanceScenarioData), 0\n"
    text += "AcceptanceScenarioPointers:\n" + "".join(f"\tdw {label}\n" for label in headers) + "AcceptanceScenarioPointersEnd:\n"
    text += f"AcceptanceScenarioTotal:: db {len(SCENES)}\n"
    text += "AcceptanceScenarioCounts:\n\tdb " + ", ".join(map(str, counts)) + "\n"
    text += "AcceptanceScenarioControls:\n\tdb " + ", ".join(map(str, controls)) + "\n"
    text += 'AcceptanceEmptyDestination:\n\tdef_scene_scripts\n\tdef_callbacks\n\tdef_warp_events\n\tdef_coord_events\n\tdef_bg_events\n\tdef_object_events\n'
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(text)
    output.with_suffix('.json').write_text(json.dumps(scenarios, indent=2))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, default=Path.cwd())
    parser.add_argument('--baseline', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    generate(args.source, args.baseline, args.output)
