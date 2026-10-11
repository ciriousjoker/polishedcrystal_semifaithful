#!/usr/bin/env python3
"""Build an ordinary-controller walk route that loads retained map actors.

This is a conservative fixture planner, not a reachability proof. The real core
must reach every waypoint without RAM writes. Jumps, objects
and warps are excluded; collision or script surprises cause a test failure.
"""
import argparse
from collections import deque
import hashlib
import json
from pathlib import Path
import re
from audit_map_slots import read_symbols
from probe_emulator import require

REPO = Path(__file__).resolve().parents[2]


def terrain(rom_path, sym_path, name, collision_path):
    """Return compiled native collision permissions, including actor anchors.

    This deliberately does not assume an anchor is permanently occupied. An
    actor can move or be hidden by ordinary gameplay. Scripted movement and
    collision-changing callbacks still need their separate lifecycle checks.
    """
    rom, symbols = rom_path.read_bytes(), read_symbols(sym_path)
    pointer = symbols[name + "_MapScriptHeader"]
    for size in (2, 3, 5, 5, 5):
        pointer += 1 + rom[pointer] * size
    objects = [rom[pointer + 1 + i * 13:pointer + 1 + (i + 1) * 13] for i in range(rom[pointer])]
    attributes = symbols[name + "_MapAttributes"]
    height, width = rom[attributes + 1:attributes + 3]
    blocks = (REPO / "maps" / (name + ".ablk")).read_bytes()
    require(len(blocks) == width * height, "Unsupported map block format")
    collisions = collision_path.with_suffix(".bin").read_bytes()
    labels = [re.findall(r"\b[A-Z_]+\b", line.partition(";")[0]) for line in collision_path.read_text().splitlines() if "tilecoll" in line]
    permission = symbols["CollisionPermissionTable"]
    require(len(collisions) == 4 * len(labels), "Collision format changed")
    tiles = {}
    for y in range(height * 2):
        for x in range(width * 2):
            block = blocks[(y // 2) * width + x // 2]
            quadrant = (y % 2) * 2 + x % 2
            value = collisions[4 * block + quadrant]
            tiles[(x, y)] = (value, rom[permission + value], labels[block][quadrant])
    return rom, symbols, objects, tiles, collisions


def plan(rom_path, sym_path, name, collision_path, start):
    rom, symbols, objects, terrain_tiles, collisions = terrain(rom_path, sym_path, name, collision_path)
    blocked = {(o[2] - 4, o[1] - 4) for o in objects}
    map_code = (REPO / "maps" / (name + ".asm")).read_text()
    blocked |= {(int(x), int(y)) for x, y in re.findall(r"^\s*warp_event\s+(\d+),\s*(\d+),", map_code, re.M)}
    walk, tiles = set(), {}
    for position, (value, permission, kind) in terrain_tiles.items():
        if permission == 0 and "LEDGE" not in kind and position not in blocked:
            walk.add(position)
            tiles[position] = value
    require(start in walk, "Fixture start is not an ordinary unobstructed walk tile; nearby candidates: " +
            str(sorted(walk, key=lambda p: (abs(p[0] - start[0]) + abs(p[1] - start[1]), p))[:8]))
    targets = [i for i, o in enumerate(objects, 1) if rom[symbols["SpriteMovementData"] + o[3] * 6 + 3] & 2]
    def observed(position):
        x, y = position
        return {i for i, o in enumerate(objects, 1) if (o[2] + 1 - x) % 256 < 12 and (o[1] + 1 - y) % 256 < 11}
    def blocked_wall(tile, facing):
        return tile & 0xf0 == 0xb0 and bool((1, 2, 4, 8, 9, 10, 5, 6)[tile & 7] & facing)
    def can_step(position, target):
        dx, dy = target[0] - position[0], target[1] - position[1]
        facing, opposite = (1, 2) if dx > 0 else (2, 1) if dx < 0 else (8, 4) if dy > 0 else (4, 8)
        return not blocked_wall(tiles[position], facing) and not blocked_wall(tiles[target], opposite)
    def paths(position):
        result, queue = {position: []}, deque([position])
        while queue:
            x, y = queue.popleft()
            for next_pos in ((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)):
                if next_pos in walk and next_pos not in result and can_step((x, y), next_pos):
                    result[next_pos] = result[(x, y)] + [next_pos]
                    queue.append(next_pos)
        return result
    route, current, seen, unreachable = [], start, observed(start), []
    for index in targets:
        if index in seen:
            continue
        choices = [(path, position) for position, path in paths(current).items() if index in observed(position)]
        if not choices:
            unreachable.append(index)
            continue
        path, current = min(choices, key=lambda entry: (len(entry[0]), entry[1]))
        route.extend(path)
        for point in path:
            seen |= observed(point)
    return_path = paths(current).get(start)
    require(return_path is not None, "Route cannot return to its starting tile")
    route.extend(return_path)
    return route, {"map": name, "start": start, "waypoints": len(route), "retained_records": targets,
                   "potentially_loaded_records": sorted(seen), "unreachable_retained_records": unreachable,
                   "rom_sha256": hashlib.sha256(rom).hexdigest(),
                   "collision_sha256": hashlib.sha256(collisions).hexdigest()}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rom", type=Path, required=True)
    parser.add_argument("--sym", type=Path, required=True)
    parser.add_argument("--map", required=True)
    parser.add_argument("--collision", type=Path, required=True)
    parser.add_argument("--x", type=int, required=True)
    parser.add_argument("--y", type=int, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    route, metadata = plan(args.rom, args.sym, args.map, args.collision, (args.x, args.y))
    require(not args.output.exists(), "Choose a fresh input path")
    args.output.write_text("".join(f"WALK 3300 {x} {y} 180\n" for x, y in route))
    args.output.with_suffix(".json").write_text(json.dumps(metadata, indent=2) + "\n")
    print(json.dumps(metadata, indent=2))


if __name__ == "__main__":
    main()
