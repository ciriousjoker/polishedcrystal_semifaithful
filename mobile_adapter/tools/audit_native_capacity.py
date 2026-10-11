#!/usr/bin/env python3
"""Conservative native-slot bound after protected-default NPC leasing.

Passing a bound is useful static evidence, not a proof of script-created actors,
changed movement flags or all story states. Unresolved maps require real fixtures
and source review; they are never silently declared compatible.
"""
import argparse
from collections import deque
import hashlib
import json
from pathlib import Path
import re
from audit_map_slots import max_snapshot, read_symbols
from probe_emulator import require
from build_crowd_route import terrain

REPO = Path(__file__).resolve().parents[2]


def terrain_bound(rom_path, sym_path, name, objects, width, height, retained, collision):
    """Overapproximate ordinary walking/surfing and every literal tile change.

    Actor anchors remain traversable for this upper bound. Native side walls
    use their eight direction masks; one-way ledges are overapproximated.
    Scripted camera movement is outside this ordinary-exploration model. Cart
    retention covers all regions because native scripts cross their boundaries.
    """
    _, _, _, cells, collisions = terrain(rom_path, sym_path, name, collision)
    data, symbols = rom_path.read_bytes(), read_symbols(sym_path)
    permission = symbols['CollisionPermissionTable']
    positions = {point for point, (_, kind, _) in cells.items() if kind in (0, 1)}
    variants = {point: {value} for point, (value, _, _) in cells.items()}
    changes = re.findall(r'^\s*changeblock\s+(\d+),\s*(\d+),\s*\$([0-9a-fA-F]+)',
                         (REPO / 'maps' / (name + '.asm')).read_text(), re.M)
    for x, y, block in changes:
        x, y, block = int(x), int(y), int(block, 16)
        if name == 'SnowtopMountainOutside' and block == 0x6f:
            # The reviewed script opens this barrier only after setting the
            # panning flag. Its separate camera lifecycle defers native leases.
            continue
        for quadrant in range(4):
            point = x + quadrant % 2, y + quadrant // 2
            if point in cells:
                value = collisions[4 * block + quadrant]
                variants[point].add(value)
                if data[permission + value] in (0, 1):
                    positions.add(point)
    def wall(tile, direction):
        return tile & 0xf0 == 0xb0 and bool((1, 2, 4, 8, 9, 10, 5, 6)[tile & 7] & direction)
    def connected(point, target):
        if target not in positions:
            return False
        dx, dy = target[0] - point[0], target[1] - point[1]
        facing, opposite = (1, 2) if dx > 0 else (2, 1) if dx < 0 else (8, 4) if dy > 0 else (4, 8)
        # Keep one-way paths in the same bound region. Only a wall blocking
        # BOTH travel directions may separate retained-actor lifetimes.
        return any(not wall(a, facing) and not wall(b, opposite)
                   for a in variants[point] for b in variants[target]
                   if data[permission + a] in (0, 1) and data[permission + b] in (0, 1))
    map_code = (REPO / 'maps' / (name + '.asm')).read_text()
    warps = {(int(x), int(y)) for x, y in re.findall(r'^\s*warp_event\s+(\d+),\s*(\d+),', map_code, re.M)}
    positions -= warps
    best, regions = (0, 0, 0, 0), []
    remaining = set(positions)
    while remaining:
        component, queue = set(), deque([min(remaining)])
        while queue:
            point = queue.popleft()
            if point not in remaining:
                continue
            remaining.remove(point)
            component.add(point)
            x, y = point
            queue.extend(target for target in ((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
                         if connected(point, target))
        # A retained actor can only have loaded when an earlier camera window
        # in this same overapproximated region contained its native anchor.
        exposure = component | {point for point in warps if
                    any((point[0] + dx, point[1] + dy) in component and connected(point, (point[0] + dx, point[1] + dy))
                        for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1)))}
        relevant = [obj for obj in objects if name == 'AzaleaGym' or obj[3] not in retained or
                    any((obj[2] + 1 - x) % 256 < 12 and (obj[1] + 1 - y) % 256 < 11
                        for x, y in exposure)]
        value = max_snapshot(relevant, width, height, movement_envelope=True,
                             retained_movements=retained, positions=sorted(exposure))
        regions.append({'tiles': len(component), 'protected_bound': value[0],
                        'sample': min(component), 'warps': sorted(exposure & warps),
                        'retained_indices': [i for i, obj in enumerate(objects, 1) if obj in relevant and obj[3] in retained]})
        # These reviewed maps have no map connections. Ordinary entries (and
        # Snowtop's lower fly spawn) lead to a warp-connected region. Camera
        # panning above Snowtop's closed barrier has its own deferred lifecycle.
        if name not in ('CinnabarVolcano1F', 'SnowtopMountainOutside') or exposure & warps:
            best = max(best, value)
    return best, {'model': 'land-water-and-all-literal-block-variants', 'regions': regions,
                  'block_changes': len(changes), 'witness': best,
                  'cart_retention_across_scripted_paths': name == 'AzaleaGym',
                  'collision_sha256': hashlib.sha256(collisions).hexdigest()}


def check_sources():
    contracts = json.loads((REPO / 'mobile_adapter/research/native-capacity-contracts.json').read_text())
    for path, expected in contracts['source_sha256'].items():
        require(hashlib.sha256((REPO / path).read_bytes()).hexdigest() == expected,
                'Reviewed native capacity behavior changed: ' + path)


def audit(rom_path, sym_path, policy_path):
    data, symbols = rom_path.read_bytes(), read_symbols(sym_path)
    policy = json.loads(policy_path.read_text())
    require(policy['version'] == 2, 'Review changed native lease policy version')
    check_sources()
    # Resolve the bounded native enum, including its explicit const_next gap.
    sprite_ids, next_id = {}, 0
    for line in (REPO / 'constants/sprite_constants.asm').read_text().splitlines():
        code = line.partition(';')[0].strip()
        if code.startswith('const_next '):
            next_id = int(code.split()[1].replace('$', '0x'), 0)
        match = re.fullmatch(r'const (SPRITE_\w+)', code)
        if match:
            sprite_ids[match[1]], next_id = next_id, next_id + 1
    special = {sprite_ids[name] for name in ('SPRITE_BIG_GYARADOS', 'SPRITE_ALOLAN_EXEGGUTOR', 'SPRITE_SAILBOAT')}
    variable_start = sprite_ids['SPRITE_CONSOLE']
    rows = []
    for item in policy['maps']:
        name = item['map']
        cursor = symbols[name + '_MapScriptHeader']
        for size in (2, 3, 5, 5, 5):
            cursor += 1 + data[cursor] * size
        count = data[cursor]
        require(count == item['objects'] <= 20, 'Policy object inventory changed: ' + name)
        objects = [data[cursor + 1 + i * 13:cursor + 1 + (i + 1) * 13] for i in range(count)]
        protected = [obj for i, obj in enumerate(objects, 1) if i not in item['approved_indices']]
        limit = 11 if any(o[0] in special or o[0] >= variable_start for o in objects) else 12
        attributes = symbols[name + '_MapAttributes']
        height, width = data[attributes + 1:attributes + 3]
        movement = symbols['SpriteMovementData']
        retained = frozenset(o[3] for o in protected if data[movement + o[3] * 6 + 3] & 2)
        bound = max_snapshot(protected, width, height, movement_envelope=True, retained_movements=retained)
        refined, review = bound, None
        if name in ('AzaleaGym', 'CinnabarVolcano1F', 'SnowtopMountainOutside'):
            collision_name = {'AzaleaGym': 'gym', 'CinnabarVolcano1F': 'volcano',
                              'SnowtopMountainOutside': 'snowtop_mountain'}[name]
            refined, review = terrain_bound(rom_path, sym_path, name, protected, width, height, retained,
                                          REPO / 'data/tilesets' / (collision_name + '_collision.asm'))
        elif name == 'CinnabarLab':
            require(count == 11 and not item['approved_indices'], 'Lab actor inventory changed')
            # The ordinary native gender table appears exactly one of four
            # substitutes; every other actor is still counted as present.
            require(len({tuple(obj[1:3]) for obj in objects[7:11]}) == 1, 'Lab substitute anchors changed')
            refined = max(max_snapshot(objects[:7] + [avatar], width, height,
                                        movement_envelope=True, retained_movements=retained)
                          for avatar in objects[7:11])
            review = {'model': 'four-native-gender-substitutes-at-most-one', 'all_other_actors_counted': True}
        elif name == 'Route36NationalParkGate':
            require(count == 12 and not item['approved_indices'], 'Contest actor inventory changed')
            # Native selection reveals five of ten. Count BOTH officers to
            # remain conservative during their callback transitions.
            refined = (7, 0, 0, 0)
            review = {'model': 'five-native-contestants-plus-both-officers'}
        rows.append({'map': name, 'protected_bound': bound[0], 'nonplayer_slot_limit': limit,
                     'required_remote_slots': 3, 'static_bound_fits': bound[0] + 3 <= limit,
                     'reviewed_bound': refined[0], 'reviewed_bound_fits': refined[0] + 3 <= limit,
                     'review': review,
                     'geometry_witness': {'x': bound[1], 'y': bound[2], 'time': bound[3]},
                     'approved_indices': item['approved_indices']})
    return {'rom_sha256': hashlib.sha256(data).hexdigest(), 'map_count': len(rows),
            'bounded_maps': sum(r['static_bound_fits'] for r in rows),
            'reviewed_bounded_maps': sum(r['reviewed_bound_fits'] for r in rows),
            'unresolved_maps': [r for r in rows if not r['reviewed_bound_fits']],
            'limitations': ['The original static bounds ignore story flags and retain actors across disconnected regions.',
                            'Refined bounds depend on fingerprinted native entry, story and camera lifecycle contracts.',
                            'Snowtop panning is a protected deferred scene; off-screen shared-tile window records may be leased afterward.',
                            'Native script-created actors and dynamic movement/asset changes need separate lifecycle fixtures.',
                            'This is a slot bound, not a scanline/OAM, timing or full gameplay proof.'],
            'maps': rows}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rom', type=Path)
    parser.add_argument('--sym', type=Path)
    parser.add_argument('--policy', type=Path, default=REPO / 'mobile_adapter/research/native-lease-policy.json')
    parser.add_argument('--output', type=Path)
    parser.add_argument('--check-sources', action='store_true')
    args = parser.parse_args()
    if args.check_sources:
        check_sources()
        print('Reviewed native capacity sources match')
        return
    require(args.rom and args.sym and args.output, 'Provide --rom, --sym and --output')
    result = audit(args.rom, args.sym, args.policy)
    require(not args.output.exists(), 'Choose a new audit output')
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({k: v for k, v in result.items() if k != 'maps'}, indent=2))
    require(not result['unresolved_maps'], 'Unresolved native map capacity; see ' + str(args.output))


if __name__ == '__main__':
    main()
