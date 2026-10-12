#!/usr/bin/env python3
"""Check every compiled map against its multiplayer-disabled counterpart.

Independent peers require no ordinary object slots, NPC permissions or map
changes. This checks map data and script references, not every reachable scene
or the renderer's runtime timing. Emitted graphics contracts and real core
stress tests cover those separate boundaries.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
from audit_map_slots import read_symbols
from probe_emulator import require

REPO = Path(__file__).resolve().parents[2]


def map_inventory(repo=REPO):
    """Read the current native map order, without a frozen map-count contract."""
    group = number = 0
    constants = []
    for line in (repo / 'constants/map_constants.asm').read_text().splitlines():
        if re.match(r'\s*newgroup\s*(;.*)?$', line):
            group += 1
            number = 0
        match = re.match(r'\s*map_const (\w+),\s*(\d+),\s*(\d+)', line)
        if match:
            number += 1
            constants.append((match[1], group, number, int(match[2]), int(match[3])))
    names = re.findall(r'^\s*map (\w+),', (repo / 'data/maps/maps.asm').read_text(), re.M)
    attributes = re.findall(r'^\s*map_attributes (\w+),',
                            (repo / 'data/maps/attributes.asm').read_text(), re.M)
    require(names and len(names) == len(constants) == len(attributes) and
            len(names) == len(set(names)) and set(names) == set(attributes), 'Invalid map inventory')
    rows = []
    for name, (constant, group, number, width, height) in zip(names, constants):
        require(name.upper().replace('_', '') == constant.replace('_', '') and group > 0,
                'Changed map constant order: ' + name)
        rows.append({'map': name, 'group': group, 'number': number, 'width': width, 'height': height})
    return rows


def objects(data, symbols, name):
    cursor = symbols[name + '_MapScriptHeader']
    for size in (2, 3, 5, 5, 5):
        require(cursor < len(data), 'Truncated map event header: ' + name)
        cursor += 1 + data[cursor] * size
    require(cursor < len(data), 'Missing map object count: ' + name)
    count = data[cursor]
    require(count <= 20, 'Changed native map record capacity: ' + name)
    records = [data[cursor + 1 + i * 13:cursor + 1 + (i + 1) * 13] for i in range(count)]
    require(all(len(row) == 13 for row in records), 'Truncated object record: ' + name)
    return cursor // 0x4000, records


def labels(symbols):
    result = {}
    for name, address in symbols.items():
        result.setdefault(address, set()).add(name)
    return result


def audit(baseline, mobile, repo=REPO):
    before, after = baseline.read_bytes(), mobile.read_bytes()
    old, new = (read_symbols(path.with_suffix('.sym')) for path in (baseline, mobile))
    require('MobileVirtualActorVersion' in new, 'Expected independent peer renderer')
    require('MobileNativeLeaseSlot' not in new, 'NPC leasing returned')
    inventory = map_inventory(repo)
    old_labels, new_labels = labels(old), labels(new)
    rows = []
    for entry in inventory:
        name = entry['map']
        headers = [data[symbols['MapGroup' + str(entry['group'])] + (entry['number'] - 1) * 7:
                        symbols['MapGroup' + str(entry['group'])] + entry['number'] * 7]
                   for data, symbols in ((before, old), (after, new))]
        require(all(len(header) == 7 for header in headers) and
                headers[0][:2] == headers[1][:2] and headers[0][4:] == headers[1][4:],
                'Native tileset, environment, location, music or palette changed: ' + name)
        old_bank, native = objects(before, old, name)
        new_bank, current = objects(after, new, name)
        require(len(native) == len(current), 'Native object count changed: ' + name)
        for index, (left, right) in enumerate(zip(native, current), 1):
            require(left[:9] == right[:9] and left[11:] == right[11:],
                    f'Native actor data changed: {name}:{index}')
            pointers = [int.from_bytes(row[9:11], 'little') for row in (left, right)]
            if pointers[0] != pointers[1]:
                addresses = [bank * 0x4000 + pointer - 0x4000 if pointer >= 0x4000 else pointer
                             for bank, pointer in zip((old_bank, new_bank), pointers)]
                require(bool(old_labels.get(addresses[0], set()) &
                             new_labels.get(addresses[1], set())),
                        f'Native actor script target changed: {name}:{index}')
        old_attribute, new_attribute = old[name + '_MapAttributes'], new[name + '_MapAttributes']
        require(before[old_attribute:old_attribute + 3] == after[new_attribute:new_attribute + 3],
                'Native border block or dimensions changed: ' + name)
        rows.append({'map': name, 'native_object_records': len(native),
                     'required_remote_object_slots': 0, 'npc_permissions_required': False})
    movement_size = len(re.findall(r'^\s*db\s+',
                         (repo / 'data/sprites/map_objects.asm').read_text(), re.M))
    require(movement_size and movement_size % 6 == 0, 'Changed movement table source format')
    movement = before[old['SpriteMovementData']:old['SpriteMovementData'] + movement_size]
    require(movement == after[new['SpriteMovementData']:new['SpriteMovementData'] + len(movement)],
            'Native movement table changed')
    return {'passed': True, 'map_count': len(rows), 'maps': rows,
            'baseline_sha256': hashlib.sha256(before).hexdigest(),
            'mobile_sha256': hashlib.sha256(after).hexdigest(),
            'scope': 'Compiled native actors, script targets, tilesets, dimensions and movement data.',
            'limitations': ['No exhaustive playthrough or frame-timing proof is inferred from this audit.',
                            'Hardware capacity is handled by whole-figure presentation admission.',
                            'Future native graphics formats must pass the emitted assembly contracts.']}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--baseline', type=Path, required=True)
    parser.add_argument('--mobile', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    require(not args.output.exists(), 'Choose a fresh audit report')
    report = audit(args.baseline, args.mobile)
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    print(f"Independent map audit passed: {report['map_count']} maps, zero native peer slots")


if __name__ == '__main__':
    main()
