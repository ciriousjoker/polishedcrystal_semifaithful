#!/usr/bin/env python3
"""Replay the native NPC workload in the exact disabled ROM.

A full thirteen-object map cannot accommodate three additional ordinary NPCs.
This control preserves every native actor, uses the same ordinary batteries and
replays the measured local movement. It proves frame cadence for that workload;
an unsaturated ordinary-avatar control separately measures equal figure load.
No map changes, SRAM edits, snapshots or RAM injection are used here.
"""
import argparse
import json
from pathlib import Path
import subprocess
from probe_emulator import BASELINE_SHA256, digest, read_symbols, require

REPO = Path(__file__).resolve().parents[2]
FIGURES = ('chris', 'kris', 'crys', 'beta')
BUTTONS = {4: 'UP', 5: 'DOWN', 6: 'LEFT', 7: 'RIGHT'}


def inventory(ram, symbols):
    bank, address = symbols['wObjectStructs']
    start = bank * 0x1000 + address - 0xd000
    records = [ram[start + i * 34:start + (i + 1) * 34] for i in range(13)]
    return sorted([obj[0], obj[1]] for obj in records if obj[0])


def compare(reference, controls):
    manifest = json.loads((controls / 'controls.json').read_text())
    evidence = json.loads((reference / 'evidence.json').read_text())
    require(manifest['passed'] and manifest['kind'] == 'unchanged-native-inventory',
            'Unchanged native controls did not pass')
    symbols = read_symbols(reference / 'test.sym')
    comparisons = []
    require(len(manifest['controls']) == 4, 'Four native controls are required')
    for i, report in enumerate(manifest['controls']):
        directory = reference / str(i)
        state = json.loads((directory / 'state.json').read_text())['symbols']
        events = [json.loads(line) for line in (directory / 'input-events.jsonl').read_text().splitlines()]
        movements = [{'frame': e['frame'], 'button': e['button']} for e in events
                     if e['frame'] >= 3300 and e['button'] in BUTTONS]
        native_inventory = inventory((directory / 'physical-wram.bin').read_bytes(), symbols)
        require(report['reference_rom_sha256'] == digest(reference / 'test.gbc') and
                report['control_rom_sha256'] == BASELINE_SHA256 and
                report['battery_sha256'] == evidence['battery_sha256'][i] and
                report['speed'] == evidence['emulator_speeds'][i] and
                report['movement_events'] == movements and
                report['native_actor_inventory'] == native_inventory and
                report['local_position'] == [state['wXCoord'], state['wYCoord']],
                f'Client {i}: native controls do not match the reference')
        mobile = json.loads((directory / 'profile.json').read_text())
        native = json.loads((controls / str(i) / 'profile.json').read_text())
        require(mobile['world_window_start'] == native['world_window_start'] == 3300,
                'Cadence windows differ')
        require(mobile['world_frames_with_update'] == native['world_frames_with_update'] == 800 and
                mobile['world_max_frame_gap'] == native['world_max_frame_gap'] == 1 and
                mobile['world_duplicate_updates'] == native['world_duplicate_updates'] == 0,
                f'Client {i}: native frame cadence changed')
        comparisons.append({'figure': FIGURES[i], 'speed': report['speed'],
            'native_actor_count': len(native_inventory), 'additional_private_peers': 3,
            'mobile_update_frames': mobile['world_frames_with_update'],
            'native_update_frames': native['world_frames_with_update'],
            'mobile_active_cycles': mobile['active_cycles_in_800_frames'],
            'native_active_cycles': native['active_cycles_in_800_frames'],
            'maximum_frame_gap': 1, 'duplicate_updates': 0})
    return comparisons


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('reference', 'output', 'runner', 'profile-core'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--battery', type=Path, action='append', required=True)
    args = parser.parse_args()
    require(not args.output.exists(), 'Choose a fresh directory')
    require(len(args.battery) == 4, 'Four ordinary batteries are required')
    rom = REPO / 'polishedcrystal-3.2.3.gbc'
    symbols = read_symbols(rom.with_suffix('.sym'))
    reference_symbols = read_symbols(args.reference / 'test.sym')
    evidence = json.loads((args.reference / 'evidence.json').read_text())
    require(digest(rom) == BASELINE_SHA256, 'Control must be the exact disabled ROM')
    require('MobileVirtualActorVersion' in reference_symbols, 'Expected independent actors')
    require([digest(path) for path in args.battery] == evidence['battery_sha256'],
            'Control batteries differ from the reference')
    args.output.mkdir(parents=True)
    reports = []
    for i, figure in enumerate(FIGURES):
        source = args.reference / str(i)
        directory = args.output / str(i)
        directory.mkdir()
        events = [json.loads(line) for line in (source / 'input-events.jsonl').read_text().splitlines()]
        moves = [event for event in events if event['frame'] >= 3300 and event['button'] in BUTTONS]
        require(len(moves) == 1, 'Expected one cadence movement per player')
        script = (REPO / 'mobile_adapter/tools/fixtures/continue.input').read_text()
        script += '\n' + ''.join(f"{event['frame']} {BUTTONS[event['button']]} 12\n" for event in moves)
        (directory / 'input.txt').write_text(script)
        frames = json.loads((source / 'profile.json').read_text())['video_frames'] + 1
        command = [str(args.runner), str(args.profile_core), str(rom),
            str(directory / 'screen.ppm'), str(directory), '--frames', str(frames),
            '--input', str(directory / 'input.txt'), '--symbols', str(rom.with_suffix('.sym')),
            '--adapter', '0', '--save-in', str(args.battery[i]), '--state-out', str(directory / 'state.json'),
            '--ram-out', str(directory / 'physical-wram.bin'), '--profile-out', str(directory / 'profile.json'),
            '--profile-window-start', '3300']
        with (directory / 'process.log').open('w') as log:
            subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=120)
        state = json.loads((directory / 'state.json').read_text())['symbols']
        reference = json.loads((source / 'state.json').read_text())['symbols']
        native_inventory = inventory((directory / 'physical-wram.bin').read_bytes(), symbols)
        require(native_inventory == inventory((source / 'physical-wram.bin').read_bytes(), reference_symbols),
                f'{figure}: native actor identities changed')
        require(not state['hCrashCode'] and state['wPlayerGender'] == reference['wPlayerGender'] and
                [state['wXCoord'], state['wYCoord']] == [reference['wXCoord'], reference['wYCoord']],
                f'{figure}: ordinary movement replay differs')
        reports.append({'figure': figure, 'speed': evidence['emulator_speeds'][i],
            'native_actor_inventory': native_inventory,
            'movement_events': [{'frame': e['frame'], 'button': e['button']} for e in moves],
            'local_position': [state['wXCoord'], state['wYCoord']],
            'control_rom_sha256': digest(rom), 'battery_sha256': digest(args.battery[i]),
            'reference_rom_sha256': digest(args.reference / 'test.gbc')})
    (args.output / 'controls.json').write_text(json.dumps({'passed': True,
        'kind': 'unchanged-native-inventory', 'controls': reports,
        'limitations': ['Additional private guests exceed the full ordinary pool and are absent from this control.',
                        'This comparison preserves native NPC workload; equal-avatar load requires an unsaturated control.',
                        'NPC random history can differ.']}, indent=2) + '\n')
    comparison = compare(args.reference, args.output)
    (args.output / 'cadence-comparison.json').write_text(json.dumps({'passed': True,
        'comparisons': comparison}, indent=2) + '\n')
    print('Unchanged native inventory and exact frame cadence passed', flush=True)


if __name__ == '__main__':
    main()
