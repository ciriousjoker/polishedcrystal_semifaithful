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
        first = 3300 if manifest.get('workload', 'cadence') in ('cadence', 'stationary') else 3600
        require(mobile['world_window_start'] == native['world_window_start'] == first,
                'Cadence windows differ')
        parity = (mobile['world_frames_with_update'] >= native['world_frames_with_update'] and
                  mobile['world_max_frame_gap'] <= native['world_max_frame_gap'] and
                  mobile['world_duplicate_updates'] <= native['world_duplicate_updates'])
        # Menu reload also depends on sprite fetch/transfer geometry. The full
        # native pool cannot hold these three extra ordinary actors, so this is
        # an unequal-load diagnostic. The separate equal-avatar menu control is
        # the mandatory parity gate; keep this result rather than hiding its gap.
        if manifest.get('workload') != 'start-menu':
            require(parity, f'Client {i}: native frame cadence changed')
        if manifest.get('workload', 'cadence') == 'cadence':
            require(native['world_frames_with_update'] == 800 and native['world_max_frame_gap'] == 1 and
                    native['world_duplicate_updates'] == 0, 'Ordinary steady cadence control failed')
        comparisons.append({'figure': FIGURES[i], 'speed': report['speed'],
            'equivalent_figure_load': False, 'cadence_parity': parity,
            'native_actor_count': len(native_inventory), 'additional_private_peers': 3,
            'mobile_update_frames': mobile['world_frames_with_update'],
            'native_update_frames': native['world_frames_with_update'],
            'mobile_active_cycles': mobile['active_cycles_in_800_frames'],
            'native_active_cycles': native['active_cycles_in_800_frames'],
            'mobile_maximum_frame_gap': mobile['world_max_frame_gap'],
            'native_maximum_frame_gap': native['world_max_frame_gap'],
            'mobile_duplicate_updates': mobile['world_duplicate_updates'],
            'native_duplicate_updates': native['world_duplicate_updates']})
    return comparisons


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('reference', 'output', 'runner', 'profile-core'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--battery', type=Path, action='append', required=True)
    parser.add_argument('--workload', choices=('cadence', 'stationary', 'running', 'start-menu'), default='cadence')
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
        require(len(moves) == (0 if args.workload == 'stationary' else
                              3 if args.workload == 'running' and i == 0 else 1),
                'Unexpected ordinary movement workload')
        script = (REPO / 'mobile_adapter/tools/fixtures/continue.input').read_text()
        script += '\n' + ''.join(f"{event['frame']} {BUTTONS[event['button']]} " +
            ('24\n' if args.workload == 'running' and event['frame'] >= 3750 else '12\n') for event in moves)
        if args.workload == 'running' and i == 0:
            require(any(e['frame'] == 3750 and e['button'] == 0 for e in events),
                    'Running reference lacks the ordinary B input')
            script += '3750 B 24\n'
        if args.workload == 'start-menu' and i == 0:
            require(any(e['frame'] == 3750 and e['button'] == 3 for e in events) and
                    any(e['frame'] == 4250 and e['button'] == 0 for e in events),
                    'Start-menu reference lacks the ordinary open/close inputs')
            script += '3750 START 8\n4250 B 8\n'
        (directory / 'input.txt').write_text(script)
        frames = json.loads((source / 'profile.json').read_text())['video_frames'] + 1
        command = [str(args.runner), str(args.profile_core), str(rom),
            str(directory / 'screen.ppm'), str(directory), '--frames', str(frames),
            '--input', str(directory / 'input.txt'), '--symbols', str(rom.with_suffix('.sym')),
            '--adapter', '0', '--save-in', str(args.battery[i]), '--state-out', str(directory / 'state.json'),
            '--ram-out', str(directory / 'physical-wram.bin'), '--profile-out', str(directory / 'profile.json'),
            '--trace-out', str(directory / 'trace.jsonl.gz'),
            '--profile-window-start', '3300' if args.workload in ('cadence', 'stationary') else '3600']
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
        'kind': 'unchanged-native-inventory', 'workload': args.workload, 'controls': reports,
        'limitations': ['Additional private guests exceed the full ordinary pool and are absent from this control.',
                        'This comparison preserves native NPC workload; equal-avatar load requires an unsaturated control.',
                        'NPC random history can differ.']}, indent=2) + '\n')
    comparison = compare(args.reference, args.output)
    diagnostic = args.workload == 'start-menu'
    (args.output / 'cadence-comparison.json').write_text(json.dumps({
        'passed': None if diagnostic else True, 'diagnostic_only': diagnostic,
        'required_equal_load_gate': 'menu-avatar' if diagnostic else None,
        'comparisons': comparison}, indent=2) + '\n')
    print('Unchanged native inventory passed; unequal-load menu timing recorded' if diagnostic else
          'Unchanged native inventory and exact frame cadence passed', flush=True)


if __name__ == '__main__':
    main()
