#!/usr/bin/env python3
"""Compare four independent figures against equal ordinary NPC figure load.

Append ordinary avatar events without removing native NPCs. The bedroom and
measured Game Corner viewport have room in the runtime pool. Each disposable
control has the three chosen avatars at measured final positions and replays
ordinary local movement. Verify that every original active actor remains present.
"""
import argparse
import json
from pathlib import Path
import subprocess
from prepare_independent_controls import BUTTONS, FIGURES, inventory
from probe_emulator import BASELINE_SHA256, digest, read_symbols, require

REPO = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('reference', 'output', 'runner', 'core', 'profile-core', 'rgbds'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--workload', choices=('cadence', 'start-menu'), default='cadence')
    parser.add_argument('--scene', choices=('bedroom', 'game-corner'), default='bedroom')
    args = parser.parse_args()
    require(not args.output.exists(), 'Choose a fresh directory')
    symbols = read_symbols(args.reference / 'test.sym')
    require('MobileVirtualActorVersion' in symbols, 'Expected independent remote actors')
    states = [json.loads((args.reference / str(i) / 'state.json').read_text())['symbols'] for i in range(4)]
    map_id = [24, 5] if args.scene == 'bedroom' else [11, 23]
    require(all([state['wMapGroup'], state['wMapNumber']] == map_id for state in states),
            'Equal-figure control requires its measured native scene')
    peer_indices = (5, 6, 7) if args.scene == 'bedroom' else (13, 14, 15)
    map_name = 'PlayersHouse2F' if args.scene == 'bedroom' else 'GoldenrodGameCorner'
    spawn = ('3', '3') if args.scene == 'bedroom' else ('14', '5')
    args.output.mkdir(parents=True)
    reports = []
    for i, figure in enumerate(FIGURES):
        source = args.reference / str(i)
        positions = [item for j, name in enumerate(FIGURES) if j != i for item in
                     ('--peer-position', f"{name}={states[j]['wXCoord']},{states[j]['wYCoord']}")]
        build = args.output / (figure + '-build')
        subprocess.run(['python3', str(REPO / 'mobile_adapter/tools/build_native_controls.py'),
            '--rgbds', str(args.rgbds), '--scene', args.scene, '--avatar', figure,
            '--output', str(build)] + positions +
            (['--keep-native-objects'] if args.scene == 'game-corner' else []), cwd=REPO, check=True)
        scene = args.output / (figure + '-save')
        subprocess.run(['python3', str(REPO / 'mobile_adapter/tools/create_scene_save.py'),
            '--runner', str(args.runner), '--core', str(args.core), '--rom', str(build / 'control.gbc'),
            '--symbols', str(build / 'control.sym'), '--map', map_name, '--x', spawn[0], '--y', spawn[1],
            '--avatar', figure, '--output', str(scene)], cwd=REPO, check=True)
        directory = args.output / str(i)
        directory.mkdir()
        events = [json.loads(line) for line in (source / 'input-events.jsonl').read_text().splitlines()]
        moves = [e for e in events if e['frame'] >= 3300 and e['button'] in BUTTONS]
        require(len(moves) == 1, 'Expected one ordinary movement input')
        script = (REPO / 'mobile_adapter/tools/fixtures/continue.input').read_text() + \
            '\n' + ''.join(f"{e['frame']} {BUTTONS[e['button']]} 12\n" for e in moves)
        if args.workload == 'start-menu' and i == 0:
            require(any(e['frame'] == 3750 and e['button'] == 3 for e in events) and
                    any(e['frame'] == 4250 and e['button'] == 0 for e in events),
                    'Start-menu reference lacks open/close inputs')
            script += '3750 START 8\n4250 B 8\n'
        (directory / 'input.txt').write_text(script)
        first = 3600 if args.workload == 'start-menu' else 3300
        mobile = json.loads((source / 'profile.json').read_text())
        command = [str(args.runner), str(args.profile_core), str(build / 'control.gbc'),
            str(directory / 'screen.ppm'), str(directory), '--frames', str(mobile['video_frames'] + 1),
            '--input', str(directory / 'input.txt'), '--symbols', str(build / 'control.sym'),
            '--adapter', '0', '--save-in', str(scene / 'ingame.sav'), '--state-out', str(directory / 'state.json'),
            '--ram-out', str(directory / 'physical-wram.bin'), '--profile-out', str(directory / 'profile.json'),
            '--profile-window-start', str(first)]
        with (directory / 'process.log').open('w') as log:
            subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=120)
        native_symbols = read_symbols(build / 'control.sym')
        ram = (directory / 'physical-wram.bin').read_bytes()
        bank, address = native_symbols['wObjectStructs']
        start = bank * 0x1000 + address - 0xd000
        objects = [ram[start + j * 34:start + (j + 1) * 34] for j in range(13)]
        peers = sorted([obj[0], obj[16], obj[17]] for obj in objects if obj[0] and obj[1] in peer_indices)
        expected = sorted([state['wPlayerSprite'], state['wXCoord'] + 4, state['wYCoord'] + 4]
                          for j, state in enumerate(states) if j != i)
        require(peers == expected, f'{figure}: native peer figures differ')
        original = inventory((source / 'physical-wram.bin').read_bytes(), symbols)
        ordinary = sorted([obj[0], obj[1]] for obj in objects if obj[0] and obj[1] not in peer_indices)
        require(ordinary == original, f'{figure}: original native figures changed')
        native_state = json.loads((directory / 'state.json').read_text())['symbols']
        require(not native_state['hCrashCode'] and native_state['wPlayerGender'] == states[i]['wPlayerGender'] and
                [native_state['wXCoord'], native_state['wYCoord']] == [states[i]['wXCoord'], states[i]['wYCoord']],
                f'{figure}: native movement differs')
        native = json.loads((directory / 'profile.json').read_text())
        comparison = {key: {'mobile': mobile[key], 'native': native[key]} for key in
                      ('world_frames_with_update', 'world_max_frame_gap', 'world_duplicate_updates')}
        (directory / 'cadence-comparison.json').write_text(json.dumps(comparison, indent=2) + '\n')
        require(mobile['world_window_start'] == native['world_window_start'] == first and
                mobile['world_frames_with_update'] >= native['world_frames_with_update'] and
                mobile['world_max_frame_gap'] <= native['world_max_frame_gap'] and
                mobile['world_duplicate_updates'] <= native['world_duplicate_updates'],
                f'{figure}: extra world-frame delay compared with ordinary NPC avatars')
        if args.workload == 'cadence':
            require(native['world_frames_with_update'] == 800 and native['world_max_frame_gap'] == 1 and
                    native['world_duplicate_updates'] == 0, 'Ordinary steady cadence control failed')
        reports.append({'figure': figure, 'native_original_inventory': original,
            'peer_figures': peers, 'total_figures': len(original) + 3,
            'movement_events': [{'frame': e['frame'], 'button': e['button']} for e in moves],
            'reference_rom_sha256': digest(args.reference / 'test.gbc'),
            'control_rom_sha256': digest(build / 'control.gbc'),
            'control_battery_sha256': digest(scene / 'ingame.sav'),
            'cadence_comparison': comparison,
            'mobile_active_cycles': mobile['active_cycles_in_800_frames'],
            'native_active_cycles': native['active_cycles_in_800_frames']})
    require(digest(REPO / 'polishedcrystal-3.2.3.gbc') == BASELINE_SHA256, 'Baseline was not restored')
    (args.output / 'controls.json').write_text(json.dumps({'passed': True, 'scene': args.scene,
        'workload': args.workload, 'controls': reports,
        'limitations': ['Ordinary control peers stand at the settled endpoint positions.',
                        'Cold joins, NPC random history and palette-slot allocation can differ.',
                        'This viewport permits three additional ordinary objects; the full-pool stress case is separate.']}, indent=2) + '\n')
    print('Equal ordinary-avatar load and native frame cadence passed', flush=True)


if __name__ == '__main__':
    main()
