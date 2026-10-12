#!/usr/bin/env python3
"""Compare four independent figures against equal ordinary NPC figure load.

Use the unsaturated bedroom so the three ordinary control actors fit without
removing any native NPC. Each disposable control has the other three chosen
avatars at their measured final positions and replays ordinary local movement.
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
    args = parser.parse_args()
    require(not args.output.exists(), 'Choose a fresh directory')
    symbols = read_symbols(args.reference / 'test.sym')
    require('MobileVirtualActorVersion' in symbols, 'Expected independent remote actors')
    states = [json.loads((args.reference / str(i) / 'state.json').read_text())['symbols'] for i in range(4)]
    require(all([state['wMapGroup'], state['wMapNumber']] == [24, 5] for state in states),
            'Equal-figure control requires the unsaturated bedroom')
    args.output.mkdir(parents=True)
    reports = []
    for i, figure in enumerate(FIGURES):
        source = args.reference / str(i)
        positions = [item for j, name in enumerate(FIGURES) if j != i for item in
                     ('--peer-position', f"{name}={states[j]['wXCoord']},{states[j]['wYCoord']}")]
        build = args.output / (figure + '-build')
        subprocess.run(['python3', str(REPO / 'mobile_adapter/tools/build_native_controls.py'),
            '--rgbds', str(args.rgbds), '--scene', 'bedroom', '--avatar', figure,
            '--output', str(build)] + positions, cwd=REPO, check=True)
        scene = args.output / (figure + '-save')
        subprocess.run(['python3', str(REPO / 'mobile_adapter/tools/create_scene_save.py'),
            '--runner', str(args.runner), '--core', str(args.core), '--rom', str(build / 'control.gbc'),
            '--symbols', str(build / 'control.sym'), '--map', 'PlayersHouse2F', '--x', '3', '--y', '3',
            '--avatar', figure, '--output', str(scene)], cwd=REPO, check=True)
        directory = args.output / str(i)
        directory.mkdir()
        events = [json.loads(line) for line in (source / 'input-events.jsonl').read_text().splitlines()]
        moves = [e for e in events if e['frame'] >= 3300 and e['button'] in BUTTONS]
        require(len(moves) == 1, 'Expected one ordinary movement input')
        (directory / 'input.txt').write_text((REPO / 'mobile_adapter/tools/fixtures/continue.input').read_text() +
            '\n' + ''.join(f"{e['frame']} {BUTTONS[e['button']]} 12\n" for e in moves))
        mobile = json.loads((source / 'profile.json').read_text())
        command = [str(args.runner), str(args.profile_core), str(build / 'control.gbc'),
            str(directory / 'screen.ppm'), str(directory), '--frames', str(mobile['video_frames'] + 1),
            '--input', str(directory / 'input.txt'), '--symbols', str(build / 'control.sym'),
            '--adapter', '0', '--save-in', str(scene / 'ingame.sav'), '--state-out', str(directory / 'state.json'),
            '--ram-out', str(directory / 'physical-wram.bin'), '--profile-out', str(directory / 'profile.json'),
            '--profile-window-start', '3300']
        with (directory / 'process.log').open('w') as log:
            subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=120)
        native_symbols = read_symbols(build / 'control.sym')
        ram = (directory / 'physical-wram.bin').read_bytes()
        bank, address = native_symbols['wObjectStructs']
        start = bank * 0x1000 + address - 0xd000
        objects = [ram[start + j * 34:start + (j + 1) * 34] for j in range(13)]
        peers = sorted([obj[0], obj[16], obj[17]] for obj in objects if obj[0] and obj[1] in (5, 6, 7))
        expected = sorted([state['wPlayerSprite'], state['wXCoord'] + 4, state['wYCoord'] + 4]
                          for j, state in enumerate(states) if j != i)
        require(peers == expected, f'{figure}: native peer figures differ')
        original = inventory((source / 'physical-wram.bin').read_bytes(), symbols)
        ordinary = sorted([obj[0], obj[1]] for obj in objects if obj[0] and obj[1] not in (5, 6, 7))
        require(ordinary == original, f'{figure}: original native figures changed')
        native_state = json.loads((directory / 'state.json').read_text())['symbols']
        require(not native_state['hCrashCode'] and native_state['wPlayerGender'] == states[i]['wPlayerGender'] and
                [native_state['wXCoord'], native_state['wYCoord']] == [states[i]['wXCoord'], states[i]['wYCoord']],
                f'{figure}: native movement differs')
        native = json.loads((directory / 'profile.json').read_text())
        require(mobile['world_window_start'] == native['world_window_start'] == 3300 and
                mobile['world_frames_with_update'] == native['world_frames_with_update'] == 800 and
                mobile['world_max_frame_gap'] == native['world_max_frame_gap'] == 1 and
                mobile['world_duplicate_updates'] == native['world_duplicate_updates'] == 0,
                f'{figure}: extra world-frame delay compared with ordinary NPC avatars')
        reports.append({'figure': figure, 'native_original_inventory': original,
            'peer_figures': peers, 'total_figures': len(original) + 3,
            'movement_events': [{'frame': e['frame'], 'button': e['button']} for e in moves],
            'reference_rom_sha256': digest(args.reference / 'test.gbc'),
            'control_rom_sha256': digest(build / 'control.gbc'),
            'control_battery_sha256': digest(scene / 'ingame.sav'),
            'world_update_frames': 800, 'maximum_frame_gap': 1, 'duplicate_updates': 0,
            'mobile_active_cycles': mobile['active_cycles_in_800_frames'],
            'native_active_cycles': native['active_cycles_in_800_frames']})
    require(digest(REPO / 'polishedcrystal-3.2.3.gbc') == BASELINE_SHA256, 'Baseline was not restored')
    (args.output / 'controls.json').write_text(json.dumps({'passed': True, 'controls': reports,
        'limitations': ['Ordinary control peers stand at the settled endpoint positions.',
                        'Cold joins, NPC random history and palette-slot allocation can differ.',
                        'The full native pool is covered separately by unchanged-inventory controls.']}, indent=2) + '\n')
    print('Equal ordinary-avatar load and native frame cadence passed', flush=True)


if __name__ == '__main__':
    main()
