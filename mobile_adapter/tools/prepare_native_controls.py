#!/usr/bin/env python3
"""Build per-figure native NPC controls and replay measured movement inputs.

Run after the reference ROM cadence scenario. Networking chords have no native
NPC equivalent and are omitted; actual movement start frames/durations are kept.
Peers use ordinary standing NPCs at the reference's final positions. This is an
actor-load counterfactual, not identical cold-join animation or NPC random history.
Builds temporarily change fixture/map source; do not run another build concurrently.
Save tests must snapshot their disabled inputs before this tool starts.
"""
import argparse
from core_runtime import CORE_SOURCE, OBSERVED_CORE, PROFILE_CORE
import json
from pathlib import Path
import subprocess
from probe_emulator import BASELINE_SHA256, digest, read_symbols, require

REPO = Path(__file__).resolve().parents[2]
FIGURES = ('chris', 'kris', 'crys', 'beta')
BUTTONS = {4: 'UP', 5: 'DOWN', 6: 'LEFT', 7: 'RIGHT'}


def compare_reference(reference, controls):
    """Reject stale controls and compare native world cadence for this run."""
    manifest = json.loads((controls / 'controls.json').read_text())
    require(manifest['passed'], 'Native controls did not pass actor validation')
    comparisons = []
    states = [json.loads((reference / str(i) / 'state.json').read_text())['symbols'] for i in range(4)]
    for i, report in enumerate(manifest['controls']):
        directory = reference / str(i)
        state = states[i]
        peers = [[peer['wPlayerSprite'], peer['wXCoord'] + 4, peer['wYCoord'] + 4]
                 for j, peer in enumerate(states) if j != i]
        events = [json.loads(line) for line in (directory / 'input-events.jsonl').read_text().splitlines()]
        movements = [{'frame': e['frame'], 'button': e['button']} for e in events
                     if e['frame'] >= 3300 and e['button'] in BUTTONS]
        require(report['reference_rom_sha256'] == digest(reference / 'test.gbc') and
                report['figure'] == FIGURES[i] and
                report['peer_positions'] == peers and
                report['movement_events'] == movements and
                report['local_position'] == [state['wXCoord'], state['wYCoord']] and
                report['actor_count'] == state['vNPCOccupiedMask'].bit_count(),
                f'Client {i}: controls do not match the measured reference')
        mobile = json.loads((directory / 'profile.json').read_text())
        native = json.loads((controls / str(i) / 'profile.json').read_text())
        require(mobile['world_window_start'] == native['world_window_start'] == 3300,
                'Cadence windows differ')
        require(mobile['world_frames_with_update'] >= native['world_frames_with_update'] and
                mobile['world_max_frame_gap'] <= native['world_max_frame_gap'] and
                mobile['world_duplicate_updates'] <= native['world_duplicate_updates'],
                f'Client {i}: extra native world-update gaps with multiplayer')
        comparisons.append({'figure': report['figure'], 'speed': report['speed'],
            'actor_count': report['actor_count'],
            'mobile_update_frames': mobile['world_frames_with_update'],
            'native_update_frames': native['world_frames_with_update'],
            'mobile_max_gap': mobile['world_max_frame_gap'],
            'native_max_gap': native['world_max_frame_gap'],
            'mobile_duplicates': mobile['world_duplicate_updates'],
            'native_duplicates': native['world_duplicate_updates']})
    require(len(comparisons) == 4, 'Four validated controls are required')
    return comparisons


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cache', type=Path, default=Path('/Volumes/Storage/Cache/mobileadapter'))
    parser.add_argument('--reference', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument("--core", type=Path, default=OBSERVED_CORE)
    parser.add_argument("--profile-core", type=Path, default=PROFILE_CORE)
    parser.add_argument("--runner", type=Path)
    args = parser.parse_args()
    require(not args.output.exists(), 'Choose a fresh control directory')
    args.output.mkdir(parents=True)
    if args.runner is None:
        args.runner = args.output / "smoke-libretro"
        subprocess.run(["clang++", "-std=c++17", "-O2", "-I",
                        str(CORE_SOURCE / "libretro/DoubleCherryEngine"),
                        str(REPO / "mobile_adapter/tools/smoke_libretro.cpp"), "-lz", "-o", str(args.runner)], check=True)
    states = [json.loads((args.reference / str(i) / 'state.json').read_text())['symbols'] for i in range(4)]
    reports = []
    reference_symbols = read_symbols(args.reference / 'test.sym')
    for i, name in enumerate(FIGURES):
        build = args.output / (name + '-build')
        physical = (args.reference / str(i) / 'physical-wram.bin').read_bytes()
        bank, address = reference_symbols['wMobileNativeOriginals']
        require(bank == 0, 'Original actor views must remain unbanked')
        originals = physical[address - 0xc000:address - 0xc000 + 3 * 34]
        replacements = sorted({originals[j * 34 + 1] for j in range(3) if originals[j * 34]})
        require(len(replacements) == 2 and all(1 <= index <= 12 for index in replacements),
                'Crowded reference must contain two actual reclaimed map objects')
        lease_arguments = [item for index in replacements for item in ('--replace-object', str(index))]
        positions = [item for j, figure in enumerate(FIGURES) if j != i for item in
                     ('--peer-position', f"{figure}={states[j]['wXCoord']},{states[j]['wYCoord']}")]
        subprocess.run(['python3', str(REPO / 'mobile_adapter/tools/build_native_controls.py'),
                        '--rgbds', str(args.cache / 'rgbds-1.0.4'), '--scene', 'game-corner',
                        '--avatar', name, '--output', str(build)] + positions + lease_arguments, check=True, cwd=REPO)
        # Continue restores cached map/object records from SRAM. A battery from
        # the original map would silently bypass these ordinary control actors.
        # Generate the control's own real New Game/Save rather than editing SRAM.
        scene = args.output / (name + '-save')
        subprocess.run(['python3', str(REPO / 'mobile_adapter/tools/create_scene_save.py'),
                        '--runner', str(args.runner),
                        '--core', str(args.core),
                        '--rom', str(build / 'control.gbc'), '--symbols', str(build / 'control.sym'),
                        '--map', 'GoldenrodGameCorner', '--x', '14', '--y', '5', '--hour', '10',
                        '--avatar', name, '--output', str(scene)], check=True, cwd=REPO)
        directory = args.output / str(i)
        directory.mkdir()
        events = [json.loads(line) for line in (args.reference / str(i) / 'input-events.jsonl').read_text().splitlines()]
        moves = [event for event in events if event['frame'] >= 3300 and event['button'] in BUTTONS]
        require(len(moves) == 1, 'Expected one stationary-cadence movement per player')
        script = (REPO / 'mobile_adapter/tools/fixtures/continue.input').read_text()
        script += '\n' + ''.join(f"{event['frame']} {BUTTONS[event['button']]} 12\n" for event in moves)
        (directory / 'input.txt').write_text(script)
        frames = 3250 + (5500 - 3250) * (i + 1)
        command = [str(args.runner),
                   str(args.profile_core),
                   str(build / 'control.gbc'), str(directory / 'screen.ppm'), str(directory),
                   '--frames', str(frames), '--input', str(directory / 'input.txt'),
                   '--symbols', str(build / 'control.sym'), '--adapter', '0',
                   '--save-in', str(scene / 'ingame.sav'),
                   '--state-out', str(directory / 'state.json'), '--ram-out', str(directory / 'physical-wram.bin'),
                   '--profile-out', str(directory / 'profile.json'), '--profile-window-start', '3300']
        with (directory / 'process.log').open('w') as log:
            subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=120)
        symbols = read_symbols(build / 'control.sym')
        bank, address = symbols['wObjectStructs']
        offset = bank * 0x1000 + address - 0xd000
        ram = (directory / 'physical-wram.bin').read_bytes()
        objects = [ram[offset + j * 34:offset + (j + 1) * 34] for j in range(13)]
        peers = {(obj[0], obj[16], obj[17]) for obj in objects[1:] if obj[1] in (*replacements, 13)}
        expected = {(state['wPlayerSprite'], state['wXCoord'] + 4, state['wYCoord'] + 4)
                    for j, state in enumerate(states) if j != i}
        require(peers == expected, f'{name}: ordinary peer figures/positions differ: {peers} != {expected}')
        occupied = sum(bool(obj[0]) for obj in objects)
        bank, address = reference_symbols['wObjectStructs']
        offset = bank * 0x1000 + address - 0xd000
        reference_objects = [physical[offset + j * 34:offset + (j + 1) * 34] for j in range(13)]
        native_inventory = sorted((obj[0], obj[1]) for obj in objects[1:]
                                  if obj[0] and obj[1] not in (*replacements, 13))
        reference_inventory = sorted((obj[0], obj[1]) for obj in reference_objects[1:]
                                     if obj[0] and obj[1] != 254)
        require(native_inventory == reference_inventory,
                f'{name}: remaining ordinary actor identities differ')
        reference_occupied = states[i]['vNPCOccupiedMask'].bit_count()
        require(occupied == reference_occupied, f'{name}: actor count differs: {occupied} != {reference_occupied}')
        native = json.loads((directory / 'state.json').read_text())['symbols']
        require(native['wPlayerGender'] == states[i]['wPlayerGender'] and
                (native['wXCoord'], native['wYCoord']) == (states[i]['wXCoord'], states[i]['wYCoord']),
                f'{name}: movement replay did not match local final position')
        reports.append({'figure': name, 'speed': i + 1, 'actor_count': occupied,
                        'replaced_map_objects': replacements,
                        'native_actor_inventory': native_inventory,
                        'movement_input_sha256': digest(directory / 'input.txt'),
                        'movement_events': [{'frame': e['frame'], 'button': e['button']} for e in moves],
                        'control_rom_sha256': digest(build / 'control.gbc'),
                        'control_battery_sha256': digest(scene / 'ingame.sav'),
                        'local_position': [native['wXCoord'], native['wYCoord']],
                        'peer_positions': [[peer['wPlayerSprite'], peer['wXCoord'] + 4, peer['wYCoord'] + 4]
                                           for j, peer in enumerate(states) if j != i],
                        'reference_rom_sha256': digest(args.reference / 'test.gbc')})
    require(digest(REPO / 'polishedcrystal-3.2.3.gbc') == BASELINE_SHA256, 'Native baseline was not restored')
    (args.output / 'controls.json').write_text(json.dumps({'passed': True, 'controls': reports,
        'limitations': ['Standing ordinary peers model final actor geometry, not cold-join animation.',
                        'Networking chords are omitted; measured movement frames/durations are replayed.',
                        'NPC random history and palette-slot allocation can differ.']}, indent=2) + '\n')
    comparison = compare_reference(args.reference, args.output)
    (args.output / 'cadence-comparison.json').write_text(json.dumps(
        {'passed': True, 'comparisons': comparison}, indent=2) + '\n')
    print('Per-figure native controls, movement replay and cadence comparison passed', flush=True)


if __name__ == '__main__':
    main()
