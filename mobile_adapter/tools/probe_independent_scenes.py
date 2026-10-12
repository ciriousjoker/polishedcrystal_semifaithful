#!/usr/bin/env python3
"""Exercise protected actors, irregular graphics, weather and night colors.

Fresh fixtures come from ordinary New Game and Save inputs in a disposable
spawn/clock producer. Optional existing fixtures must include that provenance
and valid native battery checksums. The tested ROM remains unchanged except
for the isolated test endpoint. No game RAM or saved bytes are injected.
"""
import argparse
import json
from pathlib import Path
import subprocess
from probe_emulator import digest, read_symbols, require, trace_rows, validate_battery
from audit_independent_maps import objects
from audit_map_slots import read_symbols as rom_symbols

REPO = Path(__file__).resolve().parents[2]
FIGURES = ('chris', 'kris', 'crys', 'beta')
SCENES = {
    'route37': ('Route37', 8, 12, 10),
    'exeggutor': ('ShamoutiIsland', 16, 10, 10),
    'night': ('Route37', 8, 12, 22),
    'snow': ('SnowtopMountainOutside', 17, 25, 22),
    'pool-full': ('PlayersHouse2F', 3, 3, 10),
}


def verify_scene(output, scene, rom, symbols):
    """Require the stress condition itself, not merely its map name."""
    reports = []
    for client in range(4):
        directory = output / 'probe' / str(client)
        ram = (directory / 'physical-wram.bin').read_bytes()

        def byte(name):
            bank, address = symbols[name]
            offset = address - 0xc000 if bank == 0 else bank * 0x1000 + address - 0xd000
            return ram[offset]

        online = [row['symbols'] for row in trace_rows(directory / 'trace.jsonl')
                  if row['symbols']['wMobileOnline']]
        metrics = json.loads((directory / 'profile.json').read_text())
        report = {'client': client, 'ordinary_time_of_day': byte('wTimeOfDay'),
                  'ordinary_weather': byte('wCurWeather'),
                  'world_update_frames': metrics['world_frames_with_update'],
                  'maximum_frame_gap': metrics['world_max_frame_gap'],
                  'duplicate_updates': metrics['world_duplicate_updates']}
        # Outdoor native updates can straddle the core's video callback even
        # without the addon. The unchanged native replay is the mandatory gate.
        if scene == 'pool-full':
            require(report['world_update_frames'] == 800 and report['maximum_frame_gap'] == 1 and
                    report['duplicate_updates'] == 0, 'Full-pool scene missed steady native world cadence')
        if scene == 'pool-full':
            report['full_native_pool_frames'] = sum(row['vNPCOccupiedMask'] == 8191 for row in online)
            require(report['full_native_pool_frames'] > 100,
                    'Full-pool fixture did not retain thirteen actual native objects alongside private peers')
            report['native_palette_usage_mask'] = byte('wUsedObjectPals')
            require(report['native_palette_usage_mask'] & 63 == 63,
                    'Full-pool fixture did not exhaust the six ordinary native object palettes')
        if scene == 'pool-full':
            native_figures = {}
            for row in online:
                for slot in range(1, 13):
                    prefix = f'vNativeSource{slot}'
                    if not row.get(prefix + 'Count'):
                        continue
                    key = (slot, row[prefix + 'Sprite'], row[prefix + 'Map'])
                    item = native_figures.setdefault(key, {'native_slot': slot,
                        'sprite': key[1], 'map_object': key[2],
                        'source_frames': 0, 'complete_hardware_frames': 0})
                    item['source_frames'] += 1
                    item['complete_hardware_frames'] += row[prefix + 'Complete']
            report['native_figure_presentations'] = list(native_figures.values())
            require({key[0] for key in native_figures} == set(range(1, 13)) and
                    all(item['complete_hardware_frames'] > 0
                    for item in native_figures.values() if item['source_frames'] > 100),
                    'A visible full-pool source was omitted from the census or never presented')
        if scene in ('night', 'snow'):
            require(report['ordinary_time_of_day'] == 2, 'Night fixture did not exercise native night colors')
        if scene == 'snow':
            require(report['ordinary_weather'] == 2 and
                    any(row['hUsedWeatherSpriteIndex'] < 160 for row in online),
                    'Snow fixture did not exercise native weather sprites')
        if scene == 'exeggutor':
            _, records = objects(rom.read_bytes(), rom_symbols(rom.with_suffix('.sym')), 'ShamoutiIsland')
            report['large_native_sprite'] = records[0][0]
            report['irregular_compositions'] = sum(row['wMobileVirtualFastAdmission'] != 2 and
                any(row[f'vNativeObject{slot}Map'] == 1 and
                    row[f'vNativeObject{slot}Sprite'] == records[0][0] for slot in range(1, 13))
                for row in online)
            report['large_complete_hardware_frames'] = sum(any(
                row.get(f'vNativeSource{slot}Map') == 1 and
                row.get(f'vNativeSource{slot}Sprite') == records[0][0] and
                row.get(f'vNativeSource{slot}Count', 0) > 4 and
                row.get(f'vNativeSource{slot}Complete') == 1 for slot in range(1, 13))
                for row in online)
            require(report['large_complete_hardware_frames'] > 100,
                    'Large native actor never received complete hardware presentation')
            require(report['irregular_compositions'] > 100,
                    'Exeggutor fixture did not exercise the large native actor and irregular renderer')
        reports.append(report)
    return reports


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('runner', 'core', 'server', 'rom', 'symbols', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--scene', choices=SCENES, required=True)
    parser.add_argument('--fixtures', type=Path, help='Existing four ordinary-save producer directories')
    parser.add_argument('--rgbds', type=Path, default=Path('/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4'))
    parser.add_argument('--port', type=int, default=17800)
    args = parser.parse_args()
    require(not args.output.exists(), 'Choose a fresh output directory')
    args.output.mkdir(parents=True)
    parent_rom_sha256 = digest(args.rom)
    if args.scene == 'pool-full':
        model = args.output / 'model-build'
        subprocess.run(['python3', str(REPO / 'mobile_adapter/tools/build_native_controls.py'),
            '--rgbds', str(args.rgbds), '--output', str(model), '--scene', 'bedroom',
            '--mobile', '--stress-count', '12'], cwd=REPO, check=True)
        args.rom, args.symbols = model / 'control.gbc', model / 'control.sym'
    map_name, x, y, hour = SCENES[args.scene]
    fixture_root = args.fixtures or args.output / 'fixtures'
    symbols = read_symbols(args.symbols)
    fixtures = []
    for figure in FIGURES:
        directory = fixture_root / figure
        if args.fixtures is None:
            subprocess.run(['python3', str(REPO / 'mobile_adapter/tools/create_scene_save.py'),
                '--runner', str(args.runner), '--core', str(args.core), '--rom', str(args.rom),
                '--symbols', str(args.symbols), '--map', map_name, '--x', str(x), '--y', str(y),
                '--hour', str(hour), '--avatar', figure, '--rgbds', str(args.rgbds),
                '--output', str(directory)], cwd=REPO, check=True)
        metadata = json.loads((directory / 'fixture.json').read_text())
        battery = directory / 'ingame.sav'
        require(metadata['map'] == map_name and metadata['x'] == x and metadata['y'] == y and
                metadata.get('hour', 10) == hour and metadata['avatar'] == figure and
                metadata['source'] == 'ordinary New Game, character-choice menu and in-game Save menu',
                f'{figure}: fixture does not describe the requested ordinary scene')
        if args.scene in ('night', 'snow'):
            require(metadata.get('rtc_source') == 'external cartridge clock offset produced by ordinary ROM RTC writes' and
                    digest(battery.with_suffix('.rtc')) == metadata.get('rtc_sha256'),
                    'Night/weather fixture is missing its ordinary external clock provenance')
        validate_battery(battery, symbols)
        require(digest(battery) == metadata['sha256'], f'{figure}: ordinary battery changed')
        fixtures.append(battery)
    command = ['python3', str(REPO / 'mobile_adapter/tools/probe_lobby.py'),
        '--runner', str(args.runner), '--core', str(args.core), '--server', str(args.server),
        '--rom', str(args.rom), '--symbols', str(args.symbols), '--players', '4',
        '--native-actor-probe', '--profile', '--profile-window-start', '3300',
        '--speeds', '1,2,3,4', '--observation-frames', '5500',
        '--battery', str(fixtures[0]), '--second-battery', str(fixtures[1]),
        '--different-battery', str(fixtures[2]), '--fourth-battery', str(fixtures[3]),
        '--port', str(args.port), '--output', str(args.output / 'probe')]
    if args.scene in ('night', 'snow'):
        command += ['--restore-fixture-rtc']
    if args.scene != 'pool-full':
        command += ['--scenario', 'scanline']
    subprocess.run(command, cwd=REPO, check=True)
    if args.scene != 'pool-full':
        subprocess.run(['python3', str(REPO / 'mobile_adapter/tools/prepare_independent_controls.py'),
            '--runner', str(args.runner), '--profile-core', str(args.core), '--workload', 'stationary',
            '--reference', str(args.output / 'probe'), '--output', str(args.output / 'native-controls')]
            + [item for battery in fixtures for item in ('--battery', str(battery))], cwd=REPO, check=True)
    conditions = verify_scene(args.output, args.scene, args.rom, symbols)
    (args.output / 'scene.json').write_text(json.dumps({
        'passed': True, 'scene': args.scene, 'map': map_name, 'clock_hour': hour,
        'rom_sha256': digest(args.rom), 'parent_rom_sha256': parent_rom_sha256,
        'synthetic_native_events': args.scene == 'pool-full',
        'batteries': [digest(path) for path in fixtures],
        'fixture_directories': [str(path.parent.resolve()) for path in fixtures],
        'native_cadence_control': str(args.output / 'native-controls/cadence-comparison.json')
            if args.scene != 'pool-full' else None,
        'players': 4, 'speed_multipliers': [1, 2, 3, 4],
        'stress_conditions': conditions,
        'scope': 'Finite native instructions, palettes, pose tiles, frame cadence and hardware OAM; no universal map-state proof.',
    }, indent=2) + '\n')


if __name__ == '__main__':
    main()
