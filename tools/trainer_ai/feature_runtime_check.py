#!/usr/bin/env python3
"""Serial actual-game feature extraction and trained integer inference oracle.

The fixture guard is deliberately narrow. This is not a general game trainer
hook or an implementation of general action legality.
"""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
import random
import struct
import subprocess

from network_runtime_check import ROOT, State, reference
from policy_pilot import A, H, N, SCOPE, decode, features, integer_forward

BASE_ROM = '314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e'
BASE_SYM = 'ee66579bea2f6212559b49cf4beda79316831bc068297f6e3206cc3037a4aca0'
MODEL = '2b0e3027dbde05d1802d0ecd61d02e62fd9874f7b1739fea518e29d3f59708b3'
OBJECTS = ('main.o home.o ram.o audio.o audio/music_player.o '
           'data/pokemon/dex_entries.o data/pokemon/egg_moves.o '
           'data/pokemon/evos_attacks.o data/maps/map_data.o data/text/common.o '
           'data/tilesets.o engine/movie/credits.o engine/overworld/events.o '
           'gfx/minis_icons.o gfx/pokemon.o gfx/sprites.o gfx/trainers.o '
           'gfx/items.o gfx/misc.o').split()
STATS = ('Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def symbols(path):
    result = {}
    for line in path.read_text().splitlines():
        fields = line.split()
        if len(fields) == 2 and ':' in fields[0]:
            bank, address = fields[0].split(':')
            result[fields[1]] = (int(bank, 16), int(address, 16))
    return result


def generate(build, sy, sample):
    addresses = {}
    guards = []

    def address(name, offset=0):
        bank, value = sy[name]
        if not (0xc000 <= value <= 0xdfff and bank in (0, 1)):
            raise ValueError('Fixture address requires an unaudited RAM bank')
        addresses[name] = value
        return f'FX_{name}' + (f'+{offset}' if offset else '')

    def guard(name, expected, offset=0):
        guards.append((name, offset, expected))
        address(name, offset)

    guard('wBattleMode', 2)
    guard('wPartyCount', 1)
    guard('wOTPartyCount', 1)
    guard('wCurBattleMon', 0)
    guard('wCurOTMon', 0)
    for side, stem in enumerate(('wBattleMon', 'wEnemyMon')):
        mon = sample['teams'][side][0]
        for suffix, expected in (('Species', mon['species']), ('Level', 50),
                                 ('Status', 0), ('Item', 0), ('Type1', 0), ('Type2', 0)):
            guard(stem + suffix, expected)
        for i, move in enumerate((33, 55, 0, 0)):
            guard(stem + 'Moves', move, i)
        team = ('wPlayer', 'wEnemy')[side]
        guard(team + 'Ability', mon['ability'])
        for index in range(1, 5):
            guard(team + 'SubStatus' + str(index), 0)
        for suffix in ('Atk', 'Def', 'SAtk', 'SDef', 'Spe', 'Acc', 'Eva'):
            guard(team + suffix + 'Level', 7)
        for suffix in ('RolloutCount', 'ConfuseCount', 'ToxicCount', 'DisableCount',
                       'EncoreCount', 'PerishCount', 'ProtectCount', 'Screens',
                       'Guards', 'Hazards', 'WrapCount', 'TrappingMove', 'Charging',
                       'FutureSightCount'):
            guard(team + suffix, 0)
        for suffix in ('HP', 'MaxHP', *STATS, 'PP'):
            address(stem + suffix)
    for name in ('wBattleWeather', 'wWeatherCount'):
        guard(name, 0)
    lines = []
    for side in (0, 1):
        own, other = [('wBattleMon', 'wEnemyMon')[i] for i in (side, side ^ 1)]
        descriptors = [(0, address(stem + 'HP'), address(stem + 'MaxHP'))
                       for stem in (own, other)]
        descriptors += [(0, address(stem + stat), 'FeatureDenominator255')
                        for stem in (own, other) for stat in STATS]
        descriptors += [(1, address(stem + 'PP', i), 'FeatureDenominator63')
                        for stem in (own, other) for i in (0, 1)]
        lines.append(f'FeatureDescriptors{side}:')
        for kind, numerator, denominator in descriptors:
            lines += [f'    db {kind}', f'    dw {numerator}, {denominator}']
    lines.append('FeatureGuards:')
    for name, offset, expected in guards:
        lines += [f'    dw {address(name, offset)}', f'    db {expected}']
    lines.append('    dw 0')
    (build / 'feature_runtime_tables.inc').write_text('\n'.join(lines) + '\n')
    (build / 'feature_runtime_addresses.inc').write_text('\n'.join(
        f'DEF FX_{name} EQU ${value:04x}' for name, value in sorted(addresses.items())) + '\n')
    return guards


def run(build, game, prefix, model, evaluation):
    build.mkdir(parents=True, exist_ok=True)
    base_rom = game / 'polishedcrystal-3.2.3.gbc'
    base_sym = game / 'polishedcrystal-3.2.3.sym'
    if sha(base_rom) != BASE_ROM or sha(base_sym) != BASE_SYM:
        raise ValueError('Frozen fixture changed: re-audit source and feature guard first')
    if sha(model) != MODEL:
        raise ValueError('Expected the exact final learned 312-byte pilot model')
    payload, report = model.read_bytes(), json.loads(evaluation.read_text())
    if report['scope'] != SCOPE or report['provenance']['trained'] != MODEL:
        raise ValueError('Evaluation provenance mismatch')
    params = decode(payload)
    probes = report['runtime_probes']
    if len(probes) != 8:
        raise ValueError('Expected eight retained heldout public probes')
    for probe in probes:
        if probe['inputs'] != features(probe['observation']):
            raise ValueError('Retained input schema mismatch')
    sy = symbols(base_sym)
    guards = generate(build, sy, probes[0]['observation'])
    frozen_paths = [game / p for p in OBJECTS + ['layout.link']]
    frozen_paths += [base_rom, base_sym, model, evaluation]
    frozen_paths += [ROOT / 'tools/trainer_ai' / name for name in (
        'network_runtime.asm', 'network_runtime_oracle.c', 'network_runtime_check.py',
        'policy_pilot.py', 'feature_runtime.asm', 'feature_runtime_check.py')]
    frozen = {str(path): sha(path) for path in frozen_paths}
    commands = []

    def command(args, cwd=build):
        commands.append(list(map(str, args)))
        subprocess.run(list(map(str, args)), cwd=cwd, check=True)

    (build / 'network_runtime_model.bin').write_bytes(payload)
    command(['rgbasm', '-D', 'NET_GAME=1', '-D', 'NET_BANK=127',
             '-D', 'NET_INPUTS=16', '-D', 'NET_HIDDEN=16', '-D', 'NET_ACTIONS=12',
             '-o', build / 'network.o', ROOT / 'tools/trainer_ai/network_runtime.asm'])
    command(['rgbasm', '-o', build / 'feature.o', ROOT / 'tools/trainer_ai/feature_runtime.asm'])
    rom, sym = build / 'game.gbc', build / 'game.sym'
    command(['rgblink', '-Weverything', '-Wtruncation=1', '-M', '-n', sym,
             '-m', build / 'game.map', '-p', '0xff', '-l', 'layout.link', '-o', rom,
             *OBJECTS, build / 'network.o', build / 'feature.o'], game)
    command(['rgbfix', '-Weverything', '-csjv', '-t', 'PKPCRYSTAL', '-i', 'PKPC',
             '-n', '0x32', '-p', '0xff', '-k', '01', '-l', '0x33',
             '-m', 'MBC3+TIMER+RAM+BATTERY', '-r', '3', rom])
    linked = symbols(sym)

    def addr(name):
        return linked[name][1]

    for name in ('FeatureEvaluate', 'FeatureExtract', 'NetworkEvaluate', 'NetworkModel'):
        if linked[name][0] != 127:
            raise AssertionError('Code or model escaped the selected game bank')
    model_offset = 127 * 0x4000 + addr('NetworkModel') - 0x4000
    if rom.read_bytes()[model_offset:model_offset + 312] != payload:
        raise AssertionError('Model changed during linking')
    library = build / 'oracle.dylib'
    command(['cc', '-O2', '-dynamiclib', '-I' + str(prefix / 'include'),
             ROOT / 'tools/trainer_ai/network_runtime_oracle.c', '-L' + str(prefix / 'lib'),
             '-lmgba', '-Wl,-rpath,' + str(prefix / 'lib'), '-o', library])
    mg = C.CDLL(str(library))
    mg.oracle_open.argtypes = [C.c_char_p]
    mg.oracle_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16, C.c_uint16]
    mg.network_last_cycles.restype = C.c_uint64
    mg.network_last_ticks.restype = C.c_uint64
    if mg.oracle_open(str(rom).encode()):
        raise RuntimeError('Could not open linked game')

    def put(state, name, value, width=1, offset=0):
        pointer = sy[name][1] + offset
        state.m[pointer:pointer + width] = value.to_bytes(width, 'big')

    def state_for(observation):
        state = State()
        state.pc, state.sp, state.bank = addr('FeatureEvaluate'), 0xc0f0, 127
        state.a = observation['side']
        state.m[state.sp], state.m[state.sp + 1] = 0xfe, 0xff
        for name, offset, expected in guards:
            put(state, name, expected, offset=offset)
        for side, stem in enumerate(('wBattleMon', 'wEnemyMon')):
            mon = observation['teams'][side][0]
            for stat in ('HP', 'MaxHP', *STATS):
                put(state, stem + stat, mon[stat], 2)
            for slot, pp in enumerate(mon['pp']):
                put(state, stem + 'PP', pp, offset=slot)
        for pointer in range(0xc0d0, 0xc0f0):
            state.m[pointer] = 0xa5
        return state

    def call(state):
        before = bytes(state.m)
        rc = mg.oracle_run(C.byref(state), 1000000, 0xfffe, 0)
        if rc or state.sp != 0xc0f2 or state.bank != 127:
            raise AssertionError(('SM83 call fault or return ABI', rc, state.sp, state.bank))
        scratch = set(range(addr('NetworkInput'), addr('NetworkScratchEnd')))
        scratch.update(range(addr('FeatureSide'), addr('FeatureScratchEnd')))
        scratch.add(addr('NetworkPairs'))
        scratch.update(range(0xc0ea, 0xc0f0))  # deepest frame, six bytes below entry
        changed = [pointer for pointer in (*range(0xc000, 0xe000), *range(0xff80, 0xffff))
                   if state.m[pointer] != before[pointer] and pointer not in scratch]
        if changed:
            raise AssertionError(('Unexpected RAM write', changed[:10]))
        return mg.network_last_cycles()

    cases = []
    try:
        timing = state_for(probes[0]['observation'])
        timing.pc = addr('NetworkTimingProbe')
        call(timing)
        if mg.network_last_cycles() != 80 or mg.network_last_ticks() != 160:
            raise AssertionError('Timing calibration changed')
        for index, probe in enumerate(probes):
            observation = probe['observation']
            state = state_for(observation)
            cycles = call(state)
            inputs = list(state.m[addr('NetworkInput'):addr('NetworkInput') + N])
            hidden = list(state.m[addr('NetworkHidden'):addr('NetworkHidden') + H])
            logits = list(struct.unpack('<' + 'h' * A,
                          bytes(state.m[addr('NetworkLogits'):addr('NetworkLogits') + A * 2])))
            actual = (hidden, logits, state.a)
            if inputs != features(observation) or state.m[addr('FeatureStatus')] != 0:
                raise AssertionError(('Feature mismatch', index, inputs))
            if actual != integer_forward(params, inputs, probe['legal']):
                raise AssertionError(('End-to-end policy mismatch', index))
            if actual != reference(N, H, A, *params, inputs, probe['legal']):
                raise AssertionError(('Independent numerical reference mismatch', index))
            if state.m[addr('NetworkLegal')] != probe['legal']:
                raise AssertionError('Mask mismatch')
            numerical = state_for(observation)
            numerical.pc = addr('NetworkEvaluate')
            numerical.m[addr('NetworkInput'):addr('NetworkInput') + N] = bytes(inputs)
            numerical.m[addr('NetworkLegal')] = probe['legal']
            numerical_cycles = call(numerical)
            if numerical.a != state.a:
                raise AssertionError('Direct numerical action differs')
            extraction = state_for(observation)
            extraction.pc = addr('FeatureExtract')
            extraction_cycles = call(extraction)
            if list(extraction.m[addr('NetworkInput'):addr('NetworkInput') + N]) != inputs:
                raise AssertionError('Direct extraction differs')
            cases.append(dict(kind='heldout', probe=index, cycles=cycles,
                              extraction_cycles=extraction_cycles,
                              numerical_cycles=numerical_cycles,
                              guard_and_call_overhead_cycles=cycles - numerical_cycles - extraction_cycles,
                              inputs=inputs, legal=probe['legal'], action=state.a))
            private = state_for(observation)
            for name in ('wCurPlayerMove', 'wCurEnemyMove'):
                private.m[sy[name][1]] = 255
            for offset in range(7):
                private.m[sy['wRNGState'][1] + offset] = 255 - index - offset
            private.m[sy['hRandomAdd'][1]] = index + 17
            private.m[sy['hRandomSub'][1]] = index + 39
            call(private)
            if bytes(private.m[addr('NetworkInput'):addr('NetworkLogits') + 24]) != bytes(
                    state.m[addr('NetworkInput'):addr('NetworkLogits') + 24]) or private.a != state.a:
                raise AssertionError('Private committed actions/RNG leaked into policy features')
            cases.append(dict(kind='private-state-noninterference', probe=index))
        rng = random.Random(1542)
        edge_values = (0, 1, 16, 17, 254, 255, 256, 257, 32767, 32768, 65534, 65535)
        for index in range(36):
            observation = json.loads(json.dumps(probes[index % 8]['observation']))
            for mon in (observation['teams'][0][0], observation['teams'][1][0]):
                mon['MaxHP'] = edge_values[1 + index % (len(edge_values) - 1)]
                mon['HP'] = edge_values[index % len(edge_values)]
                for stat in STATS:
                    mon[stat] = rng.choice(edge_values)
                mon['pp'] = [rng.randrange(256) for _ in range(2)]
            state = state_for(observation)
            state.pc = addr('FeatureExtract')
            cycles = call(state)
            for mon in (observation['teams'][0][0], observation['teams'][1][0]):
                mon['pp'] = [pp & 63 for pp in mon['pp']]
            actual = list(state.m[addr('NetworkInput'):addr('NetworkInput') + N])
            if actual != features(observation):
                raise AssertionError(('16-bit edge extraction mismatch', index, actual))
            cases.append(dict(kind='edge-extraction', index=index, cycles=cycles))
        # Direct arithmetic coverage includes seventeenth-bit remainder carries.
        for numerator in (0, 1, 16, 254, 255, 256, 32768, 65534, 65535):
            for denominator in (1, 63, 255, 32769, 65535):
                state = state_for(probes[0]['observation'])
                state.pc = addr('FeatureRatio')
                state.b, state.c = numerator >> 8, numerator & 255
                state.d, state.e = denominator >> 8, denominator & 255
                cycles = call(state)
                if state.a != min(15, 15 * numerator // denominator):
                    raise AssertionError(('Direct ratio mismatch', numerator, denominator))
                cases.append(dict(kind='ratio', numerator=numerator,
                                  denominator=denominator, cycles=cycles))
        state = state_for(probes[0]['observation'])
        state.pc = addr('FeatureRatio')
        state.d, state.e = 0, 0
        call(state)
        if state.a != 255 or state.m[addr('FeatureStatus')] != 1:
            raise AssertionError('Direct zero denominator accepted')
        cases.append(dict(kind='ratio-zero-denominator'))
        # Each supported-entry guard independently rejects; no stale output survives.
        for name, offset, expected in guards:
            state = state_for(probes[0]['observation'])
            put(state, name, (expected + 1) & 255, offset=offset)
            for pointer in range(addr('NetworkInput'), addr('NetworkLogits') + 24):
                state.m[pointer] = 0x77
            state.m[addr('NetworkAction')] = 9
            cycles = call(state)
            if state.a != 255 or state.m[addr('FeatureStatus')] != 1:
                raise AssertionError(('Guard failed', name, offset))
            if any(state.m[addr('NetworkInput'):addr('NetworkLogits') + 24]):
                raise AssertionError('Unsupported entry retained stale feature/logit data')
            cases.append(dict(kind='guard', name=name, offset=offset, cycles=cycles))
        for side in (0, 1):
            for pp in ((0, 0), (1, 0), (0, 1), (1, 1), (0xc0, 0x80), (0xc1, 0x81)):
                observation = json.loads(json.dumps(probes[0]['observation']))
                observation['side'] = side
                observation['teams'][side][0]['pp'] = pp
                state = state_for(observation)
                cycles = call(state)
                mask = sum(1 << i for i, value in enumerate(pp) if value & 63)
                if mask:
                    if state.m[addr('FeatureStatus')] != 0 or state.m[addr('NetworkLegal')] != mask:
                        raise AssertionError('PP mask mismatch')
                    public = json.loads(json.dumps(observation))
                    for mon in (public['teams'][0][0], public['teams'][1][0]):
                        mon['pp'] = [value & 63 for value in mon['pp']]
                    expected = integer_forward(params, features(public), mask)[2]
                    if state.a != expected:
                        raise AssertionError('PP-mask selected wrong action')
                elif state.a != 255 or state.m[addr('FeatureStatus')] != 2:
                    raise AssertionError('Empty PP mask did not report unsupported Struggle')
                cases.append(dict(kind='PP-mask', side=side, pp=pp, mask=mask, cycles=cycles))
        for name in ('wBattleMonHP', 'wEnemyMonHP', 'wBattleMonMaxHP', 'wEnemyMonMaxHP'):
            state = state_for(probes[0]['observation'])
            put(state, name, 0, 2)
            call(state)
            if state.a != 255 or state.m[addr('FeatureStatus')] != 1:
                raise AssertionError(('Zero HP/denominator accepted', name))
            cases.append(dict(kind='zero-word', name=name))
        state = state_for(probes[0]['observation'])
        state.a = 2
        call(state)
        if state.a != 255 or state.m[addr('FeatureStatus')] != 1:
            raise AssertionError('Invalid perspective accepted')
        cases.append(dict(kind='invalid-perspective'))
    finally:
        mg.oracle_close()
    if any(sha(path) != value for path, value in frozen.items()):
        raise AssertionError('A frozen input changed during compilation/execution')
    rom_sections = [('runtime', addr('NetworkEvaluate'), addr('NetworkCodeEnd')),
                    ('products', addr('NetworkProductLow'), addr('NetworkProductHigh') + 256),
                    ('model', addr('NetworkModel'), addr('NetworkModelEnd')),
                    ('features', addr('FeatureEvaluate'), addr('FeatureROMEnd'))]
    occupied = sum(end - start for _, start, end in rom_sections)
    footprint = max(end for _, _, end in rom_sections) - min(start for _, start, _ in rom_sections)
    output = dict(status='passed', scope=SCOPE, schema='policy-pilot-16-v1',
                  claims='Conditional feature/numerical ABI only; no general trainer integration',
                  model_sha256=MODEL, commands=commands, frozen_sha256=frozen,
                  mgba_sha256=sha(prefix / 'lib/libmgba.dylib'), cases=cases,
                  memory=dict(rom_sections=rom_sections, occupied_rom_bytes=occupied,
                              rom_interval_bytes=footprint, alignment_bytes=footprint - occupied,
                              network_wram=addr('NetworkScratchEnd') - addr('NetworkInput'),
                              feature_wram=addr('FeatureScratchEnd') - addr('FeatureSide'),
                              hram_bytes=1, stack_below_entry_bytes=6,
                              entry_return_address_bytes=2),
                  artifacts={str(path): sha(path) for path in build.iterdir()
                             if path.is_file() and path.name != 'results.json'})
    (build / 'results.json').write_text(json.dumps(output, indent=2) + '\n')
    return output


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('build-dir', 'game', 'mgba-prefix', 'model', 'evaluation'):
        parser.add_argument('--' + name, type=pathlib.Path, required=True)
    args = parser.parse_args()
    result = run(*(getattr(args, name).resolve() for name in
                   ('build_dir', 'game', 'mgba_prefix', 'model', 'evaluation')))
    print(json.dumps(dict(status=result['status'], cases=len(result['cases']), memory=result['memory'])))
