#!/usr/bin/env python3
"""Bounded actual-ROM event replay proof, using independent unmodified mGBA."""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
import random
import subprocess
import stateful_lifting
from stateful_fixture_check import State
from rng_events import ENTRIES, STOP, BUDGET, prepare_call, replay_call, replay_events

ROOT = pathlib.Path(__file__).resolve().parents[2]
REGISTERS = ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank')
RAM = (*range(0xc000, 0xe000), *range(0xff80, 0xffff))


def digest(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def run(rom, sym, source, prefix, build):
    build.mkdir(parents=True, exist_ok=True)
    roots = (*ENTRIES, 'DoNothing')  # Actual farjp return trampoline.
    manifest = stateful_lifting.build(rom, sym, roots, build, source_root=source)
    lib = stateful_lifting.load_verified(build, rom, sym, source, roots)
    lib.stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]
    wrapper = ROOT / 'tools/trainer_ai/rng_events_oracle.c'
    oracle = build / 'oracle.dylib'
    command = ['cc', '-O2', '-dynamiclib', '-I' + str(prefix / 'include'),
               str(wrapper), '-L' + str(prefix / 'lib'), '-lmgba',
               '-Wl,-rpath,' + str(prefix / 'lib'), '-o', str(oracle)]
    subprocess.run(command, check=True)
    mg = C.CDLL(str(oracle))
    mg.oracle_open.argtypes = [C.c_char_p]
    mg.oracle_div_inputs.argtypes = [C.POINTER(C.c_uint8), C.c_uint32]
    mg.oracle_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16, C.c_uint16]
    assert mg.oracle_open(str(rom).encode()) == 0
    sy = stateful_lifting.symbols(sym)
    rng = random.Random(93117)
    records = []
    try:
        for case in range(24):
            initial = State()
            initial.sp = 0xdff0
            initial.bank = 1
            initial.a, initial.f = 173, (case % 2) * 16
            initial.b, initial.c, initial.d, initial.e = 11, 23, 37, 41
            initial.h, initial.l = 53, 61
            initial.m[sy['hROMBank'][1]] = 1
            for address in RAM:
                initial.m[address] = rng.randrange(256)
            initial.m[sy['hROMBank'][1]] = 1
            initial.m[sy['wLinkMode'][1]] = 0
            state_address = sy['wRNGState'][1]
            seven = [0] * 7 if case == 0 else [255] * 7 if case == 1 else [rng.randrange(256) for _ in range(7)]
            initial.m[state_address:state_address + 7] = seven
            for refreshes in (0, 1, 2):
                reference = State.from_buffer_copy(bytes(initial))
                native = State.from_buffer_copy(bytes(initial))
                calls = [('UpdateDividerCounters', None), ('AdvanceRNGState', None)] * refreshes
                calls += [('Random', None), ('BattleRandomRange', (1, 2, 3, 127, 128, 129, 255)[case % 7])]
                events = []
                for name, argument in calls:
                    forced = ([0, 255, 255, 0] if case % 2 == 0 else [255, 0, 0, 255]) * 1024 if case >= 12 else []
                    assert mg.oracle_div_inputs((C.c_uint8 * len(forced))(*forced), len(forced)) == 0
                    before = list(reference.m[state_address:state_address + 7])
                    incoming_flags = reference.f
                    prepare_call(reference, sy, name, argument)
                    assert mg.oracle_run(C.byref(reference), BUDGET, STOP, sy['hROMBank'][1]) == 0
                    tape = list(reference.events[:reference.event_count])

                    try:
                        replay_events(lib, native, sy, [{'routine': name, 'div_reads': tape, 'argument': argument}])
                    except ValueError as error:
                        raise ValueError((case, refreshes, name, error)) from error
                    differences = [field for field in REGISTERS if getattr(reference, field) != getattr(native, field)]
                    ramdiff = [a for a in RAM if reference.m[a] != native.m[a]]
                    assert not differences and not ramdiff, (case, refreshes, name, differences, ramdiff[:10])
                    events.append({'routine': name, 'argument': argument, 'incoming_flags': incoming_flags,
                                   'div_reads': tape, 'before_rng7': before,
                                   'after_rng7': list(native.m[state_address:state_address + 7]),
                                   'registers': {f: getattr(native, f) for f in REGISTERS},
                                   'full_supported_ram_sha256': hashlib.sha256(bytes(native.m[a] for a in RAM)).hexdigest()})
                records.append({'case': case, 'div_mode': 'boundary-injected' if case >= 12 else 'actual-recorded', 'initial_carry': bool(initial.f & 16), 'initial_rng7': seven,
                                'refresh_pairs': refreshes, 'events': events, 'draw': native.a})
    finally:
        mg.oracle_close()
    # Unknown events and invalid/underconsumed tapes must fail closed.
    rejected = []
    for name, tape, argument in [('VBlank', [], None), ('BattleRandomRange', [], 0),
                                  ('UpdateDividerCounters', [], None), ('AdvanceRNGState', [0, 255], None)]:
        try:
            replay_call(lib, State.from_buffer_copy(bytes(initial)), sy, name, tape, argument)
        except ValueError:
            rejected.append(name + ':' + str(tape))
        else:
            raise AssertionError('Expected rejection')
    sensitivity = [{'case': i, 'draws_by_refresh_count': [r['draw'] for r in records if r['case'] == i]}
                   for i in range(24)]
    assert any(len(set(row['draws_by_refresh_count'])) > 1 for row in sensitivity)
    report = {'profile': 'explicit-source-routine-events-v1', 'cases': len(records),
              'calls': sum(len(r['events']) for r in records), 'records': records,
              'sensitivity': sensitivity, 'rejections': rejected,
              'manifest_sha256': digest(build / 'manifest.json'),
              'generator_sha256': manifest['generator_sha256'], 'rom_sha256': digest(rom),
              'symbols_sha256': digest(sym), 'oracle_wrapper_sha256': digest(wrapper),
              'base_oracle_wrapper_sha256': digest(ROOT / 'tools/trainer_ai/stateful_oracle.c'),
              'mgba_library_sha256': digest(prefix / 'lib/libmgba.dylib'),
              'compile_command': command,
              'limits': 'First 36 sequences record actual DIV; last 36 inject declared boundary inputs through the wrapper only. '
                        'Independent mGBA resets DIV phase per routine call; interrupts/LCD disabled. '
                        'This proves ordered synchronous routine semantics with exact DIV replay, '
                        'not continuous hardware timing, VBlank scheduling, distribution, frontend projection, or whole M2.'}
    (build / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    return report


if __name__ == '__main__':
    p = argparse.ArgumentParser()
    for name in ('rom', 'sym', 'source', 'prefix', 'build'):
        p.add_argument('--' + name, type=pathlib.Path, required=True)
    a = p.parse_args()
    report = run(a.rom, a.sym, a.source, a.prefix, a.build)
    print(json.dumps({k: report[k] for k in ('cases', 'calls', 'sensitivity', 'rejections')}))
