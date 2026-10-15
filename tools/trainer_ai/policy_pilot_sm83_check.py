#!/usr/bin/env python3
"""One final learned TAI4 artifact on the existing real mGBA SM83 oracle.

No new emulator, no broad network suite, and no ordinary-game strength claim.
Run serially only after the coordinator hands over the sole CPU slot.
"""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
import struct
import subprocess

from network_runtime_check import ROOT, State, reference
from policy_pilot import N, H, A, SCOPE, decode, digest, features, integer_forward


def run(model, evaluation, build, prefix):
    payload = model.read_bytes()
    params = decode(payload)
    report = json.loads(evaluation.read_text())
    sha = hashlib.sha256(payload).hexdigest()
    if report['scope'] != SCOPE or report['provenance']['trained'] != sha:
        raise ValueError('Evaluation does not identify this exported experimental model')
    cases = report.get('runtime_probes', [])
    if not cases or len(cases) > 8:
        raise ValueError('Expected one through eight actual heldout observation probes')
    for case in cases:
        if case['inputs'] != features(case['observation']) or case['legal'] not in (1, 2, 3):
            raise ValueError('Invalid actual observation encoding or restricted legal mask')
    build.mkdir(parents=True, exist_ok=True)
    build = build.resolve()
    prefix = prefix.resolve()
    commands = []

    def command(arguments):
        arguments = list(map(str, arguments))
        commands.append(arguments)
        subprocess.run(arguments, cwd=build, check=True)

    # Existing runtime includes this exact filename with INCBIN, not ROM patching.
    (build / 'network_runtime_model.bin').write_bytes(payload)
    runtime = ROOT / 'tools/trainer_ai/network_runtime.asm'
    wrapper = ROOT / 'tools/trainer_ai/network_runtime_oracle.c'
    obj, rom, sym = [build / name for name in ('runtime.o', 'learned.gb', 'learned.sym')]
    command(['rgbasm', '-D', 'NET_INPUTS=16', '-D', 'NET_HIDDEN=16', '-D',
             'NET_ACTIONS=12', '-o', obj, runtime])
    command(['rgblink', '-o', rom, '-n', sym, '-m', build / 'learned.map', obj])
    command(['rgbfix', '-v', '-p', '0', rom])
    symbols = {}
    for line in sym.read_text().splitlines():
        fields = line.split()
        if len(fields) == 2 and ':' in fields[0]:
            symbols[fields[1]] = int(fields[0].split(':')[1], 16)
    start = symbols['NetworkModel']
    if rom.read_bytes()[start:start + len(payload)] != payload:
        raise AssertionError('Linked ROM did not INCBIN the exact final model')
    library = build / 'oracle.dylib'
    command(['cc', '-O2', '-dynamiclib', '-I' + str(prefix / 'include'), wrapper,
             '-L' + str(prefix / 'lib'), '-lmgba', '-Wl,-rpath,' + str(prefix / 'lib'),
             '-o', library])
    mg = C.CDLL(str(library))
    mg.oracle_open.argtypes = [C.c_char_p]
    mg.oracle_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16, C.c_uint16]
    mg.network_last_cycles.restype = C.c_uint64
    mg.network_last_ticks.restype = C.c_uint64
    if mg.oracle_open(str(rom).encode()):
        raise RuntimeError('mGBA oracle_open failed')
    results = []
    try:
        for index, case in enumerate(cases):
            inputs = case['inputs']
            # Actual legal mask plus no-legal-action sentinel ABI check.
            for legal in (case['legal'], 0):
                state = State()
                state.pc, state.sp, state.bank = symbols['NetworkEvaluate'], 0xdff0, 1
                state.m[state.sp], state.m[state.sp + 1] = 0xfe, 0xff
                for i, value in enumerate(inputs):
                    state.m[symbols['NetworkInput'] + i] = value
                state.m[symbols['NetworkLegal']] = legal & 255
                state.m[symbols['NetworkLegal'] + 1] = legal >> 8
                rc = mg.oracle_run(C.byref(state), 1000000, 0xfffe, 0)
                if rc:
                    raise RuntimeError(f'mGBA oracle fault {rc} on probe {index}')
                hidden = list(state.m[symbols['NetworkHidden']:symbols['NetworkHidden'] + H])
                offset = symbols['NetworkLogits']
                logits = list(struct.unpack('<' + 'h' * A, bytes(state.m[offset:offset + 2*A])))
                actual = (hidden, logits, state.m[symbols['NetworkAction']])
                expected = integer_forward(params, inputs, legal)
                if actual != expected or actual != reference(N, H, A, *params, inputs, legal):
                    raise AssertionError(f'Final model SM83 export mismatch on probe {index}')
                if state.a != expected[2] or state.sp != 0xdff2:
                    raise AssertionError('SM83 network return ABI mismatch')
                results.append(dict(probe=index, legal=legal, action=state.a,
                                    steps=state.steps, cycles=mg.network_last_cycles()))
    finally:
        mg.oracle_close()
    output = dict(scope=SCOPE, status='passed', model_sha256=sha, model_bytes=len(payload),
                  evaluation_sha256=digest(evaluation), commands=commands, cases=results,
                  sources={str(path): digest(path) for path in
                           (pathlib.Path(__file__), runtime, wrapper, ROOT / 'tools/trainer_ai/policy_pilot.py')},
                  mgba_sha256=digest(prefix / 'lib/libmgba.dylib'),
                  artifacts={str(path): digest(path) for path in build.iterdir() if path.is_file()
                             and path.name != 'results.json'})
    (build / 'results.json').write_text(json.dumps(output, indent=2) + '\n')
    return output


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('model', 'evaluation', 'build-dir', 'mgba-prefix'):
        parser.add_argument('--' + name, type=pathlib.Path, required=True)
    args = parser.parse_args()
    result = run(args.model, args.evaluation, args.build_dir, args.mgba_prefix)
    print(json.dumps(dict(status=result['status'], model_sha256=result['model_sha256'],
                          cases=len(result['cases']))))
