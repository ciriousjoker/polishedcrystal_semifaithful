#!/usr/bin/env python3
"""Golden networks executed by real mGBA. Serial, no training or emulation shim."""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
import random
import struct
import subprocess

ROOT = pathlib.Path(__file__).resolve().parents[2]
class State(C.Structure):
    _fields_ = [(x, C.c_uint8) for x in 'afbcdehl'] + [(x, C.c_uint16) for x in ('sp', 'pc', 'bank')] + [('m', C.c_uint8 * 65536)] + [(x, C.c_uint32) for x in ('steps', 'fault', 'event_count', 'event_pos')] + [('events', C.c_uint8 * 4096)]

def model_bytes(n, h, a, weights, biases, shifts):
    assert all(-8 <= x <= 7 for x in weights)
    assert all(-16384 <= x <= 16384 for x in biases)
    assert all(0 <= x <= 15 for x in shifts)
    return b'TAI4' + bytes([1, n, h, a]) + bytes(8) + bytes((weights[i] & 15) | ((weights[i+1] & 15) << 4) for i in range(0, len(weights), 2)) + struct.pack('<'+'h'*len(biases), *biases) + bytes(shifts)

def reference(n, h, a, weights, biases, shifts, inputs, legal):
    hidden = []
    for j in range(h):
        s = biases[j] + sum(inputs[i] * weights[j*n+i] for i in range(n))
        assert -32768 <= s <= 32767
        hidden.append(min(15, max(0, s // (1 << shifts[j]))))
    logits = []
    for j in range(a):
        s = biases[h+j] + sum(hidden[i] * weights[n*h+j*h+i] for i in range(h))
        assert -32768 <= s <= 32767
        logits.append(s)
    slots = [i for i in range(a) if legal & (1 << i)]
    action = max(slots, key=lambda i: logits[i]) if slots else 255
    return hidden, logits, action

def run(build, prefix):
    build.mkdir(parents=True, exist_ok=True)
    commands = []
    def command(args, cwd=None):
        commands.append(list(map(str, args)))
        subprocess.run(list(map(str, args)), cwd=cwd, check=True)
    wrapper = ROOT/'tools/trainer_ai/network_runtime_oracle.c'
    library = build/'oracle.dylib'
    command(['cc', '-O2', '-dynamiclib', '-I'+str(prefix/'include'), wrapper, '-L'+str(prefix/'lib'), '-lmgba', '-Wl,-rpath,'+str(prefix/'lib'), '-o', library])
    mg = C.CDLL(str(library))
    mg.oracle_open.argtypes = [C.c_char_p]
    mg.oracle_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16, C.c_uint16]
    mg.network_last_cycles.restype = C.c_uint64
    mg.network_last_ticks.restype = C.c_uint64
    rng = random.Random(71423)
    reports = []
    for n,h,a in [(2,16,12), (64,48,12), (136,136,12)]:
        directory = build/f'{n}x{h}x{a}'
        directory.mkdir(exist_ok=True)
        count = n*h+h*a
        blank = model_bytes(n,h,a,[0]*count,[0]*(h+a),[0]*h)
        (directory/'network_runtime_model.bin').write_bytes(blank)
        command(['rgbasm', '-D', f'NET_INPUTS={n}', '-D', f'NET_HIDDEN={h}', '-D', f'NET_ACTIONS={a}', '-o', directory/'runtime.o', ROOT/'tools/trainer_ai/network_runtime.asm'], cwd=directory)
        command(['rgblink', '-o', directory/'base.gb', '-n', directory/'runtime.sym', '-m', directory/'runtime.map', directory/'runtime.o'])
        command(['rgbfix', '-v', '-p', '0', directory/'base.gb'])
        symbols = {}
        for line in (directory/'runtime.sym').read_text().splitlines():
            fields = line.split()
            if len(fields)==2 and ':' in fields[0]:
                symbols[fields[1]] = int(fields[0].split(':')[1],16)
        base = (directory/'base.gb').read_bytes()
        cases = []
        cases.append(('zero_ties', [0]*count, [0]*(h+a), [0]*h, [0]*n))
        cases.append(('negative_extreme', [-8]*count, [-16384]*(h+a), [0]*h, [15]*n))
        cases.append(('positive_extreme', [7]*count, [16384]*(h+a), [0]*h, [15]*n))
        cases.append(('all_shifts', [7]*(n*h)+[-8]*(h*a), [(-1 if j%2 else 16384) for j in range(h)]+[-16384]*a, [j%16 for j in range(h)], [15]*n))
        cases.append(('signed_logits', [0]*count, [15]*h+[-16384,16384,-1,0,1,255,256,-255,-256,3276,-3276,16384][:a], [0]*h, [15]*n))
        for k in range(6):
            cases.append((f'random_{k}', [rng.randrange(-8,8) for _ in range(count)], [rng.randint(-16384,16384) for _ in range(h+a)], [rng.randrange(16) for _ in range(h)], [rng.randrange(16) for _ in range(n)]))
        for name, weights, biases, shifts, inputs in cases:
            data = bytearray(base)
            payload = model_bytes(n,h,a,weights,biases,shifts)
            offset = symbols['NetworkModel']
            data[offset:offset+len(payload)] = payload
            rom = directory/'fixture.gb'
            rom.write_bytes(data)
            assert mg.oracle_open(str(rom).encode()) == 0
            probe = State(); probe.pc = symbols['NetworkTimingProbe']; probe.sp = 0xdff0; probe.bank = 1
            probe.m[0xdff0] = 0xfe; probe.m[0xdff1] = 0xff
            assert mg.oracle_run(C.byref(probe),100,0xfffe,0) == 0
            assert mg.network_last_ticks() == 160
            assert mg.network_last_cycles() == 80, ('timing_unit_calibration', mg.network_last_cycles())
            for mask in [0,0xfff,0x3f0,1,0x800,0x1000]:
                state = State()
                state.pc = symbols['NetworkEvaluate']; state.sp = 0xdff0; state.bank = 1
                state.m[0xdff0] = 0xfe; state.m[0xdff1] = 0xff
                for i,v in enumerate(inputs): state.m[symbols['NetworkInput']+i] = v
                state.m[symbols['NetworkLegal']] = mask & 255
                state.m[symbols['NetworkLegal']+1] = mask >> 8
                rc = mg.oracle_run(C.byref(state), 10000000, 0xfffe, 0)
                assert not rc, (n,h,name,mask,rc,hex(state.pc))
                expected = reference(n,h,a,weights,biases,shifts,inputs,mask)
                actual = (list(state.m[symbols['NetworkHidden']:symbols['NetworkHidden']+h]), list(struct.unpack('<'+'h'*a,bytes(state.m[symbols['NetworkLogits']:symbols['NetworkLogits']+a*2]))), state.m[symbols['NetworkAction']])
                assert actual == expected, (n,h,name,mask,actual,expected)
                assert state.a == expected[2] and state.sp == 0xdff2
                reports.append(dict(shape=[n,h,a],case=name,mask=mask,steps=state.steps,cycles=mg.network_last_cycles(),action=state.a))
            mg.oracle_close()
        reports.append(dict(shape=[n,h,a],model_bytes=len(blank),code_bytes=symbols['NetworkCodeEnd']-0x150,table_bytes=512,rom_end=max(symbols['NetworkModelEnd'],symbols['NetworkProductHigh']+256,symbols['NetworkCodeEnd']),model_start=symbols['NetworkModel'],alignment_bytes=symbols['NetworkProductLow']-symbols['NetworkCodeEnd'],scratch_bytes=symbols['NetworkScratchEnd']-0xc000,hram_bytes=1,stack_bytes=6))
    sources = [ROOT/'tools/trainer_ai'/name for name in ['network_runtime.asm','network_runtime_check.py','network_runtime_oracle.c']]
    artifacts = {str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in build.rglob('*') if p.is_file() and p.suffix in ('.gb','.sym','.map','.o','.dylib','.bin')}
    result = dict(artifacts=artifacts,commands=commands,sources={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in sources},mgba_sha256=hashlib.sha256((prefix/'lib/libmgba.dylib').read_bytes()).hexdigest(),reports=reports)
    (build/'results.json').write_text(json.dumps(result,indent=2)+'\n')
    return result
if __name__ == '__main__':
    p = argparse.ArgumentParser(); p.add_argument('--build-dir',type=pathlib.Path,required=True); p.add_argument('--mgba-prefix',type=pathlib.Path,required=True)
    args=p.parse_args(); result=run(args.build_dir.resolve(),args.mgba_prefix.resolve()); print(json.dumps(dict(cases=len(result['reports']),results=str(args.build_dir/'results.json'))))
