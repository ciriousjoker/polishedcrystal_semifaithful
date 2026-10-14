#!/usr/bin/env python3
"""Execute the assembled dot kernel's small opcode subset, with SM83 T cycles.
This is a bounded kernel checker, not a general emulator or hardware measurement.
Requires RGBDS and a C compiler. No desktop throughput extrapolation.
"""
import argparse
import pathlib
import random
import subprocess
import tempfile


def run(rom, inputs, weights, bias):
    memory = bytearray(65536)
    memory[:len(rom)] = rom
    memory[0xc000:0xc000 + len(inputs)] = bytes(inputs)
    memory[0xc100:0xc100 + len(weights)] = bytes(weights)
    regs = dict(a=len(inputs) // 2, b=0xc1, c=0, d=(bias >> 8) & 255,
                e=bias & 255, h=0xc0, l=0)
    pc, sp, cycles, carry, zero = 0x100, 0xfffe, 0, False, False
    counts = {}
    def pair(hi, lo): return regs[hi] * 256 + regs[lo]
    def write_pair(hi, lo, value):
        regs[hi], regs[lo] = (value >> 8) & 255, value & 255
    for _ in range(100000):
        op = memory[pc]; pc += 1
        elapsed = 0
        if op == 0xe0:
            memory[0xff00 + memory[pc]] = regs['a']; pc += 1; elapsed = 12
        elif op == 0xf0:
            regs['a'] = memory[0xff00 + memory[pc]]; pc += 1; elapsed = 12
        elif op == 0x2a:
            addr = pair('h', 'l'); regs['a'] = memory[addr]
            write_pair('h', 'l', addr + 1); elapsed = 8
        elif op == 0xcb:
            assert memory[pc] == 0x37
            pc += 1; a = regs['a']; regs['a'] = ((a << 4) | (a >> 4)) & 255
            carry, zero, elapsed = False, regs['a'] == 0, 8
        elif op == 0xe6:
            regs['a'] &= memory[pc]; pc += 1
            carry, zero, elapsed = False, regs['a'] == 0, 8
        elif op == 0xe5:
            sp -= 1; memory[sp] = regs['h']; sp -= 1; memory[sp] = regs['l']; elapsed = 16
        elif op == 0xe1:
            regs['l'] = memory[sp]; sp += 1; regs['h'] = memory[sp]; sp += 1; elapsed = 12
        elif op == 0x6f: regs['l'] = regs['a']; elapsed = 4
        elif op == 0x0a: regs['a'] = memory[pair('b', 'c')]; elapsed = 8
        elif op == 0xb5:
            regs['a'] |= regs['l']; carry, zero, elapsed = False, regs['a'] == 0, 4
        elif op == 0x26: regs['h'] = memory[pc]; pc += 1; elapsed = 8
        elif op == 0x7e: regs['a'] = memory[pair('h', 'l')]; elapsed = 8
        elif op in (0x83, 0x8a):
            n = regs['a'] + regs['e' if op == 0x83 else 'd'] + (int(carry) if op == 0x8a else 0)
            regs['a'] = n & 255; carry, zero, elapsed = n > 255, (n & 255) == 0, 4
        elif op == 0x5f: regs['e'] = regs['a']; elapsed = 4
        elif op == 0x57: regs['d'] = regs['a']; elapsed = 4
        elif op == 0x24:
            regs['h'] = (regs['h'] + 1) & 255; zero, elapsed = regs['h'] == 0, 4
        elif op == 0x03: write_pair('b', 'c', pair('b', 'c') + 1); elapsed = 8
        elif op == 0x3d:
            regs['a'] = (regs['a'] - 1) & 255; zero, elapsed = regs['a'] == 0, 4
        elif op == 0x20:
            rel = memory[pc]; pc += 1
            if not zero: pc += rel if rel < 128 else rel - 256
            elapsed = 8 if zero else 12
        elif op == 0xc9:
            cycles += 16
            result = pair('d', 'e')
            return result if result < 32768 else result - 65536, cycles + 0, counts
        else: raise AssertionError(f"unsupported opcode {op:02x} at {pc - 1:04x}")
        cycles += elapsed
        counts[f'{op:02x}'] = counts.get(f'{op:02x}', 0) + 1
    raise AssertionError('kernel did not return')


def signed(n): return n if n < 8 else n - 16


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--build-dir', type=pathlib.Path)
    args = parser.parse_args()
    source = pathlib.Path(__file__).resolve().parent
    with tempfile.TemporaryDirectory() as temporary:
        build = args.build_dir or pathlib.Path(temporary)
        build.mkdir(parents=True, exist_ok=True)
        subprocess.run(['rgbasm', '-o', str(build / 'dot.o'), str(source / 'inference_sm83.asm')], check=True)
        subprocess.run(['rgblink', '-o', str(build / 'dot.gb'), '-m', str(build / 'dot.map'), str(build / 'dot.o')], check=True)
        rom = (build / 'dot.gb').read_bytes()
        for x in range(16):
            for w in range(16):
                got, cycles, _ = run(rom, [x, 0], [w], 0)
                assert got == x * signed(w), (x, w, got)
                assert cycles == 328, cycles
        rng = random.Random(421)
        for length in (2, 48, 64, 96, 192):
            for _ in range(30):
                xs = [rng.randrange(16) for _ in range(length)]
                ws = [rng.randrange(16) for _ in range(length)]
                packed = [ws[i] | ws[i + 1] << 4 for i in range(0, length, 2)]
                bias = rng.randrange(-16384, 16385)
                expected = bias + sum(x * signed(w) for x, w in zip(xs, ws))
                assert -32768 <= expected <= 32767
                got, cycles, counts = run(rom, xs, packed, bias)
                assert got == expected, (length, expected, got)
                assert cycles == 304 * (length // 2) + 24, (length, cycles)
            print(f'dot length={length}: {cycles} T cycles; products and signed carry exact')
        # Boundary vectors test both signed extremes and accumulator carry chains.
        for length in (64, 96):
            for w, bias in ((8, -16384), (7, 16384)):
                expected = bias + length * 15 * signed(w)
                got, _, _ = run(rom, [15] * length, [w | w << 4] * (length // 2), bias)
                assert got == expected and -32768 <= expected <= 32767
        binary = build / 'inference'
        subprocess.run(['cc', '-std=c11', '-O2', '-Wall', '-Wextra', '-Werror',
                        '-fsanitize=undefined,address', '-o', str(binary), str(source / 'inference.c')], check=True)
        before = subprocess.check_output([str(binary)], text=True)
        subprocess.run([str(binary), 'export', str(build / 'fixture.tai')], check=True)
        after = subprocess.check_output([str(binary), 'load', str(build / 'fixture.tai')], text=True)
        assert before == after
        assert (build / 'fixture.tai').stat().st_size == 3976
        data = (build / 'fixture.tai').read_bytes()
        ni, nh, no = data[5:8]
        nw = ni * nh + nh * no
        ws = [signed((data[16 + i // 2] >> (4 * (i % 2))) & 15) for i in range(nw)]
        bias_start = 16 + nw // 2
        bs = [int.from_bytes(data[bias_start + 2*i:bias_start + 2*i + 2], 'little', signed=True)
              for i in range(nh + no)]
        shifts = data[bias_start + 2 * (nh + no):]
        xs = [i % 16 for i in range(ni)]
        hidden = [min(15, max(0, (bs[j] + sum(xs[i] * ws[j * ni + i] for i in range(ni))) //
                              (1 << shifts[j]))) for j in range(nh)]
        expected = [bs[nh + j] + sum(hidden[i] * ws[ni * nh + j * nh + i] for i in range(nh))
                    for j in range(no)]
        observed = [int(v) for v in before.split('logits=')[1].strip().split(',')]
        assert observed == expected
        invalid = bytearray(data)
        invalid[-1] = 16
        (build / 'invalid.tai').write_bytes(invalid)
        assert subprocess.run([str(binary), 'load', str(build / 'invalid.tai')], capture_output=True).returncode == 3
        print('256 scalar products, 150 vector cases, 4 boundary vectors, independent network oracle passed')
        print(before.strip())
        print('Cycle formula: 304 T cycles per pair + 24, including entry and ret.')

if __name__ == '__main__': main()
