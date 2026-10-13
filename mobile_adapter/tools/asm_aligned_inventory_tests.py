"""Execute the dense-scene proof against an independent source census."""
import random


def verify_aligned_inventory(rom, symbols, require):
    bank, entry = symbols['MobileVirtualAlignedScene']
    source = symbols['wMobileNativeOriginals'][1]
    force = symbols['wMobileVirtualForceFull'][1]
    fast, limit = symbols['wMobileVirtualFastAdmission'][1], symbols['wMobileVirtualRemoteLimit'][1]
    shadow, used = symbols['wShadowOAM'][1], symbols['hUsedOAMIndex'][1]
    lines = ((symbols['wMobileActorTiles1'][1] + 13 * 4 + 255) & 0xff00) + 160
    rng = random.Random(0x70726f6f66)
    cases = maximum = 0
    for vector in range(4096):
        memory = bytearray(rng.randbytes(65536))
        origin = rng.randrange(16)
        tops = [origin + rng.randrange(1, 10) * 16 if vector % 4 < 2 else rng.randrange(256)
                for _ in range(13)]
        raw_offsets = rng.sample(range(0, 160, 16), 10)
        for slot, top in enumerate(tops):
            record = source + slot * 8
            memory[record + 1] = top
            memory[record + 6] = 0xc4 if slot < 10 else rng.choice((0, 0x44, 0x44))
            if slot < 10:
                memory[record + 7] = raw_offsets[slot]
                memory[shadow + raw_offsets[slot] + 8] = top
        memory[fast] = 2 if vector % 8 else rng.choice((0, 1))
        memory[limit] = 1 if vector % 9 else rng.choice((0, 2, 3))
        memory[used] = 160 if vector % 10 else rng.randrange(160)
        expected = bytearray(memory)
        expected[force] = expected[lines + 14] = 0
        valid = memory[fast] == 2 and memory[limit] == 1 and memory[used] == 160
        if valid:
            alignment = tops[0] % 16
            visible = [top for slot, top in enumerate(tops)
                       if (slot < 10 or memory[source + slot * 8 + 6]) and 0 < top < 160]
            expected[lines + 14], expected[lines + 15] = len(visible), alignment
            expected[force] = int(len(visible) > 10 and all(top % 16 == alignment for top in visible))
        registers = {key: 0 for key in 'abcdehl'}
        pc, zero, carry, cycles = entry, False, False, 0

        def pair():
            return registers['h'] * 256 + registers['l']

        def fetch():
            nonlocal pc
            value = rom[bank * 0x4000 + pc - 0x4000]
            pc += 1
            return value

        for _ in range(600):
            op = fetch()
            if 0x40 <= op < 0x80 and op != 0x76:
                s, t = op & 7, op >> 3 & 7
                value = memory[pair()] if s == 6 else registers['bcdehl?a'[s]]
                if t == 6:
                    memory[pair()] = value
                else:
                    registers['bcdehl?a'[t]] = value
                cycles += 8 if 6 in (s, t) else 4
            elif op & 0xc7 == 0x06:
                registers['bcdehl?a'[op >> 3 & 7]] = fetch()
                cycles += 8
            elif op == 0xfa:
                registers['a'] = memory[fetch() | fetch() << 8]
                cycles += 16
            elif op == 0xea:
                memory[fetch() | fetch() << 8] = registers['a']
                cycles += 16
            elif op == 0xf0:
                registers['a'] = memory[0xff00 + fetch()]
                cycles += 12
            elif op == 0x0c:
                registers['c'] = (registers['c'] + 1) & 255
                zero = not registers['c']
                cycles += 4
            elif op in (0xe6, 0xc6, 0xfe, 0xa7, 0xaa, 0xaf, 0xb0):
                a = registers['a']
                value = fetch() if op in (0xe6, 0xc6, 0xfe) else registers['d'] if op == 0xaa else registers['b'] if op == 0xb0 else a
                result = a & value if op in (0xe6, 0xa7) else (a + value) & 255 if op == 0xc6 else a ^ value if op in (0xaa, 0xaf) else a | value if op == 0xb0 else (a - value) & 255
                carry = a + value > 255 if op == 0xc6 else a < value if op == 0xfe else False
                zero = not result
                if op != 0xfe:
                    registers['a'] = result
                cycles += 8 if op in (0xe6, 0xc6, 0xfe) else 4
            elif op == 0xcb:
                require(fetch() == 0x7f, 'Unexpected aligned inventory bit instruction')
                zero = not registers['a'] & 128
                cycles += 8
            elif op in (0x18, 0x20, 0x28, 0x30):
                delta = fetch()
                taken = op == 0x18 or (not zero if op == 0x20 else zero if op == 0x28 else not carry)
                if taken:
                    pc += delta if delta < 128 else delta - 256
                cycles += 12 if taken else 8
            elif op in (0xc9, 0xc0, 0xd8):
                taken = op == 0xc9 or (not zero if op == 0xc0 else carry)
                cycles += 16 if op == 0xc9 else 20 if taken else 8
                if taken:
                    require(memory == expected, 'Aligned scene proof changed its census, admission or non-private RAM')
                    cases += 1
                    maximum = max(maximum, cycles)
                    break
            else:
                raise ValueError(f'Unexpected inventory proof opcode {op:02x}')
        else:
            raise ValueError('Aligned scene proof did not terminate')
    require(maximum <= 3000, 'Aligned inventory exceeded its idle-stage bound')
    print(f'Emitted aligned native census: {cases} cases, max {maximum} cycles')
