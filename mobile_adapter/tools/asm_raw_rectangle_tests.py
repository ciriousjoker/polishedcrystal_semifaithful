"""Execute direct native OAM copies, including the reversed local source."""
import itertools
import random


def verify_raw_rectangles(rom, symbols, require):
    bank, entry = symbols['MobileVirtualAlignedRawRectangle']
    descriptor = symbols['wMobileNativeOriginals'][1]
    owners = symbols['wMobileNativePaletteOwners'][1]
    length, index, used = owners + 5, owners + 3, owners + 8
    output = (symbols['wMobileActorTiles1'][1] + 13 * 4 + 255) & 0xff00
    lines, bindings = output + 160, symbols['wMobileVirtualScratch'][1] + 64
    shadow = symbols['wShadowOAM'][1]
    rng = random.Random(0x726177)
    cases = maximum = 0
    vectors = itertools.product(range(0, 160, 16), (0, 1, 8, 16, 159, 160, 255),
                                (0, 1), (0, 128, 144, 145, 160), (0, 1), (0, 4, 5), (129, 139, 140))
    for raw, top, local, size, occupied, row_count, ly in vectors:
        memory = bytearray(65536)
        memory[output:output + 160] = b'\xa5' * 160
        source = shadow + raw
        memory[source:source + 16] = rng.randbytes(16)
        palette = rng.randrange(8)
        memory[source + 8] = top
        memory[source + 11] = (memory[source + 11] & 0xf8) | palette
        slot = 0 if local else rng.randrange(1, 13)
        origin = descriptor + slot * 8 + 6
        memory[origin] = 0xc4
        memory[origin + 1] = raw
        memory[index], memory[length], memory[bindings + palette] = slot, size, occupied
        row = lines + 16 + (top >> 4)
        memory[row] = row_count
        memory[0xff44] = ly
        expected = bytearray(memory)
        eligible = 0 < top < 160 and not occupied
        if eligible and local:
            expected[used] |= 1 << palette
        admitted = eligible and row_count < 5 and size <= 144 and (local or ly < 140)
        busy = eligible and row_count < 5 and size <= 144 and not local and ly >= 140
        if admitted:
            expected[row] += 1
            expected[output + size:output + size + 16] = memory[source:source + 16]
            expected[length] += 16
        registers = {key: 0 for key in 'abcdehl'}
        registers['h'], registers['l'] = origin >> 8, origin & 255
        pc, zero, carry, cycles = entry, False, False, 0

        def pair(name):
            return registers[name[0]] * 256 + registers[name[1]]

        def read(address):
            return rom[bank * 0x4000 + address - 0x4000] if 0x4000 <= address < 0x8000 else memory[address]

        def fetch():
            nonlocal pc
            value = read(pc)
            pc += 1
            return value

        for _ in range(300):
            op = fetch()
            if 0x40 <= op < 0x80 and op != 0x76:
                source_index, target = op & 7, op >> 3 & 7
                value = read(pair('hl')) if source_index == 6 else registers['bcdehl?a'[source_index]]
                if target == 6:
                    memory[pair('hl')] = value
                else:
                    registers['bcdehl?a'[target]] = value
                cycles += 8 if 6 in (source_index, target) else 4
            elif op == 0x21:
                registers['l'], registers['h'] = fetch(), fetch()
                cycles += 12
            elif op & 0xc7 == 0x06:
                registers['bcdehl?a'[op >> 3 & 7]] = fetch()
                cycles += 8
            elif op in (0x09, 0x19):
                value = pair('hl') + pair('bc' if op == 0x09 else 'de')
                registers['h'], registers['l'] = value >> 8 & 255, value & 255
                carry = value > 65535
                cycles += 8
            elif op == 0xfa:
                registers['a'] = read(fetch() | fetch() << 8)
                cycles += 16
            elif op == 0xea:
                memory[fetch() | fetch() << 8] = registers['a']
                cycles += 16
            elif op == 0xf0:
                registers['a'] = memory[0xff00 + fetch()]
                cycles += 12
            elif op == 0x2a:
                registers['a'] = read(pair('hl'))
                value = pair('hl') + 1
                registers['h'], registers['l'] = value >> 8 & 255, value & 255
                cycles += 8
            elif op == 0x12:
                memory[pair('de')] = registers['a']
                cycles += 8
            elif op & 0xc7 == 0x04:
                target = op >> 3 & 7
                if target == 6:
                    memory[pair('hl')] = (read(pair('hl')) + 1) & 255
                    zero = not read(pair('hl'))
                else:
                    key = 'bcdehl?a'[target]
                    registers[key] = (registers[key] + 1) & 255
                    zero = not registers[key]
                cycles += 12 if target == 6 else 4
            elif op == 0xcb:
                require(fetch() == 0x37, 'Unexpected raw rectangle bit instruction')
                a = registers['a']
                registers['a'] = (a >> 4) | ((a & 15) << 4)
                zero, carry = not registers['a'], False
                cycles += 8
            elif op in (0xe6, 0xc6, 0xfe, 0xa7, 0xb6):
                a = registers['a']
                value = fetch() if op in (0xe6, 0xc6, 0xfe) else a if op == 0xa7 else read(pair('hl'))
                result = a & value if op in (0xe6, 0xa7) else (a + value) & 255 if op == 0xc6 else a | value if op == 0xb6 else (a - value) & 255
                carry = a + value > 255 if op == 0xc6 else a < value if op == 0xfe else False
                zero = not result
                if op != 0xfe:
                    registers['a'] = result
                cycles += 4 if op == 0xa7 else 8
            elif op in (0x18, 0x28, 0x30, 0x20):
                delta = fetch()
                taken = op == 0x18 or (zero if op == 0x28 else not carry if op == 0x30 else not zero)
                if taken:
                    pc += delta if delta < 128 else delta - 256
                cycles += 12 if taken else 8
            elif op == 0x37:
                carry = True
                cycles += 4
            elif op in (0xc9, 0xc0, 0xc8, 0xd0):
                taken = op == 0xc9 or (not zero if op == 0xc0 else zero if op == 0xc8 else not carry)
                cycles += 16 if op == 0xc9 else 20 if taken else 8
                if taken:
                    require(carry == busy and memory == expected,
                            f'Raw copy changed native bytes, escaped its bounds or changed admission: raw={raw}, local={local}, top={top}, length={size}')
                    cases += 1
                    maximum = max(maximum, cycles)
                    break
            else:
                raise ValueError(f'Unexpected raw rectangle opcode {op:02x}')
        else:
            raise ValueError('Direct raw rectangle copy did not terminate')
    require(maximum <= 1100, 'Raw native copy exceeded its idle group bound')
    print(f'Emitted direct native OAM copies: {cases} cases, max {maximum} cycles')
