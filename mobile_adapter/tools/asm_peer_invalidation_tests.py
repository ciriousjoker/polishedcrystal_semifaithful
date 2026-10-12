"""Execute the production peer invalidation routine without an LCD or emulator."""
import random


def verify_peer_invalidation(rom, symbols, require):
    bank, entry = symbols['MobileInvalidatePeer']
    source = symbols['wMobileApp'][1] + 13
    token = symbols['wMobileVirtualPreviousOAM'][1]
    stage = symbols['wMobileNativePaletteOwners'][1] + 4
    critical = set(range(4)) | set(range(6, 12)) | {17, 18}
    rng = random.Random(0x70656572)
    cases = maximum = 0

    for peer in range(1, 4):
        destination = symbols[f'wMobilePeer{peer}'][1]
        draw_valid = destination + 24
        visible = destination + 27
        for field in range(-1, 20):
            for _ in range(32):
                memory = bytearray(rng.randbytes(65536))
                record = rng.randbytes(20)
                memory[source:source + 20] = record
                memory[destination:destination + 20] = record
                if field >= 0:
                    memory[source + field] ^= rng.randrange(1, 256)
                memory[draw_valid] = memory[visible] = memory[token] = 1
                memory[stage] = rng.randrange(1, 9)
                expected = bytearray(memory)
                if field in critical:
                    expected[draw_valid] = expected[visible] = expected[token] = expected[stage] = 0

                registers = {'a': 0x57, 'b': 0xa5, 'c': 0x5a,
                             'd': destination >> 8, 'e': destination & 255,
                             'h': source >> 8, 'l': source & 255}
                pc, zero, carry, cycles, stack = entry, False, False, 0, []

                def pair(name):
                    return registers[name[0]] * 256 + registers[name[1]]

                def put_pair(name, value):
                    registers[name[0]], registers[name[1]] = value >> 8 & 255, value & 255

                def fetch():
                    nonlocal pc
                    value = rom[bank * 0x4000 + pc - 0x4000]
                    pc += 1
                    return value

                for _ in range(300):
                    op = fetch()
                    if op in (0xc5, 0xd5, 0xe5, 0xc1, 0xd1, 0xe1):
                        name = {0xc5: 'bc', 0xd5: 'de', 0xe5: 'hl',
                                0xc1: 'bc', 0xd1: 'de', 0xe1: 'hl'}[op]
                        if op & 4:
                            stack.append(pair(name))
                            cycles += 16
                        else:
                            put_pair(name, stack.pop())
                            cycles += 12
                    elif op in (0x01, 0x11, 0x21):
                        put_pair({0x01: 'bc', 0x11: 'de', 0x21: 'hl'}[op], fetch() | fetch() << 8)
                        cycles += 12
                    elif op in (0x09, 0x19):
                        total = pair('hl') + pair('bc' if op == 0x09 else 'de')
                        carry = total > 65535
                        put_pair('hl', total)
                        cycles += 8
                    elif op in (0x13, 0x23):
                        name = 'de' if op == 0x13 else 'hl'
                        put_pair(name, pair(name) + 1)
                        cycles += 8
                    elif op == 0x1a:
                        registers['a'] = memory[pair('de')]
                        cycles += 8
                    elif op == 0xbe:
                        a, value = registers['a'], memory[pair('hl')]
                        zero, carry = a == value, a < value
                        cycles += 8
                    elif op in (0x04, 0x05, 0x14, 0x15):
                        name = 'b' if op < 0x10 else 'd'
                        registers[name] = (registers[name] + (1 if op & 1 == 0 else -1)) & 255
                        zero = registers[name] == 0
                        cycles += 4
                    elif op == 0x06:
                        registers['b'] = fetch()
                        cycles += 8
                    elif op == 0x36:
                        memory[pair('hl')] = fetch()
                        cycles += 12
                    elif op == 0xea:
                        memory[fetch() | fetch() << 8] = registers['a']
                        cycles += 16
                    elif op in (0x7b, 0x5f):
                        registers['a' if op == 0x7b else 'e'] = registers['e' if op == 0x7b else 'a']
                        cycles += 4
                    elif op == 0xc6:
                        a, value = registers['a'], fetch()
                        registers['a'] = (a + value) & 255
                        zero, carry = registers['a'] == 0, a + value > 255
                        cycles += 8
                    elif op == 0xaf:
                        registers['a'], zero, carry = 0, True, False
                        cycles += 4
                    elif op in (0x18, 0x20, 0x28, 0x30):
                        delta = fetch()
                        taken = op == 0x18 or (not zero if op == 0x20 else zero if op == 0x28 else not carry)
                        if taken:
                            pc += delta if delta < 128 else delta - 256
                        cycles += 12 if taken else 8
                    elif op == 0xc9:
                        require(not stack and pair('hl') == source and pair('de') == destination and
                                pair('bc') == 0xa55a, 'Peer invalidation changed caller pointers or stack')
                        require(memory == expected,
                                f'Peer {peer}, changed field {field}: invalidation touched unexpected RAM or missed a stale appearance')
                        maximum = max(maximum, cycles + 16)
                        cases += 1
                        break
                    else:
                        raise ValueError(f'Unexpected peer invalidation opcode {op:02x}')
                else:
                    raise ValueError('Peer invalidation did not terminate')
    require(maximum <= 1600, 'Peer invalidation exceeded the bounded parser budget')
    print(f'Emitted peer invalidation: {cases} cases, maximum {maximum} cycles')
