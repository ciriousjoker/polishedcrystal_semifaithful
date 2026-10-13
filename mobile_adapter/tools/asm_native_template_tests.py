"""Execute emitted native-facing transforms without an LCD or game engine."""
import random


def verify_native_templates(rom, symbols, require, mode="generic"):
    entry_name = {"generic": "MobileVirtualWriteNativeQuads", "private": "MobileVirtualWritePrivateRectangle",
                  "descriptor": "MobileVirtualWriteDescriptorRectangle", "ordinary": "MobileVirtualWriteOrdinaryRectangle"}[mode]
    bank, entry = symbols[entry_name]
    template = symbols['wMobileVirtualScratch'][1] + 64 + 8
    group_length = template + 80
    group_pointer = symbols['wMobileVirtualGroupPointer'][1]
    output = (symbols['wMobileActorTiles1'][1] + 13 * 4 + 255) & 0xff00
    native_bank, table = symbols['Facings']
    native_body = symbols['FacingStepDown0'][1]
    copy_bank, copy_body = symbols['MobileCopiedFacingStepDown0']
    if mode == 'generic':
        require(bank == copy_bank, 'Native mirror is not directly readable by the transform')
    count = (native_body - table) // 2 - 1
    rng = random.Random(0x4f414d)
    cases = maximum = 0
    for facing in range(count):
        offset = native_bank * 0x4000 + table - 0x4000 + facing * 2
        pointer = int.from_bytes(rom[offset:offset + 2], 'little')
        original = native_bank * 0x4000 + pointer - 0x4000
        quads = rom[original]
        if mode != "generic" and quads != 4:
            continue
        source = pointer + copy_body - native_body + 1
        for _ in range(32):
            origin_x, origin_y = rng.randrange(256), rng.randrange(256)
            tile_base, attributes, relative = rng.randrange(256), rng.randrange(256), rng.randrange(256)
            destination = output + rng.randrange(0, 160 - quads * 4 + 1, 4)
            expected = bytearray()
            for quad in range(quads):
                y, x, flags, tile = rom[original + 1 + quad * 4:original + 5 + quad * 4]
                tile = ((0 if flags & 4 else tile_base) + tile) & 255
                if attributes & 8 and tile >= 128:
                    tile -= 64
                color = (((relative if flags & 2 else 0) | flags) & 0xe0) | attributes
                if flags & 8:
                    color = (color + 1) & 255
                expected += bytes(((origin_y + y) & 255, (origin_x + x) & 255, tile, color))
            if mode == "ordinary":
                expected = b"".join(expected[index:index + 4] for index in range(12, -1, -4))
            memory = bytearray(65536)
            return_bank = native_bank if mode == 'ordinary' else rng.choice((copy_bank, copy_bank + 5))
            memory[symbols['wMobileVirtualRectangleReturnBank'][1]] = return_bank
            memory[template:template + 4] = bytes((origin_x, origin_y, tile_base, attributes))
            memory[template + 7] = relative
            memory[group_length] = quads * 4
            memory[group_pointer:group_pointer + 2] = destination.to_bytes(2, 'little')
            registers = {key: 0 for key in 'abcdehl'}
            if mode == 'descriptor':
                descriptor = symbols['wMobileNativeOriginals'][1] + rng.randrange(13) * 8
                memory[descriptor:descriptor + 8] = bytes((origin_x, origin_y, tile_base, attributes,
                    source & 255, source >> 8, 0x44, relative))
                memory[template:template + 4] = bytes(value ^ 255 for value in (origin_x, origin_y, tile_base, attributes))
                memory[template + 7] = relative ^ 255
                registers['b'], registers['c'] = descriptor >> 8, descriptor & 255
            if mode == "ordinary":
                source = pointer + 1
                for name, value in (("hCurSpriteXPixel", origin_x), ("hCurSpriteYPixel", origin_y),
                                    ("hCurSpriteTile", tile_base), ("hCurSpriteOAMFlags", relative)):
                    memory[symbols[name][1]] = value
                registers['b'], registers['c'] = destination >> 8, (destination + 12) & 255
                registers['d'] = attributes
            else:
                registers['d'], registers['e'] = destination >> 8, destination & 255
            registers['h'], registers['l'] = source >> 8, source & 255
            pc, zero, carry, cycles, stack = entry, False, False, 0, []

            def pair(name):
                return registers[name[0]] * 256 + registers[name[1]]

            def read(address):
                if address < 0x4000:
                    return rom[address]
                if address < 0x8000:
                    return rom[bank * 0x4000 + address - 0x4000]
                return memory[address]

            def write(address, value):
                require(destination <= address < destination + len(expected) or
                        (mode == 'descriptor' and address == template + 7) or
                        (mode != 'generic' and address in (symbols['hUsedSpriteTile'][1], symbols['hROMBank'][1], 0x2000)),
                        'Native transform wrote outside its admitted private output')
                memory[address] = value

            def fetch():
                nonlocal pc
                value = read(pc)
                pc += 1
                return value

            for _ in range(3000):
                op = fetch()
                if 0x40 <= op < 0x80 and op != 0x76:
                    source_index, target_index = op & 7, op >> 3 & 7
                    value = read(pair('hl')) if source_index == 6 else registers['bcdehl?a'[source_index]]
                    if target_index == 6:
                        write(pair('hl'), value)
                    else:
                        registers['bcdehl?a'[target_index]] = value
                    cycles += 8 if 6 in (source_index, target_index) else 4
                elif op in (0x01, 0x11, 0x21):
                    value = fetch() | fetch() << 8
                    name = {0x01: 'bc', 0x11: 'de', 0x21: 'hl'}[op]
                    registers[name[0]], registers[name[1]] = value >> 8, value & 255
                    cycles += 12
                elif op & 0xc7 == 0x06:
                    registers['bcdehl?a'[op >> 3 & 7]] = fetch()
                    cycles += 8
                elif op in (0xc5, 0xd5, 0xe5):
                    stack.append(pair({0xc5: 'bc', 0xd5: 'de', 0xe5: 'hl'}[op]))
                    cycles += 16
                elif op in (0xc1, 0xd1, 0xe1):
                    value = stack.pop()
                    name = {0xc1: 'bc', 0xd1: 'de', 0xe1: 'hl'}[op]
                    registers[name[0]], registers[name[1]] = value >> 8, value & 255
                    cycles += 12
                elif op in (0x09, 0x29):
                    value = pair('hl') + pair('bc' if op == 0x09 else 'hl')
                    carry = value > 65535
                    registers['h'], registers['l'] = value >> 8 & 255, value & 255
                    cycles += 8
                elif op == 0xe9:
                    pc = pair('hl')
                    cycles += 4
                elif op == 0x02:
                    write(pair('bc'), registers['a'])
                    cycles += 8
                elif op in (0xf0, 0xe0):
                    address = 0xff00 + fetch()
                    if op == 0xf0:
                        registers['a'] = read(address)
                    else:
                        write(address, registers['a'])
                    cycles += 12
                elif op & 0xc7 == 0x04:
                    name = 'bcdehl?a'[op >> 3 & 7]
                    registers[name] = (registers[name] + 1) & 255
                    zero = not registers[name]
                    cycles += 4
                elif op == 0xea:
                    write(fetch() | fetch() << 8, registers['a'])
                    cycles += 16
                elif op in (0xc3, 0xc2, 0xca, 0xd2, 0xda):
                    target = fetch() | fetch() << 8
                    taken = {0xc3: True, 0xc2: not zero, 0xca: zero,
                             0xd2: not carry, 0xda: carry}[op]
                    if taken:
                        pc = target
                        if target == symbols['Bankswitch'][1]:
                            require(registers['a'] == return_bank,
                                    'Rectangle did not restore its source ROM bank')
                        else:
                            require(0x4000 <= target < 0x8000, 'Rectangle jump escaped its upper program bank')
                    cycles += 16 if taken else 12
                elif op == 0xfa:
                    registers['a'] = read(fetch() | fetch() << 8)
                    cycles += 16
                elif op == 0x2a:
                    registers['a'] = read(pair('hl'))
                    address = pair('hl') + 1
                    registers['h'], registers['l'] = address >> 8, address & 255
                    cycles += 8
                elif op == 0x12:
                    write(pair('de'), registers['a'])
                    cycles += 8
                elif op == 0x13:
                    address = pair('de') + 1
                    registers['d'], registers['e'] = address >> 8, address & 255
                    cycles += 8
                elif op == 0x23:
                    address = pair('hl') + 1
                    registers['h'], registers['l'] = address >> 8, address & 255
                    cycles += 8
                elif op == 0x3c:
                    registers['a'] = (registers['a'] + 1) & 255
                    zero = not registers['a']
                    cycles += 4
                elif op == 0xcb:
                    bit = fetch()
                    require(bit & 0xc0 == 0x40 and bit & 7 != 6, 'Unexpected native transform bit instruction')
                    zero = not (registers['bcdehl?a'[bit & 7]] & (1 << (bit >> 3 & 7)))
                    cycles += 8
                elif op in (0xe6, 0xd6, 0xfe, 0xc6, 0xf6) or 0x80 <= op <= 0x87 or 0x90 <= op <= 0x97 or 0xb0 <= op <= 0xb7 or op == 0xaf or 0xb8 <= op <= 0xbf:
                    a = registers['a']
                    immediate = op in (0xe6, 0xd6, 0xfe, 0xc6, 0xf6)
                    operand = fetch() if immediate else (read(pair('hl')) if op & 7 == 6 else registers['bcdehl?a'[op & 7]])
                    if op == 0xe6:
                        value, carry = a & operand, False
                    elif op == 0xf6:
                        value, carry = a | operand, False
                    elif op == 0xaf:
                        value, carry = 0, False
                    elif 0xb0 <= op <= 0xb7:
                        value, carry = a | operand, False
                    elif 0x80 <= op <= 0x87 or op == 0xc6:
                        value, carry = (a + operand) & 255, a + operand > 255
                    else:
                        value, carry = (a - operand) & 255, a < operand
                    zero = not value
                    if not (0xb8 <= op <= 0xbf or op == 0xfe):
                        registers['a'] = value
                    cycles += 8 if immediate or op & 7 == 6 else 4
                elif op in (0x18, 0x20, 0x28, 0x30, 0x38):
                    delta = fetch()
                    taken = {0x18: True, 0x20: not zero, 0x28: zero, 0x30: not carry, 0x38: carry}[op]
                    if taken:
                        pc += delta if delta < 128 else delta - 256
                    cycles += 12 if taken else 8
                elif op == 0xc9:
                    require(memory[destination:destination + len(expected)] == expected,
                            f'Native facing {facing} transform differs from unchanged native data')
                    require(not stack, 'Native rectangle program left an unbalanced stack')
                    if mode == 'ordinary':
                        require(registers['b'] == destination >> 8 and registers['c'] == (destination - 4) & 255,
                                'Ordinary rectangle changed native OAM cursor semantics')
                        require(registers['d'] == attributes and memory[symbols['hUsedSpriteTile'][1]] == 0,
                                'Ordinary rectangle changed native flags/count semantics')
                    else:
                        require(pair('de') == destination + len(expected), 'Native transform output length differs')
                    if mode != 'generic':
                        require(pair('hl') == source + len(expected), 'Rectangle source cursor differs')
                    cases += 1
                    maximum = max(maximum, cycles + 16)
                    break
                else:
                    raise ValueError(f'Unexpected emitted native transform opcode {op:02x}')
            else:
                raise ValueError('Native transform failed to terminate')
    require(maximum <= (7000 if mode == 'generic' else 1200),
            'Native template transform exceeds its guarded budget')
    print(f'Emitted native {mode} transforms: {cases} cases, maximum {maximum} cycles')
    if mode == 'generic':
        for program in ('private', 'descriptor', 'ordinary'):
            verify_native_templates(rom, symbols, require, program)
