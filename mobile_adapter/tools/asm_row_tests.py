"""Execute production quad row accounting and rollback without an emulator.

The instruction subset deliberately has no LCD or game engine. The independent
oracle clips each hardware object's eight rows against the visible screen.
"""
import random


def verify_rows(rom, symbols, require):
    bank, count_entry = symbols['MobileVirtualAdmitGroup.count']
    undo_entry = symbols['MobileVirtualAdmitGroup.undo'][1]
    group = symbols['wMobileVirtualScratch'][1]
    length = group + 64 + 8 + 80
    rejected = length + 1
    lines = symbols['wMobileActorTiles1'][1] + 13 * 64 + 160

    def execute(entry, memory):
        registers = {key: 0 for key in 'abcdehl'}
        registers['h'], registers['l'] = group >> 8, group & 255
        pc, zero, carry, cycles, stack = entry, False, False, 0, []

        def pair(name):
            return registers[name[0]] * 256 + registers[name[1]]

        def put_pair(name, value):
            registers[name[0]], registers[name[1]] = value >> 8 & 255, value & 255

        def read(address):
            if address < 0x4000:
                return rom[address]
            if address < 0x8000:
                return rom[bank * 0x4000 + address - 0x4000]
            return memory[address]

        def write(address, value):
            require(address == rejected or lines <= address < lines + 144,
                    f'Quad accounting wrote outside its private rows: {address:04x}, pc={pc:04x}, length={memory[length]}')
            memory[address] = value

        def fetch():
            nonlocal pc
            value = read(pc)
            pc += 1
            return value

        def value(index):
            return read(pair('hl')) if index == 6 else registers['bcdehl?a'[index]]

        def put(index, data):
            if index == 6:
                write(pair('hl'), data)
            else:
                registers['bcdehl?a'[index]] = data

        for _ in range(10000):
            opcode = fetch()
            if 0x40 <= opcode < 0x80 and opcode != 0x76:
                source, destination = opcode & 7, opcode >> 3 & 7
                put(destination, value(source))
                cycles += 8 if 6 in (source, destination) else 4
            elif opcode in (0x01, 0x11, 0x21):
                put_pair({0x01: 'bc', 0x11: 'de', 0x21: 'hl'}[opcode], fetch() | fetch() << 8)
                cycles += 12
            elif opcode & 0xc7 == 0x06:
                put(opcode >> 3 & 7, fetch())
                cycles += 12 if opcode == 0x36 else 8
            elif opcode in (0xfa, 0xea):
                address = fetch() | fetch() << 8
                if opcode == 0xfa:
                    registers['a'] = read(address)
                else:
                    write(address, registers['a'])
                cycles += 16
            elif opcode == 0x22:
                write(pair('hl'), registers['a'])
                put_pair('hl', pair('hl') + 1)
                cycles += 8
            elif opcode & 0xc7 in (0x04, 0x05):
                index = opcode >> 3 & 7
                data = (value(index) + (1 if opcode & 1 == 0 else -1)) & 255
                put(index, data)
                zero = data == 0
                cycles += 12 if index == 6 else 4
            elif opcode in (0x23, 0x2b):
                put_pair('hl', pair('hl') + (1 if opcode == 0x23 else -1))
                cycles += 8
            elif opcode in (0x09, 0x19):
                result = pair('hl') + pair('bc' if opcode == 0x09 else 'de')
                carry = result > 65535
                put_pair('hl', result)
                cycles += 8
            elif opcode == 0x0f:
                a = registers['a']
                registers['a'] = a >> 1 | (a & 1) << 7
                zero, carry = False, bool(a & 1)
                cycles += 4
            elif opcode in (0xa7, 0xaf, 0x81, 0xc6, 0xd6) or 0x90 <= opcode <= 0x97 or 0xb8 <= opcode <= 0xbf or opcode == 0xfe:
                a = registers['a']
                immediate = opcode in (0xc6, 0xd6, 0xfe)
                operand = fetch() if immediate else value(opcode & 7)
                if opcode == 0xa7:
                    result, carry = a, False
                elif opcode == 0xaf:
                    result, carry = 0, False
                elif opcode in (0x81, 0xc6):
                    result, carry = (a + operand) & 255, a + operand > 255
                else:
                    result, carry = (a - operand) & 255, a < operand
                zero = result == 0
                if opcode < 0xb8 or opcode in (0xc6, 0xd6):
                    registers['a'] = result
                cycles += 8 if immediate or opcode == 0xbe else 4
            elif opcode in (0xc5, 0xd5, 0xe5, 0xc1, 0xd1, 0xe1):
                name = {0xc0: 'bc', 0xd0: 'de', 0xe0: 'hl'}[opcode & 0xf0]
                if opcode & 4:
                    stack.append(pair(name))
                    cycles += 16
                else:
                    put_pair(name, stack.pop())
                    cycles += 12
            elif opcode in (0x18, 0x20, 0x28, 0x30, 0x38, 0xc2, 0xc3, 0xca, 0xd2, 0xda):
                delta = fetch()
                absolute = opcode >= 0xc0
                target = delta | fetch() << 8 if absolute else pc + (delta if delta < 128 else delta - 256)
                taken = {0x18: True, 0x20: not zero, 0x28: zero, 0x30: not carry, 0x38: carry,
                         0xc2: not zero, 0xc3: True, 0xca: zero, 0xd2: not carry, 0xda: carry}[opcode]
                if taken:
                    pc = target
                cycles += (16 if taken else 12) if absolute else (12 if taken else 8)
            elif opcode == 0xc9:
                require(not stack, 'Quad accounting left an unbalanced stack')
                return cycles + 16
            else:
                raise ValueError(f'Unexpected emitted row-accounting opcode {opcode:02x}')
        raise ValueError('Quad accounting failed to terminate')

    rng = random.Random(0x4f414d)
    groups = [[y] for y in range(256)]
    groups += [[y, y, (y + 8) & 255, (y + 8) & 255] for y in range(256)]
    groups += [[(y + 8) & 255, (y + 8) & 255, y, y] for y in range(256)]
    groups += [[y] * size for size in range(1, 17) for y in (0, 8, 9, 15, 16, 76, 152, 153, 159, 160, 255)]
    groups += [[rng.randrange(256) for _ in range(rng.randrange(1, 17))] for _ in range(1200)]
    maximum = 0
    for ys in groups:
        memory = bytearray(65536)
        baseline = [rng.randrange(10) for _ in range(144)]
        memory[lines:lines + 144] = bytes(baseline)
        memory[length] = len(ys) * 4
        for index, y in enumerate(ys):
            memory[group + index * 4] = y
        expected = [count + sum(y - 16 <= row < y - 8 for y in ys) for row, count in enumerate(baseline)]
        cycles = execute(count_entry, memory)
        require(list(memory[lines:lines + 144]) == expected, f'Emitted quad clipping disagrees: {ys}')
        require(bool(memory[rejected]) == any(count > 10 for count in expected), 'Wrong quad pressure flag')
        cycles += execute(undo_entry, memory)
        require(list(memory[lines:lines + 144]) == baseline, 'Rejected figure did not roll back exactly')
        maximum = max(maximum, cycles)
    # Generic admission now starts below LY 110. At double speed that leaves
    # at least 34 * 912 cycles before LY 144, including 9,000 caller cycles.
    require(maximum <= 22000, f'Quad count and rollback exceed the generic idle-cycle bound: {maximum}')
    print(f'Emitted quad instructions: {len(groups)} cases, count plus rollback max {maximum} cycles')
