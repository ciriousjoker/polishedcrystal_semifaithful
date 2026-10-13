"""Execute emitted rectangle admission instructions without an emulator.

This small instruction runner has no LCD, game engine or rewritten admission
predicate. The oracle independently counts selected hardware objects per row.
"""
import random


def verify_rectangles(rom, symbols, require):
    bank, entry = symbols['MobileVirtualAdmitRectangleTop']
    top_address = symbols['wMobileVirtualRectangleTop'][1]
    count_address = symbols['wMobileVirtualRectangleCount'][1]
    lines = ((symbols['wMobileActorTiles1'][1] + 13 * 4 + 255) & 0xff00) + 160

    def execute(tops, candidate, origin=0, aligned=False):
        memory = bytearray(65536)
        memory[top_address] = candidate
        memory[count_address] = len(tops)
        memory[lines + 15] = origin
        memory[symbols['wMobileVirtualForceFull'][1]] = int(aligned)
        for index, top in enumerate(tops):
            memory[lines + index] = top
        for row in range(144):
            count = sum(top <= row + 16 < top + 16 for top in tops)
            for plane in range(3):
                memory[lines + 64 + row // 8 * 3 + plane] |= ((count >> plane) & 1) << (row % 8)
        before_planes = bytes(memory[lines + 64:lines + 127])
        if aligned:
            for bucket in range(10):
                left, right = max(16, origin + bucket * 16), min(160, origin + (bucket + 1) * 16)
                memory[lines + 16 + bucket] = sum(max(left, top) < min(right, top + 16) for top in tops)
        before_bins = bytes(memory[lines + 16:lines + 27])
        registers = {key: 0 for key in 'abcdehl'}
        pc, zero, carry, cycles, stack = entry, False, False, 0, []

        def read(address):
            if address < 0x4000:
                return rom[address]
            if address < 0x8000:
                return rom[bank * 0x4000 + address - 0x4000]
            return memory[address]

        def fetch():
            nonlocal pc
            value = read(pc)
            pc += 1
            return value

        def pair(name):
            return registers[name[0]] * 256 + registers[name[1]]

        def put_pair(name, value):
            registers[name[0]], registers[name[1]] = value >> 8 & 255, value & 255

        def value(index):
            return read(pair('hl')) if index == 6 else registers['bcdehl?a'[index]]

        def put(index, data):
            if index == 6:
                memory[pair('hl')] = data
            else:
                registers['bcdehl?a'[index]] = data

        for _ in range(3000):
            opcode = fetch()
            if 0x40 <= opcode < 0x80 and opcode != 0x76:
                source, destination = opcode & 7, opcode >> 3 & 7
                put(destination, value(source))
                cycles += 8 if 6 in (source, destination) else 4
            elif opcode in (0x01, 0x11, 0x21):
                immediate = fetch() | fetch() << 8
                put_pair({0x01: 'bc', 0x11: 'de', 0x21: 'hl'}[opcode], immediate)
                cycles += 12
            elif opcode & 0xc7 == 0x06:
                put(opcode >> 3 & 7, fetch())
                cycles += 12 if opcode == 0x36 else 8
            elif opcode == 0xea:
                memory[fetch() | fetch() << 8] = registers['a']
                cycles += 16
            elif opcode == 0xfa:
                registers['a'] = read(fetch() | fetch() << 8)
                cycles += 16
            elif opcode in (0x22, 0x2a):
                address = pair('hl')
                if opcode == 0x22:
                    memory[address] = registers['a']
                else:
                    registers['a'] = read(address)
                put_pair('hl', address + 1)
                cycles += 8
            elif opcode in (0x04, 0x05, 0x14, 0x15, 0x34, 0x35):
                index = opcode >> 3 & 7
                data = (value(index) + (1 if opcode & 1 == 0 else -1)) & 255
                put(index, data)
                zero = data == 0
                cycles += 12 if index == 6 else 4
            elif opcode in (0x23, 0x2b):
                put_pair('hl', pair('hl') + (1 if opcode == 0x23 else -1))
                cycles += 8
            elif opcode == 0x19:
                result = pair('hl') + pair('de')
                carry = result > 65535
                put_pair('hl', result)
                cycles += 8
            elif opcode == 0xcb:
                require(fetch() == 0x37, 'Unexpected rectangle CB instruction')
                a = registers['a']
                registers['a'] = (a << 4 | a >> 4) & 255
                zero, carry = registers['a'] == 0, False
                cycles += 8
            elif opcode in (0xe6, 0xa7, 0xaf, 0x81, 0xc6, 0x90, 0x92, 0xfe, 0xb0, 0xa0, 0xa2, 0xa3, 0xaa, 0xab):
                a = registers['a']
                if opcode in (0xe6, 0xc6, 0xfe):
                    operand = fetch()
                elif opcode == 0x81:
                    operand = registers['c']
                elif opcode in (0x90, 0x92):
                    operand = registers['b' if opcode == 0x90 else 'd']
                elif opcode in (0xb0, 0xa0, 0xa2, 0xa3, 0xaa, 0xab):
                    operand = registers[{0xb0: 'b', 0xa0: 'b', 0xa2: 'd', 0xa3: 'e', 0xaa: 'd', 0xab: 'e'}[opcode]]
                else:
                    operand = a
                if opcode in (0xe6, 0xa7, 0xa0, 0xa2, 0xa3):
                    result, carry = a & operand, False
                elif opcode == 0xb0:
                    result, carry = a | operand, False
                elif opcode in (0xaa, 0xab):
                    result, carry = a ^ operand, False
                elif opcode == 0xaf:
                    result, carry = 0, False
                elif opcode in (0x81, 0xc6):
                    result, carry = (a + operand) & 255, a + operand > 255
                else:
                    result, carry = (a - operand) & 255, a < operand
                zero = result == 0
                if opcode != 0xfe:
                    registers['a'] = result
                cycles += 8 if opcode in (0xe6, 0xc6, 0xfe) else 4
            elif opcode in (0xe5, 0xd5, 0xe1, 0xd1):
                name = 'hl' if opcode & 0x30 == 0x20 else 'de'
                if opcode & 4:
                    stack.append(pair(name))
                    cycles += 16
                else:
                    put_pair(name, stack.pop())
                    cycles += 12
            elif opcode in (0x18, 0x20, 0x28, 0x30, 0x38, 0xc2, 0xc3, 0xd2):
                delta = fetch()
                if opcode in (0xc2, 0xc3, 0xd2):
                    target = delta | fetch() << 8
                else:
                    target = pc + (delta if delta < 128 else delta - 256)
                taken = {0x18: True, 0x20: not zero, 0x28: zero, 0x30: not carry,
                         0x38: carry, 0xc2: not zero, 0xc3: True, 0xd2: not carry}[opcode]
                if taken:
                    pc = target
                cycles += (16 if taken else 12) if opcode in (0xc2, 0xc3, 0xd2) else (12 if taken else 8)
            elif opcode == 0x37:
                carry = True
                cycles += 4
            elif opcode == 0xcd:
                target = fetch() | fetch() << 8
                stack.append(("call", pc))
                pc = target
                cycles += 24
            elif opcode in (0xc9, 0xd8):
                taken = opcode == 0xc9 or carry
                if not taken:
                    cycles += 8
                    continue
                cycles += 16 if opcode == 0xc9 else 20
                if stack and isinstance(stack[-1], tuple):
                    pc = stack.pop()[1]
                    continue
                require(not stack, 'Rectangle admission left an unbalanced stack')
                if carry:
                    require(bytes(memory[lines + 64:lines + 127]) == before_planes,
                            'Rejected rectangle changed admitted hardware row counters')
                    require(bytes(memory[lines + 16:lines + 27]) == before_bins,
                            'Rejected aligned rectangle changed admitted hardware bins')
                return not carry, memory, cycles
            else:
                raise ValueError(f'Unexpected emitted rectangle opcode {opcode:02x}')
        raise ValueError('Rectangle admission failed to terminate')

    def valid(tops):
        return all(sum(top <= row < top + 16 for top in tops) <= 5 for row in range(16, 160))

    seeds = [[], [0] * 5, [159] * 5, [76] * 5,
             [1, 17, 33, 49, 65, 81, 97, 113, 129], [20, 28, 36, 44, 52],
             [16, 31, 32, 47, 48, 63, 64, 79, 80]]
    rng = random.Random(0x5043)
    for _ in range(80):
        tops = []
        for _ in range(rng.randrange(1, 11)):
            candidate = rng.randrange(160)
            if valid(tops + [candidate]):
                tops.append(candidate)
        seeds.append(tops)
    maximum = fast_maximum = 0
    cases = 0
    vectors = [(tops, 0) for tops in seeds]
    vectors += [(tops, origin) for tops in seeds[:7] for origin in range(1, 16)]
    for tops, origin in vectors:
        require(valid(tops), 'Invalid independently generated admitted list')
        for candidate in range(160):
            admitted, memory, cycles = execute(tops, candidate, origin)
            expected = len(tops) < 10 and valid(tops + [candidate])
            require(admitted == expected, f'Emitted rectangle admission disagrees at {tops}, {candidate}')
            require(memory[count_address] == len(tops) + int(admitted), 'Wrong admission count')
            require(list(memory[lines:lines + len(tops)]) == tops, 'Admission changed a previous figure')
            if admitted:
                require(memory[lines + len(tops)] == candidate, 'Admitted figure was not recorded')
            for row in range(144):
                count = sum(top <= row + 16 < top + 16 for top in tops + ([candidate] if admitted else []))
                actual = sum(((memory[lines + 64 + row // 8 * 3 + plane] >> (row % 8)) & 1) << plane
                             for plane in range(3))
                require(actual == count, f'Wrong exact row count at row {row}')
            maximum = max(maximum, cycles)
            if not tops:
                fast_maximum = max(fast_maximum, cycles)
            cases += 1
    # A single guarded native-group job starting below LY 137 must return well
    # before LY 144 at CGB double speed (912 CPU cycles per line).
    require(maximum <= 1300, 'Rectangle admission exceeds its idle-cycle bound')
    print(f'Emitted rectangle instructions: {cases} cases, max {maximum} cycles, empty fast path {fast_maximum}')
    aligned_cases = aligned_maximum = 0
    for origin in range(16):
        for _ in range(16):
            native = []
            for _ in range(rng.randrange(11)):
                top = origin + 16 * rng.randrange(10)
                if valid(native + [top]):
                    native.append(top)
            guest = rng.randrange(160)
            inputs = [(native, range(160))]
            if len(native) < 10 and valid(native + [guest]):
                inputs.append((native + [guest], range(origin, 160, 16)))
            for tops, candidates in inputs:
                for candidate in candidates:
                    admitted, memory, cycles = execute(tops, candidate, origin, aligned=True)
                    expected = len(tops) < 10 and valid(tops + [candidate])
                    require(admitted == expected,
                            f'Aligned proof disagrees at origin {origin}, {tops}, {candidate}')
                    require(memory[count_address] == len(tops) + int(admitted), 'Wrong aligned admission count')
                    for bucket in range(10):
                        left, right = max(16, origin + bucket * 16), min(160, origin + (bucket + 1) * 16)
                        count = sum(max(left, top) < min(right, top + 16)
                                    for top in tops + ([candidate] if admitted else []))
                        if left < right:
                            require(memory[lines + 16 + bucket] == count,
                                    f'Wrong visible aligned bin {bucket}: origin {origin}, {tops}, {candidate}, {admitted}')
                    aligned_cases += 1
                    aligned_maximum = max(aligned_maximum, cycles)
    require(aligned_maximum <= 600, 'Aligned admission exceeds its guarded idle budget')
    print(f'Emitted aligned admission: {aligned_cases} cases, max {aligned_maximum} cycles')
