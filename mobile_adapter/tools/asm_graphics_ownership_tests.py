"""Execute the native VRAM ownership fence without starting an emulator."""
import random


def verify_graphics_ownership(rom, symbols, require):
    rng = random.Random(0x74696c65)
    names = "bcdehl?a"

    def execute(label, memory, registers):
        bank, pc = symbols[label]
        zero, carry = bool(registers['f'] & 128), bool(registers['f'] & 16)
        stack = []

        def pair(name):
            return registers[name[0]] * 256 + registers[name[1]]

        def put_pair(name, value):
            registers[name[0]], registers[name[1]] = value >> 8 & 255, value & 255

        def fetch():
            nonlocal pc
            value = rom[bank * 0x4000 + pc - 0x4000]
            pc += 1
            return value

        def condition(code):
            return (not zero, zero, not carry, carry)[code]

        for _ in range(300):
            op = fetch()
            if 0x40 <= op < 0x80 and op != 0x76:
                source, target = op & 7, op >> 3 & 7
                value = memory[pair('hl')] if source == 6 else registers[names[source]]
                if target == 6:
                    memory[pair('hl')] = value
                else:
                    registers[names[target]] = value
            elif op & 0xcf == 0x01:
                lo, hi = fetch(), fetch()
                put_pair(('bc', 'de', 'hl', 'sp')[op >> 4 & 3], lo | hi << 8)
            elif op & 0xc7 == 0x06:
                registers[names[op >> 3 & 7]] = fetch()
            elif op == 0xfa:
                registers['a'] = memory[fetch() | fetch() << 8]
            elif op == 0xea:
                memory[fetch() | fetch() << 8] = registers['a']
            elif op == 0xf0:
                registers['a'] = memory[0xff00 + fetch()]
            elif op == 0xe0:
                memory[0xff00 + fetch()] = registers['a']
            elif op in (0x19, 0x29):
                value = pair('hl') + pair('de' if op == 0x19 else 'hl')
                carry = value > 65535
                registers['f'] = (registers['f'] & 128) | int(carry) * 16
                put_pair('hl', value)
            elif op in (0xe6, 0xfe, 0xa7, 0xaf) or 0xb8 <= op <= 0xbf:
                value = fetch() if op in (0xe6, 0xfe) else registers['a'] if op in (0xa7, 0xaf) else registers[names[op & 7]]
                a = registers['a']
                comparing = op == 0xfe or 0xb8 <= op <= 0xbf
                result = (a - value) & 255 if comparing else a ^ value if op == 0xaf else a & value
                zero, carry = result == 0, comparing and a < value
                registers['f'] = int(zero) * 128 | int(carry) * 16
                if not comparing:
                    registers['a'] = result
            elif op in (0xc5, 0xd5, 0xe5, 0xf5):
                stack.append(pair(('bc', 'de', 'hl', 'af')[(op - 0xc5) // 16]))
            elif op in (0xc1, 0xd1, 0xe1, 0xf1):
                name = ('bc', 'de', 'hl', 'af')[(op - 0xc1) // 16]
                put_pair(name, stack.pop())
                if name == 'af':
                    zero, carry = bool(registers['f'] & 128), bool(registers['f'] & 16)
            elif op in (0x18, 0x20, 0x28, 0x30, 0x38):
                delta = fetch()
                if op == 0x18 or condition((op - 0x20) // 8):
                    pc += delta if delta < 128 else delta - 256
            elif op in (0xc3, 0xc2, 0xca, 0xd2, 0xda):
                target = fetch() | fetch() << 8
                if op == 0xc3 or condition((op - 0xc2) // 8):
                    pc = target
            elif op == 0xc9:
                require(not stack, 'Ownership routine left native registers on its stack')
                return
            else:
                raise ValueError(f'Unsupported ownership instruction {op:02x}')
        raise ValueError('Ownership routine did not terminate')

    def address(name):
        aliases = {
            'wMobileVirtualStage': ('wMobileNativePaletteOwners', 4),
            'wMobileVirtualWindowOwners': ('wMobileNativePaletteOwners', 0),
            'wMobileVirtualQueuedPeer': ('wMobileSavedFacings', 0),
        }
        base, offset = aliases.get(name, (name, 0))
        return symbols[base][1] + offset

    windows = ((0, 0x8640, 0x86d0), (1, 0x83f0, 0x8400),
               (1, 0x87f0, 0x8800), (1, 0x8ff0, 0x9000))
    cases = 0
    for bank in range(2):
        for destination in range(0x8000, 0xa001, 16):
            for count in (0, 1, 2, 8, 12, 127, 255):
                for online in (0, 1):
                    memory = bytearray(65536)
                    memory[0xff4f], memory[0xff70] = bank, rng.randrange(1, 8)
                    memory[address('wMobileWanted')] = online
                    memory[address('wMobileVirtualHasPrivateColors')] = 1
                    memory[address('wMobileVirtualStage')] = 4
                    memory[address('wMobileVirtualWindowOwners'):address('wMobileVirtualWindowOwners') + 3] = b'\x01\x02\x03'
                    memory[address('wMobileVirtualQueuedPeer')] = 2
                    expected = bytearray(memory)
                    end = destination + count * 16
                    overlap = online and count and any(bank == b and destination < high and end > low
                                                       for b, low, high in windows)
                    if overlap:
                        expected[address('wMobileNativeGraphicsFence')] = 1
                        expected[address('hCGBPalUpdate')] = 1
                        for name in ('wMobileVirtualStage', 'wMobileVirtualPreviousOAM',
                                     'wMobileVirtualPublished', 'wMobileVirtualHasPrivateColors',
                                     'wMobileVirtualQueuedPeer'):
                            expected[address(name)] = 0
                        first = address('wMobileVirtualWindowOwners')
                        expected[first:first + 3] = b'\x00' * 3
                    registers = {name: rng.randrange(256) for name in 'abcdefghl'}
                    registers['f'] &= 0xf0
                    registers['h'], registers['l'], registers['c'] = destination >> 8, destination & 255, count
                    before = dict(registers)
                    execute('MobileNativeGraphicsRequest', memory, registers)
                    require(registers == before, 'Ownership request changed native registers/flags')
                    require(memory == expected, f'Ownership overlap mismatch: bank={bank}, destination={destination:04x}, count={count}')
                    cases += 1
    for menu in (0, 1):
        for script in (0, 1, 255):
            for panning in (0, 1):
                for active in range(11):
                    memory = bytearray(65536)
                    memory[0xff70] = rng.randrange(1, 8)
                    memory[address('wMobileNativeGraphicsFence')] = 1
                    memory[address('hInMenu')] = menu
                    memory[address('wScriptRunning')] = script
                    memory[address('wPanningAroundTinyMap')] = panning
                    if active:
                        memory[address(f'wSpriteAnim{active}')] = 1
                    expected = bytearray(memory)
                    held = bool(menu or script or panning or active)
                    expected[address('wMobileNativeGraphicsFence')] = int(held)
                    registers = {name: rng.randrange(256) for name in 'abcdefghl'}
                    registers['f'] &= 0xf0
                    before = dict(registers)
                    execute('MobileNativeGraphicsRelease', memory, registers)
                    require(registers['a'] == int(held), 'Ownership release misreported native lifetime')
                    require(all(registers[name] == before[name] for name in 'bcdehl'),
                            'Ownership release changed native input registers')
                    require(memory == expected, 'Ownership released while native graphics were active')
                    cases += 1
    print(f'Emitted native VRAM ownership: {cases} interval/lifetime/register cases passed')
