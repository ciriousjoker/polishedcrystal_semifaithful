"""Execute compiled admission metadata routines with bank-aware private RAM."""
import random


def verify_admission_plans(rom, symbols, require):
    rng = random.Random(0x706c616e)
    names = 'bcdehl?a'

    def address(name):
        return symbols[name][1]

    class Machine:
        def __init__(self):
            self.memory = bytearray(rng.randbytes(65536))
            self.banks = [bytearray(rng.randbytes(4096)) for _ in range(8)]
            self.memory[0xff70] = 5
            self.regs = {k: rng.randrange(256) for k in 'abcdehl'}
            self.zero = self.carry = False
            self.cycles = 0
            self.rom_bank = symbols["MobilePlanStart"][0]

        def read(self, a):
            if 0x4000 <= a < 0x8000:
                return rom[self.rom_bank * 0x4000 + a - 0x4000]
            if 0xd000 <= a < 0xe000:
                return self.banks[self.memory[0xff70] & 7 or 1][a - 0xd000]
            return self.memory[a]

        def write(self, a, value):
            if 0xd000 <= a < 0xe000:
                self.banks[self.memory[0xff70] & 7 or 1][a - 0xd000] = value
            else:
                self.memory[a] = value

        def pair(self, pair):
            if pair == 'af':
                return self.regs['a'] << 8 | int(self.zero) * 128 | int(self.carry) * 16
            return self.regs[pair[0]] << 8 | self.regs[pair[1]]

        def put(self, pair, value):
            self.regs[pair[0]] = value >> 8 & 255
            if pair == 'af':
                self.zero, self.carry = bool(value & 128), bool(value & 16)
            else:
                self.regs[pair[1]] = value & 255

        def snapshot(self):
            return bytes(self.memory), tuple(map(bytes, self.banks))

        def execute(self, label):
            bank, pc = symbols[label]
            self.rom_bank = bank
            stack = []
            guards = {value[1] for name, value in symbols.items() if
                      name.startswith('MobilePlanEmitNativeView.raw_guard') or
                      name == 'MobilePlanEmitNativeView.local_template_guard' or
                      name.startswith('MobilePlanNativeShape') and name.endswith(('.hit_guard', '.miss'))}
            self.guard_cycles = 0

            def fetch():
                nonlocal pc
                value = rom[bank * 0x4000 + pc - 0x4000]
                pc += 1
                return value

            def condition(n):
                return (not self.zero, self.zero, not self.carry, self.carry)[n]

            for _ in range(10000):
                if pc in guards: self.guard_cycles = self.cycles
                op = fetch()
                # LR35902 T-states for emitted instructions. Native helper ABI
                # models use a conservative bound including their RST/return.
                if 0x40 <= op < 0x80: cost = 8 if (op & 7) == 6 or (op >> 3 & 7) == 6 else 4
                elif 0x80 <= op <= 0xbf: cost = 8 if (op & 7) == 6 else 4
                elif op & 0xcf == 0x01: cost = 12
                elif op & 0xc7 == 0x06: cost = 12 if (op >> 3 & 7) == 6 else 8
                elif op & 0xc7 in (0x04, 0x05): cost = 12 if (op >> 3 & 7) == 6 else 4
                elif op in (0x20, 0x28, 0x30, 0x38): cost = 12 if condition((op - 0x20) // 8) else 8
                elif op in (0xc4, 0xcc, 0xd4, 0xdc): cost = 24 if condition((op - 0xc4) // 8) else 12
                elif op in (0xc2, 0xca, 0xd2, 0xda): cost = 16 if condition((op - 0xc2) // 8) else 12
                elif op in (0xc0, 0xc8, 0xd0, 0xd8): cost = 20 if condition((op - 0xc0) // 8) else 8
                elif op in (0xc5, 0xd5, 0xe5, 0xf5): cost = 16
                elif op in (0xc1, 0xd1, 0xe1, 0xf1): cost = 12
                elif op in (0xe7, 0xef): cost = 400 if op == 0xe7 and self.pair('bc') == 8 else self.pair('bc') * 32 + 320
                else: cost = {0xfa:16, 0xea:16, 0xf0:12, 0xe0:12, 0x12:8, 0x1a:8,
                    0x22:8, 0x2a:8, 0x13:8, 0x23:8, 0x1b:8, 0x2b:8, 0x09:8, 0x19:8, 0x29:8,
                    0x07:4, 0x0f:4, 0xe9:4, 0xcb:8, 0xe6:8, 0xee:8, 0xf6:8, 0xc6:8, 0xd6:8, 0xfe:8, 0xa7:4, 0xaf:4, 0xbe:8,
                    0x18:12, 0xc3:16, 0xcd:24, 0x37:4, 0xc9:16}.get(op, 0)
                require(cost > 0, f'Missing timing for plan opcode {op:02x}')
                self.cycles += cost
                if 0x40 <= op < 0x80 and op != 0x76:
                    source, target = op & 7, op >> 3 & 7
                    value = self.read(self.pair('hl')) if source == 6 else self.regs[names[source]]
                    if target == 6: self.write(self.pair('hl'), value)
                    else: self.regs[names[target]] = value
                elif op & 0xcf == 0x01:
                    self.put(('bc', 'de', 'hl', 'sp')[op >> 4 & 3], fetch() | fetch() << 8)
                elif op & 0xc7 == 0x06:
                    value = fetch()
                    if op >> 3 & 7 == 6: self.write(self.pair('hl'), value)
                    else: self.regs[names[op >> 3 & 7]] = value
                elif op == 0xfa: self.regs['a'] = self.read(fetch() | fetch() << 8)
                elif op == 0xea: self.write(fetch() | fetch() << 8, self.regs['a'])
                elif op == 0xf0: self.regs['a'] = self.read(0xff00 + fetch())
                elif op == 0xe0: self.write(0xff00 + fetch(), self.regs['a'])
                elif op == 0x1a: self.regs['a'] = self.read(self.pair('de'))
                elif op == 0x12: self.write(self.pair('de'), self.regs['a'])
                elif op == 0x22:
                    self.write(self.pair('hl'), self.regs['a'])
                    self.put('hl', self.pair('hl') + 1)
                elif op == 0x2a:
                    self.regs['a'] = self.read(self.pair('hl'))
                    self.put('hl', self.pair('hl') + 1)
                elif op in (0x13, 0x23, 0x1b, 0x2b):
                    pair = 'de' if op in (0x13, 0x1b) else 'hl'
                    self.put(pair, self.pair(pair) + (1 if op in (0x13, 0x23) else -1))
                elif op & 0xc7 in (0x04, 0x05):
                    target, delta = op >> 3 & 7, 1 if op & 7 == 4 else -1
                    value = self.read(self.pair('hl')) if target == 6 else self.regs[names[target]]
                    value = (value + delta) & 255
                    self.zero = value == 0
                    if target == 6: self.write(self.pair('hl'), value)
                    else: self.regs[names[target]] = value
                elif op in (0x09, 0x19, 0x29):
                    value = self.pair('hl') + self.pair({0x09: 'bc', 0x19: 'de', 0x29: 'hl'}[op])
                    self.carry = value > 65535
                    self.put('hl', value)
                elif op == 0xe9:
                    pc = self.pair('hl')
                elif op == 0x0f:
                    a = self.regs['a']
                    self.regs['a'] = (a >> 1 | a << 7) & 255
                    self.zero, self.carry = False, bool(a & 1)
                elif op == 0x07:
                    a = self.regs['a']
                    self.regs['a'] = (a << 1 | a >> 7) & 255
                    self.zero, self.carry = False, bool(a & 128)
                elif op == 0xcb:
                    sub = fetch()
                    if sub == 0x37:
                        a = self.regs['a']
                        self.regs['a'] = a >> 4 | (a & 15) << 4
                        self.zero, self.carry = self.regs['a'] == 0, False
                    elif 0xc0 <= sub <= 0xff and sub & 7 != 6:
                        self.regs[names[sub & 7]] |= 1 << ((sub - 0xc0) >> 3)
                    else:
                        require(0x40 <= sub <= 0x7f and sub & 7 != 6, 'Unexpected plan bit instruction')
                        self.zero = not self.regs[names[sub & 7]] & (1 << ((sub - 0x40) >> 3))
                elif 0x80 <= op <= 0xbf:
                    a = self.regs['a']
                    v = self.read(self.pair('hl')) if op & 7 == 6 else self.regs[names[op & 7]]
                    group = (op - 0x80) >> 3
                    extra = int(self.carry) if group in (1, 3) else 0
                    raw = a + v + extra if group < 2 else a - v - extra if group in (2, 3, 7) else a & v if group == 4 else a ^ v if group == 5 else a | v
                    result = raw & 255
                    self.zero = result == 0
                    self.carry = raw > 255 if group < 2 else raw < 0 if group in (2, 3, 7) else False
                    if group != 7: self.regs['a'] = result
                elif op in (0xe6, 0xee, 0xf6, 0xc6, 0xd6, 0xfe, 0xa7, 0xaf, 0xbe):
                    a = self.regs['a']
                    v = fetch() if op in (0xe6, 0xee, 0xf6, 0xc6, 0xd6, 0xfe) else self.read(self.pair('hl')) if op == 0xbe else a
                    value = a | v if op == 0xf6 else a & v if op in (0xe6, 0xa7) else a ^ v if op in (0xaf, 0xee) else (a + v) & 255 if op == 0xc6 else (a - v) & 255
                    self.zero = value == 0
                    self.carry = a + v > 255 if op == 0xc6 else a < v if op in (0xd6, 0xfe, 0xbe) else False
                    if op not in (0xfe, 0xbe): self.regs['a'] = value
                elif op in (0x18, 0x20, 0x28, 0x30, 0x38):
                    delta = fetch()
                    if op == 0x18 or condition((op - 0x20) // 8):
                        pc += delta if delta < 128 else delta - 256
                elif op in (0xc3, 0xc2, 0xca, 0xd2, 0xda):
                    target = fetch() | fetch() << 8
                    if op == 0xc3 or condition((op - 0xc2) // 8): pc = target
                elif op in (0xcd, 0xc4, 0xcc, 0xd4, 0xdc):
                    target = fetch() | fetch() << 8
                    if op != 0xcd and not condition((op - 0xc4) // 8): continue
                    if target == address('MobileNativeRectangleTrampoline'):
                        # Independent emitted facing tests bound the complete
                        # descriptor transform at984 clocks. Reserve1300 here
                        # for its resident entry and bank-return ABI as well.
                        self.cycles += 1300
                        desc = self.pair('bc')
                        src = self.read(desc + 4) | self.read(desc + 5) << 8
                        if 0x4000 <= src <= 0x7ff0:
                            source_bank = symbols['MobileCopiedFacingStepDown0'][0]
                            body = source_bank * 0x4000 + src - 0x4000
                            x, y, tile, attrs, relative = (self.read(desc + field) for field in (0, 1, 2, 3, 7))
                            for q in range(4):
                                oy, ox, flags, t = rom[body + q * 4:body + q * 4 + 4]
                                t = ((0 if flags & 4 else tile) + t) & 255
                                if attrs & 8 and t >= 128: t -= 64
                                color = (((relative if flags & 2 else 0) | flags) & 0xe0) | attrs
                                if flags & 8: color = (color + 1) & 255
                                for v in ((y + oy) & 255, (x + ox) & 255, t, color):
                                    self.write(self.pair('de'), v)
                                    self.put('de', self.pair('de') + 1)
                        else:
                            self.put('de', self.pair('de') + 16)
                    else:
                        stack.append(pc)
                        pc = target
                elif op == 0x37:
                    self.carry = True
                elif op in (0xc5, 0xd5, 0xe5, 0xf5):
                    stack.append(self.pair(('bc', 'de', 'hl', 'af')[(op - 0xc5) // 16]))
                elif op in (0xc1, 0xd1, 0xe1, 0xf1):
                    self.put(('bc', 'de', 'hl', 'af')[(op - 0xc1) // 16], stack.pop())
                elif op in (0xe7, 0xef):
                    # Native CopyBytes/ByteFill are not under test. Model their
                    # documented helper ABI, retaining all caller-side opcodes.
                    count = self.pair('bc')
                    for _ in range(count):
                        value = self.read(self.pair('hl')) if op == 0xe7 else self.regs['a']
                        self.write(self.pair('de') if op == 0xe7 else self.pair('hl'), value)
                        self.put('hl', self.pair('hl') + 1)
                        if op == 0xe7: self.put('de', self.pair('de') + 1)
                    self.put('bc', 0)
                elif op in (0xc9, 0xc0, 0xc8, 0xd0, 0xd8):
                    if op == 0xc9 or condition((op - 0xc0) // 8):
                        if not stack: return
                        pc = stack.pop()
                else:
                    raise ValueError(f'Unexpected plan opcode {op:02x} at {pc - 1:04x}')
            raise ValueError('Plan metadata routine did not terminate')

    checksum_cases = checksum_chunk_max = 0
    for size in (0, 1, 2, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 127, 128, 191, 192, 255, 256, 257, 511, 512):
        for resumed in (False, True):
            for _ in range(8):
                m = Machine()
                start = 0xd010
                data = rng.randbytes(size)
                m.banks[5][0x10:0x10 + size] = data
                initial = rng.randrange(65536) if resumed else 0
                m.put('hl', start)
                m.put('bc', size)
                m.put('de', initial)
                before = m.snapshot()
                m.execute('MobileChecksum.loop' if resumed else 'MobileChecksum')
                require(m.pair('de') == (initial + sum(data)) & 65535 and m.pair('hl') == start + size and m.pair('bc') == 0,
                        'Batched checksum changed unsigned sum, carry, length or resume pointers')
                require(m.snapshot() == before, 'Batched checksum changed ROM, RAM or bank selectors')
                if size == 32: checksum_chunk_max = max(checksum_chunk_max, m.cycles)
                checksum_cases += 1
    require(checksum_chunk_max <= 1300, 'Batched32-byte checksum did not improve its previous1,808-clock loop bound')
    print(f'Compiled batched checksum:{checksum_cases} zero/tail/word/carry/resume vectors,32-byte max{checksum_chunk_max} clocks')

    gather_cases = gather_max = 0
    for peer in range(3):
        for fault in ('none', 'Visible', 'DrawValid', 'MapGroup', 'MapNumber', 'Instance', 'Status', 'LoadedSprite', 'NativeStage', 'PrivatePalValid', 'window', 'dialog'):
            for _ in range(12):
                m = Machine()
                for name, value in (('wMobileVirtualStage', 1), ('wMobileVirtualFastAdmission', 2),
                                    ('wMobileVirtualForceFull', 1), ('wMobileVirtualMapGroup', 3),
                                    ('wMobileVirtualMapNumber', 5)):
                    m.write(address(name), value)
                m.write(address('wMobileInstanceKey'), 0x34)
                m.write(address('wMobileInstanceKey') + 1, 0x12)
                m.banks[1][address('wScriptRunning') - 0xd000] = 0
                avatar = rng.randrange(4)
                x, y = rng.randrange(256), rng.randrange(256)
                frame, cache = rng.randrange(16), rng.choice((1, 2))
                m.memory[0xff70] = 2
                fields = {'Visible':1, 'DrawValid':1, 'MapGroup':3, 'MapNumber':5, 'Instance':0x34,
                          'Status':1, 'Avatar':avatar, 'LoadedSprite':avatar, 'ScreenX':x, 'ScreenY':y,
                          'NativeStage':7, 'NativeFacing':frame, 'PrivatePalValid':cache}
                for name, value in fields.items(): m.write(address(f'wMobilePeer{peer + 1}' + name), value)
                m.write(address(f'wMobilePeer{peer + 1}Instance') + 1, 0x12)
                if fault in fields: m.write(address(f'wMobilePeer{peer + 1}' + fault), fields[fault] ^ 255)
                if fault == 'dialog':
                    m.banks[1][address('wScriptRunning') - 0xd000] = 255
                    m.write(address(f'wMobilePeer{peer + 1}ScreenX'), 72)
                m.memory[0xff70] = 5
                for slot in range(3): m.write(address('wMobileVirtualWindowOwners') + slot, slot + 1)
                if fault == 'window': m.write(address('wMobileVirtualWindowOwners') + peer, 255)
                m.write(address('wMobileVirtualNativeColors'), rng.randrange(256))
                m.write(address('wMobileVirtualNativeColors') + 1, rng.randrange(256))
                m.regs['a'] = peer
                twin = Machine()
                twin.memory[:] = m.memory
                twin.banks = list(map(bytearray, m.banks))
                twin.regs = dict(m.regs)
                before = m.snapshot()
                m.execute('MobilePlanGatherPeer')
                twin.execute('MobileVirtualGatherPeer')
                for name in ('wMobileVirtualPlanGatherValid', 'wMobileBudgetIndex', 'wMobileAvatarTile',
                             'wMobileOAMWriteCursor', 'wMobileOAMDemand', 'wMobileVirtualFrame',
                             'wMobileVirtualCacheHit', 'wMobileVirtualWindow', 'wMobileAvatarPalette', 'wMobileVirtualGroupOwner'):
                    # Read private bank5 aliases through their physical page,
                    # since rejected eligibility may intentionally return bank2.
                    addr = address(name)
                    actual = m.banks[5][addr - 0xd000] if addr >= 0xd000 else m.memory[addr]
                    expected = twin.banks[5][addr - 0xd000] if addr >= 0xd000 else twin.memory[addr]
                    require(actual == expected, f'Plan-only gather changed ordinary live eligibility or pose:{name}/{fault}')
                require(all(bytes(m.banks[b]) == before[1][b] for b in (1, 2, 3, 4, 6, 7)), 'Plan gather mutated native/peer RAM')
                if fault == 'none':
                    require(m.banks[5][address('wMobileVirtualPlanGatherValid') - 0xd000] == 1, 'Valid committed pose failed gather')
                    gather_max = max(gather_max, m.cycles)
                gather_cases += 1
    require(gather_max <= 1300, 'Plan-only gather did not remove redundant ordinary row preflight')
    print(f'Compiled plan-only gather:{gather_cases} live eligibility/pose/native-policy comparison vectors, max{gather_max} clocks')

    guest_cases = guest_max = 0
    for window in range(3):
        for frame in range(16):
            for kind in (1, 2):
                for palette in range(8):
                    m = Machine()
                    peer = rng.randrange(3)
                    x, y = rng.randrange(256), rng.randrange(256)
                    for name, value in (('wMobileBudgetIndex', peer), ('wMobileVirtualWindow', window),
                                        ('wMobileVirtualFrame', frame), ('wMobileAvatarPalette', palette),
                                        ('wMobileOAMWriteCursor', x), ('wMobileOAMDemand', y)):
                        m.write(address(name), value)
                    m.write(address('wMobileVirtualPlanPending') + 8, kind)
                    for slot in range(8): m.write(address('wMobileVirtualBindings') + slot, 0)
                    m.write(address('wMobileVirtualPlannedPrivateColors'), 0)
                    rgb = rng.randbytes(8)
                    for byte, value in enumerate(rgb): m.write(address('wMobilePrivatePalColors') + peer * 8 + byte, value)
                    before_memory, before_banks = m.snapshot()
                    m.execute('MobilePlanWriteGuest')
                    mirror = frame & 12 == 12 or frame & 12 < 8 and frame & 3 == 3
                    wanted = bytearray()
                    base = 0x64 + window * 4 + int(mirror)
                    for quad in range(4):
                        tile = (base ^ (quad & 1)) + (2 if quad >= 2 else 0)
                        attr = palette | (32 if mirror else 0)
                        if 0x6d <= tile < 0x70: tile, attr = (0x3f, 0x7f, 0xff)[tile - 0x6d], attr | 8
                        wanted.extend(((y + (8 if quad >= 2 else 0)) & 255, (x + (8 if quad & 1 else 0)) & 255, tile, attr))
                    output = address('wMobileVirtualOutput') - 0xd000 + 16
                    require(not m.carry and bytes(m.banks[5][output:output + 16]) == wanted,
                            'Direct guest changed pose, exact color or third-window tile ownership')
                    require(m.read(address('wMobileVirtualPeerMask')) == 1 << peer and m.read(address('wMobileRenderedCount')) == 1,
                            'Direct guest lost selected peer metadata')
                    binding = m.read(address('wMobileVirtualBindings') + palette)
                    require(binding == (peer + 1 if kind == 1 else 0), 'Direct guest changed exact/native palette provenance')
                    if kind == 1:
                        start = address('wMobileVirtualFrameColors') - 0xd000 + peer * 8
                        require(bytes(m.banks[5][start:start + 8]) == rgb, 'Direct guest substituted current private RGB')
                    expected_banks = list(map(bytearray, before_banks))
                    for field in ('OAMStart', 'Palette'):
                        target = address(f'wMobilePeer{peer + 1}' + field) + (1 if field == 'OAMStart' else 0) - 0xd000
                        expected_banks[2][target] = 16 if field == 'OAMStart' else palette
                    require(all(bytes(m.banks[b]) == bytes(expected_banks[b]) for b in range(8) if b != 5),
                            'Direct guest changed native RAM or unselected peer state')
                    guest_max = max(guest_max, m.cycles)
                    guest_cases += 1
    require(guest_max < 2400, 'Direct admitted guest exceeded its bounded output budget')
    print(f'Compiled direct guest:{guest_cases} exact pose/color/bank/window vectors, max {guest_max} clocks')

    invalid_guest_cases = 0
    for field, valid, invalid_values in (
            ('wMobileBudgetIndex', 0, range(3, 256)),
            ('wMobileVirtualWindow', 0, range(3, 256)),
            ('wMobileAvatarPalette', 0, range(8, 256)),
            ('kind', 1, [0, *range(3, 256)])):
        for invalid in invalid_values:
            m = Machine()
            m.write(address('wMobileBudgetIndex'), 0)
            m.write(address('wMobileVirtualWindow'), 0)
            m.write(address('wMobileAvatarPalette'), 0)
            m.write(address('wMobileVirtualPlanPending') + 8, 1)
            m.write(address('wMobileVirtualPlanPending') + 8 if field == 'kind' else address(field), invalid)
            before = m.snapshot()
            m.execute('MobilePlanWriteGuest')
            require(m.carry and m.snapshot() == before, 'Invalid guest input escaped validation or changed RAM')
            invalid_guest_cases += 1
    print(f'Compiled direct guest rejection:{invalid_guest_cases} invalid indices/color/provenance values, no RAM writes')

    fused_max = 0
    for changed in (False, True):
        for case in range(96):
            m = Machine()
            m.memory[0xff44] = 100
            shadow = address('wShadowOAM')
            source = rng.randbytes(160)
            m.memory[shadow:shadow + 160] = source
            selected = [0] + rng.sample(range(1, 13), 8)
            offsets = [255] * 13
            offsets[0] = 0
            for ordinal, actor in enumerate(selected[1:]): offsets[actor] = 32 + ordinal * 16
            expected = bytearray(m.banks[5][address('wMobileVirtualOutput') - 0xd000:address('wMobileVirtualOutput') - 0xd000 + 160])
            for actor in range(13):
                start = actor % 10 * 16
                descriptor = address('wMobileVirtualDescriptors') + actor * 8
                m.write(descriptor + 6, 0xc4 | (32 if actor > 6 else 0))
                m.write(descriptor + 7, start)
                signature = address('wMobileVirtualPlanPreviousSignature') + actor * 3
                for field, value in enumerate((4 | (32 if actor > 6 else 0), source[start + 8], source[start + 11] & 7)):
                    m.write(signature + field, value)
                m.write(address('wMobileVirtualPlanPending') + 13 + actor, offsets[actor])
                if offsets[actor] != 255:
                    expected[offsets[actor]:offsets[actor] + 16] = source[start:start + 16]
            m.memory[0xff70] = 1
            used = m.read(address('wUsedObjectPals'))
            m.memory[0xff70] = 5
            m.write(address('wMobileVirtualPlanPreviousSignature') + 39, used)
            colors = rng.randrange(256)
            m.write(address('wMobileVirtualNativeAvatarColors'), colors)
            m.write(address('wMobileVirtualPlanPreviousSignature') + 40, colors)
            m.write(address('wMobileVirtualPlanSignatureValid'), 1)
            m.write(address('wMobileVirtualForceFull'), 1)
            if changed:
                field = rng.randrange(41)
                a = address('wMobileVirtualPlanPreviousSignature') + field
                m.write(a, m.read(a) ^ 1)
            before_memory, before_banks = m.snapshot()
            m.execute('MobilePlanEmitNativeView')
            require(m.carry == changed, 'Fused live validation accepted a changed native resource field')
            require(all(bytes(m.banks[b]) == before_banks[b] for b in range(8) if b != 5) and m.memory[:0xcec4] == before_memory[:0xcec4],
                    'Fused fresh emission changed native game memory')
            if changed:
                require(m.read(address('wMobileVirtualPlanSignatureValid')) == 0 and m.read(address('wMobileVirtualForceFull')) == 0,
                        'Changed native view retained a publishable cached proof')
            else:
                output = address('wMobileVirtualOutput') - 0xd000
                require(m.banks[5][output:output + 160] == expected,
                        'Fused fresh raw groups changed geometry, attributes or admitted presentation order')
                fused_max = max(fused_max, m.cycles)
    print(f'Compiled fused native view: 192 exact/mismatched fresh-output vectors, max {fused_max} clocks')

    tail_max = 0
    tail_cases = 0
    for mode in ('raw', 'cold', 'warm'):
        for first in range(13):
            if first == 0 and mode == 'warm': continue
            template = mode != 'raw'
            if not template:
                deadline = 142 if first >= 11 else 141 if first >= 8 else 140 if first >= 4 else 139 if first >= 1 else 138
            elif first == 0:
                deadline = 137
            elif mode == 'warm':
                deadline = 142 if first >= 11 else 141 if first >= 8 else 140 if first >= 4 else 139
            else:
                deadline = 140 if first >= 10 else 139 if first >= 7 else 138 if first >= 4 else 137
            m = Machine()
            m.memory[0xff44] = deadline - 1
            for actor in range(13):
                descriptor = address('wMobileVirtualDescriptors') + actor * 8
                raw = not (template and actor == first)
                m.write(descriptor + 6, 0xc4 if raw else 0x44)
                start = actor % 10 * 16
                m.write(descriptor + 7, start)
                m.write(descriptor + 1, 24)
                m.write(descriptor + 3, 1)
                src = address('MobileCopiedFacingStepDown0') + 1
                m.write(descriptor + 4, src & 255)
                m.write(descriptor + 5, src >> 8)
                m.memory[address('wShadowOAM') + start + 8] = 24
                m.memory[address('wShadowOAM') + start + 11] = 1
                signature = address('wMobileVirtualPlanPreviousSignature') + actor * 3
                for field, value in enumerate((4, 24, 1)): m.write(signature + field, value)
                m.write(address('wMobileVirtualPlanPending') + 13 + actor, 0 if actor == first else 255)
                if actor:
                    key = address('wMobileNativeShapeKeys') + (actor - 1) * 6
                    for field, source_field in enumerate((2, 3, 4, 5, 7)):
                        m.write(key + field, m.read(descriptor + source_field))
                    m.write(key + 5, int(mode == 'warm'))
            m.memory[0xff70] = 1
            used = m.read(address('wUsedObjectPals'))
            m.memory[0xff70] = 5
            m.write(address('wMobileVirtualPlanPreviousSignature') + 39, used)
            m.write(address('wMobileVirtualNativeAvatarColors'), 0)
            m.write(address('wMobileVirtualPlanPreviousSignature') + 40, 0)
            m.execute(f'MobilePlanEmitNativeView.actor{first}')
            require(not m.carry, f'Late native {mode} actor{first} rejected its proved validation tail')
            require(m.cycles - m.guard_cycles + 700 <= (144 - deadline) * 912,
                    f'Actor{first} {mode} exceeds its remaining validation plus700-clock caller return margin: {m.cycles - m.guard_cycles}')
            tail_max = max(tail_max, m.cycles - m.guard_cycles + 700)
            print(f'Native tail budget:{mode} actor{first} remaining{m.cycles - m.guard_cycles + 700} clocks, latest safe guard{144 - (m.cycles - m.guard_cycles + 700 + 911) // 912}')
            m.memory[0xff44] = deadline
            if first and mode == 'cold': m.write(address('wMobileNativeShapeKeys') + (first - 1) * 6 + 5, 0)
            output = bytes(m.banks[5][address('wMobileVirtualOutput') - 0xd000:address('wMobileVirtualOutput') - 0xd000 + 160])
            m.execute(f'MobilePlanEmitNativeView.actor{first}')
            require(m.carry and bytes(m.banks[5][address('wMobileVirtualOutput') - 0xd000:address('wMobileVirtualOutput') - 0xd000 + 160]) == output,
                    'Native shape guard accepted work beyond its complete-tail deadline')
            tail_cases += 1
    print(f'Compiled late native tails:{tail_cases} raw/cold/warm guards include all remaining validation and700 return clocks, max {tail_max}')

    shape_cases = shape_cold_max = shape_warm_max = 0
    for actor in range(1, 13):
        for _ in range(32):
            m = Machine()
            m.memory[0xff44] = 100
            desc = address('wMobileVirtualDescriptors') + actor * 8
            key = address('wMobileNativeShapeKeys') + (actor - 1) * 6
            shape = address('wMobileNativeShapes') + (actor - 1) * 16
            src = address('MobileCopiedFacingStepDown0') + 1
            x, y, tile, attrs, relative = (rng.randrange(256) for _ in range(5))
            for field, value in enumerate((x, y, tile, attrs, src & 255, src >> 8, 0x44, relative)): m.write(desc + field, value)
            m.write(key + 5, 0)
            offset = rng.randrange(10) * 16
            destination = address('wMobileVirtualOutput') + offset
            body = symbols['MobileCopiedFacingStepDown0'][0] * 0x4000 + src - 0x4000
            normalized = bytearray()
            for q in range(4):
                oy, ox, flags, t = rom[body + q * 4:body + q * 4 + 4]
                t = ((0 if flags & 4 else tile) + t) & 255
                if attrs & 8 and t >= 128: t -= 64
                color = (((relative if flags & 2 else 0) | flags) & 0xe0) | attrs
                if flags & 8: color = (color + 1) & 255
                normalized.extend((oy, ox, t, color))
            before_memory, before_banks = m.snapshot()
            m.put('de', destination)
            m.execute(f'MobilePlanNativeShape{actor}')
            require(not m.carry and bytes(m.banks[5][shape - 0xd000:shape - 0xd000 + 16]) == normalized,
                    'Cold native shape cache changed immutable facing offsets, tile or attribute semantics')
            shape_cold_max = max(shape_cold_max, m.cycles)
            for _ in range(3):
                x, y = rng.randrange(256), rng.randrange(256)
                m.write(desc, x)
                m.write(desc + 1, y)
                m.put('de', destination)
                m.cycles = 0
                m.execute(f'MobilePlanNativeShape{actor}')
                expected = bytearray(normalized)
                for byte in range(0, 16, 4):
                    expected[byte] = (expected[byte] + y) & 255
                    expected[byte + 1] = (expected[byte + 1] + x) & 255
                require(not m.carry and bytes(m.banks[5][destination - 0xd000:destination - 0xd000 + 16]) == expected,
                        'Warm native shape cache froze live coordinates or changed current tile/palette flags')
                shape_warm_max = max(shape_warm_max, m.cycles)
                shape_cases += 1
            require(all(bytes(m.banks[b]) == before_banks[b] for b in range(8) if b != 5) and m.memory[:0xcec4] == before_memory[:0xcec4],
                    'Native shape cache changed native game RAM')
    print(f'Compiled normalized native shapes:384 cold and{shape_cases} moving/wrapping warm vectors, maxima {shape_cold_max}/{shape_warm_max} clocks')

    audit_max = 0
    for case in range(384):
        m = Machine()
        ids = [rng.randrange(4 if case % 3 else 32) for _ in range(8)]
        type_mask = rng.randrange(256)
        glows = [rng.choice((0, 0, 1, 128)) for _ in range(8)]
        previous = [rng.choice((0, 0, 2, 64)) for _ in range(8)]
        weather, cooldown = rng.randrange(2), rng.randrange(2)
        m.memory[0xff70] = 1
        for slot in range(8):
            m.write(address('wLoadedObjPal0') + slot, ids[slot])
            m.write(address('wLoadedObjPalGlows') + slot, glows[slot])
            m.write(address('wLoadedObjPalPrevGlows') + slot, previous[slot])
        m.write(address('wLoadedObjPalType'), type_mask)
        m.write(address('wCurWeather'), weather)
        m.write(address('wOverworldWeatherCooldown'), cooldown)
        native_before = bytes(m.banks[1])
        m.memory[0xff70] = 5
        expected = [15] * 4
        mask = 0
        # PAL_OW_WEATHER is the native weather's fixed slot (6).
        for slot, avatar in enumerate(ids):
            if slot == 6 and (weather or cooldown):
                mask |= 1 << slot
            elif avatar < 4 and not (type_mask >> slot & 1) and not (glows[slot] or previous[slot]):
                expected[avatar] = slot
                mask |= 1 << slot
        m.execute('MobileVirtualCurrentColors')
        require(m.memory[0xff70] == 5 and bytes(m.banks[1]) == native_before,
                'Live native color audit changed native palette metadata or return bank')
        require([m.read(address('wMobileVirtualNativeColors') + i) for i in range(2)] ==
                [expected[0] | expected[1] << 4, expected[2] | expected[3] << 4],
                'Exact avatar slot audit differs from live type/glow/weather provenance')
        require(m.read(address('wMobileVirtualNativeAvatarColors')) == mask,
                'Native exact-color/weather reservation differs')
        require(m.read(address('wMobileVirtualNeedColorAudit')) == int(15 in expected),
                'Missing avatar audit status differs')
        audit_max = max(audit_max, m.cycles)
    print(f'Compiled live color audit: 384 independent type/glow/weather/source cases, max {audit_max} clocks')

    # Native CopyBytes(8) has an unchanged 328-clock RST/helper path. The
    # 400-clock ABI bound above includes margin; other sizes retain the generic
    # conservative helper bound. A full OAM page has no suffix stores.
    publish_max = 0
    for phase in range(3):
        for colors in (False, True):
            for native_next in (255, 1, 6):
                for ground_next in (255, 1, 6):
                    m = Machine()
                    m.memory[0xff44] = 141
                    m.write(address('wMobileVirtualLength'), 160)
                    m.write(address('wMobileVirtualStage'), 7)
                    m.write(address('wMobileVirtualPhase'), phase)
                    m.write(address('wMobileVirtualNativeNext'), native_next)
                    m.write(address('wMobileVirtualGroundNext'), ground_next)
                    for slot in range(8):
                        m.write(address('wMobileVirtualBindings') + slot, slot % 3 + 1 if colors else 0)
                    twin = Machine()
                    twin.memory[:] = m.memory
                    twin.banks = list(map(bytearray, m.banks))
                    twin.regs = dict(m.regs)
                    twin.zero, twin.carry = m.zero, m.carry
                    twin.execute('MobilePlanPublish')
                    m.execute('MobileVirtualComposeStep.publish')
                    require(twin.snapshot() == m.snapshot(), 'Bank-local publication diverged from ordinary DMA/color/rotation policy')
                    require(m.read(address('wMobileVirtualPublished')) == 1 and m.read(address('wMobileVirtualStage')) == 0,
                            'Full-page publication failed its bounded late window')
                    publish_max = max(publish_max, m.cycles)
    require(publish_max <= 1200, 'Full-page publication exceeded its LY142 complete-return budget')
    print(f'Compiled full-page publication: 54 color/phase/rotation cases, conservative max {publish_max} clocks')

    count_cases = 0
    for count in range(12):
        for actor in range(13):
            m = Machine()
            m.write(address('wMobileVirtualPlanCount'), count)
            m.write(address('wMobileVirtualIndex'), actor)
            expected_mem, expected_banks = m.snapshot()
            expected_banks = list(map(bytearray, expected_banks))
            if count < 9:
                expected_banks[5][address('wMobileVirtualPlanPending') + 13 + actor - 0xd000] = m.read(address('wMobileVirtualLength'))
                expected_banks[5][address('wMobileVirtualPlanCount') - 0xd000] = count + 1
            m.execute('MobilePlanRecordNative')
            require(m.snapshot() == (expected_mem, tuple(map(bytes, expected_banks))), 'Native ID collection escaped its private bounded record')
            count_cases += 1

    for slot in range(16):
        m = Machine()
        before = m.snapshot()
        c = m.regs['c']
        m.regs['a'] = slot
        m.execute('MobilePlanEntryPointer')
        require(m.pair('hl') == address('wMobileVirtualPlanEntries') + slot * 28 and m.regs['c'] == c and m.snapshot() == before,
                'Plan slot arithmetic changed lookup state or memory')

    signature_cases = 0
    for previous_valid in (0, 1, 255):
        for unchanged in (False, True):
            for _ in range(80):
                m = Machine()
                descriptors = address('wMobileVirtualDescriptors')
                shadow = address('wShadowOAM')
                for actor in range(13):
                    base = descriptors + actor * 8
                    m.write(base + 6, rng.choice((0x44, 0x64, 0xc4, 0xe4)))
                    m.write(base + 7, rng.randrange(10) * 16)
                wanted = bytearray()
                for actor in range(13):
                    base = descriptors + actor * 8
                    flags = m.read(base + 6)
                    raw = shadow + m.read(base + 7)
                    top = m.read(raw + 8) if flags & 128 else m.read(base + 1)
                    color = m.read(raw + 11) if flags & 128 else m.read(base + 3)
                    wanted.extend((flags & 63, top, color & 7))
                used = address('wUsedObjectPals')
                wanted.extend((m.banks[1][used - 0xd000], m.read(address('wMobileVirtualNativeAvatarColors'))))
                previous = address('wMobileVirtualPlanPreviousSignature') - 0xd000
                valid = address('wMobileVirtualPlanSignatureValid') - 0xd000
                m.banks[5][valid] = previous_valid
                if unchanged: m.banks[5][previous:previous + 41] = wanted
                else: m.banks[5][previous:previous + 41] = bytes(v ^ 255 for v in wanted)
                before_memory, before_banks = m.snapshot()
                expected = list(map(bytearray, before_banks))
                if previous_valid != 1 or not unchanged:
                    expected[5][previous:previous + 41] = wanted
                    expected[5][valid] = 1
                    expected[5][address('wMobileVirtualPlanSlot') - 0xd000] = 0
                    expected[5][address('wMobileVirtualPlanEntriesUsed') - 0xd000] = 0
                    for slot in range(16):
                        expected[5][address('wMobileVirtualPlanEntries') + slot * 28 + 26 - 0xd000] = 0
                    expected[5][address('wMobileVirtualPlanExpandedValid') - 0xd000] = 0
                m.execute('MobilePlanSignature')
                require(m.snapshot() == (before_memory, tuple(map(bytes, expected))),
                        'Live resource signature changed native RAM or invalidated the wrong plan fields')
                require(m.carry == (previous_valid != 1 or not unchanged), 'Signature did not report its proof invalidation')
                signature_cases += 1
    print(f'Compiled live resource signatures: {signature_cases} bank-aware changed/unchanged vectors passed')

    def hash_slot(key):
        rotate = key[3] & 7 if key[5] >= 8 else 0
        native = ((key[0] << 1) | (key[0] >> 7)) & 255
        source = (key[5] << 4 | key[5] >> 4) & 255
        y = (key[6] >> 2 | key[6] << 6) & 255
        return (((key[1] * 5 + rotate) ^ native ^ source ^ y) & 7) * 2

    def model_payload(key, order, npc_count, selected, palette):
        offsets = [255] * 13
        offsets[0] = 0
        for ordinal, actor in enumerate(a for a in order if a in selected): offsets[actor] = 32 + ordinal * 16
        def next_in_class(actors, rotation):
            if not actors or rotation > len(actors): return 255
            return next(((rotation + d) % len(actors) + 1 for d in range(len(actors))
                         if actors[(rotation + d) % len(actors)] in selected), 255)
        return bytes((palette, 2 if key[5] < 8 else 1,
                      next_in_class(order[:npc_count], key[0]), next_in_class(order[npc_count:], key[1]),
                      key[3] if key[5] < 8 else (palette + 1) & 7, 1 + len(selected), *offsets))

    lookup_cases = 0
    for canonical in (False, True):
        for valid in (False, True):
            for count in range(13):
                for case in range(16):
                    m = Machine()
                    npc_count = rng.randrange(count + 1)
                    order = rng.sample(range(1, 13), count)
                    selected = set(rng.sample(order, rng.randrange(min(8, count) + 1)))
                    native_rotation = rng.randrange(npc_count + 1)
                    ground_rotation = rng.randrange(count - npc_count + 1)
                    key = bytearray((native_rotation, ground_rotation, rng.randrange(3), rng.randrange(8), rng.randrange(4),
                                     rng.choice((0, 3, 7, 15)), rng.randrange(160)))
                    pending = address('wMobileVirtualPlanPending')
                    for field, value in enumerate(key): m.write(pending + field, value)
                    for ordinal, actor in enumerate(order): m.write(address('wMobileVirtualOrderBase') + ordinal, actor)
                    m.write(address('wMobileVirtualNPCCount'), npc_count)
                    m.write(address('wMobileVirtualOrderCount'), count)
                    m.write(address('wMobileVirtualNativeRotation'), native_rotation)
                    m.write(address('wMobileVirtualGroundRotation'), ground_rotation)
                    m.write(address('wMobileVirtualPlanExpandedValid'), 0)
                    for slot in range(16): m.write(address('wMobileVirtualPlanEntries') + slot * 28 + 26, 0)
                    cached = bytearray(key)
                    if canonical: cached[0] = 255
                    if cached[5] < 8: cached[3] ^= 255 # exact native source preserves live loan rotation
                    slot = hash_slot(cached) + case % 2
                    entry = address('wMobileVirtualPlanEntries') + slot * 28
                    palette = key[5] if key[5] < 8 else rng.randrange(8)
                    payload = bytearray(model_payload(key, order, npc_count, selected, palette))
                    if canonical: payload[2] = 254
                    for field, value in enumerate(cached + payload): m.write(entry + field, value)
                    m.write(entry + 26, int(valid))
                    before_memory, before_banks = m.snapshot()
                    m.execute('MobilePlanLookup')
                    require(m.carry == valid, 'Full-plan lookup accepted an invalid marker or lost an exact/canonical match')
                    require(all(m.read(pending + field) == value for field, value in enumerate(key)), 'Full-plan lookup changed live phase/appearance/rotation/key')
                    if valid:
                        expected = model_payload(key, order, npc_count, selected, palette)
                        require(bytes(m.read(pending + field) for field in range(7, 26)) == expected,
                                'Full-plan payload changed priority ordering, resource count or live class/palette successors')
                        m.cycles = 0
                        m.execute('MobilePlanLookup')
                        require(m.carry and m.cycles <= 400, 'Unchanged failed-frame key recopied the cached payload')
                    require(all(bytes(m.banks[b]) == before_banks[b] for b in range(8) if b != 5) and bytes(m.memory) == before_memory,
                            'Full-plan lookup or retry changed native game state')
                    lookup_cases += 1
    print(f'Compiled two-way full-plan lookup:{lookup_cases} exact/canonical/class/empty/native-color/retry vectors passed')

    rejected_markers = 0
    for marker in range(256):
        if marker == 1: continue
        m = Machine()
        key = bytes((0, 0, 0, 0, 0, 15, 76))
        pending = address('wMobileVirtualPlanPending')
        for field, value in enumerate(key): m.write(pending + field, value)
        for slot in range(16): m.write(address('wMobileVirtualPlanEntries') + slot * 28 + 26, 0)
        entry = address('wMobileVirtualPlanEntries') + hash_slot(key) * 28
        for field, value in enumerate(key): m.write(entry + field, value)
        m.write(entry + 26, marker)
        before_memory, before_banks = m.snapshot()
        m.execute('MobilePlanLookupExact')
        require(not m.carry, 'Full entry accepted invalid valid-last marker')
        require(all(bytes(m.banks[b]) == before_banks[b] for b in range(8) if b != 5) and bytes(m.memory) == before_memory,
                'Invalid full metadata changed native game state')
        rejected_markers += 1
    print(f'Compiled full-plan rejection:{rejected_markers} invalid valid-last markers passed')

    replacement_cases = 0
    for bucket in range(8):
        key = bytes((255, bucket, 0, 0, 0, 15, 76))
        for states in ((0, 0), (1, 0), (0, 1), (1, 1)):
            for victim in (0, 1, 254, 255):
                m = Machine()
                pending = address('wMobileVirtualPlanPending')
                for field, value in enumerate(key): m.write(pending + field, value)
                slot = hash_slot(key)
                for way, marker in enumerate(states):
                    m.write(address('wMobileVirtualPlanEntries') + (slot + way) * 28 + 26, marker)
                m.write(address('wMobileVirtualPlanVictims') + slot // 2, victim)
                before_memory, before_banks = m.snapshot()
                m.execute('MobilePlanInsertionSlot')
                wanted = slot + (0 if states[0] != 1 else 1 if states[1] != 1 else victim & 1)
                require(m.read(address('wMobileVirtualPlanSlot')) == wanted, 'Two-way insertion failed invalid-way preference or bounded replacement')
                require(all(bytes(m.banks[b]) == before_banks[b] for b in range(8) if b != 5) and bytes(m.memory) == before_memory,
                        'Two-way insertion changed native state')
                replacement_cases += 1
    print(f'Compiled two-way replacement:{replacement_cases} invalid-way and independent bucket vectors passed')

    for count, rotation in [(n, 0) for n in range(13, 256)] + [(n, 255) for n in range(13)]:
        m = Machine()
        m.write(address('wMobileVirtualNPCCount'), count)
        m.write(address('wMobileVirtualNativeRotation'), rotation)
        before_memory, before_banks = m.snapshot()
        m.execute('MobilePlanCanonicalSuccessor')
        expected = list(map(bytearray, before_banks))
        expected[5][address('wMobileVirtualPlanPending') + 9 - 0xd000] = 255
        require(m.snapshot() == (before_memory, tuple(map(bytes, expected))) and m.cycles <= 250,
                'Invalid native class size/rotation escaped its bounded successor rejection')

    seed_cases = 0
    seed_max_cycles = 0
    for npc_count in (0, 6, 12):
        for private in (False, True):
            for rotation in range(8):
                for peer_top in (0, 8, 24, 55, 56, 57, 151, 159, 168):
                    m = Machine()
                    def put(name, value): m.write(address(name), value)
                    nr = rng.randrange(npc_count) if npc_count else 0
                    gc = 12 - npc_count
                    gr = rng.randrange(gc) if gc else 0
                    put('wMobileVirtualOrderValid', 0)
                    put('wMobileVirtualNativeRotation', nr)
                    put('wMobileVirtualGroundRotation', gr)
                    put('wMobileVirtualPaletteRotation', rotation)
                    put('wMobileVirtualPlanPending', nr)
                    m.write(address('wMobileVirtualPlanPending') + 1, gr)
                    m.write(address('wMobileVirtualPlanPending') + 3, rotation)
                    put('wMobileVirtualPlanMode', 1)
                    put('wMobileBudgetIndex', 2)
                    put('wMobileAvatarTile', 3)
                    put('wMobileOAMDemand', peer_top)
                    put('wMobileOAMWriteCursor', 80)
                    put('wMobileAvatarPalette', 15 if private else 3)
                    put('wMobileVirtualCacheHit', 1)
                    protected = rng.randrange(256) | 1
                    if private: protected &= ~128 # one valid loan must remain
                    put('wMobileVirtualNativeAvatarColors', protected)
                    used = rng.randrange(256)
                    m.banks[1][address('wUsedObjectPals') - 0xd000] = used
                    for slot in range(1, 13):
                        m.write(address('wMobileVirtualDescriptors') + slot * 28 + 6,
                                4 | (32 if slot > npc_count else 0))
                    m.write(address('wMobileVirtualPlanPreviousSignature') + 1, 56)
                    m.write(address('wMobileVirtualPlanPreviousSignature') + 2, 0)
                    m.write(address('wMobileVirtualLines') + 14, 13)
                    m.write(address('wMobileVirtualLines') + 15, 8)
                    m.execute('MobilePlanBuildOrder')
                    m.cycles = 0
                    before_memory, before_banks = m.snapshot()
                    source_inputs = {key: m.read(address(key)) for key in (
                        'wMobileBudgetIndex', 'wMobileAvatarTile', 'wMobileOAMDemand', 'wMobileOAMWriteCursor')}
                    m.execute('MobilePlanSeedResources')
                    seed_max_cycles = max(seed_max_cycles, m.cycles)
                    require(not m.carry, 'Resource prefix rejected a valid source/loan/row')
                    available = ~(used | protected) & 255
                    palette = 3 if not private else next(i for i in range(8) if available & 1 << i) if available else next(
                        (rotation + i) & 7 for i in range(8) if not protected & 1 << ((rotation + i) & 7))
                    require(m.read(address('wMobileAvatarPalette')) == palette, 'Prefix substituted a palette or used a protected loan')
                    require(m.read(address('wMobileVirtualCacheHit')) == (1 if private else 2), 'Prefix changed exact color provenance')
                    require(m.read(address('wMobileVirtualPaletteUsed')) == (1 | 1 << palette), 'Prefix did not reserve local color before the peer')
                    require(m.read(address('wMobileVirtualPaletteRotation')) == ((palette + 1) & 7 if private else rotation), 'Prefix changed pending palette successor')
                    rows = [0] * 16
                    rows[3] = 1 # local top56, native origin8
                    if 0 < peer_top < 160:
                        delta = max(0, peer_top - 8)
                        bucket = delta >> 4
                        rows[bucket] += 1
                        if bucket < 9 and delta & 15: rows[bucket + 1] += 1
                    require([m.read(address('wMobileVirtualLines') + 16 + i) for i in range(16)] == rows, 'Prefix lost the second row of an unaligned guest')
                    require(m.read(address('wMobileVirtualLines') + 14) == 13 and m.read(address('wMobileVirtualLines') + 15) == 8, 'Prefix overwrote its native alignment proof')
                    require(all(m.read(address(key)) == value for key, value in source_inputs.items()), 'Prefix overwrote gathered peer inputs')
                    require(m.read(address('wMobileVirtualLength')) == 32 and m.read(address('wMobileVirtualPlanCount')) == 1 and
                            m.read(address('wMobileRenderedCount')) == 1 and m.read(address('wMobileVirtualPlanPending') + 13) == 0,
                            'Prefix did not select the complete local/peer resource prefix')
                    require(m.banks[5][address('wMobileVirtualOutput') - 0xd000:address('wMobileVirtualOutput') - 0xd000 + 160] ==
                            before_banks[5][address('wMobileVirtualOutput') - 0xd000:address('wMobileVirtualOutput') - 0xd000 + 160], 'Resource prefix materialized pixels')
                    require(all(bytes(m.banks[b]) == before_banks[b] for b in range(8) if b != 5) and
                            m.memory[:0xcec4] == before_memory[:0xcec4], 'Resource prefix modified native or peer RAM')
                    seed_cases += 1
    print(f'Compiled resource prefix: {seed_cases} independent local-first/color/two-row/input-preservation vectors, max {seed_max_cycles} clocks')

    packed_classes = 0
    for count in range(13):
        for _ in range(24):
            m = Machine()
            order = rng.sample(range(1, 13), count)
            selected = set(rng.sample(order, rng.randrange(min(8, count) + 1)))
            # Unused class entries can be zero or old valid IDs. Neither may be
            # packed as a local player or a duplicate surviving native figure.
            for ordinal in range(12):
                m.write(address('wMobileVirtualOrderBase') + ordinal,
                        order[ordinal] if ordinal < count else rng.choice((0, *range(1, 13))))
            pending = address('wMobileVirtualPlanPending')
            key = bytes((0, 0, 0, 0, 0, 15, 76))
            for field, value in enumerate(key): m.write(pending + field, value)
            for actor in range(13): m.write(pending + 13 + actor, 0 if not actor else 32 if actor in selected else 255)
            for name, value in (('wMobileVirtualOrderCount', count), ('wMobileVirtualPlanCount', len(selected) + 1),
                                ('wMobileVirtualPlanAllNPC', 0), ('wMobileVirtualPlanEntriesUsed', 0),
                                ('wMobileRenderedCount', 1), ('wMobileBudgetIndex', 0), ('wMobileAvatarPalette', 7),
                                ('wMobileVirtualCacheHit', 1), ('wMobileVirtualNativeRotation', 0),
                                ('wMobileVirtualPaletteRotation', 0), ('wMobileVirtualNativeNext', 255), ('wMobileVirtualGroundNext', 255)):
                m.write(address(name), value)
            for slot in range(16): m.write(address('wMobileVirtualPlanEntries') + slot * 28 + 26, 0)
            before_memory, before_banks = m.snapshot()
            m.execute('MobilePlanComplete')
            entry = address('wMobileVirtualPlanEntries') + hash_slot(key) * 28
            require(m.read(entry + 26) == 1 and m.read(entry + 7) == 7 and
                    [m.read(entry + 13 + actor) for actor in range(13)] == [0 if not actor else 32 if actor in selected else 255 for actor in range(13)],
                    'Full payload credited unused class entries as local or stale native IDs')
            require(all(bytes(m.banks[b]) == before_banks[b] for b in range(8) if b != 5) and bytes(m.memory)[:0xcec4] == before_memory[:0xcec4],
                    'Full payload installation changed native game state')
            packed_classes += 1
    print(f'Compiled full payload installation:{packed_classes} sparse class lists with zero/stale unused entries passed')

    selection_cases = 0
    selection_max_cycles = 0
    for npc_count in (0, 4, 6, 12):
        for private in (0, 1):
            for _ in range(100):
                m = Machine()
                m.memory[0xff44] = 120
                def put(name, value): m.write(address(name), value)
                nr = rng.randrange(npc_count) if npc_count else 0
                ground_count = 12 - npc_count
                gr = rng.randrange(ground_count) if ground_count else 0
                order = list(range(1, npc_count + 1))
                order = order[nr:] + order[:nr]
                ground = list(range(npc_count + 1, 13))
                ground = ground[gr:] + ground[:gr]
                order += ground
                for ordinal, actor in enumerate(order): m.write(address('wMobileVirtualOrder') + ordinal, actor)
                for ordinal, actor in enumerate(range(1, 13)): m.write(address('wMobileVirtualOrderBase') + ordinal, actor)
                put('wMobileVirtualOrderCount', 12)
                put('wMobileVirtualNPCCount', npc_count)
                put('wMobileVirtualNativeRotation', nr)
                put('wMobileVirtualGroundRotation', gr)
                put('wMobileVirtualNativeNext', 255)
                put('wMobileVirtualGroundNext', 255)
                put('wMobileVirtualCursor', 0)
                put('wMobileVirtualLength', 32)
                put('wMobileVirtualPlanCount', 1)
                put('wMobileVirtualPlanEntriesUsed', 0)
                put('wMobileVirtualPlanSlot', 0)
                for slot in range(16): m.write(address('wMobileVirtualPlanEntries') + slot * 28 + 26, 0)
                put('wMobileVirtualPlanMode', 1)
                put('wMobileRenderedCount', 1)
                put('wMobileBudgetIndex', 0)
                put('wMobileVirtualPlanPending', nr)
                m.write(address('wMobileVirtualPlanPending') + 1, gr)
                m.write(address('wMobileVirtualPlanPending') + 2, 0)
                m.write(address('wMobileVirtualPlanPending') + 3, 0)
                for actor in range(13): m.write(address('wMobileVirtualPlanPending') + 13 + actor, 255)
                m.write(address('wMobileVirtualPlanPending') + 13, 0)
                loan = rng.randrange(1, 8)
                m.write(address('wMobileVirtualPlanPending') + 5, 15 if private else loan)
                put('wMobileAvatarPalette', loan)
                put('wMobileVirtualCacheHit', 1 if private else 2)
                put('wMobileVirtualPaletteRotation', (loan + 1) & 7 if private else 0)
                put('wMobileVirtualPlannedPrivateColors', private)
                put('wMobileVirtualDroppedSlot', loan)
                rows = [0] * 16
                rows[2] = 2 # the already admitted local player and urgent peer
                for bucket, count in enumerate(rows): m.write(address('wMobileVirtualLines') + 16 + bucket, count)
                sources = {}
                for actor in range(1, 13):
                    count = rng.choice((0, 4, 4, 4))
                    top, color = rng.choice((0, 8, 24, 40, 56, 72, 152, 168)), rng.randrange(8)
                    sources[actor] = count, top, color
                    base = address('wMobileVirtualPlanPreviousSignature') + actor * 3
                    for offset, value in enumerate((count | (32 if actor > npc_count else 0), top, color)):
                        m.write(base + offset, value)
                wanted, length, next_native, next_ground = [0], 32, 255, 255
                canonical = True
                examined = 0
                for ordinal, actor in enumerate(order):
                    if length >= 160: break
                    examined = ordinal + 1
                    count, top, color = sources[actor]
                    if not count or not 0 < top < 160 or private and color == loan: continue
                    if rows[top >> 4] >= 5:
                        if ordinal < npc_count: canonical = False
                        continue
                    rows[top >> 4] += 1
                    wanted.append(actor)
                    length += 16
                    if ordinal < npc_count and next_native == 255:
                        next_native = (nr + ordinal) % npc_count + 1
                    elif ordinal >= npc_count and next_ground == 255:
                        next_ground = (gr + ordinal - npc_count) % ground_count + 1
                canonical &= examined >= npc_count
                before_memory, before_banks = m.snapshot()
                output = address('wMobileVirtualOutput') - 0xd000
                m.execute('MobilePlanSelectNatives')
                selection_max_cycles = max(selection_max_cycles, m.cycles)
                base = address('wMobileVirtualPlanPending') + 13
                actual = [m.read(base + i) for i in range(13)]
                offsets = [255] * 13
                offsets[0] = 0
                for ordinal, actor in enumerate(wanted[1:]): offsets[actor] = 32 + ordinal * 16
                require(actual == offsets, 'Resource-only selector changed admitted native IDs or priority order')
                require(bool(m.read(address('wMobileVirtualPlanAllNPC'))) == canonical,
                        'Selector canonicalized an unproved NPC prefix or rejected a fully admitted class')
                slot = m.read(address('wMobileVirtualPlanSlot'))
                entry = address('wMobileVirtualPlanEntries') + slot * 28
                require(m.read(entry) == (255 if canonical else nr) and
                        [m.read(entry + 13 + actor) for actor in range(13)] == offsets and m.read(entry + 7) == loan and m.read(entry + 26) == 1,
                        'Cached key/successor does not match independently proved rotation invariance')
                require(m.read(address('wMobileVirtualNativeNext')) == next_native and m.read(address('wMobileVirtualGroundNext')) == next_ground,
                        'Resource-only selector changed the native fairness successors')
                require([m.read(address('wMobileVirtualLines') + 16 + i) for i in range(16)] == rows, 'Selector violated complete-figure row capacity')
                require(m.banks[5][output:output + 160] == before_banks[5][output:output + 160], 'Selection copied or changed pixel output')
                require(all(bytes(m.banks[b]) == before_banks[b] for b in range(8) if b != 5) and m.memory[:0xcec4] == before_memory[:0xcec4],
                        'Selection mutated native game state')
                require(m.read(address('wMobileVirtualStage')) == 9 and m.read(address('wMobileVirtualPlanEntriesUsed')) == 1, 'Selection did not commit a reusable plan')
                selection_cases += 1
    require(selection_max_cycles + 700 <= (144 - 134) * 912, 'Whole-selection bound no longer fits its earlier LY134 admission plus caller return margin')
    print(f'Compiled resource-only selector: {selection_cases} independent priority/row/color/capacity vectors passed, conservative max {selection_max_cycles} clocks')

    for previous_valid in (0, 1, 255):
        for caller_bank in (1, 2, 5, 7):
            m = Machine()
            m.memory[0xff70] = caller_bank
            m.banks[5][address('wMobileVirtualPlanSignatureValid') - 0xd000] = previous_valid
            m.banks[5][address('wMobileVirtualPlanMode') - 0xd000] = previous_valid
            before_banks = tuple(map(bytes, m.banks))
            regs = dict(m.regs)
            m.execute('MobileNativeResetLeases')
            for bank in range(8):
                expected = bytearray(before_banks[bank])
                if bank == 5:
                    expected[address('wMobileVirtualPlanSignatureValid') - 0xd000] = 0
                    expected[address('wMobileVirtualPlanMode') - 0xd000] = 0
                    for actor in range(12): expected[address('wMobileNativeShapeKeys') + actor * 6 + 5 - 0xd000] = 0
                require(m.banks[bank] == expected, 'Reset wrote the caller/native RAM bank')
            require(m.memory[0xff70] == caller_bank and m.regs == regs, 'Plan reset failed to preserve native register/bank state')
    print(f'Compiled admission metadata: {count_cases} bounded ID vectors, 16 slot vectors and 12 bank-preserving resets passed')
