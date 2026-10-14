#!/usr/bin/env python3
"""Execute exact native Reset/Release bytes with the production banked RAM layout."""
import argparse
from pathlib import Path
import random
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]


def require(condition, message):
    if not condition:
        raise AssertionError(message)


def fixture(original_clear=False):
    source = (ROOT / 'mobile_adapter/rom/virtual_actors.asm').read_text()
    reset = source[source.index('MobileNativeResetLeases:'):]
    if original_clear:
        needle = '; Native ownership survives map reloads until Release verifies its lifetime.\n'
        require(reset.count(needle) == 1, 'Original fence-clear mutation anchor missing')
        reset = reset.replace(needle, '\tld [wMobileNativeGraphicsFence], a\n')
    ownership = (ROOT / 'mobile_adapter/rom/graphics_ownership.asm').read_text()
    release = ownership[ownership.index('MobileNativeGraphicsRelease::'):]
    copy = (ROOT / 'home/copy.asm').read_text()
    fill = copy[copy.index('_ByteFill::'):copy.index('GetFarWord::')]
    lcd = ''
    for path, label in (('engine/pokedex/lcd.asm', 'PHB_LCDCode:'),
                        ('engine/pc/bills_pc_ui.asm', 'BillsPC_LCDCode:')):
        source = (ROOT / path).read_text()
        start = source.index('\n' + label) + 1
        end = source.index('\n.End:', start) + len('\n.End:')
        lcd += source[start:end] + '\n'
    return ('INCLUDE "ram.asm"\n'
            'SECTION "Reset ByteFill vector", ROM0[$28]\nByteFill::\n jp _ByteFill\n'
            'SECTION "Reset native ByteFill", ROM0[$1000]\n' + fill + '\n'
            'SECTION "Reset leases fixture", ROMX, BANK[MOBILE_ROM_BANK]\n' + reset + '\n'
            'SECTION "Reset release fixture", ROMX, BANK[MOBILE_ROM_BANK + 5]\n' + release + '\n'
            'SECTION \"Reset OAM layout\", ROM0[$1800]\nINCLUDE \"engine/gfx/load_push_oam.asm\"\n'
            + 'SECTION \"Reset LCD layouts\", ROMX, BANK[18]\n' + lcd
            + '; Unexecuted LCD layout dependencies.\nDEF CopyBytes EQU $20\nDEF Bankswitch EQU $8\nDEF DoNothing EQU $0\n'
            + 'DEF MobileIRQRuntimeStart EQU $d980\nDEF MobileIRQRuntimeEnd EQU $dad0\n')


def compile_fixture(rgbds, directory, original_clear=False):
    directory.mkdir()
    (directory / 'fixture.asm').write_text(fixture(original_clear))
    layout = (ROOT / 'layout.link').read_text()
    (directory / 'ram.link').write_text(layout[layout.index('VRAM $0'):])
    subprocess.run([str(rgbds / 'rgbasm'), '-Weverything', '-DMOBILE_ADAPTER', '-P', 'includes.asm',
                    '-o', str(directory / 'fixture.o'), str(directory / 'fixture.asm')], cwd=ROOT, check=True)
    subprocess.run([str(rgbds / 'rgblink'), '-Weverything', '-n', str(directory / 'fixture.sym'),
                    '-l', str(directory / 'ram.link'), '-o', str(directory / 'fixture.gb'),
                    str(directory / 'fixture.o')], cwd=ROOT, check=True)
    symbols = {}
    for line in (directory / 'fixture.sym').read_text().splitlines():
        match = re.fullmatch(r'([0-9a-fA-F]+):([0-9a-fA-F]+) (\S+)', line)
        if match:
            bank, address, name = match.groups()
            symbols[name] = (int(bank, 16), int(address, 16))
        else:
            match = re.fullmatch(r'([0-9a-fA-F]+) (wMobile\S+)', line)
            if match:
                address, name = match.groups()
                symbols[name] = (0, int(address, 16))
    return (directory / 'fixture.gb').read_bytes(), symbols


class Machine:
    """Bounded LR35902 runner; WRAM selector zero maps to physical bank one."""
    def __init__(self, rom, symbols, seed):
        rng = random.Random(seed)
        self.rom, self.symbols = rom, symbols
        self.memory = bytearray(rng.randbytes(65536))
        self.banks = [bytearray(rng.randbytes(4096)) for _ in range(8)]
        self.regs = {name: rng.randrange(256) for name in 'abcdehl'}
        self.flags = rng.randrange(16) << 4
        self.bank = 128
        self.writes = []

    def location(self, address):
        return ((self.memory[0xff70] & 7 or 1) if 0xd000 <= address < 0xe000 else 0, address)

    def read(self, address):
        if address < 0x4000:
            return self.rom[address]
        if address < 0x8000:
            return self.rom[self.bank * 0x4000 + address - 0x4000]
        bank, _ = self.location(address)
        return self.banks[bank][address - 0xd000] if bank else self.memory[address]

    def write(self, address, value):
        bank, _ = self.location(address)
        self.writes.append((bank, address, value))
        if bank:
            self.banks[bank][address - 0xd000] = value
        else:
            self.memory[address] = value

    def pair(self, name):
        return self.regs[name[0]] << 8 | (self.flags if name == 'af' else self.regs[name[1]])

    def put(self, name, value):
        self.regs[name[0]] = value >> 8 & 255
        if name == 'af':
            self.flags = value & 0xf0
        else:
            self.regs[name[1]] = value & 255

    def snapshot(self):
        return bytes(self.memory), tuple(map(bytes, self.banks))

    def execute(self, label):
        self.bank, pc = self.symbols[label]
        stack = []
        self.writes = []
        names = 'bcdehl?a'

        def fetch():
            nonlocal pc
            value = self.read(pc)
            pc += 1
            return value

        def word():
            return fetch() | fetch() << 8

        def condition(code):
            return (not self.flags & 128, bool(self.flags & 128),
                    not self.flags & 16, bool(self.flags & 16))[code]

        for _ in range(500):
            op = fetch()
            if 0x40 <= op < 0x80 and op != 0x76:
                source, target = op & 7, op >> 3 & 7
                value = self.read(self.pair('hl')) if source == 6 else self.regs[names[source]]
                if target == 6:
                    self.write(self.pair('hl'), value)
                else:
                    self.regs[names[target]] = value
            elif op & 0xcf == 0x01:
                self.put(('bc', 'de', 'hl', 'sp')[op >> 4 & 3], word())
            elif op & 0xc7 == 0x06:
                self.regs[names[op >> 3 & 7]] = fetch()
            elif op in (0xfa, 0xf0):
                self.regs['a'] = self.read(word() if op == 0xfa else 0xff00 + fetch())
            elif op in (0xea, 0xe0):
                self.write(word() if op == 0xea else 0xff00 + fetch(), self.regs['a'])
            elif op == 0x22:
                address = self.pair('hl')
                self.write(address, self.regs['a'])
                self.put('hl', address + 1)
            elif op in (0x04, 0x05, 0x0d):
                name = 'c' if op == 0x0d else 'b'
                old = self.regs[name]
                value = old + (1 if op == 0x04 else -1) & 255
                self.regs[name] = value
                self.flags = (self.flags & 16) | (128 if value == 0 else 0) | (0 if op == 0x04 else 64)
            elif op == 0xcb:
                require(fetch() == 0x39, 'Unexpected ByteFill shift')
                old = self.regs['c']
                self.regs['c'] >>= 1
                self.flags = (128 if self.regs['c'] == 0 else 0) | (16 if old & 1 else 0)
            elif op in (0xa7, 0xaf):
                if op == 0xaf:
                    self.regs['a'] = 0
                self.flags = (128 if self.regs['a'] == 0 else 0) | (32 if op == 0xa7 else 0)
            elif op in (0xc5, 0xd5, 0xe5, 0xf5):
                stack.append(self.pair(('bc', 'de', 'hl', 'af')[(op - 0xc5) // 16]))
            elif op in (0xc1, 0xd1, 0xe1, 0xf1):
                self.put(('bc', 'de', 'hl', 'af')[(op - 0xc1) // 16], stack.pop())
            elif op == 0xef:
                stack.append(pc)
                pc = 0x28
            elif op == 0xc3:
                pc = word()
            elif op in (0x18, 0x20, 0x28, 0x30, 0x38):
                delta = fetch()
                if op == 0x18 or condition((op - 0x20) // 8):
                    pc += delta if delta < 128 else delta - 256
            elif op in (0xc9, 0xc8):
                if op == 0xc9 or condition(1):
                    if not stack:
                        return
                    pc = stack.pop()
            else:
                raise AssertionError(f'Unsupported reset/release opcode {op:02x} at {pc - 1:04x}')
        raise AssertionError('Reset/release exceeded bounded instruction count')


def verify(rom, symbols):
    cases = 0
    def address(name):
        return symbols[name][1]

    def initialize_native(m, name, value):
        addr = address(name)
        if 0xd000 <= addr < 0xe000:
            require(symbols[name][0] == 1, f'Unexpected native bank for {name}')
            m.banks[1][addr - 0xd000] = value
        else:
            m.memory[addr] = value

    object_count = address('wMobileCameraX') - address('wMobileSavedFacings')
    require(object_count == 13, 'Native object count/layout changed; review reset fixture')
    invalidated = [(0, address('wMobileVirtualWindowOwners') + i, 0) for i in range(object_count)]
    invalidated += [(0, address(name), value) for name, value in (
        ('wMobileVirtualQueuedPeer', 0), ('wMobileVirtualForceFull', 0),
        ('wMobileVirtualBaseReady', 0), ('wMobileVirtualNeedColorAudit', 128))]
    invalidated += [(5, address('wMobileVirtualPlanSignatureValid'), 0),
                    (5, address('wMobileVirtualPlanMode'), 0)]
    invalidated += [(5, address('wMobileNativeShapeKeys') + i * 6 + 5, 0) for i in range(object_count - 1)]
    for selector in range(8):
        for fence in (1, 0):
            for menu in (0, 1):
                for script in (0, 1, 255):
                    for panning in (0, 1):
                        for animation in range(11):
                            m = Machine(rom, symbols, 0x72657365 + cases)
                            m.memory[0xff70] = selector
                            m.memory[address('wMobileNativeGraphicsFence')] = fence
                            m.memory[address('hInMenu')] = menu
                            for name, value in (('wScriptRunning', script), ('wPanningAroundTinyMap', panning)):
                                initialize_native(m, name, value)
                            for i in range(1, 11):
                                initialize_native(m, f'wSpriteAnim{i}', int(i == animation))
                            # All queued/source/cache fields start dirty, including the shape key validity bytes.
                            for bank, addr, _ in invalidated:
                                if bank:
                                    m.banks[bank][addr - 0xd000] = 0x5a
                                else:
                                    m.memory[addr] = 0xa5
                            expected_memory, expected_banks = m.snapshot()
                            expected_memory = bytearray(expected_memory)
                            expected_banks = list(map(bytearray, expected_banks))
                            for bank, addr, value in invalidated:
                                if bank:
                                    expected_banks[bank][addr - 0xd000] = value
                                else:
                                    expected_memory[addr] = value
                            before_regs, before_flags = dict(m.regs), m.flags
                            m.execute('MobileNativeResetLeases')
                            context = f'WBK={selector}, fence={fence}, menu={menu}, script={script}, panning={panning}, animation={animation}'
                            require(m.read(address('wMobileNativeGraphicsFence')) == fence,
                                    f'Reset cleared native ownership: {context}')
                            require(m.snapshot() == (bytes(expected_memory), tuple(map(bytes, expected_banks))),
                                    f'Reset changed native simulation or missed presentation invalidation: {context}')
                            require(m.regs == before_regs and m.flags == before_flags,
                                    f'Reset changed native registers/flags: {context}')
                            require(m.bank == symbols['MobileNativeResetLeases'][0], 'Reset changed ROM bank')
                            require(all(addr != address('wMobileNativeGraphicsFence') for _, addr, _ in m.writes),
                                    'Reset wrote the native ownership fence')
                            held = bool(menu or script or panning or animation)
                            m.execute('MobileNativeGraphicsRelease')
                            expected_memory[address('wMobileNativeGraphicsFence')] = fence if held else 0
                            require(m.snapshot() == (bytes(expected_memory), tuple(map(bytes, expected_banks))),
                                    f'Release changed native state or released active ownership: {context}')
                            require(m.regs['a'] == int(held) and all(m.regs[k] == before_regs[k] for k in 'bcdehl'),
                                    f'Release violated native ABI: {context}')
                            require(m.bank == symbols['MobileNativeGraphicsRelease'][0], 'Release changed ROM bank')
                            require(all(addr in (0xff70, address('wMobileNativeGraphicsFence')) for _, addr, _ in m.writes),
                                    'Release wrote outside its selector/fence contract')
                            cases += 1
    # One retained ownership lifetime, with every blocker removed in sequence.
    transitions = 0
    for selector in range(8):
        m = Machine(rom, symbols, 0x6c696665 + selector)
        m.memory[0xff70] = selector
        m.memory[address('wMobileNativeGraphicsFence')] = 1
        blockers = ['hInMenu', 'wScriptRunning', 'wPanningAroundTinyMap']
        blockers += [f'wSpriteAnim{i}' for i in range(1, 11)]
        for name in blockers:
            initialize_native(m, name, 255)
        m.execute('MobileNativeResetLeases')
        require(m.memory[address('wMobileNativeGraphicsFence')] == 1, 'Active reset lost ownership')
        for cleared in range(len(blockers) + 1):
            held = cleared < len(blockers)
            snapshot = m.snapshot()
            registers = dict(m.regs)
            m.execute('MobileNativeGraphicsRelease')
            expected_memory, expected_banks = snapshot
            expected_memory = bytearray(expected_memory)
            expected_memory[address('wMobileNativeGraphicsFence')] = int(held)
            require(m.snapshot() == (bytes(expected_memory), expected_banks),
                    'Progressive release changed native state or cleared ownership early')
            require(m.regs['a'] == int(held) and all(m.regs[k] == registers[k] for k in 'bcdehl'),
                    'Progressive release violated native ABI')
            require(m.memory[0xff70] == selector, 'Progressive release lost WRAM selector')
            if held:
                require(all(addr != address('wMobileNativeGraphicsFence') for _, addr, _ in m.writes),
                        'Held release wrote the native fence')
                initialize_native(m, blockers[cleared], 0)
            else:
                require((0, address('wMobileNativeGraphicsFence'), 0) in m.writes,
                        'Clean release lacks actual source fence-clear write')
            transitions += 1
    return cases, transitions


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rgbds', type=Path, required=True)
    parser.add_argument('--artifacts', type=Path)
    args = parser.parse_args()

    def run(directory):
        rom, symbols = compile_fixture(args.rgbds, directory / 'current')
        cases, transitions = verify(rom, symbols)
        print(f'Emitted native map reset/Release: {cases} bank/lifetime/invalidation/register cases and {transitions} progressive release returns passed')
        old_rom, old_symbols = compile_fixture(args.rgbds, directory / 'original-clear', True)
        try:
            verify(old_rom, old_symbols)
        except AssertionError as error:
            require(str(error).startswith('Reset cleared native ownership:'), f'Unexpected mutation failure: {error}')
            print(f'Original fence-clear mutation rejected: {error}')
        else:
            raise AssertionError('Original unconditional fence clear escaped the regression')

    if args.artifacts:
        args.artifacts.mkdir(parents=True, exist_ok=True)
        run(args.artifacts)
    else:
        with tempfile.TemporaryDirectory(prefix='native-reset-retention-') as directory:
            run(Path(directory))


if __name__ == '__main__':
    main()
