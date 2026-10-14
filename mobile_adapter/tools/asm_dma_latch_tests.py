#!/usr/bin/env python3
"""Compile and execute only the production actual-DMA latch and caller tails."""
import argparse
import json
from pathlib import Path
import random
import re
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]


def require(condition, message):
    if not condition:
        raise AssertionError(message)


def fixture(latch=True, disabled=False):
    video = (ROOT / 'home/video.asm').read_text().split('\nif DEF(MOBILE_ADAPTER)\n\tmobile_native_rectangle_trampoline')[0]
    renderer = (ROOT / 'mobile_adapter/rom/virtual_renderer.asm').read_text()
    renderer = renderer[renderer.index('MobileVirtualFrameValid:'):renderer.index('MobileVirtualCopyRawRectangle:')]
    area = (ROOT / 'engine/pokedex/area.asm').read_text()
    start = area.index('\tldh a, [rWBK]\n\tpush af\n\tld a, BANK(wDexAreaShadowOAM)')
    area = area[start:area.index('\nPHB_WriteNestOAM_FirstRun:', start)]
    far = (ROOT / 'home/farcall.asm').read_text()
    far = far[far.index('_RstFarCall::'):far.index('FarPointerCall::')] + far[far.index('_ReturnFarCall:'):far.index('FarCall_de::')]
    oam = (ROOT / 'engine/gfx/load_push_oam.asm').read_text()
    oam = oam[oam.index('OAMDMACode:'):]
    lcd = ''
    for path, label in (('engine/pokedex/lcd.asm', 'PHB_LCDCode:'),
                        ('engine/pc/bills_pc_ui.asm', 'BillsPC_LCDCode:')):
        source = (ROOT / path).read_text()
        start = source.index('\n' + label) + 1
        end = source.index('\n.End:', start) + len('\n.End:')
        lcd += source[start:end] + '\n'
    if not latch:
        video = video.replace('\tmobile_actual_native_oam_setup', '\tlb bc, 41, LOW(rDMA)\n\tld a, HIGH(wShadowOAM)')
        renderer = renderer.replace('\tmobile_actual_oam_dma_latch b\n', '')
        area = area.replace('\tmobile_actual_oam_dma_latch\n', '')
    return ('INCLUDE "ram.asm"\nSECTION "DMA fixture", ROM0[$1000]\n'
            + ('PURGE MOBILE_ADAPTER\n' if disabled else '') + video + '\nPushOAMFixtureEnd::\n'
            + '\nDexFixture::\n push hl\n push de\n push bc\n' + area
            + ('DEF MOBILE_ADAPTER EQU 1\n' if disabled else '')
            + '\nPopBCDEHL::\n pop bc\n pop de\n pop hl\n ret\n'
            + '\nSECTION "DMA farcall fixture", ROM0[$1800]\n' + far
            + '\nSECTION "DMA bank switch", ROM0[$8]\nBankswitch::\n ldh [hROMBank], a\n ld [rROMB], a\n ret\nDoNothing:: ret\n'
            + '\nSECTION "DMA farcall vector", ROM0[$10]\nFarCall::\n dec sp\n call _RstFarCall\n jp _ReturnFarCall\n'
            + '\nSECTION "DMA load image", ROM0[$2000]\n' + oam
            + '\nSECTION "DMA private fixture", ROMX, BANK[128]\n' + renderer + '\nMobileVirtualPushOAMFixtureEnd::\n'
            + '\nSECTION "DMA LCD layouts", ROMX, BANK[18]\n' + lcd
            # Unexecuted serial IRQ layout dependencies, matching production.
            + '\nDEF MobileIRQRuntimeStart EQU $d980\nDEF MobileIRQRuntimeEnd EQU $dad0\nDEF CopyBytes EQU $20\n')


def compile_fixture(rgbds, directory, latch=True, disabled=False):
    directory.mkdir()
    (directory / 'fixture.asm').write_text(fixture(latch, disabled))
    layout = (ROOT / 'layout.link').read_text()
    (directory / 'ram.link').write_text(layout[layout.index('VRAM $0'):])
    subprocess.run([str(rgbds / 'rgbasm'), '-Weverything', '-DMOBILE_ADAPTER', '-P', 'includes.asm',
                    '-o', str(directory / 'fixture.o'), str(directory / 'fixture.asm')], cwd=ROOT, check=True)
    subprocess.run([str(rgbds / 'rgblink'), '-Weverything', '-n', str(directory / 'fixture.sym'),
                    '-m', str(directory / 'fixture.map'), '-l', str(directory / 'ram.link'), '-o', str(directory / 'fixture.gb'),
                    str(directory / 'fixture.o')], cwd=ROOT, check=True)
    symbols = {}
    for line in (directory / 'fixture.sym').read_text().splitlines():
        match = re.match(r'([0-9a-fA-F]+):([0-9a-fA-F]+) (\S+)$', line)
        if match:
            bank, address, name = match.groups()
            symbols[name] = (int(bank, 16), int(address, 16))
        else:
            match = re.match(r'([0-9a-fA-F]+) (wMobile\S+)$', line)
            if match:
                address, name = match.groups()
                symbols[name] = (0, int(address, 16))
    return (directory / 'fixture.gb').read_bytes(), symbols


class Machine:
    def __init__(self, rom, symbols, seed):
        rng = random.Random(seed)
        self.rom, self.symbols = rom, symbols
        self.memory = bytearray(rng.randbytes(65536))
        self.banks = [bytearray(rng.randbytes(4096)) for _ in range(8)]
        self.r = {name: rng.randrange(256) for name in 'abcdehl'}
        self.f = rng.randrange(16) << 4
        self.sp, self.bank, self.cycles, self.transfers = 0xc800, 1, 0, []
        source = symbols['OAMDMACode'][1]
        target = symbols['hPushOAM'][1]
        size = symbols['OAMDMACode.End'][1] - source
        self.memory[target:target + size] = rom[source:source + size]

    def address(self, name):
        return self.symbols[name][1]

    def read(self, address):
        if address < 0x4000:
            return self.rom[address]
        if address < 0x8000:
            return self.rom[self.bank * 0x4000 + address - 0x4000]
        if 0xd000 <= address < 0xe000:
            return self.banks[self.memory[0xff70] & 7 or 1][address - 0xd000]
        return self.memory[address]

    def write(self, address, value):
        if address == 0x2000:
            self.bank = value
        elif 0xd000 <= address < 0xe000:
            self.banks[self.memory[0xff70] & 7 or 1][address - 0xd000] = value
        else:
            self.memory[address] = value
        if address == 0xff46:
            self.transfers.append((value, self.memory[self.address('hVBlankCounter')], self.memory[0xff70]))

    def pair(self, name):
        if name == 'sp':
            return self.sp
        if name == 'af':
            return self.r['a'] << 8 | self.f
        return self.r[name[0]] << 8 | self.r[name[1]]

    def put(self, name, value):
        value &= 65535
        if name == 'sp':
            self.sp = value
        elif name == 'af':
            self.r['a'], self.f = value >> 8, value & 0xf0
        else:
            self.r[name[0]], self.r[name[1]] = value >> 8, value & 255

    def push(self, value):
        self.sp -= 2
        self.write(self.sp, value & 255)
        self.write(self.sp + 1, value >> 8)

    def pop(self):
        value = self.read(self.sp) | self.read(self.sp + 1) << 8
        self.sp += 2
        return value

    def execute(self, label):
        self.pc = self.address(label)
        self.push(0xffff)
        start = self.cycles
        names = 'bcdehl?a'
        def fetch():
            value = self.read(self.pc)
            self.pc += 1
            return value
        def word():
            return fetch() | fetch() << 8
        def condition(index):
            return (not self.f & 128, bool(self.f & 128), not self.f & 16, bool(self.f & 16))[index]
        for _ in range(3000):
            op = fetch()
            if 0x40 <= op < 0x80:
                source, target = op & 7, op >> 3 & 7
                value = self.read(self.pair('hl')) if source == 6 else self.r[names[source]]
                if target == 6:
                    self.write(self.pair('hl'), value)
                else:
                    self.r[names[target]] = value
                self.cycles += 8 if 6 in (source, target) else 4
            elif op & 0xcf == 0x01:
                self.put(('bc', 'de', 'hl', 'sp')[op >> 4 & 3], word()); self.cycles += 12
            elif op & 0xc7 == 0x06:
                target, value = op >> 3 & 7, fetch()
                if target == 6:
                    self.write(self.pair('hl'), value)
                else:
                    self.r[names[target]] = value
                self.cycles += 12 if target == 6 else 8
            elif op in (0xfa, 0xf0):
                self.r['a'] = self.read(word() if op == 0xfa else 0xff00 + fetch()); self.cycles += 16 if op == 0xfa else 12
            elif op in (0xea, 0xe0):
                self.write(word() if op == 0xea else 0xff00 + fetch(), self.r['a']); self.cycles += 16 if op == 0xea else 12
            elif op == 0xe2:
                self.write(0xff00 + self.r['c'], self.r['a']); self.cycles += 8
            elif op in (0x22, 0x2a):
                address = self.pair('hl')
                if op == 0x22:
                    self.write(address, self.r['a'])
                else:
                    self.r['a'] = self.read(address)
                self.put('hl', address + 1); self.cycles += 8
            elif op in (0x23, 0x3b, 0x33):
                name = 'hl' if op == 0x23 else 'sp'
                self.put(name, self.pair(name) + (-1 if op == 0x3b else 1)); self.cycles += 8
            elif op == 0xf8:
                value = fetch(); offset = value if value < 128 else value - 256
                self.f = (32 if (self.sp & 15) + (value & 15) > 15 else 0) | (16 if (self.sp & 255) + value > 255 else 0)
                self.put('hl', self.sp + offset); self.cycles += 12
            elif op in (0xc5, 0xd5, 0xe5, 0xf5):
                self.push(self.pair(('bc', 'de', 'hl', 'af')[(op - 0xc5) // 16])); self.cycles += 16
            elif op in (0xc1, 0xd1, 0xe1, 0xf1):
                self.put(('bc', 'de', 'hl', 'af')[(op - 0xc1) // 16], self.pop()); self.cycles += 12
            elif op == 0xcd:
                target = word(); self.push(self.pc); self.pc = target; self.cycles += 24
            elif op in (0xcf, 0xd7):
                self.push(self.pc); self.pc = op & 0x38; self.cycles += 16
            elif op in (0xc9, 0xc0, 0xc8):
                take = op == 0xc9 or condition((op - 0xc0) // 8)
                self.cycles += 16 if op == 0xc9 else 20 if take else 8
                if take:
                    self.pc = self.pop()
                    if self.pc == 0xffff:
                        require(self.sp == 0xc800, 'DMA caller stack was not restored')
                        return self.cycles - start
            elif op == 0xc3:
                self.pc = word(); self.cycles += 16
            elif op in (0x18, 0x20, 0x28, 0x30, 0x38):
                delta = fetch(); take = op == 0x18 or condition((op - 0x20) // 8)
                self.cycles += 12 if take else 8
                if take:
                    self.pc += delta if delta < 128 else delta - 256
            elif op in (0xa7, 0xaf, 0xe6, 0xfe, 0xb8, 0x87):
                a = self.r['a']; value = fetch() if op in (0xe6, 0xfe) else self.r['b'] if op == 0xb8 else a
                if op in (0xfe, 0xb8):
                    self.f = (128 if a == value else 0) | 64 | (32 if (a & 15) < (value & 15) else 0) | (16 if a < value else 0)
                else:
                    result = a * 2 & 255 if op == 0x87 else 0 if op == 0xaf else a & value
                    self.r['a'] = result
                    self.f = (128 if result == 0 else 0) | (16 if op == 0x87 and a >= 128 else 0) | (32 if op in (0xa7, 0xe6) or op == 0x87 and (a & 15) >= 8 else 0)
                self.cycles += 8 if op in (0xe6, 0xfe) else 4
            elif op == 0x05:
                old = self.r['b']; self.r['b'] = old - 1 & 255
                self.f = (self.f & 16) | 64 | (128 if self.r['b'] == 0 else 0) | (32 if old & 15 == 0 else 0); self.cycles += 4
            elif op == 0xcb:
                require(fetch() == 0xba, 'Unexpected far-jump bit instruction')
                self.r['d'] &= 127; self.cycles += 8
            else:
                raise AssertionError(f'Unsupported emitted DMA opcode {op:02x} at {self.pc - 1:04x}')
        raise AssertionError('DMA caller exceeded the bounded instruction runner')


def verify(enabled, baseline, disabled, disabled_baseline):
    costs, cases = {}, 0
    rng = random.Random(0x646d61)
    gates = ('wMobileNativeGraphicsFence', 'wMobileWanted', 'wMobileOnline', 'wMobileLocalValid', 'hInMenu', 'hVBlank', 'wMobileVirtualHeaderOAM')
    variants = [('raw', 0, None), ('private', 1, None), ('private_nonzero_token', 255, None)] + [('reject_' + gate, 1, gate) for gate in gates]
    for name, token, reject in variants:
        for frame in (0, 1, 127, 254, 255):
            for wbk in range(8):
                seed = rng.randrange(2**32)
                current, control = (Machine(*image, seed) for image in (enabled, baseline))
                for machine in (current, control):
                    for gate in gates:
                        machine.memory[machine.address(gate)] = 0
                    for gate in ('wMobileWanted', 'wMobileOnline', 'wMobileLocalValid'):
                        machine.memory[machine.address(gate)] = 1
                    machine.memory[machine.address('wMobileVirtualHeaderOAM')] = frame
                    machine.memory[machine.address('hVBlankCounter')] = frame
                    machine.memory[machine.address('wMobileVirtualPreviousOAM')] = token
                    machine.memory[machine.address('hOAMUpdate')] = 0
                    machine.memory[0xff70] = wbk
                    machine.memory[machine.address('hROMBank')] = machine.bank
                    if reject:
                        value = 1 if reject in ('wMobileNativeGraphicsFence', 'hInMenu', 'hVBlank') else (frame + 1) & 255 if reject == 'wMobileVirtualHeaderOAM' else 0
                        machine.memory[machine.address(reject)] = value
                c1, c0 = current.execute('PushOAM'), control.execute('PushOAM')
                expected = current.address('wMobileVirtualOutput') >> 8 if name.startswith('private') else current.address('wShadowOAM') >> 8
                dma_bank = 5 if name.startswith('private') else 2 if token else wbk
                require(current.transfers == control.transfers == [(expected, frame, dma_bank)], f'{name}: wrong actual source/frame/bank')
                require(current.memory[current.address('wMobileActualOAMPage')] == expected and current.memory[current.address('wMobileActualOAMFrame')] == frame, f'{name}: stamp disagrees with DMA')
                require(current.r == control.r and current.f == control.f and current.bank == control.bank, f'{name}: native register/flag/bank convention changed')
                require(current.banks == control.banks, f'{name}: banked native memory changed')
                differences = {i for i, (a, b) in enumerate(zip(current.memory, control.memory)) if a != b}
                # Far-call return addresses differ because the fixture's emitted
                # wrapper gains ten bytes. Dead stack bytes are not live state.
                allowed = {current.address('wMobileActualOAMPage'), current.address('wMobileActualOAMFrame')} | set(range(0xc780, 0xc800))
                require(differences <= allowed, f'{name}: unexpected resident mutation {differences - allowed}')
                require(c1 - c0 == 52, f'{name}: unpriced latch clocks')
                costs[name] = {'baseline': c0, 'latched': c1, 'added': c1 - c0}
                require(current.memory[0xff70] == wbk, f'{name}: WBK was not restored')
                require(current.memory[current.address('wMobileVirtualPreviousOAM')] == 0, f'{name}: token not consumed')
                # A skipped PushOAM does not consume a token or touch the stamp.
                current.memory[current.address('hOAMUpdate')] = 1
                current.memory[current.address('wMobileVirtualPreviousOAM')] = 1
                before = bytes(current.memory)
                n = len(current.transfers)
                skip = current.execute('PushOAM')
                require(skip == 36 and len(current.transfers) == n, 'Suppressed DMA executed')
                for field in ('wMobileActualOAMPage', 'wMobileActualOAMFrame', 'wMobileVirtualPreviousOAM'):
                    require(current.memory[current.address(field)] == before[current.address(field)], 'Suppressed DMA changed stamp/token')
                # Force ignores hOAMUpdate, latest actual transfer replaces stamp,
                # and counter rollover is simply the actual byte sampled.
                current.memory[current.address('wMobileVirtualPreviousOAM')] = 0
                current.memory[current.address('hVBlankCounter')] = (frame + 1) & 255
                current.execute('ForcePushOAM')
                require(current.memory[current.address('wMobileActualOAMFrame')] == (frame + 1) & 255, 'Repeated Force/counter wrap lost actual stamp')
                cases += 1
    for seed in range(256):
        current, control = (Machine(*image, seed) for image in (enabled, baseline))
        for machine in (current, control):
            machine.memory[machine.address('hVBlankCounter')] = seed
            machine.memory[0xff70] = seed % 8
        c1, c0 = current.execute('DexFixture'), control.execute('DexFixture')
        require(current.transfers == control.transfers == [(current.address('wDexAreaShadowOAM') >> 8, seed, 3)], 'Direct dex DMA page/frame/bank changed')
        require(current.r == control.r and current.f == control.f and current.memory[0xff70] == seed % 8, 'Direct dex native registers/flags/WBK changed')
        require(current.memory[current.address('wMobileActualOAMPage')] == current.address('wDexAreaShadowOAM') >> 8 and current.memory[current.address('wMobileActualOAMFrame')] == seed, 'Dex DMA stamp incorrect')
        require(c1 - c0 == 72, 'Direct dex latch cost changed')
        costs['dex_tail'] = {'baseline': c0, 'latched': c1, 'added': c1 - c0}
        cases += 1
    rom, symbols = disabled
    dex_start, dex_end = symbols['DexFixture'][1], symbols['PopBCDEHL'][1] + 4
    require(rom[dex_start:dex_end] == disabled_baseline[0][dex_start:dex_end], 'Disabled direct dex DMA bytes changed')
    raw = symbols['ForcePushOAM'][1]
    target = symbols['hPushOAM'][1]
    expected = bytes([0x01, 0x46, 41, 0x3e, symbols['wShadowOAM'][1] >> 8, 0xc3, target & 255, target >> 8])
    require(rom[raw:raw + len(expected)] == expected, 'Disabled ForcePushOAM bytes changed')
    require(symbols['wMobileActualOAMPage'][1] == 0xcf39 and symbols['wMobileActualOAMFrame'][1] == 0xcf3a, 'Resident latch allocation moved')
    require(enabled[1]['ForcePushOAM'][1] - enabled[1]['PushOAM'][1] == 4, 'hOAMUpdate suppression gate changed')
    emitted_growth = {}
    for name, start, end in (('raw_rom0', 'PushOAM', 'PushOAMFixtureEnd'),
                             ('private_upper_rom', 'MobileVirtualPushOAM', 'MobileVirtualPushOAMFixtureEnd'),
                             ('dex_lower_rom', 'DexFixture', 'PopBCDEHL')):
        current_size = enabled[1][end][1] - enabled[1][start][1]
        baseline_size = baseline[1][end][1] - baseline[1][start][1]
        emitted_growth[name] = current_size - baseline_size
        require(current_size - baseline_size == 10, f'{name}: unexpected emitted latch size')
    for suppressed in range(1, 256):
        machine = Machine(*enabled, suppressed)
        machine.memory[machine.address('hOAMUpdate')] = suppressed
        before = bytes(machine.memory)
        require(machine.execute('PushOAM') == 36 and not machine.transfers, 'Nonzero hOAMUpdate was not suppressed')
        for field in ('wMobileActualOAMPage', 'wMobileActualOAMFrame', 'wMobileVirtualPreviousOAM'):
            require(machine.memory[machine.address(field)] == before[machine.address(field)], 'Suppressed DMA touched stamp or publication')
        cases += 1
    return {'cases': cases, 'costs_t_clocks': costs, 'emitted_growth_bytes': emitted_growth,
            'resident_growth_bytes': 2, 'disabled_raw_bytes': expected.hex(),
            'disabled_dex_tail_bytes': rom[dex_start:dex_end].hex(),
            'native_cadence': 'Not run. Coordinator four-core validation required.'}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rgbds', type=Path, required=True)
    parser.add_argument('--output', type=Path)
    parser.add_argument('--artifacts', type=Path, help='Retain focused assembly images, symbols and maps')
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix='mobile-dma-latch-') as temp:
        directory = Path(temp)
        result = verify(compile_fixture(args.rgbds, directory / 'enabled'),
                        compile_fixture(args.rgbds, directory / 'baseline', latch=False),
                        compile_fixture(args.rgbds, directory / 'disabled', disabled=True),
                        compile_fixture(args.rgbds, directory / 'disabled_baseline', latch=False, disabled=True))
        if args.artifacts:
            shutil.copytree(directory, args.artifacts, dirs_exist_ok=True)
        if args.output:
            args.output.write_text(json.dumps(result, indent=2) + '\n')
        print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
