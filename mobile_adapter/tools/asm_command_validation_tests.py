#!/usr/bin/env python3
"""Execute linked production command preparation, including native recovery.

Bounded LR35902 instruction runner, not native emulator acceptance. No injected
interrupts; fixed LY is paired with a conservative zero partial-line reserve.
"""
import argparse
import hashlib
import json
from pathlib import Path
from probe_emulator import read_symbols
import random


def require(condition, message):
    if not condition:
        raise AssertionError(message)


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
        if label=="MobileTick.return":
            for value in (0,0,0,0,0):self.push(value)
        if label=="MobileNativeIdle.done":
            for value in (0,0,0,0,0,0):self.push(value)
        if label=="MobileNativeWait.done":
            for value in (0,0):self.push(value)
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
        for _ in range(10000):
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
            elif op in (0x13, 0x1b, 0x03, 0x0b, 0x23, 0x2b, 0x3b, 0x33):
                name = {0x13:'de',0x1b:'de',0x03:'bc',0x0b:'bc',0x23:'hl',0x2b:'hl',0x3b:'sp',0x33:'sp'}[op]
                self.put(name, self.pair(name) + (-1 if op in (0x1b,0x0b,0x2b,0x3b) else 1)); self.cycles += 8
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
            elif op & 0xc7 == 0xc7:
                self.push(self.pc); self.pc = op & 0x38; self.cycles += 16
            elif op in (0xc9, 0xc0, 0xc8, 0xd0, 0xd8):
                take = op == 0xc9 or condition((op - 0xc0) // 8)
                self.cycles += 16 if op == 0xc9 else 20 if take else 8
                if take:
                    self.pc = self.pop()
                    if self.pc == 0xffff:
                        require(self.sp == 0xc800, 'Command caller stack was not restored')
                        return self.cycles - start
            elif op == 0xc3:
                self.pc = word(); self.cycles += 16
            elif op in (0x18, 0x20, 0x28, 0x30, 0x38):
                delta = fetch(); take = op == 0x18 or condition((op - 0x20) // 8)
                self.cycles += 12 if take else 8
                if take:
                    self.pc += delta if delta < 128 else delta - 256
            elif 0x80 <= op <= 0xbf or op in (0xc6,0xce,0xd6,0xde,0xe6,0xee,0xf6,0xfe):
                a=self.r['a']; n=(op>>3)&7
                value=fetch() if op>=0xc0 else self.read(self.pair('hl')) if op&7==6 else self.r[names[op&7]]
                carry=bool(self.f&16)
                if n in (0,1):
                    total=a+value+(carry if n==1 else 0); result=total&255; self.f=(128 if result==0 else 0)|(16 if total>255 else 0)
                elif n in (2,3,7):
                    total=a-value-(carry if n==3 else 0); result=total&255; self.f=(128 if result==0 else 0)|64|(16 if total<0 else 0)
                else:
                    result= a&value if n==4 else a^value if n==5 else a|value
                    self.f=(128 if result==0 else 0)|(32 if n==4 else 0)
                if n!=7:self.r['a']=result
                self.cycles+=8 if op>=0xc0 or op&7==6 else 4
            elif op & 0xc7 in (0x04,0x05):
                target=op>>3&7; old=self.read(self.pair('hl')) if target==6 else self.r[names[target]]
                value=(old+(1 if op&7==4 else -1))&255
                if target==6:self.write(self.pair('hl'),value)
                else:self.r[names[target]]=value
                self.f=(self.f&16)|(128 if value==0 else 0)|(64 if op&7==5 else 0);self.cycles+=12 if target==6 else 4
            elif op in (0x09,0x19,0x29,0x39):
                total=self.pair('hl')+self.pair(('bc','de','hl','sp')[op>>4&3]); self.put('hl',total); self.f=(self.f&128)|(16 if total>65535 else 0); self.cycles+=8
            elif op==0xcb:
                cb=fetch();target=cb&7; value=self.read(self.pair('hl')) if target==6 else self.r[names[target]]
                if cb>>6==1:self.f=(self.f&16)|32|(128 if not value&(1<<(cb>>3&7)) else 0)
                elif cb>>3==7:self.f=(128 if value>>1==0 else 0)|(16 if value&1 else 0);value>>=1
                elif cb>>3==6:value=((value>>4)|(value<<4))&255;self.f=128 if value==0 else 0
                else:raise AssertionError(f'unsupported CB {cb:02x}')
                if cb>>6!=1:
                    if target==6:self.write(self.pair('hl'),value)
                    else:self.r[names[target]]=value
                self.cycles+=16 if target==6 else 8
            elif op in (0x12,0x1a):
                if op==0x12:self.write(self.pair('de'),self.r['a'])
                else:self.r['a']=self.read(self.pair('de'))
                self.cycles+=8
            elif op in (0xf3,0xfb,0x00,0x37):
                if op in (0xf3,0xfb):self.ime=(op==0xfb)
                if op==0x37:self.f=(self.f&128)|16
                self.cycles+=4
            elif op in (0xc2,0xca,0xd2,0xda):
                target=word();take=condition((op-0xc2)//8);self.cycles+=16 if take else 12
                if take:self.pc=target
            elif op in (0xc4,0xcc,0xd4,0xdc):
                target=word();take=condition((op-0xc4)//8);self.cycles+=24 if take else 12
                if take:self.push(self.pc);self.pc=target
            else:
                raise AssertionError(f'Unsupported emitted command opcode {op:02x} at {self.pc - 1:04x}')
        raise AssertionError('Command caller exceeded the bounded instruction runner')



class CommandMachine(Machine):
    def __init__(self, rom, symbols):
        super().__init__(rom, symbols, 0)
        self.memory = bytearray(65536)
        self.banks = [bytearray(4096) for _ in range(8)]
        self.memory[0xff70] = 2
        self.bank = symbols['MobilePrepareCommand'][0]
        self.memory[self.address('hROMBank')] = self.bank
        self.memory[0xff44] = 129
        self.reads, self.writes = [], []
        self.tracing = False
        self.ime = False
        for field, value in (('wMobileOwner', 1), ('wMobileOnline', 1),
                             ('wMobilePeerCount', 3), ('wMobileBurst', 16),
                             ('wMobileWanted', 1), ('wMobileEmergencyPending', 1),
                             ('wMobileSavedSerialIE', 8), ('wMobileSavedSB', 0x57),
                             ('wMobileSavedSC', 0x83)):
            self.set(field, value)
        for field in ('wMobileGuardStart', 'wMobileGuardTx', 'wMobileGuardRx', 'wMobileGuardEnd'):
            self.set(field, 0xa5)
        self.memory[0xffff] = 0x15
        self.memory[0xff0f] = 0x1f
        for offset in range(128):
            self.write(self.address('wMobileOutgoing') + offset, offset ^ 0x5a)

    def set(self, name, value):
        self.write(self.address(name), value)

    def read(self, address):
        if getattr(self, 'tracing', False):
            self.reads.append(address)
        return super().read(address)

    def write(self, address, value):
        if getattr(self, 'tracing', False):
            self.writes.append((address, value))
        super().write(address, value)

    def run(self):
        wanted = self.read(self.address('wMobileWanted'))
        self.tracing = True
        clocks = self.execute('MobilePrepareCommand')
        self.tracing = False
        require(self.bank == self.symbols['MobilePrepareCommand'][0], 'Caller ROM bank changed')
        require(self.memory[0xff70] == 2, 'Caller WRAM bank changed')
        for field in ('wMobileGuardStart', 'wMobileGuardTx', 'wMobileGuardRx', 'wMobileGuardEnd'):
            require(self.read(self.address(field)) == 0xa5, 'Packet guard overwritten: ' + field)
        require(self.read(self.address('wMobileEmergencyPending')) == 1, 'Physical exit intent lost')
        require(self.read(self.address('wMobileWanted')) == wanted, 'Reconnect intent changed')
        return clocks

    def assert_released(self):
        for field in ('wMobileOwner', 'wMobileTransferActive', 'wMobileWire', 'wMobileOnline', 'wMobilePeerCount'):
            require(self.read(self.address(field)) == 0, 'Fault did not release ' + field)
        require(self.ime and self.f == 0x20, 'Recovery EI/flags convention changed')
        require(self.read(self.address('wMobileError')) == 3, 'Wrong packet error')
        require(self.read(self.address('wMobileCooldown')) != 0, 'Missing abort cooldown')
        require(self.memory[0xffff] == 0x1d and self.memory[0xff0f] == 0x17, 'Serial IE/IF restoration changed')
        require(self.memory[0xff01] == 0x57 and self.memory[0xff02] == 3, 'Saved serial state was not restored safely')
        require(not any(address == 0xff02 and value & 0x80 for address, value in self.writes), 'Fault launched serial transfer')
        tx = self.address('wMobileTx')
        require(not any(tx <= address < tx + 256 for address, value in self.writes), 'Fault wrote packet buffer')
        outgoing = self.address('wMobileOutgoing')
        require(not any(outgoing <= address < outgoing + 128 for address in self.reads), 'Fault copied outgoing payload')


def verify(rom, symbols):
    maximum, fault_maximum, cases = 0, 0, 0
    for length in range(256):
        machine = CommandMachine(rom, symbols)
        for field, value in (('wMobileStep', 4), ('wMobileMode', 1),
                             ('wMobilePhase', 0), ('wMobileOutgoingLength', length)):
            machine.set(field, value)
        clocks = machine.run()
        if 8 <= length <= 128:
            require(machine.read(machine.address('wMobileOwner')) == 1, 'Valid length released ownership')
            require(machine.read(machine.address('wMobileTxSize')) == length + 13, 'Valid payload/header size changed')
            payload = bytes(machine.read(machine.address('wMobileTx') + 7 + i) for i in range(length))
            require(payload == bytes(i ^ 0x5a for i in range(length)), 'Valid payload changed')
            require(any(address == 0xff02 and value & 0x80 for address, value in machine.writes), 'Valid request did not launch')
            maximum = max(maximum, clocks)
        else:
            machine.assert_released()
            fault_maximum = max(fault_maximum, clocks)
        cases += 1
    count = (symbols['MobileCommandsEnd'][1] - symbols['MobileCommands'][1]) // 4
    require(count == 9, 'Unexpected production command count')
    for step in range(256):
        machine = CommandMachine(rom, symbols)
        for field, value in (('wMobileStep', step), ('wMobileMode', 1), ('wMobileOutgoingLength', 128)):
            machine.set(field, value)
        clocks = machine.run()
        if step >= count:
            machine.assert_released()
            table = machine.address('MobileCommands')
            require(not any(table <= address < table + count * 4 for address in machine.reads), 'Invalid step indexed command table')
            fault_maximum = max(fault_maximum, clocks)
        else:
            require(machine.read(machine.address('wMobileOwner')) == 1, 'Valid step rejected')
            maximum = max(maximum, clocks)
        cases += 1
    # Meaningful diagnostic, HELLO, UPDATE, POLL and disconnect preparation branches.
    for step in range(count):
        for mode in (0, 1):
            for phase in (0, 1, 2):
                for wanted in (0, 1):
                    for cooldown in (0, 1):
                        machine = CommandMachine(rom, symbols)
                        for field, value in (('wMobileStep', step), ('wMobileMode', mode),
                                             ('wMobilePhase', phase), ('wMobileWanted', wanted),
                                             ('wMobileSendCooldown', cooldown), ('wMobileOutgoingLength', 128)):
                            machine.set(field, value)
                        clocks = machine.run()
                        maximum = max(maximum, clocks)
                        cases += 1
    # Repeated diagnostic ping and already-sent HELLO both choose shorter paths.
    for mode in (0, 1):
        for phase in (0, 1, 2):
            machine = CommandMachine(rom, symbols)
            for field, value in (('wMobileStep', 4), ('wMobileMode', mode),
                                 ('wMobilePhase', phase), ('wMobilePingSent', 1),
                                 ('wMobileSendCooldown', 1), ('wMobileOutgoingLength', 128)):
                machine.set(field, value)
            maximum = max(maximum, machine.run())
            cases += 1
    # Existing admission window and deadline are unchanged. Charge the complete
    # immediate Tick return, CALL and jumps, with zero partial scanline reserve.
    immediate = maximum + 12 + 8 + 12 + 24 + 16 + 168
    require(immediate < 14 * 912, 'Prepare plus immediate Tick return crosses LY144')
    return {'cases': cases, 'prepare_max_clocks': maximum, 'fault_max_clocks': fault_maximum,
            'immediate_tick_clocks': immediate, 'remaining_full_line_clocks': 14 * 912 - immediate,
            'partial_scanline_reserve': 0,
            'outer_return_proved': False, 'interrupts_injected': False}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rom', required=True, type=Path)
    parser.add_argument('--sym', required=True, type=Path)
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    rom = args.rom.read_bytes()
    result = verify(rom, read_symbols(args.sym))
    result.update(rom_sha256=hashlib.sha256(rom).hexdigest(),
                  sym_sha256=hashlib.sha256(args.sym.read_bytes()).hexdigest())
    rendered = json.dumps(result, indent=2) + '\n'
    if args.output:
        args.output.write_text(rendered)
    print(rendered, end='')


if __name__ == '__main__':
    main()
