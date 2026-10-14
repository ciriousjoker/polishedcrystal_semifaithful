"""Bounded instruction runner for retained-source emitted checks.

Derived from the existing DMA runner and the selected producer proof runner.
This is a functional CPU subset, not an emulator or timing acceptance oracle.
"""
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
        for _ in range(100000):
            op = fetch()
            if op in (0x02,0x12,0x0a,0x1a):
                address=self.pair('bc' if op in (0x02,0x0a) else 'de')
                if op in (0x0a,0x1a): self.r['a']=self.read(address)
                else:self.write(address,self.r['a'])
                self.cycles+=8
            elif op & 0xc7 in (0x04,0x05):
                i=op>>3&7;old=self.read(self.pair('hl')) if i==6 else self.r[names[i]];down=op&1
                result=(old+(-1 if down else 1))&255
                if i==6:self.write(self.pair('hl'),result)
                else:self.r[names[i]]=result
                self.f=(self.f&16)|(64 if down else 0)|(128 if result==0 else 0)|(32 if (old&15)==(0 if down else 15) else 0);self.cycles+=12 if i==6 else 4
            elif op in (0x03,0x0b):
                self.put('bc',self.pair('bc')+(-1 if op==0x0b else 1));self.cycles+=8
            elif op in (0x2b,0x13,0x1b):
                name='hl' if op==0x2b else 'de';self.put(name,self.pair(name)+(-1 if op!=0x13 else 1));self.cycles+=8
            elif op in (0x09,0x19,0x29):
                old=self.pair('hl');value=self.pair({9:'bc',25:'de',41:'hl'}[op]);result=old+value
                self.put('hl',result);self.f=(self.f&128)|(32 if (old&4095)+(value&4095)>4095 else 0)|(16 if result>65535 else 0);self.cycles+=8
            elif op == 0xe9:self.pc=self.pair('hl');self.cycles+=4
            elif op == 0x2f:self.r['a']^=255;self.f|=96;self.cycles+=4
            elif op == 0x37:self.f=(self.f&128)|16;self.cycles+=4
            elif op == 0xcb:
                sub=fetch();i=sub&7;value=self.read(self.pair('hl')) if i==6 else self.r[names[i]];bit=sub>>3&7
                if sub>=128:
                    result=value|(1<<bit) if sub>=192 else value&~(1<<bit)
                    if i==6:self.write(self.pair('hl'),result)
                    else:self.r[names[i]]=result
                elif sub>=64:self.f=(self.f&16)|32|(128 if not value&(1<<bit) else 0)
                elif sub<64:
                    group=sub>>3;carry=bool(self.f&16)
                    if group==0:result=((value<<1)|(value>>7))&255;c=value>>7
                    elif group==1:result=(value>>1)|((value&1)<<7);c=value&1
                    elif group==2:result=((value<<1)|carry)&255;c=value>>7
                    elif group==3:result=(value>>1)|(carry<<7);c=value&1
                    elif group==4:result=(value<<1)&255;c=value>>7
                    elif group==5:result=(value>>1)|(value&128);c=value&1
                    elif group==6:result=((value>>4)|(value<<4))&255;c=0
                    else:result=value>>1;c=value&1
                    if i==6:self.write(self.pair('hl'),result)
                    else:self.r[names[i]]=result
                    self.f=(128 if result==0 else 0)|(16 if c else 0)
                else:raise AssertionError(hex(sub))
                self.cycles+=16 if i==6 and sub>=128 else 12 if i==6 else 8
            elif 0x80<=op<=0xbf or op in (0xc6,0xce,0xd6,0xde,0xe6,0xee,0xf6,0xfe):
                a=self.r['a'];v=fetch() if op>=0xc0 else self.read(self.pair('hl')) if op&7==6 else self.r[names[op&7]]
                group={0xc6:0,0xce:1,0xd6:2,0xde:3,0xe6:4,0xee:5,0xf6:6,0xfe:7}.get(op,(op-0x80)//8);carry=bool(self.f&16)
                result=a+v+(carry if group==1 else 0) if group in (0,1) else a-v-(carry if group==3 else 0) if group in (2,3,7) else a&v if group==4 else a^v if group==5 else a|v if group==6 else None
                if result is None:raise AssertionError(hex(op))
                self.f=(128 if result&255==0 else 0)|(64 if group in (2,3,7) else 0)|(16 if result<0 or result>255 else 0)|(32 if group==4 or group in (0,1) and (a&15)+(v&15)+(carry if group==1 else 0)>15 or group in (2,3,7) and (a&15)<(v&15)+(carry if group==3 else 0) else 0)
                if group!=7:self.r['a']=result&255
                self.cycles+=8 if op>=0xc0 or op&7==6 else 4
            elif op in (0xc2,0xca,0xd2,0xda):
                target=word();take=condition((op-0xc2)//8)
                if take:self.pc=target
                self.cycles+=16 if take else 12
            elif op in (0xd0,0xd8):
                take=condition((op-0xc0)//8);self.cycles+=20 if take else 8
                if take:
                    self.pc=self.pop()
                    if self.pc==0xffff:return self.cycles-start
            elif 0x40 <= op < 0x80:
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
            elif op == 0x19:
                old, value = self.pair('hl'), self.pair('de')
                self.put('hl', old + value)
                self.f = (self.f & 128) | (32 if (old & 4095) + (value & 4095) > 4095 else 0) | (16 if old + value > 65535 else 0)
                self.cycles += 8
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
            elif op in (0x07,0x0f,0x17,0x1f):
                value=self.r['a']; carry=bool(self.f&16)
                if op==7:result=((value<<1)|(value>>7))&255;c=value>>7
                elif op==15:result=(value>>1)|((value&1)<<7);c=value&1
                elif op==23:result=((value<<1)|carry)&255;c=value>>7
                else:result=(value>>1)|(carry<<7);c=value&1
                self.r['a']=result;self.f=16 if c else 0;self.cycles+=4
            elif op in (0x32,0x3a):
                ad=self.pair('hl')
                if op==0x32:self.write(ad,self.r['a'])
                else:self.r['a']=self.read(ad)
                self.put('hl',ad-1);self.cycles+=8
            elif op in (0xcc,0xc4,0xdc,0xd4):
                target=word();take=condition((op-0xc4)//8)
                if take:self.push(self.pc);self.pc=target
                self.cycles+=24 if take else 12
            elif op in (0x00,0xf3,0xfb):self.cycles+=4
            else:
                raise AssertionError(f'Unsupported emitted DMA opcode {op:02x} at {self.pc - 1:04x}')
        raise AssertionError('DMA caller exceeded the bounded instruction runner')

