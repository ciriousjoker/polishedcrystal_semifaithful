#!/usr/bin/env python3
"""Independent RGBDS byte execution versus generated native arithmetic code."""
import argparse
import ctypes as C
import json
import pathlib
import random
import shutil
import subprocess
import tempfile
import lifting

class State(C.Structure):
    _fields_=[(k,C.c_uint8) for k in 'abcdehl']+[(k,C.c_uint8) for k in ('z','cy','n','hf')]+[('sp',C.c_uint16),('m',C.c_uint8*65536),('fault',C.c_int)]

def execute(rom,initial,pc):
    m=bytearray(initial.m);m[:len(rom)]=rom
    r=[initial.b,initial.c,initial.d,initial.e,initial.h,initial.l,0,initial.a]
    z,cy,n,hf=initial.z,initial.cy,initial.n,initial.hf
    sp=initial.sp
    def pair(p):return r[p]*256+r[p+1]
    def setpair(p,v):r[p],r[p+1]=(v>>8)&255,v&255
    def get(i):return m[pair(4)] if i==6 else r[i]
    def put(i,v):
        if i==6:m[pair(4)]=v&255
        else:r[i]=v&255
    def push(v):
        nonlocal sp
        sp=(sp-1)&65535;m[sp]=v>>8;sp=(sp-1)&65535;m[sp]=v&255
    def pop():
        nonlocal sp
        v=m[sp]|m[(sp+1)&65535]<<8;sp=(sp+2)&65535;return v
    push(0xffff)
    for steps in range(100000):
        at=pc;op=m[pc];pc+=1
        if op==0x76: return r,(z,cy,n,hf),m,sp,True
        if 0x40<=op<=0x7f: put((op>>3)&7,get(op&7))
        elif op&0xc7==0x06:put((op>>3)&7,m[pc]);pc+=1
        elif op&0xcf==0x01:setpair(((op>>4)&3)*2,m[pc]|m[pc+1]<<8);pc+=2
        elif op in (0xe0,0xf0):
            addr=0xff00+m[pc];pc+=1
            if op==0xe0:m[addr]=r[7]
            else:r[7]=m[addr]
        elif op in (0xe2,0xf2):
            if op==0xe2:m[0xff00+r[1]]=r[7]
            else:r[7]=m[0xff00+r[1]]
        elif op in (0x22,0x32,0x2a,0x3a):
            addr=pair(4)
            if op in (0x22,0x32):m[addr]=r[7]
            else:r[7]=m[addr]
            setpair(4,addr+(1 if op in (0x22,0x2a) else -1))
        elif op in (0x03,0x13,0x23,0x0b,0x1b,0x2b):
            p=((op>>4)&3)*2;setpair(p,pair(p)+(1 if op&15==3 else -1))
        elif op&0xc7 in (4,5):
            i=(op>>3)&7;old=get(i);dec=op&1;v=(old+(-1 if dec else 1))&255;put(i,v)
            z=int(v==0);n=dec;hf=int((old&15)==(0 if dec else 15))
        elif op in (0xc5,0xd5,0xe5):push(pair(((op>>4)&3)*2))
        elif op in (0xc1,0xd1,0xe1):setpair(((op>>4)&3)*2,pop())
        elif op in (0xcd,0xc3):
            dest=m[pc]|m[pc+1]<<8;pc+=2
            if op==0xcd:push(pc)
            pc=dest
        elif op in (0x18,0x20,0x28,0x30,0x38):
            rel=m[pc];pc+=1
            take=op==0x18 or {0x20:not z,0x28:z,0x30:not cy,0x38:cy}.get(op)
            if take:pc+=rel if rel<128 else rel-256
        elif op in (0xc9,0xc0,0xc8,0xd0,0xd8):
            if op==0xc9 or {0xc0:not z,0xc8:z,0xd0:not cy,0xd8:cy}.get(op):
                pc=pop()
                if pc==0xffff:return r,(z,cy,n,hf),m,sp,False
        elif op==0x29:
            old=pair(4);v=old*2;setpair(4,v);cy=int(v>65535);hf=int((old&4095)*2>4095);n=0
        elif op==0x1f:
            old=r[7];r[7]=(old>>1)|(cy<<7);cy=old&1;z=n=hf=0
        elif op==0xcb:
            ext=m[pc];pc+=1;i=ext&7;old=get(i)
            if ext&0xf8==0x10:v=(old<<1)|cy
            elif ext&0xf8==0x20:v=old<<1
            else:raise AssertionError(('cb',hex(ext)))
            put(i,v);cy=old>>7;z=int((v&255)==0);n=hf=0
        elif 0x80<=op<=0xbf or op in (0xc6,0xce,0xd6,0xe6,0xee,0xf6,0xfe):
            if op>=0xc0:v=m[pc];pc+=1;group={0xc6:0,0xce:1,0xd6:2,0xe6:4,0xee:5,0xf6:6,0xfe:7}[op]
            else:v=get(op&7);group=(op>>3)&7
            old=r[7]
            if group in (0,1):
                addcy=cy if group==1 else 0;total=old+v+addcy;hf=int((old&15)+(v&15)+addcy>15);cy=int(total>255);n=0
            elif group in (2,7):total=old-v;hf=int((old&15)<(v&15));cy=int(old<v);n=1
            elif group in (4,5,6):total={4:old&v,5:old^v,6:old|v}[group];cy=n=0;hf=int(group==4)
            else:raise AssertionError(('alu',hex(op)))
            z=int((total&255)==0)
            if group!=7:r[7]=total&255
        else:raise AssertionError(('unsupported byte',hex(op),hex(at)))
    raise AssertionError('instruction limit')

def main():
    p=argparse.ArgumentParser();p.add_argument('--build-dir',type=pathlib.Path,required=True);a=p.parse_args();build=a.build_dir
    source,symbols=lifting.build(lifting.ROOT,build);rom=(build/'fixture.gb').read_bytes();lib=C.CDLL(str(build/'lifted.dylib'));lib.lifted.argtypes=[C.POINTER(State),C.c_int]
    rng=random.Random(16763);counts={};vectors=[]
    for entry,name,count in ((0,'Entry',800),(1,'Multiply',600),(2,'Divide',800),(3,'DamagePass4',256)):
        for i in range(count):
            s=State();s.sp=0xfffe
            for reg in 'abcdehl':setattr(s,reg,rng.randrange(256))
            s.z,s.cy,s.n,s.hf=[rng.randrange(2) for _ in range(4)]
            for addr in range(0xffa0,0xffaa):s.m[addr]=rng.randrange(256)
            s.m[0xc000],s.m[0xc001]=rng.randrange(256),rng.randrange(256)
            if entry==0:
                s.b=(0,1,255,rng.randrange(256))[i%4];s.c=(0,1,255,rng.randrange(256))[(i//4)%4]
                s.d=(0,1,255,rng.randrange(256))[(i//16)%4];s.e=(0,1,127,128,255,rng.randrange(256))[(i//64)%6]
            if entry==3 and i<12:
                q=((0,0,0),(0,0,253),(0,0,254),(0,254,253),(0,254,254),(0,254,255),(0,255,0),(0,255,255),(1,0,0),(1,255,255),(255,0,0),(255,255,255))[i]
                for j,v in enumerate(q):s.m[symbols['hQuotient']+j]=v
            if entry==2:s.b=1+i%4;s.m[symbols['hDivisor']]=1+i%255
            before=bytes(s);r,flags,m,sp,fault=execute(rom,s,symbols[name]);lib.lifted(C.byref(s),entry)
            observed=[s.b,s.c,s.d,s.e,s.h,s.l,0,s.a]
            assert observed==r,(entry,i,'regs',observed,r)
            assert (s.z,s.cy,s.n,s.hf)==flags,(entry,i,'flags',(s.z,s.cy,s.n,s.hf),flags)
            assert bytes(s.m[0xffa0:0xffaa])==m[0xffa0:0xffaa],(entry,i,'math')
            assert bytes(s.m[0xc000:0xc002])==m[0xc000:0xc002],(entry,i,'damage')
            assert s.sp==sp==0xfffe and bool(s.fault)==fault
            if entry==0:
                old=State.from_buffer_copy(before)
                x=(old.e*2//5+2)*old.d*old.b//max(1,old.c)//50
                expected=(0xff00|old.m[0xc001]) if x>=0xff00 else x+2
                assert int.from_bytes(bytes(s.m[0xc000:0xc002]),'big')==expected
            if entry==1:
                old=State.from_buffer_copy(before);x=int.from_bytes(bytes(old.m[symbols['hMultiplicand']:symbols['hMultiplicand']+3]),'big');mult=old.m[symbols['hMultiplier']]
                assert int.from_bytes(bytes(s.m[symbols['hProduct']:symbols['hProduct']+4]),'big')==x*mult
            if entry==2:
                old=State.from_buffer_copy(before);x=int.from_bytes(bytes(old.m[symbols['hDividend']:symbols['hDividend']+old.b]),'big');div=old.m[symbols['hDivisor']]
                assert int.from_bytes(bytes(s.m[symbols['hDividend']:symbols['hDividend']+4]),'big')==x//div
                assert s.m[symbols['hRemainder']]==x%div
        counts[name]=count
    # Division by zero must retain an explicit fault, never silently return a quotient.
    s=State();s.sp=0xfffe;s.b=4;lib.lifted(C.byref(s),2);assert s.fault==1
    # Source mutation propagates through generation. Original source remains untouched.
    with tempfile.TemporaryDirectory(dir=build) as td:
        root=pathlib.Path(td)
        for path in lifting.SOURCES:
            dest=root/path;dest.parent.mkdir(parents=True,exist_ok=True);shutil.copyfile(lifting.ROOT/path,dest)
        battle=root/lifting.SOURCES[0];text=battle.read_text();text=text.replace('; / 5\n\tld a, 5','; / 5\n\tld a, 7',1);battle.write_text(text)
        _,mut_symbols=lifting.build(root,root/'build');mut=C.CDLL(str(root/'build/lifted.dylib'));mut.lifted.argtypes=lib.lifted.argtypes
        s=State();s.sp=0xfffe;s.b=100;s.c=80;s.d=90;s.e=50;t=State.from_buffer_copy(s);lib.lifted(C.byref(s),0);mut.lifted(C.byref(t),0)
        assert bytes(s.m[0xc000:0xc002])!=bytes(t.m[0xc000:0xc002])
        shutil.copytree(root/'build',build/'mutation',dirs_exist_ok=True)
        mutation={'baseline':int.from_bytes(bytes(s.m[0xc000:0xc002]),'big'),'mutated':int.from_bytes(bytes(t.m[0xc000:0xc002]),'big')}
        battle.write_text((lifting.ROOT/lifting.SOURCES[0]).read_text().replace('\tadd a\n\tjr nc, .level_not_overflowing', '\tadd hl, bc\n\tjr nc, .level_not_overflowing', 1))
        try:lifting.build(root,root/'unsupported-build')
        except ValueError as error:
            assert 'add hl, bc' in str(error)
            assert (root/'unsupported-build'/'fixture.gb').exists()
            assert not (root/'unsupported-build'/'lifted.c').exists()
        else:raise AssertionError('build accepted unsupported ADD HL,BC')
    rejected = []
    for instruction in ('daa', 'add hl, bc', 'add hl, de', 'add hl, sp',
                        'add sp, 1', 'adc hl, bc', 'ld sp, hl', 'ld hl, sp + 1',
                        'inc sp', 'push af', 'jp hl', 'ld [hDividend], b',
                        'ldh b, [hDividend]', 'ldh [hDividend], b', 'add -1',
                        'rl bc', 'sla hl', 'rra a', 'ld [hDividend], 1'):
        try:lifting.generate(source.replace('add a', instruction, 1),symbols)
        except ValueError:rejected.append(instruction)
        else:raise AssertionError('unsupported operand form accepted: '+instruction)
    report={'build_cli_rejection':'RGBDS accepts ADD HL,BC, native generation rejects before writing C','rejected_operand_forms':rejected,'parity_cases':counts,'mutation_divide_5_to_7':mutation,'independent_formula_checks':'Full DamagePass1..4, Multiply and Divide','zero_division':'explicit fault','compared':'all 7 registers, Z/C/N/H flags, 10 math bytes, 2 damage bytes, balanced stack; return stack bytes excluded'}
    (build/'checks.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
if __name__=='__main__':main()
