#!/usr/bin/env python3
"""Compare bounded real game routines with an independent mGBA execution oracle."""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
import random
import subprocess
import stateful_lifting

ROOT=pathlib.Path(__file__).resolve().parents[2]
ENTRIES=['HitSelfInConfusion','ApplyStatBoostDamage','ApplyDefStatBoostDamage','RandomRange','BattleRandom','ConfusedDamageCalc','GetOpponentMonAttr','GetBattleVar','DoNothing']
class State(C.Structure):
    _fields_=[(x,C.c_uint8) for x in 'afbcdehl']+[(x,C.c_uint16) for x in ('sp','pc','bank')]+[('m',C.c_uint8*65536)]+[(x,C.c_uint32) for x in ('steps','fault','event_count','event_pos')]+[('events',C.c_uint8*4096)]
def symbols(path):
    result={}
    for line in path.read_text().splitlines():
        if ':' in line and len(line.split())==2:
            loc,name=line.split();bank,addr=loc.split(':');result[name]=(int(bank,16),int(addr,16))
    return result

class UnsupportedTransition(ValueError):
    pass

def run(rom,sym,build,seed=16763,source_root=None,mgba_prefix=pathlib.Path("/opt/homebrew"),reject_fault=None):
    source_root=source_root or ROOT
    build.mkdir(parents=True,exist_ok=True)
    manifest=stateful_lifting.build(rom,sym,ENTRIES,build,source_root=source_root)
    if isinstance(manifest,pathlib.Path):manifest=json.loads(manifest.read_text())
    if manifest is None:manifest=json.loads((build/'manifest.json').read_text())
    lib=stateful_lifting.load_verified(build,rom,sym,source_root,ENTRIES)
    lib.stateful_run.argtypes=[C.POINTER(State),C.c_uint32,C.c_uint16]
    oracle=build/'oracle.dylib'
    subprocess.run(['cc','-O2','-dynamiclib','-I'+str(mgba_prefix/'include'),str(ROOT/'tools/trainer_ai/stateful_oracle.c'),'-L'+str(mgba_prefix/'lib'),'-lmgba','-Wl,-rpath,'+str(mgba_prefix/'lib'),'-o',str(oracle)],check=True)
    mg=C.CDLL(str(oracle));mg.oracle_open.argtypes=[C.c_char_p];mg.oracle_run.argtypes=[C.POINTER(State),C.c_uint32,C.c_uint16,C.c_uint16]
    assert mg.oracle_open(str(rom).encode())==0
    sy=symbols(sym);rng=random.Random(seed);counts={};traces=[];outputs={}
    def addr(n):return sy[n][1]
    for entry in ['HitSelfInConfusion','ApplyStatBoostDamage','ApplyDefStatBoostDamage','RandomRange','BattleRandom','ConfusedDamageCalc','GetBattleVar','ConfusionTransition']:
        counts[entry]=0;outputs[entry]=[]
        for i in range(140 if entry!='RandomRange' else 260):
            s=State();s.sp=0xdff0;s.pc=addr('HitSelfInConfusion' if entry=='ConfusionTransition' else entry);s.bank=sy['HitSelfInConfusion' if entry=='ConfusionTransition' else entry][0] or (1+i%20)
            s.m[addr('hROMBank')]=s.bank;s.m[s.sp]=0xfe;s.m[s.sp+1]=0xff
            s.f=(i%16)<<4;s.a=rng.randrange(1,256);s.b=11;s.c=23;s.d=37;s.e=41;s.h=53;s.l=61
            s.m[addr('hBattleTurn')]=i%2
            if entry=='RandomRange':s.a=0 if i==0 else [1,2,3,127,128,129,255][i%7]
            for side in ('wBattleMon','wEnemyMon'):
                for stat in ('Attack','Defense'):
                    value=[0,1,2,255,256,511,1023,65535][i%8] if side=='wBattleMon' else rng.randrange(65536)
                    a=addr(side+stat);s.m[a]=value>>8;s.m[a+1]=value&255
                s.m[addr(side+'Level')]=[1,50,100,255][i%4]
            for side in ('wPlayer','wEnemy'):
                for stat in ('AtkLevel','DefLevel'):
                    s.m[addr(side+stat)]=1+(i%13 if side=='wPlayer' else (7*i+4)%13)
            if entry.startswith('Apply'):
                names=('wPlayerAtkLevel','wEnemyAtkLevel') if entry=='ApplyStatBoostDamage' else ('wEnemyDefLevel','wPlayerDefLevel')
                s.h,s.l=addr(names[0])>>8,addr(names[0])&255;s.d,s.e=addr(names[1])>>8,addr(names[1])&255
            if entry in ('ConfusedDamageCalc','ConfusionTransition'):
                for side in ('wBattleMon','wEnemyMon'):
                    if s.m[addr(side+'Defense')]==s.m[addr(side+'Defense')+1]==0:s.m[addr(side+'Defense')+1]=1
            if entry=='GetBattleVar':
                s.a=14;s.m[addr('wPlayerMoveStructPower')]=19+i%200;s.m[addr('wEnemyMoveStructPower')]=41+i%180;s.m[addr('wPlayerMoveStructAccuracy')]=101;s.m[addr('wEnemyMoveStructAccuracy')]=203
            s.m[addr('wMoveHitState')]=0xff if i%3==0 else 0
            for j in range(7):s.m[addr('wRNGState')+j]=rng.randrange(256)
            # Multiplicands are a source-defined HRAM union, initialized explicitly.
            for j in range(4):s.m[addr('hProduct')+j]=rng.randrange(256)
            initial=bytes(s);reference=State.from_buffer_copy(initial)
            mg.oracle_run(C.byref(reference),100000,0xfffe,addr('hROMBank'))
            if reference.fault and not(entry=='RandomRange' and i==0 and reference.fault==3):raise AssertionError((entry,i,'oracle',reference.fault,hex(reference.pc)))
            native=State.from_buffer_copy(initial);native.event_count=reference.event_count
            native.events[:reference.event_count]=reference.events[:reference.event_count]
            lib.stateful_run(C.byref(native),100000,0xfffe)
            if reject_fault is not None and native.fault==reject_fault:
                mg.oracle_close()
                raise UnsupportedTransition(f'{entry} case {i}: independent mGBA completed or reached its bounded oracle stop; freshly compiled native rejects transition with fault {native.fault}, oracle fault {reference.fault}')
            if native.fault and not(entry=='RandomRange' and i==0 and native.fault==3):raise AssertionError((entry,i,'native',native.fault,hex(native.pc)))
            if entry=='ConfusionTransition':
                for target in (reference,native):
                    target.sp-=2;target.m[target.sp]=0xfe;target.m[target.sp+1]=0xff;target.pc=addr('ConfusedDamageCalc')
                mg.oracle_run(C.byref(reference),100000,0xfffe,addr('hROMBank'))
                native.event_count=reference.event_count;native.event_pos=0;native.events[:reference.event_count]=reference.events[:reference.event_count]
                lib.stateful_run(C.byref(native),100000,0xfffe)
                if reference.fault or native.fault:raise AssertionError((entry,i,'continuation',reference.fault,native.fault))
            assert reference.fault==native.fault
            fields=['a','f','b','c','d','e','h','l','sp','pc','bank']
            differences={n:(getattr(reference,n),getattr(native,n)) for n in fields if getattr(reference,n)!=getattr(native,n)}
            ramdiff=[a for a in [*range(0xc000,0xe000),*range(0xff80,0xffff)] if reference.m[a]!=native.m[a]]
            if differences or ramdiff:raise AssertionError((entry,i,differences,[(hex(a),reference.m[a],native.m[a]) for a in ramdiff[:12]]))
            assert native.event_pos==reference.event_count
            counts[entry]+=1
            outputs[entry].append({'registers':[getattr(reference,n) for n in ('a','b','c','d','e','h','l')],'product':list(reference.m[addr('hProduct'):addr('hProduct')+5]),'rng':list(reference.m[addr('wRNGState'):addr('wRNGState')+7])})
            if i<2 or reference.event_count>3:traces.append({'entry':entry,'case':i,'initial_sha256':hashlib.sha256(initial).hexdigest(),'div_reads':list(reference.events[:reference.event_count]),'result':[reference.a,reference.b,reference.c,reference.d,reference.e],'stack_sha256':hashlib.sha256(bytes(reference.m[0xdf80:0xdff2])).hexdigest()})
    mg.oracle_close()
    oracle_lib=mgba_prefix/'lib/libmgba.dylib'
    oracle_provenance={'library_path':str(oracle_lib.resolve()),'library_sha256':hashlib.sha256(oracle_lib.read_bytes()).hexdigest(),'wrapper_sha256':hashlib.sha256((ROOT/'tools/trainer_ai/stateful_oracle.c').read_bytes()).hexdigest(),'fixture_sha256':hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest(),'compile_command':['cc','-O2','-dynamiclib','-I'+str(mgba_prefix/'include'),str(ROOT/'tools/trainer_ai/stateful_oracle.c'),'-L'+str(mgba_prefix/'lib'),'-lmgba','-Wl,-rpath,'+str(mgba_prefix/'lib')],'input_schema':'registers, flat WRAM bank1, HRAM, mapped MBC3 bank, routine action, ordered DIV read tape','zero_range':'Both engines hit explicit instruction budget on RandomRange(0); outside terminating input domain.'}
    report={'oracle':oracle_provenance,'outputs':outputs,'counts':counts,'rom_sha256':hashlib.sha256(rom.read_bytes()).hexdigest(),'seed':seed,'traces':traces,'scope':'Bounded actual game routines, WRAM bank 1, interrupts disabled, ordinary DIV reads recorded from mGBA and replayed. Full fight unsupported.'}
    (build/'report.json').write_text(json.dumps(report,indent=2)+'\n');return report
if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('--rom',type=pathlib.Path,required=True);p.add_argument('--sym',type=pathlib.Path,required=True);p.add_argument('--build-dir',type=pathlib.Path,required=True);p.add_argument('--source-root',type=pathlib.Path);p.add_argument('--mgba-prefix',type=pathlib.Path,default=pathlib.Path('/opt/homebrew'));a=p.parse_args();print(json.dumps(run(a.rom,a.sym,a.build_dir,source_root=a.source_root,mgba_prefix=a.mgba_prefix)['counts']))
