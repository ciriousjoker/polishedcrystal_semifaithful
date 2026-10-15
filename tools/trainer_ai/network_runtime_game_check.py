#!/usr/bin/env python3
"""Artifact-only link of the same network into frozen real game objects."""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
import random
import struct
import subprocess
from network_runtime_check import State, ROOT, model_bytes, reference

def run(build, game, prefix):
    build.mkdir(parents=True,exist_ok=True)
    commands=[]
    def command(args,cwd=None):
        commands.append(list(map(str,args))); subprocess.run(list(map(str,args)),cwd=cwd,check=True)
    rng=random.Random(71423);n,h,a=64,48,12
    weights=[rng.randrange(-8,8) for _ in range(n*h+h*a)]
    biases=[rng.randint(-16384,16384) for _ in range(h+a)]
    shifts=[j%16 for j in range(h)]
    inputs=[rng.randrange(16) for _ in range(n)]
    payload=model_bytes(n,h,a,weights,biases,shifts)
    (build/'network_runtime_model.bin').write_bytes(payload)
    command(['rgbasm','-D','NET_GAME=1','-D','NET_BANK=127','-o',build/'runtime.o',ROOT/'tools/trainer_ai/network_runtime.asm'],build)
    objects='main.o home.o ram.o audio.o audio/music_player.o data/pokemon/dex_entries.o data/pokemon/egg_moves.o data/pokemon/evos_attacks.o data/maps/map_data.o data/text/common.o data/tilesets.o engine/movie/credits.o engine/overworld/events.o gfx/minis_icons.o gfx/pokemon.o gfx/sprites.o gfx/trainers.o gfx/items.o gfx/misc.o'.split()
    frozen={str(game/p):hashlib.sha256((game/p).read_bytes()).hexdigest() for p in objects+['layout.link','polishedcrystal-3.2.3.gbc','polishedcrystal-3.2.3.sym']}
    rom=build/'game.gbc';sym=build/'game.sym';mp=build/'game.map'
    command(['rgblink','-Weverything','-Wtruncation=1','-M','-n',sym,'-m',mp,'-p','0xff','-l','layout.link','-o',rom]+objects+[build/'runtime.o'],game)
    command(['rgbfix','-Weverything','-csjv','-t','PKPCRYSTAL','-i','PKPC','-n','0x32','-p','0xff','-k','01','-l','0x33','-m','MBC3+TIMER+RAM+BATTERY','-r','3',rom])
    sy={}
    for line in sym.read_text().splitlines():
        f=line.split()
        if len(f)==2 and ':' in f[0]:
            bank,address=f[0].split(':');sy[f[1]]=(int(bank,16),int(address,16))
    def addr(name):return sy[name][1]
    assert all(sy[name][0]==127 for name in ['NetworkEvaluate','NetworkProductLow','NetworkProductHigh','NetworkModel'])
    lib=build/'oracle.dylib'
    command(['cc','-O2','-dynamiclib','-I'+str(prefix/'include'),ROOT/'tools/trainer_ai/network_runtime_oracle.c','-L'+str(prefix/'lib'),'-lmgba','-Wl,-rpath,'+str(prefix/'lib'),'-o',lib])
    mg=C.CDLL(str(lib));mg.oracle_open.argtypes=[C.c_char_p];mg.oracle_run.argtypes=[C.POINTER(State),C.c_uint32,C.c_uint16,C.c_uint16];mg.network_last_cycles.restype=C.c_uint64;mg.network_last_ticks.restype=C.c_uint64
    assert mg.oracle_open(str(rom).encode())==0
    probe=State();probe.pc=addr('NetworkTimingProbe');probe.sp=0xc0f0;probe.bank=127
    probe.m[0xc0f0]=0xfe;probe.m[0xc0f1]=0xff
    assert mg.oracle_run(C.byref(probe),100,0xfffe,0)==0
    assert mg.network_last_ticks()==160 and mg.network_last_cycles()==80
    reports=[]
    for mask in [0,0xfff,0x3f0,1,0x800,0x1000]:
        s=State();s.pc=addr('NetworkEvaluate');s.sp=0xc0f0;s.bank=127
        s.m[0xc0f0]=0xfe;s.m[0xc0f1]=0xff
        for i,v in enumerate(inputs):s.m[addr('NetworkInput')+i]=v
        s.m[addr('NetworkLegal')]=mask&255;s.m[addr('NetworkLegal')+1]=mask>>8
        assert not mg.oracle_run(C.byref(s),1000000,0xfffe,0)
        actual=(list(s.m[addr('NetworkHidden'):addr('NetworkHidden')+h]),list(struct.unpack('<'+'h'*a,bytes(s.m[addr('NetworkLogits'):addr('NetworkLogits')+a*2]))),s.m[addr('NetworkAction')])
        assert actual==reference(n,h,a,weights,biases,shifts,inputs,mask),(mask,actual)
        assert s.a==actual[2] and s.sp==0xc0f2 and s.bank==127
        reports.append(dict(mask=mask,cycles=mg.network_last_cycles(),action=s.a,bank=s.bank))
    mg.oracle_close()
    assert all(hashlib.sha256(pathlib.Path(p).read_bytes()).hexdigest()==v for p,v in frozen.items())
    output=dict(commands=commands,frozen_sha256=frozen,reports=reports,symbols={k:sy[k] for k in sy if k.startswith('Network')},artifacts={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in build.iterdir() if p.is_file() and p.name != 'results.json'},source_sha256={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in (ROOT/'tools/trainer_ai').glob('network_runtime*')})
    (build/'results.json').write_text(json.dumps(output,indent=2)+'\n');return output
if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('--build-dir',type=pathlib.Path,required=True);p.add_argument('--game',type=pathlib.Path,required=True);p.add_argument('--mgba-prefix',type=pathlib.Path,required=True);a=p.parse_args()
    r=run(a.build_dir.resolve(),a.game.resolve(),a.mgba_prefix.resolve());print(json.dumps(r['reports']))
