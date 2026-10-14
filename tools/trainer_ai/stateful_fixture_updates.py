#!/usr/bin/env python3
"""Regenerate copied actual game sources and gate supported and rejected updates."""
import argparse
import hashlib
import json
import pathlib
import shutil
import subprocess
import stateful_fixture_check as check
import stateful_lifting

def run(game,build,mgba):
    build.mkdir(parents=True,exist_ok=True)
    rom=game/'polishedcrystal-3.2.3.gbc';sym=rom.with_suffix('.sym')
    baseline_symbols=check.symbols(sym)
    baseline=check.run(rom,sym,build/'baseline',source_root=game,mgba_prefix=mgba)
    changes=[
        ('stage-rule','engine/battle/effect_commands.asm','\tln a, 1, 11','\tln a, 1, 10','main.o',True),
        ('variable-table','home/battle_vars.asm','dw wPlayerMoveStructPower,        wEnemyMoveStructPower','dw wPlayerMoveStructAccuracy,     wEnemyMoveStructAccuracy','home.o',True),
        ('state-layout','ram/hram.asm','hBattleTurn:: db\nhBattlePalFadeMode:: db','hBattlePalFadeMode:: db\nhBattleTurn:: db','ram.o',True),
        ('bank-wrapper','home/random.asm','\tfarjp _BattleRandom','\tfarcall _BattleRandom\n\tret','home.o',True),
        ('unsupported-opcode','home/random.asm','\nRandomRange::\n','\nRandomRange::\n\tstop\n','home.o',False),
        ('unsupported-mmio','home/random.asm','\nRandomRange::\n','\nRandomRange::\n\tldh a, [rLY]\n','home.o',False),
    ]
    results=[]
    for name,path,before,after,obj,supported in changes:
        out=build/name;out.mkdir(exist_ok=True);source=game/path;original=source.read_text()
        assert original.count(before)==1,(name,'ambiguous mutation')
        source.write_text(original.replace(before,after));(out/'changed-source.asm').write_text(source.read_text())
        try:
            (game/obj).unlink(missing_ok=True);rom.unlink(missing_ok=True)
            with (out/'assemble.log').open('w') as log:
                subprocess.run(['make','-j1'],cwd=game,stdout=log,stderr=subprocess.STDOUT,check=True)
            shutil.copy2(rom,out/rom.name);shutil.copy2(sym,out/sym.name)
            try:stateful_lifting.load_verified(build/'baseline',rom,sym,game,check.ENTRIES)
            except (ValueError,RuntimeError) as e:stale=str(e)
            else:raise AssertionError((name,'stale native accepted'))
            if supported:
                report=check.run(rom,sym,out,source_root=game,mgba_prefix=mgba)
                changed=[entry for entry in baseline['outputs'] if baseline['outputs'][entry]!=report['outputs'][entry]]
                observable={'changed_semantic_output_entries':changed,'rom_bytes_changed':baseline['rom_sha256']!=report['rom_sha256']}
                if name in ('stage-rule','variable-table'):assert changed,(name,'no observable rule/data change')
                if name=='state-layout':
                    observable['hBattleTurn']=check.symbols(sym)['hBattleTurn'][1]
                    assert observable['hBattleTurn']!=baseline_symbols['hBattleTurn'][1]
                assert observable['rom_bytes_changed']
                results.append({'name':name,'status':'matched','counts':report['counts'],'observable':observable,'stale_rejection':stale})
            else:
                try:check.run(rom,sym,out,source_root=game,mgba_prefix=mgba,reject_fault=2 if name=='unsupported-mmio' else None)
                except check.UnsupportedTransition as e:
                    assert name=='unsupported-mmio'
                    results.append({'name':name,'status':'fresh native rejected transition','fault':2,'reason':str(e),'stale_rejection':stale})
                except ValueError as e:
                    assert not (out/'manifest.json').exists()
                    results.append({'name':name,'status':'rejected before load','reason':str(e),'stale_rejection':stale})
                else:raise AssertionError((name,'unsupported update accepted'))
        finally:
            source.write_text(original)
            # Restore actual ROM as well as the source snapshot before the next mutation.
            (game/obj).unlink(missing_ok=True);rom.unlink(missing_ok=True)
            with (out/'restore.log').open('w') as log:
                subprocess.run(['make','-j1'],cwd=game,stdout=log,stderr=subprocess.STDOUT,check=True)
    (build/'updates.json').write_text(json.dumps(results,indent=2)+'\n')
    return results
if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('--game-source',type=pathlib.Path,required=True);p.add_argument('--build-dir',type=pathlib.Path,required=True);p.add_argument('--mgba-prefix',type=pathlib.Path,required=True);a=p.parse_args()
    print(json.dumps(run(a.game_source,a.build_dir,a.mgba_prefix),indent=2))
