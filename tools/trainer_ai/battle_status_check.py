#!/usr/bin/env python3
"""Falsifiable conditional headless routine and composed status-fight parity."""
import argparse
import sys,pathlib,ctypes as C,json,hashlib
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from battle_env import NativeFight, MoveAction, UnsupportedFight
from battle_status import PoisonLeaf, StatusFight
from battle_env_check import Hook
from stateful_fixture_check import State
import battle_env_profile as profile
p=argparse.ArgumentParser()
p.add_argument('--artifact-root',type=pathlib.Path,required=True)
args=p.parse_args()
root=args.artifact_root
game=root/'build/stateful/game'
f=NativeFight(game/'polishedcrystal-3.2.3.gbc',game/'polishedcrystal-3.2.3.sym',game,root/'build/battle-env/caller')
l=PoisonLeaf(f,root/'build/battle-status/leaf')
mg=C.CDLL(str(root/'build/battle-env/caller/headless-oracle.dylib'))
mg.oracle_open.argtypes=[C.c_char_p]
mg.oracle_run.argtypes=[C.POINTER(State),C.c_uint32,C.c_uint16,C.c_uint16,C.POINTER(Hook),C.c_uint32]
assert mg.oracle_open(str(f.rom_path).encode())==0
hooks=(Hook*len(profile.PRESENTATION))(*[Hook(*f.symbols[n]) for n in profile.PRESENTATION])
rows=[]
for variant in range(4):
 for orientation in (0,1):
  for poison in (0,1,2,3):
   for hp,maxhp in ((0,145),(145,145),(1,145),(18,145),(19,145),(1,1),(7,7),(8,8),(257,513)):
    f.reset(193,orientation)
    f._put('hBattleTurn',variant%2)
    for side,stem in enumerate(('wBattleMon','wEnemyMon')):
     f._put(stem+'Status',8 if poison & (1<<side) else 0)
     f._put(stem+'HP',hp,2);f._put(stem+'MaxHP',maxhp,2)
    for n,v in zip(('a','f','b','c','d','e','h','l'),(17+variant,variant*80,21+variant,31,41,51,61,71)):setattr(f.state,n,v)
    f._put('wPlayerSpeLevel', (1,7,13,7)[variant])
    f._put('wEnemySpeLevel', (13,7,1,7)[variant])
    if variant == 3:
     f._put('wBattleMonSpeed',125,2)
     f._put('wEnemyMonSpeed',125,2)
    l.prepare_call('HandlePoison')
    ref=State.from_buffer_copy(f.state)
    mg.oracle_run(C.byref(ref),200000,65534,f.address('hROMBank'),hooks,len(hooks))
    assert not ref.fault,('oracle',ref.fault,hex(ref.pc))
    tape=tuple(ref.events[:ref.event_count])
    used,front=l.run_prepared(tape)
    fields=('a','f','b','c','d','e','h','l','sp','pc','bank')
    diff={n:(getattr(ref,n),getattr(f.state,n)) for n in fields if getattr(ref,n)!=getattr(f.state,n)}
    ram=[a for a in (*range(0xc000,0xe000),*range(0xff80,0xffff)) if ref.m[a]!=f.state.m[a]]
    assert not diff and not ram,(orientation,poison,hp,maxhp,diff,ram[:10])
    assert used==tape
    if poison==0:
     assert [f.word(n+'HP') for n in ('wBattleMon','wEnemyMon')]==[hp,hp]
    rows.append(dict(variant=variant,orientation=orientation,poison=poison,hp=hp,maxhp=maxhp,after=[f.word(s+'HP') for s in ('wBattleMon','wEnemyMon')],frontend=front,steps=f.state.steps))

# Each call starts from the identical committed State. Source HandlePoison itself
# selects residual order, independent of the host move order/controller.
comparisons=[]
def checked_call(engine, entry):
 engine.prepare_call(entry)
 ref=State.from_buffer_copy(engine.state)
 mg.oracle_run(C.byref(ref),200000,65534,f.address('hROMBank'),hooks,len(hooks))
 assert not ref.fault,(entry,'oracle',ref.fault,hex(ref.pc))
 tape=tuple(ref.events[:ref.event_count])
 used,front=engine.run_prepared(tape)
 for n in ('a','f','b','c','d','e','h','l','sp','pc','bank'):
  assert getattr(ref,n)==getattr(engine.state,n),(entry,n)
 assert all(ref.m[a]==engine.state.m[a] for a in (*range(0xc000,0xe000),*range(0xff80,0xffff))),entry
 assert used==tape
 comparisons.append(dict(entry=entry,hp=[engine.state.m[f.address(n)]*256+engine.state.m[f.address(n)+1] for n in ('wBattleMonHP','wEnemyMonHP')]))
 return used,front

terminal_comparisons=[]
def install_terminal_check(sf):
 engine=sf.terminal_leaf
 original=engine.call
 def checked(entry):
  initial=bytes(engine.state)
  engine.prepare_call(entry)
  ref=State.from_buffer_copy(engine.state)
  C.memmove(C.byref(engine.state),initial,C.sizeof(State))
  stop=65534
  if entry=='ResolveFaints.check_battle_over' and 0 in (sf.fight.word('wBattleMonHP'),sf.fight.word('wEnemyMonHP')):
   expected='LostBattle' if sf.fight.word('wBattleMonHP')==0 else 'WinTrainerBattle'
   stop=engine.address(expected)
  mg.oracle_run(C.byref(ref),200000,stop,f.address('hROMBank'),hooks,len(hooks))
  assert not ref.fault,(entry,'terminal oracle',ref.fault,hex(ref.pc))
  event=original(entry)
  for n in ('a','f','b','c','d','e','h','l','sp','pc','bank'):
   assert getattr(ref,n)==getattr(engine.state,n),(entry,n,getattr(ref,n),getattr(engine.state,n))
  assert all(ref.m[a]==engine.state.m[a] for a in (*range(0xc000,0xe000),*range(0xff80,0xffff))),entry
  if stop!=65534:assert event==expected
  else:assert event is None
  terminal_comparisons.append(dict(entry=entry,event=event))
  return event
 engine.call=checked

def make(orientation=0,side=0,seed=193,max_turns=64):
 return StatusFight(game/'polishedcrystal-3.2.3.gbc',game/'polishedcrystal-3.2.3.sym',game,root/'build/battle-env/caller',root/'build/battle-status/leaf',seed,orientation,side,max_turns)

episodes=[]
for orientation in (0,1):
 for side in (0,1):
  for strategy in ('physical','special','alternating'):
   sf=make(orientation,side)
   sf.fight._call=lambda entry:checked_call(sf.fight,entry)
   sf.leaf.call=lambda:checked_call(sf.leaf,'HandlePoison')
   install_terminal_check(sf)
   outcome='terminal'
   while not sf.terminal and not sf.truncated:
    turn=sf.fight.turn
    slots=(0,0) if strategy=='physical' else (1,1) if strategy=='special' else (turn%2,(turn+1)%2)
    sf.step(tuple(MoveAction(i) for i in slots))
   assert sf.terminal and sf.winner in (0,1) and not sf.truncated
   episodes.append(dict(orientation=orientation,poisoned_side=side,strategy=strategy,outcome=outcome,turns=sf.fight.turn,winner=sf.winner,residuals=len(sf.residuals)))
assert any(e['outcome']=='terminal' for e in episodes)
assert all(e['outcome']=='terminal' for e in episodes)
assert len(terminal_comparisons)==sum(e['residuals'] for e in episodes)*3

# Native-only replay is deterministic and reset removes all previous mutations.
native=[]
for orientation in (0,1):
 for side in (0,1):
  sf=make(orientation,side,731)
  initial=bytes(sf.fight.state)
  while not sf.terminal and not sf.truncated:sf.step((MoveAction(0),MoveAction(1)))
  final=bytes(sf.fight.state)
  sf.reset(731)
  assert bytes(sf.fight.state)==initial
  while not sf.terminal and not sf.truncated:sf.step((MoveAction(0),MoveAction(1)))
  assert bytes(sf.fight.state)==final
  native.append(dict(orientation=orientation,poisoned_side=side,winner=sf.winner,failed=sf.fight.failed))

sf=make()
before=bytes(sf.fight.state)
try:sf.step((MoveAction(2),MoveAction(0)))
except UnsupportedFight:assert bytes(sf.fight.state)==before and not sf.fight.failed
else:raise AssertionError('Illegal action accepted')
# Partycheck double-KO is explicitly unsupported, with no invented winner.
sf=make()
sf.fight._put('wBattleMonHP',0,2)
sf.fight._put('wEnemyMonHP',0,2)
try:sf.terminal_leaf.resolve()
except UnsupportedFight:assert sf.winner is None
else:raise AssertionError('Double KO accepted')
# A post-commit execution error poisons the whole wrapper until reset.
sf=make()
original=sf.leaf.call
def broken():raise IndexError('Injected leaf failure')
sf.leaf.call=broken
try:sf.step((MoveAction(0),MoveAction(0)))
except IndexError:assert sf.fight.failed
else:raise AssertionError('Leaf failure swallowed')
try:sf.step((MoveAction(0),MoveAction(0)))
except UnsupportedFight:pass
else:raise AssertionError('Failed wrapper resumed')
sf.leaf.call=original
sf.reset()
assert not sf.fight.failed
sf=make(max_turns=1)
sf.step((MoveAction(0),MoveAction(0)))
assert sf.truncated and not sf.terminal and sf.winner is None
# Arbitrary non-entry execution is a native fault, not a presentation event.
sf=make()
sf.fight.state.pc=0x8000
try:sf.leaf.run_prepared(())
except UnsupportedFight:pass
else:raise AssertionError('Unknown PC accepted')
# The independent leaf must reject an entry/boundary configuration mismatch.
import stateful_lifting
try:stateful_lifting.load_verified(root/'build/battle-status/leaf',f.rom_path,f.symbols_path,game,['HandlePoison'])
except ValueError:pass
else:raise AssertionError('Incorrect leaf configuration accepted')
mg.oracle_close()
report=dict(scope='conditional identity-return headless source calls with declared host status orchestration; actual party synchronization and battle-over check; no whole endturn or ordinary frontend/RNG claim',routine_cases=rows,episodes=episodes,comparisons=comparisons,terminal_comparisons=terminal_comparisons,native_replays=native,guards=['atomic illegal action','single residual faint source terminal decision','double KO rejects','leaf error poisons until reset','deterministic reset','turn budget is not win','unknown PC rejects','independent leaf profile mismatch rejects'],source_sha256={name:hashlib.sha256((pathlib.Path(__file__).parent/name).read_bytes()).hexdigest() for name in ('battle_status.py','battle_status_profile.py','battle_status_check.py')})
(root/'build/battle-status/status-report.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(dict(routine_cases=len(rows),episodes=episodes,call_comparisons=len(comparisons),native_replays=native)))
