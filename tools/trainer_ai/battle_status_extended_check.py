#!/usr/bin/env python3
"""Bounded burn/Toxic continuation, preserving the separate poison evidence."""
import argparse
import ctypes as C
import hashlib
import json
import pathlib

from battle_env import MoveAction, NativeFight, UnsupportedFight
from battle_env_check import Hook
from battle_status import PoisonLeaf, StatusFight
from stateful_fixture_check import State
import battle_env_profile as profile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
args = parser.parse_args()
root = args.artifact_root
build = root / 'build'
game = build / 'stateful/game'
rom = game / 'polishedcrystal-3.2.3.gbc'
sym = game / 'polishedcrystal-3.2.3.sym'
f = NativeFight(rom, sym, game, build / 'battle-env/caller')
leaf = PoisonLeaf(f, build / 'battle-status/leaf')
mg = C.CDLL(str(build / 'battle-env/caller/headless-oracle.dylib'))
mg.oracle_open.argtypes = [C.c_char_p]
mg.oracle_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16,
                         C.c_uint16, C.POINTER(Hook), C.c_uint32]
assert mg.oracle_open(str(rom).encode()) == 0
hooks = (Hook * len(profile.PRESENTATION))(
    *[Hook(*f.symbols[n]) for n in profile.PRESENTATION])
comparisons = []


def compare(reference, native, entry):
    fields = ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank')
    differences = {n: (getattr(reference, n), getattr(native, n))
                   for n in fields if getattr(reference, n) != getattr(native, n)}
    ram = [a for a in (*range(0xc000, 0xe000), *range(0xff80, 0xffff))
           if reference.m[a] != native.m[a]]
    assert not differences and not ram, (entry, differences, ram[:10])
    comparisons.append(dict(entry=entry, pc=native.pc, bank=native.bank))


def replay(engine, entry):
    engine.prepare_call(entry)
    reference = State.from_buffer_copy(engine.state)
    mg.oracle_run(C.byref(reference), 200000, 65534, f.address('hROMBank'), hooks, len(hooks))
    assert not reference.fault, (entry, 'oracle', reference.fault, hex(reference.pc))
    tape = tuple(reference.events[:reference.event_count])
    used, frontend = engine.run_prepared(tape)
    assert used == tape
    compare(reference, engine.state, entry)
    return used, frontend


routine_cases = []
for mode, status, entry in (('burn', 16, 'HandleBurn'), ('toxic', 136, 'HandlePoison')):
    for side in (0, 1):
        for counter in ((0,) if mode == 'burn' else (0, 1, 14, 15)):
            for hp, maxhp in ((0, 145), (1, 7), (9, 145), (513, 513)):
                f.reset(seed=193, orientation=side)
                stem = ('wBattleMon', 'wEnemyMon')[side]
                count_name = ('wPlayerToxicCount', 'wEnemyToxicCount')[side]
                f._put(stem + 'Status', status)
                f._put(stem + 'HP', hp, 2)
                f._put(stem + 'MaxHP', maxhp, 2)
                f._put(count_name, counter)
                replay(leaf, entry)
                after_counter = f.byte(count_name)
                if mode == 'toxic' and hp:
                    assert after_counter == min(counter + 1, 15)
                if not hp:
                    assert f.word(stem + 'HP') == 0
                routine_cases.append(dict(mode=mode, side=side, hp=hp, maxhp=maxhp,
                    counter_before=counter, counter_after=after_counter, hp_after=f.word(stem + 'HP')))

# Replay the actual physical and special move callers in the burned fixture.
# Paired native calls below use identical DIV tapes to isolate the burn branch.
burn_move_cases = []
for slot in (0, 1):
    results = []
    for burned in (False, True):
        f.reset(seed=193)
        f._put('wBattleMonStatus', 16 if burned else 0)
        f.commit_actions((MoveAction(slot), MoveAction(slot)))
        f._put('hBattleTurn', 0)
        f._put('wDamageTaken', 0, 2)
        replay(f, 'DoTurn')
        oracle_damage = 150 - f.word('wEnemyMonHP')
        # Separate fixed-tape native probe, using the same validated actual caller.
        f.reset(seed=193)
        f._put('wBattleMonStatus', 16 if burned else 0)
        f.commit_actions((MoveAction(slot), MoveAction(slot)))
        f._put('hBattleTurn', 0)
        f._put('wDamageTaken', 0, 2)
        f.prepare_call('DoTurn')
        f.run_prepared(bytes([71]) * 4096)
        results.append(150 - f.word('wEnemyMonHP'))
        burn_move_cases.append(dict(slot=slot, burned=burned, oracle_damage=oracle_damage,
                                    fixed_tape_damage=results[-1]))
    assert (0 < results[1] < results[0]) if slot == 0 else results[0] == results[1]


def install_terminal_check(sf):
    engine = sf.terminal_leaf
    original = engine.call

    def checked(entry):
        initial = bytes(engine.state)
        engine.prepare_call(entry)
        reference = State.from_buffer_copy(engine.state)
        C.memmove(C.byref(engine.state), initial, C.sizeof(State))
        stop = 65534
        expected = None
        if entry == 'ResolveFaints.check_battle_over' and 0 in (
                sf.fight.word('wBattleMonHP'), sf.fight.word('wEnemyMonHP')):
            expected = 'LostBattle' if sf.fight.word('wBattleMonHP') == 0 else 'WinTrainerBattle'
            stop = engine.address(expected)
        mg.oracle_run(C.byref(reference), 200000, stop, f.address('hROMBank'), hooks, len(hooks))
        assert not reference.fault, (entry, 'terminal oracle', reference.fault)
        event = original(entry)
        assert event == expected
        compare(reference, engine.state, entry)
        return event
    engine.call = checked


episodes = []
for mode in ('poison', 'burn', 'toxic'):
    for orientation in (0, 1):
        for side in (0, 1):
            sf = StatusFight(rom, sym, game, build / 'battle-env/caller',
                build / 'battle-status/leaf', seed=193, orientation=orientation,
                poisoned_side=side, status=mode)
            sf.fight._call = lambda entry: replay(sf.fight, entry)
            sf.leaf.call = lambda: replay(sf.leaf, sf.leaf.entry)
            install_terminal_check(sf)
            initial = bytes(sf.fight.state)
            while not sf.terminal and not sf.truncated:
                sf.step((MoveAction(0), MoveAction(1)))
                observed = sf.observation(0)
                assert all('toxic_count' in team[0] for team in observed['teams'])
            assert sf.terminal and not sf.truncated
            episodes.append(dict(mode=mode, orientation=orientation, side=side, turns=sf.fight.turn,
                winner=sf.winner, residuals=len(sf.residuals),
                final_toxic=sf.observation(0)['teams'][side][0]['toxic_count']))
            sf.reset(193)
            assert bytes(sf.fight.state) == initial

# Native-only complete fights and deterministic reset in both new modes.
native = []
for mode in ('burn', 'toxic'):
    sf = StatusFight(rom, sym, game, build / 'battle-env/caller',
        build / 'battle-status/leaf', seed=731, status=mode)
    finals = []
    for repetition in range(2):
        sf.reset(731)
        while not sf.terminal and not sf.truncated:
            sf.step((MoveAction(0), MoveAction(1)))
        assert sf.terminal and not sf.truncated
        finals.append(bytes(sf.fight.state))
    assert finals[0] == finals[1]
    native.append(dict(mode=mode, turns=sf.fight.turn, winner=sf.winner))

mg.oracle_close()
report = dict(scope='conditional six-hook headless status continuation, not whole endturn or ordinary frontend/RNG proof',
    routine_cases=routine_cases, burn_move_cases=burn_move_cases, episodes=episodes,
    comparisons=comparisons, native=native, source_sha256={p.name: hashlib.sha256(p.read_bytes()).hexdigest()
        for p in pathlib.Path(__file__).parent.glob('battle_status*.py')})
(build / 'battle-status/extended-report.json').write_text(json.dumps(report, indent=2) + '\n')
print(json.dumps(dict(routine_cases=len(routine_cases), burn_move_cases=burn_move_cases,
    episodes=episodes, comparisons=len(comparisons), native=native)))
