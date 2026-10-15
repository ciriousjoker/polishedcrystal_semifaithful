#!/usr/bin/env python3
"""Assert actual-caller parity under the explicitly instrumented headless profile.

This checks CPU/RAM equality conditional on the listed frontend identity-return
hooks. It does not establish equivalence to the unmodified graphical frontend.
"""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
import subprocess

from battle_env import NativeFight, MoveAction, UnsupportedFight
from stateful_fixture_check import State
import stateful_lifting
import battle_env_profile as profile

ROOT = pathlib.Path(__file__).resolve().parents[2]


class Hook(C.Structure):
    _fields_ = [('bank', C.c_uint16), ('pc', C.c_uint16)]


def run(rom, symbols, source_root, build_dir, mgba_prefix):
    build_dir.mkdir(parents=True, exist_ok=True)
    oracle = build_dir / 'headless-oracle.dylib'
    command = ['cc', '-O2', '-dynamiclib', '-I' + str(mgba_prefix / 'include'),
               str(ROOT / 'tools/trainer_ai/battle_env_oracle.c'),
               '-L' + str(mgba_prefix / 'lib'), '-lmgba',
               '-Wl,-rpath,' + str(mgba_prefix / 'lib'), '-o', str(oracle)]
    subprocess.run(command, check=True)
    mg = C.CDLL(str(oracle))
    mg.oracle_open.argtypes = [C.c_char_p]
    mg.oracle_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16,
                             C.c_uint16, C.POINTER(Hook), C.c_uint32]
    assert mg.oracle_open(str(rom).encode()) == 0
    transitions = []
    episodes = []
    for orientation in (0, 1):
        for strategy in ('physical', 'special', 'alternating'):
            fight = NativeFight(rom, symbols, source_root, build_dir,
                                seed=193 + orientation, orientation=orientation)
            hooks = (Hook * len(profile.PRESENTATION))(
                *[Hook(*fight.symbols[n]) for n in profile.PRESENTATION])

            def checked_call(entry):
                fight.prepare_call(entry)
                reference = State.from_buffer_copy(fight.state)
                mg.oracle_run(C.byref(reference), 200000, 65534,
                              fight.address('hROMBank'), hooks, len(hooks))
                assert not reference.fault, (entry, 'oracle', reference.fault, hex(reference.pc))
                tape = tuple(reference.events[:reference.event_count])
                used, frontend = fight.run_prepared(tape)
                native = fight.state
                assert used == tape, (entry, 'RNG replay consumption')
                fields = ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank')
                differences = {n:(getattr(reference, n), getattr(native, n))
                               for n in fields if getattr(reference, n) != getattr(native, n)}
                ram = [a for a in [*range(0xc000, 0xe000), *range(0xff80, 0xffff)]
                       if reference.m[a] != native.m[a]]
                assert not differences and not ram, (entry, differences, ram[:10])
                transitions.append(dict(orientation=orientation, strategy=strategy,
                    turn=fight.turn, actor=fight.byte('hBattleTurn'), entry=entry,
                    div_events=tape, presentation=frontend, steps=native.steps,
                    hp=[fight.word('wBattleMonHP'), fight.word('wEnemyMonHP')],
                    cpu_ram_sha256=hashlib.sha256(bytes(native)[:65550]).hexdigest()))
                return used, frontend

            fight._call = checked_call
            while not fight.terminal and not fight.truncated:
                if strategy == 'physical':
                    actions = (MoveAction(0), MoveAction(0))
                elif strategy == 'special':
                    actions = (MoveAction(1), MoveAction(1))
                else:
                    actions = (MoveAction(fight.turn % 2), MoveAction((fight.turn + 1) % 2))
                fight.step(actions)
            assert fight.terminal and not fight.truncated, (strategy, 'incomplete fight')
            assert fight.winner in (0, 1)
            # A defeated slower actor must not consume PP or execute another turn.
            assert fight.events[-1].target_hp == 0
            assert len(fight.events) <= fight.turn * 2
            episodes.append(dict(orientation=orientation, strategy=strategy,
                                 turns=fight.turn, winner=fight.winner,
                                 attacks=len(fight.events), final=fight.observation(0)))
    mg.oracle_close()

    # Native-only whole fights prove the production transition loop does not
    # depend on oracle throughput. No performance or training-strength claim.
    native_episodes = []
    for orientation in (0, 1):
        fight = NativeFight(rom, symbols, source_root, build_dir,
                            seed=731, orientation=orientation)
        while not fight.terminal and not fight.truncated:
            fight.step((MoveAction(fight.turn % 2), MoveAction((fight.turn + 1) % 2)))
        assert fight.terminal and not fight.truncated
        native_episodes.append(dict(orientation=orientation, turns=fight.turn,
                                    winner=fight.winner, attacks=len(fight.events)))

    fight = NativeFight(rom, symbols, source_root, build_dir)
    before = bytes(fight.state)
    try:
        fight.step((MoveAction(2), MoveAction(0)))
    except UnsupportedFight:
        assert bytes(fight.state) == before
    else:
        raise AssertionError('Illegal slot accepted')
    assert fight.moves == (33, 55), 'Fixture constant identities changed; review required'
    assert fight.observation(0)['teams'][0][0]['Attack'] != fight.observation(0)['teams'][1][0]['Attack']
    for side in (0, 1):
        stem = ('wBattleMon', 'wEnemyMon')[side]
        for slot in range(2):
            fight.state.m[fight.address(stem + 'PP') + slot] = 0
        assert fight.legal_actions(side) == ()

    # Configuration and frontend body changes never become silently accepted.
    try:
        stateful_lifting.load_verified(build_dir, rom, symbols, source_root,
                                       profile.ENTRIES)
    except ValueError as error:
        assert 'boundary configuration' in str(error)
    else:
        raise AssertionError('Omitted frontend profile accepted')
    crash_pc = fight.symbols['Crash'][1]
    assert crash_pc < 0x4000
    try:
        stateful_lifting.build(rom, symbols, ['DoNothing'], build_dir / 'negative-alias',
                              boundaries=[(1, crash_pc)])
    except ValueError as error:
        assert 'Ambiguous' in str(error)
    else:
        raise AssertionError('ROM0 Crash alias became a presentation boundary')
    mutated = bytearray(pathlib.Path(rom).read_bytes())
    bank, pc = fight.symbols['StdBattleTextbox']
    offset = pc if pc < 0x4000 else bank * 0x4000 + pc - 0x4000
    mutated[offset] ^= 1
    bad_rom = build_dir / 'changed-frontend.gbc'
    bad_rom.write_bytes(mutated)
    try:
        profile.verify_audit(bad_rom, symbols, source_root)
    except ValueError as error:
        assert 'Headless ROM profile changed' in str(error)
    else:
        raise AssertionError('Changed presentation body accepted after regeneration')
    finally:
        bad_rom.unlink()

    # A transitive dependency below a skipped frontend root is pinned too.
    changed = bytearray(pathlib.Path(rom).read_bytes())
    bank, pc = fight.symbols['_StdBattleTextbox']
    offset = bank * 0x4000 + pc - 0x4000
    changed[offset] ^= 1
    bad_rom.write_bytes(changed)
    try:
        try:
            profile.verify_audit(bad_rom, symbols, source_root)
        except ValueError as error:
            assert 'Headless ROM profile changed' in str(error)
        else:
            raise AssertionError('Changed transitive frontend code accepted')
    finally:
        bad_rom.unlink()
    fight.reset()
    original_call = fight._call
    def failed_execution(entry):
        raise IndexError('Injected partial execution fault')
    fight._call = failed_execution
    try:
        fight.step((MoveAction(0), MoveAction(1)))
    except IndexError:
        assert fight.failed
    else:
        raise AssertionError('Injected failure did not propagate')
    fight._call = original_call
    try:
        fight.step((MoveAction(0), MoveAction(1)))
    except UnsupportedFight:
        pass
    else:
        raise AssertionError('Poisoned fight resumed without reset')
    fight.reset()
    assert not fight.failed

    report = dict(scope='conditional instrumented-headless actual caller parity; frontend equivalence unproven',
                  rom_sha256=hashlib.sha256(pathlib.Path(rom).read_bytes()).hexdigest(),
                  oracle_wrapper_sha256=hashlib.sha256((ROOT / 'tools/trainer_ai/battle_env_oracle.c').read_bytes()).hexdigest(),
                  environment_sha256=hashlib.sha256((ROOT / 'tools/trainer_ai/battle_env.py').read_bytes()).hexdigest(),
                  profile_sha256=hashlib.sha256((ROOT / 'tools/trainer_ai/battle_env_profile.py').read_bytes()).hexdigest(),
                  checker_sha256=hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest(),
                  episodes=episodes, transitions=transitions, native_episodes=native_episodes,
                  guards=['illegal slot atomic rejection', 'zero PP no legal action',
                          'boundary profile mismatch', 'ROM0 Crash alias rejection',
                          'changed presentation body rejection', 'changed transitive frontend rejection',
                          'all execution exception types poison until reset'])
    (build_dir / 'fight-report.json').write_text(json.dumps(report, indent=2) + '\n')
    return report


if __name__ == '__main__':
    p = argparse.ArgumentParser()
    p.add_argument('--rom', type=pathlib.Path, required=True)
    p.add_argument('--sym', type=pathlib.Path, required=True)
    p.add_argument('--source-root', type=pathlib.Path, required=True)
    p.add_argument('--build-dir', type=pathlib.Path, required=True)
    p.add_argument('--mgba-prefix', type=pathlib.Path, required=True)
    p.add_argument('--build', action='store_true')
    args = p.parse_args()
    if args.build:
        profile.build(args.rom, args.sym, args.source_root, args.build_dir)
    result = run(args.rom, args.sym, args.source_root, args.build_dir, args.mgba_prefix)
    print(json.dumps(dict(episodes=len(result['episodes']),
                          transitions=len(result['transitions']),
                          native_episodes=result['native_episodes'], guards=result['guards'])))
