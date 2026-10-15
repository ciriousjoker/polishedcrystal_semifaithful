"""Independent ROM parity for bounded voluntary-switch battles."""
import argparse
import ctypes as C
import json
import itertools
import pathlib
from battle_actions import ActionFight, SwitchAction, ItemAction
from battle_teams import TeamFight
from battle_env import MoveAction, UnsupportedFight
from battle_env_check import Hook
from stateful_fixture_check import State
import battle_env_profile as base
import battle_actions_profile as actions_profile
import battle_party_profile as party_profile


_ORACLE = None
_ORACLE_ROM = None


def compare_call(env, entry, root, rows):
    """Compare exact prepared caller under the declared rendering projection."""
    global _ORACLE, _ORACLE_ROM
    f = env.fight
    if _ORACLE is None:
        _ORACLE = C.CDLL(str(root / 'build/battle-env/caller/headless-oracle.dylib'))
        _ORACLE.oracle_open.argtypes = [C.c_char_p]
        _ORACLE.oracle_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16,
                                      C.c_uint16, C.POINTER(Hook), C.c_uint32]
    if _ORACLE_ROM != f.rom_path:
        if _ORACLE_ROM is not None:
            _ORACLE.oracle_close()
        assert _ORACLE.oracle_open(str(f.rom_path).encode()) == 0
        _ORACLE_ROM = f.rom_path
    f.prepare_call(entry)
    ref = State.from_buffer_copy(f.state)
    names = actions_profile.PRESENTATION
    hooks = (Hook * len(names))(*[Hook(*f.symbols[n]) for n in names])
    expected = None
    if entry == 'ResolveFaints.check_battle_over':
        if not any(f.word('wPartyMon' + str(i) + 'HP') for i in (1, 2)):
            expected = 'LostBattle'
        elif not any(f.word('wOTPartyMon' + str(i) + 'HP') for i in (1, 2)):
            expected = 'WinTrainerBattle'
    _ORACLE.oracle_run(C.byref(ref), 200000, f.address(expected) if expected else 65534,
                      f.address('hROMBank'), hooks, len(hooks))
    assert not ref.fault, (entry, ref.fault, ref.pc, ref.bank)
    tape = tuple(ref.events[:ref.event_count])
    used, events = env.action_leaf.run_prepared(tape)
    assert used == tape
    for name in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'):
        assert getattr(ref, name) == getattr(f.state, name), (entry, name)
    differences = [a for a in (*range(0xc000, 0xe000), *range(0xff80, 0xffff))
                   if ref.m[a] != f.state.m[a]]
    assert not differences, (entry, differences[:10])
    f.rng.consume(len(used))
    rows.append(dict(entry=entry, div_events=len(used)))
    return used, events


def main(root):
    game = root / 'build/stateful/game'
    env = TeamFight(game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym',
        game, root / 'build/battle-env/caller', root / 'build/battle-party/sendin',
        root / 'build/battle-status/terminal', action_build=root / 'build/battle-actions/full')
    f = env.fight
    comparisons = []
    env.action_leaf.call = lambda entry: compare_call(env, entry, root, comparisons)
    episodes = []
    for teams, orientation in itertools.product(((0, 1), (0, 2), (1, 2)), (0, 1)):
        env.team_indices = teams
        for mode in ('player', 'enemy', 'both', 'item', 'full-restore', 'item-switch'):
            env.reset(193, orientation)
            initial = bytes(f.state)
            assert SwitchAction(1) in env.legal_actions(0)
            assert bytes(f.state) == initial
            action = tuple(SwitchAction(1) if mode == 'both' or mode == ('player', 'enemy')[side]
                           else MoveAction(0) for side in (0, 1))
            if mode in ('item', 'full-restore', 'item-switch'):
                f._put('wEnemyMonHP', 10, 2)
                action = (SwitchAction(1) if mode == 'item-switch' else MoveAction(0),
                          ItemAction(1 if mode == 'full-restore' else 0))
            env.step(action)
            if mode == 'player':
                assert env.last_order[0] == 0
            if mode == 'enemy':
                assert env.last_order[0] == 1
            if mode in ('item', 'full-restore', 'item-switch'):
                assert not f.byte('wEnemyTrainerItem' + ('2' if mode == 'full-restore' else '1'))
                assert f.word('wEnemyMonHP') > 10
                if mode == 'item-switch':
                    assert f.word('wEnemyMonHP') == 60
            if mode == 'both':
                speeds = [f.word(stem + 'Speed') for stem in ('wPartyMon1', 'wOTPartyMon1')]
                if speeds[0] != speeds[1]:
                    assert env.last_order[0] == (0 if speeds[0] > speeds[1] else 1)
            assert not f.byte('wPlayerSwitchTarget') and not f.byte('wEnemySwitchTarget')
            assert f.byte('wCurBattleMon') == (mode in ('player', 'both', 'item-switch'))
            assert f.byte('wCurOTMon') == (mode in ('enemy', 'both'))
            assert not f.byte('wDeferredSwitch')
            for _ in range(50):
                if env.terminal or env.truncated:
                    break
                env.step(tuple(next(a for a in env.legal_actions(side) if isinstance(a, MoveAction))
                               for side in (0, 1)))
            assert env.terminal and not env.truncated
            episodes.append(dict(teams=teams, orientation=orientation, mode=mode, turns=f.turn, winner=env.winner))
    result = dict(episodes=episodes, comparisons=comparisons)
    (root / 'build/battle-actions/switch-proof.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(dict(episodes=episodes, comparisons=len(comparisons))))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    main(parser.parse_args().artifact_root)
