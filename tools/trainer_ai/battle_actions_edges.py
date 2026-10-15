"""Targeted source-call item cases and public action/observation seam guards."""
import argparse
import hashlib
import json
import pathlib
import re
from battle_actions import SwitchAction, ItemAction, ActionLeaf
from battle_actions_check import compare_call
from battle_teams import TeamFight
from battle_env import MoveAction, UnsupportedFight


def make_env(root):
    game = root / 'build/stateful/game'
    return TeamFight(game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym',
                     game, action_build=root / 'build/battle-actions/full')


def enum_bit(fight, name):
    """Read only the simple local const block containing a status flag."""
    text = (fight.source_root / 'constants/battle_constants.asm').read_text()
    target = re.search(r'^\s*const ' + re.escape(name) + r'\s*(?:;.*)?$', text, re.M)
    assert target is not None
    start = text.rfind('const_def', 0, target.start())
    block = text[start:target.end()]
    assert not re.search(r'^\s*(if|else|endc|const_next)\b', block, re.M)
    initial = re.match(r'const_def(?:[ \t]+(\d+))?', block)
    value = int(initial[1] or 0)
    for line in block.splitlines()[1:]:
        line = line.split(';', 1)[0].strip()
        if line == 'const_skip':
            value += 1
        elif line.startswith('const '):
            if line.split()[1] == name:
                return value
            value += 1
        elif line:
            raise AssertionError('Unsupported local flag declaration: ' + line)
    raise AssertionError('Missing source flag')


def main(root):
    env = make_env(root)
    f = env.fight
    rows, items = [], []
    # Actual effect leaves, no hand-authored healing formula implementation.
    for entry, amount in (('EnemyUsedPotion', 20), ('EnemyUsedSuperPotion', 50),
                          ('EnemyUsedHyperPotion', 200), ('EnemyUsedMaxPotion', None),
                          ('EnemyUsedFullRestore', None), ('EnemyUsedFullHeal', 0)):
        for hp, maximum in ((10, 100), (95, 100), (250, 400), (400, 400)):
            env.reset()
            f._put('hBattleTurn', 1)
            f._put('wEnemyMonHP', hp, 2)
            f._put('wEnemyMonMaxHP', maximum, 2)
            status = (1 << enum_bit(f, 'PSN')) | (1 << enum_bit(f, 'TOX'))
            f._put('wEnemyMonStatus', status)
            f._put('wOTPartyMon1Status', status)
            confused_bit = enum_bit(f, 'SUBSTATUS_CONFUSED')
            f._put('wEnemySubStatus3', 1 << confused_bit)
            f._put('wEnemyConfuseCount', 3)
            compare_call(env, entry, root, rows)
            expected = maximum if amount is None else min(maximum, hp + amount)
            assert f.word('wEnemyMonHP') == expected
            assert f.word('wOTPartyMon1HP') == expected
            if entry in ('EnemyUsedFullRestore', 'EnemyUsedFullHeal'):
                assert f.byte('wEnemyMonStatus') == f.byte('wOTPartyMon1Status') == 0
                assert not f.byte('wEnemySubStatus3') & (1 << confused_bit)
            if entry == 'EnemyUsedFullRestore':
                assert f.byte('wEnemyConfuseCount') == 0
            elif entry == 'EnemyUsedFullHeal':
                assert f.byte('wEnemyConfuseCount') == 3
            assert f.byte('wEnemyItemState') == 254
            items.append(dict(entry=entry, hp=hp, maximum=maximum, result=expected))
    env.reset()
    pristine = bytes(f.state)
    before_rng = f.rng.rng.getstate()
    observation = env.observation(0)
    for side in (0, 1):
        assert SwitchAction(1) in env.legal_actions(side)
        assert SwitchAction(0) not in env.legal_actions(side)
    assert bytes(f.state) == pristine and f.rng.rng.getstate() == before_rng
    # Current action and RNG scratch are private even with full roster access.
    for field in ('wCurPlayerMove', 'wCurEnemyMove', 'wPlayerSwitchTarget',
                  'wEnemySwitchTarget', 'wEnemyUsingItem', 'hRandomAdd', 'hRandomSub'):
        f._put(field, 255)
    for i in range(7):
        f.state.m[f.address('wRNGState') + i] ^= 255
    assert env.observation(0) == observation
    env.reset()
    for side in (0, 1):
        f._put(('wPlayerWrapCount', 'wEnemyWrapCount')[side], 2)
        assert SwitchAction(1) not in env.legal_actions(side)
        env.reset()
        opponent_status = ('wEnemySubStatus2', 'wPlayerSubStatus2')[side]
        bit = enum_bit(f, 'SUBSTATUS_CANT_RUN')
        f._put(opponent_status, 1 << bit)
        assert SwitchAction(1) not in env.legal_actions(side)
        env.reset()
        party = ('wPartyMon2', 'wOTPartyMon2')[side]
        f._put(party + 'HP', 0, 2)
        assert SwitchAction(1) not in env.legal_actions(side)
        env.reset()
        a = f.address(party + 'Personality') + 1
        f.state.m[a] |= 0x40
        assert SwitchAction(1) not in env.legal_actions(side)
        env.reset()
    for invalid in ((ItemAction(0), MoveAction(0)), (MoveAction(0), ItemAction(2)),
                    (SwitchAction(0), MoveAction(0)), (MoveAction(True), MoveAction(0))):
        initial = bytes(f.state)
        rng = f.rng.rng.getstate()
        try:
            env.step(invalid)
        except UnsupportedFight:
            assert bytes(f.state) == initial and f.rng.rng.getstate() == rng and not f.failed
        else:
            raise AssertionError('Illegal action accepted')
    # Inventory consumption and no PP decrement on an item turn.
    initial_pp = tuple(f.state.m[f.address('wEnemyMonPP') + i] for i in range(4))
    env.step((MoveAction(0), ItemAction(0)))
    assert not f.byte('wEnemyTrainerItem1') and f.byte('wEnemyTrainerItem2')
    assert f.byte('wEnemyItemState') == 254
    assert tuple(f.state.m[f.address('wEnemyMonPP') + i] for i in range(4)) == initial_pp
    initial = bytes(f.state)
    try:
        env.step((MoveAction(0), ItemAction(0)))
    except UnsupportedFight:
        assert bytes(f.state) == initial and not f.failed
    else:
        raise AssertionError('Consumed item reused')
    # Execution exceptions after commit poison until public reset.
    env.reset()
    original_call = env.action_leaf.call
    def fail(entry):
        if entry == 'DoTurn':
            raise UnsupportedFight('Injected execution failure')
        return original_call(entry)
    env.action_leaf.call = fail
    try:
        env.step((MoveAction(0), MoveAction(0)))
    except UnsupportedFight:
        assert f.failed
    else:
        raise AssertionError('Injected execution failure did not propagate')
    try:
        env.step((MoveAction(0), MoveAction(0)))
    except UnsupportedFight:
        pass
    else:
        raise AssertionError('Poisoned environment reused')
    env.action_leaf.call = original_call
    env.reset()
    assert bytes(f.state) == pristine and not f.failed
    # Public trainer inventory permits duplicates, holes and all six effects.
    game = root / 'build/stateful/game'
    duplicate = TeamFight(game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym',
                          game, action_build=root / 'build/battle-actions/full',
                          inventory=('POTION', 'POTION'))
    duplicate.step((MoveAction(0), ItemAction(1)))
    assert duplicate.fight.byte('wEnemyTrainerItem1')
    assert not duplicate.fight.byte('wEnemyTrainerItem2')
    empty = TeamFight(game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym',
                     game, action_build=root / 'build/battle-actions/full', inventory=(None, None))
    assert not any(isinstance(action, ItemAction) for action in empty.legal_actions(1))
    for inventory in (('NOT_AN_ITEM', None), ('POTION',), ('POTION', None, None)):
        try:
            TeamFight(game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym',
                      game, action_build=root / 'build/battle-actions/full', inventory=inventory)
        except UnsupportedFight:
            pass
        else:
            raise AssertionError('Unknown or malformed inventory accepted')
    replayed = []
    for replay in range(2):
        env.reset(seed=269, orientation=1)
        trace = []
        while not env.terminal and not env.truncated:
            observations = tuple(env.observation(side) for side in (0, 1))
            legal = tuple(env.legal_actions(side) for side in (0, 1))
            chosen = (SwitchAction(1), ItemAction(0)) if f.turn == 0 else tuple(
                MoveAction((f.turn + side) % 2) for side in (0, 1))
            actions = tuple(action if action in legal[side] else legal[side][0]
                            for side, action in enumerate(chosen))
            events = env.step(actions)
            trace.append((observations, legal, actions, events))
        assert env.terminal and not env.truncated
        replayed.append((trace, env.winner, f.turn, bytes(f.state), f.rng.rng.getstate(),
                         list(env.replacements), list(f.events)))
    assert replayed[0] == replayed[1]
    replay_summary = dict(repeats=2, seed=269, orientation=1, turns=f.turn, winner=env.winner,
                          final_state_sha256=hashlib.sha256(bytes(f.state)).hexdigest())
    result = dict(item_cases=items, source_comparisons=rows,
                  legality_pure=True, trapped_fainted_egg_active_masks=True,
                  illegal_item_and_switch_atomic=True, no_current_actions_or_rng_observed=True,
                  item_inventory_consumed=True, item_turn_preserves_pp=True,
                  execution_poison_and_reset=True, public_duplicate_and_empty_inventory=True, deterministic_complete_replays=replay_summary)
    (root / 'build/battle-actions/edge-proof.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(dict(item_cases=len(items), source_comparisons=len(rows), guards='passed')))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    main(parser.parse_args().artifact_root)
