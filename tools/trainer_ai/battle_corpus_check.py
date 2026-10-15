"""Cheap corpus-factory contract checks, separate from actual fight validation."""
import argparse
import json
from pathlib import Path
import re

from battle_corpus import desired_moves, move_cases, scenario


def check(inventory_path, source_root):
    inventory = json.loads(inventory_path.read_text())
    cases = list(move_cases(inventory))
    moves = [row for row in inventory['rules'] if row['group'] == 'move']
    assert len(cases) == len(moves) * 4
    assert len({case.name for case in cases}) == len(cases)
    assert {case.move for case in cases} == {row['name'] for row in moves}
    keys = {row['key'] for row in inventory['rules']}
    assert all(set(case.candidate_rules) <= keys for case in cases)
    assert all(case.inventory_sha256 == inventory['inventory_sha256'] for case in cases)
    assert all(case.record()['coverage_phase'] == 'unproven' for case in cases)
    source = (source_root / 'constants/battle_constants.asm').read_text()
    status_block = source.split('; status condition bit flags', 1)[1].split('DEF ALL_STATUS', 1)[0]
    start = int(re.search(r'const_def (\d+)', status_block)[1])
    masks = {name: 1 << (start + index)
             for index, name in enumerate(re.findall(r'\bconst (\w+)', status_block))}
    assert re.search(r'DEF SLP_MASK EQU %111', status_block)
    ready = blocked = 0
    for case in cases:
        if not case.ready:
            try:
                scenario(case, status_values=masks)
            except ValueError:
                blocked += 1
            else:
                raise AssertionError('Blocked case materialized')
            continue
        ready += 1
        fight = scenario(case, status_values=masks)
        assert len(fight.player) == len(fight.enemy) == 2
        assert fight.inventory == (None, None)
        actor = (fight.player, fight.enemy)[case.actor_side][0]
        assert case.move in actor.moves
        assert all(1 <= len(member.moves) <= 4 for team in (fight.player, fight.enemy) for member in team)
        assert desired_moves(case, len(case.prelude))[case.actor_side] == case.move
        finish = len(case.prelude) + case.probe_turns
        assert desired_moves(case, finish) == ('FINISH_LEGAL_ATTACK', 'FINISH_LEGAL_ATTACK')
        assert fight.initial_options2 & 4  # checked against actual source NO_EXP_OPT by controller gate
    by_move = {case.move: case for case in cases if case.actor_side == 0 and case.seed == 193}
    assert by_move['COUNTER'].opponent['moves'][0] == 'TACKLE'
    assert by_move['MIRROR_COAT'].opponent['moves'][0] == 'WATER_GUN'
    assert by_move['ROOST'].actor['species'] == 'PIDGEY'
    assert by_move['SKETCH'].prelude[0].opponent == 'BUBBLE'
    assert 'BUBBLE' not in by_move['SKETCH'].actor['moves']
    assert not by_move['ATTRACT'].ready
    assert not by_move['CONVERSION'].ready
    assert by_move['SLEEP_TALK'].actor['status_name'] == 'SLP_3'
    assert by_move['DREAM_EATER'].opponent['status_name'] == 'SLP_3'
    # Immutable record container does not allow shared mutable per-case rosters.
    original = cases[1].actor['species']
    cases[0].actor['species'] = 'MISSINGNO'
    assert cases[1].actor['species'] == original
    return dict(plans=len(cases), materializable=ready, fixture_blocked=blocked,
                full_fights_executed=0, coverage_claims=0)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--inventory', type=Path, required=True)
    parser.add_argument('--source-root', type=Path, required=True)
    args = parser.parse_args()
    print(json.dumps(check(args.inventory, args.source_root), sort_keys=True))
