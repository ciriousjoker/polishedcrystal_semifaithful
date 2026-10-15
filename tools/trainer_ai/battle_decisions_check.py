"""Contract checks to invoke on a source-initialized ControllerMachine.

These checks test decision isolation and observation content. They do not claim
ROM equivalence; the controller oracle must separately validate query entries.
"""
import copy
from dataclasses import asdict, is_dataclass

import battle_decisions as decisions


def check_machine(machine):
    before = bytes(machine.state)
    random_before = copy.deepcopy(machine.rng.rng.getstate())
    events_before = list(machine.events)
    checker, callback = machine.segment_checker, machine.on_choice
    records = []
    for side in (0, 1):
        first = decisions.observation(machine, side)
        actions = decisions.legal_actions(machine, side)
        assert first == decisions.observation(machine, side)
        assert actions == decisions.legal_actions(machine, side)
        assert bytes(machine.state) == before
        assert machine.rng.rng.getstate() == random_before
        assert machine.events == events_before
        assert machine.segment_checker is checker and machine.on_choice is callback
        assert first['side'] == side
        assert not first['player_bag'] if side else not first['trainer_items']
        assert len(first['teams']) == 2
        assert all(1 <= len(team) <= 6 for team in first['teams'])
        for team in first['teams']:
            for member in team:
                assert {'species', 'level', 'moves', 'pp', 'ability', 'gender', 'HP', 'MaxHP'} <= member.keys()
                assert member['gender'] in ('male', 'female', 'genderless')
                assert not {'Exp', 'EVs', 'next_level', 'pending_action', 'rng'} & member.keys()
        records.append(dict(side=side, observation=first,
                            actions=[dict(type=type(action).__name__,
                                          **(asdict(action) if is_dataclass(action) else {}))
                                     for action in actions]))
    # Observations are detached values, not aliases onto mutable simulation RAM.
    records[0]['observation']['teams'][0][0]['HP'] = -1
    assert decisions.observation(machine, 0)['teams'][0][0]['HP'] >= 0
    assert bytes(machine.state) == before
    return dict(passed=True, actors=2, state_and_random_unchanged=True,
                pending_choices_absent=True, detached_observations=True,
                records=records[1:])


def check_gender_queries(machine, oracle):
    """Compare isolated gender queries with actual ROM on both party layouts.

    This is a routine-level check, not full-fight coverage for Attract or Rivalry.
    The deliberately contradictory form flags test fixed-gender species; the
    active record's opposite flag tests the true-party query contract.
    """
    from stateful_fixture_check import State
    before = bytes(machine.state)
    rng_before = copy.deepcopy(machine.rng.rng.getstate())
    rows = []
    fixtures = (('PSYDUCK', False, 'male'), ('PSYDUCK', True, 'female'),
                ('NIDORAN_M', True, 'male'), ('NIDORAN_F', False, 'female'),
                ('MAGNEMITE', False, 'genderless'), ('MAGNEMITE', True, 'genderless'))
    for side in (0, 1):
        for species_name, female, expected in fixtures:
            clone = copy.copy(machine)
            clone.state = State.from_buffer_copy(before)
            clone.rng = copy.deepcopy(machine.rng)
            clone.events = []
            clone.segment_checker = oracle.compare
            clone.single_step_checker = clone.on_choice = None
            clone._constant_cache = dict(machine._constant_cache)
            species = clone._constant('constants/pokemon_constants.asm', species_name)
            stem = ('wPartyMon1', 'wOTPartyMon1')[side]
            clone._put(stem + 'Species', species)
            clone._put(stem + 'Form', 128 if female else 0)
            # Even an opposite active form flag must not replace true-party
            # attributes used by CheckOppositeGender in the actual source.
            clone._put(('wBattleMonForm', 'wEnemyMonForm')[side], 0 if female else 128)
            queried = decisions._query(clone, 'GetGender', side, party=(0, species))
            actual = 'genderless' if queried.f & 16 else 'female' if queried.f & 128 else 'male'
            assert actual == expected, (species_name, female, actual, expected)
            clone._put('hBattleTurn', side)
            clone._put('wMonType', side)
            clone._put('wCurPartyMon', 0)
            clone._put('wCurPartySpecies', species)
            clone.state.a = clone.state.c = clone.state.h = clone.state.l = 0
            clone.state.fault = 0
            clone.prepare_call('GetGender')
            boundary, used = oracle.compare(clone, 65534)
            assert boundary is None and not used
            assert bytes(queried.m) == bytes(clone.state.m)
            assert all(getattr(queried, name) == getattr(clone.state, name)
                       for name in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'))
            rows.append(dict(side=side, species=species_name, recorded_female=female,
                             gender=actual, actual_rom_equal=True))
    assert bytes(machine.state) == before and machine.rng.rng.getstate() == rng_before
    return dict(scope='isolated-source-gender-queries', cases=rows,
                unchanged_original=True, full_fight_mechanics_claimed=False)
