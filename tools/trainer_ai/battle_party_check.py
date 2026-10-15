"""Whole source SendInUserPkmn parity in a conditional presentation profile."""
import argparse
import ctypes as C
import itertools
import json
import pathlib
import hashlib

from battle_env import NativeFight, MoveAction, UnsupportedFight
from battle_env_check import Hook
from battle_party import SendInLeaf, PartyFight
from battle_party_probe import fixture
from stateful_fixture_check import State
import battle_party_profile as profile


def main(root):
    game = root / 'build/stateful/game'
    fight = NativeFight(game / 'polishedcrystal-3.2.3.gbc',
        game / 'polishedcrystal-3.2.3.sym', game, root / 'build/battle-env/caller')
    leaf = SendInLeaf(fight, root / 'build/battle-party/sendin')
    oracle = C.CDLL(str(root / 'build/battle-env/caller/headless-oracle.dylib'))
    oracle.oracle_open.argtypes = [C.c_char_p]
    oracle.oracle_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16,
                                 C.c_uint16, C.POINTER(Hook), C.c_uint32]
    assert oracle.oracle_open(str(fight.rom_path).encode()) == 0
    hooks = (Hook * len(profile.PRESENTATION))(*[Hook(*fight.symbols[n]) for n in profile.PRESENTATION])
    classes = [line.split()[1] for line in (game / 'constants/trainer_constants.asm').read_text().splitlines()
               if line.strip().startswith('trainerclass ')]
    rows = []
    for side, fainted in itertools.product((0, 1), (False, True)):
        fight.reset(193)
        fixture(fight)
        fight._put('wOtherTrainerClass', classes.index('YOUNGSTER'))
        fight._put('wOtherTrainerID', 1)
        fight._put('hBattleTurn', side)
        if fainted:
            fight._put(('wBattleMonHP', 'wEnemyMonHP')[side], 0, 2)
        target = ('wPlayerSwitchTarget', 'wEnemySwitchTarget')[side]
        fight._put(target, 2)
        fight.prepare_call('SendInUserPkmn')
        ref = State.from_buffer_copy(fight.state)
        oracle.oracle_run(C.byref(ref), 200000, 65534, fight.address('hROMBank'), hooks, len(hooks))
        assert not ref.fault, ('mGBA', side, ref.fault, hex(ref.pc))
        tape = tuple(ref.events[:ref.event_count])
        used, events = leaf.run_prepared(tape)
        assert used == tape
        for n in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'):
            assert getattr(ref, n) == getattr(fight.state, n), (side, n)
        differences = [a for a in (*range(0xc000, 0xe000), *range(0xff80, 0xffff))
                       if ref.m[a] != fight.state.m[a]]
        assert not differences, (side, differences[:10])
        assert fight.byte(target) == 0
        assert fight.byte(('wCurBattleMon', 'wCurOTMon')[side]) == 1
        assert fight.word(('wBattleMonHP', 'wEnemyMonHP')[side]) == 113 + side
        rows.append(dict(side=side, fainted=fainted, presentation=events, steps=fight.state.steps))
    fixture_sha = hashlib.sha256((pathlib.Path(__file__).parent / 'battle_party_probe.py').read_bytes()).hexdigest()
    result = dict(scope='conditional-headless-whole-sendin', comparisons=rows,
                  fixture_source_sha256=fixture_sha)
    (root / 'build/battle-party/sendin-proof.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))

    episodes, comparisons, native_replays = [], [], []
    def compare(f, ref, entry):
        for name in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'):
            assert getattr(ref, name) == getattr(f.state, name), (entry, name)
        assert all(ref.m[a] == f.state.m[a] for a in
                   (*range(0xc000, 0xe000), *range(0xff80, 0xffff))), entry
        comparisons.append(dict(entry=entry, pc=f.state.pc, bank=f.state.bank))

    for orientation, strategy in itertools.product((0, 1), ('physical', 'special', 'alternating')):
        pf = PartyFight(game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym',
            game, root / 'build/battle-env/caller', root / 'build/battle-party/sendin',
            root / 'build/battle-status/terminal', seed=193, orientation=orientation)
        f = pf.fight
        import battle_env_profile as base
        base_hooks = (Hook * len(base.PRESENTATION))(*[Hook(*f.symbols[n]) for n in base.PRESENTATION])
        def checked_source(engine, entry, active_hooks):
            f.prepare_call(entry)
            ref = State.from_buffer_copy(f.state)
            oracle.oracle_run(C.byref(ref), 200000, 65534, f.address('hROMBank'), active_hooks, len(active_hooks))
            assert not ref.fault, (entry, ref.fault, hex(ref.pc))
            tape = tuple(ref.events[:ref.event_count])
            used, front = engine.run_prepared(tape)
            assert used == tape
            f.rng.consume(len(used))
            compare(f, ref, entry)
            return used, front
        f._call = lambda entry: checked_source(f, entry, base_hooks)
        pf.leaf.call = lambda entry: checked_source(pf.leaf, entry, hooks)
        original_terminal = pf.terminal_leaf.call
        def checked_terminal(entry):
            initial = bytes(f.state)
            f.prepare_call(entry)
            ref = State.from_buffer_copy(f.state)
            C.memmove(C.byref(f.state), initial, C.sizeof(State))
            expected = None
            if entry == 'ResolveFaints.check_battle_over':
                if not any(f.word('wPartyMon' + str(i) + 'HP') for i in (1, 2)):
                    expected = 'LostBattle'
                elif not any(f.word('wOTPartyMon' + str(i) + 'HP') for i in (1, 2)):
                    expected = 'WinTrainerBattle'
            oracle.oracle_run(C.byref(ref), 200000, f.address(expected) if expected else 65534,
                              f.address('hROMBank'), None, 0)
            assert not ref.fault, (entry, ref.fault, hex(ref.pc))
            event = original_terminal(entry)
            assert event == expected
            compare(f, ref, entry)
            return event
        pf.terminal_leaf.call = checked_terminal
        while not pf.terminal and not pf.truncated:
            slots = (0, 0) if strategy == 'physical' else (1, 1) if strategy == 'special' else (f.turn % 2, (f.turn + 1) % 2)
            actions = tuple(MoveAction(slot) if MoveAction(slot) in pf.legal_actions(side)
                            else pf.legal_actions(side)[0] for side, slot in enumerate(slots))
            pf.step(actions)
        assert pf.terminal and not pf.truncated
        assert pf.replacements
        observation = pf.observation(0)
        assert len(observation['teams'][0]) == len(observation['teams'][1]) == 2
        episodes.append(dict(orientation=orientation, strategy=strategy, turns=f.turn,
                             winner=pf.winner, replacements=pf.replacements))
        # Reusable public reset must clear source events, replacements and failed
        # state without loading, hashing or rebuilding libraries per episode.
        f._call = NativeFight._call.__get__(f)
        pf.leaf.call = SendInLeaf.call.__get__(pf.leaf)
        pf.terminal_leaf.call = original_terminal
        outcomes = []
        for replay in range(2):
            pf.reset(seed=193, orientation=orientation)
            initial = bytes(f.state)
            try:
                pf.step((MoveAction(99), MoveAction(0)))
                raise AssertionError('Invalid move accepted')
            except UnsupportedFight:
                assert bytes(f.state) == initial and not f.failed
            while not pf.terminal and not pf.truncated:
                slots = (0, 0) if strategy == 'physical' else (1, 1) if strategy == 'special' else (f.turn % 2, (f.turn + 1) % 2)
                actions = tuple(MoveAction(slot) if MoveAction(slot) in pf.legal_actions(side)
                                else pf.legal_actions(side)[0] for side, slot in enumerate(slots))
                pf.step(actions)
            assert pf.terminal and not pf.truncated
            outcomes.append((bytes(f.state), pf.observation(0), list(pf.replacements), list(f.events), pf.winner))
        assert outcomes[0] == outcomes[1]
        native_replays.append(dict(orientation=orientation, strategy=strategy, winner=pf.winner, turns=f.turn))
    assert {r['side'] for e in episodes for r in e['replacements']} == {0, 1}
    result = dict(scope='conditional-headless-host-orchestrated-2v2', episodes=episodes,
                  source_comparisons=comparisons, reset_native_replays=native_replays,
                  fixture_source_sha256=fixture_sha)
    (root / 'build/battle-party/fight-proof.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(dict(episodes=episodes, source_comparisons=len(comparisons)), indent=2))


def guards(root):
    game = root / 'build/stateful/game'
    pf = PartyFight(game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym',
        game, root / 'build/battle-env/caller', root / 'build/battle-party/sendin',
        root / 'build/battle-status/terminal')
    f = pf.fight
    assert 'wPartySpecies' not in f.symbols and 'wOTPartySpecies' not in f.symbols
    name_length = f.address('wPartyMon2Nickname') - f.address('wPartyMon1Nickname')
    species = f._constant('constants/pokemon_constants.asm', 'SMEARGLE')
    expected = f._rom_bytes('PokemonNames', species * (name_length - 1), name_length - 1)
    for side, party in enumerate(('wPartyMon', 'wOTPartyMon')):
        assert f.byte(('wPartyCount', 'wOTPartyCount')[side]) == 2
        for index in (1, 2):
            assert f.byte(party + str(index) + 'Species') == species
            address = f.address(party + str(index) + 'Nickname')
            assert bytes(f.state.m[address:address + name_length - 1]) == expected
            assert f.state.m[address + name_length - 1] == expected[-1]
        active = f.address(('wBattleMonNickname', 'wEnemyMonNickname')[side])
        assert bytes(f.state.m[active:active + name_length - 1]) == expected
    for side, party in enumerate(('wPartyMon', 'wOTPartyMon')):
        assert pf.reserves(side) == (1,)  # Active member is excluded.
        f._put(party + '2HP', 0, 2)
        assert pf.reserves(side) == ()
        pf.reset()
        p = f.address(party + '2Personality') + 1
        f.state.m[p] |= 0x40  # Raw validation fixture only, MON_IS_EGG_F.
        assert pf.reserves(side) == ()
        pf.reset()
    initial = bytes(f.state)
    try:
        while not pf.terminal and not pf.truncated:
            actions = tuple(pf.legal_actions(side)[0] for side in (0, 1))
            pf.step(actions, choose=lambda side, observation, legal: 99)
        raise AssertionError('Invalid replacement callback was never rejected')
    except UnsupportedFight as error:
        assert str(error) == 'Illegal replacement choice' and f.failed
    try:
        pf.step((MoveAction(0), MoveAction(0)))
        raise AssertionError('Partially failed fight reused')
    except UnsupportedFight:
        pass
    pf.reset()
    assert bytes(f.state) == initial and not f.failed and not pf.replacements and not f.events
    result = dict(active_mask=True, fainted_mask=True, egg_mask=True,
                  invalid_callback_poisoned=True, reset_restores_full_state=True,
                  party_count_and_species=True, source_table_nicknames=True,
                  source_layout_has_no_species_lists=True,
                  fixture_source_sha256=hashlib.sha256((pathlib.Path(__file__).parent /
                      'battle_party_probe.py').read_bytes()).hexdigest())
    (root / 'build/battle-party/guard-proof.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    parser.add_argument('--guards-only', action='store_true')
    args = parser.parse_args()
    (guards if args.guards_only else main)(args.artifact_root)
