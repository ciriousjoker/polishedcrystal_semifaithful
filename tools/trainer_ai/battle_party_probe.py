"""Switch-prefix parity probe; no full-switch or 2v2 claim by this probe.

Uses fixed two-Smeargle parties and the existing State ABI. It compares the
actual computational prefix with independent mGBA stopped at the same PC.
No arbitrary state is accepted through the command line.
"""
import argparse
import ctypes as C
import json
import pathlib
import itertools
import re
import hashlib

from battle_env import NativeFight, UnsupportedFight
from battle_env_check import Hook
from stateful_fixture_check import State
import battle_party_profile as profile
import stateful_lifting


class PartyPrefix:
    def __init__(self, fight, build_dir):
        self.fight = fight
        manifest = json.loads((pathlib.Path(build_dir) / 'manifest.json').read_text())
        self.lib = stateful_lifting.load_verified(build_dir, fight.rom_path,
            fight.symbols_path, fight.source_root, profile.ENTRIES,
            profile.BOUNDARIES, tuple(manifest.get('excluded', {})))
        self.lib.stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]

    def run_prepared(self, entry):
        if entry not in profile.ENTRIES:
            raise UnsupportedFight('Unsupported party prefix entry')
        s = self.fight.state
        s.event_count = s.event_pos = 0
        self.lib.stateful_run(C.byref(s), 200000, 65534)
        if s.fault == 0:
            return None
        if s.fault == 5:
            for name in profile.BOUNDARIES:
                bank, pc = self.fight.symbols[name]
                if s.pc == pc and (pc < 0x4000 or s.bank == bank):
                    return name
        raise UnsupportedFight(f'Party prefix fault {s.fault} at {s.bank:02x}:{s.pc:04x}')


def fixture(fight):
    """Host battle-start fixture, never an implementation of switching."""
    constants = (fight.source_root / 'constants/text_constants.asm').read_text()
    name_length = int(re.search(r'^DEF MON_NAME_LENGTH\s+EQU\s+(\d+)$', constants, re.M).group(1))
    charmap = (fight.source_root / 'constants/charmap.asm').read_text()
    terminator = int(re.search(r'charmap "@",\s+\$([0-9a-fA-F]+)', charmap).group(1), 16)
    species = fight._constant('constants/pokemon_constants.asm', 'SMEARGLE')
    # Same source-table offset/length/terminator as home/names.asm:GetPokemonName.
    nickname = fight._rom_bytes('PokemonNames', species * (name_length - 1), name_length - 1)
    nickname += bytes((terminator,))
    fight._put('wInitialOptions', 2)  # ABILITIES_OPTMASK, constants/ram_constants.asm
    for side, party in enumerate(('wPartyMon', 'wOTPartyMon')):
        fight._put(('wPartyCount', 'wOTPartyCount')[side], 2)
        a = fight.address(party + '1')
        b = fight.address(party + '2')
        length = b - a
        fight.state.m[b:b + length] = fight.state.m[a:a + length]
        for index in (1, 2):
            fight._put(party + str(index) + 'Personality', 0x20)  # ABILITY_1
            address = fight.address(party + str(index) + 'Nickname')
            fight.state.m[address:address + name_length] = nickname
        # Distinct inactive party stats let checks detect stale active data.
        for name, value in zip(('HP', 'MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef'),
                               (113 + side, 173 + side, 45, 83, 107, 61, 92)):
            fight._put(party + '2' + name, value, 2)
        for slot in range(2):
            fight.state.m[fight.address(party + '2PP') + slot] = 7 + slot
        fight._put(('wBattleMonPersonality', 'wEnemyMonPersonality')[side], 0x20)


def main(root):
    game = root / 'build/stateful/game'
    fight = NativeFight(game / 'polishedcrystal-3.2.3.gbc',
        game / 'polishedcrystal-3.2.3.sym', game, root / 'build/battle-env/caller')
    leaf = PartyPrefix(fight, root / 'build/battle-party/prefix')
    oracle = C.CDLL(str(root / 'build/battle-env/caller/headless-oracle.dylib'))
    oracle.oracle_open.argtypes = [C.c_char_p]
    oracle.oracle_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16,
                                 C.c_uint16, C.POINTER(Hook), C.c_uint32]
    assert oracle.oracle_open(str(fight.rom_path).encode()) == 0
    rows = []
    for side, fainted in itertools.product((0, 1), (False, True)):
        fight.reset(193)
        fixture(fight)
        fight._put('hBattleTurn', side)
        target = ('wPlayerSwitchTarget', 'wEnemySwitchTarget')[side]
        fight._put(target, 2)  # Validated host choice: one-based reserve index.
        if fainted:
            fight._put(('wBattleMonHP', 'wEnemyMonHP')[side], 0, 2)
            fight._put(('wPartyMon1HP', 'wOTPartyMon1HP')[side], 0, 2)
        team = ('wPlayer', 'wEnemy')[side]
        for stat in ('Atk', 'Def', 'SAtk', 'SDef', 'Spe', 'Acc', 'Eva'):
            fight._put(team + stat + 'Level', 13)
        for suffix in ('SubStatus1', 'SubStatus2', 'SubStatus3', 'SubStatus4',
                       'DisableCount', 'EncoreCount', 'CudChewBerry', 'TurnsTaken'):
            fight._put(team + suffix, 255)
        for entry in ('GetUserSwitchTarget', 'SendInUserPkmn'):
            fight.prepare_call(entry)
            ref = State.from_buffer_copy(fight.state)
            stop = fight.address('SendInUserPkmn.done_ability') if entry == 'SendInUserPkmn' else 65534
            oracle.oracle_run(C.byref(ref), 200000, stop, fight.address('hROMBank'), None, 0)
            assert not ref.fault, ('mGBA', entry, ref.fault, ref.pc)
            event = leaf.run_prepared(entry)
            assert event == ('SendInUserPkmn.done_ability' if entry == 'SendInUserPkmn' else None)
            for n in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'):
                assert getattr(ref, n) == getattr(fight.state, n), (entry, n)
            assert not ref.event_count and not fight.state.event_pos
            assert all(ref.m[a] == fight.state.m[a] for a in
                       (*range(0xc000, 0xe000), *range(0xff80, 0xffff))), entry
            rows.append(dict(side=side, fainted=fainted, entry=entry, event=event, steps=fight.state.steps))
        stem = ('wBattleMon', 'wEnemyMon')[side]
        assert fight.byte(('wCurBattleMon', 'wCurOTMon')[side]) == 1
        assert fight.word(stem + 'HP') == 113 + side
        assert fight.word(stem + 'MaxHP') == 173 + side
        assert tuple(fight.word(stem + n) for n in ('Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')) == (45, 83, 107, 61, 92)
        assert tuple(fight.state.m[fight.address(stem + 'PP') + i] for i in range(2)) == (7, 8)
        assert tuple(fight.byte(stem + n) for n in ('Type1', 'Type2')) == (0, 0)
        assert all(fight.byte(team + stat + 'Level') == 7
                   for stat in ('Atk', 'Def', 'SAtk', 'SDef', 'Spe', 'Acc', 'Eva'))
        assert all(fight.byte(team + n) == 0 for n in
                   ('SubStatus1', 'SubStatus2', 'SubStatus3', 'SubStatus4',
                    'DisableCount', 'EncoreCount', 'CudChewBerry', 'TurnsTaken'))
        assert fight.byte(('wPlayerAbility', 'wEnemyAbility')[side]) == fight._constant(
            'constants/ability_constants.asm', 'OWN_TEMPO')
    for side, expected in enumerate(('GetPlayerSwitchTarget', 'GetUserSwitchTarget.enemy_switch')):
        fight.reset(193)
        fixture(fight)
        fight._put('hBattleTurn', side)
        fight.prepare_call('GetUserSwitchTarget')
        ref = State.from_buffer_copy(fight.state)
        oracle.oracle_run(C.byref(ref), 200000, fight.address(expected),
                          fight.address('hROMBank'), None, 0)
        assert not ref.fault
        event = leaf.run_prepared('GetUserSwitchTarget')
        assert event == expected
        assert all(getattr(ref, n) == getattr(fight.state, n) for n in
                   ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'))
        assert all(ref.m[a] == fight.state.m[a] for a in
                   (*range(0xc000, 0xe000), *range(0xff80, 0xffff)))
        rows.append(dict(side=side, entry='GetUserSwitchTarget', event=event))
    result = dict(scope='computational-prefix-only', comparisons=rows,
                  fixture_source_sha256=hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest())
    (root / 'build/battle-party/prefix-proof.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    main(parser.parse_args().artifact_root)
