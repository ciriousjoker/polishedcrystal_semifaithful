"""Small heterogeneous battle-start teams derived from the assembled base table.

These are synthetic level-50 battle fixtures, not a cartridge team-builder or
an assertion of natural learnset legality. Combat stats use the documented zero
EV/DV neutral formula; species types and ability slots come from ROM tables.
"""
from battle_actions import ActionFight
from battle_env import UnsupportedFight

TEAMS = (
    (('PSYDUCK', 0), ('PIDGEY', 0)),
    (('SENTRET', 1), ('SPEAROW', 0)),
    (('SMEARGLE', 1), ('HOOTHOOT', 0)),
)


class TeamFight(ActionFight):
    def __init__(self, *args, teams=(0, 1), **kwargs):
        if len(teams) != 2 or any(type(i) is not int or not 0 <= i < len(TEAMS) for i in teams):
            raise UnsupportedFight('Unknown team fixture')
        self.team_indices = tuple(teams)
        super().__init__(*args, **kwargs)

    def _initialize_party(self):
        super()._initialize_party()
        f = self.fight
        # The RAM and table records share BASE_DATA_SIZE in the actual source.
        size = f.address('wCurBaseDataEnd') - f.address('wCurBaseData')
        self.roster_profiles = []
        indices = self.team_indices[::-1] if f.orientation else self.team_indices
        for side, team_index in enumerate(indices):
            party = ('wPartyMon', 'wOTPartyMon')[side]
            profiles = []
            for index, (name, ability_slot) in enumerate(TEAMS[team_index]):
                species = f._constant('constants/pokemon_constants.asm', name)
                data = f._rom_bytes('BaseData', (species - 1) * size, size)
                assert len(data) == size
                stem = party + str(index + 1)
                f._put(stem + 'Species', species)
                # lvl50, zero DV/EV: HP=base+60, other stats=base+5.
                # Source stat generation is outside this battle-start interface.
                hp, attack, defense, speed, special_attack, special_defense = data[:6]
                for key, value in zip(('HP', 'MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef'),
                    (hp + 60, hp + 60, attack + 5, defense + 5, speed + 5,
                     special_attack + 5, special_defense + 5)):
                    f._put(stem + key, value, 2)
                f._put(stem + 'Personality', (ability_slot + 1) << 5)
                for slot, move in enumerate(f.moves):
                    f.state.m[f.address(stem + 'PP') + slot] = f.move_data(move)[5]
                length = f.address('wPartyMon2Nickname') - f.address('wPartyMon1Nickname')
                nickname = f._rom_bytes('PokemonNames', species * (length - 1), length - 1)
                f.state.m[f.address(stem + 'Nickname'):f.address(stem + 'Nickname') + length] = nickname + b'\x50'
                types_offset = f.address('wBaseType1') - f.address('wCurBaseData')
                ability_offset = f.address('wBaseAbility1') - f.address('wCurBaseData')
                profiles.append(dict(types=tuple(data[types_offset:types_offset + 2]),
                                     ability=data[ability_offset + ability_slot]))
            self.roster_profiles.append(tuple(profiles))
            self._sendin(side, 0)
            active = ('wBattleMon', 'wEnemyMon')[side]
            assert tuple(f.byte(active + key) for key in ('Type1', 'Type2')) == profiles[0]['types']
            assert f.byte(('wPlayerAbility', 'wEnemyAbility')[side]) == profiles[0]['ability']

    def observation(self, side):
        result = super().observation(side)
        # PartyFight's homogeneous cached metadata is deliberately replaced with
        # each member's own ROM-derived profile, including inactive reserves.
        for actor, team in enumerate(result['teams']):
            for index, mon in enumerate(team):
                mon.update(self.roster_profiles[actor][index])
        return result
