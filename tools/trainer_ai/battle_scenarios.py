"""Battle-start roster records initialized through actual stat/EXP routines."""
from dataclasses import dataclass
import re
from battle_env import UnsupportedFight


@dataclass(frozen=True)
class Pokemon:
    species: str
    moves: tuple = ('TACKLE', 'WATER_GUN')
    level: int = 50
    ability_slot: int = 0
    item: str = 'NO_ITEM'
    form: int = 0
    status: int = 0
    nature: int = 0
    dvs: tuple = (0, 0, 0)
    evs: tuple = (0, 0, 0, 0, 0, 0)
    happiness: int = 70
    hp: int | None = None
    female: bool = False
    egg: bool = False
    pp: tuple | None = None


@dataclass(frozen=True)
class Scenario:
    player: tuple = (Pokemon('PSYDUCK'), Pokemon('PIDGEY'))
    enemy: tuple = (Pokemon('SENTRET', ability_slot=1), Pokemon('SPEAROW'))
    inventory: tuple = ('SUPER_POTION', 'FULL_RESTORE')
    bag: tuple = ()
    initial_options: int = 131  # natures, abilities and physical/special split
    initial_options2: int = 6  # modern EVs and actual NO_EXP_OPT; levels remain fixed


def initialize(machine, scenario=Scenario(), seed=193):
    """Set supplied game-start choices, compute stats/EXP with assembled rules."""
    m = machine
    masks = {}
    personality_source = (m.source_root / 'constants/pokemon_data_constants.asm').read_text()
    for name, expected in (('GENDER_MASK', 128), ('IS_EGG_MASK', 64), ('PP_MASK', 63)):
        match = re.search(r'^DEF\s+' + name + r'\s+EQU\s+%([01]+)\s*(?:;.*)?$', personality_source, re.M)
        if match is None or int(match[1], 2) != expected:
            raise UnsupportedFight('Scenario field encoding requires source review: ' + name)
        masks[name] = expected
    from battle_coverage import runtime_option_bits
    if not scenario.initial_options2 & (1 << runtime_option_bits(m.source_root)['NO_EXP_OPT']):
        raise UnsupportedFight('Fixed-level training requires the actual source no-experience option')
    for team in (scenario.player, scenario.enemy):
        if not 1 <= len(team) <= 6:
            raise UnsupportedFight('Ordinary party must contain one through six Pokemon')
        for member in team:
            if not 1 <= member.level <= 100 or not 1 <= len(member.moves) <= 4:
                raise UnsupportedFight('Invalid scenario level or move count')
            if member.ability_slot not in (0, 1, 2) or not 0 <= member.form <= 31:
                raise UnsupportedFight('Invalid scenario ability slot or form')
            if not 0 <= member.nature < 25 or len(member.dvs) != 3 or len(member.evs) != 6:
                raise UnsupportedFight('Invalid nature or genotype record')
            if any(type(value) is not int or not 0 <= value <= 255 for value in (*member.dvs, *member.evs)):
                raise UnsupportedFight('Invalid DV or EV byte')
            if type(member.female) is not bool or type(member.egg) is not bool:
                raise UnsupportedFight('Scenario gender and egg flags must be booleans')
            if member.pp is not None and (type(member.pp) is not tuple or len(member.pp) != len(member.moves) or
                    any(type(value) is not int or not 0 <= value <= masks['PP_MASK'] for value in member.pp)):
                raise UnsupportedFight('Initial PP must specify one integer per occupied move slot')
    if len(scenario.inventory) != 2:
        raise UnsupportedFight('Trainer inventory has exactly two slots')
    m.reset_empty(seed)
    m.call('ClearWindowData')
    m._put('wBattleMode', 2)
    m._put('wInitialOptions', scenario.initial_options)
    m._put('wInitialOptions2', scenario.initial_options2)
    classes = [line.split()[1] for line in
               (m.source_root / 'constants/trainer_constants.asm').read_text().splitlines()
               if line.strip().startswith('trainerclass ')]
    trainer = classes.index('YOUNGSTER')
    m._put('wOtherTrainerClass', trainer)
    m._put('wTrainerClass', trainer)
    m._put('wOtherTrainerID', 1)
    m._put('wEnemyItemState', 255)
    for index, item in enumerate(scenario.inventory):
        m._put('wEnemyTrainerItem' + str(index + 1), m._constant('constants/item_constants.asm', item) if item else 0)
    for side, team in enumerate((scenario.player, scenario.enemy)):
        party = ('wPartyMon', 'wOTPartyMon')[side]
        m._put(('wPartyCount', 'wOTPartyCount')[side], len(team))
        for index, member in enumerate(team):
            stem = party + str(index + 1)
            species = m._constant('constants/pokemon_constants.asm', member.species)
            m._put(stem + 'Species', species & 255)
            m._put(stem + 'Level', member.level)
            m._put(stem + 'Personality', ((member.ability_slot + 1) << 5) | member.nature)
            encoded_form = (member.form | (0x20 if species >= 256 else 0) |
                            (masks['GENDER_MASK'] if member.female else 0) |
                            (masks['IS_EGG_MASK'] if member.egg else 0))
            m._put(stem + 'Form', encoded_form)
            m._put(stem + 'Item', m._constant('constants/item_constants.asm', member.item))
            m._put(stem + 'Status', member.status)
            m._put(stem + 'Happiness', member.happiness)
            m.state.m[m.address(stem + 'DVs'):m.address(stem + 'DVs') + 3] = member.dvs
            m.state.m[m.address(stem + 'EVs'):m.address(stem + 'EVs') + 6] = member.evs
            for slot, name in enumerate(member.moves):
                move = m._constant('constants/move_constants.asm', name)
                m.state.m[m.address(stem + 'Moves') + slot] = move
                base_pp = m.move_data(move)[5]
                pp = base_pp if member.pp is None else member.pp[slot]
                if pp > base_pp:
                    raise UnsupportedFight('Initial PP exceeds assembled base PP; PP Ups are not configured')
                m.state.m[m.address(stem + 'PP') + slot] = pp
            length = m.address('wPartyMon2Nickname') - m.address('wPartyMon1Nickname')
            nickname = m._rom_bytes('PokemonNames', species * (length - 1), length - 1)
            m.state.m[m.address(stem + 'Nickname'):m.address(stem + 'Nickname') + length] = nickname + b'\x50'
            # Both teams use the real party-stat routine. Opposing battle-start
            # records temporarily occupy the source's player-party parameter
            # slot, then are copied back with no live fight yet in progress.
            start = m.address(stem)
            size = m.address(stem + 'End') - start
            if side:
                temporary = m.address('wPartyMon1')
                original = bytes(m.state.m[temporary:temporary + size])
                m.state.m[temporary:temporary + size] = m.state.m[start:start + size]
                target = 'wPartyMon1'
            else:
                target = stem
            m._put('wCurPartyMon', 0 if side else index)
            m.call('UpdatePkmnStats')
            hp = m.word(target + 'MaxHP') if member.hp is None else member.hp
            if not 0 <= hp <= m.word(target + 'MaxHP'):
                raise UnsupportedFight('Initial HP exceeds source-calculated MaxHP')
            m._put(target + 'HP', hp, 2)
            m._put('wCurSpecies', species & 255)
            m._put('wCurForm', member.form | (0x20 if species >= 256 else 0))
            m.call('GetBaseData')
            m.state.d = member.level
            m.call('CalcExpAtLevel')
            product = m.address('hProduct')
            m.state.m[m.address(target + 'Exp'):m.address(target + 'Exp') + 3] = m.state.m[product + 1:product + 4]
            if side:
                m.state.m[start:start + size] = m.state.m[temporary:temporary + size]
                m.state.m[temporary:temporary + size] = original
    m._put('wCurPartyMon', 0)
    m._put('wCurBattleMon', 0)
    m._put('wCurOTMon', 0)
    from battle_bag import initialize_bag
    initialize_bag(m, scenario.bag)
    return m
