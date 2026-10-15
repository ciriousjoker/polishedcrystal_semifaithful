"""Explicit observations and source-queried decisions for the full controller.

The controller freezes both actors' observations and masks before committing
either action. This module never reads pending actions, script state or RNG.
"""
import copy
from dataclasses import dataclass
import re

from battle_env import MoveAction, UnsupportedFight
from battle_actions import SwitchAction, ItemAction
from battle_bag import BagAction, inventory, item_record
from stateful_fixture_check import State

TrainerItemAction = ItemAction
QUERY_ROOTS = ('CheckLockedIn', 'CheckUsableMove', 'UserCanSwitch', 'GetAbility', 'EnemyUsedXItem', 'GetGender')
TRAINER_STAT_ITEMS = ('X_ATTACK', 'X_DEFEND', 'X_SPEED', 'X_SP_ATK', 'X_SP_DEF', 'X_ACCURACY')
TRAINER_OTHER_ITEMS = ('POTION', 'SUPER_POTION', 'HYPER_POTION', 'MAX_POTION',
                       'FULL_HEAL', 'FULL_RESTORE', 'GUARD_SPEC', 'DIRE_HIT')
SUBSTATUSES = (
    ('UNBURDEN', 'CURSE', 'PROTECT', 'IDENTIFIED', 'ENDURE', 'FLASH_FIRE', 'IN_LOVE'),
    ('MINIMIZED', 'FAINTED', 'IN_ABILITY', 'TRANSFORMED', 'LOCK_ON', 'DESTINY_BOND', 'CANT_RUN'),
    ('ROLLOUT', 'RAMPAGE', 'IN_LOOP', 'RECHARGE', 'CHARGED', 'UNDERGROUND', 'FLYING', 'CONFUSED'),
    ('CURLED', 'PENDING_ITEMLOSS', 'FOCUS_ENERGY', 'ROOST', 'SUBSTITUTE', 'FLINCHED', 'RAGE', 'LEECH_SEED'))


@dataclass(frozen=True)
class ContinueAction:
    """Continue the move locked by recharge, charge, rampage or rollout."""


@dataclass(frozen=True)
class StruggleAction:
    """Let the real move-selection caller choose its no-usable-moves action."""


def _side(side):
    if type(side) is not int or side not in (0, 1):
        raise UnsupportedFight('Actor must be zero or one')


def _literal(machine, name, expected):
    text = (machine.source_root / 'constants/pokemon_data_constants.asm').read_text()
    match = re.search(r'^DEF\s+' + re.escape(name) + r'\s+EQU\s+%([01]+)\s*(?:;.*)?$', text, re.M)
    if not match or int(match[1], 2) != expected:
        raise UnsupportedFight('Personality decoder requires review: ' + name)
    return expected


def _query(machine, entry, side, *, a=0, c=0, hl=0, presentation=False, party=None):
    """Execute a manifest entry on isolated CPU, RAM, stack and random state.

    Queries must return without input suspension or random draws. Stat item
    probes may use only the manifest's declared presentation projection, never
    policy or oracle callbacks. The caller explicitly opts into that projection.
    """
    if entry not in machine.entries:
        raise UnsupportedFight('Decision query is outside controller manifest: ' + entry)
    query = copy.copy(machine)
    query.state = State.from_buffer_copy(bytes(machine.state))
    query.rng = copy.deepcopy(machine.rng)
    query.events = []
    query.segment_checker = query.on_choice = None
    query._constant_cache = dict(machine._constant_cache)
    query._put('hBattleTurn', side)
    if party is not None:
        if entry != 'GetGender':
            raise UnsupportedFight('Party gender arguments supplied to another query')
        index, species = party
        # GetGender uses wCurPartyMon for both ordinary party layouts. Match
        # CheckOppositeGender's true party attributes, even under Transform.
        query._put('wMonType', side)
        query._put('wCurPartyMon', index)
        query._put('wCurPartySpecies', species)
    query.state.a, query.state.c = a, c
    query.state.h, query.state.l = hl >> 8, hl & 255
    query.state.fault = 0
    query.prepare_call(entry)
    for _ in range(128):
        boundary, used = query.raw_segment(query.rng.preview())
        if used:
            raise UnsupportedFight('Decision query consumed random events: ' + entry)
        if boundary is None:
            return query.state
        if not presentation:
            raise UnsupportedFight('Decision query suspended: ' + entry)
        import battle_controller_profile as profile
        if boundary.name not in profile.PRESENTATION:
            raise UnsupportedFight('Item legality reached a non-presentation boundary: ' + boundary.name)
        query.return_from_boundary()
    raise UnsupportedFight('Decision query presentation bound exceeded: ' + entry)


def trainer_item_actions(machine):
    """Filter only actual stat-handler refusal, never AI desirability thresholds.

    Guard Spec and Dire Hit source effects unconditionally execute, including
    when already active. Healing and status items likewise remain selectable
    when wasteful. The source stat handler can refuse at its actual stage limit
    or under abilities such as Contrary, so checking a hardcoded cap is wrong.
    """
    identifiers = {machine._constant('constants/item_constants.asm', name): name
                   for name in (*TRAINER_OTHER_ITEMS, *TRAINER_STAT_ITEMS)}
    result = []
    for slot in range(2):
        item = machine.byte('wEnemyTrainerItem' + str(slot + 1))
        if not item:
            continue
        name = identifiers.get(item)
        if name is None:
            raise UnsupportedFight('Trainer item has no reviewed source effect ABI')
        if name in TRAINER_STAT_ITEMS:
            state = _query(machine, 'EnemyUsedXItem', 1, a=item, presentation=True)
            if state.f & 16:
                continue
        result.append(ItemAction(slot))
    return tuple(result)


def _values(machine, stem, names):
    return tuple(machine.byte(stem + name) for name in names)


def _roster(machine, side):
    count = machine.byte(('wPartyCount', 'wOTPartyCount')[side])
    if not 1 <= count <= 6:
        raise UnsupportedFight('Party count is outside ordinary trainer bounds')
    active = machine.byte(('wCurBattleMon', 'wCurOTMon')[side])
    if not 0 <= active < count:
        raise UnsupportedFight('Active party index is invalid')
    party = ('wPartyMon', 'wOTPartyMon')[side]
    result = []
    for index in range(count):
        stored = party + str(index + 1)
        stem = ('wBattleMon', 'wEnemyMon')[side] if index == active else stored
        # Named symbols identify actual assembled fields. These distance checks
        # protect the few repeated arrays, rather than assuming struct offsets.
        moves_end = stem + ('DVs' if index == active else 'ID')
        if (machine.address(moves_end) - machine.address(stem + 'Moves') != 4 or
                machine.address(stem + 'Personality') - machine.address(stem + 'DVs') != 3 or
                machine.address(stem + 'Happiness') - machine.address(stem + 'PP') != 4):
            raise UnsupportedFight('Pokemon repeated-field layout changed')
        form = machine.byte(stem + 'Form')
        personality = machine.byte(stem + 'Personality')
        pp = tuple(machine.state.m[machine.address(stem + 'PP') + i] for i in range(4))
        member = dict(index=index, active=index == active,
                      species=machine.byte(stem + 'Species') | ((form & 32) << 3),
                      form=form & 31, egg=bool(form & 64),
                      level=machine.byte(stem + 'Level'), item=machine.byte(stem + 'Item'),
                      status=machine.byte(stem + 'Status'), nature=personality & 31,
                      ability_slot=(personality & 96) >> 5,
                      happiness=machine.byte(stem + 'Happiness'),
                      dvs=tuple(machine.state.m[machine.address(stem + 'DVs') + i] for i in range(3)),
                      moves=tuple(machine.state.m[machine.address(stem + 'Moves') + i] for i in range(4)),
                      pp=tuple(value & 63 for value in pp), pp_ups=tuple(value >> 6 for value in pp))
        gender = _query(machine, 'GetGender', side,
                        party=(index, machine.byte(stored + 'Species')))
        member['gender'] = ('genderless' if gender.f & 16 else
                            'female' if gender.f & 128 else 'male')
        member.update({name: machine.word(stem + name) for name in
                       ('HP', 'MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')})
        if index == active:
            member['ability'] = machine.byte(('wPlayerAbility', 'wEnemyAbility')[side])
            member['types'] = _values(machine, stem, ('Type1', 'Type2'))
        else:
            member['ability'] = _query(machine, 'GetAbility', side,
                c=machine.byte(stem + 'Species'), hl=machine.address(stem + 'Personality')).a
        result.append(member)
    return tuple(result)


def observation(machine, side):
    """Read a detached, explicit full-information state at a choice boundary.

    XP, EV progress, next level, pending moves, future RNG, CPU registers,
    scratch buffers and controller internals are deliberately absent.
    """
    _side(side)
    for name, expected in (('EXTSPECIES_MASK', 32), ('FORM_MASK', 31),
                           ('IS_EGG_MASK', 64), ('ABILITY_MASK', 96), ('NATURE_MASK', 31)):
        _literal(machine, name, expected)
    teams = tuple(_roster(machine, actor) for actor in (0, 1))
    battlefield = []
    for prefix in ('wPlayer', 'wEnemy'):
        raw_screens, raw_guards, raw_hazards = _values(machine, prefix, ('Screens', 'Guards', 'Hazards'))
        volatile = {}
        for number, names in enumerate(SUBSTATUSES, 1):
            value = machine.byte(prefix + 'SubStatus' + str(number))
            for name in names:
                bit = machine._constant('constants/battle_constants.asm', 'SUBSTATUS_' + name)
                if not 0 <= bit < 8:
                    raise UnsupportedFight('Volatile status layout changed')
                volatile[name.lower()] = bool(value & (1 << bit))
        field = dict(volatile=volatile,
                     stages=_values(machine, prefix, ('AtkLevel', 'DefLevel', 'SpeLevel', 'SAtkLevel',
                                                      'SDefLevel', 'AccLevel', 'EvaLevel')),
                     reflect_turns=raw_screens & 15, light_screen_turns=raw_screens >> 4,
                     safeguard_turns=raw_guards & 15, mist_turns=raw_guards >> 4,
                     spikes=(raw_hazards >> 4) & 3, toxic_spikes=(raw_hazards >> 6) & 3)
        for name in ('RolloutCount', 'ConfuseCount', 'ToxicCount', 'DisableCount', 'EncoreCount',
                     'PerishCount', 'ProtectCount', 'MetronomeCount', 'CudChewBerry',
                     'FutureSightCount', 'TrappingMove', 'WrapCount', 'Charging', 'TurnsTaken'):
            field[name] = machine.byte(prefix + name)
        for name in ('SubstituteHP', 'FutureSightDamage'):
            field[name] = machine.word(prefix + name)
        battlefield.append(field)
    return dict(side=side, turn=machine.turn, teams=teams, battlefield=tuple(battlefield),
                weather=machine.byte('wBattleWeather'), weather_turns=machine.byte('wWeatherCount'),
                trick_room=machine.byte('wTrickRoom'),
                trainer_items=tuple(machine.byte('wEnemyTrainerItem' + str(i)) for i in (1, 2)) if side else (),
                player_bag=inventory(machine) if not side else {}, terminal=machine.terminal,
                truncated=machine.truncated)


def _bag_actions(machine, count):
    names = {}
    path = machine.source_root / 'constants/item_constants.asm'
    first_block = path.read_text().split('DEF NUM_ITEMS EQU', 1)[0]
    for match in re.finditer(r'^\s*const\s+(\w+)', first_block, re.M):
        name = match[1]
        names[machine._constant('constants/item_constants.asm', name)] = name
    result = []
    for pocket, records in inventory(machine).items():
        occurrences = {}
        for identifier, quantity in records:
            if identifier not in names or not 1 <= quantity <= 99:
                raise UnsupportedFight('Unknown or corrupt bag stack')
            name = names[identifier]
            occurrence = occurrences.get(identifier, 0)
            occurrences[identifier] = occurrence + 1
            _, actual_pocket, context = item_record(machine, name)
            if actual_pocket != pocket or context not in (0, 4, 5, 6):
                raise UnsupportedFight('Unsupported assembled bag menu context')
            if context == 0:
                continue
            # Context 5 delegates target selection to the actual effect. Supply
            # complete target intents; source determines success and retry.
            if context == 5:
                result.extend(BagAction(name, occurrence, pokemon, move)
                              for pokemon in range(count) for move in range(4))
            else:
                result.append(BagAction(name, occurrence))
    return tuple(result)


def legal_actions(machine, side):
    """Return selectable intents, using actual source for move/trap legality.

    An item may be selectable but ineffective. Its actual source handler owns
    target rejection, retry and turn consumption. AI desirability is no mask.
    Forced replacement is a separate controller phase with its own targets.
    """
    _side(side)
    if machine.terminal or machine.truncated or machine.failed:
        return ()
    roster = _roster(machine, side)
    active = next(member for member in roster if member['active'])
    if not active['HP']:
        return ()
    locked = _query(machine, 'CheckLockedIn', side)
    if not locked.f & 128:
        return (ContinueAction(),)
    result = []
    for slot, move in enumerate(active['moves']):
        if move and _query(machine, 'CheckUsableMove', side, a=slot).f & 128:
            result.append(MoveAction(slot))
    if not result:
        result.append(StruggleAction())
    reserves = [member['index'] for member in roster
                if not member['active'] and member['HP'] and not member['egg']]
    if reserves and _query(machine, 'UserCanSwitch', side).f & 128:
        result.extend(SwitchAction(index) for index in reserves)
    if side:
        result.extend(trainer_item_actions(machine))
    else:
        result.extend(_bag_actions(machine, len(roster)))
    return tuple(result)
