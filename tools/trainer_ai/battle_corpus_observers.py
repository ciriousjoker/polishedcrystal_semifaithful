"""Fail-closed semantic activation checks for actual-source move trace frames.

These checks establish that a scenario exercised a mechanism. They do not
reimplement numerical battle rules or replace ROM differential comparison.
Diagnostics and source command traces are validation-only, never AI inputs.
"""
from dataclasses import dataclass
import hashlib


@dataclass(frozen=True)
class MoveFrame:
    before: dict
    after: dict
    actor_side: int
    executed_move: int
    source_commands: tuple[str, ...]
    attack_missed: int
    phase: str = 'single-source-move'
    opponent_executed_move: int | None = None
    move_execution_verified: bool = False


@dataclass(frozen=True)
class ProbeResult:
    state: str  # activated, unsatisfied, blocked
    reason: str
    frame_index: int | None = None
    candidate_rules: tuple[str, ...] = ()


DAMAGE_ONLY = {'EFFECT_NORMAL_HIT', 'EFFECT_PRIORITY_HIT', 'EFFECT_ALWAYS_HIT',
               'EFFECT_STATIC_DAMAGE', 'EFFECT_LEVEL_DAMAGE'}
STATUS = {
    'EFFECT_POISON': ('PSN', 'BattleCommand_poison'),
    'EFFECT_TOXIC': ('TOXIC', 'BattleCommand_toxic'),
    'EFFECT_BURN': ('BRN', 'BattleCommand_burn'),
    'EFFECT_PARALYZE': ('PAR', 'BattleCommand_paralyze'),
    'EFFECT_SLEEP': ('SLEEP', 'BattleCommand_sleep'),
    'EFFECT_FREEZE': ('FRZ', 'BattleCommand_freezetarget'),
    'EFFECT_POISON_HIT': ('PSN', 'BattleCommand_poisontarget'),
    'EFFECT_BURN_HIT': ('BRN', 'BattleCommand_burntarget'),
    'EFFECT_PARALYZE_HIT': ('PAR', 'BattleCommand_paralyzetarget'),
    'EFFECT_SLEEP_HIT': ('SLEEP', 'BattleCommand_sleep'),
    'EFFECT_FREEZE_HIT': ('FRZ', 'BattleCommand_freezetarget'),
}
WEATHER = {'EFFECT_RAIN_DANCE': ('WEATHER_RAIN', 'BattleCommand_startrain'),
           'EFFECT_SUNNY_DAY': ('WEATHER_SUN', 'BattleCommand_startsun'),
           'EFFECT_SANDSTORM': ('WEATHER_SANDSTORM', 'BattleCommand_startsandstorm'),
           'EFFECT_HAIL': ('WEATHER_HAIL', 'BattleCommand_starthail')}
STATS = ('ATTACK', 'DEFENSE', 'SPEED', 'SP_ATK', 'SP_DEF', 'ACCURACY', 'EVASION')
# Each tuple is side relative to actor, stage array index, direction. Numeric
# increment size, saturation, ability interactions and damage stay ROM-owned.
COMPOUND_STAGES = {
    'EFFECT_BULK_UP': ((0, 0, 1), (0, 1, 1)),
    'EFFECT_CALM_MIND': ((0, 3, 1), (0, 4, 1)),
    'EFFECT_GROWTH': ((0, 0, 1), (0, 3, 1)),
    'EFFECT_DRAGON_DANCE': ((0, 0, 1), (0, 2, 1)),
    'EFFECT_HONE_CLAWS': ((0, 0, 1), (0, 5, 1)),
    'EFFECT_SHELL_SMASH': ((0, 0, 1), (0, 3, 1), (0, 2, 1), (0, 1, -1), (0, 4, -1)),
}
FIELDS = {'EFFECT_REFLECT': ('reflect_turns', 0),
          'EFFECT_LIGHT_SCREEN': ('light_screen_turns', 0),
          'EFFECT_SAFEGUARD': ('safeguard_turns', 0)}


def source_bindings(machine, inventory):
    """Resolve observer masks through the runner's same-source constant reader."""
    for path in ('constants/battle_constants.asm', 'data/moves/moves.asm',
                 'constants/move_constants.asm', 'constants/move_effect_constants.asm',
                 'constants/ability_constants.asm', 'engine/battle/endturn.asm'):
        actual = hashlib.sha256((machine.source_root / path).read_bytes()).hexdigest()
        if inventory['source_inputs'].get(path) != actual:
            raise ValueError('Observer source binding is stale: ' + path)
    import re
    source = (machine.source_root / 'constants/battle_constants.asm').read_text()
    if not re.search(r'^DEF SLP_MASK EQU %111\s*(?:;.*)?$', source, re.M):
        raise ValueError('Sleep status mask requires source review')
    endturn = (machine.source_root / 'engine/battle/endturn.asm').read_text()
    # The low nibble is the remaining Encore duration; the high nibble holds
    # the last move index even without active Encore (also used for Choice).
    # Require both duration testing and move-index extraction to remain explicit.
    region = re.search(r'^HandleEncore:\s*\n(.*?)^TickDisableAndEncoreAfterMove:',
                       endturn, re.M | re.S)
    if region is None:
        raise ValueError('Encore duration region requires source review')
    code = '\n'.join(line.split(';', 1)[0].strip() for line in region[1].splitlines()
                     if line.split(';', 1)[0].strip())
    duration = re.search(r'^\.do_encore\nld a, \[hl\]\nand \$([0-9a-f]+)\nret z\n'
                         r'push hl\nld a, \[hl\]\nswap a\nand \$f\ndec a$', code, re.M)
    if duration is None or int(duration[1], 16) != 15 or not re.search(
            r'^EndturnEncoreDisable:\nld a, \[hl\]\nand \$f\nret z\ndec \[hl\]$', code, re.M):
        raise ValueError('Encore duration mask requires source review')
    return dict(inventory_sha256=inventory['inventory_sha256'],
                encore_duration_mask=int(duration[1], 16),
                moves={row['name']: row['identifier'] for row in inventory['rules'] if row['group'] == 'move'},
                status_masks={name: 1 << machine._constant('constants/battle_constants.asm', name)
                              for name in ('PSN', 'BRN', 'FRZ', 'PAR', 'TOX')},
                sleep_mask=7,
                weather={name: machine._constant('constants/battle_constants.asm', name)
                         for name in ('WEATHER_NONE', *(name for name, _ in WEATHER.values()))},
                neutral_ability=machine._constant('constants/ability_constants.asm', 'SWIFT_SWIM'))


def _active(observation, side):
    members = [member for member in observation['teams'][side] if member['active']]
    if len(members) != 1:
        raise ValueError('Observation needs exactly one active Pokemon per side')
    return members[0]


def _same_participant(frame, side):
    return _active(frame.before, side)['index'] == _active(frame.after, side)['index']


def _changed_hp(frame, side, direction):
    # Replacement can occur before the frame ends. A reserve's HP must never
    # be mistaken for damage/healing to the previous combatant.
    if not _same_participant(frame, side):
        return False
    first, last = _active(frame.before, side), _active(frame.after, side)
    return (last['HP'] - first['HP']) * direction > 0


def _command(frame, name):
    return name in frame.source_commands or frame.phase == 'controlled-source-turn'


def _damage(frame):
    return (_command(frame, 'BattleCommand_applydamage') and
            frame.attack_missed == 0 and _changed_hp(frame, frame.actor_side ^ 1, -1))


def _stage_recipe(effect):
    if effect in COMPOUND_STAGES:
        return COMPOUND_STAGES[effect]
    for index, stat in enumerate(STATS):
        prefix = 'EFFECT_' + stat
        if not effect.startswith(prefix + '_'):
            continue
        tail = effect[len(prefix) + 1:]
        if tail in ('UP', 'UP_2', 'UP_HIT'):
            return ((0, index, 1),)
        if tail in ('DOWN', 'DOWN_2', 'DOWN_HIT'):
            return ((1, index, -1),)
    return None


def supported_effect(effect):
    return (effect in DAMAGE_ONLY or effect in STATUS or effect in WEATHER or
            effect in FIELDS or effect in {'EFFECT_HEAL', 'EFFECT_HEALING_LIGHT', 'EFFECT_SPLASH',
                                          'EFFECT_CONFUSE', 'EFFECT_CONFUSE_HIT'} or
            _stage_recipe(effect) is not None)


def _splash_projection(observation, bindings):
    # Deliberately excludes PP and ordinary turn/cursor bookkeeping. Include
    # active HP/status/item/ability/moves/types and field state. Encore's last
    # move index is ordinary move-use bookkeeping; retain its actual duration.
    # Every other field flag, counter and stage remains compared unchanged.
    fields = []
    for field in observation['battlefield']:
        count = field['EncoreCount']
        if type(count) is not int or not 0 <= count <= 255:
            raise ValueError('Invalid Encore counter byte')
        fields.append({key: (value & bindings['encore_duration_mask'] if key == 'EncoreCount' else value)
                       for key, value in field.items() if key != 'TurnsTaken'})
    return (tuple(tuple((member['index'], member['active'], member['HP'], member['status'],
                         member['item'], member['ability'], tuple(member['moves']),
                         tuple(member.get('types', ()))) for member in team)
                  for team in observation['teams']),
            tuple(fields), observation['weather'], observation['weather_turns'],
            observation['trick_room'])


def _activated(effect, frame, bindings):
    side, opposite = frame.actor_side, frame.actor_side ^ 1
    if effect in DAMAGE_ONLY:
        extra = effect not in {'EFFECT_STATIC_DAMAGE', 'EFFECT_LEVEL_DAMAGE'} or _command(frame, 'BattleCommand_constantdamage')
        return extra and _damage(frame)
    if effect in STATUS:
        name, command = STATUS[effect]
        if not _command(frame, command) or not _same_participant(frame, opposite):
            return False
        old, new = (_active(state, opposite)['status'] for state in (frame.before, frame.after))
        if name == 'SLEEP':
            changed = not old & bindings['sleep_mask'] and bool(new & bindings['sleep_mask'])
        elif name == 'TOXIC':
            mask = bindings['status_masks']['PSN'] | bindings['status_masks']['TOX']
            changed = (old & mask) != mask and (new & mask) == mask
        else:
            mask = bindings['status_masks'][name]
            changed = not old & mask and bool(new & mask)
        return frame.attack_missed == 0 and changed and (not effect.endswith('_HIT') or _damage(frame))
    if effect in WEATHER:
        name, command = WEATHER[effect]
        wanted = bindings['weather'][name]
        return (_command(frame, command) and frame.before['weather'] != wanted and
                frame.after['weather'] == wanted and frame.after['weather_turns'] > 0)
    if effect in FIELDS:
        field, relative_side = FIELDS[effect]
        actor = side ^ relative_side
        old, new = (state['battlefield'][actor][field] for state in (frame.before, frame.after))
        # Source command spellings differ by screen. State change and move
        # identity are both required; caller owns exact single-move span.
        return frame.attack_missed == 0 and old == 0 and new > 0
    if effect in {'EFFECT_HEAL', 'EFFECT_HEALING_LIGHT'}:
        command = 'BattleCommand_heal' if effect == 'EFFECT_HEAL' else 'BattleCommand_healweather'
        return _command(frame, command) and frame.attack_missed == 0 and _changed_hp(frame, side, 1)
    if effect in {'EFFECT_CONFUSE', 'EFFECT_CONFUSE_HIT'}:
        command = 'BattleCommand_confuse' if effect == 'EFFECT_CONFUSE' else 'BattleCommand_confusetarget'
        old, new = (state['battlefield'][opposite] for state in (frame.before, frame.after))
        return (_command(frame, command) and frame.attack_missed == 0 and
                not old['volatile']['confused'] and new['volatile']['confused'] and
                new['ConfuseCount'] > 0 and (effect != 'EFFECT_CONFUSE_HIT' or _damage(frame)))
    stages = _stage_recipe(effect)
    if stages:
        if frame.attack_missed or any(not _same_participant(frame, side ^ relative) for relative, _, _ in stages):
            return False
        if not any(_command(frame, name) for name in ('BattleCommand_raisestat', 'BattleCommand_lowerstat',
                       'BattleCommand_raisestathit', 'BattleCommand_forceraisestat',
                       'BattleCommand_forcelowerstat', 'BattleCommand_loweroppstat',
                       'BattleCommand_loweroppstathit', 'ForceRaiseStat', 'ForceLowerStat')):
            return False
        changed = all((frame.after['battlefield'][side ^ relative]['stages'][index] -
                       frame.before['battlefield'][side ^ relative]['stages'][index]) * direction > 0
                      for relative, index, direction in stages)
        return changed and (not effect.endswith('_HIT') or _damage(frame))
    if effect == 'EFFECT_SPLASH':
        if not _command(frame, 'BattleCommand_splash') or not _same_participant(frame, side):
            return False
        before, after = (_active(state, side) for state in (frame.before, frame.after))
        used = any(old > new for old, new in zip(before['pp'], after['pp']))
        return used and _splash_projection(frame.before, bindings) == _splash_projection(frame.after, bindings)
    return False


def _controlled_fixture(frame, bindings, effect):
    """Derive a narrow noninterference condition from explicit observations.

    This is a practical source-backed fixture restriction, not a general theorem
    about arbitrary battle states. The baseline actor/target are neutral Horsea
    with Swift Swim, no items/status/volatiles/field effects, and opposing Splash.
    Actual move-execution metadata must come from the source controller, never
    merely from submitted actions or a reduced PP count.
    """
    if not frame.move_execution_verified or frame.opponent_executed_move != bindings['moves'].get('SPLASH'):
        return False
    if effect.endswith('_HIT') and effect not in DAMAGE_ONLY:
        return False  # secondary activation requires an action-local span
    if effect not in (DAMAGE_ONLY | set(STATUS) | set(WEATHER) | set(FIELDS) |
                      {'EFFECT_HEAL', 'EFFECT_HEALING_LIGHT', 'EFFECT_SPLASH', 'EFFECT_CONFUSE'}):
        if _stage_recipe(effect) is None:
            return False
    if any(not _same_participant(frame, side) for side in (0, 1)):
        return False
    before = frame.before
    if (before['weather'] != bindings['weather']['WEATHER_NONE'] or before['weather_turns'] or
            before['trick_room']):
        return False
    if any(member['status'] or member['item'] for team in before['teams'] for member in team):
        return False
    if any(_active(before, side)['ability'] != bindings['neutral_ability'] for side in (0, 1)):
        return False
    for field in before['battlefield']:
        if any(field['volatile'].values()):
            return False
        if any(field[name] for name in ('reflect_turns', 'light_screen_turns', 'safeguard_turns',
                                        'mist_turns', 'spikes', 'toxic_spikes', 'FutureSightCount',
                                        'TrappingMove', 'WrapCount', 'PerishCount')):
            return False
    return True


def observe_move(case, frames, bindings):
    """Require matching actual execution and a positive semantic transition.

    Single-move diagnostic spans permit exact command-presence checks. A narrow
    controlled-source-turn with verified opposing Splash can instead establish
    clear first-family transitions without requiring a new instruction tracer.
    Unknown effects and incomplete/confounded frames remain blocked.
    """
    if case.inventory_sha256 != bindings['inventory_sha256']:
        return ProbeResult('blocked', 'Observer bindings do not match the case source inventory.')
    if not supported_effect(case.effect):
        return ProbeResult('blocked', 'No reviewed semantic observer for ' + case.effect)
    found = False
    for index, frame in enumerate(frames):
        if (frame.actor_side != case.actor_side or
                frame.executed_move != bindings['moves'].get(case.move)):
            continue
        try:
            if frame.phase == 'controlled-source-turn':
                if not _controlled_fixture(frame, bindings, case.effect):
                    continue
            elif frame.phase == 'single-source-move':
                if 'BattleCommand_usedmovetext' not in frame.source_commands:
                    continue
            else:
                continue
            found = True
            if _activated(case.effect, frame, bindings):
                return ProbeResult('activated', 'The intended source mechanism produced its required state transition.',
                                   index, case.candidate_rules)
        except (KeyError, TypeError, IndexError, ValueError):
            return ProbeResult('blocked', 'Diagnostic observation or source binding is incomplete.')
    return ProbeResult('unsatisfied' if found else 'blocked',
                       'No positive semantic transition in retained move spans.' if found else
                       'No matching unconfounded actual execution span was supplied.')
