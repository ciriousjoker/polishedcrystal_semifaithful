"""Source-bound full-fight case plans, never a substitute for observed coverage.

Catalog construction is cheap and does not load native libraries or an emulator.
A case's candidate_rules are hypotheses, not evidence or accepted coverage keys.
"""
from __future__ import annotations

from dataclasses import asdict, dataclass
import hashlib
import json
from pathlib import Path

SCHEMA = 1
DEFAULT_SEEDS = (193, 271)


@dataclass(frozen=True)
class TurnPlan:
    """Both intents are chosen from the same pre-turn state, before stepping."""
    actor: str
    opponent: str


@dataclass(frozen=True)
class Case:
    name: str
    inventory_sha256: str
    actor_side: int
    seed: int
    move: str
    effect: str
    actor: dict
    opponent: dict
    prelude: tuple[TurnPlan, ...]
    probe_turns: int
    candidate_rules: tuple[str, ...]
    prerequisites: tuple[str, ...]
    semantic_requirements: tuple[str, ...]
    fixture_requirements: tuple[str, ...] = ()
    max_turns: int = 256

    @property
    def ready(self):
        """Ready for the proposed runner, not automatically positive coverage."""
        return not self.fixture_requirements

    def record(self):
        result = asdict(self)
        result['ready'] = self.ready
        result['coverage_phase'] = 'unproven'
        return result


def _moves(*names):
    return tuple(dict.fromkeys(names))[:4]


def _member(move, **overrides):
    # Swift Swim has no effect in neutral weather. Neither combatant has a
    # baseline type/ability immunity to the ordinary status families.
    return dict(species='HORSEA', moves=_moves(move, 'TACKLE', 'WATER_GUN', 'SPLASH'),
                level=50, ability_slot=0, **overrides)


def _recipe(rule):
    """Explicit initial conditions and source-generated setup for move families.

    Unsupported fixtures remain visible rather than silently running a failed
    move. These are plans; actual source branches and state changes must be
    checked in a retained differential full-fight report.
    """
    effect = rule['effect']
    actor, target = _member(rule['name']), _member('SPLASH')
    prelude, prerequisites, fixtures = (), [], []
    turns = 1
    if rule['power'] != '0':
        expected = ['Probe move executes, accuracy outcome recorded, and successful damage reaches the intended target.']
    else:
        expected = ['Probe move executes and its named effect changes the intended battle state.']
    if effect in {'EFFECT_HEAL', 'EFFECT_HEALING_LIGHT', 'EFFECT_ROOST', 'EFFECT_LEECH_HIT'}:
        actor['hp'] = 10
        prerequisites.append('Actor starts below source-calculated maximum HP; target uses Splash during probe.')
        expected.append('Actor HP increases, with the source maximum-HP cap respected.')
        if effect == 'EFFECT_LEECH_HIT':
            actor['hp'] = 85
            prerequisites.append('Near-full initial HP exercises source healing saturation when absorption exceeds the observed gap.')
    if effect == 'EFFECT_RECOIL_HIT':
        expected.append('Both target HP loss and actor recoil HP loss occur during the actual source move.')
        prerequisites.append('Neutral Swift Swim and no items avoid recoil suppression or other HP effects.')
        if rule['name'] == 'STRUGGLE':
            actor['moves'], actor['pp'] = ('TACKLE',), (0,)
            prerequisites.append('Known Tackle starts at zero PP; the source legal mask must force the actual StruggleAction.')
    if effect == 'EFFECT_ROOST':
        actor['species'] = 'PIDGEY'
        expected.append('Flying type is removed during the turn and restored at the source end-turn point.')
    if effect == 'EFFECT_DREAM_EATER':
        target['status_name'] = 'SLP_3'
        actor['hp'] = 10
        prerequisites.append('Target starts asleep and actor is injured.')
        expected.append('Sleeping target takes damage and actor recovers HP.')
    if effect == 'EFFECT_SLEEP_TALK':
        actor['status_name'] = 'SLP_3'
        prerequisites.append('Actor starts asleep and knows eligible ordinary attacking moves.')
        expected = ['Sleep Talk selects an eligible different move and executes it while actor remains asleep.']
    if effect in {'EFFECT_HEAL_BELL'}:
        actor['status_name'] = 'PSN'
        prerequisites.append('Actor and reserve start poisoned.')
        expected = ['Party primary status is cured, including the inactive reserve.']
    if effect in {'EFFECT_DISABLE', 'EFFECT_ENCORE', 'EFFECT_SKETCH'}:
        target['moves'] = ('BUBBLE', 'SPLASH', 'TACKLE')
        prelude = (TurnPlan('SPLASH', 'BUBBLE'),)
        prerequisites.append('Opponent executes Bubble first; actor does not already know Bubble.')
        expected = [dict(EFFECT_DISABLE='Bubble becomes disabled for its source duration.',
                         EFFECT_ENCORE='Opponent is locked into Bubble for its source duration.',
                         EFFECT_SKETCH='Sketch slot and party PP/move record become Bubble.')[effect]]
    if effect == 'EFFECT_COUNTER':
        attack = 'WATER_GUN' if rule['category'] == 'SPECIAL' else 'TACKLE'
        target['moves'] = (attack, 'SPLASH')
        prerequisites.append('Opponent selects a matching-category damaging move on the retaliation turn.')
        expected = ['A matching-category hit is received before retaliation and source-calculated retaliation damage is applied.']
    if effect == 'EFFECT_SUCKER_PUNCH':
        target['moves'] = ('TACKLE', 'SPLASH')
        prerequisites.append('Opponent selects a damaging move on the probe turn.')
        expected = ['Sucker Punch succeeds before the opposing damaging move, rather than its failure branch.']
    if effect in {'EFFECT_THIEF', 'EFFECT_TRICK', 'EFFECT_KNOCK_OFF', 'EFFECT_BUG_BITE'}:
        target['item'] = 'SITRUS_BERRY'
        if effect == 'EFFECT_TRICK':
            actor['item'] = 'ORAN_BERRY'
        expected.append('Held-item ownership/consumption changes according to the actual selected effect.')
    if effect == 'EFFECT_PAIN_SPLIT':
        actor['hp'] = 10
        expected = ['Both HP values change to source-computed shared HP, independently capped by each maximum.']
    if effect in {'EFFECT_REVERSAL', 'EFFECT_CONDITIONAL_BOOST'}:
        if rule['name'] in {'FACADE'}:
            actor['status_name'] = 'PSN'
        elif rule['name'] in {'HEX', 'VENOSHOCK'}:
            target['status_name'] = 'PSN'
        elif rule['name'] == 'AVALANCHE':
            target['moves'] = ('TACKLE', 'SPLASH')
        else:
            actor['hp'] = 10 if effect == 'EFFECT_REVERSAL' else None
        expected.append('Check the active power formula against a contrasting unboosted case; one damage sample is insufficient.')
    if effect == 'EFFECT_CONVERSION':
        prerequisites.append('Actor is Water-type and knows Tackle, allowing a non-Water conversion.')
        fixtures.append('Typed nested Conversion move/type selection must select Tackle on player side.')
        expected = ['Actor changes both battle types to Normal through the real Conversion selection path.']
    if effect == 'EFFECT_ATTRACT':
        fixtures.append('Scenario initialization must support explicit opposite recorded genders.')
        expected = ['Opposite-gender target gains infatuation; same-gender negative control must fail.']
    if effect in {'EFFECT_PURSUIT', 'EFFECT_RAPID_SPIN', 'EFFECT_BRICK_BREAK', 'EFFECT_EARTHQUAKE', 'EFFECT_GUST', 'EFFECT_STOMP'}:
        requirements = {
            'EFFECT_PURSUIT': 'Opponent must select a voluntary switch on the probe turn.',
            'EFFECT_RAPID_SPIN': 'Actor side must have source-created entry hazards or a source-created partial trap/Leech Seed.',
            'EFFECT_BRICK_BREAK': 'Opponent must establish Reflect and/or Light Screen through source move execution.',
            'EFFECT_EARTHQUAKE': 'Target must be in the source Dig underground state for its boosted branch.',
            'EFFECT_GUST': 'Target must be in the source Fly airborne state for its boosted branch.',
            'EFFECT_STOMP': 'Target must first execute Minimize for its boosted branch.',
        }
        fixtures.append(requirements[effect])
        expected.append('The conditional interaction must activate; ordinary damage alone does not cover this effect branch.')
    if effect in {'EFFECT_BATON_PASS', 'EFFECT_ROAR', 'EFFECT_SWITCH_HIT', 'EFFECT_TELEPORT'}:
        prerequisites.append('Both parties contain a living reserve; replacement choice is explicit.')
        expected.append('Source switching/replacement changes the active index and runs entry effects.')
        if effect == 'EFFECT_BATON_PASS':
            prelude = (TurnPlan('AGILITY', 'SPLASH'),)
            actor['moves'] = _moves(rule['name'], 'AGILITY', 'TACKLE', 'SPLASH')
            expected.append('Speed stage passes to the selected reserve; ordinary-switch negative control resets it.')
    if effect in {'EFFECT_RAMPAGE', 'EFFECT_HYPER_BEAM', 'EFFECT_ROLLOUT', 'EFFECT_SOLAR_BEAM', 'EFFECT_FLY'}:
        turns = 4
        expected.append('Continue through forced continuation/recharge/charge state and its release, observing PP semantics.')
    if effect in {'EFFECT_DESTINY_BOND', 'EFFECT_ENDURE', 'EFFECT_FALSE_SWIPE', 'EFFECT_PROTECT'}:
        fixtures.append('Arrange a source-calculated lethal opposing attack and verify the conditional prevention/faint result.')
    if effect in {'EFFECT_SPIKES', 'EFFECT_TOXIC_SPIKES'}:
        fixtures.append('Opponent must switch after hazard creation; repeat to test source layer limits and grounded entry.')
        expected.append('A later entrant receives the actual source hazard effect; setting the field byte alone is insufficient.')
    if effect == 'EFFECT_PERISH_SONG':
        turns = 5
        expected = ['Perish counters decrement through end-turn processing and cause source faint/replacement or terminal handling.']
    if effect == 'EFFECT_FUTURE_SIGHT':
        turns = 4
        expected.append('Delayed damage resolves at the source delay, including replacement if the target changes.')
    if effect in {'EFFECT_POISON_HIT', 'EFFECT_BURN_HIT', 'EFFECT_PARALYZE_HIT', 'EFFECT_SLEEP_HIT',
                  'EFFECT_FREEZE_HIT', 'EFFECT_CONFUSE_HIT', 'EFFECT_FLINCH_HIT', 'EFFECT_ALL_UP_HIT',
                  'EFFECT_TRI_ATTACK', 'EFFECT_SACRED_FIRE', 'EFFECT_FLARE_BLITZ', 'EFFECT_BODY_SLAM'} or effect.endswith('_DOWN_HIT') or effect.endswith('_UP_HIT'):
        expected.append('At least one retained deterministic seed must activate the secondary effect; non-triggering hits are negative controls only.')
    if effect == 'EFFECT_SPLASH':
        expected = ['Actual no-effect move path executes, PP decreases, and gameplay state remains unchanged except normal turn bookkeeping.']
    if effect == 'EFFECT_METRONOME':
        expected = ['Source random selection chooses an allowed move and executes that move, recording the chosen identity.']
    if effect == 'EFFECT_TRANSFORM':
        expected = ['Battle types/stats/moves transform through the source path; original party identity remains and switch-out restores it.']
    if effect == 'EFFECT_CURSE':
        expected.append('Normal-type fixture covers stat Curse; a separate Ghost-type actor must cover Ghost Curse.')
    if effect == 'EFFECT_SKILL_SWAP':
        target['species'] = 'SENTRET'
        expected = ['Different initial abilities exchange through the source path, including the actual ability caches.']
    prerequisites.append('Use real ordinary-trainer fixed-level/no-XP options; no post-hoc HP, status, or RNG edits during execution.')
    return actor, target, prelude, turns, tuple(prerequisites), tuple(expected), tuple(fixtures)


def move_cases(inventory, seeds=DEFAULT_SEEDS, sides=(0, 1)):
    """Build two-sided plans for every move present in the bound inventory."""
    if inventory.get('profile') != 'default-non-faithful-ordinary-trainer-fixed-level':
        raise ValueError('Corpus requires the explicit fixed-level source profile')
    for rule in inventory['rules']:
        if rule['group'] != 'move':
            continue
        recipe = _recipe(rule)
        actor, opponent, prelude, turns, prerequisites, expected, fixtures = recipe
        for side in sides:
            if type(side) is not int or side not in (0, 1):
                raise ValueError('Invalid actor side')
            for seed in seeds:
                if type(seed) is not int or not 0 <= seed < 2 ** 32:
                    raise ValueError('Invalid deterministic seed')
                yield Case(f"move-{rule['name'].lower()}-side{side}-seed{seed}",
                           inventory['inventory_sha256'], side, seed, rule['name'], rule['effect'],
                           dict(actor), dict(opponent), prelude, turns,
                           (rule['key'], 'effect:' + rule['effect']), prerequisites, expected, fixtures)


def scenario(case, *, status_values=None):
    """Materialize an existing Scenario without loading a native engine here.

    status_values must be obtained from the same source version as the runner.
    Sleep is supplied explicitly as a three-turn counter, not a separate bit.
    """
    if not case.ready:
        raise ValueError('Case still requires explicit fixture support: ' + '; '.join(case.fixture_requirements))
    from battle_scenarios import Pokemon, Scenario
    def member(record):
        data = dict(record)
        status = data.pop('status_name', None)
        if status:
            if status == 'SLP_3':
                data['status'] = 3
            elif status_values is None or status not in status_values:
                raise ValueError('Missing source-derived status mask: ' + status)
            else:
                data['status'] = status_values[status]
        return Pokemon(**data)
    active = [member(case.opponent), member(case.opponent)]
    active[case.actor_side] = member(case.actor)
    teams = [(active[side], Pokemon('HORSEA', moves=('TACKLE', 'WATER_GUN'),
                 status=(status_values['PSN'] if case.effect == 'EFFECT_HEAL_BELL' and side == case.actor_side else 0)))
             for side in (0, 1)]
    return Scenario(player=teams[0], enemy=teams[1], inventory=(None, None))


def desired_moves(case, turn):
    """Named simultaneous intent pair; legal-action resolution belongs to runner.

    A missing requested move is an error during the probe, never silently
    replaced by Tackle. After the probe the runner may choose any legal attack
    to finish, allowing Transform, Sketch, locks and depleted PP to run normally.
    """
    if turn < len(case.prelude):
        plan = case.prelude[turn]
    elif turn < len(case.prelude) + case.probe_turns:
        attacking_target = case.effect in {'EFFECT_COUNTER', 'EFFECT_SUCKER_PUNCH'} or case.move == 'AVALANCHE'
        target_move = case.opponent['moves'][0] if case.effect == 'EFFECT_COUNTER' else ('TACKLE' if attacking_target else 'SPLASH')
        plan = TurnPlan(case.move, target_move)
    else:
        plan = TurnPlan('FINISH_LEGAL_ATTACK', 'FINISH_LEGAL_ATTACK')
    result = [plan.opponent, plan.opponent]
    result[case.actor_side] = plan.actor
    return tuple(result)


def write_catalog(inventory_path, output):
    inventory = json.loads(Path(inventory_path).read_text())
    cases = [case.record() for case in move_cases(inventory)]
    groups = {}
    for rule in inventory['rules']:
        if rule.get('required') and rule['group'] not in {'move', 'effect'}:
            groups.setdefault(rule['group'], []).append(rule['key'])
    body = dict(schema=SCHEMA, inventory_sha256=inventory['inventory_sha256'],
                cases=cases, other_required_rules=groups,
                scope='Unexecuted full-fight scenario plans; no row confers coverage.')
    body['catalog_sha256'] = hashlib.sha256(json.dumps(body, sort_keys=True).encode()).hexdigest()
    Path(output).write_text(json.dumps(body, indent=2) + '\n')
    return body


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--inventory', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    write_catalog(args.inventory, args.output)
