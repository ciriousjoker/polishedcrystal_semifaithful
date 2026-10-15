"""Serial actual-ROM checks for typed source-controller whole battles.

Run only while holding the project's sole heavy CPU slot. This runner loads an
existing native build and independent mGBA oracle. It does not build a ROM or
claim coverage for mechanics that the selected cases do not activate.
"""
import argparse
import ctypes as C
from dataclasses import replace
import hashlib
import json
import pathlib

from battle_actions import SwitchAction, ItemAction
from battle_bag import BagStack, BagAction
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight
from battle_decisions import ContinueAction, StruggleAction
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Scenario, Pokemon
from battle_decisions import trainer_item_actions
from stateful_fixture_check import State


class RecordedRun:
    """Record the reference-derived event contract, then replay natively.

    A seed does not determine fresh hardware DIV samples. Replay therefore
    supplies the actual retained per-segment events instead of asking mGBA for
    another trajectory. All callbacks and combat still execute source code.
    """
    def __init__(self, oracle):
        self.oracle, self.rows, self.position = oracle, [], 0

    @staticmethod
    def digest(machine):
        # Unconsumed tape capacity is transport state, not executed machine
        # behavior. The single-step native interface previews 4096 bytes while
        # replay can supply just the recorded draws. Compare every consumed
        # event plus CPU, memory, step count and fault, ignoring only that tail.
        state = State.from_buffer_copy(bytes(machine.state))
        used = bytes(state.events[:state.event_pos])
        state.events[:] = bytes(len(state.events))
        state.events[:len(used)] = used
        state.event_count = state.event_pos
        return hashlib.sha256(bytes(state)).hexdigest()

    def capture_segment(self, machine, stop, stop_bank=None):
        event, used = self.oracle.compare(machine, stop, stop_bank)
        self.rows.append(dict(kind='segment', stop=stop, bank=stop_bank,
                              tape=tuple(used), boundary=event.name if event else None,
                              state=self.digest(machine)))
        return event, used

    def capture_one(self, machine):
        self.oracle.single_step(machine)
        self.rows.append(dict(kind='one', tape=tuple(machine.state.events[:machine.state.event_pos]),
                              state=self.digest(machine)))

    def _next(self, kind):
        if self.position >= len(self.rows) or self.rows[self.position]['kind'] != kind:
            raise UnsupportedFight('Recorded controller segment sequence diverged')
        row = self.rows[self.position]
        self.position += 1
        return row

    def replay_segment(self, machine, stop, stop_bank=None):
        row = self._next('segment')
        if (stop, stop_bank) != (row['stop'], row['bank']):
            raise UnsupportedFight('Recorded stop contract diverged')
        event, used = machine.raw_segment(row['tape'], stop, stop_bank=stop_bank)
        machine.rng.consume(len(used))
        if (tuple(used) != row['tape'] or (event.name if event else None) != row['boundary'] or
                self.digest(machine) != row['state']):
            raise UnsupportedFight('Recorded source segment state diverged')
        return event, used

    def replay_one(self, machine):
        row = self._next('one')
        machine._execute_instruction(row['tape'])
        if machine.state.event_pos != len(row['tape']):
            raise UnsupportedFight('Recorded single instruction diverged')
        if self.digest(machine) != row['state']:
            raise UnsupportedFight('Recorded single instruction state diverged')


def first_attack(side, observation, actions):
    """An explicit deterministic baseline policy, not an engine fallback."""
    for cls in (MoveAction, ContinueAction, StruggleAction):
        selected = [action for action in actions if type(action) is cls]
        if selected:
            return selected[0]
    raise UnsupportedFight('Baseline policy has no selectable attack intent')


def first_replacement(side, observation, actions):
    if not actions or any(type(action) is not SwitchAction for action in actions):
        raise UnsupportedFight('Invalid replacement policy mask')
    return actions[0]


def check_prepared_gene_mask(fight, case):
    if case != 'berserk-mask':
        return
    own = fight.observation(1)['teams'][1][0]
    state = fight.observation(1)['battlefield'][1]
    if own['item'] or state['stages'][0] <= 7 or not state['volatile']['confused']:
        raise UnsupportedFight('First public decision preceded actual Berserk Gene preparation')
    actual = trainer_item_actions(fight.machine)
    mask = tuple(action for action in fight.legal_actions(1) if type(action) is ItemAction)
    if mask != actual or ItemAction(0) not in mask:
        raise UnsupportedFight('First X Attack mask does not match the prepared source state')


def selected_actions(fight, case, turn):
    actions = list(fight.choose())
    if case.startswith('encore-'):
        desired = MoveAction(min(turn, 3))
        if desired in fight.legal_actions(0):
            actions[0] = desired
        if turn >= 3 and StruggleAction() in fight.legal_actions(1):
            if case == 'encore-item' and ItemAction(0) in fight.legal_actions(1):
                actions[1] = ItemAction(0)
            elif case == 'encore-switch' and SwitchAction(1) in fight.legal_actions(1):
                actions[1] = SwitchAction(1)
    if case == 'attract' and turn:
        selected = MoveAction(1)
        if selected in fight.legal_actions(0):
            actions[0] = selected
    if turn == 0:
        if case in ('switch', 'six-party'):
            for side in (0, 1):
                action = SwitchAction(5 if case == 'six-party' else 1)
                if action not in fight.legal_actions(side):
                    raise UnsupportedFight('Fixture did not expose its intended voluntary switch')
                actions[side] = action
        elif case == 'trainer-item':
            actions[1] = ItemAction(0)
        elif case in ('player-bag', 'ether'):
            action = BagAction('ETHER', pokemon=0, move=1) if case == 'ether' else BagAction('POTION', pokemon=0, move=0)
            if action not in fight.legal_actions(0):
                raise UnsupportedFight('Fixture did not expose its intended bag target')
            actions[0] = action
    return tuple(actions)


def case_scenario(case):
    base = Scenario(inventory=(None, None))
    if case == 'trainer-item':
        return replace(base, enemy=(replace(base.enemy[0], hp=10), *base.enemy[1:]),
                       inventory=('SUPER_POTION', None))
    if case == 'player-bag':
        return replace(base, player=(replace(base.player[0], hp=10), *base.player[1:]),
                       bag=(BagStack('POTION'),))
    if case == 'ether':
        return replace(base, player=(replace(base.player[0], pp=(1, 0)), *base.player[1:]),
                       bag=(BagStack('ETHER'),))
    if case.startswith('encore-'):
        own = Pokemon('SMEARGLE', moves=('SPLASH', 'ENCORE', 'DISABLE', 'TACKLE'))
        enemy = (Pokemon('SENTRET', moves=('TACKLE', 'WATER_GUN'), ability_slot=1),)
        if case == 'encore-switch':
            enemy += (Pokemon('PIDGEY', moves=('TACKLE',)),)
        return replace(base, player=(own,), enemy=enemy,
                       inventory=('POTION', None) if case == 'encore-item' else (None, None))
    if case == 'berserk-mask':
        return replace(base, enemy=(replace(base.enemy[0], item='BERSERK_GENE'), *base.enemy[1:]),
                       inventory=('X_ATTACK', None))
    if case == 'forced-charge':
        return replace(base, player=(Pokemon('SMEARGLE', moves=('FLY', 'TACKLE')),),
                       enemy=(Pokemon('SNORLAX', moves=('TACKLE',)),))
    if case == 'struggle':
        return replace(base, player=(Pokemon('SMEARGLE', moves=('TACKLE',), pp=(0,)),),
                       enemy=(Pokemon('SMEARGLE', moves=('TACKLE',), pp=(0,)),))
    if case == 'attract':
        return replace(base, player=(Pokemon('PIDGEY', moves=('ATTRACT', 'TACKLE'), female=True),),
                       enemy=(Pokemon('SENTRET', moves=('TACKLE',), ability_slot=1),))
    if case == 'six-party':
        names = ('PSYDUCK', 'PIDGEY', 'SENTRET', 'SPEAROW', 'SMEARGLE', 'HOOTHOOT')
        return replace(base, player=tuple(Pokemon(name, moves=('TACKLE',), hp=10) for name in names),
                       enemy=tuple(Pokemon(name, moves=('TACKLE',), hp=10) for name in reversed(names)))
    return base


def activated_case(case, initial, transitions, final):
    """Reject vacuous fixtures; these observations do not replace local proofs."""
    if not transitions:
        raise UnsupportedFight('Fixture did not execute a source turn')
    observations = [initial, *(transition.observations for transition in transitions)]
    actions = [action for transition in transitions for action in transition.actions]
    first = transitions[0]
    if case in ('switch', 'six-party'):
        for side in (0, 1):
            selected = first.actions[side]
            before = [member['index'] for member in initial[side]['teams'][side] if member['active']]
            after = [member['index'] for member in first.observations[side]['teams'][side] if member['active']]
            if (type(selected) is not SwitchAction or before == [selected.index] or after != [selected.index]):
                raise UnsupportedFight('Both source switches must actually enter their requested party slots')
    if case == 'switch':
        return dict(scope='actual-active-party-indices-after-both-voluntary-switches',
                    sides=(0, 1), requested=tuple(action.index for action in first.actions))
    if case == 'trainer-item':
        selected = first.actions[1]
        before = initial[1]['trainer_items']
        after = first.observations[1]['trainer_items']
        if (type(selected) is not ItemAction or selected.slot != 0 or len(before) != 2 or len(after) != 2 or
                not before[0] or after[0] or before[1] != after[1]):
            raise UnsupportedFight('Trainer item fixture did not consume its selected actual source slot')
        if 'AnimateHPBar' not in first.boundaries:
            raise UnsupportedFight('Trainer healing fixture did not reach its actual HP animation caller')
        return dict(scope='actual-source-slot-consumption-and-healing-caller', slot=0,
                    effect_boundary='AnimateHPBar',
                    limitation='No pre-incoming-attack HP snapshot: healing quantity needs separate routine evidence.')
    if case in ('player-bag', 'ether'):
        selected = first.actions[0]
        intended = 'ETHER' if case == 'ether' else 'POTION'
        if type(selected) is not BagAction or selected.item != intended or selected.pokemon != 0:
            raise UnsupportedFight('Player bag fixture did not submit its intended typed Potion target')
        identifier = final.machine._constant('constants/item_constants.asm', selected.item)
        def quantity(observation):
            return sum(number for records in observation['player_bag'].values()
                       for item, number in records if item == identifier)
        before, after = quantity(initial[0]), quantity(first.observations[0])
        if before != 1 or after != 0:
            raise UnsupportedFight('Player bag fixture did not consume its actual source Potion stack')
        if case == 'ether':
            old_pp = initial[0]['teams'][0][0]['pp']
            new_pp = first.observations[0]['teams'][0][0]['pp']
            if selected.move != 1 or old_pp[1] != 0 or new_pp[1] != 10 or old_pp[0] != new_pp[0]:
                raise UnsupportedFight('Ether did not restore exactly its selected move through source')
            return dict(scope='actual-source-Ether-target-PP-and-consumption', move=1,
                        pp_before=old_pp, pp_after=new_pp, quantity_before=before, quantity_after=after)
        return dict(scope='actual-source-player-bag-consumption', item=selected.item,
                    target=selected.pokemon, quantity_before=before, quantity_after=after,
                    limitation='No pre-incoming-attack HP snapshot: healing quantity needs separate routine evidence.')
    if case.startswith('encore-'):
        intended = {'encore-disable': StruggleAction, 'encore-item': ItemAction,
                    'encore-switch': SwitchAction}[case]
        for index, transition in enumerate(transitions):
            preceding = initial if not index else transitions[index-1].observations
            enemy = preceding[1]['battlefield'][1]
            if (enemy['EncoreCount'] and enemy['DisableCount'] and
                    type(transition.actions[1]) is intended and '_AIChooseMove' in transition.boundaries):
                return dict(scope='source-provisional-selection-after-frozen-Encore-Disable-mask',
                            turn=transition.turn, intent=intended.__name__,
                            encore=enemy['EncoreCount'], disable=enemy['DisableCount'])
        raise UnsupportedFight('Combined Encore/Disable provisional callback was not activated')
    if case == 'forced-charge':
        if not any(type(action) is ContinueAction for action in actions):
            raise UnsupportedFight('Charge fixture never required a continuation action')
    elif case == 'struggle':
        if len(actions) < 2 or any(type(action) is not StruggleAction for action in actions):
            raise UnsupportedFight('Exhausted PP fixture did not execute explicit Struggle intents')
    elif case == 'attract':
        if not any(pair[0]['battlefield'][1]['volatile']['in_love'] for pair in observations):
            raise UnsupportedFight('Opposite-gender fixture never exposed infatuation')
    elif case == 'six-party':
        if not any(type(action) is SwitchAction and action.index == 5 for action in actions):
            raise UnsupportedFight('Six-member fixture never selected its sixth party slot')
        if final.winner not in (0, 1):
            raise UnsupportedFight('Six-member fixture has no actual source winner')
        losing = final.observation(final.winner ^ 1)['teams'][final.winner ^ 1]
        if len(losing) != 6 or any(member['HP'] for member in losing):
            raise UnsupportedFight('Six-member fixture did not exhaust all six losing reserves')


def run(root, cases, seeds, output, *, native_build=None, optimization='O0'):
    game = root / 'build/stateful/game'
    machine = ControllerMachine(game / 'polishedcrystal-3.2.3.gbc',
        game / 'polishedcrystal-3.2.3.sym', game,
        pathlib.Path(native_build) if native_build is not None else root / 'build/battle-controller/native',
        optimization=optimization)
    oracle = Oracle(root, machine)
    machine.segment_checker = oracle.compare
    machine.single_step_checker = oracle.single_step
    fight = ControllerFight(machine, chooser=first_attack,
        replacement_chooser=first_replacement, max_turns=128)
    rows = []
    output.mkdir(parents=True, exist_ok=True)
    for case in cases:
        base = case_scenario(case)
        for seed in seeds:
            start = len(oracle.rows)
            recording = RecordedRun(oracle)
            fight._old_segment_checker = recording.capture_segment
            fight._old_single_step_checker = recording.capture_one
            initial = fight.reset(base, seed)
            check_prepared_gene_mask(fight, case)
            transitions = []
            while not fight.terminal and not fight.truncated:
                transitions.append(fight.step(selected_actions(fight, case, machine.turn)))
            if not fight.terminal or fight.truncated or machine.failed:
                raise UnsupportedFight('Whole-fight case did not reach a source terminal')
            activation = activated_case(case, initial, transitions, fight)
            first = fight.replay_record()
            first['comparisons'] = oracle.rows[start:]
            first['case'] = case
            first['recorded_segments'] = recording.rows
            first['activation'] = activation
            # Retain original evidence before replay. Reproducibility includes
            # all source callbacks, consumed events and final machine state.
            destination = output / (case + '-' + str(seed) + '.json')
            destination.write_text(json.dumps(first, indent=2) + '\n')
            fight._old_segment_checker = recording.replay_segment
            fight._old_single_step_checker = recording.replay_one
            fight.reset(base, seed)
            check_prepared_gene_mask(fight, case)
            while not fight.terminal and not fight.truncated:
                fight.step(selected_actions(fight, case, machine.turn))
            second = fight.replay_record()
            if second['turns'] != first['turns'] or second['winner'] != first['winner']:
                raise UnsupportedFight('Identical whole-fight replay diverged')
            if recording.position != len(recording.rows):
                raise UnsupportedFight('Recorded whole-fight replay did not consume all segments')
            row = dict(case=case, seed=seed, turns=machine.turn, winner=fight.winner,
                       comparisons=len(first['comparisons']), replay_equal=True,
                       evidence=str(destination))
            rows.append(row)
            print(json.dumps(row), flush=True)
    (output / 'report.json').write_text(json.dumps(dict(
        scope='typed-source-controller-conditional-presentation', passed=True, cases=rows,
        limitations=['Same-hook differential checks do not prove ordinary frontend projection.',
                     'Item activation checks source consumption; healing quantity needs separate routine evidence.',
                     'Only activated case mechanics can become coverage evidence.']), indent=2) + '\n')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    parser.add_argument('--output', type=pathlib.Path, required=True)
    parser.add_argument('--cases', nargs='+', choices=('neutral', 'switch', 'trainer-item', 'player-bag',
                        'berserk-mask', 'forced-charge', 'struggle', 'attract', 'six-party',
                        'ether', 'encore-disable', 'encore-item', 'encore-switch'),
                        default=['neutral'])
    parser.add_argument('--seeds', nargs='+', type=int, default=[193])
    parser.add_argument('--native-build', type=pathlib.Path)
    parser.add_argument('--optimization', choices=('O0', 'O1', 'O2', 'O3'), default='O0')
    args = parser.parse_args()
    run(args.artifact_root, args.cases, args.seeds, args.output,
        native_build=args.native_build, optimization=args.optimization)
