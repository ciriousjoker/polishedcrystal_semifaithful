"""Bounded source corpus bridge with actual-ROM comparison and taped replay.

Run serially in the project's heavy CPU slot. No mechanic is accepted merely
because an action was submitted, PP fell, or a terminal fight was reached.
"""
from dataclasses import asdict
from copy import deepcopy
import argparse
import hashlib
import json
from pathlib import Path

from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_corpus import desired_moves, move_cases, scenario
from battle_corpus_observers import (MoveFrame, observe_move, source_bindings,
                                     supported_effect, execution_boundaries)
from battle_env import MoveAction, UnsupportedFight
from battle_decisions import StruggleAction, observation

FIRST_MOVES = ('TACKLE', 'THUNDER_WAVE', 'SWORDS_DANCE', 'GROWL', 'RECOVER',
               'REFLECT', 'LIGHT_SCREEN', 'RAIN_DANCE', 'SUNNY_DAY', 'SPLASH')


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


class ExecutionTrace:
    """Read-only diagnostics at existing presentation hooks, never actor data."""
    def __init__(self, machine, actor_side=None, probe_turns=0):
        self.used, self.end = execution_boundaries(machine)
        self.rows = []
        self.pending = None
        self.actor_side = actor_side
        self.probe_turns = probe_turns

    @staticmethod
    def snapshot(machine, side):
        # Read the live source state, not ControllerFight's frozen turn-start
        # actor inputs. Observation queries execute on isolated copies. Assert
        # that the complete live CPU/RAM/event tape and RNG remain unchanged.
        state = bytes(machine.state)
        rng = machine.rng.rng.getstate()
        result = deepcopy(observation(machine, side))
        if bytes(machine.state) != state or machine.rng.rng.getstate() != rng:
            raise UnsupportedFight('Action-local observation mutated live State or RNG')
        return result

    @staticmethod
    def matches(machine, contract):
        state = machine.state
        if not 0 <= state.sp < 65535:
            return False
        returned = state.m[state.sp] | state.m[state.sp + 1] << 8
        return state.bank == contract['bank'] and returned == contract['return_pc']

    def capture(self, machine, event):
        if event is None:
            return
        if event.name == self.used['target'] and self.matches(machine, self.used):
            if self.pending is not None:
                raise UnsupportedFight('Nested or missing-end actual move span')
            side = machine.byte('hBattleTurn')
            if side not in (0, 1):
                raise UnsupportedFight('Invalid actual move actor')
            row = dict(turn=machine.turn, side=side, move=machine.byte('wMoveGrammar'),
                       attack_missed=None, used_call=self.used, end_call=None)
            if side == self.actor_side and machine.turn < self.probe_turns:
                row['before'] = self.snapshot(machine, side)
            self.rows.append(row)
            self.pending = row
        elif event.name == self.end['target'] and self.matches(machine, self.end):
            side = machine.byte('hBattleTurn')
            row = self.pending
            self.pending = None
            if row is not None:
                if row['side'] != side or row['turn'] != machine.turn:
                    raise UnsupportedFight('Actual move span identity changed before its end')
                # wMoveGrammar aliases damage-stat scratch storage. Only the
                # used-text call owns its move identity; retain the later byte
                # for diagnostics without treating it as another move witness.
                row['end_move_grammar_scratch'] = machine.byte('wMoveGrammar')
                row['attack_missed'] = machine.byte('wAttackMissed')
                row['end_call'] = self.end
                if 'before' in row:
                    row['after'] = self.snapshot(machine, side)
                    row['observation_read_only_verified'] = True

    def segment(self, delegate):
        def checked(machine, stop, stop_bank=None):
            event, used = delegate(machine, stop, stop_bank)
            self.capture(machine, event)
            return event, used
        return checked


def choose_named(side, observation, actions, wanted, bindings):
    """Resolve only own observed moves and legal mask, with an explicit finish policy."""
    if wanted == 'STRUGGLE':
        selected = [action for action in actions if type(action) is StruggleAction]
        if len(selected) != 1:
            raise UnsupportedFight('Requested Struggle is not uniquely forced by the source legal mask')
        return selected[0]
    if wanted == 'FINISH_LEGAL_ATTACK':
        own = next(p for p in observation['teams'][side] if p['active'])
        for name in ('TACKLE', 'WATER_GUN'):
            for action in actions:
                if type(action) is MoveAction and own['moves'][action.slot] == bindings['moves'][name]:
                    return action
        return first_attack(side, observation, actions)  # Includes forced and Struggle intents.
    own = next(p for p in observation['teams'][side] if p['active'])
    selected = [a for a in actions if type(a) is MoveAction and
                own['moves'][a.slot] == bindings['moves'].get(wanted)]
    if len(selected) != 1:
        raise UnsupportedFight('Requested probe move is not uniquely legal: ' + wanted)
    return selected[0]


def actions_for(fight, case, bindings):
    wanted = desired_moves(case, fight.turn)
    # Both inputs are detached before either decision; diagnostics are absent.
    observations = fight.observations()
    masks = tuple(fight.legal_actions(side) for side in (0, 1))
    return tuple(choose_named(side, observations[side], masks[side], wanted[side], bindings)
                 for side in (0, 1))


def execute(fight, case, bindings, trace):
    initial = fight.reset(scenario(case, status_values=bindings['status_masks']), case.seed)
    initial_wram = bytes(fight.machine.state.m[0xc000:0xe000])
    frames = []
    states = [RecordedRun.digest(fight.machine)]
    while not fight.terminal and not fight.truncated:
        turn = fight.turn
        before = fight.observation(case.actor_side)
        first = len(trace.rows)
        transition = fight.step(actions_for(fight, case, bindings))
        states.append(RecordedRun.digest(fight.machine))
        actual = trace.rows[first:]
        if trace.pending is not None:
            raise UnsupportedFight('Actual move span did not finish within its source round')
        for row in actual:
            if 'after' not in row:
                continue
            frames.append(MoveFrame(row['before'], row['after'], row['side'], row['move'],
                (), row['attack_missed'], phase='source-action-span',
                move_execution_verified=True,
                source_interval=dict(turn=row['turn'], side=row['side'], move=row['move'],
                    used_call=row['used_call'], end_call=row['end_call']),
                observation_read_only_verified=row['observation_read_only_verified']))
        # A round is never represented as an isolated move. Ambiguous repeated
        # execution, absent opposing Splash, or missing actor-local miss blocks it.
        own = [r for r in actual if r['side'] == case.actor_side and r['attack_missed'] is not None]
        other = [r for r in actual if r['side'] != case.actor_side and r['attack_missed'] is not None]
        if (len(actual) == 2 and len(own) == len(other) == 1 and
                turn < len(case.prelude) + case.probe_turns):
            frames.append(MoveFrame(before, transition.observations[case.actor_side],
                case.actor_side, own[0]['move'], (), own[0]['attack_missed'],
                phase='controlled-source-turn', opponent_executed_move=other[0]['move'],
                move_execution_verified=True))
    return initial, frames, states, initial_wram


def run(args):
    inventory = json.loads(args.inventory.read_text())
    body = {key: value for key, value in inventory.items() if key != 'inventory_sha256'}
    if hashlib.sha256(json.dumps(body, sort_keys=True).encode()).hexdigest() != inventory['inventory_sha256']:
        raise UnsupportedFight('Inventory content digest does not match')
    game = args.artifact_root / 'build/stateful/game'
    machine = ControllerMachine(game / 'polishedcrystal-3.2.3.gbc',
        game / 'polishedcrystal-3.2.3.sym', game,
        args.native_build or args.artifact_root / 'build/battle-controller/native',
        optimization=args.optimization)
    for path, digest in inventory['source_inputs'].items():
        if sha(machine.source_root / path) != digest:
            raise UnsupportedFight('Inventory source differs from executed source: ' + path)
    bindings = source_bindings(machine, inventory)
    oracle = Oracle(args.artifact_root, machine)
    machine.segment_checker, machine.single_step_checker = oracle.compare, oracle.single_step
    fight = ControllerFight(machine, replacement_chooser=first_replacement, max_turns=args.max_turns)
    args.output.mkdir(parents=True, exist_ok=True)
    requested = set(args.moves)
    cases = [case for case in move_cases(inventory, args.seeds, args.sides) if case.move in requested]
    missing = requested - {case.move for case in cases}
    if missing:
        raise UnsupportedFight('Requested moves absent from source inventory: ' + ', '.join(sorted(missing)))
    rows = []
    runner_files = ('battle_corpus_run.py', 'battle_corpus.py', 'battle_corpus_observers.py',
                    'battle_controller_fight.py', 'battle_controller_fight_check.py',
                    'battle_scenarios.py', 'battle_controller_check.py',
                    'battle_decisions.py', 'battle_controller.py', 'battle_controller_profile.py',
                    'battle_env.py', 'battle_bag.py', 'battle_actions.py',
                    'stateful_fixture_check.py', 'battle_env_check.py')
    provenance = {name: sha(Path(__file__).with_name(name)) for name in runner_files}
    for case in cases:
        summary = dict(case=case.name, state='blocked', accepted_rules=[])
        evidence = dict(case=case.record(), inventory_sha256=inventory['inventory_sha256'],
                        source_inputs=inventory['source_inputs'], runner_sha256=provenance,
                        oracle_provenance=oracle.provenance)
        recording, trace = None, None
        reset_complete = False
        start = len(oracle.rows)
        try:
            if not case.ready or not supported_effect(case.effect):
                raise UnsupportedFight('Fixture or semantic observer is unsupported')
            recording = RecordedRun(oracle)
            trace = ExecutionTrace(machine, case.actor_side, len(case.prelude) + case.probe_turns)
            fight._old_segment_checker = trace.segment(recording.capture_segment)
            fight._old_single_step_checker = recording.capture_one
            start = len(oracle.rows)
            initial, frames, states, initial_wram = execute(fight, case, bindings, trace)
            initial_wram_path = args.output / (case.name + '-initial-wram.bin')
            initial_wram_path.write_bytes(initial_wram)
            evidence['initial_wram'] = dict(path=str(initial_wram_path),
                sha256=hashlib.sha256(initial_wram).hexdigest(),
                address_start=0xc000, address_end_exclusive=0xe000,
                boundary='after-actual-ControllerFight.reset-before-first-actor-action')
            reset_complete = True
            first = fight.replay_record()
            result = observe_move(case, frames, bindings)
            evidence.update(initial=initial, frames=[asdict(f) for f in frames],
                semantic=asdict(result), execution_trace=trace.rows, fight=first,
                comparisons=oracle.rows[start:], recorded_segments=recording.rows)
            summary.update(state=result.state, reason=result.reason, terminal=fight.terminal,
                           truncated=fight.truncated, turns=fight.turn, replay_equal=False)
            if not fight.terminal or fight.truncated or machine.failed:
                summary.update(state='truncated' if fight.truncated else 'blocked',
                               reason='No complete source terminal fight')
            else:
                replay_trace = ExecutionTrace(machine, case.actor_side, len(case.prelude) + case.probe_turns)
                fight._old_segment_checker = replay_trace.segment(recording.replay_segment)
                fight._old_single_step_checker = recording.replay_one
                reset_complete = False
                _, replay_frames, replay_states, replay_initial_wram = execute(fight, case, bindings, replay_trace)
                reset_complete = True
                second = fight.replay_record()
                # Raw hashes include unconsumed event transport capacity. The
                # recorded segment digests and these round digests normalize
                # only that unused tail; every executed CPU/memory value stays.
                def public_record(record):
                    result = dict(record)
                    result.pop('initial_state_sha256', None)
                    result['turns'] = [{k: v for k, v in row.items() if k != 'state_sha256'}
                                       for row in record['turns']]
                    return result
                if (initial_wram != replay_initial_wram or
                        public_record(second) != public_record(first) or states != replay_states or
                        replay_trace.rows != trace.rows or replay_frames != frames or
                        recording.position != len(recording.rows)):
                    raise UnsupportedFight('Exact native whole-fight replay diverged')
                evidence['native_replay_equal'] = True
                summary['replay_equal'] = True
                if result.state == 'activated':
                    summary.update(state='accepted', accepted_rules=list(result.candidate_rules))
        except (UnsupportedFight, AssertionError, ValueError, KeyError) as error:
            summary.update(state='blocked', reason=type(error).__name__ + ': ' + str(error), accepted_rules=[])
            if reset_complete:
                try:
                    evidence['partial_fight'] = fight.replay_record()
                except (AttributeError, OSError, ValueError) as diagnostic_error:
                    evidence['partial_fight_error'] = type(diagnostic_error).__name__ + ': ' + str(diagnostic_error)
            if recording is not None:
                state = machine.state
                evidence['partial_machine'] = dict(
                    reset_completion_verified=reset_complete, pc=state.pc, bank=state.bank,
                    sp=state.sp, fault=state.fault, turn=machine.turn,
                    failed=machine.failed, state_sha256=hashlib.sha256(bytes(state)).hexdigest())
            if recording is not None:
                evidence['partial_recorded_segments'] = recording.rows
            if trace is not None:
                evidence['partial_execution_trace'] = trace.rows
            evidence['partial_comparisons'] = oracle.rows[start:]
        evidence['result'] = summary
        destination = args.output / (case.name + '.json')
        destination.write_text(json.dumps(evidence, indent=2) + '\n')
        summary['evidence'] = str(destination)
        rows.append(summary)
        print(json.dumps(summary), flush=True)
    report = dict(scope='bounded-controlled-source-turn-whole-fight-corpus',
        inventory_sha256=inventory['inventory_sha256'], cases=rows,
        native_build=str(machine.build_dir), optimization=args.optimization,
        all_requested_accepted=bool(rows) and all(row['state'] == 'accepted' for row in rows),
        limitations=['Only positive semantic activation with terminal differential and exact replay is accepted.',
                     'Existing presentation hooks condition the comparison; ordinary frontend projection remains unproven.',
                     'Controlled whole-round observations are not isolated move spans.',
                     'This report does not update the coverage ledger or claim complete coverage.'])
    (args.output / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    return 0 if report['all_requested_accepted'] else 1


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=Path, required=True)
    parser.add_argument('--inventory', type=Path, required=True)
    parser.add_argument('--native-build', type=Path,
                        help='Existing native build directory; defaults to artifact-root/build/battle-controller/native')
    parser.add_argument('--optimization', choices=('O0', 'O1', 'O2', 'O3'), default='O0')
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--moves', nargs='+', default=list(FIRST_MOVES))
    parser.add_argument('--seeds', nargs='+', type=int, default=[193])
    parser.add_argument('--sides', nargs='+', type=int, choices=(0, 1), default=[0, 1])
    parser.add_argument('--max-turns', type=int, default=256)
    raise SystemExit(run(parser.parse_args()))
