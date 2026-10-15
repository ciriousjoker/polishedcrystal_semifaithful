"""Bounded actual Baton Pass replacement privacy and terminal replay checks."""
import argparse
import copy
import hashlib
import json
import pathlib

from battle_actions import SwitchAction
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight, encode_action
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_decisions import observation
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Pokemon, Scenario


DEPENDENCIES = ('battle_controller_pivot_privacy_check.py', 'battle_controller_fight.py',
    'battle_controller.py', 'battle_controller_profile.py', 'battle_controller_check.py',
    'battle_controller_oracle.c', 'battle_controller_fight_check.py', 'battle_decisions.py',
    'battle_scenarios.py', 'battle_bag.py', 'battle_env.py', 'battle_env_check.py',
    'stateful_fixture_check.py', 'stateful_lifting.py')


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def sources():
    return {name: sha(pathlib.Path(__file__).with_name(name)) for name in DEPENDENCIES}


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def active(obs, side):
    return next(member for member in obs['teams'][side] if member['active'])


def scenario(fast_side, double):
    fast = (Pokemon('JOLTEON', moves=('BATON_PASS', 'TACKLE'), happiness=0),
            Pokemon('PSYDUCK', moves=('TACKLE',), happiness=0))
    slow = (Pokemon('SLOWPOKE', moves=('BATON_PASS', 'BATON_PASS', 'TACKLE') if double
                    else ('SPLASH', 'TACKLE'), happiness=0),
            Pokemon('PSYDUCK', moves=('TACKLE',), happiness=0)) if double else (
                Pokemon('SLOWPOKE', moves=('SPLASH', 'TACKLE'), happiness=0),)
    return Scenario(player=fast if fast_side == 0 else slow,
                    enemy=slow if fast_side == 0 else fast, inventory=(None, None))


def selected(fight, fast_side, secret, double):
    if fight.turn == int(double):
        actions = [MoveAction(secret), MoveAction(secret)]
        actions[fast_side] = MoveAction(0)
        return tuple(actions)
    actions = []
    for side in (0, 1):
        mask = fight.legal_actions(side)
        own = active(fight.observation(side), side)
        wanted = MoveAction(own['moves'].index(fight.machine._constant(
            'constants/move_constants.asm', 'TACKLE')))
        actions.append(wanted if wanted in mask else first_attack(side, fight.observation(side), mask))
    return tuple(actions)


def run_case(machine, oracle, fast_side, secret, double, output, identities):
    name = ('double' if double else 'player' if fast_side == 0 else 'enemy') + '-' + str(secret)
    callbacks, events = [], []
    fight = None
    initial = None
    def policy(side, obs, choices):
        raw = observation(machine, side)
        before = bytes(machine.state.m[0xc000:0xe000])
        pending = copy.deepcopy(fight._pending)
        expected = copy.deepcopy(raw)
        opposite = 1 - side
        same = active(initial[side], opposite)['index'] == active(obs, opposite)['index']
        high = initial[side]['battlefield'][opposite]['EncoreCount'] & 240 if same else 0
        expected['battlefield'][opposite]['EncoreCount'] = (
            raw['battlefield'][opposite]['EncoreCount'] & 15) | high
        if obs != expected:
            raise UnsupportedFight('Replacement projection changed more than the opposite private nibble')
        if not callbacks:
            if side != fast_side or machine.turn != int(double) or choices != (SwitchAction(1),):
                raise UnsupportedFight('Expected faster actual Baton Pass replacement')
            if active(raw, opposite)['pp'] != active(initial[side], opposite)['pp']:
                raise UnsupportedFight('Opposite actor already acted before faster pivot input')
            if raw['battlefield'][opposite]['EncoreCount'] >> 4 != secret + 1:
                raise UnsupportedFight('Actual source did not expose the pending slot packing witness')
        elif not double or side == fast_side or same or not (
                initial[side]['battlefield'][opposite]['EncoreCount'] & 240):
            raise UnsupportedFight('Expected changed opposing participant at slower Baton Pass input')
        callbacks.append(dict(side=side, input_sha256=digest(obs), observation=copy.deepcopy(obs),
            choices=tuple(encode_action(action) for action in choices),
            raw_opposite_encore=raw['battlefield'][opposite]['EncoreCount'],
            changed_opponent=not same, pending=tuple(encode_action(action) for action in pending)))
        obs['teams'][side][0]['HP'] = 0
        if bytes(machine.state.m[0xc000:0xe000]) != before or fight._pending != pending:
            raise UnsupportedFight('Detached replacement input mutated live RAM or pending actions')
        return SwitchAction(1)
    def observe(stage, record):
        if stage == 'replacement-choice':
            events.append(record)
    machine.segment_checker, machine.single_step_checker = oracle.compare, oracle.single_step
    recording = RecordedRun(oracle)
    fight = ControllerFight(machine, chooser=first_attack, replacement_chooser=policy,
                            observer=observe, max_turns=128)
    fight._old_segment_checker, fight._old_single_step_checker = recording.capture_segment, recording.capture_one
    base = scenario(fast_side, double)
    initial = fight.reset(base, 193)
    if active(initial[fast_side], fast_side)['Speed'] <= active(initial[fast_side], 1-fast_side)['Speed']:
        raise UnsupportedFight('Pivot fixture is not faster')
    raw_path = output/(name+'.initial-wram.bin')
    raw_path.write_bytes(bytes(machine.state.m[0xc000:0xe000]))
    if double:
        fight.step((MoveAction(1), MoveAction(2)))
        initial = fight.observations()
    first = fight.step(selected(fight, fast_side, secret, double))
    if len(callbacks) != (2 if double else 1) or active(first.observations[fast_side], fast_side)['index'] != 1:
        raise UnsupportedFight('Actual source failed its requested pivot switch')
    if double and active(first.observations[0], 1)['index'] != 1:
        raise UnsupportedFight('Actual slower source pivot did not switch')
    # Subsequent faint replacements use the existing explicit policy, while the
    # bounded pivot callback records remain confined to the first actual round.
    fight.replacement_chooser = first_replacement
    while not fight.terminal and not fight.truncated:
        fight.step(selected(fight, fast_side, secret, double))
    if not fight.terminal or fight.truncated or machine.failed:
        raise UnsupportedFight('Pivot fight did not reach actual terminal')
    result = fight.replay_record()
    saved_callbacks, saved_events = copy.deepcopy((callbacks, events))
    result.update(case=name, callbacks=saved_callbacks, replacement_events=saved_events,
        recorded_segments=recording.rows, host_sources=identities,
        oracle_provenance=copy.deepcopy(oracle.provenance),
        initial_wram=str(raw_path), initial_wram_sha256=sha(raw_path))
    evidence = output/(name+'.json')
    evidence.write_text(json.dumps(result, indent=2)+'\n')
    callbacks.clear(); events.clear()
    fight._old_segment_checker, fight._old_single_step_checker = recording.replay_segment, recording.replay_one
    fight.replacement_chooser = policy
    initial = fight.reset(base, 193)
    if double:
        fight.step((MoveAction(1), MoveAction(2)))
        initial = fight.observations()
    fight.step(selected(fight, fast_side, secret, double))
    fight.replacement_chooser = first_replacement
    while not fight.terminal and not fight.truncated:
        fight.step(selected(fight, fast_side, secret, double))
    if (recording.position != len(recording.rows) or not fight.terminal or
            fight.replay_record()['turns'] != result['turns'] or
            fight.winner != result['winner'] or callbacks != saved_callbacks or events != saved_events):
        raise UnsupportedFight('Pivot exact recorded native replay diverged')
    row = dict(case=name, passed=True, turns=fight.turn, comparisons=len(recording.rows),
        replay_equal=True, input_sha256=callbacks[0]['input_sha256'],
        choices=callbacks[0]['choices'], changed_opponent=double,
        initial_wram_sha256=sha(raw_path), evidence=str(evidence), evidence_sha256=sha(evidence))
    print(json.dumps(row), flush=True)
    return row


def run(root, output):
    identities = sources()
    game = root/'build/stateful/game'
    machine = ControllerMachine(game/'polishedcrystal-3.2.3.gbc', game/'polishedcrystal-3.2.3.sym',
        game, root/'build/battle-controller/native-o3', optimization='O3')
    oracle = Oracle(root, machine)
    output.mkdir(parents=True, exist_ok=True)
    rows = []
    for fast_side, double in ((0, False), (1, False), (0, True)):
        pair = [run_case(machine, oracle, fast_side, secret, double, output, identities) for secret in (0, 1)]
        if any(pair[0][key] != pair[1][key] for key in ('input_sha256', 'choices', 'initial_wram_sha256')):
            raise UnsupportedFight('Faster replacement actor input or mask leaked the hidden opposite slot')
        rows.extend(pair)
    oracle.oracle.oracle_close()
    if identities != sources():
        raise UnsupportedFight('Host sources changed during pivot privacy proof')
    report = dict(passed=True, cases=rows, hidden_opposite_input_equality=True,
        host_sources=identities, oracle_provenance=oracle.provenance,
        manifest_sha256=sha(machine.build_dir/'manifest.json'), rom_sha256=sha(machine.rom_path),
        symbols_sha256=sha(machine.symbols_path), scope='actual-Baton-Pass-replacement-input',
        limits=['Bounded pending-slot pairs only; presentation/input parity remains conditional.'])
    (output/'report.json').write_text(json.dumps(report, indent=2)+'\n')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    parser.add_argument('--output', type=pathlib.Path, required=True)
    args = parser.parse_args()
    run(args.artifact_root, args.output)
