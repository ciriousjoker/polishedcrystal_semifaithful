"""Actual failed bag-attempt retarget/cancel checks without manual effects."""
import argparse
import copy
from dataclasses import asdict
import hashlib
import json
import pathlib

from battle_bag import BagAction, BagStack, CancelBagAction
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight, encode_action
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_decisions import observation as live_observation
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Pokemon, Scenario


CASES = ('retarget-hp', 'cancel-splash', 'cancel-secret-attack', 'retarget-pp')
DEPENDENCIES = ('battle_controller_bag_retry_check.py', 'battle_bag.py',
    'battle_controller_fight.py', 'battle_controller.py', 'battle_controller_profile.py',
    'battle_controller_check.py', 'battle_controller_oracle.c',
    'battle_controller_fight_check.py', 'battle_decisions.py', 'battle_scenarios.py',
    'battle_env.py', 'battle_env_check.py', 'stateful_fixture_check.py', 'stateful_lifting.py')


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def host_sources():
    return {name: sha(pathlib.Path(__file__).with_name(name)) for name in DEPENDENCIES}


def scenario(machine, case):
    pp = None
    if case == 'retarget-pp':
        move = machine._constant('constants/move_constants.asm', 'TACKLE')
        pp = (machine.move_data(move)[5], 0)
    return Scenario(player=(Pokemon('PSYDUCK', pp=pp, happiness=0),
                            Pokemon('PSYDUCK', hp=10 if case == 'retarget-hp' else None, happiness=0)),
        enemy=(Pokemon('PSYDUCK', moves=('SPLASH', 'TACKLE'), happiness=0),),
        inventory=(None,None),bag=(BagStack('ETHER' if pp is not None else 'POTION'),))


def quantity(observation, identifier):
    return sum(number for rows in observation['player_bag'].values()
               for item, number in rows if item == identifier)


def private_enemy(fight):
    m = fight.machine
    return dict(intent=encode_action(fight._pending[1]),
                move=m.byte('wCurEnemyMove'),slot=m.byte('wCurEnemyMoveNum'),
                switch_target=m.byte('wEnemySwitchTarget'))


def selected(fight, case):
    if not fight.machine.turn:
        return (BagAction('ETHER' if case == 'retarget-pp' else 'POTION',pokemon=0,move=0),
                MoveAction(1 if case == 'cancel-secret-attack' else 0))
    actions = []
    for side in (0,1):
        mask = fight.legal_actions(side)
        wanted = MoveAction(0 if side == 0 else 1)
        actions.append(wanted if wanted in mask else first_attack(side,fight.observation(side),mask))
    return tuple(actions)


def run_case(machine, oracle, case, output, sources):
    callbacks, outer, events = [], [], []
    item = 'ETHER' if case == 'retarget-pp' else 'POTION'
    identifier = machine._constant('constants/item_constants.asm',item)
    initial = None
    fight = None
    def bag_policy(side, observation, choices):
        if side != 0 or len(callbacks) != 0 or observation['turn'] != 0 or machine.turn != 0:
            raise UnsupportedFight('Unexpected repeated or advanced-turn inner bag request')
        if quantity(observation,identifier) != 1:
            raise UnsupportedFight('Rejected attempt consumed an item')
        for actor in (0,1):
            for old,new in zip(initial[actor]['teams'][actor],observation['teams'][actor]):
                if any(old[field] != new[field] for field in ('HP','status','pp')):
                    raise UnsupportedFight('Rejected attempt advanced combat effects or PP')
        chosen = (BagAction(item,pokemon=1,move=0) if case == 'retarget-hp' else
                  BagAction(item,pokemon=0,move=1) if case == 'retarget-pp' else CancelBagAction())
        if chosen not in choices:
            raise UnsupportedFight('Intended inner choice is absent from detached source mask')
        callbacks.append(dict(observation=copy.deepcopy(observation),
            input_sha256=digest(observation),enemy_before=private_enemy(fight),
            choice=asdict(chosen),choice_type=type(chosen).__name__,no_turn=True,no_consumption=True))
        # This actor-owned copy must not alias live battle data.
        observation['teams'][0][0]['HP'] = 0
        return chosen
    def outer_policy(side, observation, choices):
        if not case.startswith('cancel-') or side != 0 or outer or machine.turn != 0:
            raise UnsupportedFight('Unexpected outer retry instead of source inner retry')
        if quantity(observation,identifier) != 1:
            raise UnsupportedFight('Cancellation consumed the ineffective item')
        if private_enemy(fight) != callbacks[0]['enemy_before']:
            raise UnsupportedFight('Cancellation replaced or changed the hidden enemy intent')
        for actor in (0,1):
            for old,new in zip(initial[actor]['teams'][actor],observation['teams'][actor]):
                if any(old[field] != new[field] for field in ('HP','status','pp')):
                    raise UnsupportedFight('Cancellation advanced the source turn')
        action = MoveAction(0)
        if action not in choices:
            raise UnsupportedFight('Source did not offer its outer attack after cancellation')
        outer.append(dict(turn=machine.turn,no_consumption=True,no_turn=True,
                          retained_enemy=private_enemy(fight),action=encode_action(action)))
        return action
    def observe(stage,record):
        if stage == 'bag-retry':
            if not callbacks or private_enemy(fight) != callbacks[0]['enemy_before']:
                raise UnsupportedFight('Inner retarget/cancel changed the hidden enemy intent')
            if machine.word('wBattleMonHP') != initial[0]['teams'][0][0]['HP']:
                raise UnsupportedFight('Actor input observation was not detached')
            events.append(record)
    machine.segment_checker,machine.single_step_checker = oracle.compare,oracle.single_step
    recording = RecordedRun(oracle)
    fight = ControllerFight(machine,chooser=first_attack,replacement_chooser=first_replacement,
        bag_retry_chooser=bag_policy,retry_chooser=outer_policy,observer=observe,max_turns=128)
    fight._old_segment_checker,fight._old_single_step_checker = recording.capture_segment,recording.capture_one
    base = scenario(machine,case)
    initial = fight.reset(base,193)
    raw = output/(case+'.initial-wram.bin')
    raw.write_bytes(bytes(machine.state.m[0xc000:0xe000]))
    first = fight.step(selected(fight,case))
    if len(callbacks) != 1 or len(events) != 1 or machine.turn != 1:
        raise UnsupportedFight('No-effect attempt did not request exactly one explicit nested decision')
    own = first.observations[0]['teams'][0]
    if case == 'retarget-hp':
        if own[1]['HP'] != 30 or own[0]['HP'] != initial[0]['teams'][0][0]['HP'] or quantity(first.observations[0],identifier):
            raise UnsupportedFight('Retargeted Potion did not heal/consume its actual selected reserve')
    elif case == 'retarget-pp':
        if (own[0]['pp'][0] != initial[0]['teams'][0][0]['pp'][0] or
                own[0]['pp'][1] != 10 or quantity(first.observations[0],identifier)):
            raise UnsupportedFight('Retargeted Ether did not restore only its intended second move')
    else:
        if len(outer) != 1 or quantity(first.observations[0],identifier) != 1:
            raise UnsupportedFight('Explicit cancellation did not preserve its item and request the outer action')
        if own[0]['pp'][0] != initial[0]['teams'][0][0]['pp'][0]-1:
            raise UnsupportedFight('Outer retry did not execute exactly one actual player attack')
    first_callbacks,first_outer,first_events = copy.deepcopy((callbacks,outer,events))
    while not fight.terminal and not fight.truncated:
        fight.step(selected(fight,case))
    if not fight.terminal or fight.truncated or machine.failed:
        raise UnsupportedFight('Bag retry case failed to reach source terminal')
    result = fight.replay_record()
    result.update(case=case,callbacks=first_callbacks,outer_retries=first_outer,
        retry_events=first_events,recorded_segments=recording.rows,host_sources=sources,
        oracle_provenance=copy.deepcopy(oracle.provenance),initial_wram=str(raw),initial_wram_sha256=sha(raw))
    evidence = output/(case+'.json')
    evidence.write_text(json.dumps(result,indent=2)+'\n')
    callbacks.clear()
    outer.clear()
    events.clear()
    fight._old_segment_checker,fight._old_single_step_checker = recording.replay_segment,recording.replay_one
    initial = fight.reset(base,193)
    while not fight.terminal and not fight.truncated:
        fight.step(selected(fight,case))
    replay = fight.replay_record()
    if (recording.position != len(recording.rows) or not fight.terminal or
            replay['turns'] != result['turns'] or replay['winner'] != result['winner'] or
            callbacks != first_callbacks or outer != first_outer or events != first_events):
        raise UnsupportedFight('Nested bag retry native whole-fight replay diverged')
    row = dict(case=case,passed=True,turns=machine.turn,comparisons=len(recording.rows),
        winner=fight.winner,replay_equal=True,inner_actor_input_sha256=callbacks[0]['input_sha256'],
        no_turn_at_failure=True,no_consumption_at_failure=True,enemy_intent_retained=True,
        evidence=str(evidence),evidence_sha256=sha(evidence),
        initial_wram=str(raw),initial_wram_sha256=sha(raw))
    print(json.dumps(row),flush=True)
    return row


def negative_guards(machine,oracle):
    guards = []
    for invalid in (False,True):
        machine.segment_checker,machine.single_step_checker = oracle.compare,oracle.single_step
        bad = (lambda side,obs,choices: MoveAction(0)) if invalid else None
        fight = ControllerFight(machine,chooser=first_attack,replacement_chooser=first_replacement,
                                bag_retry_chooser=bad)
        initial = fight.reset(scenario(machine,'cancel-splash'),193)
        state = bytes(machine.state)
        try:
            fight.step((CancelBagAction(),MoveAction(0)))
        except UnsupportedFight:
            assert bytes(machine.state) == state
        else:
            raise AssertionError('Nested cancel accepted as an ordinary turn action')
        expected = ('outside its detached mask' if invalid else 'explicit inner retry policy')
        try:
            fight.step(selected(fight,'cancel-splash'))
        except UnsupportedFight as error:
            if expected not in str(error):
                raise
        else:
            raise AssertionError('Rejected target invented an unspecified retry')
        assert machine.failed and machine.turn == 0
        item = machine._constant('constants/item_constants.asm','POTION')
        actual = live_observation(machine,0)
        assert quantity(actual,item) == 1
        assert actual['teams'][0][0]['HP'] == initial[0]['teams'][0][0]['HP']
        try:
            fight.step((MoveAction(0),MoveAction(0)))
        except UnsupportedFight:
            pass
        else:
            raise AssertionError('Failed controller resumed without reset')
        recovered = fight.reset(scenario(machine,'cancel-splash'),193)
        assert not machine.failed and machine.turn == 0
        assert quantity(recovered[0],item) == 1
        guards.append(dict(case='invalid-inner-choice' if invalid else 'missing-inner-policy',
                           rejected=True,poisoned_until_reset=True,reset_recovers=True,
                           no_turn=True,no_consumption=True))
    return guards


def run(root,output):
    game = root/'build/stateful/game'
    sources = host_sources()
    machine = ControllerMachine(game/'polishedcrystal-3.2.3.gbc',game/'polishedcrystal-3.2.3.sym',
                                game,root/'build/battle-controller/native-o3',optimization='O3')
    oracle = Oracle(root,machine)
    output.mkdir(parents=True,exist_ok=True)
    rows = [run_case(machine,oracle,case,output,sources) for case in CASES]
    hidden = {row['case']:row['inner_actor_input_sha256'] for row in rows if row['case'].startswith('cancel-')}
    if hidden['cancel-splash'] != hidden['cancel-secret-attack']:
        raise UnsupportedFight('Inner actor input leaked the opposing current decision')
    guards = negative_guards(machine,oracle)
    oracle.oracle.oracle_close()
    if sources != host_sources():
        raise UnsupportedFight('Host sources changed during focused bag retry checks')
    report = dict(passed=True,cases=rows,guards=guards,hidden_enemy_input_equality=True,
        host_sources=sources,oracle_provenance=oracle.provenance,
        manifest_sha256=sha(machine.build_dir/'manifest.json'),rom_sha256=sha(machine.rom_path),
        symbols_sha256=sha(machine.symbols_path),
        scope='actual-source-no-effect-bag-retarget-and-explicit-cancellation',
        limits=['Only the declared ineffective-item text scaffold is recognized.',
                'Presentation/input projection remains conditional.'])
    (output/'report.json').write_text(json.dumps(report,indent=2)+'\n')


if __name__ == '__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root',type=pathlib.Path,required=True)
    parser.add_argument('--output',type=pathlib.Path,required=True)
    args=parser.parse_args()
    run(args.artifact_root,args.output)
