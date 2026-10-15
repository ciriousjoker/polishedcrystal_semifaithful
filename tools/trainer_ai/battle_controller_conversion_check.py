"""Actual player Conversion nested type decisions, including zero-PP targets."""
import argparse
import copy
import hashlib
import json
import pathlib

from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight, encode_action
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_decisions import observation
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Pokemon, Scenario

CASES = ('normal-to-water-splash', 'normal-to-water-secret-attack', 'water-to-normal')
DEPENDENCIES = ('battle_controller_conversion_check.py', 'battle_controller_fight.py',
    'battle_controller.py', 'battle_controller_profile.py', 'battle_controller_check.py',
    'battle_controller_oracle.c', 'battle_controller_fight_check.py', 'battle_decisions.py',
    'battle_scenarios.py', 'battle_bag.py', 'battle_env.py', 'battle_env_check.py',
    'stateful_fixture_check.py', 'stateful_lifting.py')


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def host_sources():
    return {name:sha(pathlib.Path(__file__).with_name(name)) for name in DEPENDENCIES}


def digest(value):
    return hashlib.sha256(json.dumps(value,sort_keys=True).encode()).hexdigest()


def scenario(machine, case):
    names = ('CONVERSION','WATER_GUN','TACKLE')
    pp = tuple(machine.move_data(machine._constant('constants/move_constants.asm',name))[5]
               if index != 1 else 0 for index,name in enumerate(names))
    return Scenario(player=(Pokemon('PSYDUCK' if case == 'water-to-normal' else 'PORYGON',
        moves=names,pp=pp,happiness=0,ability_slot=0 if case == 'water-to-normal' else 2),),
        enemy=(Pokemon('SLOWPOKE',moves=('SPLASH','TACKLE'),happiness=0),),
        inventory=(None,None))


def selected(fight,case):
    if not fight.turn:
        return (MoveAction(0), MoveAction(1 if case.endswith('secret-attack') else 0))
    actions=[]
    for side in (0,1):
        wanted=MoveAction(2 if side == 0 else 1)
        mask=fight.legal_actions(side)
        actions.append(wanted if wanted in mask else first_attack(side,fight.observation(side),mask))
    return tuple(actions)


def private_enemy(fight):
    m=fight.machine
    return dict(intent=encode_action(fight._pending[1]),move=m.byte('wCurEnemyMove'),
                slot=m.byte('wCurEnemyMoveNum'),switch_target=m.byte('wEnemySwitchTarget'))


def run_case(machine,oracle,case,output,sources):
    callbacks,events=[],[]
    fight=None
    desired=MoveAction(0 if case == 'water-to-normal' else 1)
    def policy(side,obs,choices):
        if side != 0 or callbacks or machine.turn != 0 or desired not in choices:
            raise UnsupportedFight('Expected one actual Conversion type input with the requested target')
        active=next(p for p in obs['teams'][0] if p['active'])
        if case != 'water-to-normal' and (active['pp'][1] != 0 or MoveAction(0) in choices):
            raise UnsupportedFight('Conversion mask reused attack PP/current-type legality')
        callbacks.append(dict(input_sha256=digest(obs),observation=copy.deepcopy(obs),
            choices=tuple(encode_action(a) for a in choices),choice=encode_action(desired),
            enemy_before=private_enemy(fight),hp_before=machine.word('wBattleMonHP')))
        obs['teams'][0][0]['HP']=0
        return desired
    def observe(stage,record):
        if stage == 'conversion-choice':
            if (private_enemy(fight) != callbacks[0]['enemy_before'] or
                    machine.word('wBattleMonHP') != callbacks[0]['hp_before']):
                raise UnsupportedFight('Nested Conversion input mutated live state or enemy intent')
            events.append(record)
    machine.segment_checker,machine.single_step_checker=oracle.compare,oracle.single_step
    recording=RecordedRun(oracle)
    fight=ControllerFight(machine,chooser=first_attack,replacement_chooser=first_replacement,
        conversion_chooser=policy,observer=observe,max_turns=128)
    fight._old_segment_checker,fight._old_single_step_checker=recording.capture_segment,recording.capture_one
    base=scenario(machine,case)
    initial=fight.reset(base,193)
    raw=output/(case+'.initial-wram.bin')
    raw.write_bytes(bytes(machine.state.m[0xc000:0xe000]))
    first=fight.step(selected(fight,case))
    expected=machine.move_data(initial[0]['teams'][0][0]['moves'][desired.slot])[3]
    own=first.observations[0]['teams'][0][0]
    if own['types'] != (expected,expected) or own['pp'][1] != 0 or len(callbacks) != 1:
        raise UnsupportedFight('Actual Conversion failed its requested type or spent target move PP')
    while not fight.terminal and not fight.truncated:
        fight.step(selected(fight,case))
    if not fight.terminal or fight.truncated or machine.failed:
        raise UnsupportedFight('Conversion source fight did not reach terminal')
    result=fight.replay_record()
    first_callbacks,first_events=copy.deepcopy((callbacks,events))
    result.update(case=case,callbacks=first_callbacks,nested_events=first_events,
        actual_type=expected,zero_pp_target=case != 'water-to-normal',
        recorded_segments=recording.rows,host_sources=sources,
        oracle_provenance=copy.deepcopy(oracle.provenance),
        initial_wram=str(raw),initial_wram_sha256=sha(raw))
    evidence=output/(case+'.json'); evidence.write_text(json.dumps(result,indent=2)+'\n')
    callbacks.clear();events.clear()
    fight._old_segment_checker,fight._old_single_step_checker=recording.replay_segment,recording.replay_one
    fight.reset(base,193)
    while not fight.terminal and not fight.truncated:
        fight.step(selected(fight,case))
    if (recording.position != len(recording.rows) or callbacks != first_callbacks or events != first_events
            or not fight.terminal or fight.replay_record()['turns'] != result['turns']):
        raise UnsupportedFight('Conversion exact native replay diverged')
    row=dict(case=case,passed=True,turns=fight.turn,comparisons=len(recording.rows),replay_equal=True,
        actual_type=expected,inner_actor_input_sha256=callbacks[0]['input_sha256'],
        evidence=str(evidence),evidence_sha256=sha(evidence),initial_wram=str(raw),initial_wram_sha256=sha(raw))
    print(json.dumps(row),flush=True)
    return row


def negative_guards(machine,oracle):
    result=[]
    for invalid in (False,True):
        machine.segment_checker,machine.single_step_checker=oracle.compare,oracle.single_step
        policy=(lambda side,obs,choices:MoveAction(0)) if invalid else None
        fight=ControllerFight(machine,chooser=first_attack,replacement_chooser=first_replacement,
                              conversion_chooser=policy)
        base=scenario(machine,'normal-to-water-splash')
        fight.reset(base,193)
        expected='outside its detached type mask' if invalid else 'explicit nested type policy'
        try:
            fight.step(selected(fight,'normal-to-water-splash'))
        except UnsupportedFight as error:
            if expected not in str(error):
                raise
        else:
            raise UnsupportedFight('Conversion silently supplied an absent or invalid type choice')
        assert machine.failed and machine.turn == 0
        live=observation(machine,0)['teams'][0][0]
        assert live['types'][0] == machine._constant('constants/type_constants.asm','NORMAL')
        try:
            fight.step((MoveAction(2),MoveAction(0)))
        except UnsupportedFight:
            pass
        else:
            raise UnsupportedFight('Failed Conversion resumed without reset')
        fight.reset(base,193)
        assert not machine.failed and not machine.turn
        result.append(dict(case='current-type-invalid' if invalid else 'missing-policy',
                           rejected=True,poisoned_until_reset=True,reset_recovers=True))
    return result


def run(root,output):
    sources=host_sources();game=root/'build/stateful/game'
    machine=ControllerMachine(game/'polishedcrystal-3.2.3.gbc',game/'polishedcrystal-3.2.3.sym',
        game,root/'build/battle-controller/native-o3',optimization='O3')
    oracle=Oracle(root,machine);output.mkdir(parents=True,exist_ok=True)
    rows=[run_case(machine,oracle,case,output,sources) for case in CASES]
    if rows[0]['inner_actor_input_sha256'] != rows[1]['inner_actor_input_sha256']:
        raise UnsupportedFight('Nested Conversion input leaked the current hidden enemy action')
    guards=negative_guards(machine,oracle)
    oracle.oracle.oracle_close()
    if sources != host_sources():
        raise UnsupportedFight('Host source changed during Conversion proof')
    report=dict(passed=True,cases=rows,guards=guards,hidden_enemy_input_equality=True,
        host_sources=sources,oracle_provenance=oracle.provenance,
        manifest_sha256=sha(machine.build_dir/'manifest.json'),rom_sha256=sha(machine.rom_path),
        symbols_sha256=sha(machine.symbols_path),scope='actual-player-Conversion-nested-type-choice',
        limits=['Player Conversion only. Enemy Conversion remains actual source AI.',
                'Presentation/input projection remains conditional.'])
    (output/'report.json').write_text(json.dumps(report,indent=2)+'\n')


if __name__ == '__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root',type=pathlib.Path,required=True)
    parser.add_argument('--output',type=pathlib.Path,required=True)
    args=parser.parse_args();run(args.artifact_root,args.output)
