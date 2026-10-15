"""Actual-source hazard placement, entry, immunity and entry-KO full fights.

No execution occurs on import. Run only with the project's single CPU slot.
The scripted Splash interval isolates entry effects from incoming move damage.
"""
import argparse
import hashlib
import json
import pathlib
from dataclasses import dataclass

from battle_actions import SwitchAction
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Pokemon, Scenario
import stateful_lifting


@dataclass(frozen=True)
class HazardCase:
    move: str
    layers: int
    entrant: str = 'RATTATA'
    control: str = 'grounded'
    hp: int | None = None


CASES = {
    **{'spikes-' + str(n): HazardCase('SPIKES', n) for n in (1, 2, 3)},
    'spikes-flying': HazardCase('SPIKES', 1, 'PIDGEY', 'flying'),
    'spikes-entry-ko': HazardCase('SPIKES', 1, hp=1, control='entry-ko'),
    **{'toxic-' + str(n): HazardCase('TOXIC_SPIKES', n) for n in (1, 2)},
    'toxic-flying': HazardCase('TOXIC_SPIKES', 2, 'PIDGEY', 'flying'),
    'toxic-poison': HazardCase('TOXIC_SPIKES', 2, 'GRIMER', 'poison'),
}
DEPENDENCIES = (
    'battle_controller_hazards.py', 'battle_controller.py', 'battle_controller_profile.py',
    'battle_controller_fight.py', 'battle_controller_fight_check.py',
    'battle_controller_check.py', 'battle_controller_oracle.c', 'battle_decisions.py',
    'battle_scenarios.py', 'battle_bag.py', 'battle_env.py', 'stateful_lifting.py',
    'stateful_fixture_check.py',
)


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def host_sources():
    return {name: sha(pathlib.Path(__file__).with_name(name)) for name in DEPENDENCIES}


def scenario(case, setter):
    placer = (Pokemon('SMEARGLE', moves=(case.move, 'SPLASH', 'TACKLE'), happiness=0),)
    targets = (Pokemon('SMEARGLE', moves=('SPLASH', 'TACKLE'), happiness=0),
               Pokemon(case.entrant, moves=('SPLASH', 'TACKLE'), happiness=0, hp=case.hp))
    if case.control == 'entry-ko':
        targets += (Pokemon('SENTRET', moves=('SPLASH', 'TACKLE'), happiness=0),)
    return Scenario(player=placer if setter == 0 else targets,
                    enemy=targets if setter == 0 else placer,
                    inventory=(None, None))


def choose(fight, case, setter):
    turn, target = fight.machine.turn, setter ^ 1
    if turn < case.layers:
        return tuple(MoveAction(0) for _ in (0, 1))
    if turn == case.layers:
        result = [None, None]
        result[setter], result[target] = MoveAction(1), SwitchAction(1)
        return tuple(result)
    result = []
    for side in (0, 1):
        moves = fight.legal_actions(side)
        wanted = MoveAction(2 if side == setter else 1)
        result.append(wanted if wanted in moves else first_attack(side, fight.observation(side), moves))
    return tuple(result)


def active(observation, side):
    candidates = [member for member in observation['teams'][side] if member['active']]
    if len(candidates) != 1:
        raise UnsupportedFight('Hazard fixture lost its active-party identity')
    return candidates[0]


def assert_entry(case, setter, initial, transitions, replacements, machine):
    target = setter ^ 1
    key = 'spikes' if case.move == 'SPIKES' else 'toxic_spikes'
    other = 'toxic_spikes' if key == 'spikes' else 'spikes'
    facts = []
    for index in range(case.layers):
        transition = transitions[index]
        before = initial if index == 0 else transitions[index-1].observations
        after = transition.observations
        expected = index + 1
        field = after[target]['battlefield'][target]
        if field[key] != expected or field[other]:
            raise UnsupportedFight('Actual source did not place the requested hazard layer')
        for side in (0, 1):
            old, new = active(before[side], side), active(after[side], side)
            if (old['index'], old['HP'], old['status']) != (new['index'], new['HP'], new['status']):
                raise UnsupportedFight('Hazard placement interval had an unexpected HP/status/party change')
        facts.append(dict(phase='placement', turn=transition.turn, target=target, hazard=key,
                          previous=index, current=expected, move=case.move))
    before = transitions[case.layers-1].observations
    entered = transitions[case.layers]
    after = entered.observations
    if type(entered.actions[target]) is not SwitchAction or entered.actions[target].index != 1:
        raise UnsupportedFight('Hazard entry did not submit its intended source switch')
    old_roster = before[target]['teams'][target]
    entrant_before = old_roster[1]
    entrant_after = after[target]['teams'][target][1]
    field = after[target]['battlefield'][target]
    if case.control == 'entry-ko':
        living = active(after[target], target)
        if entrant_before['HP'] != 1 or entrant_after['HP'] != 0 or living['index'] != 2:
            raise UnsupportedFight('Hazard entry KO did not force the intended real replacement')
        if not any(row['side'] == target and row['action'].get('index') == 2 for row in replacements):
            raise UnsupportedFight('Entry KO lacks its actual source replacement choice')
        maximum = living['MaxHP']
        if living['HP'] != maximum - max(1, maximum // 8) or living['status']:
            raise UnsupportedFight('Replacement did not receive its own source Spikes damage')
        facts.append(dict(phase='entry-ko-and-replacement', turn=entered.turn,
                          fainted_index=1, requested_replacement=2, active=living,
                          hazard_layers=field[key], replacement_choices=replacements))
    else:
        if active(after[target], target)['index'] != 1:
            raise UnsupportedFight('Requested hazard entrant never became active')
        maximum = entrant_before['MaxHP']
        hp_before, hp_after = entrant_before['HP'], entrant_after['HP']
        if case.control == 'flying':
            flying = machine._constant('constants/type_constants.asm', 'FLYING')
            if flying not in entrant_after['types'] or hp_before != hp_after or entrant_after['status']:
                raise UnsupportedFight('Flying entrant did not preserve HP/status across grounded hazards')
            if field[key] != case.layers:
                raise UnsupportedFight('Flying entrant unexpectedly removed the hazard')
        elif case.control == 'poison':
            poison = machine._constant('constants/type_constants.asm', 'POISON')
            if (poison not in entrant_after['types'] or field[key] != 0 or
                    hp_before != hp_after or entrant_after['status']):
                raise UnsupportedFight('Grounded Poison entrant did not actually absorb Toxic Spikes')
        elif case.move == 'SPIKES':
            denominator = {1: 8, 2: 6, 3: 4}[case.layers]
            if hp_before - hp_after != max(1, maximum // denominator) or entrant_after['status']:
                raise UnsupportedFight('Source Spikes entry amount differs from its declared source layer')
        else:
            psn = 1 << machine._constant('constants/battle_constants.asm', 'PSN')
            toxic = 1 << machine._constant('constants/battle_constants.asm', 'TOX')
            expected = psn | (toxic if case.layers == 2 else 0)
            if entrant_after['status'] != expected or hp_after >= hp_before:
                raise UnsupportedFight('Toxic Spikes did not poison its entrant and activate residual damage')
            if field[key] != case.layers:
                raise UnsupportedFight('Poisoned entrant unexpectedly removed Toxic Spikes')
        facts.append(dict(phase='entry', turn=entered.turn, target=target, control=case.control,
                          before=entrant_before, after=entrant_after, hazard_layers=field[key]))
    setter_before, setter_after = active(before[setter], setter), active(after[setter], setter)
    if (setter_before['HP'], setter_before['status']) != (setter_after['HP'], setter_after['status']):
        raise UnsupportedFight('Supposedly isolated hazard interval harmed its setter')
    return facts


def run(root, output, cases, sides, seeds, native_build=None, optimization='O3'):
    game = root / 'build/stateful/game'
    sources = host_sources()
    machine = ControllerMachine(game/'polishedcrystal-3.2.3.gbc', game/'polishedcrystal-3.2.3.sym',
        game, native_build or root/'build/battle-controller/native-o3', optimization=optimization)
    oracle = Oracle(root, machine)
    output.mkdir(parents=True, exist_ok=True)
    rows = []
    for name in cases:
        case = CASES[name]
        for setter in sides:
            for seed in seeds:
                tag = f'{name}-setter{setter}-{seed}'
                replacements = []
                def observe(stage, record):
                    if stage == 'replacement-choice':
                        replacements.append(dict(side=record['side'], action=record['action']))
                    elif stage == 'failure':
                        (output/(tag+'.failure.json')).write_text(json.dumps(dict(
                            passed=False, case=name, setter=setter, seed=seed, diagnostic=record,
                            host_sources=sources), indent=2)+'\n')
                def replacement(side, observation, actions):
                    desired = SwitchAction(2)
                    if case.control == 'entry-ko' and side == (setter ^ 1) and desired in actions:
                        return desired
                    return first_replacement(side, observation, actions)
                machine.segment_checker, machine.single_step_checker = oracle.compare, oracle.single_step
                recording = RecordedRun(oracle)
                fight = ControllerFight(machine, chooser=first_attack, replacement_chooser=replacement,
                                        observer=observe, max_turns=128)
                fight._old_segment_checker = recording.capture_segment
                fight._old_single_step_checker = recording.capture_one
                base = scenario(case, setter)
                initial = fight.reset(base, seed)
                raw = bytes(machine.state.m[0xc000:0xe000])
                initial_path = output/(tag+'.initial-wram.bin')
                initial_path.write_bytes(raw)
                transitions = []
                facts = None
                while not fight.terminal and not fight.truncated:
                    transitions.append(fight.step(choose(fight, case, setter)))
                    if machine.turn == case.layers + 1:
                        facts = assert_entry(case, setter, initial, transitions, replacements, machine)
                if facts is None or not fight.terminal or fight.truncated or machine.failed:
                    (output/(tag+'.failure.json')).write_text(json.dumps(dict(
                        passed=False, case=name, setter=setter, seed=seed,
                        reason='activation-or-source-terminal', record=fight.replay_record(),
                        host_sources=sources), indent=2)+'\n')
                    raise UnsupportedFight('Hazard family failed to activate or reach a real source terminal')
                result = fight.replay_record()
                result.update(case=name, setter=setter, seed=seed, witness=facts,
                              recorded_segments=recording.rows, host_sources=sources,
                              initial_wram=str(initial_path), initial_wram_sha256=sha(initial_path),
                              comparisons=list(oracle.rows[-len(recording.rows):]),
                              controller_audit=machine.audit_configuration)
                evidence = output/(tag+'.json')
                evidence.write_text(json.dumps(result,indent=2)+'\n')
                first_replacements = list(replacements)
                replacements.clear()
                fight._old_segment_checker = recording.replay_segment
                fight._old_single_step_checker = recording.replay_one
                fight.reset(base,seed)
                while not fight.terminal and not fight.truncated:
                    fight.step(choose(fight,case,setter))
                replay = fight.replay_record()
                if (recording.position != len(recording.rows) or not fight.terminal or
                        replay['turns'] != result['turns'] or replay['winner'] != result['winner'] or
                        replacements != first_replacements):
                    raise UnsupportedFight('Hazard whole-fight native event replay diverged')
                row = dict(case=name,setter=setter,seed=seed,passed=True,turns=machine.turn,
                           comparisons=len(recording.rows),winner=fight.winner,replay_equal=True,
                           evidence=str(evidence),evidence_sha256=sha(evidence),
                           initial_wram=str(initial_path),initial_wram_sha256=sha(initial_path),witness=facts)
                rows.append(row)
                print(json.dumps({k:v for k,v in row.items() if k!='witness'}),flush=True)
    oracle.oracle.oracle_close()
    if sources != host_sources():
        raise UnsupportedFight('Host sources changed during hazard-family validation')
    report = dict(passed=True,scope='actual-source-isolated-hazard-entry-full-fights',cases=rows,
                  generator_sha256=stateful_lifting.MODULE_DIGEST,host_sources=sources,
                  rom_sha256=sha(machine.rom_path),symbols_sha256=sha(machine.symbols_path),
                  manifest_sha256=sha(machine.build_dir/'manifest.json'),
                  limits=['Declared presentation/input projections remain conditional.',
                          'Only the listed activated hazard conditions are covered.'])
    (output/'report.json').write_text(json.dumps(report,indent=2)+'\n')


if __name__ == '__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root',type=pathlib.Path,required=True)
    parser.add_argument('--output',type=pathlib.Path,required=True)
    parser.add_argument('--cases',nargs='+',choices=tuple(CASES),default=list(CASES))
    parser.add_argument('--sides',nargs='+',type=int,choices=(0,1),default=[0,1])
    parser.add_argument('--seeds',nargs='+',type=int,default=[193])
    parser.add_argument('--native-build',type=pathlib.Path)
    parser.add_argument('--optimization',choices=('O0','O1','O2','O3'),default='O3')
    args=parser.parse_args()
    run(args.artifact_root,args.output,args.cases,args.sides,args.seeds,args.native_build,args.optimization)
