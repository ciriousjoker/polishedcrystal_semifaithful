"""Source-owned restorative player items with isolated positive witnesses."""
import argparse
import copy
from dataclasses import dataclass
import hashlib
import json
import pathlib

from battle_bag import BagAction, BagStack, item_record
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Pokemon, Scenario
import stateful_lifting


@dataclass(frozen=True)
class ItemCase:
    handler: str
    kind: str
    status: str = ''
    target: int = 0


CASES = {
    **{item: ItemCase('RestoreHPEffect', 'hp') for item in
       ('POTION', 'SUPER_POTION', 'HYPER_POTION', 'MAX_POTION')},
    'FULL_RESTORE': ItemCase('FullRestore', 'full', 'TOXIC'),
    **{item: ItemCase('HealStatusEffect', 'status', status) for item, status in
       (('ANTIDOTE', 'TOXIC'), ('BURN_HEAL', 'BRN'), ('PARALYZEHEAL', 'PAR'),
        ('AWAKENING', 'SLP'), ('ICE_HEAL', 'FRZ'), ('FULL_HEAL', 'TOXIC'))},
    'REVIVE': ItemCase('ReviveEffect', 'revive', target=1),
    'MAX_REVIVE': ItemCase('ReviveEffect', 'revive', target=1),
    'REVIVAL_HERB': ItemCase('RevivalHerb', 'revive', target=1),
    **{item: ItemCase('RestorePPEffect', kind) for item, kind in
       (('ETHER', 'single-pp'), ('MAX_ETHER', 'single-pp'),
        ('ELIXIR', 'all-pp'), ('MAX_ELIXIR', 'all-pp'))},
}
FIRST_STAGE = ('POTION', 'FULL_RESTORE', 'FULL_HEAL', 'REVIVE', 'REVIVAL_HERB', 'ETHER')
DEPENDENCIES = ('battle_controller_bag_family.py', 'battle_controller.py',
    'battle_controller_profile.py', 'battle_controller_fight.py',
    'battle_controller_fight_check.py', 'battle_controller_check.py',
    'battle_controller_oracle.c', 'battle_decisions.py', 'battle_scenarios.py',
    'battle_bag.py', 'battle_env.py', 'battle_env_check.py', 'stateful_lifting.py',
    'stateful_fixture_check.py')
BLOCKED_CONTROLS = [dict(case='ineffective-restorative-target', status='blocked',
    reason='The source can retry inside its party/bag menu. The public ControllerFight retry policy runs only after the outer battle menu reopens; this runner does not mutate its private BagInput or guess a cancellation.')]


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def host_sources():
    return {name: sha(pathlib.Path(__file__).with_name(name)) for name in DEPENDENCIES}


def source_item(machine, name, case):
    identifier, pocket, context = item_record(machine, name)
    if context != 5:
        raise UnsupportedFight('Restorative item no longer has source battle-party context: ' + name)
    attributes = machine._rom_bytes('ItemAttributes', (identifier-1)*6, 6)
    effect = machine._rom_bytes('ItemEffects', identifier, 1)[0]
    pointer = machine._rom_bytes('_DoItemEffect.ItemEffectRoutines', effect*2, 2)
    address = int.from_bytes(pointer, 'little')
    if machine.symbols[case.handler] != (machine.symbols['_DoItemEffect'][0], address):
        raise UnsupportedFight('Assembled item handler differs from selected source family: ' + name)
    return dict(item=name, identifier=identifier, pocket=pocket, context=context,
                parameter=attributes[3], effect=effect, handler=case.handler)


def scenario(machine, item, case):
    def flag(name):
        return 1 << machine._constant('constants/battle_constants.asm', name)
    status = (flag('PSN') | flag('TOX') if case.status == 'TOXIC' else
              2 if case.status == 'SLP' else flag(case.status) if case.status else 0)
    target = Pokemon('SNORLAX' if case.kind in ('hp', 'revive') else 'PSYDUCK',
        hp=0 if case.kind == 'revive' else 10 if case.kind in ('hp', 'full') else None,
        status=status, pp=(0, 1) if case.kind.endswith('pp') else None,
        happiness=140 if item == 'REVIVAL_HERB' else 0)
    other = Pokemon('PSYDUCK', happiness=0)
    own = (other, target) if case.target else (target, other)
    return Scenario(player=own, enemy=(Pokemon('PSYDUCK', moves=('SPLASH', 'TACKLE'), happiness=0),),
                    inventory=(None, None), bag=(BagStack(item),))


def choose(fight, item, case):
    if fight.machine.turn == 0:
        return BagAction(item, pokemon=case.target, move=0), MoveAction(0)
    actions = []
    for side in (0, 1):
        mask = fight.legal_actions(side)
        wanted = MoveAction(0 if side == 0 else 1)
        actions.append(wanted if wanted in mask else first_attack(side, fight.observation(side), mask))
    return tuple(actions)


def witness(machine, item, case, metadata, initial, first):
    old, new = initial[0]['teams'][0][case.target], first.observations[0]['teams'][0][case.target]
    def quantity(observation):
        return sum(number for entries in observation['player_bag'].values()
                   for identifier, number in entries if identifier == metadata['identifier'])
    if quantity(initial[0]) != 1 or quantity(first.observations[0]) != 0:
        raise UnsupportedFight('Actual source did not consume exactly the selected restorative item')
    if old['moves'] != new['moves'] or old['pp_ups'] != new['pp_ups']:
        raise UnsupportedFight('Restorative fixture unexpectedly changed moves or PP Ups')
    if case.kind == 'hp':
        amount = 999 if metadata['parameter'] == 255 else metadata['parameter']
        if new['HP'] != min(old['MaxHP'], old['HP'] + amount) or new['HP'] <= old['HP']:
            raise UnsupportedFight('Player healing amount differs from assembled item parameter')
    elif case.kind == 'full':
        if old['HP'] >= old['MaxHP'] or not old['status'] or new['HP'] != new['MaxHP'] or new['status']:
            raise UnsupportedFight('Full Restore did not positively restore both HP and status')
    elif case.kind == 'status':
        if not old['status'] or new['status'] or old['HP'] != new['HP']:
            raise UnsupportedFight('Status healer did not positively clear its intended status')
    elif case.kind == 'revive':
        expected = old['MaxHP']//2 if item == 'REVIVE' else old['MaxHP']
        if old['HP'] != 0 or new['HP'] != expected or new['HP'] <= 0:
            raise UnsupportedFight('Actual source did not revive the selected fainted reserve')
        if item == 'REVIVAL_HERB' and new['happiness'] >= old['happiness']:
            raise UnsupportedFight('Revival Herb failed to execute its source happiness penalty')
    else:
        maximum = tuple(machine.move_data(move)[5] if move else 0 for move in old['moves'])
        expected = list(old['pp'])
        for slot in range(4):
            if case.kind == 'single-pp' and slot != 0:
                continue
            expected[slot] = (maximum[slot] if item.startswith('MAX_') else
                              min(maximum[slot], old['pp'][slot]+10))
        if tuple(expected) != new['pp'] or old['pp'] == new['pp']:
            raise UnsupportedFight('PP restoration amount or selected move differs from source contract')
    if case.kind not in ('hp', 'full', 'revive') and old['HP'] != new['HP']:
        raise UnsupportedFight('Isolated status/PP probe unexpectedly changed target HP')
    if not case.kind.endswith('pp') and old['pp'] != new['pp']:
        raise UnsupportedFight('Non-PP restorative unexpectedly changed move PP')
    for index, member in enumerate(initial[0]['teams'][0]):
        if index == case.target:
            continue
        after = first.observations[0]['teams'][0][index]
        if any(member[field] != after[field] for field in ('HP', 'status', 'pp')):
            raise UnsupportedFight('Restorative changed an unselected party member')
    enemy_old, enemy_new = initial[1]['teams'][1][0], first.observations[1]['teams'][1][0]
    if any(enemy_old[field] != enemy_new[field] for field in ('HP', 'status')):
        raise UnsupportedFight('Controlled Splash probe unexpectedly harmed its opponent')
    return dict(scope='isolated-source-player-restoration', target=case.target,
                selected_move=0 if case.kind == 'single-pp' else None,
                source_item=metadata, before=old, after=new, consumed=1,
                opponent_action='SPLASH')


def run(root, output, cases, seeds, native_build=None, optimization='O3'):
    game = root/'build/stateful/game'
    sources = host_sources()
    machine = ControllerMachine(game/'polishedcrystal-3.2.3.gbc', game/'polishedcrystal-3.2.3.sym',
        game, native_build or root/'build/battle-controller/native-o3', optimization=optimization)
    oracle = Oracle(root,machine)
    if not isinstance(oracle.provenance, dict):
        raise UnsupportedFight('The loaded source oracle lacks retained provenance')
    provenance = copy.deepcopy(oracle.provenance)
    output.mkdir(parents=True,exist_ok=True)
    rows = []
    for item in cases:
        case = CASES[item]
        metadata = source_item(machine,item,case)
        base = scenario(machine,item,case)
        for seed in seeds:
            tag = f'{item.lower()}-{seed}'
            machine.segment_checker, machine.single_step_checker = oracle.compare, oracle.single_step
            recording = RecordedRun(oracle)
            def observe(stage,record):
                if stage == 'failure':
                    (output/(tag+'.failure.json')).write_text(json.dumps(dict(passed=False,
                        item=item,seed=seed,diagnostic=record,host_sources=sources,
                        oracle_provenance=provenance),indent=2)+'\n')
            fight = ControllerFight(machine,chooser=first_attack,replacement_chooser=first_replacement,
                                    observer=observe,max_turns=128)
            fight._old_segment_checker, fight._old_single_step_checker = recording.capture_segment, recording.capture_one
            initial = fight.reset(base,seed)
            raw = output/(tag+'.initial-wram.bin')
            raw.write_bytes(bytes(machine.state.m[0xc000:0xe000]))
            first = fight.step(choose(fight,item,case))
            facts = witness(machine,item,case,metadata,initial,first)
            while not fight.terminal and not fight.truncated:
                fight.step(choose(fight,item,case))
            if not fight.terminal or fight.truncated or machine.failed:
                raise UnsupportedFight('Restorative full fight did not reach its actual source terminal')
            result = fight.replay_record()
            result.update(item=item,seed=seed,witness=facts,host_sources=sources,
                oracle_provenance=provenance,initial_wram=str(raw),initial_wram_sha256=sha(raw),
                recorded_segments=recording.rows,controller_audit=machine.audit_configuration)
            evidence = output/(tag+'.json')
            evidence.write_text(json.dumps(result,indent=2)+'\n')
            fight._old_segment_checker, fight._old_single_step_checker = recording.replay_segment, recording.replay_one
            fight.reset(base,seed)
            while not fight.terminal and not fight.truncated:
                fight.step(choose(fight,item,case))
            replay = fight.replay_record()
            if (not fight.terminal or recording.position != len(recording.rows) or
                    replay['turns'] != result['turns'] or replay['winner'] != result['winner']):
                raise UnsupportedFight('Restorative source-event native whole-fight replay diverged')
            row = dict(item=item,seed=seed,passed=True,turns=machine.turn,winner=fight.winner,
                comparisons=len(recording.rows),replay_equal=True,witness=facts,
                evidence=str(evidence),evidence_sha256=sha(evidence),
                initial_wram=str(raw),initial_wram_sha256=sha(raw))
            rows.append(row)
            print(json.dumps({k:v for k,v in row.items() if k!='witness'}),flush=True)
    oracle.oracle.oracle_close()
    if sources != host_sources():
        raise UnsupportedFight('Host source changed during restorative-family checks')
    report = dict(passed=True,scope='actual-source-positive-restorative-player-bag-fights',
        cases=rows,blocked_controls=BLOCKED_CONTROLS,host_sources=sources,
        oracle_provenance=provenance,generator_sha256=stateful_lifting.MODULE_DIGEST,
        rom_sha256=sha(machine.rom_path),symbols_sha256=sha(machine.symbols_path),
        manifest_sha256=sha(machine.build_dir/'manifest.json'),
        limits=['Declared presentation/input projections remain conditional.',
                'No-effect inner-menu retry is explicitly blocked, not passed.',
                'Only the selected positive item conditions are covered.'])
    (output/'report.json').write_text(json.dumps(report,indent=2)+'\n')


if __name__ == '__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root',type=pathlib.Path,required=True)
    parser.add_argument('--output',type=pathlib.Path,required=True)
    parser.add_argument('--cases',nargs='+',choices=tuple(CASES),default=list(FIRST_STAGE))
    parser.add_argument('--seeds',nargs='+',type=int,default=[193])
    parser.add_argument('--native-build',type=pathlib.Path)
    parser.add_argument('--optimization',choices=('O0','O1','O2','O3'),default='O3')
    args=parser.parse_args()
    run(args.artifact_root,args.output,args.cases,args.seeds,args.native_build,args.optimization)
