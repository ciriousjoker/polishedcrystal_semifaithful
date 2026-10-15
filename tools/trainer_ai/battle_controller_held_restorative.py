"""Bounded actual-source held restorative fights with observed switch probes.

No execution occurs on import. Requires the project's exclusive CPU grant.
"""
import argparse
import copy
from dataclasses import dataclass
import hashlib
import json
import pathlib
import re

from battle_actions import SwitchAction
from battle_bag import item_record
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Pokemon, Scenario
import stateful_lifting


@dataclass(frozen=True)
class HeldCase:
    item: str
    effect: str
    kind: str
    parameter: int
    status: str = ''


CASES = {
    'oran': HeldCase('ORAN_BERRY', 'HELD_BERRY', 'hp', 10),
    'sitrus': HeldCase('SITRUS_BERRY', 'HELD_BERRY', 'hp', 0),
    'leftovers': HeldCase('LEFTOVERS', 'HELD_LEFTOVERS', 'residual', 10),
    'cheri': HeldCase('CHERI_BERRY', 'HELD_HEAL_STATUS', 'status', -1, 'PAR'),
    'chesto': HeldCase('CHESTO_BERRY', 'HELD_HEAL_STATUS', 'status', -1, 'SLP'),
    'pecha': HeldCase('PECHA_BERRY', 'HELD_HEAL_STATUS', 'status', -1, 'TOXIC'),
    'rawst': HeldCase('RAWST_BERRY', 'HELD_HEAL_STATUS', 'status', -1, 'BRN'),
    'aspear': HeldCase('ASPEAR_BERRY', 'HELD_HEAL_STATUS', 'status', -1, 'FRZ'),
    'lum': HeldCase('LUM_BERRY', 'HELD_HEAL_STATUS', 'status', 255, 'TOXIC'),
    'leppa': HeldCase('LEPPA_BERRY', 'HELD_RESTORE_PP', 'pp', 255),
    'leftovers-full': HeldCase('LEFTOVERS', 'HELD_LEFTOVERS', 'no-trigger', 10),
}
DEPENDENCIES = (
    'battle_controller_held_restorative.py', 'battle_controller.py',
    'battle_controller_profile.py', 'battle_controller_fight.py',
    'battle_controller_fight_check.py', 'battle_controller_check.py',
    'battle_controller_oracle.c', 'battle_decisions.py', 'battle_scenarios.py',
    'battle_bag.py', 'battle_actions.py', 'battle_env.py', 'battle_env_check.py', 'stateful_lifting.py',
    'stateful_fixture_check.py',
)
SOURCE_DEPENDENCIES = (
    'constants/item_constants.asm', 'constants/item_data_constants.asm',
    'constants/battle_constants.asm', 'data/items/attributes.asm',
    'data/pokemon/base_stats/horsea.asm', 'engine/battle/core.asm',
    'engine/battle/endturn.asm', 'engine/battle/effect_commands.asm',
    'home/battle.asm',
)


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def host_sources():
    return {name: sha(pathlib.Path(__file__).with_name(name)) for name in DEPENDENCIES}


def game_sources(machine):
    return {name: sha(machine.source_root/name) for name in SOURCE_DEPENDENCIES}


def status_value(machine, name):
    def flag(value):
        return 1 << machine._constant('constants/battle_constants.asm', value)
    if name == 'TOXIC':
        return flag('PSN') | flag('TOX')
    return 2 if name == 'SLP' else flag(name) if name else 0


def source_item(machine, case):
    # item_record guards the source attribute macro before decoding ROM bytes.
    identifier, pocket, context = item_record(machine, case.item)
    attributes = machine._rom_bytes('ItemAttributes', (identifier-1)*6, 6)
    effect = machine._constant('constants/item_data_constants.asm', case.effect)
    if case.kind == 'status' and case.status != 'TOXIC':
        if case.status == 'SLP':
            source = (machine.source_root/'constants/battle_constants.asm').read_text()
            mask = re.search(r'^DEF SLP_MASK EQU %([01]+)\s*(?:;[^\n]*)?$', source, re.M)
            if mask is None or int(mask[1], 2) != 7:
                raise UnsupportedFight('Source sleep mask requires review')
            parameter = int(mask[1], 2)
        else:
            parameter = status_value(machine, case.status)
    elif case.item == 'PECHA_BERRY':
        parameter = status_value(machine, 'TOXIC') & ~status_value(machine, 'TOX')
    else:
        parameter = case.parameter
    if (attributes[2], attributes[3]) != (effect, parameter):
        raise UnsupportedFight('Source/ROM held effect or parameter requires review: ' + case.item)
    return dict(item=case.item, identifier=identifier, effect=effect,
                effect_name=case.effect, parameter=parameter, pocket=pocket, context=context)


def scenario(machine, case, holder):
    pp = None
    if case.kind == 'pp':
        move = machine._constant('constants/move_constants.asm', 'WATER_GUN')
        pp = (0, machine.move_data(move)[5])
    lead = Pokemon('HORSEA', moves=('SPLASH', 'WATER_GUN'), happiness=0)
    reserve = Pokemon('HORSEA', moves=('SPLASH', 'WATER_GUN'), happiness=0,
        item=case.item, status=status_value(machine, case.status), pp=pp,
        hp=10 if case.kind in ('hp', 'residual') else None)
    teams = [(lead,), (lead,)]
    teams[holder] = (lead, reserve)
    return Scenario(player=teams[0], enemy=teams[1], inventory=(None, None))


def choose(fight, holder):
    if fight.machine.turn == 0:
        actions = [MoveAction(0), MoveAction(0)]
        actions[holder] = SwitchAction(1)
        return tuple(actions)
    actions = []
    for side in (0, 1):
        mask = fight.legal_actions(side)
        wanted = MoveAction(1)
        actions.append(wanted if wanted in mask else first_attack(side, fight.observation(side), mask))
    return tuple(actions)


def stored_holder(machine, holder):
    party = ('wPartyMon2', 'wOTPartyMon2')[holder]
    battle = ('wBattleMon', 'wEnemyMon')[holder]
    result = {}
    for name, stem in (('party', party), ('battle', battle)):
        result[name] = dict(item=machine.byte(stem+'Item'), status=machine.byte(stem+'Status'),
            HP=machine.word(stem+'HP'),
            pp=tuple(machine.state.m[machine.address(stem+'PP')+i] & 63 for i in range(4)))
    return result


def witness(machine, case, holder, metadata, initial, transition):
    old = initial[holder]['teams'][holder][1]
    new = transition.observations[holder]['teams'][holder][1]
    if (old['active'] or not new['active'] or
            type(transition.actions[holder]) is not SwitchAction or
            transition.actions[holder].index != 1):
        raise UnsupportedFight('Held-item probe did not cross the requested real switch boundary')
    if old['item'] != metadata['identifier'] or old['status'] != status_value(machine, case.status):
        raise UnsupportedFight('Held-item initial reserve precondition was not retained')
    swift_swim = machine._constant('constants/ability_constants.asm', 'SWIFT_SWIM')
    if old['ability'] != swift_swim or new['ability'] != swift_swim:
        raise UnsupportedFight('Neutral Horsea fixture did not retain its actual Swift Swim ability')
    if old['moves'] != new['moves'] or old['pp_ups'] != new['pp_ups']:
        raise UnsupportedFight('Held restorative changed moves or PP Ups')
    consumed = case.kind in ('hp', 'status', 'pp')
    if new['item'] != (0 if consumed else metadata['identifier']):
        raise UnsupportedFight('Held restorative consumption/nonconsumption differs from source contract')
    if case.kind in ('hp', 'residual'):
        if old['HP'] != 10 or old['HP'] > old['MaxHP']//2:
            raise UnsupportedFight('HP probe lacks its positive low-HP precondition')
        amount = (max(1, old['MaxHP']//4) if case.item == 'SITRUS_BERRY' else
                  max(1, old['MaxHP']//16) if case.kind == 'residual' else metadata['parameter'])
        if new['HP'] != min(old['MaxHP'], old['HP']+amount) or new['HP'] <= old['HP']:
            raise UnsupportedFight('Held HP recovery did not positively match its source amount')
    elif case.kind == 'status':
        if not old['status'] or new['status']:
            raise UnsupportedFight('Held status berry did not positively cure its intended condition')
    elif case.kind == 'pp':
        maximum = machine.move_data(old['moves'][0])[5]
        expected = (min(10, maximum), *old['pp'][1:])
        if old['pp'][0] != 0 or new['pp'] != expected or old['pp'] == new['pp']:
            raise UnsupportedFight('Leppa did not restore exactly the depleted move by its source amount')
    elif old['HP'] != old['MaxHP'] or new['HP'] != old['HP']:
        raise UnsupportedFight('Full-HP Leftovers control unexpectedly changed HP')
    residual = (max(1, old['MaxHP']//16)
                if case.kind == 'status' and case.status in ('TOXIC', 'BRN') else 0)
    if case.kind not in ('hp', 'residual') and old['HP']-residual != new['HP']:
        raise UnsupportedFight('Status/PP/control probe HP differs from its source residual order')
    if case.kind != 'status' and old['status'] != new['status']:
        raise UnsupportedFight('Non-status restorative unexpectedly changed status')
    if case.kind != 'pp' and old['pp'] != new['pp']:
        raise UnsupportedFight('Non-PP restorative unexpectedly changed PP')
    storage = stored_holder(machine, holder)
    for cache in storage.values():
        if any(cache[field] != new[field] for field in ('item', 'HP', 'status', 'pp')):
            raise UnsupportedFight('Held restorative party and active cache disagree at the source boundary')
    for side in (0, 1):
        for index, before in enumerate(initial[side]['teams'][side]):
            if side == holder and index == 1:
                continue
            after = transition.observations[side]['teams'][side][index]
            if any(before[field] != after[field] for field in ('item', 'HP', 'status')):
                raise UnsupportedFight('Isolated held-item probe altered another party member')
    return dict(scope='actual-source-reserve-switch-restoration', holder=holder, index=1,
        source_item=metadata, before=old, after=new, stored=storage,
        consumed=int(consumed), opponent_action='SPLASH',
        expected_pre_cure_residual=residual,
        boundary='pre-switch frozen decision to first post-switch frozen decision',
        control=case.kind == 'no-trigger')


def run(root, output, cases, sides, seeds, native_build=None, optimization='O3'):
    if not 1 <= len(cases)*len(sides)*len(seeds) <= 24:
        raise UnsupportedFight('Held restorative invocation must contain one through 24 fights')
    game = root/'build/stateful/game'
    sources = host_sources()
    machine = ControllerMachine(game/'polishedcrystal-3.2.3.gbc', game/'polishedcrystal-3.2.3.sym',
        game, native_build or root/'build/battle-controller/native-o3', optimization=optimization)
    oracle = Oracle(root, machine)
    if not oracle.banked_wram or not isinstance(oracle.provenance, dict):
        raise UnsupportedFight('Held restorative proof requires all-bank oracle and retained provenance')
    provenance, source_ids = copy.deepcopy(oracle.provenance), game_sources(machine)
    output.mkdir(parents=True, exist_ok=True)
    rows = []
    name = holder = seed = None
    try:
        for name in cases:
            case = CASES[name]
            metadata = source_item(machine, case)
            for holder in sides:
                for seed in seeds:
                    tag = f'{name}-holder{holder}-{seed}'
                    base = scenario(machine, case, holder)
                    machine.segment_checker, machine.single_step_checker = oracle.compare, oracle.single_step
                    recording = RecordedRun(oracle)
                    def observe(stage, record):
                        if stage == 'failure':
                            (output/(tag+'.failure.json')).write_text(json.dumps(dict(
                                passed=False, case=name, holder=holder, seed=seed, diagnostic=record,
                                host_sources=sources, game_sources=source_ids,
                                oracle_provenance=provenance), indent=2)+'\n')
                    fight = ControllerFight(machine, chooser=first_attack,
                        replacement_chooser=first_replacement, observer=observe, max_turns=128)
                    fight._old_segment_checker = recording.capture_segment
                    fight._old_single_step_checker = recording.capture_one
                    initial = fight.reset(base, seed)
                    raw = output/(tag+'.initial-wram.bin')
                    raw.write_bytes(bytes(machine.state.m[0xc000:0xe000]))
                    first = fight.step(choose(fight, holder))
                    facts = witness(machine, case, holder, metadata, initial, first)
                    while not fight.terminal and not fight.truncated:
                        fight.step(choose(fight, holder))
                    if not fight.terminal or fight.truncated or machine.failed:
                        raise UnsupportedFight('Held restorative fight did not reach its actual source terminal')
                    result = fight.replay_record()
                    result.update(case=name, holder=holder, seed=seed, witness=facts,
                        host_sources=sources, game_sources=source_ids, oracle_provenance=provenance,
                        initial_wram=str(raw), initial_wram_sha256=sha(raw),
                        recorded_segments=recording.rows,
                        comparisons=list(oracle.rows[-len(recording.rows):]),
                        controller_audit=machine.audit_configuration)
                    evidence = output/(tag+'.json')
                    evidence.write_text(json.dumps(result, indent=2)+'\n')
                    fight._old_segment_checker = recording.replay_segment
                    fight._old_single_step_checker = recording.replay_one
                    replay_initial = fight.reset(base, seed)
                    replay_first = fight.step(choose(fight, holder))
                    if witness(machine, case, holder, metadata, replay_initial, replay_first) != facts:
                        raise UnsupportedFight('Held restorative native replay activation witness diverged')
                    while not fight.terminal and not fight.truncated:
                        fight.step(choose(fight, holder))
                    replay = fight.replay_record()
                    if (not fight.terminal or fight.truncated or machine.failed or
                            recording.position != len(recording.rows) or
                            replay['turns'] != result['turns'] or replay['winner'] != result['winner']):
                        raise UnsupportedFight('Held restorative native whole-fight event replay diverged')
                    row = dict(case=name, holder=holder, seed=seed, passed=True,
                        turns=machine.turn, winner=fight.winner, comparisons=len(recording.rows),
                        replay_equal=True, witness=facts, evidence=str(evidence),
                        evidence_sha256=sha(evidence), initial_wram=str(raw), initial_wram_sha256=sha(raw))
                    rows.append(row)
                    print(json.dumps({k:v for k,v in row.items() if k != 'witness'}), flush=True)
    except Exception as error:
        (output/'failure.json').write_text(json.dumps(dict(passed=False, status='blocked',
            case=name, holder=holder, seed=seed, reason=str(error),
            completed_cases=[{k:v for k,v in row.items() if k != 'witness'} for row in rows],
            host_sources=sources, game_sources=source_ids,
            oracle_provenance=provenance), indent=2)+'\n')
        raise
    finally:
        oracle.oracle.oracle_close()
    if sources != host_sources() or source_ids != game_sources(machine):
        raise UnsupportedFight('Held restorative source inputs changed during the proof')
    report = dict(passed=True, scope='actual-source-held-restorative-switch-full-fights', cases=rows,
        host_sources=sources, game_sources=source_ids, oracle_provenance=provenance,
        generator_sha256=stateful_lifting.MODULE_DIGEST, rom_sha256=sha(machine.rom_path),
        symbols_sha256=sha(machine.symbols_path), manifest_sha256=sha(machine.build_dir/'manifest.json'),
        limits=['Declared presentation/input projections remain conditional.',
                'Only the listed activated conditions and full-HP Leftovers control are covered.',
                'Status application timing, Lum confusion, Unnerve and item theft are not covered.',
                'Flame Orb and Toxic Orb are deferred; no manifest or ledger coverage is promoted.'])
    (output/'report.json').write_text(json.dumps(report, indent=2)+'\n')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    parser.add_argument('--output', type=pathlib.Path, required=True)
    parser.add_argument('--cases', nargs='+', choices=tuple(CASES), default=list(CASES))
    parser.add_argument('--sides', nargs='+', type=int, choices=(0, 1), default=[0, 1])
    parser.add_argument('--seeds', nargs='+', type=int, default=[193])
    parser.add_argument('--native-build', type=pathlib.Path)
    parser.add_argument('--optimization', choices=('O0', 'O1', 'O2', 'O3'), default='O3')
    args = parser.parse_args()
    run(args.artifact_root, args.output, args.cases, args.sides, args.seeds,
        args.native_build, args.optimization)
