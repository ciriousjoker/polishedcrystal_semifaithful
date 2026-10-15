"""Actual source orb timing, type immunity and Black Sludge full fights.

Import and execution require the coordinator's exclusive CPU grant.
"""
import argparse
import copy
from dataclasses import dataclass
import json
import pathlib

from battle_actions import SwitchAction
from battle_bag import item_record
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_controller_held_restorative import sha, stored_holder, host_sources as restorative_sources
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Pokemon, Scenario
import stateful_lifting


@dataclass(frozen=True)
class ResidualCase:
    item: str
    effect: str
    kind: str
    species: str = 'HORSEA'
    ability: str = 'SWIFT_SWIM'
    types: tuple = ('WATER', 'WATER')
    attack: str = 'WATER_GUN'


CASES = {
    'flame': ResidualCase('FLAME_ORB', 'HELD_SELF_BRN', 'burn'),
    'toxic': ResidualCase('TOXIC_ORB', 'HELD_SELF_PSN', 'toxic'),
    'flame-fire': ResidualCase('FLAME_ORB', 'HELD_SELF_BRN', 'immune',
        'PONYTA', 'RUN_AWAY', ('FIRE', 'FIRE'), 'EMBER'),
    'toxic-poison': ResidualCase('TOXIC_ORB', 'HELD_SELF_PSN', 'immune',
        'TENTACOOL', 'CLEAR_BODY', ('WATER', 'POISON')),
    'sludge-poison': ResidualCase('BLACK_SLUDGE', 'HELD_BLACK_SLUDGE', 'heal',
        'TENTACOOL', 'CLEAR_BODY', ('WATER', 'POISON')),
    'sludge-other': ResidualCase('BLACK_SLUDGE', 'HELD_BLACK_SLUDGE', 'harm'),
}
SOURCE_DEPENDENCIES = (
    'constants/item_constants.asm', 'constants/item_data_constants.asm',
    'constants/battle_constants.asm', 'constants/type_constants.asm',
    'constants/ability_constants.asm', 'data/items/attributes.asm',
    'data/pokemon/base_stats/horsea.asm', 'data/pokemon/base_stats/tentacool.asm',
    'data/pokemon/base_stats/ponyta_plain.asm', 'engine/battle/core.asm',
    'engine/battle/endturn.asm', 'engine/battle/effect_commands.asm', 'home/battle.asm',
)
PROBE_ROUNDS = 3


def host_sources():
    sources = restorative_sources()
    sources[pathlib.Path(__file__).name] = sha(__file__)
    return sources


def game_sources(machine):
    return {name: sha(machine.source_root/name) for name in SOURCE_DEPENDENCIES}


def source_item(machine, case):
    identifier, pocket, context = item_record(machine, case.item)
    attributes = machine._rom_bytes('ItemAttributes', (identifier-1)*6, 6)
    effect = machine._constant('constants/item_data_constants.asm', case.effect)
    if (attributes[2], attributes[3]) != (effect, 0):
        raise UnsupportedFight('Held residual source/ROM effect or parameter requires review: '+case.item)
    return dict(item=case.item, identifier=identifier, effect=effect,
                effect_name=case.effect, parameter=attributes[3], pocket=pocket, context=context)


def scenario(case, holder):
    lead = Pokemon('HORSEA', moves=('SPLASH', 'WATER_GUN'), happiness=0)
    reserve = Pokemon(case.species, moves=('SPLASH', case.attack), item=case.item,
        happiness=0, hp=10 if case.kind == 'heal' else None)
    teams = [(lead,), (lead,)]
    teams[holder] = (lead, reserve)
    return Scenario(player=teams[0], enemy=teams[1], inventory=(None, None))


def choose(fight, holder):
    turn = fight.machine.turn
    if turn < PROBE_ROUNDS:
        actions = [MoveAction(0), MoveAction(0)]
        if turn == 0:
            actions[holder] = SwitchAction(1)
        return tuple(actions)
    actions = []
    for side in (0, 1):
        mask = fight.legal_actions(side)
        wanted = MoveAction(1)
        actions.append(wanted if wanted in mask else first_attack(side, fight.observation(side), mask))
    return tuple(actions)


def probe(fight, holder):
    transitions, caches = [], []
    for _ in range(PROBE_ROUNDS):
        transitions.append(fight.step(choose(fight, holder)))
        caches.append(stored_holder(fight.machine, holder))
    return transitions, caches


def witness(machine, case, holder, metadata, initial, transitions, caches):
    old = initial[holder]['teams'][holder][1]
    ability = machine._constant('constants/ability_constants.asm', case.ability)
    types = tuple(machine._constant('constants/type_constants.asm', name) for name in case.types)
    flag = lambda name: 1 << machine._constant('constants/battle_constants.asm', name)
    status = flag('BRN') if case.kind == 'burn' else flag('PSN') | flag('TOX') if case.kind == 'toxic' else 0
    if old['active'] or old['status'] or old['item'] != metadata['identifier'] or old['ability'] != ability:
        raise UnsupportedFight('Held residual reserve lacks its observed item/ability/status precondition')
    if old['HP'] != (10 if case.kind == 'heal' else old['MaxHP']):
        raise UnsupportedFight('Held residual reserve HP precondition differs from the requested scenario')
    expected_hp, previous = old['HP'], initial
    facts = []
    for probe, transition in enumerate(transitions):
        new = transition.observations[holder]['teams'][holder][1]
        action = transition.actions[holder]
        if probe == 0:
            if type(action) is not SwitchAction or action.index != 1:
                raise UnsupportedFight('Residual probe did not enter through its actual source switch')
        elif type(action) is not MoveAction or action.slot != 0:
            raise UnsupportedFight('Residual probe did not execute its requested Splash interval')
        change = 0
        if case.kind == 'heal':
            change = max(1, old['MaxHP']//16)
            expected_hp = min(old['MaxHP'], expected_hp+change)
        elif case.kind == 'harm':
            change = -max(1, old['MaxHP']//8)
            expected_hp += change
        elif probe and case.kind in ('burn', 'toxic'):
            change = -max(1, old['MaxHP']//16) * (probe if case.kind == 'toxic' else 1)
            expected_hp += change
        expected_pp = (old['pp'][0]-probe, *old['pp'][1:])
        if (not new['active'] or new['ability'] != ability or new['types'] != types or
                new['HP'] != expected_hp or new['status'] != status or
                new['item'] != metadata['identifier'] or new['pp'] != expected_pp or
                new['moves'] != old['moves'] or new['pp_ups'] != old['pp_ups']):
            raise UnsupportedFight('Held residual activation/timing/type/PP witness differs from actual source')
        if case.kind in ('heal', 'harm') and not change:
            raise UnsupportedFight('Black Sludge has no positive source transition')
        if case.kind in ('burn', 'toxic') and probe == 0 and expected_hp != old['HP']:
            raise UnsupportedFight('Orb inflicted residual before its source application round ended')
        for cache in caches[probe].values():
            if any(cache[field] != new[field] for field in ('item', 'HP', 'status', 'pp')):
                raise UnsupportedFight('Residual held-item active/party cache synchronization differs')
        for side in (0, 1):
            for index, before in enumerate(previous[side]['teams'][side]):
                if side == holder and index == 1:
                    continue
                after = transition.observations[side]['teams'][side][index]
                if any(before[field] != after[field] for field in ('item', 'HP', 'status')):
                    raise UnsupportedFight('Residual Splash probe changed another party member')
        facts.append(dict(probe=probe, turn=transition.turn, expected_hp_change=change,
            before=old if probe == 0 else transitions[probe-1].observations[holder]['teams'][holder][1],
            after=new, expected_status=status, stored=caches[probe]))
        previous = transition.observations
    return dict(scope='actual-source-held-residual-three-round-switch-probe', holder=holder,
        index=1, source_item=metadata, species=case.species, ability=case.ability,
        source_types=types, consumed=0, opponent_action='SPLASH',
        probe_rounds=PROBE_ROUNDS, initial=old, probes=facts,
        immunity_control=case.kind == 'immune')


def run(root, output, cases, sides, seeds, native_build=None, optimization='O3'):
    if not 1 <= len(cases)*len(sides)*len(seeds) <= 12:
        raise UnsupportedFight('Held residual batch must contain one through twelve fights')
    game = root/'build/stateful/game'
    sources = host_sources()
    machine = ControllerMachine(game/'polishedcrystal-3.2.3.gbc', game/'polishedcrystal-3.2.3.sym',
        game, native_build or root/'build/battle-controller/native-o3', optimization=optimization)
    oracle = Oracle(root, machine)
    if not oracle.banked_wram or not isinstance(oracle.provenance, dict):
        raise UnsupportedFight('Residual proof requires all-bank Oracle with retained provenance')
    provenance, source_ids = copy.deepcopy(oracle.provenance), game_sources(machine)
    output.mkdir(parents=True, exist_ok=True)
    rows = []
    name = holder = seed = None
    try:
        for name in cases:
            case, metadata = CASES[name], source_item(machine, CASES[name])
            for holder in sides:
                for seed in seeds:
                    tag = f'{name}-holder{holder}-{seed}'
                    base = scenario(case, holder)
                    machine.segment_checker, machine.single_step_checker = oracle.compare, oracle.single_step
                    recording = RecordedRun(oracle)
                    def observe(stage, record):
                        if stage == 'failure':
                            (output/(tag+'.failure.json')).write_text(json.dumps(dict(passed=False,
                                case=name, holder=holder, seed=seed, diagnostic=record,
                                host_sources=sources, game_sources=source_ids,
                                oracle_provenance=provenance), indent=2)+'\n')
                    fight = ControllerFight(machine, chooser=first_attack,
                        replacement_chooser=first_replacement, observer=observe, max_turns=128)
                    fight._old_segment_checker = recording.capture_segment
                    fight._old_single_step_checker = recording.capture_one
                    initial = fight.reset(base, seed)
                    raw = output/(tag+'.initial-wram.bin')
                    raw.write_bytes(bytes(machine.state.m[0xc000:0xe000]))
                    transitions, caches = probe(fight, holder)
                    facts = witness(machine, case, holder, metadata, initial, transitions, caches)
                    while not fight.terminal and not fight.truncated:
                        fight.step(choose(fight, holder))
                    if not fight.terminal or fight.truncated or machine.failed:
                        raise UnsupportedFight('Residual held-item fight failed its actual source terminal')
                    result = fight.replay_record()
                    result.update(case=name, holder=holder, seed=seed, witness=facts,
                        host_sources=sources, game_sources=source_ids, oracle_provenance=provenance,
                        initial_wram=str(raw), initial_wram_sha256=sha(raw), recorded_segments=recording.rows,
                        comparisons=list(oracle.rows[-len(recording.rows):]),
                        controller_audit=machine.audit_configuration)
                    evidence = output/(tag+'.json')
                    evidence.write_text(json.dumps(result, indent=2)+'\n')
                    fight._old_segment_checker = recording.replay_segment
                    fight._old_single_step_checker = recording.replay_one
                    replay_initial = fight.reset(base, seed)
                    replay_probes, replay_caches = probe(fight, holder)
                    if witness(machine, case, holder, metadata, replay_initial, replay_probes, replay_caches) != facts:
                        raise UnsupportedFight('Residual native replay probe witness diverged')
                    while not fight.terminal and not fight.truncated:
                        fight.step(choose(fight, holder))
                    replay = fight.replay_record()
                    if (not fight.terminal or fight.truncated or machine.failed or
                            recording.position != len(recording.rows) or
                            replay['turns'] != result['turns'] or replay['winner'] != result['winner']):
                        raise UnsupportedFight('Residual native whole-fight event replay diverged')
                    row = dict(case=name, holder=holder, seed=seed, passed=True, turns=machine.turn,
                        winner=fight.winner, comparisons=len(recording.rows), replay_equal=True,
                        witness=facts, evidence=str(evidence), evidence_sha256=sha(evidence),
                        initial_wram=str(raw), initial_wram_sha256=sha(raw))
                    rows.append(row)
                    print(json.dumps({k:v for k,v in row.items() if k != 'witness'}), flush=True)
    except Exception as error:
        (output/'failure.json').write_text(json.dumps(dict(passed=False, status='blocked',
            case=name, holder=holder, seed=seed, reason=str(error),
            completed_cases=[{k:v for k,v in row.items() if k != 'witness'} for row in rows],
            host_sources=sources, game_sources=source_ids, oracle_provenance=provenance), indent=2)+'\n')
        raise
    finally:
        oracle.oracle.oracle_close()
    if sources != host_sources() or source_ids != game_sources(machine):
        raise UnsupportedFight('Source inputs changed during held residual proof')
    report = dict(passed=True, scope='actual-source-held-residual-three-round-full-fights', cases=rows,
        host_sources=sources, game_sources=source_ids, oracle_provenance=provenance,
        generator_sha256=stateful_lifting.MODULE_DIGEST, rom_sha256=sha(machine.rom_path),
        symbols_sha256=sha(machine.symbols_path), manifest_sha256=sha(machine.build_dir/'manifest.json'),
        limits=['Declared presentation/input projections remain conditional.',
                'Only listed orb timing/type immunity and Black Sludge conditions are covered.',
                'No Corrosion, status/ability immunity combinations, theft or faint-boundary claims.',
                'No central coverage row is promoted automatically by this runner.'])
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
