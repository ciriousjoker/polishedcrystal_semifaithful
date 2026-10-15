"""Bounded actual-source delayed and volatile whole fights.

CODEPREP: import and execution require the coordinator's exclusive CPU grant.
"""
import argparse
import copy
from dataclasses import dataclass
import hashlib
import json
import pathlib
import sys

from battle_actions import SwitchAction
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight, encode_action
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Pokemon, Scenario
import stateful_lifting


@dataclass(frozen=True)
class DelayedCase:
    move: str
    rounds: int
    kind: str


CASES = {
    'leech': DelayedCase('LEECH_SEED', 3, 'leech'),
    'leech-switch': DelayedCase('LEECH_SEED', 3, 'leech-switch'),
    'leech-grass': DelayedCase('LEECH_SEED', 3, 'leech-grass'),
    'curse-ghost': DelayedCase('CURSE', 3, 'curse-ghost'),
    'curse-other': DelayedCase('CURSE', 3, 'curse-other'),
    'perish': DelayedCase('PERISH_SONG', 5, 'perish'),
    'future-switch': DelayedCase('FUTURE_SIGHT', 4, 'future-switch'),
    'wrap': DelayedCase('WRAP', 6, 'wrap'),
}
HOST_DEPENDENCIES = (
    'battle_controller_delayed.py', 'battle_controller.py', 'battle_controller_profile.py',
    'battle_controller_fight.py', 'battle_controller_fight_check.py',
    'battle_controller_check.py', 'battle_controller_oracle.c', 'battle_decisions.py',
    'battle_scenarios.py', 'battle_bag.py', 'battle_actions.py', 'battle_coverage.py',
    'battle_env.py', 'battle_env_check.py', 'stateful_lifting.py', 'stateful_fixture_check.py',
    'battle_actions_profile.py', 'battle_env_profile.py', 'battle_party.py',
    'battle_party_profile.py', 'battle_party_probe.py', 'battle_status.py', 'battle_status_profile.py',
)
SOURCE_PATHS = (
    'engine/battle/endturn.asm', 'engine/battle/effect_commands.asm',
    'engine/battle/core.asm', 'home/battle.asm',
    'engine/battle/move_effects/leech_seed.asm', 'engine/battle/move_effects/curse.asm',
    'engine/battle/move_effects/perish_song.asm', 'engine/battle/move_effects/future_sight.asm',
    'constants/battle_constants.asm', 'constants/type_constants.asm',
    'constants/pokemon_constants.asm', 'constants/pokemon_data_constants.asm',
    'constants/ability_constants.asm', 'constants/move_constants.asm',
    'constants/move_effect_constants.asm', 'data/moves/moves.asm',
    'data/pokemon/base_stats/horsea.asm', 'data/pokemon/base_stats/gastly.asm',
    'data/pokemon/base_stats/oddish.asm',
)


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def host_sources():
    directory = pathlib.Path(__file__).parent
    return {name: sha(directory/name) for name in HOST_DEPENDENCIES}


def retain_inputs(output, identities):
    destination = output/'inputs'
    destination.mkdir(parents=True, exist_ok=True)
    result = {}
    for source, digest in identities.items():
        path = pathlib.Path(source)
        content = path.read_bytes()
        if hashlib.sha256(content).hexdigest() != digest:
            raise UnsupportedFight('Delayed input changed before retention: '+source)
        target = destination/(digest+'-'+path.name)
        if not target.exists():
            target.write_bytes(content)
        if sha(target) != digest:
            raise UnsupportedFight('Delayed retained input hash differs: '+source)
        result[source] = dict(sha256=digest, snapshot=str(target))
    return result


def identity_files(value):
    result = {}
    if isinstance(value, dict):
        if isinstance(value.get('path'), str) and isinstance(value.get('sha256'), str):
            path = pathlib.Path(value['path'])
            if not path.is_file() or sha(path) != value['sha256']:
                raise UnsupportedFight('Delayed Oracle provenance differs from disk')
            result[str(path)] = value['sha256']
        for child in value.values():
            result.update(identity_files(child))
    elif isinstance(value, (tuple, list)):
        for child in value:
            result.update(identity_files(child))
    return result


def source_metadata(machine, case):
    identifier = machine._constant('constants/move_constants.asm', case.move)
    effect_name = 'EFFECT_TRAP' if case.move == 'WRAP' else 'EFFECT_'+case.move
    compiled = tuple(machine.move_data(identifier))
    effect = machine._constant('constants/move_effect_constants.asm', effect_name)
    if compiled[1] != effect:
        raise UnsupportedFight('Delayed named move compiled effect differs from source: '+case.move)
    # Keep the observed compiled move record. Accuracy and all damage stay source owned.
    return dict(move=case.move, identifier=identifier, compiled_move=compiled,
        effect=effect, effect_name=effect_name,
        interval_source='engine/battle/endturn.asm',
        cast_source='engine/battle/effect_commands.asm' if case.move == 'WRAP' else 'engine/battle/move_effects/'+{
            'LEECH_SEED': 'leech_seed', 'CURSE': 'curse', 'PERISH_SONG': 'perish_song',
            'FUTURE_SIGHT': 'future_sight'}[case.move]+'.asm')


def scenario(case, setter):
    neutral = Pokemon('HORSEA', moves=('SPLASH', 'WATER_GUN'), happiness=0)
    caster = Pokemon('GASTLY' if case.kind == 'curse-ghost' else 'HORSEA',
        moves=('SPLASH', 'WATER_GUN', case.move), happiness=0,
        hp=10 if case.kind.startswith('leech') else None)
    target = Pokemon('ODDISH' if case.kind == 'leech-grass' else 'HORSEA',
        moves=('SPLASH', 'WATER_GUN'), happiness=0,
        level=100 if case.kind == 'future-switch' else 50)
    reserve = Pokemon('HORSEA', moves=('SPLASH', 'WATER_GUN'), happiness=0,
        level=100 if case.kind == 'future-switch' else 50)
    teams = [None, None]
    teams[setter] = (caster, neutral) if case.kind == 'perish' else (caster,)
    teams[setter ^ 1] = (target, reserve) if case.kind in (
        'leech-switch', 'future-switch', 'wrap', 'perish') else (target,)
    return Scenario(player=teams[0], enemy=teams[1], inventory=(None, None))


def choose(fight, case, setter):
    turn = fight.machine.turn
    if turn < case.rounds:
        actions = [MoveAction(0), MoveAction(0)]
        if turn == 0:
            actions[setter] = MoveAction(2)
        elif turn == 1 and case.kind in ('leech-switch', 'future-switch'):
            actions[setter ^ 1] = SwitchAction(1)
        return tuple(actions)
    actions = []
    for side in (0, 1):
        mask = fight.legal_actions(side)
        wanted = MoveAction(1)
        actions.append(wanted if wanted in mask else first_attack(side, fight.observation(side), mask))
    return tuple(actions)


def active(observations, side):
    found = [member for member in observations[side]['teams'][side] if member['active']]
    if len(found) != 1:
        raise UnsupportedFight('Delayed probe lost its active party identity')
    return found[0]


def probe(fight, case, setter):
    transitions, masks = [], []
    for _ in range(case.rounds):
        if fight.terminal or fight.truncated:
            raise UnsupportedFight('Delayed probe terminated before its bounded witness')
        masks.append(tuple(fight.legal_actions(side) for side in (0, 1)))
        transitions.append(fight.step(choose(fight, case, setter)))
    masks.append(tuple(fight.legal_actions(side) for side in (0, 1)))
    replacements = [choice for turn in fight.records for choice in turn['replacement_choices']]
    return transitions, dict(masks=masks, replacements=replacements)


def witness(machine, case, setter, metadata, initial, transitions, probe_data):
    target, kind = setter ^ 1, case.kind
    masks = probe_data['masks']
    replacements = probe_data['replacements']
    if kind == 'perish':
        if (len(replacements) != 2 or {choice['side'] for choice in replacements} != {0, 1} or
                any(choice['action'] != encode_action(SwitchAction(1)) for choice in replacements)):
            raise UnsupportedFight('Perish Song lacks both explicit source replacement choices')
    elif replacements:
        raise UnsupportedFight('Isolated delayed probe unexpectedly requested replacement')
    before = initial
    facts = []
    for side in (0, 1):
        first = active(initial, side)
        species = 'GASTLY' if side == setter and kind == 'curse-ghost' else (
            'ODDISH' if side == target and kind == 'leech-grass' else 'HORSEA')
        ability = {'GASTLY': 'LEVITATE', 'ODDISH': 'CHLOROPHYLL', 'HORSEA': 'SWIFT_SWIM'}[species]
        expected_ability = machine._constant('constants/ability_constants.asm', ability)
        types = {'GASTLY': ('GHOST', 'POISON'), 'ODDISH': ('GRASS', 'POISON'),
            'HORSEA': ('WATER', 'WATER')}[species]
        expected_types = tuple(machine._constant('constants/type_constants.asm', name) for name in types)
        if (first['ability'] != expected_ability or tuple(first['types']) != expected_types or
                first['item'] or first['status']):
            raise UnsupportedFight('Delayed initial neutral species/ability/item/status precondition differs')
    for round_index, transition in enumerate(transitions):
        after = transition.observations
        old_target, new_target = active(before, target), active(after, target)
        old_setter, new_setter = active(before, setter), active(after, setter)
        field = after[target]['battlefield'][target]
        own_field = after[setter]['battlefield'][setter]
        if transition.actions != choose_actions(case, setter, round_index):
            raise UnsupportedFight('Delayed probe submitted a different cast/Splash/switch action')
        if kind.startswith('leech'):
            seeded = kind != 'leech-grass' and (kind != 'leech-switch' or round_index == 0)
            damage = max(1, old_target['MaxHP']//8) if seeded else 0
            if field['volatile']['leech_seed'] != seeded:
                raise UnsupportedFight('Leech Seed source volatile or switch cure differs')
            expected_hp = old_target['HP']-damage if new_target['index'] == old_target['index'] else new_target['MaxHP']
            if new_target['HP'] != expected_hp or new_setter['HP'] != min(
                    new_setter['MaxHP'], old_setter['HP']+(max(1, damage//2) if damage else 0)):
                raise UnsupportedFight('Leech Seed positive drain/heal or immunity/switch interval differs')
            if kind == 'leech-grass' and machine._constant('constants/type_constants.asm', 'GRASS') not in new_target['types']:
                raise UnsupportedFight('Leech immunity control lacks observed Grass type')
        elif kind == 'curse-ghost':
            damage = max(1, old_target['MaxHP']//4)
            cost = max(1, old_setter['MaxHP']//2) if round_index == 0 else 0
            if (not field['volatile']['curse'] or new_target['HP'] != old_target['HP']-damage or
                    new_setter['HP'] != old_setter['HP']-cost):
                raise UnsupportedFight('Ghost Curse half-HP cost or repeated quarter-HP damage differs')
            if machine._constant('constants/type_constants.asm', 'GHOST') not in new_setter['types']:
                raise UnsupportedFight('Ghost Curse caster lacks its observed Ghost type')
        elif kind == 'curse-other':
            expected = list(initial[setter]['battlefield'][setter]['stages'])
            expected[0] += 1
            expected[1] += 1
            expected[2] -= 1
            if (tuple(own_field['stages']) != tuple(expected) or field['volatile']['curse'] or
                    new_target['HP'] != old_target['HP'] or new_setter['HP'] != old_setter['HP']):
                raise UnsupportedFight('Non-Ghost Curse source stage and no-damage branch differs')
        elif kind == 'perish':
            for side in (0, 1):
                count = after[side]['battlefield'][side]['PerishCount']
                if round_index < 3:
                    if count != 3-round_index or active(after, side)['HP'] != active(before, side)['HP']:
                        raise UnsupportedFight('Perish Song source four-count decrement or no-early-damage differs')
                elif (count or after[side]['teams'][side][0]['HP'] or active(after, side)['index'] != 1 or
                        active(after, side)['HP'] != active(after, side)['MaxHP']):
                    raise UnsupportedFight('Perish Song simultaneous active faints or both replacements differ')
        elif kind == 'future-switch':
            expected_count = (0x12, 0x11, 0, 0)[round_index]
            if own_field['FutureSightCount'] != expected_count:
                raise UnsupportedFight('Future Sight source packed caster/tick counter differs')
            if new_target['index'] != (0 if round_index == 0 else 1):
                raise UnsupportedFight('Future Sight target switch was not observed')
            if round_index == 2:
                if not 0 < new_target['HP'] < old_target['HP']:
                    raise UnsupportedFight('Future Sight lacks positive delayed damage on switched target')
            elif new_target['HP'] != (old_target['HP'] if new_target['index'] == old_target['index'] else new_target['MaxHP']):
                raise UnsupportedFight('Future Sight damaged outside its source third endturn tick')
            if new_setter['HP'] != old_setter['HP']:
                raise UnsupportedFight('Future Sight Splash interval harmed its caster')
        elif kind == 'wrap':
            count = field['WrapCount']
            if round_index == 0:
                if SwitchAction(1) not in masks[0][target]:
                    raise UnsupportedFight('Wrap target did not begin with a legal reserve switch')
                if count not in (3, 4) or new_target['HP'] >= old_target['HP']:
                    raise UnsupportedFight('Wrap did not hit and create its source random four/five-tick trap')
                if field['TrappingMove'] != metadata['identifier']:
                    raise UnsupportedFight('Wrap trapping-move identity differs')
            else:
                expected = max(0, before[target]['battlefield'][target]['WrapCount']-1)
                damage = max(1, old_target['MaxHP']//8) if expected else 0
                if count != expected or new_target['HP'] != old_target['HP']-damage:
                    raise UnsupportedFight('Wrap decrement-before-damage expiry interval differs')
            if (SwitchAction(1) in masks[round_index+1][target]) != (count == 0):
                raise UnsupportedFight('Source legal switch mask did not recover exactly at Wrap expiry')
            if new_setter['HP'] != old_setter['HP']:
                raise UnsupportedFight('Wrap Splash probe harmed its caster')
        for side in (0, 1):
            for index, new in enumerate(after[side]['teams'][side]):
                old = before[side]['teams'][side][index]
                if any(old[key] != new[key] for key in ('moves', 'pp_ups', 'item', 'status')):
                    raise UnsupportedFight('Delayed probe changed fixed moves, PP Ups, item or status')
                expected_pp = list(old['pp'])
                action = transition.actions[side]
                if old['active'] and type(action) is MoveAction:
                    expected_pp[action.slot] -= 1
                if tuple(new['pp']) != tuple(expected_pp):
                    raise UnsupportedFight('Delayed probe source PP consumption differs')
                if not old['active'] and not new['active'] and old['HP'] != new['HP']:
                    raise UnsupportedFight('Delayed probe changed an inactive reserve HP')
        facts.append(dict(round=round_index, turn=transition.turn, observations=after,
            legal_masks=tuple(tuple(encode_action(action) for action in mask) for mask in masks[round_index+1])))
        before = after
    return dict(kind=kind, setter=setter, metadata=metadata, initial=initial, probes=facts,
        initial_legal_masks=tuple(tuple(encode_action(action) for action in mask) for mask in masks[0]),
        replacement_choices=replacements)


def choose_actions(case, setter, turn):
    actions = [MoveAction(0), MoveAction(0)]
    if turn == 0:
        actions[setter] = MoveAction(2)
    elif turn == 1 and case.kind in ('leech-switch', 'future-switch'):
        actions[setter ^ 1] = SwitchAction(1)
    return tuple(actions)


def initial_memory(machine):
    return bytes(machine.state.m[0:0x6000])+bytes(machine.state.m[0xc000:0xe000])+bytes(
        machine.state.m[0xff80:0xffff])+bytes(machine.state.m[0xff70:0xff71])


def replay_contract(record):
    # RecordedRun proves normalized CPU/events/memory at every segment. Its
    # documented unused event transport tail must also be excluded from turn
    # hashes here, while all typed choices and consumed events remain exact.
    result = copy.deepcopy(record)
    result.pop('initial_state_sha256', None)
    for turn in result['turns']:
        turn.pop('state_sha256', None)
    return result


def run(root, output, cases, sides, seeds, native_build=None, optimization='O3'):
    if not 1 <= len(cases)*len(sides)*len(seeds) <= 16:
        raise UnsupportedFight('Delayed invocation must contain one through sixteen fights')
    if len(set(cases)) != len(cases) or len(set(sides)) != len(sides) or len(set(seeds)) != len(seeds):
        raise UnsupportedFight('Duplicate delayed case keys are not independent evidence')
    output.mkdir(parents=True, exist_ok=True)
    game = root/'build/stateful/game'
    machine = ControllerMachine(game/'polishedcrystal-3.2.3.gbc', game/'polishedcrystal-3.2.3.sym',
        game, native_build or root/'build/battle-controller/native-o3', optimization=optimization)
    oracle = Oracle(root, machine)
    if not oracle.banked_wram or not isinstance(oracle.provenance, dict):
        raise UnsupportedFight('Delayed requires retained all-bank independent Oracle provenance')
    provenance = copy.deepcopy(oracle.provenance)
    if 'libmgba' not in provenance or 'libmgba_identity_error' in provenance:
        raise UnsupportedFight('Delayed requires the actual loaded mGBA dependency identity')
    sources = host_sources()
    source_ids = {path: sha(game/path) for path in SOURCE_PATHS}
    directory = pathlib.Path(__file__).parent
    immutable = {**{str((directory/path).resolve()): digest for path, digest in sources.items()},
        **{str(game/path): digest for path, digest in source_ids.items()},
        str(machine.rom_path): sha(machine.rom_path),
        str(machine.symbols_path): sha(machine.symbols_path),
        str(machine.build_dir/'manifest.json'): sha(machine.build_dir/'manifest.json')}
    immutable.update(identity_files(provenance))
    manifest = json.loads((machine.build_dir/'manifest.json').read_text())
    native_library = pathlib.Path(machine.lib._name).resolve()
    if sha(native_library) != manifest['library_sha256']:
        raise UnsupportedFight('Delayed loaded native library differs from retained manifest')
    immutable[str(native_library)] = sha(native_library)
    generated = machine.build_dir/'stateful.c'
    if not generated.is_file():
        raise UnsupportedFight('Delayed native generated C is unavailable for retention')
    if sha(generated) != manifest['generated_c_sha256']:
        raise UnsupportedFight('Delayed native generated C differs from retained manifest')
    immutable[str(generated)] = sha(generated)
    receipt = root/'build/battle-coverage/original-build-receipt.json'
    if receipt.exists():
        build_receipt = json.loads(receipt.read_text())
        if (build_receipt.get('rom_sha256') != sha(machine.rom_path) or
                build_receipt.get('symbols_sha256') != sha(machine.symbols_path)):
            raise UnsupportedFight('Delayed source assembler receipt differs from running ROM/symbols')
        immutable[str(receipt)] = sha(receipt)
    retained = retain_inputs(output, immutable)
    rows = []
    name = setter = seed = None
    try:
        for name in cases:
            case, metadata = CASES[name], source_metadata(machine, CASES[name])
            for setter in sides:
                for seed in seeds:
                    tag = f'{name}-setter{setter}-{seed}'
                    base = scenario(case, setter)
                    machine.segment_checker, machine.single_step_checker = oracle.compare, oracle.single_step
                    recording = RecordedRun(oracle)
                    def observe(stage, record):
                        if stage == 'failure':
                            (output/(tag+'.failure.json')).write_text(json.dumps(dict(
                                passed=False, case=name, setter=setter, seed=seed, diagnostic=record,
                                inputs=retained, oracle_provenance=provenance), indent=2)+'\n')
                    fight = ControllerFight(machine, chooser=first_attack,
                        replacement_chooser=first_replacement, observer=observe, max_turns=128)
                    fight._old_segment_checker = recording.capture_segment
                    fight._old_single_step_checker = recording.capture_one
                    initial = fight.reset(base, seed)
                    initial_contract = fight.replay_record()
                    initial_normalized_state = recording.digest(machine)
                    wram = output/(tag+'.initial-wram.bin')
                    wram.write_bytes(bytes(machine.state.m[0xc000:0xe000]))
                    raw = output/(tag+'.initial-memory.bin')
                    raw.write_bytes(initial_memory(machine))
                    transitions, caches = probe(fight, case, setter)
                    (output/(tag+'.probe.json')).write_text(json.dumps(dict(initial=initial,
                        transitions=[dict(turn=row.turn, actions=[encode_action(action) for action in row.actions],
                            observations=row.observations) for row in transitions],
                        masks=[[tuple(encode_action(action) for action in mask) for mask in pair]
                            for pair in caches['masks']], replacements=caches['replacements'],
                        host_sources=sources, game_sources=source_ids, oracle_provenance=provenance,
                        inputs=retained), indent=2)+'\n')
                    facts = witness(machine, case, setter, metadata, initial, transitions, caches)
                    while not fight.terminal and not fight.truncated:
                        fight.step(choose(fight, case, setter))
                    if not fight.terminal or fight.truncated or machine.failed:
                        raise UnsupportedFight('Delayed fight failed its real source terminal')
                    result = fight.replay_record()
                    result.update(case=name, setter=setter, seed=seed, witness=facts,
                        initial_contract=initial_contract,
                        host_sources=sources, game_sources=source_ids,
                        initial_wram=str(wram), initial_wram_sha256=sha(wram),
                        initial_wram_boundary='post-reset-first-frozen-actor-decision',
                        inputs=retained, oracle_provenance=provenance,
                        initial_normalized_state_sha256=initial_normalized_state,
                        initial_memory=str(raw), initial_memory_sha256=sha(raw),
                        initial_memory_layout=['WRAM banks 2-7:0000-5fff', 'WRAM banks 0-1:c000-dfff',
                            'HRAM:ff80-fffe', 'SVBK:ff70'],
                        recorded_segments=recording.rows,
                        comparisons=list(oracle.rows[-len(recording.rows):]),
                        controller_audit=machine.audit_configuration)
                    evidence = output/(tag+'.json')
                    evidence.write_text(json.dumps(result, indent=2)+'\n')
                    fight._old_segment_checker = recording.replay_segment
                    fight._old_single_step_checker = recording.replay_one
                    replay_initial = fight.reset(base, seed)
                    if initial_memory(machine) != raw.read_bytes():
                        raise UnsupportedFight('Delayed all-bank/HRAM/SVBK initial memory replay diverged')
                    if bytes(machine.state.m[0xc000:0xe000]) != wram.read_bytes():
                        raise UnsupportedFight('Delayed standalone initial WRAM replay diverged')
                    if recording.digest(machine) != initial_normalized_state:
                        raise UnsupportedFight('Delayed normalized initial CPU/event state replay diverged')
                    replay_probes, replay_caches = probe(fight, case, setter)
                    if witness(machine, case, setter, metadata, replay_initial, replay_probes, replay_caches) != facts:
                        raise UnsupportedFight('Delayed replay probe state diverged')
                    while not fight.terminal and not fight.truncated:
                        fight.step(choose(fight, case, setter))
                    replay = fight.replay_record()
                    if (not fight.terminal or fight.truncated or machine.failed or
                            recording.position != len(recording.rows) or
                            replay_contract(replay) != replay_contract({key: result[key] for key in replay})):
                        raise UnsupportedFight('Delayed full native state/event/fixed-party replay diverged')
                    row = dict(case=name, setter=setter, seed=seed, passed=True, turns=machine.turn,
                        winner=fight.winner, comparisons=len(recording.rows), replay_equal=True,
                        witness=facts, evidence=str(evidence), evidence_sha256=sha(evidence),
                        initial_wram=str(wram), initial_wram_sha256=sha(wram),
                        initial_wram_boundary='post-reset-first-frozen-actor-decision',
                        initial_memory=str(raw), initial_memory_sha256=sha(raw))
                    rows.append(row)
                    print(json.dumps({key:value for key,value in row.items() if key != 'witness'}), flush=True)
    except Exception as error:
        (output/'failure.json').write_text(json.dumps(dict(passed=False, status='blocked',
            case=name, setter=setter, seed=seed, reason=str(error),
            completed_cases=[{k:v for k,v in row.items() if k != 'witness'} for row in rows],
            inputs=retained, oracle_provenance=provenance), indent=2)+'\n')
        raise
    finally:
        oracle.oracle.oracle_close()
    if any(sha(path) != digest for path, digest in immutable.items()):
        raise UnsupportedFight('Delayed source or executable input changed during proof')
    final_sources = host_sources()
    if any(final_sources.get(path) != digest for path, digest in sources.items()):
        raise UnsupportedFight('Delayed loaded host dependency changed during proof')
    retained.update(retain_inputs(output, {str((directory/path).resolve()): digest
        for path, digest in final_sources.items()}))
    report = dict(passed=True, scope='bounded-source-delayed-duration-residual-entry-full-fights',
        cases=rows, inputs=retained, oracle_provenance=provenance,
        host_sources=sources, game_sources=source_ids,
        python_version=sys.version, python_executable=sys.executable,
        generator_sha256=stateful_lifting.MODULE_DIGEST,
        rom_sha256=sha(machine.rom_path), symbols_sha256=sha(machine.symbols_path),
        manifest_sha256=sha(machine.build_dir/'manifest.json'),
        limits=['Declared presentation/input and ability graphic boundaries remain conditional.',
            'Only the eight listed isolated volatile/delayed source conditions are covered.',
            'No held-item, ability-interaction, caster-switch Future Sight or trap-move variant claims.',
            'No automatic central coverage promotion or M2 completion claim.'])
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
