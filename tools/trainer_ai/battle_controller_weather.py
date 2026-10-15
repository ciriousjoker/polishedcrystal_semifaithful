"""Bounded source weather duration, residual and entry-ability whole fights.

CODEPREP: import or execute only after the coordinator grants the CPU slot.
"""
import argparse
import copy
from dataclasses import dataclass
import hashlib
import json
import pathlib
import re
import sys

from battle_actions import SwitchAction
from battle_bag import item_record
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_coverage import lines
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Pokemon, Scenario
import stateful_lifting


@dataclass(frozen=True)
class WeatherCase:
    weather: str
    move: str
    species: str = 'HORSEA'
    slot: int = 0
    entry: bool = False
    item: str = 'NO_ITEM'
    duration: int = 5
    cloud_nine: bool = False


CASES = {
    'rain-move': WeatherCase('RAIN', 'RAIN_DANCE'),
    'sun-move': WeatherCase('SUN', 'SUNNY_DAY'),
    'sand-move': WeatherCase('SANDSTORM', 'SANDSTORM'),
    'hail-move': WeatherCase('HAIL', 'HAIL'),
    'drizzle': WeatherCase('RAIN', 'RAIN_DANCE', 'POLITOED', 2, True),
    'drought': WeatherCase('SUN', 'SUNNY_DAY', 'NINETALES', 2, True),
    'sand-stream': WeatherCase('SANDSTORM', 'SANDSTORM', 'TYRANITAR', 0, True),
    'snow-warning': WeatherCase('HAIL', 'HAIL', 'GLACEON', 1, True),
    'damp-rock': WeatherCase('RAIN', 'RAIN_DANCE', item='DAMP_ROCK', duration=8),
    'cloud-nine': WeatherCase('SANDSTORM', 'SANDSTORM', cloud_nine=True),
}
SPECIES_FILES = {
    'HORSEA': 'horsea', 'POLITOED': 'politoed', 'NINETALES': 'ninetales_plain',
    'TYRANITAR': 'tyranitar', 'GLACEON': 'glaceon', 'PSYDUCK': 'psyduck',
}
EXPECTED_ABILITIES = {
    'HORSEA': 'SWIFT_SWIM', 'POLITOED': 'DRIZZLE', 'NINETALES': 'DROUGHT',
    'TYRANITAR': 'SAND_STREAM', 'GLACEON': 'SNOW_WARNING', 'PSYDUCK': 'CLOUD_NINE',
}
SOURCE_PATHS = (
    'engine/battle/endturn.asm', 'engine/battle/abilities.asm',
    'engine/battle/effect_commands.asm', 'engine/battle/move_effects/weather.asm',
    'engine/battle/core.asm', 'home/battle.asm', 'data/pokemon/base_stats.asm',
    'constants/battle_constants.asm', 'constants/type_constants.asm',
    'constants/pokemon_constants.asm', 'constants/pokemon_data_constants.asm',
    'constants/ability_constants.asm', 'constants/item_constants.asm',
    'constants/move_constants.asm',
    'constants/item_data_constants.asm', 'data/items/attributes.asm',
) + tuple('data/pokemon/base_stats/'+name+'.asm' for name in SPECIES_FILES.values())
HOST_DEPENDENCIES = (
    'battle_controller_weather.py', 'battle_controller.py', 'battle_controller_profile.py',
    'battle_controller_fight.py', 'battle_controller_fight_check.py',
    'battle_controller_check.py', 'battle_controller_oracle.c', 'battle_decisions.py',
    'battle_scenarios.py', 'battle_bag.py', 'battle_actions.py', 'battle_coverage.py',
    'battle_env.py', 'battle_env_check.py', 'stateful_lifting.py',
    'stateful_fixture_check.py', 'battle_env_profile.py', 'battle_actions_profile.py',
    'battle_party.py', 'battle_party_profile.py',
    'battle_status.py', 'battle_status_profile.py', 'battle_party_probe.py',
)


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def host_sources():
    # Only actual project execution dependencies participate in this freeze.
    # Independent workers may continue writing unrelated drivers and collectors.
    directory = pathlib.Path(__file__).parent
    return {str((directory/name).resolve()): sha(directory/name) for name in HOST_DEPENDENCIES}


def retain_inputs(output, identities):
    records = {}
    destination = output/'inputs'
    destination.mkdir(parents=True, exist_ok=True)
    for source, digest in identities.items():
        path = pathlib.Path(source)
        target = destination/(digest+'-'+path.name)
        content = path.read_bytes()
        if hashlib.sha256(content).hexdigest() != digest:
            raise UnsupportedFight('Weather evidence input changed before snapshot: '+source)
        if not target.exists():
            target.write_bytes(content)
        if sha(target) != digest:
            raise UnsupportedFight('Weather evidence snapshot hash differs: '+source)
        records[source] = dict(sha256=digest, snapshot=str(target))
    return records


def identity_files(value):
    """Collect actual loaded Oracle/ABI/library paths from retained identities."""
    result = {}
    if isinstance(value, dict):
        if isinstance(value.get('path'), str) and isinstance(value.get('sha256'), str):
            path = pathlib.Path(value['path'])
            if not path.is_file() or sha(path) != value['sha256']:
                raise UnsupportedFight('Weather Oracle provenance identity differs from disk')
            result[str(path)] = value['sha256']
        for child in value.values():
            result.update(identity_files(child))
    elif isinstance(value, (tuple, list)):
        for child in value:
            result.update(identity_files(child))
    return result


def source_species(machine, species, slot):
    path = 'data/pokemon/base_stats/'+SPECIES_FILES[species]+'.asm'
    active = [code for _, code in lines(machine.source_root, path)]
    types = [code for code in active if re.fullmatch(r'db [A-Z_]+, [A-Z_]+', code)]
    abilities = [code for code in active if code.startswith('abilities_for ')]
    # Type declaration precedes the held-item pair in this source format.
    if not types or len(abilities) != 1:
        raise UnsupportedFight('Weather species source declarations require review')
    names = tuple(types[0][3:].split(', '))
    ability_names = tuple(abilities[0].split(' ', 1)[1].split(', ')[1:])
    expected_types = tuple(machine._constant('constants/type_constants.asm', value) for value in names)
    expected_ability = machine._constant('constants/ability_constants.asm', ability_names[slot])
    if ability_names[slot] != EXPECTED_ABILITIES[species]:
        raise UnsupportedFight('Weather fixture ability slot differs from active source')
    # Read compiled base data using source table membership and linked struct spans.
    table = (machine.source_root/'data/pokemon/base_stats.asm').read_text()
    includes = re.findall(r'^INCLUDE "(data/pokemon/base_stats/[^\"]+)"', table, re.M)
    index = includes.index(path)
    stride = machine.address('wCurBaseDataEnd')-machine.address('wCurBaseData')
    origin = machine.address('wCurBaseData')
    type_offset = machine.address('wBaseType1')-origin
    ability_offset = machine.address('wBaseAbility1')-origin
    data = machine._rom_bytes('BaseData', index*stride, stride)
    if (tuple(data[type_offset:type_offset+2]) != expected_types or
            data[ability_offset+slot] != expected_ability):
        raise UnsupportedFight('Weather compiled type/ability table differs from active source')
    return dict(species=species, slot=slot, source=path, table_index=index,
        compiled_record=data.hex(), types=expected_types, type_names=names,
        ability=expected_ability, ability_name=ability_names[slot])


def source_metadata(machine, case):
    roster = {'HORSEA': source_species(machine, 'HORSEA', 0)}
    if case.entry:
        roster[case.species] = source_species(machine, case.species, case.slot)
    if case.cloud_nine:
        roster['PSYDUCK'] = source_species(machine, 'PSYDUCK', 1)
    item = None
    if case.item != 'NO_ITEM':
        identifier, pocket, context = item_record(machine, case.item)
        record = machine._rom_bytes('ItemAttributes', (identifier-1)*6, 6)
        effect = machine._constant('constants/item_data_constants.asm', 'HELD_PROLONG_RAIN')
        if tuple(record[2:4]) != (effect, case.duration):
            raise UnsupportedFight('Damp Rock compiled effect/duration differs from source')
        item = dict(name=case.item, identifier=identifier, effect=effect,
            duration=record[3], pocket=pocket, context=context)
    return dict(roster=roster, item=item,
        weather=machine._constant('constants/battle_constants.asm', 'WEATHER_'+case.weather),
        move=machine._constant('constants/move_constants.asm', case.move))


def scenario(case, setter):
    neutral = Pokemon('HORSEA', moves=('SPLASH', 'WATER_GUN'), happiness=0)
    target = (Pokemon('PSYDUCK', ability_slot=1, moves=('SPLASH', 'WATER_GUN'), happiness=0)
              if case.cloud_nine else neutral)
    if case.entry:
        setting = (neutral, Pokemon(case.species, ability_slot=case.slot,
            moves=('SPLASH', 'WATER_GUN'), happiness=0))
    else:
        setting = (Pokemon('HORSEA', moves=('SPLASH', 'WATER_GUN', case.move),
            item=case.item, happiness=0),)
    teams = [None, None]
    teams[setter], teams[setter ^ 1] = setting, (target,)
    return Scenario(player=teams[0], enemy=teams[1], inventory=(None, None))


def choose(fight, case, setter):
    if fight.machine.turn < case.duration+1:
        actions = [MoveAction(0), MoveAction(0)]
        if fight.machine.turn == 0:
            actions[setter] = SwitchAction(1) if case.entry else MoveAction(2)
        return tuple(actions)
    actions = []
    for side in (0, 1):
        mask = fight.legal_actions(side)
        wanted = MoveAction(1)
        actions.append(wanted if wanted in mask else first_attack(side, fight.observation(side), mask))
    return tuple(actions)


def storage(machine):
    rows = []
    for side in (0, 1):
        index = machine.byte(('wCurBattleMon', 'wCurOTMon')[side])
        party = ('wPartyMon', 'wOTPartyMon')[side]+str(index+1)
        battle = ('wBattleMon', 'wEnemyMon')[side]
        caches = {}
        for name, stem in (('party', party), ('battle', battle)):
            caches[name] = dict(HP=machine.word(stem+'HP'), status=machine.byte(stem+'Status'),
                item=machine.byte(stem+'Item'),
                pp=tuple(machine.state.m[machine.address(stem+'PP')+i] & 63 for i in range(4)))
        rows.append(dict(side=side, index=index, caches=caches))
    return rows


def probe(fight, case, setter):
    transitions, caches = [], []
    for _ in range(case.duration+1):
        if fight.terminal or fight.truncated:
            raise UnsupportedFight('Weather fight terminated before expiry and clear-round witness')
        transitions.append(fight.step(choose(fight, case, setter)))
        caches.append(storage(fight.machine))
    return transitions, caches


def witness(machine, case, setter, metadata, initial, transitions, caches):
    if any(obs['weather'] or obs['weather_turns'] for obs in initial):
        raise UnsupportedFight('Weather probe did not begin with clear source weather')
    facts = []
    previous = initial
    for round_index, transition in enumerate(transitions):
        remaining = max(0, case.duration-round_index-1)
        weather = metadata['weather'] if remaining else 0
        if any(obs['weather'] != weather or obs['weather_turns'] != remaining
               for obs in transition.observations):
            raise UnsupportedFight('Weather source counter/expiry differs from decrement-before-effects order')
        for side in (0, 1):
            action = transition.actions[side]
            if round_index == 0 and side == setter:
                if case.entry:
                    correct = type(action) is SwitchAction and action.index == 1
                else:
                    correct = type(action) is MoveAction and action.slot == 2
            else:
                correct = type(action) is MoveAction and action.slot == 0
            if not correct:
                raise UnsupportedFight('Weather isolation probe used an unexpected submitted action')
            before_team = previous[side]['teams'][side]
            after_team = transition.observations[side]['teams'][side]
            for index, after in enumerate(after_team):
                before = before_team[index]
                expected_active = index == (1 if case.entry and side == setter else 0)
                species = case.species if case.entry and side == setter and index == 1 else (
                    'PSYDUCK' if case.cloud_nine and side != setter else 'HORSEA')
                record = metadata['roster'][species]
                if after['active'] != expected_active or after['ability'] != record['ability']:
                    raise UnsupportedFight('Weather fixture active identity or ability differs')
                if expected_active and tuple(after['types']) != record['types']:
                    raise UnsupportedFight('Weather fixture active source types differ')
                damaging = case.weather in ('SANDSTORM', 'HAIL') and remaining and not case.cloud_nine
                immune_types = ('GROUND', 'ROCK', 'STEEL') if case.weather == 'SANDSTORM' else ('ICE',)
                immune = any(value in record['type_names'] for value in immune_types)
                damage = max(1, before['MaxHP']//16) if damaging and expected_active and not immune else 0
                if after['HP'] != before['HP']-damage or before['status'] or after['status']:
                    raise UnsupportedFight('Weather residual amount, type immunity or no-damage control differs')
                if any(after[field] != before[field] for field in ('moves', 'pp_ups', 'item')):
                    raise UnsupportedFight('Weather interval changed held item, moves or PP Ups')
                pp = list(before['pp'])
                # The newly switched entrant spends no move PP on its entry round.
                if expected_active and not (round_index == 0 and case.entry and side == setter):
                    pp[2 if round_index == 0 and side == setter else 0] -= 1
                if tuple(after['pp']) != tuple(pp):
                    raise UnsupportedFight('Weather probe source move PP consumption differs')
                if expected_active:
                    for cache in caches[round_index][side]['caches'].values():
                        if any(cache[field] != after[field] for field in ('HP', 'status', 'item', 'pp')):
                            raise UnsupportedFight('Weather party/active residual caches differ')
                facts.append(dict(round=round_index, turn=transition.turn, side=side, index=index,
                    weather=weather, weather_turns=remaining, damage=damage, type_immune=immune,
                    before=before, after=after, stored=caches[round_index][side] if expected_active else None))
        previous = transition.observations
    if case.weather in ('SANDSTORM', 'HAIL') and not case.cloud_nine:
        if not any(row['damage'] > 0 for row in facts):
            raise UnsupportedFight('Weather residual proof lacks positive damage')
        if case.entry and not any(row['type_immune'] and row['after']['active'] and row['weather'] for row in facts):
            raise UnsupportedFight('Entry weather proof lacks its active Rock/Ice immunity witness')
    return dict(scope='source-weather-creation-through-expiry-and-clear-round', setter=setter,
        metadata=metadata, duration=case.duration, damage_ticks=case.duration-1,
        cloud_nine_effects_suppressed=case.cloud_nine, probes=facts)


def initial_memory(machine):
    return bytes(machine.state.m[0:0x6000])+bytes(machine.state.m[0xc000:0xe000])+bytes(
        machine.state.m[0xff80:0xffff])+bytes(machine.state.m[0xff70:0xff71])


def run(root, output, cases, sides, seeds, native_build=None, optimization='O3'):
    if not 1 <= len(cases)*len(sides)*len(seeds) <= 20:
        raise UnsupportedFight('Weather invocation must contain one through twenty fights')
    if len(set(cases)) != len(cases) or len(set(sides)) != len(sides) or len(set(seeds)) != len(seeds):
        raise UnsupportedFight('Duplicate weather case keys are not independent evidence')
    output.mkdir(parents=True, exist_ok=True)
    game = root/'build/stateful/game'
    machine = ControllerMachine(game/'polishedcrystal-3.2.3.gbc', game/'polishedcrystal-3.2.3.sym',
        game, native_build or root/'build/battle-controller/native-o3', optimization=optimization)
    oracle = Oracle(root, machine)
    if not oracle.banked_wram or not isinstance(oracle.provenance, dict):
        raise UnsupportedFight('Weather requires retained all-bank independent Oracle provenance')
    provenance = copy.deepcopy(oracle.provenance)
    if 'libmgba' not in provenance or 'libmgba_identity_error' in provenance:
        raise UnsupportedFight('Weather requires the actual loaded mGBA dependency identity')
    sources = host_sources()
    source_ids = {str(game/path): sha(game/path) for path in SOURCE_PATHS}
    project_sources = {pathlib.Path(path).name: digest for path, digest in sources.items()}
    game_sources = {str(pathlib.Path(path).relative_to(game)): digest for path, digest in source_ids.items()}
    immutable = {**sources, **source_ids, str(machine.rom_path): sha(machine.rom_path),
        str(machine.symbols_path): sha(machine.symbols_path),
        str(machine.build_dir/'manifest.json'): sha(machine.build_dir/'manifest.json')}
    immutable.update(identity_files(provenance))
    manifest = json.loads((machine.build_dir/'manifest.json').read_text())
    native_library = pathlib.Path(machine.lib._name).resolve()
    if sha(native_library) != manifest['library_sha256']:
        raise UnsupportedFight('Weather loaded native library differs from retained manifest')
    immutable[str(native_library)] = sha(native_library)
    generated = machine.build_dir/'stateful.c'
    if not generated.is_file():
        raise UnsupportedFight('Weather native generated C is unavailable for retention')
    if sha(generated) != manifest['generated_c_sha256']:
        raise UnsupportedFight('Weather native generated C differs from retained manifest')
    immutable[str(generated)] = sha(generated)
    receipt = root/'build/battle-coverage/original-build-receipt.json'
    if receipt.exists():
        build_receipt = json.loads(receipt.read_text())
        if (build_receipt.get('rom_sha256') != sha(machine.rom_path) or
                build_receipt.get('symbols_sha256') != sha(machine.symbols_path)):
            raise UnsupportedFight('Weather source assembler receipt differs from running ROM/symbols')
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
                    initial_normalized_state = recording.digest(machine)
                    raw = output/(tag+'.initial-memory.bin')
                    raw.write_bytes(initial_memory(machine))
                    initial_wram = output/(tag+'.initial-wram.bin')
                    initial_wram.write_bytes(bytes(machine.state.m[0xc000:0xe000]))
                    transitions, caches = probe(fight, case, setter)
                    facts = witness(machine, case, setter, metadata, initial, transitions, caches)
                    while not fight.terminal and not fight.truncated:
                        fight.step(choose(fight, case, setter))
                    if not fight.terminal or fight.truncated or machine.failed:
                        raise UnsupportedFight('Weather fight failed its real source terminal')
                    result = fight.replay_record()
                    result.update(case=name, setter=setter, seed=seed, witness=facts,
                        inputs=retained, oracle_provenance=provenance,
                        host_sources=project_sources, game_sources=game_sources,
                        initial_wram=str(initial_wram), initial_wram_sha256=sha(initial_wram),
                        initial_wram_boundary='post-reset-first-frozen-actor-decision',
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
                        raise UnsupportedFight('Weather all-bank/HRAM/SVBK initial memory replay diverged')
                    if recording.digest(machine) != initial_normalized_state:
                        raise UnsupportedFight('Weather normalized initial CPU/event state replay diverged')
                    replay_probes, replay_caches = probe(fight, case, setter)
                    if witness(machine, case, setter, metadata, replay_initial, replay_probes, replay_caches) != facts:
                        raise UnsupportedFight('Weather replay probe state diverged')
                    while not fight.terminal and not fight.truncated:
                        fight.step(choose(fight, case, setter))
                    replay = fight.replay_record()
                    if (not fight.terminal or fight.truncated or machine.failed or
                            recording.position != len(recording.rows) or
                            {key:value for key,value in replay.items() if key != 'initial_state_sha256'} != {
                                key: result[key] for key in replay if key != 'initial_state_sha256'}):
                        raise UnsupportedFight('Weather full native state/event/fixed-party replay diverged')
                    row = dict(case=name, setter=setter, seed=seed, passed=True, turns=machine.turn,
                        winner=fight.winner, comparisons=len(recording.rows), replay_equal=True,
                        witness=facts, evidence=str(evidence), evidence_sha256=sha(evidence),
                        initial_wram=str(initial_wram), initial_wram_sha256=sha(initial_wram),
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
        raise UnsupportedFight('Weather source or executable input changed during proof')
    final_sources = host_sources()
    if any(final_sources.get(path) != digest for path, digest in sources.items()):
        raise UnsupportedFight('Weather loaded host dependency changed during proof')
    retained.update(retain_inputs(output, final_sources))
    report = dict(passed=True, scope='bounded-source-weather-duration-residual-entry-full-fights',
        cases=rows, inputs=retained, oracle_provenance=provenance,
        host_sources=project_sources, game_sources=game_sources,
        python_version=sys.version, python_executable=sys.executable,
        generator_sha256=stateful_lifting.MODULE_DIGEST,
        rom_sha256=sha(machine.rom_path), symbols_sha256=sha(machine.symbols_path),
        manifest_sha256=sha(machine.build_dir/'manifest.json'),
        limits=['Declared presentation/input and ability graphic boundaries remain conditional.',
            'Only listed weather creation, durations, damage, type immunity and Cloud Nine control are covered.',
            'No damage modifiers, weather ability stat modifiers, permaweather or weather replacement claims.',
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
