"""Bounded actual-source enemy trainer inventory whole fights.

CODEPREP: imports and execution require the coordinator's exclusive CPU grant.
"""
import argparse
import copy
import hashlib
import json
import pathlib
import re
import sys

from battle_actions import ItemAction
from battle_bag import item_record
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight, TRAINER_EFFECTS, encode_action
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Pokemon, Scenario
import stateful_lifting


CASES = ('POTION', 'SUPER_POTION', 'HYPER_POTION', 'MAX_POTION', 'FULL_HEAL',
         'FULL_RESTORE', 'GUARD_SPEC', 'DIRE_HIT', 'X_ATTACK', 'X_DEFEND',
         'X_SPEED', 'X_SP_ATK', 'X_SP_DEF', 'X_ACCURACY')
HEALS = ('POTION', 'SUPER_POTION', 'HYPER_POTION', 'MAX_POTION', 'FULL_RESTORE')
STATS = ('ATTACK', 'DEFENSE', 'SPEED', 'SP_ATTACK', 'SP_DEFENSE', 'ACCURACY')
MANIFEST_SHA256 = 'dec3cb17a34aeeb9e0f36e44590d0890b7d0816ddff98fc7bb0346b9b32737b0'
HOST_DEPENDENCIES = (
    'battle_controller_trainer_items.py', 'battle_controller.py', 'battle_controller_profile.py',
    'battle_controller_fight.py', 'battle_controller_fight_check.py',
    'battle_controller_check.py', 'battle_controller_oracle.c', 'battle_decisions.py',
    'battle_scenarios.py', 'battle_bag.py', 'battle_actions.py', 'battle_coverage.py',
    'battle_env.py', 'battle_env_check.py', 'stateful_lifting.py', 'stateful_fixture_check.py',
    'battle_actions_profile.py', 'battle_env_profile.py', 'battle_party.py',
    'battle_party_profile.py', 'battle_party_probe.py', 'battle_status.py', 'battle_status_profile.py',
)
SOURCE_PATHS = (
    'engine/battle/ai/items.asm', 'engine/battle/stats.asm', 'engine/battle/endturn.asm',
    'engine/battle/effect_commands.asm', 'engine/battle/core.asm', 'home/battle.asm',
    'engine/items/item_effects.asm', 'data/items/attributes.asm',
    'constants/item_constants.asm', 'constants/item_data_constants.asm',
    'constants/battle_constants.asm', 'constants/pokemon_constants.asm',
    'constants/pokemon_data_constants.asm', 'constants/ability_constants.asm',
    'constants/type_constants.asm', 'constants/move_constants.asm',
    'data/pokemon/base_stats.asm', 'data/pokemon/base_stats/horsea.asm',
)


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def host_sources():
    directory = pathlib.Path(__file__).parent.resolve()
    for module in tuple(sys.modules.values()):
        path = getattr(module, '__file__', None)
        if not path:
            continue
        path = pathlib.Path(path).resolve()
        if path.parent == directory and path.name not in HOST_DEPENDENCIES:
            raise UnsupportedFight('Trainer item loaded project module outside host allowlist: '+str(path))
    for name in HOST_DEPENDENCIES:
        if name.endswith('.py'):
            module = sys.modules.get(__name__ if name == pathlib.Path(__file__).name else name[:-3])
            if module is not None and pathlib.Path(module.__file__).resolve() != directory/name:
                raise UnsupportedFight('Trainer item loaded host module from unexpected path: '+name)
    return {name: sha(directory/name) for name in HOST_DEPENDENCIES}


def retain_inputs(output, identities):
    # All batches share content-addressed large native/source snapshots.
    destination = output.parent/'trainer-item-inputs'
    destination.mkdir(parents=True, exist_ok=True)
    result = {}
    for source, digest in identities.items():
        path = pathlib.Path(source)
        content = path.read_bytes()
        if hashlib.sha256(content).hexdigest() != digest:
            raise UnsupportedFight('Trainer item input changed before retention: '+source)
        target = destination/(digest+'-'+path.name)
        if not target.exists():
            target.write_bytes(content)
        if sha(target) != digest:
            raise UnsupportedFight('Trainer item retained input differs: '+source)
        result[source] = dict(sha256=digest, snapshot=str(target))
    return result


def identity_files(value):
    result = {}
    if isinstance(value, dict):
        if isinstance(value.get('path'), str) and isinstance(value.get('sha256'), str):
            path = pathlib.Path(value['path'])
            if not path.is_file() or sha(path) != value['sha256']:
                raise UnsupportedFight('Trainer item actual loaded Oracle identity differs')
            result[str(path)] = value['sha256']
        for child in value.values():
            result.update(identity_files(child))
    elif isinstance(value, (tuple, list)):
        for child in value:
            result.update(identity_files(child))
    return result


def source_metadata(machine, name):
    if set(TRAINER_EFFECTS) != set(CASES):
        raise UnsupportedFight('Trainer item source handler family changed')
    identifier, pocket, context = item_record(machine, name)
    source = (machine.source_root/'engine/battle/ai/items.asm').read_text()
    handler = TRAINER_EFFECTS[name]
    record = machine._rom_bytes('ItemAttributes', (identifier-1)*6, 6)
    # Item names annotate one source record in table order. Never treat the
    # player-bag parameter as the enemy potion handler's healing amount.
    attributes = (machine.source_root/'data/items/attributes.asm').read_text()
    rows = re.findall(r'^; ([^\n]+)\n\s*item_attribute ([^\n;]+)', attributes, re.M)
    if len(rows) <= identifier-1 or rows[identifier-1][0] != name.replace('_', ' ')+('.' if name == 'GUARD_SPEC' else ''):
        raise UnsupportedFight('Trainer item source attribute table membership differs')
    parameter = rows[identifier-1][1].split(',')[2].strip()
    metadata = dict(item=name, identifier=identifier, handler=handler,
        compiled_attributes=record.hex(), pocket=pocket, context=context,
        source_attribute_parameter=parameter)
    if name in ('POTION', 'SUPER_POTION', 'HYPER_POTION'):
        match = re.search(r'^'+handler+r':\n\s*ld a, '+name+r'\n\s*ld b, (\d+)\s*$', source, re.M)
        if match is None:
            raise UnsupportedFight('Trainer potion literal source scaffold changed')
        amount = int(match[1])
        if machine._rom_bytes(handler, 0, 4) != bytes((0x3e, identifier, 0x06, amount)):
            raise UnsupportedFight('Trainer potion compiled handler differs from literal source')
        metadata['heal_amount'] = amount
    elif name.startswith('X_'):
        match = re.fullmatch(r'\$([0-9a-fA-F]+) \| ([A-Z_]+)', parameter)
        if match is None or match[2] not in STATS:
            raise UnsupportedFight('Trainer X item stat parameter requires source review')
        stat = machine._constant('constants/battle_constants.asm', match[2])
        amount = int(match[1], 16)
        if amount & 15 or not 0 <= stat < 6 or record[3] != (amount | stat):
            raise UnsupportedFight('Trainer X item compiled parameter differs from source')
        # DoChangeStat decodes the high nibble plus one, so $10 means +2.
        stats = (machine.source_root/'engine/battle/stats.asm').read_text()
        scaffold = ['ld a, [wChangedStat]', 'and $f0', 'swap a', 'inc a', 'ld c, a']
        code = [line.split(';', 1)[0].strip() for line in stats.splitlines()]
        code = [line for line in code if line]
        if sum(code[i:i+len(scaffold)] == scaffold for i in range(len(code)-len(scaffold)+1)) != 1:
            raise UnsupportedFight('Trainer X item source stage amount decoding changed')
        metadata.update(stat=stat, stat_name=match[2], stage_increment=(amount >> 4)+1)
    elif name == 'GUARD_SPEC':
        match = re.search(r'^EnemyUsedGuardSpec:\n\s*call AIUsedItemSound\n\s*ld a, (\d+) << 4', source, re.M)
        if match is None:
            raise UnsupportedFight('Trainer Guard Spec source scaffold changed')
        duration = int(match[1])
        if machine._rom_bytes(handler, 3, 2) != bytes((0x3e, duration << 4)):
            raise UnsupportedFight('Trainer Guard Spec compiled duration differs')
        metadata['duration'] = duration
    elif name == 'DIRE_HIT':
        bit = machine._constant('constants/battle_constants.asm', 'SUBSTATUS_FOCUS_ENERGY')
        address = machine.address('wEnemySubStatus4')
        if (not re.search(r'^EnemyUsedDireHit:\n\s*call AIUsedItemSound\n\s*ld hl, wEnemySubStatus4\n\s*set SUBSTATUS_FOCUS_ENERGY, \[hl\]', source, re.M)
                or machine._rom_bytes(handler, 3, 5) != bytes((0x21, address & 255, address >> 8, 0xcb, 0xc6+8*bit))):
            raise UnsupportedFight('Trainer Dire Hit compiled focus-energy bit differs')
    # Bind neutral species and ability to the assembled base-data table.
    path = 'data/pokemon/base_stats/horsea.asm'
    species = (machine.source_root/path).read_text()
    if not re.search(r'^\s*abilities_for HORSEA, SWIFT_SWIM, SNIPER, DAMP\s*$', species, re.M):
        raise UnsupportedFight('Neutral Horsea ability declarations changed')
    includes = re.findall(r'^INCLUDE "(data/pokemon/base_stats/[^\"]+)"',
        (machine.source_root/'data/pokemon/base_stats.asm').read_text(), re.M)
    stride = machine.address('wCurBaseDataEnd')-machine.address('wCurBaseData')
    base = machine._rom_bytes('BaseData', includes.index(path)*stride, stride)
    offset = machine.address('wBaseAbility1')-machine.address('wCurBaseData')
    ability = machine._constant('constants/ability_constants.asm', 'SWIFT_SWIM')
    if base[offset] != ability:
        raise UnsupportedFight('Neutral Horsea compiled ability differs')
    metadata['ability'] = ability
    metadata['status'] = (1 << machine._constant('constants/battle_constants.asm', 'PAR')
        if name in ('FULL_HEAL', 'FULL_RESTORE') else 0)
    return metadata


def scenario(name, slot, metadata):
    neutral = Pokemon('HORSEA', moves=('SPLASH', 'WATER_GUN'), happiness=0)
    target = Pokemon('HORSEA', moves=('SPLASH', 'WATER_GUN'), happiness=0,
        hp=1 if name in HEALS else None, status=metadata['status'])
    inventory = ['DIRE_HIT', 'DIRE_HIT']
    inventory[slot] = name
    return Scenario(player=(neutral,), enemy=(target,), inventory=tuple(inventory))


def rounds(name):
    return 6 if name == 'GUARD_SPEC' else 1


def choose(fight, name, slot):
    if fight.machine.turn == 0:
        return MoveAction(0), ItemAction(slot)
    if fight.machine.turn < rounds(name):
        return MoveAction(0), MoveAction(0)
    actions = []
    for side in (0, 1):
        mask = fight.legal_actions(side)
        wanted = MoveAction(1)
        actions.append(wanted if wanted in mask else first_attack(side, fight.observation(side), mask))
    return tuple(actions)


def storage(machine):
    result = []
    for side in (0, 1):
        index = machine.byte(('wCurBattleMon', 'wCurOTMon')[side])
        caches = {}
        for kind, stem in (('party', ('wPartyMon', 'wOTPartyMon')[side]+str(index+1)),
                           ('battle', ('wBattleMon', 'wEnemyMon')[side])):
            caches[kind] = dict(HP=machine.word(stem+'HP'), status=machine.byte(stem+'Status'),
                item=machine.byte(stem+'Item'), MaxHP=machine.word(stem+'MaxHP'),
                pp=tuple(machine.state.m[machine.address(stem+'PP')+i] & 63 for i in range(4)))
        result.append(dict(side=side, index=index, caches=caches))
    return result


def probe(fight, name, slot):
    transitions, caches = [], []
    for _ in range(rounds(name)):
        if fight.terminal or fight.truncated:
            raise UnsupportedFight('Trainer item fight ended before its positive probe')
        transitions.append(fight.step(choose(fight, name, slot)))
        caches.append(storage(fight.machine))
    return transitions, caches


def witness(machine, name, slot, metadata, initial, transitions, caches):
    if any(len(team) != 1 for team in initial[1]['teams']):
        raise UnsupportedFight('Trainer item fixture roster differs')
    if any(initial[side]['battlefield'][side]['stages'] != (7,)*7
            or initial[side]['battlefield'][side]['volatile']['focus_energy']
            or initial[side]['battlefield'][side]['mist_turns'] for side in (0, 1)):
        raise UnsupportedFight('Trainer item probe did not start with neutral stages/flags')
    if initial[1]['teams'][1][0]['status'] != metadata['status']:
        raise UnsupportedFight('Trainer item initial status differs')
    facts, previous = [], initial
    for index, transition in enumerate(transitions):
        expected_actions = (MoveAction(0), ItemAction(slot) if index == 0 else MoveAction(0))
        if tuple(transition.actions) != expected_actions:
            raise UnsupportedFight('Trainer item probe typed submitted action differs')
        for side in (0, 1):
            before = previous[side]['teams'][side][0]
            after = transition.observations[side]['teams'][side][0]
            field_before = previous[side]['battlefield'][side]
            field_after = transition.observations[side]['battlefield'][side]
            expected_hp = before['HP']
            if side == 1 and index == 0 and name in HEALS:
                expected_hp = (min(before['MaxHP'], before['HP']+metadata['heal_amount'])
                    if 'heal_amount' in metadata else before['MaxHP'])
                if expected_hp <= before['HP']:
                    raise UnsupportedFight('Trainer healing witness lacks positive HP restoration')
            expected_status = 0 if side == 1 and index == 0 and name in ('FULL_HEAL', 'FULL_RESTORE') else before['status']
            if after['HP'] != expected_hp or after['status'] != expected_status:
                raise UnsupportedFight('Trainer item actual HP/status effect differs')
            expected_pp = list(before['pp'])
            if side == 0 or index != 0:
                expected_pp[0] -= 1
            if tuple(after['pp']) != tuple(expected_pp):
                raise UnsupportedFight('Trainer item own no-PP turn or opposing Splash PP differs')
            stages = list(field_before['stages'])
            if side == 1 and index == 0 and name.startswith('X_'):
                stages[metadata['stat']] += metadata['stage_increment']
            if tuple(field_after['stages']) != tuple(stages):
                raise UnsupportedFight('Trainer item target/other-side source stage effect differs')
            focus = field_before['volatile']['focus_energy'] or (side == 1 and name == 'DIRE_HIT')
            mist = max(0, metadata['duration']-index-1) if side == 1 and name == 'GUARD_SPEC' else 0
            if field_after['volatile']['focus_energy'] != focus or field_after['mist_turns'] != mist:
                raise UnsupportedFight('Trainer item focus flag or end-turn Mist decay differs')
            if (not after['active'] or after['level'] != 50 or after['ability'] != metadata['ability']
                    or any(after[key] != before[key] for key in ('species', 'moves', 'pp_ups', 'item', 'MaxHP'))):
                raise UnsupportedFight('Trainer item neutral roster identity changed')
            for cache in caches[index][side]['caches'].values():
                if any(cache[key] != after[key] for key in ('HP', 'status', 'item', 'MaxHP', 'pp')):
                    raise UnsupportedFight('Trainer item source party/active cache differs')
            facts.append(dict(round=index, side=side, before=before, after=after,
                stages_before=field_before['stages'], stages_after=field_after['stages'],
                focus_energy=focus, mist_turns=mist, stored=caches[index][side]))
        expected_inventory = list(previous[1]['trainer_items'])
        if index == 0:
            if expected_inventory[slot] != metadata['identifier']:
                raise UnsupportedFight('Trainer item selected slot does not contain source item')
            expected_inventory[slot] = 0
        if tuple(transition.observations[1]['trainer_items']) != tuple(expected_inventory):
            raise UnsupportedFight('Trainer item selected consumption/unselected slot preservation differs')
        previous = transition.observations
    return dict(scope='actual-enemy-trainer-item-positive-effect-and-complete-fight',
        item=name, slot=slot, enemy_side=1, metadata=metadata,
        inventory_before=initial[1]['trainer_items'], inventory_after=previous[1]['trainer_items'], probes=facts)


def initial_memory(machine):
    return bytes(machine.state.m[0:0x6000])+bytes(machine.state.m[0xc000:0xe000])+bytes(
        machine.state.m[0xff80:0xffff])+bytes(machine.state.m[0xff70:0xff71])


def replay_contract(record):
    # Compare every normalized CPU/event/memory segment via RecordedRun. The
    # unused event-tape transport tail alone is omitted from raw turn hashes.
    result = copy.deepcopy(record)
    result.pop('initial_state_sha256', None)
    for turn in result['turns']:
        turn.pop('state_sha256', None)
    return result


def run(root, output, cases, slots, seeds, freeze_ledger, native_build=None, optimization='O3'):
    if not 1 <= len(cases)*len(slots)*len(seeds) <= 28:
        raise UnsupportedFight('Trainer item invocation must contain one through twenty-eight fights')
    if len(set(cases)) != len(cases) or len(set(slots)) != len(slots) or len(set(seeds)) != len(seeds):
        raise UnsupportedFight('Duplicate trainer-item case keys are not independent evidence')
    if set(cases)-set(CASES) or any(type(slot) is not int or slot not in (0, 1) for slot in slots):
        raise UnsupportedFight('Trainer item case or inventory slot is outside the declared family')
    if output.exists() and any(output.iterdir()):
        raise UnsupportedFight('Trainer item output must be a fresh evidence directory')
    output.mkdir(parents=True, exist_ok=True)
    game = root/'build/stateful/game'
    directory = pathlib.Path(__file__).parent
    sources = host_sources()
    source_ids = {path: sha(game/path) for path in SOURCE_PATHS}
    frozen = json.loads(freeze_ledger.read_text())
    build = native_build or root/'build/battle-controller/native-o3'
    if (frozen.get('host_sources') != sources or frozen.get('game_sources') != source_ids
            or frozen.get('manifest_sha256') != MANIFEST_SHA256
            or sha(build/'manifest.json') != MANIFEST_SHA256
            or sha(build/'controller-profile.json') != frozen.get('controller_profile_sha256')):
        raise UnsupportedFight('Trainer item prepared source/host/manifest freeze changed')
    oracle_source = directory/'battle_controller_oracle.c'
    oracle_library = root/'build/battle-controller'/('controller-oracle-'+sha(oracle_source)[:16]+'.dylib')
    if not oracle_library.is_file():
        raise UnsupportedFight('Trainer item existing Oracle library absent; no implicit build allowed')
    machine = ControllerMachine(game/'polishedcrystal-3.2.3.gbc', game/'polishedcrystal-3.2.3.sym',
        game, native_build or root/'build/battle-controller/native-o3', optimization=optimization)
    oracle = Oracle(root, machine)
    if not oracle.banked_wram or not isinstance(oracle.provenance, dict):
        raise UnsupportedFight('Trainer item requires retained all-bank independent Oracle provenance')
    provenance = copy.deepcopy(oracle.provenance)
    if 'libmgba' not in provenance or 'libmgba_identity_error' in provenance:
        raise UnsupportedFight('Trainer item requires the actual loaded mGBA dependency identity')
    immutable = {**{str((directory/path).resolve()): digest for path, digest in sources.items()},
        **{str(game/path): digest for path, digest in source_ids.items()},
        str(machine.rom_path): sha(machine.rom_path),
        str(machine.symbols_path): sha(machine.symbols_path),
        str(machine.build_dir/'manifest.json'): sha(machine.build_dir/'manifest.json'),
        str(machine.build_dir/'controller-profile.json'): sha(machine.build_dir/'controller-profile.json'),
        str(freeze_ledger.resolve()): sha(freeze_ledger)}
    immutable.update(identity_files(provenance))
    manifest = json.loads((machine.build_dir/'manifest.json').read_text())
    native_library = pathlib.Path(machine.lib._name).resolve()
    if sha(native_library) != manifest['library_sha256']:
        raise UnsupportedFight('Trainer item loaded native library differs from retained manifest')
    immutable[str(native_library)] = sha(native_library)
    generated = machine.build_dir/'stateful.c'
    if not generated.is_file():
        raise UnsupportedFight('Trainer item native generated C is unavailable for retention')
    if sha(generated) != manifest['generated_c_sha256']:
        raise UnsupportedFight('Trainer item native generated C differs from retained manifest')
    immutable[str(generated)] = sha(generated)
    receipt = root/'build/battle-coverage/original-build-receipt.json'
    if not receipt.is_file():
        raise UnsupportedFight('Trainer item assembler receipt is required')
    if receipt.exists():
        build_receipt = json.loads(receipt.read_text())
        if (build_receipt.get('rom_sha256') != sha(machine.rom_path) or
                build_receipt.get('symbols_sha256') != sha(machine.symbols_path)):
            raise UnsupportedFight('Trainer item source assembler receipt differs from running ROM/symbols')
        immutable[str(receipt)] = sha(receipt)
        build_log = root/build_receipt['build_log']
        if sha(build_log) != build_receipt['build_log_sha256']:
            raise UnsupportedFight('Trainer item original assembler log differs from receipt')
        immutable[str(build_log)] = sha(build_log)
    retained = retain_inputs(output, immutable)
    shared_snapshot_root = str((output.parent/'trainer-item-inputs').resolve())
    rows = []
    name = slot = seed = None
    try:
        for name in cases:
            metadata = source_metadata(machine, name)
            for slot in slots:
                for seed in seeds:
                    tag = f'{name}-slot{slot}-{seed}'
                    base = scenario(name, slot, metadata)
                    machine.segment_checker, machine.single_step_checker = oracle.compare, oracle.single_step
                    recording = RecordedRun(oracle)
                    def observe(stage, record):
                        if stage == 'failure':
                            (output/(tag+'.failure.json')).write_text(json.dumps(dict(
                                passed=False, case=name, slot=slot, seed=seed, diagnostic=record,
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
                    transitions, caches = probe(fight, name, slot)
                    (output/(tag+'.probe.json')).write_text(json.dumps(dict(initial=initial,
                        transitions=[dict(turn=row.turn, actions=[encode_action(action) for action in row.actions],
                            observations=row.observations) for row in transitions],
                        caches=caches,
                        host_sources=sources, game_sources=source_ids, oracle_provenance=provenance,
                        inputs=retained), indent=2)+'\n')
                    facts = witness(machine, name, slot, metadata, initial, transitions, caches)
                    while not fight.terminal and not fight.truncated:
                        fight.step(choose(fight, name, slot))
                    if not fight.terminal or fight.truncated or machine.failed:
                        raise UnsupportedFight('Trainer item fight failed its real source terminal')
                    result = fight.replay_record()
                    result.update(case=name, slot=slot, enemy_side=1, seed=seed, witness=facts,
                        initial_contract=initial_contract, shared_snapshot_root=shared_snapshot_root,
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
                        raise UnsupportedFight('Trainer item all-bank/HRAM/SVBK initial memory replay diverged')
                    if bytes(machine.state.m[0xc000:0xe000]) != wram.read_bytes():
                        raise UnsupportedFight('Trainer item standalone initial WRAM replay diverged')
                    if recording.digest(machine) != initial_normalized_state:
                        raise UnsupportedFight('Trainer item normalized initial CPU/event state replay diverged')
                    replay_probes, replay_caches = probe(fight, name, slot)
                    if witness(machine, name, slot, metadata, replay_initial, replay_probes, replay_caches) != facts:
                        raise UnsupportedFight('Trainer item replay probe state diverged')
                    while not fight.terminal and not fight.truncated:
                        fight.step(choose(fight, name, slot))
                    replay = fight.replay_record()
                    if (not fight.terminal or fight.truncated or machine.failed or
                            recording.position != len(recording.rows) or
                            replay_contract(replay) != replay_contract({key: result[key] for key in replay})):
                        raise UnsupportedFight('Trainer item full native state/event/fixed-party replay diverged')
                    row = dict(case=name, slot=slot, enemy_side=1, seed=seed, passed=True, turns=machine.turn,
                        winner=fight.winner, comparisons=len(recording.rows), replay_equal=True,
                        witness=facts, evidence=str(evidence), evidence_sha256=sha(evidence),
                        initial_wram=str(wram), initial_wram_sha256=sha(wram),
                        initial_wram_boundary='post-reset-first-frozen-actor-decision',
                        initial_memory=str(raw), initial_memory_sha256=sha(raw))
                    rows.append(row)
                    print(json.dumps({key:value for key,value in row.items() if key != 'witness'}), flush=True)
    except Exception as error:
        (output/'failure.json').write_text(json.dumps(dict(passed=False, status='blocked',
            case=name, slot=slot, seed=seed, reason=str(error),
            completed_cases=[{k:v for k,v in row.items() if k != 'witness'} for row in rows],
            inputs=retained, oracle_provenance=provenance), indent=2)+'\n')
        raise
    finally:
        oracle.oracle.oracle_close()
    if any(sha(path) != digest for path, digest in immutable.items()):
        raise UnsupportedFight('Trainer item source or executable input changed during proof')
    final_sources = host_sources()
    if any(final_sources.get(path) != digest for path, digest in sources.items()):
        raise UnsupportedFight('Trainer item loaded host dependency changed during proof')
    retained.update(retain_inputs(output, {str((directory/path).resolve()): digest
        for path, digest in final_sources.items()}))
    report = dict(passed=True, scope='bounded-source-enemy-trainer-inventory-full-fights',
        cases=rows, inputs=retained, oracle_provenance=provenance,
        shared_snapshot_root=shared_snapshot_root,
        host_sources=sources, game_sources=source_ids,
        python_version=sys.version, python_executable=sys.executable,
        generator_sha256=stateful_lifting.MODULE_DIGEST,
        rom_sha256=sha(machine.rom_path), symbols_sha256=sha(machine.symbols_path),
        manifest_sha256=sha(machine.build_dir/'manifest.json'),
        limits=['Declared presentation/input and ability graphic boundaries remain conditional.',
            'Only fourteen source enemy trainer items in selected inventory slots are covered.',
            'Hyper Potion is observed at its legitimate Horsea MaxHP cap, with literal 200 source/byte binding.',
            'No AI desirability policy, player-bag symmetry, stage-cap refusal or ability-interaction claims.',
            'No automatic central coverage promotion or M2 completion claim.'])
    (output/'report.json').write_text(json.dumps(report, indent=2)+'\n')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    parser.add_argument('--output', type=pathlib.Path, required=True)
    parser.add_argument('--cases', nargs='+', choices=CASES, default=list(CASES))
    parser.add_argument('--slots', nargs='+', type=int, choices=(0, 1), default=[0, 1])
    parser.add_argument('--seeds', nargs='+', type=int, default=[193])
    parser.add_argument('--freeze-ledger', type=pathlib.Path, required=True)
    parser.add_argument('--native-build', type=pathlib.Path)
    parser.add_argument('--optimization', choices=('O0', 'O1', 'O2', 'O3'), default='O3')
    args = parser.parse_args()
    run(args.artifact_root, args.output, args.cases, args.slots, args.seeds,
        args.freeze_ledger, args.native_build, args.optimization)
