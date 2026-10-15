#!/usr/bin/env python3
"""Retain the reviewed source weather fights without importing battle drivers."""
from __future__ import annotations

import argparse
import re
from pathlib import Path

import battle_coverage as coverage
from battle_family_evidence import artifact_sink, bind_execution, finish, digest, read, require

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = 'dec3cb17a34aeeb9e0f36e44590d0890b7d0816ddff98fc7bb0346b9b32737b0'
HOSTS = {
    'battle_controller_weather.py': '7a2adefe1d51b4687b54b633009ba9defc5e06ad3533ed46649536553e04f618',
    'battle_controller.py': '6abe731e7eac74a3cae16673c2c6339ad985ddc455a1ba913d915f2f31cda9b3',
    'battle_controller_profile.py': '9cf85ef64a22eda7781b739119d4f9eb958a860453af12189900f7b84d13e33d',
    'battle_controller_fight.py': 'eb92e12bc7d1100da6e90e6382c04e8b32ce769c7747c2f679818774569deb79',
    'battle_controller_fight_check.py': '065bba593a79d3103de9d8d752b067ba9c63ec7116c9f4edbd7aa62c30eda249',
    'battle_controller_check.py': 'd3621ffd1c9c142e496476e162833a91853992788d561b083b3a873d035dc9d4',
    'battle_controller_oracle.c': '7840c8b7a31bc6431518c5c61569cf961fc82a0ae40fd365f9768ebf12648e23',
    'battle_decisions.py': '9aea66f9c5ba912da9032a503e5a3f8399ff25d42f3deec58068724fc0225d4c',
    'battle_scenarios.py': '39b61a8bd9175f610b3ad63a3167117ca3038016f73621721168f29b5e44eee9',
    'battle_bag.py': '0f77ff9fb79a8c2d58f5d1effc3f1faf731477300847f2270165091dffad95c8',
    'battle_actions.py': '09e4aa5358ac724d8c3b8c6d4209ca1aff1f6a9b7f6c92fd80a9a26a135d39cd',
    'battle_coverage.py': 'a53bb4c9352e290cd4535f260628bae00cdae1152c345501ec9dd1f8af24e490',
    'battle_env.py': '2c281bda28d290c8cb7e4e608907a69e0786ef4f72ed5e46cc28b130781865ee',
    'battle_env_check.py': 'f9403ece032b28de048a8e20469823005a870eba8ed7c6627a5a65a3792b63b7',
    'stateful_lifting.py': '5e5febf491dca885db415475d4b2c34a5a50c2d150e6ca21bcb998401dc9eade',
    'stateful_fixture_check.py': '8a01e4700625e9a8a015aa6bbc525caf070ad9d3fb84664bd7386e4be7dc3d87',
    'battle_env_profile.py': 'bf97f6cee377cb5bef9e557e8771b648ad37d4f853591b9fbfcca12468890c74',
    'battle_actions_profile.py': 'f5371e089fed22473b0fd473598d059fb04032108b45078f4bcd386ed77419e2',
    'battle_party.py': '55b77cd8911680b4b713174308b19e2f20df8f9f3443f84cbd830eadaa734ff4',
    'battle_party_profile.py': '1169d7def08b0ef8f2ee35e941278437bc40874870bafd8830220b4ef302a528',
    'battle_status.py': 'b666d872f52de6c59922c284d179cb2f2dc666fafa22081a0abad5df56d7435a',
    'battle_status_profile.py': '97cda432b618c0b119c0f66b94a4d3c9f08936b3ef393b706f9690c83ea0ee1f',
    'battle_party_probe.py': '8ff21c6c10c5995f1fe3109d561e8adc15ce1d7cfcea2a7d81a8bab0bd5715d6',
}
# weather, actual weather move, switched species/slot/ability, held duration, Cloud Nine
CASES = {
    'rain-move': ('RAIN', 'RAIN_DANCE', None, 5, False),
    'sun-move': ('SUN', 'SUNNY_DAY', None, 5, False),
    'sand-move': ('SANDSTORM', 'SANDSTORM', None, 5, False),
    'hail-move': ('HAIL', 'HAIL', None, 5, False),
    'drizzle': ('RAIN', 'RAIN_DANCE', ('POLITOED', 2, 'DRIZZLE'), 5, False),
    'drought': ('SUN', 'SUNNY_DAY', ('NINETALES', 2, 'DROUGHT'), 5, False),
    'sand-stream': ('SANDSTORM', 'SANDSTORM', ('TYRANITAR', 0, 'SAND_STREAM'), 5, False),
    'snow-warning': ('HAIL', 'HAIL', ('GLACEON', 1, 'SNOW_WARNING'), 5, False),
    'damp-rock': ('RAIN', 'RAIN_DANCE', None, 8, False),
    'cloud-nine': ('SANDSTORM', 'SANDSTORM', None, 5, True),
}
SPECIES = {'HORSEA': 'horsea', 'POLITOED': 'politoed', 'NINETALES': 'ninetales_plain',
           'TYRANITAR': 'tyranitar', 'GLACEON': 'glaceon', 'PSYDUCK': 'psyduck'}
GAME_SOURCES = {
    'engine/battle/endturn.asm', 'engine/battle/abilities.asm', 'engine/battle/effect_commands.asm',
    'engine/battle/move_effects/weather.asm', 'engine/battle/core.asm', 'home/battle.asm',
    'data/pokemon/base_stats.asm', 'constants/battle_constants.asm', 'constants/type_constants.asm',
    'constants/pokemon_constants.asm', 'constants/pokemon_data_constants.asm',
    'constants/ability_constants.asm', 'constants/item_constants.asm', 'constants/move_constants.asm',
    'constants/item_data_constants.asm', 'data/items/attributes.asm',
} | {'data/pokemon/base_stats/'+name+'.asm' for name in SPECIES.values()}
BOUNDARY = 'post-reset-first-frozen-actor-decision'


def constants(root, path):
    result, value = {}, 0
    def integer(text):
        return int(text[1:], 16) if text.startswith('$') else int(text, 0)
    for _, code in coverage.lines(root, path):
        if path == 'constants/pokemon_constants.asm' and code.startswith('DEF NUM_SPECIES '):
            break
        if code.startswith('const_def'):
            value = integer(code[len('const_def'):].strip() or '0')
        elif code.startswith('const_next '):
            value = integer(code.split()[1])
        elif code.startswith('const_skip'):
            value += integer(code[len('const_skip'):].strip() or '1')
        elif code.startswith('const '):
            result[code.split()[1]] = value
            value += 1
    return result


def source_contract(root):
    weather = [code for _, code in coverage.lines(root, 'engine/battle/move_effects/weather.asm')]
    endturn = (root/'engine/battle/endturn.asm').read_text()
    duration = (root/'engine/battle/effect_commands.asm').read_text().split('GetItemBoostedDuration:', 1)[1].split('PrintDoesntAffect:', 1)[0]
    require('ld [wBattleWeather], a' in weather and 'call GetItemBoostedDuration' in weather and
            'ld [wWeatherCount], a' in weather and 'ld a, 5' in duration and 'ld a, c' in duration,
            'Weather creation/duration source requires review')
    handle = endturn.split('HandleWeather:', 1)[1].split('WeatherEndedMessages:', 1)[0]
    require(handle.index('dec [hl]\n\tjr nz, .ongoing') < handle.index('ld [wBattleWeather], a') <
            handle.index('call GetWeatherAfterCloudNine'),
            'Weather decrement-before-expiry-before-Cloud-Nine order changed')
    require('call GetSixteenthMaxHP' in handle and all('call CheckIfUserIs'+kind+'Type' in handle
            for kind in ('Ground', 'Rock', 'Steel', 'Ice')), 'Weather residual/type immunity source changed')
    home = (root/'home/battle.asm').read_text().split('GetWeatherAfterCloudNine::', 1)[1].split('CheckNeutralizingGas::', 1)[0]
    require(home.count('xor CLOUD_NINE') == 2 and 'ld a, [wBattleWeather]' in home,
            'Cloud Nine source suppression requires review')


def validate_witness(root, name, setter, raw, row, rules, ids, rom_bytes, symbols):
    weather, move, entry, duration, cloud = CASES[name]
    fields, abilities, types, moves, items, effects, species_ids = ids
    def species_record(species, slot, expected_ability):
        path = 'data/pokemon/base_stats/'+SPECIES[species]+'.asm'
        active = [code for _, code in coverage.lines(root, path)]
        pair = next(code[3:].split(', ') for code in active if re.fullmatch(r'db [A-Z_]+, [A-Z_]+', code))
        entries = [code.split(' ', 1)[1].split(', ')[1:] for code in active if code.startswith('abilities_for ')]
        require(len(entries) == 1 and entries[0][slot] == expected_ability, 'Active source ability slot mismatch')
        table = (root/'data/pokemon/base_stats.asm').read_text()
        includes = re.findall(r'^INCLUDE "(data/pokemon/base_stats/[^\"]+)"', table, re.M)
        index = includes.index(path)
        address = lambda label: symbols[label][1]
        origin = address('wCurBaseData')
        stride = address('wCurBaseDataEnd')-origin
        data = rom_bytes('BaseData', index*stride, stride)
        type_offset, ability_offset = address('wBaseType1')-origin, address('wBaseAbility1')-origin
        values = [types[value] for value in pair]
        require(list(data[type_offset:type_offset+2]) == values and data[ability_offset+slot] == abilities[expected_ability],
                'Compiled weather species type/ability mismatch')
        return dict(species=species, slot=slot, source=path, table_index=index,
            compiled_record=data.hex(), types=values, type_names=pair,
            ability=abilities[expected_ability], ability_name=expected_ability)
    roster = {'HORSEA': species_record('HORSEA', 0, 'SWIFT_SWIM')}
    if entry:
        roster[entry[0]] = species_record(*entry)
    if cloud:
        roster['PSYDUCK'] = species_record('PSYDUCK', 1, 'CLOUD_NINE')
    item = None
    if name == 'damp-rock':
        item_rule = rules['item:DAMP_ROCK']
        attributes = rom_bytes('ItemAttributes', (items['DAMP_ROCK']-1)*6, 6)
        require(item_rule['identifier'] == items['DAMP_ROCK'] and item_rule['held_effect'] == 'HELD_PROLONG_RAIN' and
                int(item_rule['parameter'], 0) == 8 and
                list(attributes[2:4]) == [effects['HELD_PROLONG_RAIN'], 8], 'Damp Rock source/ROM duration mismatch')
        item = dict(name='DAMP_ROCK', identifier=items['DAMP_ROCK'], effect=effects['HELD_PROLONG_RAIN'],
            duration=8, pocket=attributes[4] & 15, context=attributes[5] & 15)
        require('DAMP_ROCK' in rules['held-effect:HELD_PROLONG_RAIN']['items'], 'Damp Rock effect membership mismatch')
    metadata = dict(roster=roster, item=item, weather=fields['WEATHER_'+weather], move=moves[move])
    w = raw['witness']
    require(w == row['witness'] and w.get('metadata') == metadata and w.get('setter') == setter and
            w.get('scope') == 'source-weather-creation-through-expiry-and-clear-round' and
            w.get('duration') == duration and w.get('damage_ticks') == duration-1 and
            w.get('cloud_nine_effects_suppressed') is cloud, 'Weather metadata/duration/source witness mismatch')
    teams = [raw['scenario']['player'], raw['scenario']['enemy']]
    require([len(team) for team in teams] == [2 if entry and side == setter else 1 for side in (0, 1)] and
            raw['scenario']['bag'] == [] and raw['scenario']['inventory'] == [None, None], 'Weather fixture team/inventory mismatch')
    expected_members = {}
    for side, team in enumerate(teams):
        for index, mon in enumerate(team):
            species = entry[0] if entry and side == setter and index == 1 else 'PSYDUCK' if cloud and side != setter else 'HORSEA'
            record = roster[species]
            expected_moves = ['SPLASH', 'WATER_GUN']+([move] if side == setter and not entry else [])
            require(mon['species'] == species and mon['ability_slot'] == record['slot'] and
                    mon['moves'] == expected_moves and mon['item'] == ('DAMP_ROCK' if name == 'damp-rock' and side == setter else 'NO_ITEM') and
                    mon['level'] == 50 and mon['hp'] is None and mon['status'] == 0 and mon['happiness'] == 0 and
                    mon['form'] == 0 and mon['nature'] == 0 and mon['dvs'] == [0, 0, 0] and
                    mon['evs'] == [0]*6 and mon['pp'] is None, 'Weather fixture no longer isolates reviewed inputs')
            expected_members[side, index] = (record, expected_moves, mon['item'])
    probes = w['probes']
    expected_rows = (duration+1)*sum(map(len, teams))
    require(len(probes) == expected_rows and len(raw['turns']) >= duration+1, 'Incomplete weather expiry/clear-round interval')
    before_by_member = {}
    expected_order = [(round_index, side, index) for round_index in range(duration+1)
                      for side in (0, 1) for index in range(len(teams[side]))]
    positive_damage = immune_ticks = 0
    for probe, position in zip(probes, expected_order):
        round_index, side, index = position
        require((probe['round'], probe['side'], probe['index']) == position and probe['turn'] == round_index+1,
                'Weather probe ordering/member coverage mismatch')
        remaining = max(0, duration-round_index-1)
        require(probe['weather'] == (metadata['weather'] if remaining else 0) and probe['weather_turns'] == remaining,
                'Weather counter decrement/expiry/clear round mismatch')
        actions = [dict(type='MoveAction', slot=0), dict(type='MoveAction', slot=0)]
        if round_index == 0:
            actions[setter] = dict(type='SwitchAction', index=1) if entry else dict(type='MoveAction', slot=2)
        require(raw['turns'][round_index]['actions'] == actions, 'Weather isolated submitted action mismatch')
        before, after = probe['before'], probe['after']
        record, move_names, held = expected_members[side, index]
        active = index == (1 if entry and side == setter else 0)
        if round_index:
            require(before == before_by_member[side, index], 'Weather per-member transition chain is broken')
        else:
            require(before['active'] is (index == 0) and before['HP'] == before['MaxHP'] and before['status'] == 0,
                    'Weather initial health/active precondition mismatch')
        require(after['active'] is active and before['index'] == after['index'] == index,
                'Weather switched active identity mismatch')
        expected_move_ids = [moves[value] for value in move_names]+[0]*(4-len(move_names))
        expected_pp = [rom_bytes('Moves', (value-1)*8, 8)[5] if value else 0 for value in expected_move_ids]
        if round_index == 0:
            require(before['moves'] == expected_move_ids and before['pp'] == expected_pp and before['pp_ups'] == [0]*4,
                    'Weather initial source moves/PP differ')
        for mon in (before, after):
            require(mon['species'] == species_ids[record['species']] and mon['ability'] == record['ability'] and
                    mon['ability_slot'] == record['slot']+1 and mon['item'] == (items['DAMP_ROCK'] if held == 'DAMP_ROCK' else 0) and
                    mon['status'] == 0 and mon['moves'] == expected_move_ids and mon['pp_ups'] == [0]*4,
                    'Weather observed source species/ability/item/move/status mismatch')
            if mon['active']:
                require(mon['types'] == record['types'], 'Weather active compiled type mismatch')
        for field in ('species', 'form', 'egg', 'level', 'nature', 'ability_slot', 'happiness', 'dvs', 'gender',
                      'MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef'):
            require(before[field] == after[field], 'Weather interval changed permanent identity/stat: '+field)
        immune = any(value in record['type_names'] for value in (
            ('GROUND', 'ROCK', 'STEEL') if weather == 'SANDSTORM' else ('ICE',)))
        damage = max(1, before['MaxHP']//16) if weather in ('SANDSTORM', 'HAIL') and remaining and active and not cloud and not immune else 0
        require(probe['type_immune'] is immune and probe['damage'] == damage and
                after['HP'] == before['HP']-damage and 0 < after['HP'] <= after['MaxHP'],
                'Weather residual/Cloud Nine/type immunity mismatch')
        pp = list(before['pp'])
        if active and not (round_index == 0 and entry and side == setter):
            pp[2 if round_index == 0 and side == setter else 0] -= 1
        require(after['pp'] == pp, 'Weather source PP use mismatch')
        if active:
            stored = probe['stored']
            require(stored['side'] == side and stored['index'] == index and set(stored['caches']) == {'party', 'battle'},
                    'Weather active/party cache identity mismatch')
            for cache in stored['caches'].values():
                require(all(cache[field] == after[field] for field in ('HP', 'status', 'item', 'pp')),
                        'Weather active/party cache values mismatch')
        else:
            require(probe['stored'] is None, 'Inactive weather member unexpectedly supplies active cache')
        before_by_member[side, index] = after
        positive_damage += int(damage > 0)
        immune_ticks += int(active and immune and remaining > 0)
    if weather in ('SANDSTORM', 'HAIL') and not cloud:
        require(positive_damage > 0 and (not entry or immune_ticks == duration-1), 'Weather residual/immunity lacks positive branch witness')
    key = 'field:WEATHER_'+weather
    require(key in rules, 'Weather field rule absent from actual inventory')
    keys = {key}
    if entry:
        keys.add('ability:'+entry[2])
    else:
        move_rule = rules['move:'+move]
        data = rom_bytes('Moves', (moves[move]-1)*8, 8)
        require(move_rule['identifier'] == moves[move] == data[0] and
                data[1] == rules['effect:'+move_rule['effect']]['identifier'] and move_rule['power'] == '0',
                'Weather source/ROM move/effect identity mismatch')
        keys.update(('move:'+move, 'effect:'+move_rule['effect']))
    if cloud:
        keys.add('ability:CLOUD_NINE')
    if item:
        keys.add('held-effect:HELD_PROLONG_RAIN')
    require(all(key in rules and rules[key]['required'] for key in keys), 'Invented/non-required weather promotion')
    return sorted(keys)


def collect(args):
    current = coverage.inventory(args.root)
    require(read(args.inventory) == current, 'Current source inventory mismatch')
    source_contract(args.root)
    reports = [read(path) for path in args.report]
    require(reports and len(set(path.resolve() for path in args.report)) == len(reports), 'Duplicate/no weather reports')
    first = reports[0]
    require(first.get('manifest_sha256') == MANIFEST and first.get('host_sources') == HOSTS, 'Unreviewed weather execution checkpoint')
    retain, write, artifacts = artifact_sink(args.output)
    retain(args.inventory, 'raw/inventory.json')
    # Resolve actual executed bytes from the original report snapshots only.
    host_checkpoint = {}
    for name, expected in HOSTS.items():
        matches = [entry for path, entry in first['inputs'].items()
                   if Path(path).name == name and Path(path).parent.name == 'trainer_ai']
        require(len(matches) == 1 and matches[0]['sha256'] == expected, 'Missing reviewed weather host identity: '+name)
        snapshot = Path(matches[0]['snapshot'])
        retain(snapshot, 'host/'+name, expected)
        host_checkpoint[name] = dict(sha256=expected, snapshot=str(snapshot))
    write('host/identity.json', dict(verified=True, files=host_checkpoint, basis='Original weather report retained actual input bytes'))
    manifest, execution, oracle, oracle_artifacts, configuration, identity, symbols = bind_execution(
        args, current, first, HOSTS, retain, artifacts)
    rom = args.rom.read_bytes()
    def rom_bytes(label, offset, length):
        bank, address = symbols[label]
        require(0 <= offset and length > 0, 'Invalid weather ROM table range')
        # BaseData deliberately spans banks through its contiguous linked ROM table.
        start = bank*0x4000+address-(0x4000 if bank else 0)+offset
        value = rom[start:start+length]
        require(len(value) == length, 'Weather compiled table bytes unavailable')
        return value
    rules = {rule['key']:rule for rule in current['rules']}
    paths = ('battle', 'ability', 'type', 'move', 'item', 'item_data', 'pokemon')
    ids = [constants(args.root, 'constants/'+name+'_constants.asm') for name in paths]
    records, accepted, seen = [], set(), set()
    retained_inputs = set()
    index = 0
    for batch, (report_path, report) in enumerate(zip(args.report, reports)):
        require(report.get('scope') == 'bounded-source-weather-duration-residual-entry-full-fights' and
                report.get('passed') is True and report.get('host_sources') == HOSTS and
                report.get('oracle_provenance') == oracle and report.get('game_sources') == first['game_sources'] and
                all(report.get(field) == value for field,value in identity.items()), 'Weather report execution identity mismatch')
        require({path:entry['sha256'] for path,entry in report['inputs'].items()} ==
                {path:entry['sha256'] for path,entry in first['inputs'].items()},
                'Mixed/missing weather executed input snapshots')
        require(set(report['game_sources']) == GAME_SOURCES and report.get('cases'), 'Weather source identity set/cases incomplete')
        for path, expected in report['game_sources'].items():
            require(digest(args.root/path) == expected == manifest['sources'].get(path), 'Weather executed source changed: '+path)
        retain(report_path, f'raw/report-{batch}.json')
        # Keep every original input byte, including generated native C/library.
        for original, entry in sorted(report['inputs'].items()):
            require(isinstance(entry.get('sha256'), str), 'Missing weather input identity')
            snapshot = Path(entry['snapshot']).resolve()
            snapshot.relative_to(report_path.parent.resolve())
            require(digest(snapshot) == entry['sha256'], 'Weather retained input bytes changed: '+original)
            input_key = entry['sha256'], Path(original).name
            if input_key not in retained_inputs:
                retain(snapshot, 'raw/input-'+input_key[0]+'-'+input_key[1], entry['sha256'])
                retained_inputs.add(input_key)
        for row in report['cases']:
            name, setter, seed = row['case'], row['setter'], row['seed']
            key = name, setter, seed
            require(name in CASES and type(setter) is int and setter in (0, 1) and type(seed) is int and key not in seen,
                    'Unknown/duplicate weather case')
            seen.add(key)
            path = Path(row['evidence']).resolve()
            path.relative_to(report_path.parent.resolve())
            raw = read(path)
            raw_ref = retain(path, f'raw/case-{index}.json', row['evidence_sha256'])
            require(raw.get('host_sources') == HOSTS and raw.get('game_sources') == report['game_sources'] and
                    raw.get('inputs') == report['inputs'] and raw.get('oracle_provenance') == oracle and
                    all(raw.get(field) == value for field,value in identity.items()) and
                    (raw.get('case'), raw.get('setter'), raw.get('seed')) == key, 'Weather raw/report identity mismatch')
            require(row.get('passed') is True and row.get('replay_equal') is True and raw.get('terminal') is True and
                    raw.get('failed') is False and raw.get('truncated') is False and raw.get('winner') == row['winner'] and
                    raw.get('scope') == 'ordinary-trainer-fixed-level-source-controller', 'Weather source terminal/exact replay missing')
            require(raw.get('options') == dict(initial=131, initial2=6, link=0, tower=0, battle_mode=2) and
                    raw.get('initial_normalized_state_sha256') and raw.get('initial_boundaries') and raw.get('decision_boundary'),
                    'Weather initial options/state/boundary missing')
            segments, comparisons, turns = raw['recorded_segments'], raw['comparisons'], raw['turns']
            require(len(turns) == row['turns'] and turns and turns[-1]['terminal'] is True and
                    turns[-1]['truncated'] is False and row['comparisons'] == len(segments) == len(comparisons) > 0 and
                    all(segment.get('kind') in ('segment', 'one') and segment.get('state') for segment in segments) and
                    all(isinstance(comparison.get('event'), str) and type(comparison.get('div_events')) is int and
                        comparison['div_events'] >= 0 for comparison in comparisons), 'Weather full-fight differential records missing')
            case_rules = validate_witness(args.root, name, setter, raw, row, rules, ids, rom_bytes, symbols)
            wram = Path(row['initial_wram']).resolve()
            wram.relative_to(report_path.parent.resolve())
            require(wram.stat().st_size == 8192 and raw.get('initial_wram') == row['initial_wram'] and
                    raw.get('initial_wram_sha256') == row['initial_wram_sha256'] and
                    raw.get('initial_wram_boundary') == row.get('initial_wram_boundary') == BOUNDARY,
                    'Weather standard initial WRAM/path/hash/boundary missing')
            wram_ref = retain(wram, f'raw/initial-wram-{index}.bin', row['initial_wram_sha256'])
            memory = Path(row['initial_memory']).resolve()
            memory.relative_to(report_path.parent.resolve())
            require(raw.get('initial_memory') == row['initial_memory'] and
                    raw.get('initial_memory_sha256') == row['initial_memory_sha256'] and
                    memory.stat().st_size == 0x6000+8192+127+1 and memory.read_bytes()[0x6000:0x8000] == wram.read_bytes(),
                    'Weather initial full memory differs from plain WRAM')
            retain(memory, f'raw/initial-memory-{index}.bin', row['initial_memory_sha256'])
            record = dict(case_id=f'weather-{name}-{setter}-{seed}', passed=True, unsupported=False,
                complete_fight=True, full_state_comparisons=len(segments),
                full_state_components=['cpu', 'hram', 'wram-banks-0-7', 'svbk'],
                initial_wram=wram_ref, raw_case=raw_ref, rule_keys=case_rules,
                choice_contract=dict(roles=configuration['roles'], player_bag=True))
            coverage.runtime_case_options(record, {artifact['path']:artifact for artifact in artifacts}, symbols,
                                          args.output, configuration, current)
            records.append(record)
            accepted.update(case_rules)
            index += 1
    require(records, 'No positive weather cases')
    cases_ref = write('cases.json', dict(manifest_sha256=identity['manifest_sha256'], configuration=configuration, cases=records))
    adapter = dict(**identity, inventory_sha256=current['inventory_sha256'], configuration=configuration,
        acceptance_scope='conditional-native-gameplay', oracle_kind='same-presentation-hooks-actual-rom-differential',
        failed_cases=0, unsupported_cases=0, complete_fights=len(records),
        full_state_comparisons=sum(record['full_state_comparisons'] for record in records), rule_keys=sorted(accepted),
        execution_artifacts=execution, oracle_artifacts=oracle_artifacts, case_reports=[cases_ref], artifacts=list(artifacts),
        limitations=first['limits']+['Exact replay and source before/after capture are bound to the pinned executed driver.',
            'Rock/Ice immunity witnesses strengthen weather fields; no nonexistent immunity ledger key is invented.'],
        metadata_basis='Reviewed 23-file weather execution checkpoint plus source/ROM duration, HP/type/ability/PP/cache reconstruction.')
    return finish(current, configuration, adapter, 'source-weather', args.output, write,
        dict(collected_cases=len(records), executed_cases=index, collected_rules=sorted(accepted),
             rounds=sum(row['turns'] for report in reports for row in report['cases'])))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    parser.add_argument('--report', type=Path, action='append', required=True)
    for name in ('inventory', 'manifest', 'rom', 'symbols', 'build-receipt', 'build-log',
                 'oracle-library', 'mgba-library', 'output'):
        parser.add_argument('--'+name, type=Path, required=True)
    try:
        return collect(parser.parse_args())
    except (ValueError, KeyError, OSError, TypeError, IndexError, StopIteration) as error:
        parser.exit(2, 'Weather evidence collection rejected: '+str(error)+'\n')


if __name__ == '__main__':
    raise SystemExit(main())
