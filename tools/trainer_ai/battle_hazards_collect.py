#!/usr/bin/env python3
"""Retain reviewed positive hazards full-fight evidence without executing battles."""
from __future__ import annotations

import argparse
from pathlib import Path

import battle_coverage as coverage
from battle_family_evidence import artifact_sink, bind_execution, finish, digest, read, require

ROOT = Path(__file__).resolve().parents[2]
# Explicit reviewed executed files, never inferred from mutable current hosts.
HOSTS = {'battle_controller_hazards.py': '478336a55c856508e28d086a7da2d5b5ea338821d5740d27509de8ec5be73ae9', 'battle_controller.py': '6abe731e7eac74a3cae16673c2c6339ad985ddc455a1ba913d915f2f31cda9b3', 'battle_controller_profile.py': '9cf85ef64a22eda7781b739119d4f9eb958a860453af12189900f7b84d13e33d', 'battle_controller_fight.py': 'eb92e12bc7d1100da6e90e6382c04e8b32ce769c7747c2f679818774569deb79', 'battle_controller_fight_check.py': '065bba593a79d3103de9d8d752b067ba9c63ec7116c9f4edbd7aa62c30eda249', 'battle_controller_check.py': 'd3621ffd1c9c142e496476e162833a91853992788d561b083b3a873d035dc9d4', 'battle_controller_oracle.c': '7840c8b7a31bc6431518c5c61569cf961fc82a0ae40fd365f9768ebf12648e23', 'battle_decisions.py': '9aea66f9c5ba912da9032a503e5a3f8399ff25d42f3deec58068724fc0225d4c', 'battle_scenarios.py': '39b61a8bd9175f610b3ad63a3167117ca3038016f73621721168f29b5e44eee9', 'battle_bag.py': '0f77ff9fb79a8c2d58f5d1effc3f1faf731477300847f2270165091dffad95c8', 'battle_env.py': '2c281bda28d290c8cb7e4e608907a69e0786ef4f72ed5e46cc28b130781865ee', 'stateful_lifting.py': '5e5febf491dca885db415475d4b2c34a5a50c2d150e6ca21bcb998401dc9eade', 'stateful_fixture_check.py': '8a01e4700625e9a8a015aa6bbc525caf070ad9d3fb84664bd7386e4be7dc3d87', 'battle_env_check.py': 'f9403ece032b28de048a8e20469823005a870eba8ed7c6627a5a65a3792b63b7', 'battle_actions.py': '09e4aa5358ac724d8c3b8c6d4209ca1aff1f6a9b7f6c92fd80a9a26a135d39cd'}
MANIFEST = 'dec3cb17a34aeeb9e0f36e44590d0890b7d0816ddff98fc7bb0346b9b32737b0'
# Move, layers, entrant, entry behavior. Flying cases are excluded controls.
CASES = {
    'spikes-1': ('SPIKES', 1, 'RATTATA', 'grounded'),
    'spikes-2': ('SPIKES', 2, 'RATTATA', 'grounded'),
    'spikes-3': ('SPIKES', 3, 'RATTATA', 'grounded'),
    'spikes-flying': ('SPIKES', 1, 'PIDGEY', 'flying'),
    'spikes-entry-ko': ('SPIKES', 1, 'RATTATA', 'entry-ko'),
    'toxic-1': ('TOXIC_SPIKES', 1, 'RATTATA', 'grounded'),
    'toxic-2': ('TOXIC_SPIKES', 2, 'RATTATA', 'grounded'),
    'toxic-flying': ('TOXIC_SPIKES', 2, 'PIDGEY', 'flying'),
    'toxic-poison': ('TOXIC_SPIKES', 2, 'GRIMER', 'poison'),
}
GAME_SOURCES = {
    'engine/battle/core.asm', 'engine/battle/effect_commands.asm', 'engine/battle/endturn.asm',
    'engine/battle/move_effects/spikes.asm',
    'constants/battle_constants.asm', 'constants/type_constants.asm',
    'data/pokemon/base_stats/rattata_plain.asm', 'data/pokemon/base_stats/pidgey.asm',
    'data/pokemon/base_stats/grimer_plain.asm', 'data/pokemon/base_stats/sentret.asm',
    'data/pokemon/base_stats/smeargle.asm',
}


def validate_hazard(raw, row, rules, flags, types):
    move, layers, entrant, behavior = CASES[row['case']]
    setter, target = row['setter'], row['setter'] ^ 1
    teams = [raw['scenario']['player'], raw['scenario']['enemy']]
    require(len(teams[setter]) == 1 and len(teams[target]) == (3 if behavior == 'entry-ko' else 2) and
            teams[setter][0]['species'] == teams[target][0]['species'] == 'SMEARGLE' and
            teams[setter][0]['moves'] == [move, 'SPLASH', 'TACKLE'] and
            teams[target][0]['moves'] == ['SPLASH', 'TACKLE'] and teams[target][1]['species'] == entrant and
            teams[target][1]['hp'] == (1 if behavior == 'entry-ko' else None) and
            raw['scenario']['inventory'] == [None, None] and raw['scenario']['bag'] == [],
            'Hazard fixture identity or source entry HP changed')
    require(all(mon['level'] == 50 and mon['item'] == 'NO_ITEM' and mon['status'] == 0 and
                mon['happiness'] == 0 and (side == setter or mon['moves'] == ['SPLASH', 'TACKLE'])
                for side, team in enumerate(teams) for mon in team), 'Hazard fixture isolation changed')
    facts = raw['witness']
    require(facts == row['witness'] and len(facts) == layers + 1, 'Hazard report/witness mismatch')
    hazard = 'spikes' if move == 'SPIKES' else 'toxic_spikes'
    for index, fact in enumerate(facts[:-1]):
        require(fact == dict(phase='placement', turn=index+1, target=target, hazard=hazard,
                             previous=index, current=index+1, move=move) and
                raw['turns'][index]['actions'] == [dict(type='MoveAction', slot=0)] * 2,
                'Hazard actual positive layer placement changed')
    actions = [None, None]
    actions[setter], actions[target] = dict(type='MoveAction', slot=1), dict(type='SwitchAction', index=1)
    require(raw['turns'][layers]['actions'] == actions, 'Hazard source switch/Splash isolation changed')
    entry = facts[-1]
    require(entry['turn'] == layers + 1, 'Hazard entry boundary changed')
    if behavior == 'entry-ko':
        active = entry['active']
        choices = entry['replacement_choices']
        require(entry['phase'] == 'entry-ko-and-replacement' and entry['fainted_index'] == 1 and
                entry['requested_replacement'] == active['index'] == 2 and active['active'] is True and
                teams[target][2]['species'] == 'SENTRET' and active['species'] == 161 and
                active['HP'] == active['MaxHP'] - max(1, active['MaxHP'] // 8) and active['status'] == 0 and
                entry['hazard_layers'] == 1 and
                any(c == dict(side=target, action=dict(type='SwitchAction', index=2)) for c in choices) and
                dict(side=target, action=dict(type='SwitchAction', index=2)) in raw['turns'][layers]['replacement_choices'],
                'Hazard entry KO/replacement activation missing')
    else:
        old, new = entry['before'], entry['after']
        require(entry['phase'] == 'entry' and entry['target'] == target and entry['control'] == behavior and
                old['index'] == new['index'] == 1 and old['active'] is False and new['active'] is True and
                old['HP'] == old['MaxHP'] and old['status'] == 0 and
                old['species'] == {'RATTATA': 19, 'PIDGEY': 16, 'GRIMER': 88}[entrant],
                'Hazard actual entrant/control identity changed')
        for field in old:
            if field not in ('active', 'HP', 'status'):
                require(old[field] == new[field], 'Unexpected hazard entrant change: ' + field)
        require(0 <= new['HP'] <= new['MaxHP'], 'Hazard entrant HP out of range')
        if behavior == 'flying':
            require(types['FLYING'] in new['types'] and new['HP'] == old['HP'] and new['status'] == 0 and
                    entry['hazard_layers'] == layers, 'Flying immunity control changed')
        elif behavior == 'poison':
            require(types['POISON'] in new['types'] and new['HP'] == old['HP'] and new['status'] == 0 and
                    entry['hazard_layers'] == 0, 'Positive Poison absorption missing')
        elif move == 'SPIKES':
            require(new['HP'] == old['HP'] - max(1, old['MaxHP'] // {1: 8, 2: 6, 3: 4}[layers]) and
                    new['status'] == 0 and entry['hazard_layers'] == layers,
                    'Positive Spikes damage/layer mismatch')
        else:
            expected = (1 << flags['PSN']) | ((1 << flags['TOX']) if layers == 2 else 0)
            denominator = 16 if layers == 2 else 8
            require(new['status'] == expected and
                    new['HP'] == old['HP'] - max(1, old['MaxHP'] // denominator) and
                    entry['hazard_layers'] == layers, 'Positive Toxic Spikes poison/residual mismatch')
    move_rule = rules['move:' + move]
    effect = 'effect:' + move_rule['effect']
    require(effect in rules, 'Hazard source effect missing from inventory')
    if behavior == 'flying':
        return []
    claims = [move_rule['key'], effect, 'field:HAZARDS_' + move]
    if move == 'TOXIC_SPIKES' and behavior == 'grounded':
        claims.append('status:PSN')
        if layers == 2:
            claims.append('status:TOX')
    return claims


def collect(args):
    current, report = coverage.inventory(args.root), read(args.report)
    require(read(args.inventory) == current, 'Current inventory mismatch')
    checkpoint = read(args.host_checkpoint / 'identity.json')
    require(checkpoint.get('host_sources') == HOSTS and
            checkpoint.get('report_sha256') == 'ce3fbf51c96c8dc90b681a3460fd7fcd8cec065fd0b5036623c217a908e18971' and
            report.get('host_sources') == HOSTS and report.get('passed') is True and
            report.get('scope') == 'actual-source-isolated-hazard-entry-full-fights' and
            report.get('manifest_sha256') == MANIFEST, 'Hazard checkpoint or successful report mismatch')
    for name, expected in HOSTS.items():
        require(digest(args.host_checkpoint / name) == expected, 'Executed hazard host changed: ' + name)
    retain, write, artifacts = artifact_sink(args.output)
    retain(args.report, 'raw/report.json')
    retain(args.inventory, 'raw/inventory.json')
    retain(args.host_checkpoint / 'identity.json', 'host/identity.json')
    for name, expected in HOSTS.items():
        retain(args.host_checkpoint / name, 'host/' + name, expected)
    manifest, execution, oracle, oracle_artifacts, configuration, identity, symbols = bind_execution(
        args, current, report, HOSTS, retain, artifacts)
    for source in GAME_SOURCES:
        require(digest(args.root / source) == manifest['sources'][source], 'Hazard source changed: ' + source)
    rules = {r['key']: r for r in current['rules']}
    # Inventory field rows point at the exact reviewed constant definitions.
    # Bind their assembled placement masks/step bytes as well as source hashes.
    rom = args.rom.read_bytes()
    def block(label, end):
        bank, address = symbols[label]
        end_bank, end_address = symbols[end]
        require(bank == end_bank and address < end_address, 'Hazard ROM block layout changed')
        start = bank * 0x4000 + address - (0x4000 if bank else 0)
        return rom[start:start + end_address - address]
    for move, mask in (('SPIKES', 0x30), ('TOXIC_SPIKES', 0xc0)):
        field_rule = rules['field:HAZARDS_' + move]
        definition = next(code for number, code in coverage.lines(args.root, field_rule['source'])
                          if number == field_rule['line'])
        require(definition.split() == ['DEF', 'HAZARDS_' + move, 'EQU', '%' + format(mask, '08b')],
                'Hazard source mask differs from reviewed inventory field')
        code = block('BattleCommand_spikes', 'BattleCommand_toxicspikes') if move == 'SPIKES' else block(
            'BattleCommand_toxicspikes', 'BattleCommand_toxicspikes.failed')
        require(bytes([0x3e, mask // 3, 0x86, 0x77]) in code and
                (bytes([0xf6, mask ^ 255, 0x3c]) in code if move == 'SPIKES' else
                 bytes([0xe6, mask, 0xfe, mask // 3 * 2]) in code),
                'Assembled hazard placement mask/step differs from source')
    flags = {name: identifier for name, identifier, _ in coverage.constants(
        args.root, 'constants/battle_constants.asm', 'DEF ALL_STATUS')}
    types = {name: identifier for name, identifier, _ in coverage.constants(
        args.root, 'constants/type_constants.asm', 'DEF NUM_TYPES')}
    poison_code = block('SpikesDamage_GotAbility.no_poison_type', 'SpikesDamage_GotAbility.no_toxic')
    require(bytes([0x3e, 1 << flags['PSN']]) in poison_code and
            bytes([0xf6, 1 << flags['TOX']]) in poison_code,
            'Assembled Toxic Spikes status bits differ from source')
    records, excluded, accepted, seen = [], [], set(), set()
    require(report.get('cases'), 'No hazard cases')
    for index, row in enumerate(report['cases']):
        key = row['case'], row['setter'], row['seed']
        require(row['case'] in CASES and type(row['setter']) is int and row['setter'] in (0, 1) and
                row['seed'] == 193 and key not in seen, 'Unknown or duplicate hazard case')
        seen.add(key)
        path = Path(row['evidence']).resolve()
        path.relative_to(args.report.parent.resolve())
        raw = read(path)
        raw_ref = retain(path, f'raw/case-{index}.json', row['evidence_sha256'])
        require(raw.get('case') == row['case'] and raw.get('setter') == row['setter'] and
                raw.get('seed') == row['seed'] and raw.get('host_sources') == HOSTS and
                raw.get('oracle_provenance') == oracle and
                all(raw.get(field) == expected for field, expected in identity.items()),
                'Hazard raw execution/Oracle identity mismatch')
        require(row.get('passed') is True and row.get('replay_equal') is True and
                raw.get('terminal') is True and raw.get('truncated') is False and raw.get('failed') is False and
                raw.get('winner') == row['winner'] and
                raw.get('scope') == 'ordinary-trainer-fixed-level-source-controller',
                'Hazard source terminal/exact replay missing')
        turns, segments, comparisons = raw['turns'], raw['recorded_segments'], raw['comparisons']
        require(turns and len(turns) == row['turns'] and turns[-1]['terminal'] is True and
                turns[-1]['truncated'] is False and type(row['comparisons']) is int and
                row['comparisons'] == len(segments) == len(comparisons) > 0 and
                all(s.get('kind') in ('segment', 'one') and s.get('state') for s in segments) and
                all(isinstance(c.get('event'), str) and type(c.get('div_events')) is int and
                    c['div_events'] >= 0 for c in comparisons), 'Missing hazard whole-fight comparisons')
        require(raw['options'].get('link') == 0 and raw['options'].get('tower') == 0 and
                raw['options'].get('battle_mode') == 2, 'Hazard fight is not ordinary trainer mode')
        case_rules = validate_hazard(raw, row, rules, flags, types)
        initial = Path(row['initial_wram']).resolve()
        initial.relative_to(args.report.parent.resolve())
        require(raw['initial_wram'] == row['initial_wram'] and
                raw['initial_wram_sha256'] == row['initial_wram_sha256'], 'Hazard actual initial WRAM mismatch')
        wram_ref = retain(initial, f'raw/initial-wram-{index}.bin', row['initial_wram_sha256'])
        record = dict(case_id=f'hazard-{row["case"]}-{row["setter"]}-{row["seed"]}',
            passed=True, unsupported=False, complete_fight=True, full_state_comparisons=len(segments),
            full_state_components=['cpu', 'hram', 'wram-banks-0-7', 'svbk'], initial_wram=wram_ref,
            raw_case=raw_ref, rule_keys=case_rules, choice_contract=dict(roles=configuration['roles'], player_bag=True))
        coverage.runtime_case_options(record, {a['path']: a for a in artifacts}, symbols,
                                      args.output, configuration, current)
        if case_rules:
            records.append(record)
            accepted.update(case_rules)
        else:
            excluded.append(dict(case=record['case_id'], state='control', raw_case=raw_ref,
                                 reason='Flying immunity control is retained without coverage claims'))
    require(records, 'No positive hazard cases')
    cases_ref = write('cases.json', dict(manifest_sha256=identity['manifest_sha256'], configuration=configuration, cases=records))
    adapter = dict(**identity, inventory_sha256=current['inventory_sha256'], configuration=configuration,
        acceptance_scope='conditional-native-gameplay', oracle_kind='same-presentation-hooks-actual-rom-differential',
        failed_cases=0, unsupported_cases=0, complete_fights=len(records),
        full_state_comparisons=sum(r['full_state_comparisons'] for r in records), rule_keys=sorted(accepted),
        execution_artifacts=execution, oracle_artifacts=oracle_artifacts, case_reports=[cases_ref], artifacts=list(artifacts),
        blocked_controls=excluded, excluded_corpus_cases=excluded,
        limitations=report.get('limits', []) + [
            'Entry KO fainted HP and setter/placement invariance rely on reviewed executed driver assertions; absent snapshots are not reconstructed.',
            'Only Spikes/Toxic Spikes placement and listed positive entry conditions are claimed. Flying controls do not claim ability or move coverage.'],
        metadata_basis='Original fresh report, reviewed 15-host checkpoint and explicit hazard layer/entry witnesses.')
    return finish(current, configuration, adapter, 'hazards-positive', args.output, write,
        dict(collected_cases=len(records), executed_cases=len(seen), collected_rules=sorted(accepted), blocked_controls=excluded))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    for name in ('inventory', 'report', 'host-checkpoint', 'manifest', 'rom', 'symbols',
                 'build-receipt', 'build-log', 'oracle-library', 'mgba-library', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    try:
        return collect(parser.parse_args())
    except (ValueError, KeyError, OSError, TypeError, IndexError) as error:
        parser.exit(2, 'Hazard evidence collection rejected: ' + str(error) + '\n')


if __name__ == '__main__':
    raise SystemExit(main())
