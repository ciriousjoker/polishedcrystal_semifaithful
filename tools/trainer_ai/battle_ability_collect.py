#!/usr/bin/env python3
"""Retain reviewed positive ability full-fight evidence without executing battles."""
from __future__ import annotations

import argparse
from pathlib import Path

import battle_coverage as coverage
from battle_family_evidence import artifact_sink, bind_execution, finish, digest, read, require

ROOT = Path(__file__).resolve().parents[2]
# Explicit reviewed executed files, never inferred from mutable current hosts.
HOSTS = {'battle_controller.py': '6abe731e7eac74a3cae16673c2c6339ad985ddc455a1ba913d915f2f31cda9b3', 'battle_controller_profile.py': '9cf85ef64a22eda7781b739119d4f9eb958a860453af12189900f7b84d13e33d', 'battle_controller_fight.py': 'eb92e12bc7d1100da6e90e6382c04e8b32ce769c7747c2f679818774569deb79', 'battle_controller_fight_check.py': '065bba593a79d3103de9d8d752b067ba9c63ec7116c9f4edbd7aa62c30eda249', 'battle_controller_check.py': 'd3621ffd1c9c142e496476e162833a91853992788d561b083b3a873d035dc9d4', 'battle_controller_oracle.c': '7840c8b7a31bc6431518c5c61569cf961fc82a0ae40fd365f9768ebf12648e23', 'battle_scenarios.py': '39b61a8bd9175f610b3ad63a3167117ca3038016f73621721168f29b5e44eee9', 'battle_decisions.py': '9aea66f9c5ba912da9032a503e5a3f8399ff25d42f3deec58068724fc0225d4c', 'battle_env.py': '2c281bda28d290c8cb7e4e608907a69e0786ef4f72ed5e46cc28b130781865ee', 'stateful_lifting.py': '5e5febf491dca885db415475d4b2c34a5a50c2d150e6ca21bcb998401dc9eade', 'battle_bag.py': '0f77ff9fb79a8c2d58f5d1effc3f1faf731477300847f2270165091dffad95c8', 'battle_actions.py': '09e4aa5358ac724d8c3b8c6d4209ca1aff1f6a9b7f6c92fd80a9a26a135d39cd', 'stateful_fixture_check.py': '8a01e4700625e9a8a015aa6bbc525caf070ad9d3fb84664bd7386e4be7dc3d87', 'battle_env_check.py': 'f9403ece032b28de048a8e20469823005a870eba8ed7c6627a5a65a3792b63b7', 'battle_coverage.py': 'a53bb4c9352e290cd4535f260628bae00cdae1152c345501ec9dd1f8af24e490', 'battle_controller_ability_check.py': '0b16e5a4046ba112bed20c8aaf06e293c63272347de4b3093156a82952222ba1'}
MANIFEST = 'dec3cb17a34aeeb9e0f36e44590d0890b7d0816ddff98fc7bb0346b9b32737b0'
CASES = {'trace-conversion-intimidate', 'intimidate-switch-reentry'}
GAME_SOURCES = {
    'constants/ability_constants.asm', 'engine/battle/abilities.asm',
    'engine/pokemon/abilities.asm', 'engine/battle/ability_gfx.asm',
    'engine/battle/core.asm', 'data/pokemon/base_stats/porygon.asm',
    'data/pokemon/base_stats/gyarados.asm', 'data/pokemon/base_stats/psyduck.asm',
    'data/pokemon/base_stats/snorlax.asm',
}


def validate_ability(raw, row, rules):
    name = row['case']
    intimidate, trace = (rules['ability:' + n]['identifier'] for n in ('INTIMIDATE', 'TRACE'))
    snapshots, displays, turns = raw['snapshots'], raw['displays'], raw['turns']
    require(len(snapshots) == len(turns) + 1 and all(s['ability_mode'] == 0 for s in snapshots),
            'Ability snapshots incomplete or ability mode leaked')
    require(snapshots[0]['enemy_attack'] == 6 and snapshots[0]['player'] == 0,
            'Initial positive Intimidate activation missing')
    require(raw['scenario']['inventory'] == [None, None] and raw['scenario']['bag'] == [],
            'Ability fixture inventory changed')
    for team in ('player', 'enemy'):
        require(all(p['level'] == 50 and p['item'] == 'NO_ITEM' and p['status'] == 0
                    for p in raw['scenario'][team]), 'Ability fixture status/item/level changed')
    move = lambda slot: dict(type='MoveAction', slot=slot)
    if name == 'trace-conversion-intimidate':
        player, enemy = raw['scenario']['player'], raw['scenario']['enemy']
        require(len(player) == len(enemy) == 1 and player[0]['species'] == 'PORYGON' and
                player[0]['ability_slot'] == enemy[0]['ability_slot'] == 0 and
                enemy[0]['species'] == 'GYARADOS' and enemy[0]['moves'] == ['SPLASH', 'TACKLE'] and
                player[0]['moves'] == ['CONVERSION', 'TACKLE', 'WATER_GUN'] and player[0]['pp'] == [30, 35, 0],
                'Trace fixture source identity changed')
        require(snapshots[0]['player_ability'] == snapshots[0]['enemy_ability'] == intimidate and
                snapshots[0]['player_attack'] == 6 and
                any(d['name'] == 'PerformAbilityReplacementGFX' and d['turn'] == 0 and
                    d['state']['player_ability'] == trace and d['state']['enemy_ability'] == intimidate
                    for d in displays), 'Trace did not positively copy active Intimidate')
        require(raw['callbacks'] == [dict(side=0, choice=2)] and
                turns[0]['actions'] == [move(0), move(0)] and
                turns[0]['conversion_choices'] == [move(2)], 'Trace fixture Conversion selection changed')
        return ['ability:TRACE', 'ability:INTIMIDATE']
    player, enemy = raw['scenario']['player'], raw['scenario']['enemy']
    require(len(player) == 2 and len(enemy) == 1 and
            [p['species'] for p in player] == ['GYARADOS', 'PSYDUCK'] and
            player[0]['ability_slot'] == 0 and player[0]['moves'] == ['TACKLE'] and
            player[1]['moves'] == ['SPLASH'] and enemy[0]['species'] == 'SNORLAX' and
            enemy[0]['moves'] == ['SPLASH', 'TACKLE'] and raw['callbacks'] == [],
            'Intimidate fixture source identity changed')
    require(len(snapshots) > 3 and snapshots[0]['player_ability'] == intimidate and
            snapshots[1]['player'] == 1 and snapshots[2]['player'] == 0 and
            snapshots[2]['player_ability'] == intimidate and snapshots[2]['enemy_attack'] == 5 and
            snapshots[3]['enemy_hp'] < snapshots[2]['enemy_hp'] and
            turns[0]['actions'] == [dict(type='SwitchAction', index=1), move(0)] and
            turns[1]['actions'] == [dict(type='SwitchAction', index=0), move(0)],
            'Intimidate switch/reentry positive effect missing')
    return ['ability:INTIMIDATE']


def collect(args):
    current, report = coverage.inventory(args.root), read(args.report)
    require(read(args.inventory) == current, 'Current inventory mismatch')
    expected = {k: v for k, v in HOSTS.items() if k != 'battle_controller_ability_check.py'}
    checkpoint = read(args.host_checkpoint / 'identity.json')
    require(checkpoint.get('host_sources') == HOSTS and
            checkpoint.get('report_sha256') == '211593770390cc5f161ff84d39e6b8cb4a89a791c0c5cbd18dba1d1add17e127' and
            report.get('host_sources') == expected and report.get('passed') is True and
            report.get('source_change_rejected') is True and
            report.get('runner_sha256') == HOSTS['battle_controller_ability_check.py'],
            'Ability executed host checkpoint or successful report mismatch')
    for name, expected_hash in HOSTS.items():
        require(digest(args.host_checkpoint / name) == expected_hash, 'Executed ability host changed: ' + name)
    rows = report.get('cases', [])
    require(rows and len({r['case'] for r in rows}) == len(rows) and
            all(r['case'] in CASES for r in rows), 'Unknown or duplicate ability cases')
    # The original report omits generator identity. Bind its view to the actual
    # raw execution identity without altering or writing historical metadata.
    first_path = Path(rows[0]['evidence']).resolve()
    first_path.relative_to(args.report.parent.resolve())
    first_raw = read(first_path)
    binding = dict(report, generator_sha256=first_raw['generator_sha256'])
    require(binding['manifest_sha256'] == MANIFEST, 'Not the reviewed fresh ability execution')
    retain, write, artifacts = artifact_sink(args.output)
    retain(args.report, 'raw/report.json')
    retain(args.inventory, 'raw/inventory.json')
    retain(args.host_checkpoint / 'identity.json', 'host/identity.json')
    for name, expected_hash in HOSTS.items():
        retain(args.host_checkpoint / name, 'host/' + name, expected_hash)
    manifest, execution, oracle, oracle_artifacts, configuration, identity, symbols = bind_execution(
        args, current, binding, HOSTS, retain, artifacts)
    for source in GAME_SOURCES:
        require(digest(args.root / source) == manifest['sources'][source], 'Ability source changed: ' + source)
    rules = {r['key']: r for r in current['rules']}
    records, accepted = [], set()
    for index, row in enumerate(rows):
        path = Path(row['evidence']).resolve()
        path.relative_to(args.report.parent.resolve())
        raw = read(path)
        raw_ref = retain(path, f'raw/case-{index}.json', row['evidence_sha256'])
        require(raw.get('case') == row['case'] and raw.get('seed') == 193 and
                all(raw.get(field) == expected_hash for field, expected_hash in identity.items()),
                'Ability raw execution identity mismatch')
        require(row.get('passed') is True and row.get('replay_equal') is True and
                raw.get('terminal') is True and raw.get('truncated') is False and raw.get('failed') is False and
                raw.get('scope') == 'ordinary-trainer-fixed-level-source-controller',
                'Ability source terminal/exact replay missing')
        turns, segments = raw['turns'], raw['recorded_segments']
        require(turns and len(turns) == row['turns'] and turns[-1]['terminal'] is True and
                turns[-1]['truncated'] is False and turns[-1]['winner'] == raw['winner'] and
                type(row['comparisons']) is int and row['comparisons'] == len(segments) > 0 and
                all(s.get('kind') in ('segment', 'one') and s.get('state') for s in segments),
                'Missing ability whole-fight source comparisons')
        require(raw['options'].get('link') == 0 and raw['options'].get('tower') == 0 and
                raw['options'].get('battle_mode') == 2, 'Ability fight is not ordinary trainer mode')
        case_rules = validate_ability(raw, row, rules)
        initial = Path(row['initial_wram']).resolve()
        initial.relative_to(args.report.parent.resolve())
        require(raw['initial_wram'] == row['initial_wram'] and
                raw['initial_wram_sha256'] == row['initial_wram_sha256'] and
                raw['initial_wram_boundary'] == 'after-actual-ControllerFight.reset-before-first-actor-action',
                'Ability actual initial WRAM mismatch')
        wram_ref = retain(initial, f'raw/initial-wram-{index}.bin', row['initial_wram_sha256'])
        record = dict(case_id='ability-' + row['case'], passed=True, unsupported=False,
            complete_fight=True, full_state_comparisons=len(segments),
            full_state_components=['cpu', 'hram', 'wram-banks-0-7', 'svbk'],
            initial_wram=wram_ref, raw_case=raw_ref, rule_keys=case_rules,
            choice_contract=dict(roles=configuration['roles'], player_bag=True))
        coverage.runtime_case_options(record, {a['path']: a for a in artifacts}, symbols,
                                      args.output, configuration, current)
        records.append(record)
        accepted.update(case_rules)
    cases_ref = write('cases.json', dict(manifest_sha256=identity['manifest_sha256'], configuration=configuration, cases=records))
    adapter = dict(**identity, inventory_sha256=current['inventory_sha256'], configuration=configuration,
        acceptance_scope='conditional-native-gameplay', oracle_kind='same-presentation-hooks-actual-rom-differential',
        failed_cases=0, unsupported_cases=0, complete_fights=len(records),
        full_state_comparisons=sum(r['full_state_comparisons'] for r in records), rule_keys=sorted(accepted),
        execution_artifacts=execution, oracle_artifacts=oracle_artifacts, case_reports=[cases_ref], artifacts=list(artifacts),
        limitations=report.get('projection_limits', []) + [
            'Exact replay and same-hook Oracle comparison success rely on the preserved executed runner report; raw cases omit duplicate host/Oracle fields.',
            'Only positive Trace copying Intimidate and ordinary Intimidate entry/reentry are claimed. Conversion is fixture context, not additional coverage.'],
        metadata_basis='Original fresh report and raw execution identity, reviewed 16-host checkpoint and explicit ability snapshots.')
    return finish(current, configuration, adapter, 'ability-positive', args.output, write,
        dict(collected_cases=len(records), collected_rules=sorted(accepted)))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    for name in ('inventory', 'report', 'host-checkpoint', 'manifest', 'rom', 'symbols',
                 'build-receipt', 'build-log', 'oracle-library', 'mgba-library', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    try:
        return collect(parser.parse_args())
    except (ValueError, KeyError, OSError, TypeError, IndexError) as error:
        parser.exit(2, 'Ability evidence collection rejected: ' + str(error) + '\n')


if __name__ == '__main__':
    raise SystemExit(main())
