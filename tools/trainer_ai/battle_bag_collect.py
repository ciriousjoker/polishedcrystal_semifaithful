#!/usr/bin/env python3
"""Register only the retained positive restorative bag-family execution cases."""
from __future__ import annotations

import argparse
import json
from pathlib import Path

import battle_coverage as coverage
from battle_corpus_collect import digest, read, require

ROOT = Path(__file__).resolve().parents[2]
# Reviewed execution checkpoint, not the possibly changed working-tree hosts.
HOSTS = {
    'battle_controller_bag_family.py': 'f9632ffe09dff518c80604ed088ab8ea10b25a32f07506e08b5dbd4ac45336a5',
    'battle_controller.py': '90f7698b0c83af407e1643debf979950f75285da4be8b888ed46eb8e08b8267f',
    'battle_controller_profile.py': '6da8cfd259e9109e1e91626bef4c8d624200938132e076dd6a185592eadb39ba',
    'battle_controller_fight.py': '7bcc2cd656ea726208629c149578d1c1771586b1e8b7911d91cf17b598427771',
    'battle_controller_fight_check.py': '065bba593a79d3103de9d8d752b067ba9c63ec7116c9f4edbd7aa62c30eda249',
    'battle_controller_check.py': 'd3621ffd1c9c142e496476e162833a91853992788d561b083b3a873d035dc9d4',
    'battle_controller_oracle.c': '7840c8b7a31bc6431518c5c61569cf961fc82a0ae40fd365f9768ebf12648e23',
    'battle_decisions.py': '9aea66f9c5ba912da9032a503e5a3f8399ff25d42f3deec58068724fc0225d4c',
    'battle_scenarios.py': '39b61a8bd9175f610b3ad63a3167117ca3038016f73621721168f29b5e44eee9',
    'battle_bag.py': '345b2ea9860e9d3abbc2dfbf74f939558e665a39fb5957ee5eea91780ac156a4',
    'battle_env.py': '2c281bda28d290c8cb7e4e608907a69e0786ef4f72ed5e46cc28b130781865ee',
    'battle_env_check.py': 'f9403ece032b28de048a8e20469823005a870eba8ed7c6627a5a65a3792b63b7',
    'stateful_lifting.py': '5e5febf491dca885db415475d4b2c34a5a50c2d150e6ca21bcb998401dc9eade',
    'stateful_fixture_check.py': '8a01e4700625e9a8a015aa6bbc525caf070ad9d3fb84664bd7386e4be7dc3d87',
}
CASES = {
    **{item: ('RestoreHPEffect', 'hp', 0) for item in
       ('POTION', 'SUPER_POTION', 'HYPER_POTION', 'MAX_POTION')},
    'FULL_RESTORE': ('FullRestore', 'full', 0),
    **{item: ('HealStatusEffect', 'status', 0) for item in
       ('ANTIDOTE', 'BURN_HEAL', 'PARALYZEHEAL', 'AWAKENING', 'ICE_HEAL', 'FULL_HEAL')},
    'REVIVE': ('ReviveEffect', 'revive', 1),
    'MAX_REVIVE': ('ReviveEffect', 'revive', 1),
    'REVIVAL_HERB': ('RevivalHerb', 'revive', 1),
    **{item: ('RestorePPEffect', kind, 0) for item, kind in
       (('ETHER', 'single-pp'), ('MAX_ETHER', 'single-pp'),
        ('ELIXIR', 'all-pp'), ('MAX_ELIXIR', 'all-pp'))},
}


def positive_witness(item, witness, attributes, rom_bytes):
    handler, kind, target = CASES[item]
    require(witness.get('scope') == 'isolated-source-player-restoration' and
            witness.get('target') == target and witness.get('consumed') == 1 and
            witness.get('opponent_action') == 'SPLASH' and
            witness.get('selected_move') == (0 if kind == 'single-pp' else None),
            'Missing typed target, consumption or opposing Splash witness')
    old, new = witness['before'], witness['after']
    require(old['index'] == new['index'] == target and
            old['moves'] == new['moves'] and old['pp_ups'] == new['pp_ups'] == [0] * 4 and
            old['MaxHP'] == new['MaxHP'], 'Witness target/move/PP-Up identity changed')
    require(len(old['moves']) == len(old['pp']) == len(new['pp']) == 4,
            'Malformed witness move/PP vectors')
    parameter = attributes[3]
    if kind == 'hp':
        expected = min(old['MaxHP'], old['HP'] + (999 if parameter == 255 else parameter))
        require(new['HP'] == expected and new['HP'] > old['HP'], 'No correct positive HP restoration')
    elif kind == 'full':
        require(old['HP'] < old['MaxHP'] and old['status'] and
                new['HP'] == new['MaxHP'] and new['status'] == 0, 'No positive full restoration')
    elif kind == 'status':
        require(old['status'] and new['status'] == 0 and old['HP'] == new['HP'],
                'No positive status restoration')
    elif kind == 'revive':
        expected = old['MaxHP'] // 2 if item == 'REVIVE' else old['MaxHP']
        require(old['HP'] == 0 and new['HP'] == expected and expected > 0,
                'No correct positive reserve revival')
        if item == 'REVIVAL_HERB':
            require(new['happiness'] < old['happiness'], 'No Revival Herb happiness penalty')
    else:
        maximum = []
        for move in old['moves']:
            if move:
                data = rom_bytes('Moves', (move - 1) * 8, 8)
                require(data[0] == move, 'Assembled move table layout changed')
                maximum.append(data[5])
            else:
                maximum.append(0)
        expected = list(old['pp'])
        for slot in range(4):
            if kind == 'single-pp' and slot != 0:
                continue
            expected[slot] = maximum[slot] if item.startswith('MAX_') else min(maximum[slot], old['pp'][slot] + 10)
        require(new['pp'] == expected and old['pp'] != new['pp'], 'No correct positive PP restoration')
    if kind not in ('hp', 'full', 'revive'):
        require(old['HP'] == new['HP'], 'Status/PP witness changed HP')
    if not kind.endswith('pp'):
        require(old['pp'] == new['pp'], 'Non-PP witness changed move PP')
    if item not in ('FULL_RESTORE', 'ANTIDOTE', 'BURN_HEAL', 'PARALYZEHEAL', 'AWAKENING', 'ICE_HEAL', 'FULL_HEAL'):
        require(old['status'] == new['status'], 'Non-status restorative changed status')
    require(all(0 <= hp <= mon['MaxHP'] for mon, hp in ((old, old['HP']), (new, new['HP']))),
            'Witness HP is outside valid bounds')


def collect(args):
    current = coverage.inventory(args.root)
    require(read(args.inventory) == current, 'Current inventory mismatch')
    report = read(args.report)
    require(report.get('scope') == 'actual-source-positive-restorative-player-bag-fights' and
            report.get('passed') is True, 'Unsupported or failed restorative report')
    require(report.get('host_sources') == HOSTS and read(args.host_checkpoint / 'identities.json') == HOSTS,
            'Executed host checkpoint identities differ from reviewed source')
    for name, expected in HOSTS.items():
        require(digest(args.host_checkpoint / name) == expected, 'Executed host source missing or changed: ' + name)
    require(not args.output.exists() or not any(args.output.iterdir()), 'Output must be empty')
    args.output.mkdir(parents=True, exist_ok=True)
    artifacts = []

    def retain(path, relative, expected=None):
        payload = path.read_bytes()
        actual = coverage.sha(payload)
        require(expected is None or actual == expected, 'Retained bytes mismatch: ' + str(path))
        destination = args.output / relative
        destination.resolve().relative_to(args.output.resolve())
        require(not destination.exists(), 'Retained path collision: ' + relative)
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(payload)
        artifacts.append(dict(path=relative, sha256=actual))
        return relative

    def write(relative, value):
        path = args.output / relative
        path.write_text(json.dumps(value, indent=2) + '\n')
        artifacts.append(dict(path=relative, sha256=digest(path)))
        return relative

    retain(args.report, 'raw/report.json')
    retain(args.inventory, 'raw/inventory.json')
    retain(args.host_checkpoint / 'identities.json', 'host/identities.json')
    for name, expected in HOSTS.items():
        retain(args.host_checkpoint / name, 'host/' + name, expected)
    manifest = read(args.manifest)
    require(digest(args.manifest) == report.get('manifest_sha256'), 'Executed manifest mismatch')
    for path, expected in current['source_inputs'].items():
        require(manifest.get('sources', {}).get(path) == expected, 'Manifest source mismatch: ' + path)
    retain(args.manifest, 'execution/manifest.json', report['manifest_sha256'])
    execution = {'generator': 'host/stateful_lifting.py'}
    for kind, path in (('rom', args.rom), ('symbols', args.symbols), ('build_receipt', args.build_receipt)):
        execution[kind] = retain(path, 'execution/' + kind + path.suffix,
                                 manifest.get(kind + '_sha256') if kind != 'build_receipt' else None)
    receipt = read(args.build_receipt)
    require(receipt.get('build_log_sha256'), 'Original build log identity missing')
    retain(args.build_log, 'execution/original-build.log', receipt['build_log_sha256'])
    oracle = report.get('oracle_provenance', {})
    require(oracle.get('schema') == 1 and oracle.get('kind') == 'instrumented-same-hooks-mgba' and
            not oracle.get('libmgba_identity_error') and
            oracle.get('libmgba', {}).get('identity_kind') == 'loaded-GBCoreCreate-symbol-dladdr',
            'Actual loaded oracle dependency identity missing')
    oracle_artifacts = {'source': 'host/battle_controller_oracle.c'}
    for kind, path in (('library', args.oracle_library), ('libmgba', args.mgba_library)):
        require(oracle.get(kind, {}).get('path'), 'Missing loaded oracle path: ' + kind)
        oracle_artifacts[kind] = retain(path, 'execution/' + kind + '.dylib', oracle[kind]['sha256'])
    require(oracle.get('source', {}).get('path') and
            oracle['source'].get('sha256') == HOSTS['battle_controller_oracle.c'], 'Oracle C source mismatch')
    for abi, name in (('State', 'stateful_fixture_check.py'), ('Hook', 'battle_env_check.py')):
        require(oracle.get('abi_sources', {}).get(abi, {}).get('path') and
                oracle['abi_sources'][abi].get('sha256') == HOSTS[name], 'Oracle ABI mismatch: ' + abi)
    require(manifest.get('banked_wram') is True and manifest.get('lcd_disabled') is True,
            'Bag checkpoint requires its actual banked WRAM and disabled LCD profile')
    configuration = dict(mode='ordinary-trainer', ruleset='default-non-faithful',
        profile_kind='headless-iid-div', roles=dict(player='scripted', enemy='scripted'),
        options=dict(abilities=True, experience=False, fixed_levels=True, player_bag=True,
                     battle_style='set', banked_wram=manifest.get('banked_wram', False),
                     lcd_disabled=manifest.get('lcd_disabled', False)))
    identity = {field: report[field] for field in
                ('rom_sha256', 'symbols_sha256', 'generator_sha256', 'manifest_sha256')}
    for field in ('rom_sha256', 'symbols_sha256', 'generator_sha256'):
        require(identity[field] == manifest.get(field), 'Report execution identity mismatch: ' + field)
    temporary = dict(configuration=configuration, execution_artifacts=execution, **identity)
    _, symbols = coverage.execution_artifacts(temporary, artifacts, args.output, manifest, current)
    rom = args.rom.read_bytes()

    def rom_bytes(label, offset, length):
        bank, address = symbols[label]
        require(0 <= address + offset < address + offset + length <= (0x4000 if bank == 0 else 0x8000),
                'ROM table lookup crosses bank boundary: ' + label)
        start = bank * 0x4000 + address - (0x4000 if bank else 0) + offset
        result = rom[start:start + length]
        require(len(result) == length, 'Missing assembled table bytes')
        return result

    def constant_values(path, stop):
        values, number = {}, 0
        for _, code in coverage.lines(args.root, path):
            if code.startswith(stop):
                break
            if code.startswith('const_def'):
                number = int(code[len('const_def'):].strip() or '0', 0)
            elif code.startswith('const '):
                values[code.split()[1]] = number
                number += 1
            elif code.startswith('const_skip'):
                number += int(code[len('const_skip'):].strip() or '1', 0)
            elif code.startswith('const_next'):
                raise ValueError('Constant numbering needs source review: ' + path)
        return values

    effect_ids = constant_values('constants/item_data_constants.asm', 'DEF NUM_ITEM_EFFECTS')
    battle_ids = constant_values('constants/battle_constants.asm', 'unused-stop-marker')
    expected_status = {name: 1 << battle_ids[flag] for name, flag in
        (('BURN_HEAL', 'BRN'), ('PARALYZEHEAL', 'PAR'), ('ICE_HEAL', 'FRZ'))}
    expected_status.update(AWAKENING=2, **{name: (1 << battle_ids['PSN']) | (1 << battle_ids['TOX'])
        for name in ('ANTIDOTE', 'FULL_HEAL', 'FULL_RESTORE')})
    rules = {rule['key']: rule for rule in current['rules']}
    records, accepted_rules, seen = [], set(), set()
    rows = report.get('cases', [])
    require(rows, 'No original bag-family cases')
    for index, row in enumerate(rows):
        item = row['item']
        require(item in CASES and (item, row['seed']) not in seen, 'Unknown or duplicate restorative case')
        seen.add((item, row['seed']))
        path = Path(row['evidence']).resolve()
        path.relative_to(args.report.parent.resolve())
        raw = read(path)
        raw_ref = retain(path, f'raw/case-{index}.json', row['evidence_sha256'])
        require(row.get('passed') is True and row.get('replay_equal') is True and
                raw.get('terminal') is True and raw.get('truncated') is False and raw.get('failed') is False,
                'Missing successful source terminal fight and native replay')
        require(raw.get('scope') == 'ordinary-trainer-fixed-level-source-controller' and
                raw.get('host_sources') == HOSTS and raw.get('oracle_provenance') == oracle,
                'Raw case executed-host/oracle identity mismatch')
        for field, expected in identity.items():
            require(raw.get(field) == expected, 'Raw case execution mismatch: ' + field)
        require(raw.get('item') == item and raw.get('seed') == row['seed'] and
                raw.get('witness') == row.get('witness') and raw.get('winner') == row['winner'],
                'Report differs from raw case')
        require(raw.get('options', {}).get('link') == 0 and raw['options'].get('tower') == 0 and
                raw['options'].get('battle_mode') == 2, 'Case is not ordinary trainer mode')
        turns, segments = raw.get('turns', []), raw.get('recorded_segments', [])
        require(turns and len(turns) == row['turns'] and type(row.get('comparisons')) is int and
                row['comparisons'] == len(segments) > 0 and
                all(segment.get('kind') in ('segment', 'one') and segment.get('state') for segment in segments),
                'Missing actual whole-fight segment comparisons')
        handler, kind, target = CASES[item]
        require(turns[0]['actions'] == [dict(type='BagAction', item=item, stack=0, pokemon=target, move=0),
                                       dict(type='MoveAction', slot=0)] and
                turns[-1].get('terminal') is True and turns[-1].get('truncated') is False,
                'Executed first typed item target or complete fight boundary mismatch')
        require(raw['scenario']['enemy'][0]['moves'][0] == 'SPLASH' and
                raw['scenario']['bag'] == [dict(item=item, quantity=1)],
                'Source fixture is not the reviewed isolated restorative bag case')
        item_rule = rules['player-bag-item:' + item]
        identifier = item_rule['identifier']
        attributes = rom_bytes('ItemAttributes', (identifier - 1) * 6, 6)
        effect = rom_bytes('ItemEffects', identifier, 1)[0]
        address = int.from_bytes(rom_bytes('_DoItemEffect.ItemEffectRoutines', effect * 2, 2), 'little')
        require(attributes[5] & 15 == 5 and symbols[handler] == (symbols['_DoItemEffect'][0], address) and
                effect == effect_ids[item_rule['effect']], 'Assembled source item dispatch mismatch')
        metadata = dict(item=item, identifier=identifier, pocket=attributes[4] & 15, context=5,
                        parameter=attributes[3], effect=effect, handler=handler)
        require(raw['witness']['source_item'] == metadata, 'Witness source item metadata mismatch')
        require(raw['witness']['before']['status'] == expected_status.get(item, 0) and
                raw['scenario']['player'][target]['status'] == expected_status.get(item, 0),
                'Witness did not exercise the item-specific intended status')
        positive_witness(item, raw['witness'], attributes, rom_bytes)
        effect_key = 'player-bag-effect:' + item_rule['effect']
        require(effect_key in rules and item in rules[effect_key]['items'], 'Inventory effect family mismatch')
        case_rules = [item_rule['key'], effect_key]
        wram_path = Path(row['initial_wram']).resolve()
        wram_path.relative_to(args.report.parent.resolve())
        require(raw.get('initial_wram') == row['initial_wram'] and
                raw.get('initial_wram_sha256') == row['initial_wram_sha256'], 'Initial WRAM report mismatch')
        wram_ref = retain(wram_path, f'raw/initial-wram-{index}.bin', row['initial_wram_sha256'])
        record = dict(case_id=f'bag-{item.lower()}-{row["seed"]}', passed=True, unsupported=False,
            complete_fight=True, full_state_comparisons=len(segments),
            full_state_components=['cpu', 'hram', 'wram-banks-0-7', 'svbk'],
            initial_wram=wram_ref, raw_case=raw_ref, rule_keys=case_rules,
            choice_contract=dict(roles=configuration['roles'], player_bag=True))
        coverage.runtime_case_options(record, {a['path']: a for a in artifacts}, symbols,
                                      args.output, configuration, current)
        records.append(record)
        accepted_rules.update(case_rules)
    cases_ref = write('cases.json', dict(manifest_sha256=identity['manifest_sha256'],
                                        configuration=configuration, cases=records))
    adapter = dict(**identity, inventory_sha256=current['inventory_sha256'], configuration=configuration,
        acceptance_scope='conditional-native-gameplay', oracle_kind='same-presentation-hooks-actual-rom-differential',
        failed_cases=0, unsupported_cases=0, complete_fights=len(records),
        full_state_comparisons=sum(r['full_state_comparisons'] for r in records),
        rule_keys=sorted(accepted_rules), execution_artifacts=execution, oracle_artifacts=oracle_artifacts,
        case_reports=[cases_ref], artifacts=list(artifacts),
        blocked_controls=report.get('blocked_controls', []),
        excluded_corpus_cases=report.get('blocked_controls', []), limitations=report.get('limits', []),
        metadata_basis='Exact reviewed execution checkpoint and positive retained target witnesses; no negative-branch completeness claim.')
    write('adapter.json', adapter)
    ledger = dict(inventory_sha256=current['inventory_sha256'], configuration=configuration,
        acceptance_scope='conditional-native-gameplay',
        claims={key: dict(phase='tested-full-fight', evidence=['bag-family']) for key in sorted(accepted_rules)},
        evidence={'bag-family': dict(path='adapter.json', sha256=digest(args.output / 'adapter.json'))})
    verdict = coverage.evaluate(current, ledger, args.output)
    require(all(f.startswith('Required rule lacks whole-fight evidence: ') for f in verdict['failures']),
            'Strict gate rejected the retained bag adapter')
    write('ledger.json', ledger)
    write('acceptance.json', verdict)
    write('collection.json', dict(collected_cases=len(records), collected_rules=sorted(accepted_rules),
        blocked_controls=report.get('blocked_controls', []), m2_accepted=verdict['accepted']))
    print(json.dumps(dict(collected_cases=len(records), collected_rules=len(accepted_rules), m2_accepted=verdict['accepted'])))
    return 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    for name in ('inventory', 'report', 'host-checkpoint', 'manifest', 'rom', 'symbols',
                 'build-receipt', 'build-log', 'oracle-library', 'mgba-library', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    try:
        return collect(parser.parse_args())
    except (ValueError, KeyError, OSError, TypeError) as error:
        parser.exit(2, 'Bag evidence collection rejected: ' + str(error) + '\n')


if __name__ == '__main__':
    raise SystemExit(main())
