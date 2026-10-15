#!/usr/bin/env python3
"""Retain only the reviewed held-restorative switch witnesses and full fights."""
from __future__ import annotations

import argparse
import json
from pathlib import Path

import battle_coverage as coverage
from battle_family_evidence import artifact_sink, bind_execution, finish, digest, read, require

ROOT = Path(__file__).resolve().parents[2]
# Explicit reviewed execution checkpoint, never inferred from current host files.
HOSTS = {
    "battle_controller_held_restorative.py": "4283f0385af519d6a29019eecf8abe3c90d21a95d091637319fb3266234d1cae",
    "battle_controller.py": "6abe731e7eac74a3cae16673c2c6339ad985ddc455a1ba913d915f2f31cda9b3",
    "battle_controller_profile.py": "9cf85ef64a22eda7781b739119d4f9eb958a860453af12189900f7b84d13e33d",
    "battle_controller_fight.py": "eb92e12bc7d1100da6e90e6382c04e8b32ce769c7747c2f679818774569deb79",
    "battle_controller_fight_check.py": "065bba593a79d3103de9d8d752b067ba9c63ec7116c9f4edbd7aa62c30eda249",
    "battle_controller_check.py": "d3621ffd1c9c142e496476e162833a91853992788d561b083b3a873d035dc9d4",
    "battle_controller_oracle.c": "7840c8b7a31bc6431518c5c61569cf961fc82a0ae40fd365f9768ebf12648e23",
    "battle_decisions.py": "9aea66f9c5ba912da9032a503e5a3f8399ff25d42f3deec58068724fc0225d4c",
    "battle_scenarios.py": "39b61a8bd9175f610b3ad63a3167117ca3038016f73621721168f29b5e44eee9",
    "battle_bag.py": "0f77ff9fb79a8c2d58f5d1effc3f1faf731477300847f2270165091dffad95c8",
    "battle_actions.py": "09e4aa5358ac724d8c3b8c6d4209ca1aff1f6a9b7f6c92fd80a9a26a135d39cd",
    "battle_env.py": "2c281bda28d290c8cb7e4e608907a69e0786ef4f72ed5e46cc28b130781865ee",
    "battle_env_check.py": "f9403ece032b28de048a8e20469823005a870eba8ed7c6627a5a65a3792b63b7",
    "stateful_lifting.py": "5e5febf491dca885db415475d4b2c34a5a50c2d150e6ca21bcb998401dc9eade",
    "stateful_fixture_check.py": "8a01e4700625e9a8a015aa6bbc525caf070ad9d3fb84664bd7386e4be7dc3d87",
}
CASES = {
    'oran': ('ORAN_BERRY', 'HELD_BERRY', 'hp', ''),
    'sitrus': ('SITRUS_BERRY', 'HELD_BERRY', 'hp', ''),
    'leftovers': ('LEFTOVERS', 'HELD_LEFTOVERS', 'residual', ''),
    'cheri': ('CHERI_BERRY', 'HELD_HEAL_STATUS', 'status', 'PAR'),
    'chesto': ('CHESTO_BERRY', 'HELD_HEAL_STATUS', 'status', 'SLP'),
    'pecha': ('PECHA_BERRY', 'HELD_HEAL_STATUS', 'status', 'TOXIC'),
    'rawst': ('RAWST_BERRY', 'HELD_HEAL_STATUS', 'status', 'BRN'),
    'aspear': ('ASPEAR_BERRY', 'HELD_HEAL_STATUS', 'status', 'FRZ'),
    'lum': ('LUM_BERRY', 'HELD_HEAL_STATUS', 'status', 'TOXIC'),
    'leppa': ('LEPPA_BERRY', 'HELD_RESTORE_PP', 'pp', ''),
    'leftovers-full': ('LEFTOVERS', 'HELD_LEFTOVERS', 'no-trigger', ''),
}
GAME_SOURCES = {
    'constants/item_constants.asm', 'constants/item_data_constants.asm',
    'constants/battle_constants.asm', 'data/items/attributes.asm',
    'data/pokemon/base_stats/horsea.asm', 'engine/battle/core.asm',
    'engine/battle/endturn.asm', 'engine/battle/effect_commands.asm', 'home/battle.asm',
}


def constants(root, path, stop):
    # Same source numbering rules as the existing bag collector.
    values, number = {}, 0
    for _, code in coverage.lines(root, path):
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


def validate_witness(name, holder, raw, row, rules, ids, effects, flags, abilities, rom_bytes):
    item, effect_name, kind, status = CASES[name]
    flag = lambda name: 1 << flags[name]
    expected_status = (flag('PSN') | flag('TOX') if status == 'TOXIC' else
                       2 if status == 'SLP' else flag(status) if status else 0)
    identifier = ids[item]
    attributes = rom_bytes('ItemAttributes', (identifier - 1) * 6, 6)
    item_rule = rules['item:' + item]
    require(item_rule['identifier'] == identifier and item_rule['held_effect'] == effect_name,
            'Source inventory held-item identity mismatch')
    parameter = (7 if status == 'SLP' else flag(status) if status in ('PAR', 'BRN', 'FRZ') else
                 flag('PSN') if item == 'PECHA_BERRY' else
                 255 if item in ('LUM_BERRY', 'LEPPA_BERRY') else
                 int(item_rule['parameter'], 0))
    require(attributes[2] == effects[effect_name] and attributes[3] == parameter,
            'Assembled held effect or source parameter mismatch')
    metadata = dict(item=item, identifier=identifier, effect=effects[effect_name],
        effect_name=effect_name, parameter=parameter, pocket=attributes[4] & 15,
        context=attributes[5] & 15)
    w = raw['witness']
    require(w == row['witness'] and w.get('source_item') == metadata and
            w.get('scope') == 'actual-source-reserve-switch-restoration' and
            w.get('holder') == holder and w.get('index') == 1 and
            w.get('opponent_action') == 'SPLASH' and
            w.get('boundary') == 'pre-switch frozen decision to first post-switch frozen decision' and
            w.get('control') is (kind == 'no-trigger'), 'Invalid typed held-item witness')
    old, new = w['before'], w['after']
    require(old['index'] == new['index'] == 1 and old['active'] is False and new['active'] is True and
            old['item'] == identifier and old['status'] == expected_status and
            old['ability'] == new['ability'] == abilities['SWIFT_SWIM'],
            'Held-item holder, precondition or neutral ability mismatch')
    for field in ('species', 'form', 'egg', 'level', 'nature', 'ability_slot', 'happiness',
                  'dvs', 'moves', 'pp_ups', 'MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef'):
        require(old[field] == new[field], 'Unexpected held-item identity/stat change: ' + field)
    require(len(old['moves']) == len(old['pp']) == len(new['pp']) == 4 and
            all(0 <= mon['HP'] <= mon['MaxHP'] for mon in (old, new)), 'Invalid held-item vectors/HP')
    consumed = kind in ('hp', 'status', 'pp')
    require(w['consumed'] == int(consumed) and new['item'] == (0 if consumed else identifier),
            'Held restorative consumption mismatch')
    residual = max(1, old['MaxHP'] // 16) if status in ('TOXIC', 'BRN') else 0
    require(w['expected_pre_cure_residual'] == residual, 'Residual ordering witness mismatch')
    if kind in ('hp', 'residual'):
        amount = (max(1, old['MaxHP'] // 4) if item == 'SITRUS_BERRY' else
                  max(1, old['MaxHP'] // 16) if kind == 'residual' else parameter)
        require(old['HP'] == 10 and old['HP'] <= old['MaxHP'] // 2 and
                new['HP'] == min(old['MaxHP'], old['HP'] + amount) and new['HP'] > old['HP'],
                'Incorrect positive held HP restoration')
    else:
        require(new['HP'] == old['HP'] - residual, 'Held status/PP/control changed HP incorrectly')
    if kind == 'status':
        require(old['status'] != 0 and new['status'] == 0, 'Missing positive held status cure')
    else:
        require(old['status'] == new['status'], 'Non-status held item changed status')
    if kind == 'pp':
        move = rom_bytes('Moves', (old['moves'][0] - 1) * 8, 8)
        require(move[0] == old['moves'][0] and old['pp'][0] == 0 and
                new['pp'] == [min(10, move[5]), *old['pp'][1:]] and old['pp'] != new['pp'],
                'Incorrect positive Leppa restoration')
    else:
        require(old['pp'] == new['pp'], 'Non-PP held item changed PP')
    if kind == 'no-trigger':
        require(old['HP'] == old['MaxHP'] == new['HP'], 'Full-HP control is not unchanged')
    require(set(w['stored']) == {'party', 'battle'}, 'Missing party/active cache witness')
    for cache in w['stored'].values():
        require(all(cache[field] == new[field] for field in ('item', 'HP', 'status', 'pp')),
                'Held restorative party/active cache mismatch')
    teams = [raw['scenario']['player'], raw['scenario']['enemy']]
    require(len(teams[holder]) == 2 and len(teams[1-holder]) == 1 and
            raw['scenario']['bag'] == [] and raw['scenario']['inventory'] == [None, None],
            'Held fixture teams/inventory differ from reviewed driver')
    for side, team in enumerate(teams):
        for index, mon in enumerate(team):
            require(mon['species'] == 'HORSEA' and mon['moves'] == ['SPLASH', 'WATER_GUN'] and
                    mon['level'] == 50 and mon['status'] == (expected_status if (side, index) == (holder, 1) else 0) and
                    mon['item'] == (item if (side, index) == (holder, 1) else 'NO_ITEM'),
                    'Held fixture no longer isolates its named item')
    actions = [dict(type='MoveAction', slot=0), dict(type='MoveAction', slot=0)]
    actions[holder] = dict(type='SwitchAction', index=1)
    require(raw['turns'][0]['actions'] == actions, 'Actual first held switch/target/Splash mismatch')
    key = 'held-effect:' + effect_name
    require(item in rules[key]['items'], 'Held effect inventory membership mismatch')
    return [] if kind == 'no-trigger' else [key]


def collect(args):
    current = coverage.inventory(args.root)
    require(read(args.inventory) == current, 'Current inventory mismatch')
    checkpoint = read(args.host_checkpoint / 'identity.json')
    require(checkpoint.get('verified') is True and
            checkpoint.get('source_report_sha256') == '6e850f5b74148db6a84435680f7caf553e80febe4d5adf4e90332a8b97781468' and
            {name: entry['sha256'] for name, entry in checkpoint['files'].items()} == HOSTS,
            'Held executed checkpoint requires review')
    for name, expected in HOSTS.items():
        require(digest(args.host_checkpoint / name) == expected, 'Executed held host missing or changed: ' + name)
    reports = [read(path) for path in args.report]
    require(reports and len(set(p.resolve() for p in args.report)) == len(reports), 'Duplicate/no held reports')
    first = reports[0]
    retain, write, artifacts = artifact_sink(args.output)
    retain(args.inventory, 'raw/inventory.json')
    retain(args.host_checkpoint / 'identity.json', 'host/identity.json')
    for name, expected in HOSTS.items():
        retain(args.host_checkpoint / name, 'host/' + name, expected)
    manifest, execution, oracle, oracle_artifacts, configuration, identity, symbols = bind_execution(
        args, current, first, HOSTS, retain, artifacts)
    rom = args.rom.read_bytes()

    def rom_bytes(label, offset, length):
        bank, address = symbols[label]
        require(0 <= address + offset < address + offset + length <= (0x4000 if bank == 0 else 0x8000),
                'ROM table lookup crosses bank boundary: ' + label)
        start = bank * 0x4000 + address - (0x4000 if bank else 0) + offset
        value = rom[start:start + length]
        require(len(value) == length, 'Missing assembled table bytes')
        return value

    rules = {rule['key']: rule for rule in current['rules']}
    ids = constants(args.root, 'constants/item_constants.asm', 'DEF NUM_ITEMS')
    effects = constants(args.root, 'constants/item_data_constants.asm', 'DEF NUM_HELD_ITEM_TYPES')
    flags = constants(args.root, 'constants/battle_constants.asm', 'unused-stop-marker')
    abilities = constants(args.root, 'constants/ability_constants.asm', 'DEF NUM_ABILITIES')
    # Sleep parameter is an expression in source, not a const entry.
    require(any(code == 'DEF SLP_MASK EQU %00000111' or code == 'DEF SLP_MASK EQU %111'
                for _, code in coverage.lines(args.root, 'constants/battle_constants.asm')),
            'Sleep mask requires source review')
    records, excluded, seen, accepted = [], [], set(), set()
    index = 0
    for report_path, report in zip(args.report, reports):
        require(report.get('scope') == 'actual-source-held-restorative-switch-full-fights' and
                report.get('passed') is True and report.get('host_sources') == HOSTS and
                report.get('oracle_provenance') == oracle and report.get('game_sources') == first.get('game_sources'),
                'Held report scope/host/Oracle/source mismatch')
        require(all(report.get(field) == expected for field, expected in identity.items()),
                'Mixed held execution identities')
        require(set(report['game_sources']) == GAME_SOURCES, 'Missing held game source identities')
        for name, expected in report['game_sources'].items():
            require(digest(args.root / name) == expected == manifest['sources'].get(name),
                    'Held executed game source changed: ' + name)
        retain(report_path, f'raw/report-{index}.json')
        require(report.get('cases'), 'No held cases')
        for row in report['cases']:
            name, holder, seed = row['case'], row['holder'], row['seed']
            key = (name, holder, seed)
            require(name in CASES and type(holder) is int and holder in (0, 1) and
                    type(seed) is int and key not in seen, 'Unknown/duplicate held case')
            seen.add(key)
            path = Path(row['evidence']).resolve()
            path.relative_to(report_path.parent.resolve())
            raw = read(path)
            raw_ref = retain(path, f'raw/case-{index}.json', row['evidence_sha256'])
            require(raw.get('host_sources') == HOSTS and raw.get('oracle_provenance') == oracle and
                    raw.get('game_sources') == report['game_sources'] and
                    all(raw.get(field) == expected for field, expected in identity.items()) and
                    raw.get('case') == name and raw.get('holder') == holder and raw.get('seed') == seed,
                    'Held raw/report identity mismatch')
            require(row.get('passed') is True and row.get('replay_equal') is True and
                    raw.get('terminal') is True and raw.get('truncated') is False and raw.get('failed') is False and
                    raw.get('scope') == 'ordinary-trainer-fixed-level-source-controller' and
                    raw.get('winner') == row['winner'], 'Held terminal/exact replay missing')
            require(raw.get('options', {}).get('link') == 0 and raw['options'].get('tower') == 0 and
                    raw['options'].get('battle_mode') == 2, 'Held case is not ordinary trainer mode')
            turns, segments, comparisons = raw.get('turns', []), raw.get('recorded_segments', []), raw.get('comparisons', [])
            require(turns and len(turns) == row['turns'] and turns[-1].get('terminal') is True and
                    turns[-1].get('truncated') is False and type(row.get('comparisons')) is int and
                    row['comparisons'] == len(segments) == len(comparisons) > 0 and
                    all(s.get('kind') in ('segment', 'one') and s.get('state') for s in segments) and
                    all(isinstance(c.get('event'), str) and type(c.get('div_events')) is int and c['div_events'] >= 0
                        for c in comparisons), 'Missing whole-fight held differential records')
            case_rules = validate_witness(name, holder, raw, row, rules, ids, effects, flags, abilities, rom_bytes)
            wram_path = Path(row['initial_wram']).resolve()
            wram_path.relative_to(report_path.parent.resolve())
            require(raw.get('initial_wram') == row['initial_wram'] and
                    raw.get('initial_wram_sha256') == row['initial_wram_sha256'], 'Held initial WRAM mismatch')
            wram_ref = retain(wram_path, f'raw/initial-wram-{index}.bin', row['initial_wram_sha256'])
            record = dict(case_id=f'held-{name}-{holder}-{seed}', passed=True, unsupported=False,
                complete_fight=True, full_state_comparisons=len(segments),
                full_state_components=['cpu', 'hram', 'wram-banks-0-7', 'svbk'],
                initial_wram=wram_ref, raw_case=raw_ref, rule_keys=case_rules,
                choice_contract=dict(roles=configuration['roles'], player_bag=True))
            coverage.runtime_case_options(record, {a['path']: a for a in artifacts}, symbols,
                                          args.output, configuration, current)
            if case_rules:
                records.append(record)
                accepted.update(case_rules)
            else:
                excluded.append(dict(case=record['case_id'], state='control',
                    reason='Full-HP Leftovers did not activate', raw_case=raw_ref))
            index += 1
    require(records, 'No positively activated held cases')
    cases_ref = write('cases.json', dict(manifest_sha256=identity['manifest_sha256'], configuration=configuration, cases=records))
    adapter = dict(**identity, inventory_sha256=current['inventory_sha256'], configuration=configuration,
        acceptance_scope='conditional-native-gameplay', oracle_kind='same-presentation-hooks-actual-rom-differential',
        failed_cases=0, unsupported_cases=0, complete_fights=len(records),
        full_state_comparisons=sum(r['full_state_comparisons'] for r in records), rule_keys=sorted(accepted),
        execution_artifacts=execution, oracle_artifacts=oracle_artifacts, case_reports=[cases_ref], artifacts=list(artifacts),
        blocked_controls=excluded, excluded_corpus_cases=excluded,
        limitations=first.get('limits', []) + ['Other-party invariance relies on frozen executed driver assertions; no missing snapshots reconstructed.',
            'Lum confusion, Unnerve, theft, orbs and other combinations are unproved.'],
        metadata_basis='Reviewed held driver checkpoint plus independent source/ROM restoration witness checks.')
    return finish(current, configuration, adapter, 'held-restorative', args.output, write,
        dict(collected_cases=len(records), executed_cases=index, collected_rules=sorted(accepted), blocked_controls=excluded))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    parser.add_argument('--report', type=Path, action='append', required=True)
    for name in ('inventory', 'host-checkpoint', 'manifest', 'rom', 'symbols', 'build-receipt',
                 'build-log', 'oracle-library', 'mgba-library', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    try:
        return collect(parser.parse_args())
    except (ValueError, KeyError, OSError, TypeError, IndexError) as error:
        parser.exit(2, 'Held evidence collection rejected: ' + str(error) + '\n')


if __name__ == '__main__':
    raise SystemExit(main())
