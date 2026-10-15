#!/usr/bin/env python3
"""Retain actual corpus evidence and register only witnessed whole-fight rules."""
from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import sys
from types import SimpleNamespace

import battle_coverage as coverage

ROOT = Path(__file__).resolve().parents[2]
# These reviewed implementations establish the metadata absent from old raw
# reports: modeled comparison regions, scripted actors and available bag ABI.
# A changed boundary requires review, never an optimistic metadata translation.
REVIEWED = {
    'battle_controller_check.py': 'd3621ffd1c9c142e496476e162833a91853992788d561b083b3a873d035dc9d4',
    'battle_corpus_run.py': 'a47853077c6af4eb954e47059ac1a56279af066c7e8e587bc181ea78dc60c903',
    'battle_controller_fight.py': '7bcc2cd656ea726208629c149578d1c1771586b1e8b7911d91cf17b598427771',
    'battle_controller.py': '90f7698b0c83af407e1643debf979950f75285da4be8b888ed46eb8e08b8267f',
    'battle_controller_profile.py': '6da8cfd259e9109e1e91626bef4c8d624200938132e076dd6a185592eadb39ba',
    'battle_corpus_observers.py': 'b0d64e84330b8e12fc33d23be46c2299d5ef81883623e5d95ba1154300af2680',
}
REVIEWED_ORACLE_SOURCE = '7840c8b7a31bc6431518c5c61569cf961fc82a0ae40fd365f9768ebf12648e23'


def require(condition, message):
    if not condition:
        raise ValueError(message)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read(path):
    return json.loads(path.read_text())


def collect(args):
    current = coverage.inventory(args.root)
    inventory = read(args.inventory)
    require(inventory == current, 'Inventory differs from current source inventory')
    manifest = read(args.manifest)
    manifest_sha = digest(args.manifest)
    report = read(args.report)
    require(report.get('scope') == 'bounded-controlled-source-turn-whole-fight-corpus',
            'Unsupported corpus report scope')
    require(report.get('inventory_sha256') == current['inventory_sha256'], 'Stale corpus inventory')
    for path, expected in current['source_inputs'].items():
        require(manifest.get('sources', {}).get(path) == expected, 'Manifest source mismatch: ' + path)
    for name, expected in REVIEWED.items():
        require(digest(args.runner_sources / name) == expected,
                'Collector boundary review required for runner source: ' + name)
    # Load only the hash-checked pure observer, never the native controller.
    spec = importlib.util.spec_from_file_location('retained_corpus_observers',
        args.runner_sources / 'battle_corpus_observers.py')
    observer = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = observer
    spec.loader.exec_module(observer)

    def source_constant(path, wanted):
        value = 0
        for _, code in coverage.lines(args.root, path):
            if code.startswith('const_def'):
                value = int(code[len('const_def'):].strip() or '0', 0)
            elif code.startswith('const '):
                if code.split()[1] == wanted:
                    return value
                value += 1
            elif code.startswith('const_skip'):
                value += int(code[len('const_skip'):].strip() or '1', 0)
            elif code.startswith('const_next'):
                raise ValueError('Observer binding needs review: ' + path)
        raise ValueError('Missing observer constant: ' + wanted)

    bindings = observer.source_bindings(SimpleNamespace(source_root=args.root,
        _constant=source_constant), current)
    require(args.output.resolve() not in [p.resolve() for p in
            (args.root, args.runner_sources)], 'Evidence output cannot replace source directory')
    require(not args.output.exists() or not any(args.output.iterdir()), 'Evidence output must be empty')
    args.output.mkdir(parents=True, exist_ok=True)
    artifacts = []

    def retain(path, relative, expected=None):
        payload = path.read_bytes()
        actual = hashlib.sha256(payload).hexdigest()
        require(expected is None or actual == expected, 'Artifact hash mismatch: ' + str(path))
        destination = args.output / relative
        destination.resolve().relative_to(args.output.resolve())
        destination.parent.mkdir(parents=True, exist_ok=True)
        require(not destination.exists(), 'Duplicate retained artifact: ' + relative)
        destination.write_bytes(payload)
        artifacts.append(dict(path=relative, sha256=actual))
        return relative

    def write(relative, value):
        destination = args.output / relative
        destination.write_text(json.dumps(value, indent=2) + '\n')
        artifacts.append(dict(path=relative, sha256=digest(destination)))
        return relative

    retain(args.report, 'raw/report.json')
    retain(args.inventory, 'raw/inventory.json')
    retain(args.manifest, 'execution/manifest.json', manifest_sha)
    execution = {}
    for kind, path in (('rom', args.rom), ('symbols', args.symbols),
                       ('generator', args.generator), ('build_receipt', args.build_receipt)):
        execution[kind] = retain(path, 'execution/' + kind + path.suffix,
                                 manifest.get(kind + '_sha256') if kind != 'build_receipt' else None)
    # Keep the original invocation log when the original receipt names one.
    receipt = read(args.build_receipt)
    if receipt.get('build_log_sha256'):
        require(args.build_log is not None, 'Receipt requires explicit --build-log input')
        retain(args.build_log, 'execution/original-build.log', receipt['build_log_sha256'])
    for name in REVIEWED:
        retain(args.runner_sources / name, 'runner/' + name, REVIEWED[name])
    oracle_artifacts = {
        'source': retain(args.oracle_source, 'execution/oracle-source.c', REVIEWED_ORACLE_SOURCE),
        'library': retain(args.oracle_library, 'execution/oracle-library.dylib'),
        'libmgba': retain(args.mgba_library, 'execution/libmgba.dylib'),
    }

    rows = report.get('cases', [])
    require(isinstance(rows, list) and rows, 'No corpus cases')
    inputs = {path.resolve(): path for path in args.case}
    require(len(inputs) == len(args.case), 'Duplicate case inputs')
    expected_paths = [Path(row['evidence']).resolve() for row in rows]
    require(len(set(expected_paths)) == len(rows), 'Duplicate report case references')
    require(set(expected_paths) == set(inputs), 'Explicit --case inputs must match every report case')
    records, blocked, all_rules = [], [], set()
    configuration = None
    symbols = None
    for index, row in enumerate(rows):
        path = inputs[expected_paths[index]]
        case = read(path)
        result = case.get('result', {})
        require({k: v for k, v in row.items() if k != 'evidence'} == result,
                'Report summary disagrees with raw case: ' + str(path))
        raw_reference = retain(path, f'raw/case-{index}.json')
        require(case.get('inventory_sha256') == current['inventory_sha256'] and
                case.get('source_inputs') == current['source_inputs'], 'Case source identity mismatch')
        provenance = case.get('runner_sha256', {})
        require(set(REVIEWED).issubset(provenance), 'Missing reviewed runner provenance')
        require({'stateful_fixture_check.py', 'battle_env_check.py'}.issubset(provenance),
                'Missing direct State/Hook ABI module provenance')
        for name, expected in provenance.items():
            require(Path(name).name == name and name.endswith('.py'), 'Invalid runner source name')
            reference = 'runner/' + name
            if reference not in {item['path'] for item in artifacts}:
                retain(args.runner_sources / name, reference, expected)
            else:
                require(digest(args.output / reference) == expected, 'Executed runner source changed: ' + name)
        oracle = case.get('oracle_provenance', {})
        require(oracle.get('schema') == 1 and oracle.get('kind') == 'instrumented-same-hooks-mgba',
                'Missing execution-time oracle identity; old cases require new execution')
        require(not oracle.get('libmgba_identity_error'), 'Loaded mGBA dependency was not identified')
        for kind, reference in oracle_artifacts.items():
            identity = oracle.get(kind, {})
            require(isinstance(identity.get('path'), str) and identity['path'] and
                    identity.get('sha256') == digest(args.output / reference),
                    'Executed oracle artifact mismatch: ' + kind)
        require(oracle['libmgba'].get('identity_kind') == 'loaded-GBCoreCreate-symbol-dladdr',
                'mGBA identity was not resolved from loaded dependency')
        for abi, name in (('State', 'stateful_fixture_check.py'), ('Hook', 'battle_env_check.py')):
            identity = oracle.get('abi_sources', {}).get(abi, {})
            require(identity.get('sha256') == provenance[name] and
                    isinstance(identity.get('path'), str) and identity['path'],
                    'Actual oracle ABI source mismatch: ' + abi)
        if result.get('state') != 'accepted':
            require(not result.get('accepted_rules'), 'Blocked case claims accepted rules')
            blocked.append(dict(case=result.get('case'), state=result.get('state'),
                                reason=result.get('reason'), raw_case=raw_reference))
            continue
        fight = case['fight']
        require(result.get('terminal') is True and result.get('truncated') is False and
                result.get('replay_equal') is True and case.get('native_replay_equal') is True and
                fight.get('terminal') is True and fight.get('truncated') is False and
                fight.get('failed') is False, 'Accepted case lacks successful terminal exact replay')
        require(fight.get('scope') == 'ordinary-trainer-fixed-level-source-controller', 'Wrong fight scope')
        require(fight.get('options', {}).get('link') == 0 and
                fight['options'].get('tower') == 0 and fight['options'].get('battle_mode') == 2,
                'Not an ordinary trainer fight')
        for field in ('rom_sha256', 'symbols_sha256', 'generator_sha256'):
            require(fight.get(field) == manifest.get(field), 'Fight provenance mismatch: ' + field)
        require(fight.get('manifest_sha256') == manifest_sha, 'Fight manifest identity mismatch')
        semantic = case['semantic']
        frame_index = semantic.get('frame_index')
        require(semantic.get('state') == 'activated' and type(frame_index) is int and
                0 <= frame_index < len(case.get('frames', [])), 'Missing actual activation witness')
        frame = case['frames'][frame_index]
        require(frame.get('move_execution_verified') is True and
                frame.get('phase') == 'controlled-source-turn', 'Activation lacks verified source turn')
        plan = case['case']
        moves = [r for r in current['rules'] if r['group'] == 'move' and r['name'] == plan.get('move')]
        require(len(moves) == 1, 'Case move is not uniquely present in source inventory')
        move = moves[0]
        require(plan.get('inventory_sha256') == current['inventory_sha256'] and
                plan.get('effect') == move['effect'], 'Case effect disagrees with actual source move')
        expected_rules = [move['key'], 'effect:' + move['effect']]
        require(type(plan.get('actor_side')) is int and plan['actor_side'] in (0, 1) and
                frame.get('actor_side') == plan['actor_side'] and
                frame.get('executed_move') == move['identifier'],
                'Witness actor or executed move disagrees with source-bound case')
        rules = result.get('accepted_rules', [])
        require(rules == expected_rules and rules == semantic.get('candidate_rules') and
                rules == plan.get('candidate_rules') and
                not set(rules) - {r['key'] for r in current['rules']}, 'Invalid positively observed rule keys')
        frames = [observer.MoveFrame(**{**item, 'source_commands': tuple(item['source_commands'])})
                  for item in case['frames']]
        recomputed = observer.observe_move(SimpleNamespace(**plan), frames, bindings)
        require(recomputed.state == 'activated' and recomputed.frame_index == frame_index and
                list(recomputed.candidate_rules) == expected_rules,
                'Retained observations do not reproduce declared semantic activation')
        comparisons = case.get('comparisons', [])
        require(comparisons and all(isinstance(c.get('event'), str) and
                type(c.get('div_events')) is int and c['div_events'] >= 0 for c in comparisons),
                'Missing actual differential comparison rows')
        require(case.get('recorded_segments') and fight.get('turns'), 'Missing whole-fight replay records')
        initial = case['initial_wram']
        require(initial.get('address_start') == 0xc000 and initial.get('address_end_exclusive') == 0xe000 and
                initial.get('boundary') == 'after-actual-ControllerFight.reset-before-first-actor-action',
                'Unsupported initial WRAM boundary')
        # WRAM paths come from explicitly supplied case records; retained references
        # below are always relative to the new evidence root.
        wram_ref = retain(Path(initial['path']), f'raw/initial-wram-{index}.bin', initial['sha256'])
        options = dict(abilities=True, experience=False, fixed_levels=True, player_bag=True,
                       battle_style='set', banked_wram=manifest.get('banked_wram', False),
                       lcd_disabled=manifest.get('lcd_disabled', False))
        candidate_config = dict(mode='ordinary-trainer', ruleset='default-non-faithful',
                                profile_kind='headless-iid-div', options=options,
                                roles=dict(player='scripted', enemy='scripted'))
        coverage.validate_configuration(candidate_config)
        coverage.validate_execution_profile(manifest, options)
        # Set/Switch/Predict is read from the real retained initial RAM. All other
        # required runtime flags are independently checked by runtime_case_options.
        if symbols is None:
            temporary = dict(configuration=candidate_config, execution_artifacts=execution,
                             **{field: manifest[field] for field in
                                ('rom_sha256', 'symbols_sha256', 'generator_sha256')})
            _, symbols = coverage.execution_artifacts(temporary, artifacts, args.output, manifest, current)
        wram = (args.output / wram_ref).read_bytes()
        require(len(wram) == 8192, 'Initial WRAM must contain exactly C000-DFFF')
        bank, address = symbols['wOptions2']
        require(bank == 0 and 0xc000 <= address < 0xe000, 'Unsupported battle-style symbol')
        bits = current['runtime_option_bits']
        switch, predict = (1 << bits[key] for key in ('BATTLE_SWITCH', 'BATTLE_PREDICT'))
        style = wram[address - 0xc000] & (switch | predict)
        require(style in (0, switch, predict), 'Invalid simultaneous Switch/Predict bits')
        options['battle_style'] = {0: 'set', switch: 'switch', predict: 'predict'}[style]
        if configuration is None:
            configuration = candidate_config
        require(configuration == candidate_config, 'Mixed configurations require separate collections')
        record = dict(case_id=result['case'], passed=True, unsupported=False,
                      complete_fight=True, full_state_comparisons=len(comparisons),
                      full_state_components=['cpu', 'hram', 'wram-banks-0-7', 'svbk']
                      if options['banked_wram'] else ['cpu', 'hram', 'wram-flat'],
                      initial_wram=wram_ref, rule_keys=rules, raw_case=raw_reference,
                      activation_frame_index=frame_index,
                      choice_contract=dict(roles=configuration['roles'], player_bag=True))
        coverage.runtime_case_options(record, {a['path']: a for a in artifacts}, symbols,
                                      args.output, configuration, current)
        records.append(record)
        all_rules.update(rules)
    require(records, 'No accepted cases eligible for the coverage ledger; raw blocked cases retained')
    case_report = write('cases.json', dict(manifest_sha256=manifest_sha, configuration=configuration,
                                        cases=records))
    adapter = dict(inventory_sha256=current['inventory_sha256'], configuration=configuration,
                   acceptance_scope='conditional-native-gameplay',
                   oracle_kind='same-presentation-hooks-actual-rom-differential',
                   **{field: manifest[field] for field in
                      ('rom_sha256', 'symbols_sha256', 'generator_sha256')},
                   manifest_sha256=manifest_sha, failed_cases=0, unsupported_cases=0,
                   complete_fights=len(records),
                   full_state_comparisons=sum(r['full_state_comparisons'] for r in records),
                   rule_keys=sorted(all_rules), execution_artifacts=execution,
                   oracle_artifacts=oracle_artifacts,
                   case_reports=[case_report], artifacts=list(artifacts),
                   limitations=report.get('limitations', []), excluded_corpus_cases=blocked,
                   metadata_basis='Reviewed retained runner source hashes; bag ABI availability does not claim bag activation.')
    write('adapter.json', adapter)
    ledger = dict(inventory_sha256=current['inventory_sha256'], configuration=configuration,
                  acceptance_scope='conditional-native-gameplay',
                  claims={key: dict(phase='tested-full-fight', evidence=['corpus']) for key in sorted(all_rules)},
                  evidence=dict(corpus=dict(path='adapter.json', sha256=digest(args.output / 'adapter.json'))))
    verdict = coverage.evaluate(current, ledger, args.output)
    require(not any(f.startswith(('Invalid evidence ', 'Missing valid evidence:',
                                  'No complete fight evidence:', 'Evidence does not exercise'))
                    for f in verdict['failures']), 'Collected adapter failed strict evidence validation')
    write('ledger.json', ledger)
    write('acceptance.json', verdict)
    write('collection.json', dict(collected_cases=len(records), blocked_cases=blocked,
                                collected_rules=sorted(all_rules), m2_accepted=verdict['accepted']))
    print(json.dumps(dict(collected_cases=len(records), collected_rules=len(all_rules),
                          blocked_cases=len(blocked), m2_accepted=verdict['accepted'])))
    return 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    for name in ('inventory', 'report', 'manifest', 'rom', 'symbols', 'generator', 'build-receipt',
                 'runner-sources', 'oracle-source', 'oracle-library', 'mgba-library', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--case', type=Path, action='append', required=True)
    parser.add_argument('--build-log', type=Path)
    try:
        return collect(parser.parse_args())
    except (ValueError, KeyError, OSError, TypeError) as error:
        parser.exit(2, 'Evidence collection rejected: ' + str(error) + '\n')


if __name__ == '__main__':
    raise SystemExit(main())
