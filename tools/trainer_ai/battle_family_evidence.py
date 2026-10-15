"""Shared byte retention and execution binding for bag and held collectors only."""
from __future__ import annotations

import hashlib
import json
from pathlib import PurePosixPath

import battle_coverage as coverage


def require(condition, message):
    if not condition:
        raise ValueError(message)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read(path):
    return json.loads(path.read_text())


def artifact_sink(output):
    require(not output.exists() or not any(output.iterdir()), 'Output must be empty')
    output.mkdir(parents=True, exist_ok=True)
    artifacts = []

    def retain(path, relative, expected=None):
        payload = path.read_bytes()
        actual = coverage.sha(payload)
        require(expected is None or actual == expected, 'Retained bytes mismatch: ' + str(path))
        reference = PurePosixPath(relative)
        require(not reference.is_absolute() and '..' not in reference.parts and
                str(reference) == relative, 'Noncanonical artifact path: ' + relative)
        destination = output / relative
        destination.resolve().relative_to(output.resolve())
        require(not destination.exists(), 'Retained path collision: ' + relative)
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(payload)
        artifacts.append(dict(path=relative, sha256=actual))
        return relative

    def write(relative, value):
        reference = PurePosixPath(relative)
        require(not reference.is_absolute() and '..' not in reference.parts and
                str(reference) == relative, 'Noncanonical artifact path: ' + relative)
        path = output / relative
        path.resolve().relative_to(output.resolve())
        require(not path.exists(), 'Retained path collision: ' + relative)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(value, indent=2) + '\n')
        artifacts.append(dict(path=relative, sha256=digest(path)))
        return relative

    return retain, write, artifacts


def bind_execution(args, current, report, hosts, retain, artifacts):
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
            oracle['source'].get('sha256') == hosts['battle_controller_oracle.c'], 'Oracle C source mismatch')
    for abi, name in (('State', 'stateful_fixture_check.py'), ('Hook', 'battle_env_check.py')):
        require(oracle.get('abi_sources', {}).get(abi, {}).get('path') and
                oracle['abi_sources'][abi].get('sha256') == hosts[name], 'Oracle ABI mismatch: ' + abi)
    require(manifest.get('banked_wram') is True and manifest.get('lcd_disabled') is True,
            'Family checkpoint requires its actual banked WRAM and disabled LCD profile')
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
    return manifest, execution, oracle, oracle_artifacts, configuration, identity, symbols


def finish(current, configuration, adapter, label, output, write, collection):
    write('adapter.json', adapter)
    ledger = dict(inventory_sha256=current['inventory_sha256'], configuration=configuration,
        acceptance_scope='conditional-native-gameplay',
        claims={key: dict(phase='tested-full-fight', evidence=[label]) for key in adapter['rule_keys']},
        evidence={label: dict(path='adapter.json', sha256=digest(output / 'adapter.json'))})
    verdict = coverage.evaluate(current, ledger, output)
    require(all(f.startswith('Required rule lacks whole-fight evidence: ') for f in verdict['failures']),
            'Strict gate rejected the retained family adapter')
    write('ledger.json', ledger)
    write('acceptance.json', verdict)
    write('collection.json', dict(collection, m2_accepted=verdict['accepted']))
    print(json.dumps(dict(collected_cases=collection['collected_cases'],
        collected_rules=len(adapter['rule_keys']), m2_accepted=verdict['accepted'])))
    return 0
