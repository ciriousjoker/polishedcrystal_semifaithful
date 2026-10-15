#!/usr/bin/env python3
"""Join validated retained coverage collections without rerunning their fights."""
from __future__ import annotations

import argparse
from copy import deepcopy
import json
from pathlib import Path, PurePosixPath

import battle_coverage as coverage

ROOT = Path(__file__).resolve().parents[2]


def require(condition, message):
    if not condition:
        raise ValueError(message)


def read(path):
    return json.loads(path.read_text())


def reference(root, relative):
    require(isinstance(relative, str) and relative, 'Missing artifact reference')
    path = PurePosixPath(relative)
    require(not path.is_absolute() and '..' not in path.parts and str(path) == relative,
            'Artifact reference must be canonical and root-relative: ' + relative)
    resolved = (root / relative).resolve()
    resolved.relative_to(root.resolve())
    return resolved


def validate(current, ledger, root):
    verdict = coverage.evaluate(current, ledger, root)
    invalid = [failure for failure in verdict['failures']
               if not failure.startswith('Required rule lacks whole-fight evidence: ')]
    require(not invalid, 'Invalid retained collection: ' + '; '.join(invalid))
    require(ledger.get('acceptance_scope') == 'conditional-native-gameplay',
            'Merge supports only conditional native gameplay evidence')
    return verdict


def merge(args):
    current = coverage.inventory(args.root)
    roots = [root.resolve() for root in args.collection]
    require(len(set(roots)) == len(roots), 'Duplicate input collection')
    output = args.output.resolve()
    for root in roots:
        require(not output.is_relative_to(root), 'Output cannot be inside an input collection')
    require(not output.exists() or not any(output.iterdir()), 'Output must be empty')
    # Validate every input before copying. Missing mechanic rows are the only
    # expected gate failures; invalid evidence and configuration never pass.
    ledger_bytes = [(root / 'ledger.json').read_bytes() for root in roots]
    ledgers = [json.loads(payload) for payload in ledger_bytes]
    for root, ledger in zip(roots, ledgers):
        validate(current, ledger, root)
    require(ledgers, 'At least one collection is required')
    for ledger in ledgers[1:]:
        for field in ('inventory_sha256', 'configuration', 'acceptance_scope'):
            require(ledger.get(field) == ledgers[0].get(field), 'Mixed collection ' + field)
    output.mkdir(parents=True, exist_ok=True)
    written = {}

    def save(relative, payload):
        destination = reference(output, relative)
        actual = coverage.sha(payload)
        if relative in written:
            require(written[relative] == actual, 'Conflicting retained path: ' + relative)
        else:
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.write_bytes(payload)
            written[relative] = actual
        return dict(path=relative, sha256=actual)

    def write(relative, value):
        return save(relative, (json.dumps(value, indent=2) + '\n').encode())

    combined = {field: deepcopy(ledgers[0][field]) for field in
                ('inventory_sha256', 'configuration', 'acceptance_scope')}
    combined.update(claims={}, evidence={})
    origins, excluded = [], []
    for index, (root, ledger) in enumerate(zip(roots, ledgers)):
        prefix = f'collections/{index}'
        names = {}
        require((root / 'ledger.json').read_bytes() == ledger_bytes[index], 'Input ledger changed during merge')
        original_ledger = save(prefix + '/ledger.json', ledger_bytes[index])
        for evidence_index, (name, entry) in enumerate(ledger.get('evidence', {}).items()):
            merged_name = f'collection-{index}-evidence-{evidence_index}'
            names[name] = merged_name
            retained = {}

            def keep(relative, expected=None):
                source = reference(root, relative)
                payload = source.read_bytes()
                require(expected is None or coverage.sha(payload) == expected,
                        'Changed original artifact: ' + str(source))
                artifact = save(prefix + '/' + relative, payload)
                retained[artifact['path']] = artifact
                return artifact

            origin_adapter = keep(entry['path'], entry['sha256'])
            original = read(reference(output, origin_adapter['path']))
            retained[original_ledger['path']] = original_ledger
            for artifact in original['artifacts']:
                keep(artifact['path'], artifact['sha256'])
            derived = deepcopy(original)
            case_reports = []
            for case_index, relative in enumerate(original.get('case_reports', [])):
                source_report = read(reference(output, prefix + '/' + relative))
                case_report = deepcopy(source_report)
                case_report['origin'] = retained[prefix + '/' + relative]
                for case in case_report['cases']:
                    for field in ('initial_wram', 'raw_case'):
                        if field in case:
                            require(prefix + '/' + case[field] in retained,
                                    'Case refers to unretained original bytes: ' + case[field])
                            case[field] = prefix + '/' + case[field]
                artifact = write(f'derived/{index}-{evidence_index}-cases-{case_index}.json', case_report)
                retained[artifact['path']] = artifact
                case_reports.append(artifact['path'])
            derived['case_reports'] = case_reports
            for field in ('execution_artifacts', 'oracle_artifacts'):
                if field in derived:
                    for kind, relative in derived[field].items():
                        require(prefix + '/' + relative in retained,
                                'Adapter refers to unretained execution bytes: ' + relative)
                        derived[field][kind] = prefix + '/' + relative
            for case in derived.get('excluded_corpus_cases', []):
                if 'raw_case' in case:
                    require(prefix + '/' + case['raw_case'] in retained,
                            'Excluded case lacks retained original bytes')
                    case['raw_case'] = prefix + '/' + case['raw_case']
                excluded.append(deepcopy(case))
            derived['origin'] = origin_adapter
            derived['origin_ledger'] = original_ledger
            derived['derivation'] = 'Artifact reference rebasing only; no new execution or rule promotion.'
            derived['artifacts'] = list(retained.values())
            artifact = write(f'derived/{index}-{evidence_index}-adapter.json', derived)
            combined['evidence'][merged_name] = artifact
        for key, claim in ledger.get('claims', {}).items():
            phase = claim.get('phase', 'unproven')
            incoming = deepcopy(claim)
            incoming['phase'] = phase
            incoming['evidence'] = [names[name] for name in claim.get('evidence', [])]
            previous = combined['claims'].get(key)
            if previous is None or coverage.PHASES.index(phase) > coverage.PHASES.index(previous['phase']):
                combined['claims'][key] = incoming
            elif phase == previous['phase']:
                previous['evidence'] = sorted(set(previous.get('evidence', []) + incoming['evidence']))
        origins.append(dict(collection_index=index, source_directory=str(root), ledger=original_ledger))
    verdict = validate(current, combined, output)
    write('ledger.json', combined)
    write('acceptance.json', verdict)
    write('merge.json', dict(derived=True, original_collections=origins,
                            distinct_claimed_rules=len(combined['claims']),
                            phase_counts=verdict['phase_counts'], excluded_corpus_cases=excluded,
                            m2_accepted=verdict['accepted'],
                            limitations=['No fights rerun; original evidence bytes are retained unchanged.',
                                         'Conditional native gameplay does not establish frontend parity.',
                                         'Required rules missing from the input collections remain unproven.']))
    (output / 'COVERAGE.md').write_text(coverage.markdown(current, verdict, combined))
    print(json.dumps(dict(collections=len(roots), distinct_claimed_rules=len(combined['claims']),
                          excluded_cases=len(excluded), m2_accepted=verdict['accepted'])))
    return 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    parser.add_argument('--collection', type=Path, action='append', required=True)
    parser.add_argument('--output', type=Path, required=True)
    try:
        return merge(parser.parse_args())
    except (ValueError, KeyError, OSError, TypeError) as error:
        parser.exit(2, 'Coverage merge rejected: ' + str(error) + '\n')


if __name__ == '__main__':
    raise SystemExit(main())
