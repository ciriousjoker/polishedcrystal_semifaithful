#!/usr/bin/env python3
"""Experimental 1v1 initial/trained exported-policy comparison only."""
import argparse
import hashlib
import json
import pathlib
from collections import Counter, defaultdict

from policy_pilot import SCOPE, canonical, digest, save
from ratings import compare


def sha(value):
    return hashlib.sha256(canonical(value).encode()).hexdigest()


def run(directory):
    evaluation = json.loads((directory / 'evaluation.json').read_text())
    checkpoint = json.loads((directory / 'checkpoint.json').read_text())
    if evaluation['scope'] != SCOPE or checkpoint['identity'] != evaluation['identity']:
        raise ValueError('Experimental evaluation/checkpoint identity mismatch')
    models = {}
    for name in ('initial', 'trained'):
        policy_sha = digest(directory / (name + '.tai4'))
        if policy_sha != evaluation['provenance'][name]:
            raise ValueError('Export artifact changed: ' + name)
        models[name] = dict(policy_sha256=policy_sha, checkpoint=sha(dict(
            identity=checkpoint['identity'], model=checkpoint['initial_model' if name == 'initial' else 'model'],
            fight_index=0 if name == 'initial' else checkpoint['next_fight'])))
    rows = [row for row in evaluation['rows'] if set(row['policies']) == {'initial', 'trained'}]
    suite = sorted({(row['seed'], row['orientation']) for row in rows})
    identity = evaluation['identity']
    rules = dict(scope=SCOPE, rom=identity['rom'], symbols=identity['symbols'],
                 native_manifest=identity['native_manifest'],
                 environment_sources={name: value for name, value in identity['files'].items()
                                      if name in ('battle_env.py', 'battle_env_profile.py',
                                                  'stateful_lifting.py', 'stateful_fixture_check.py')},
                 event_law='private IID uniform DIV bytes, no frame/VBlank advancement',
                 fixture=identity['fixture'])
    protocol = dict(inference=evaluation['inference'], seed_split=evaluation['seed_split'],
                    schema=identity['schema'], roles='each scenario both actor roles',
                    scenario_cluster='private event seed; both stat orientations and actor roles within cell',
                    outcome='terminal only; exclude entire incomplete scenario cluster',
                    fixed_schedule=[dict(seed=row['seed'], orientation=row['orientation'],
                                         policies=row['policies']) for row in rows])
    provenance = dict(rules_sha256=sha(rules), suite_sha256=sha(suite),
                      protocol_sha256=sha(protocol), policy_format='exported-integer')
    grouped = defaultdict(list)
    for row in rows:
        grouped[row['seed']].append(row)
    matches, excluded = [], []
    for seed, group in sorted(grouped.items()):
        schedule = {(row['orientation'], tuple(row['policies'])) for row in group}
        expected_schedule = {(orientation, roles) for orientation in (0, 1)
                             for roles in (('initial', 'trained'), ('trained', 'initial'))}
        if (len(group) != 4 or schedule != expected_schedule or
                any(row['outcome'] != 'terminal' for row in group)):
            excluded.append(dict(seed=seed, outcomes=group))
            continue
        for row in group:
            a, b = row['policies']
            matches.append(dict(a=a, b=b, provenance=provenance,
                policy_a_sha256=models[a]['policy_sha256'], policy_b_sha256=models[b]['policy_sha256'],
                scenario_id=f'seed:{seed}',
                result='a_win' if row['winner'] == 0 else 'b_win'))
    dataset = dict(dataset_kind='evaluation', provenance=provenance, models=models,
                   reference='initial', matches=matches)
    save(directory / 'ratings-input.json', dataset)
    fitted = compare(dataset) if matches else None
    scripts = {}
    for actor in ('initial', 'trained'):
        for script in ('fixed_slot_0', 'fixed_slot_1'):
            counts = Counter()
            for row in evaluation['rows']:
                if set(row['policies']) != {actor, script}:
                    continue
                if row['outcome'] == 'terminal':
                    counts['wins' if row['policies'][row['winner']] == actor else 'losses'] += 1
                else:
                    counts[row['outcome']] += 1
            terminal = counts['wins'] + counts['losses']
            scripts[actor + '_vs_' + script] = dict(counts=counts,
                terminal_winrate=counts['wins']/terminal if terminal else None)
    result = dict(scope=SCOPE, claim='Experimental synthetic IID-DIV/no-frame 1v1 only; '
                  'no ordinary-game Elo or strength claim; no hyperparameter search',
                  initial_anchor=1000, exports_identical=models['initial']['policy_sha256'] == models['trained']['policy_sha256'],
                  rules=rules, suite=suite, protocol=protocol, fitted=fitted,
                  excluded_scenario_clusters=excluded, scripts_plain_winrates=scripts,
                  source_sha256={str(path): digest(path) for path in
                                 (pathlib.Path(__file__), pathlib.Path(__file__).with_name('ratings.py'))},
                  evaluation_sha256=digest(directory/'evaluation.json'),
                  checkpoint_sha256=digest(directory/'checkpoint.json'))
    save(directory / 'ratings.json', result)
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--directory', type=pathlib.Path, required=True)
    args = parser.parse_args()
    result = run(args.directory)
    print(json.dumps(dict(scope=result['scope'], exports_identical=result['exports_identical'],
                          ratings=result['fitted']['ratings'] if result['fitted'] else None,
                          scripts=result['scripts_plain_winrates'])))
