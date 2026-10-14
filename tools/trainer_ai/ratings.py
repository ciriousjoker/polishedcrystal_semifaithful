#!/usr/bin/env python3
"""Provenance-checked, scenario-balanced Bradley-Terry comparison, standard library only."""
import argparse
import hashlib
import json
import math
import random
from collections import defaultdict

SCALE = 400.0 / math.log(10.0)


def _hash(value):
    return isinstance(value, str) and len(value) == 64 and all(c in '0123456789abcdef' for c in value)


def compare(data):
    """Fit deterministic regularized batch ratings; never infer evaluator fidelity."""
    if data.get('dataset_kind') not in ('synthetic', 'evaluation'):
        raise ValueError('dataset_kind must be synthetic or evaluation')
    provenance = data.get('provenance', {})
    for key in ('rules_sha256', 'suite_sha256', 'protocol_sha256'):
        if not _hash(provenance.get(key)):
            raise ValueError('missing or invalid provenance ' + key)
    if provenance.get('policy_format') != 'exported-integer':
        raise ValueError('evaluate exported-integer policies')
    models = data.get('models', {})
    if not models or not all(isinstance(m, str) and m for m in models):
        raise ValueError('models must have nonempty string IDs')
    for model in models.values():
        if not isinstance(model, dict) or not _hash(model.get('policy_sha256')):
            raise ValueError('each model needs a policy_sha256')
        if not isinstance(model.get('checkpoint'), str) or not model['checkpoint']:
            raise ValueError('each model needs a fixed checkpoint identifier')
    reference = data.get('reference')
    if reference not in models:
        raise ValueError('reference must identify a fixed baseline model')
    groups = defaultdict(list)
    raw = defaultdict(int)
    graph = {m: set() for m in models}
    orientation = defaultdict(set)
    for match in data.get('matches', []):
        a, b = match.get('a'), match.get('b')
        if a not in models or b not in models or a == b:
            raise ValueError('matches need distinct known a and b model IDs')
        if match.get('provenance') != provenance:
            raise ValueError('mixed or missing match provenance')
        if match.get('policy_a_sha256') != models[a]['policy_sha256'] or match.get('policy_b_sha256') != models[b]['policy_sha256']:
            raise ValueError('match policy hashes do not match fixed checkpoints')
        scenario = match.get('scenario_id')
        if not isinstance(scenario, str) or not scenario:
            raise ValueError('each match needs a scenario_id clustering all repeats and swaps')
        result = match.get('result')
        if result not in ('a_win', 'draw', 'b_win'):
            raise ValueError('result must be a_win, draw or b_win')
        score = {'a_win': 1., 'draw': .5, 'b_win': 0.}[result]
        lo, hi = sorted((a, b))
        groups[(lo, hi, scenario)].append(score if a == lo else 1. - score)
        orientation[(lo, hi)].add((scenario, a))
        raw[lo] += 1
        raw[hi] += 1
        graph[lo].add(hi)
        graph[hi].add(lo)
    if not groups:
        raise ValueError('no whole-fight outcomes supplied')
    # One unit of fit weight per pair/scenario, regardless of repeated seeds/swaps.
    edges = [(a, b, scenario, math.fsum(sorted(scores)) / len(scores))
             for (a, b, scenario), scores in sorted(groups.items())]
    components = []
    unseen = set(models)
    while unseen:
        todo = [min(unseen)]
        seen = set()
        while todo:
            m = todo.pop()
            if m not in seen:
                seen.add(m)
                todo.extend(graph[m] - seen)
        unseen -= seen
        components.append(sorted(seen))
    ratings = {}
    fits = []
    for component in components:
        anchor = reference if reference in component else component[0]
        z = dict.fromkeys(component, 0.)
        local = [edge for edge in edges if edge[0] in z]
        # MAP with N(0, SCALE**2) prior relative to fixed component anchor.
        # A finite prior makes all-win pools finite, but is not evidence of strength.
        for iteration in range(1000):
            largest = 0.
            for m in component:
                if m == anchor:
                    continue
                gradient, curvature = z[m], 1.
                for a, b, _, score in local:
                    if m not in (a, b):
                        continue
                    p = 1. / (1. + math.exp(max(-700., min(700., z[b] - z[a]))))
                    sign = 1. if m == a else -1.
                    gradient += sign * (p - score)
                    curvature += p * (1. - p)
                delta = gradient / curvature
                z[m] -= delta
                largest = max(largest, abs(delta))
            if largest < 1e-10:
                break
        if largest >= 1e-10:
            raise ValueError('rating solver did not converge')
        fits.append({'models': component, 'anchor': anchor, 'reference_connected': reference in component,
                     'iterations': iteration + 1})
        for m in component:
            by_scenario = defaultdict(list)
            for a, b, scenario, score in local:
                if m in (a, b):
                    by_scenario[scenario].append(score if m == a else 1. - score)
            values = [math.fsum(by_scenario[s]) / len(by_scenario[s]) for s in sorted(by_scenario)]
            interval = None
            if len(values) >= 2:
                rng = random.Random(0)
                samples = sorted(math.fsum(rng.choice(values) for _ in values) / len(values) for _ in range(500))
                interval = [samples[12], samples[487]]
            ratings[m] = {'rating': round(1000. + SCALE * z[m], 3) if values else None,
                          'reference_connected': reference in component,
                          'opponents': sorted(graph[m]), 'raw_fights': raw[m],
                          'pair_scenario_cells': sum(m in edge[:2] for edge in local),
                          'scenario_clusters': len(values),
                          'mean_scenario_score': math.fsum(values) / len(values) if values else None,
                          'scenario_score_cluster_bootstrap_95': interval,
                          'boundary_scores_only': bool(values) and all(v == values[0] and v in (0., 1.) for v in values)}
    pairs = []
    for a, b in sorted(orientation):
        scenarios = sorted(s for x, y, s in groups if (x, y) == (a, b))
        paired = sum((s, a) in orientation[(a, b)] and (s, b) in orientation[(a, b)] for s in scenarios)
        pairs.append({'a': a, 'b': b, 'scenario_clusters': len(scenarios), 'both_a_b_orientations': paired,
                      'mean_a_score': math.fsum(e[3] for e in edges if e[:2] == (a, b)) / len(scenarios),
                      'fitted_a_score': 1. / (1. + 10. ** ((ratings[b]['rating'] - ratings[a]['rating']) / 400.))})
    canonical = json.dumps(data, sort_keys=True, separators=(',', ':')).encode()
    return {'schema_version': 1, 'dataset_kind': data['dataset_kind'],
            'quality_claim': 'synthetic only, no trained-strength evidence' if data['dataset_kind'] == 'synthetic' else 'conditional on supplied evaluator fidelity and held-out protocol',
            'input_sha256': hashlib.sha256(canonical).hexdigest(), 'provenance': provenance, 'models': models,
            'method': 'batch Bradley-Terry, 400/ln(10) scale, 1000 anchor, unit Gaussian logit prior, pair/scenario balanced',
            'ratings': ratings, 'components': fits, 'pair_coverage': pairs,
            'uncertainty': 'Intervals describe scenario-average observed scores, not Elo posteriors or independent repeated fights. Bootstrap assumes independent scenario clusters and fixed opponents; fewer than two clusters yields null. Boundary intervals can collapse and do not establish certainty.',
            'comparability': 'Disconnected components have arbitrary local anchors and cannot be ranked against each other. Ratings are prior-dependent and pool-dependent. Pair residuals and cycles require inspection.'}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('input', help='JSON containing fixed-policy whole-fight evaluation outcomes')
    parser.add_argument('--output', help='result JSON path, otherwise stdout')
    args = parser.parse_args()
    try:
        with open(args.input, encoding='utf-8') as source:
            result = compare(json.load(source))
    except (ValueError, TypeError, KeyError, AttributeError) as error:
        parser.error(str(error))
    output = json.dumps(result, indent=2, sort_keys=True) + '\n'
    if args.output:
        with open(args.output, 'w', encoding='utf-8') as destination:
            destination.write(output)
    else:
        print(output, end='')


if __name__ == '__main__':
    main()
