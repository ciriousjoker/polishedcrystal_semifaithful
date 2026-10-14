#!/usr/bin/env python3
"""Small synthetic-only rating checks. These do not evaluate a trained policy."""
import copy
import unittest

from ratings import compare


def fixture():
    provenance = dict(rules_sha256='a' * 64, suite_sha256='b' * 64,
                      protocol_sha256='c' * 64, policy_format='exported-integer')
    models = {m: dict(policy_sha256=str(i) * 64, checkpoint='synthetic-' + m)
              for i, m in enumerate(('baseline', 'strong', 'weak', 'isolated'), 1)}
    data = dict(dataset_kind='synthetic', provenance=provenance, models=models,
                reference='baseline', matches=[])
    for m, wins in (('strong', 8), ('weak', 2)):
        for i in range(10):
            data['matches'].append(dict(a=m, b='baseline', scenario_id='case-' + str(i),
                result='a_win' if i < wins else 'b_win', provenance=provenance.copy(),
                policy_a_sha256=models[m]['policy_sha256'], policy_b_sha256=models['baseline']['policy_sha256']))
    return data


class SyntheticChecks(unittest.TestCase):
    def test_known_order_and_coverage(self):
        result = compare(fixture())
        ratings = result['ratings']
        self.assertGreater(ratings['strong']['rating'], 1000)
        self.assertLess(ratings['weak']['rating'], 1000)
        self.assertEqual(ratings['baseline']['rating'], 1000)
        self.assertIsNone(ratings['isolated']['rating'])
        self.assertFalse(ratings['isolated']['reference_connected'])
        self.assertEqual(ratings['strong']['scenario_clusters'], 10)
        self.assertEqual(result['dataset_kind'], 'synthetic')

    def test_input_order_and_orientation(self):
        data = fixture()
        original = compare(data)
        data['matches'].reverse()
        for match in data['matches']:
            match['a'], match['b'] = match['b'], match['a']
            match['policy_a_sha256'], match['policy_b_sha256'] = match['policy_b_sha256'], match['policy_a_sha256']
            match['result'] = {'a_win': 'b_win', 'b_win': 'a_win', 'draw': 'draw'}[match['result']]
        actual = compare(data)
        self.assertEqual(original['ratings'], actual['ratings'])

    def test_repeats_do_not_create_independent_evidence(self):
        data = fixture()
        original = compare(data)
        data['matches'] *= 20
        repeated = compare(data)
        for m in ('strong', 'weak', 'baseline'):
            for key in ('rating', 'scenario_clusters', 'scenario_score_cluster_bootstrap_95'):
                self.assertEqual(original['ratings'][m][key], repeated['ratings'][m][key])

    def test_draws_and_all_win_separation(self):
        data = fixture()
        for match in data['matches']:
            match['result'] = 'draw'
        self.assertEqual(compare(data)['ratings']['strong']['rating'], 1000)
        for match in data['matches']:
            match['result'] = 'a_win'
        result = compare(data)
        self.assertLess(result['ratings']['strong']['rating'], 2000)
        self.assertTrue(result['ratings']['strong']['boundary_scores_only'])

    def test_disconnected_pool(self):
        data = fixture()
        data['models']['other'] = dict(policy_sha256='e' * 64, checkpoint='synthetic-other')
        match = copy.deepcopy(data['matches'][0])
        match.update(a='isolated', b='other', policy_a_sha256='4' * 64, policy_b_sha256='e' * 64)
        data['matches'].append(match)
        result = compare(data)
        self.assertFalse(result['ratings']['other']['reference_connected'])
        self.assertIsNone(result['ratings']['other']['scenario_score_cluster_bootstrap_95'])

    def test_balanced_swaps_and_cycle(self):
        data = fixture()
        data['matches'] = []
        for a, b in (('baseline', 'strong'), ('strong', 'weak'), ('weak', 'baseline')):
            for i in range(3):
                data['matches'].append(dict(a=a, b=b, scenario_id='cycle-' + str(i),
                    result='a_win', provenance=data['provenance'].copy(),
                    policy_a_sha256=data['models'][a]['policy_sha256'],
                    policy_b_sha256=data['models'][b]['policy_sha256']))
                data['matches'].append(dict(a=b, b=a, scenario_id='cycle-' + str(i),
                    result='b_win', provenance=data['provenance'].copy(),
                    policy_a_sha256=data['models'][b]['policy_sha256'],
                    policy_b_sha256=data['models'][a]['policy_sha256']))
        result = compare(data)
        for model in ('baseline', 'strong', 'weak'):
            self.assertEqual(result['ratings'][model]['rating'], 1000)
        for pair in result['pair_coverage']:
            self.assertEqual(pair['both_a_b_orientations'], 3)
            self.assertEqual(pair['fitted_a_score'], .5)
            self.assertIn(pair['mean_a_score'], (0., 1.))

    def test_invalid_provenance(self):
        for field in ('rules_sha256', 'suite_sha256', 'protocol_sha256', 'policy_format'):
            data = fixture()
            data['matches'][0]['provenance'][field] = 'different'
            with self.assertRaises(ValueError):
                compare(data)
        data = fixture()
        data['matches'][0]['policy_a_sha256'] = 'f' * 64
        with self.assertRaises(ValueError):
            compare(data)


if __name__ == '__main__':
    unittest.main()
