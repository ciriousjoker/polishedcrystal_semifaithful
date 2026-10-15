#!/usr/bin/env python3
"""Prepared-only experimental headless NativeFight plumbing pilot.

No ordinary-game equivalence or trained-strength claim. The required profile
labels the synthetic IID-DIV/no-frame event law. No GPU or workers.
"""
import os
for _variable in ('OMP_NUM_THREADS', 'OPENBLAS_NUM_THREADS', 'MKL_NUM_THREADS',
                  'VECLIB_MAXIMUM_THREADS', 'NUMEXPR_NUM_THREADS'):
    os.environ[_variable] = '1'

import argparse
import hashlib
import json
import math
import pathlib
import random
import struct

N, H, A = 16, 16, 12
SCOPE = 'experimental-headless-iid-div-v1'
SCHEMA = {
    'id': 'policy-pilot-16-v1', 'shape': [N, H, A],
    'features': ['own_HP/MaxHP', 'opponent_HP/MaxHP'] +
                [f'{side}_{stat}/255' for side in ('own', 'opponent')
                 for stat in ('Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')] +
                [f'{side}_PP{slot}/63' for side in ('own', 'opponent') for slot in (0, 1)],
    'scaling': 'floor(15 * ratio), clipped to [0,15]',
    'actions': 'slots 0 and 1 only; outputs 2 through 11 are always masked',
    'gradient': 'REINFORCE, terminal +/-1 only, SGD momentum; explicitly approximate STE',
    'ste': 'rounding derivative 1; hidden derivative 1/8 inside unsaturated [0,15] range',
    'integer': 'signed int4 weights; int16 bias clipped +/-16384; hidden floor shift 3',
    'limitations': ['provisional feature schema; no empirical ablation',
                    'omitted VBlank can change RNG counters; not ordinary-game faithful',
                    'uniform private DIV byte law is experimental'],
}


def digest(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def canonical(value):
    return json.dumps(value, sort_keys=True, separators=(',', ':'))


def identity(args):
    here = pathlib.Path(__file__).resolve().parent
    names = ('policy_pilot.py', 'network_runtime_check.py', 'battle_env.py',
             'battle_env_profile.py', 'stateful_lifting.py', 'stateful_fixture_check.py')
    return dict(scope=SCOPE, fixture='Smeargle Own Tempo, Tackle/Water Gun, closed 1v1', schema=SCHEMA,
                files={name: digest(here / name) for name in names},
                rom=digest(args.rom), symbols=digest(args.symbols),
                native_manifest=digest(args.build_dir / 'manifest.json'),
                source_root=str(args.source_root.resolve()),
                seed=args.seed, train_fights=args.train_fights,
                heldout_pairs=args.heldout_pairs, learning_rate=args.learning_rate,
                momentum=0.9, temperature=16, max_turns=64)


def features(observation):
    side = observation['side']
    own = observation['teams'][side][0]
    other = observation['teams'][side ^ 1][0]
    ratios = [(own['HP'], own['MaxHP']), (other['HP'], other['MaxHP'])]
    ratios += [(mon[stat], 255) for mon in (own, other)
               for stat in ('Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')]
    ratios += [(pp, 63) for mon in (own, other) for pp in mon['pp']]
    return [max(0, min(15, 15 * value // denominator)) for value, denominator in ratios]


def quantize(values, low, high):
    return [max(low, min(high, math.floor(float(value) + 0.5))) for value in values]


def parameters(model):
    return (quantize(model['weights'], -8, 7),
            quantize(model['biases'], -16384, 16384), [3] * H)


def export(model):
    from network_runtime_check import model_bytes
    return model_bytes(N, H, A, *parameters(model))


def decode(payload):
    count = N * H + H * A
    expected = 16 + count // 2 + (H + A) * 2 + H
    if len(payload) != expected or payload[:16] != b'TAI4' + bytes([1, N, H, A]) + bytes(8):
        raise ValueError('Unexpected TAI4 model shape/header/size')
    weights = []
    for value in payload[16:16 + count // 2]:
        for nibble in (value & 15, value >> 4):
            weights.append(nibble if nibble < 8 else nibble - 16)
    offset = 16 + count // 2
    biases = list(struct.unpack('<' + 'h' * (H + A), payload[offset:offset + (H + A) * 2]))
    shifts = list(payload[-H:])
    if any(abs(x) > 16384 for x in biases) or any(x > 15 for x in shifts):
        raise ValueError('Invalid bias or shift')
    return weights, biases, shifts


def integer_forward(params, inputs, legal):
    # Separate implementation checked against the existing runtime reference.
    weights, biases, shifts = params
    hidden = [max(0, min(15, (biases[j] + sum(inputs[i] * weights[j*N+i]
              for i in range(N))) // (1 << shifts[j]))) for j in range(H)]
    logits = [biases[H+j] + sum(hidden[i] * weights[N*H+j*H+i] for i in range(H))
              for j in range(A)]
    allowed = [slot for slot in range(A) if legal & (1 << slot)]
    return hidden, logits, max(allowed, key=lambda slot: logits[slot]) if allowed else 255


def check_export(model):
    from network_runtime_check import reference
    payload = export(model)
    params = decode(payload)
    if params != parameters(model):
        raise AssertionError('Export roundtrip differs')
    rng = random.Random(64172)
    for inputs in ([0]*N, [15]*N, *[[rng.randrange(16) for _ in range(N)] for _ in range(32)]):
        for mask in (0, 1, 2, 3, 0xfff):
            if integer_forward(params, inputs, mask) != reference(N, H, A, *params, inputs, mask):
                raise AssertionError('Exported inference differs from network_runtime_check reference')
    return payload


def initial_model(seed):
    rng = random.Random(seed)
    count = N * H + H * A
    return dict(weights=[rng.uniform(-1, 1) for _ in range(count)],
                biases=[24.0] * H + [0.0] * A,
                velocity_weights=[0.0] * count, velocity_biases=[0.0] * (H+A))


def sample_policy(model, inputs, legal, rng):
    params = parameters(model)
    hidden, logits, _ = integer_forward(params, inputs, sum(1 << a for a in legal))
    top = max(logits[a] for a in legal)
    exp = [math.exp((logits[a] - top) / 16) for a in legal]
    total = sum(exp)
    probabilities = [value / total for value in exp]
    draw, cumulative = rng.random(), 0.0
    chosen = legal[-1]
    for action, probability in zip(legal, probabilities):
        cumulative += probability
        if draw < cumulative:
            chosen = action
            break
    return chosen, (inputs, hidden, legal, probabilities, chosen)


def episode(args, model, rng, seed, orientation, policies=None, runtime_probes=None):
    from battle_env import UnsupportedFight
    trajectories = [[], []]
    try:
        fight = args.engine
        fight.reset(seed=seed, orientation=orientation)
        while not fight.terminal and not fight.truncated:
            # Both precommit observations and legal masks are captured together.
            observations = [fight.observation(side) for side in (0, 1)]
            legal = [fight.legal_actions(side) for side in (0, 1)]
            if any(not actions for actions in legal):
                return dict(outcome='unsupported', reason='empty-precommit-mask'), trajectories
            choices = []
            for side in (0, 1):
                slots = [action.slot for action in legal[side]]
                inputs = features(observations[side])
                if runtime_probes is not None and fight.turn < 2:
                    key = (orientation, side, fight.turn)
                    if not any(tuple(case['key']) == key for case in runtime_probes):
                        runtime_probes.append(dict(key=list(key), seed=seed,
                            observation=observations[side], inputs=inputs,
                            legal=sum(1 << slot for slot in slots)))
                if policies is None:
                    chosen, record = sample_policy(model, inputs, slots, rng)
                    trajectories[side].append(record)
                else:
                    policy = policies[side]
                    if isinstance(policy, int):
                        chosen = policy if policy in slots else slots[0]
                    else:
                        chosen = integer_forward(policy, inputs, sum(1 << a for a in slots))[2]
                choices.append(next(action for action in legal[side] if action.slot == chosen))
            fight.step(tuple(choices))
            fight.events.clear()  # bounded trajectory; no retained native event tapes
        if fight.terminal:
            if fight.winner not in (0, 1):
                raise ValueError('Terminal fight without valid winner')
            return dict(outcome='terminal', winner=fight.winner, turns=fight.turn), trajectories
        return dict(outcome='truncated', reason=fight.truncation_reason, turns=fight.turn), trajectories
    except UnsupportedFight as error:
        return dict(outcome='unsupported', reason=str(error)), [[], []]


def update(model, trajectories, winner, learning_rate, np):
    # Exact quantized forward above, approximate STE backward below. Both actors
    # receive only their final whole-fight outcome, never transition damage labels.
    weights, biases, _ = parameters(model)
    w1 = np.array(weights[:N*H], dtype=float).reshape(H, N)
    w2 = np.array(weights[N*H:], dtype=float).reshape(A, H)
    gw1, gw2 = np.zeros_like(w1), np.zeros_like(w2)
    gb = np.zeros(H+A)
    for side, records in enumerate(trajectories):
        reward = 1.0 if winner == side else -1.0
        for inputs, hidden, legal, probabilities, chosen in records:
            x, h = np.array(inputs), np.array(hidden)
            dlogits = np.zeros(A)
            for slot, probability in zip(legal, probabilities):
                dlogits[slot] = reward * ((slot == chosen) - probability) / 16
            gw2 += np.outer(dlogits, h)
            gb[H:] += dlogits
            preactivation = w1 @ x + np.array(biases[:H])
            dh = (w2.T @ dlogits) * ((preactivation > 0) & (preactivation < 128)) / 8
            gw1 += np.outer(dh, x)
            gb[:H] += dh
    gradients = (np.concatenate((gw1.ravel(), gw2.ravel())), gb)
    for key, velocity, gradient, low, high in (
            ('weights', 'velocity_weights', gradients[0], -8, 7),
            ('biases', 'velocity_biases', gradients[1], -16384, 16384)):
        change = 0.9 * np.array(model[velocity]) + gradient
        model[velocity] = change.tolist()
        model[key] = np.clip(np.array(model[key]) + learning_rate * change, low, high).tolist()


def tuple_tree(value):
    return tuple(tuple_tree(item) for item in value) if isinstance(value, list) else value


def save(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + '.tmp')
    temporary.write_text(json.dumps(value, indent=2) + '\n')
    temporary.replace(path)


def next_update(args, checkpoint, np):
    state = json.loads(canonical(checkpoint))
    rng = random.Random()
    rng.setstate(tuple_tree(state['rng']))
    index = state['next_fight']
    # Private event seeds are disjoint from heldout seeds by construction.
    seed = 2 * (args.seed + index + 1)
    result, trajectories = episode(args, state['model'], rng, seed, index % 2)
    if result['outcome'] == 'terminal':
        update(state['model'], trajectories, result['winner'], args.learning_rate, np)
    state['results'].append(dict(index=index, seed=seed, orientation=index % 2, **result))
    state['next_fight'] = index + 1
    state['rng'] = rng.getstate()
    return state


def evaluate(args, initial, trained):
    models = {'initial': decode(initial), 'trained': decode(trained),
              'fixed_slot_0': 0, 'fixed_slot_1': 1}
    rows, runtime_probes = [], []
    comparisons = [('trained', other) for other in ('initial', 'fixed_slot_0', 'fixed_slot_1')]
    comparisons += [('initial', other) for other in ('fixed_slot_0', 'fixed_slot_1')]
    for first, second in comparisons:
        for index in range(args.heldout_pairs):
            seed = 2 * (args.seed + index + 1) + 1
            for orientation in (0, 1):
                for first_side in (0, 1):
                    names = [first, second] if first_side == 0 else [second, first]
                    result, _ = episode(args, None, None, seed, orientation,
                                        policies=[models[name] for name in names], runtime_probes=runtime_probes)
                    rows.append(dict(policies=names, seed=seed, orientation=orientation, **result))
    return dict(scope=SCOPE, inference='frozen exported integer argmax; lowest-slot ties',
                provenance={'initial': hashlib.sha256(initial).hexdigest(),
                            'trained': hashlib.sha256(trained).hexdigest(),
                            'fixed_slot_0': 'script: prefer legal slot 0, fallback first legal',
                            'fixed_slot_1': 'script: prefer legal slot 1, fallback first legal'},
                seed_split='even train; odd heldout; each heldout seed in both orientations and sides',
                rows=rows, runtime_probes=runtime_probes)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('rom', 'symbols', 'source-root', 'build-dir', 'output'):
        parser.add_argument('--' + name, type=pathlib.Path, required=True)
    parser.add_argument('--profile', choices=[SCOPE], required=True)
    parser.add_argument('--seed', type=int, default=7123)
    parser.add_argument('--train-fights', type=int, default=256)
    parser.add_argument('--heldout-pairs', type=int, default=8)
    parser.add_argument('--learning-rate', type=float, default=0.02)
    parser.add_argument('--resume', type=pathlib.Path)
    parser.add_argument('--stop-after', type=int)
    parser.add_argument('--resume-check', action='store_true')
    parser.add_argument('--identity-only', action='store_true')
    args = parser.parse_args()
    if not 1 <= args.train_fights <= 256 or not 1 <= args.heldout_pairs <= 32 or args.seed < 0:
        parser.error('Pilot bounds: train 1..256, heldout 1..32, seed >=0')
    if not math.isfinite(args.learning_rate) or not 0 < args.learning_rate <= 1:
        parser.error('learning-rate must be finite and in (0,1]')
    ident = identity(args)
    if args.identity_only:
        print(json.dumps(dict(identity=ident, identity_sha256=hashlib.sha256(canonical(ident).encode()).hexdigest()), indent=2))
        return
    import numpy as np
    from battle_env import NativeFight
    # One verification at run start; inputs must remain frozen until completion.
    args.engine = NativeFight(args.rom, args.symbols, args.source_root, args.build_dir,
                              seed=args.seed, orientation=0, max_turns=64)
    if args.resume:
        checkpoint = json.loads(args.resume.read_text())
        if checkpoint['identity'] != ident:
            raise ValueError('Checkpoint identity changed; refusing resume')
    else:
        model = initial_model(args.seed)
        checkpoint = dict(identity=ident, model=model, initial_model=model,
                          rng=random.Random(args.seed).getstate(), next_fight=0, results=[])
    if args.resume_check:
        first = next_update(args, checkpoint, np)
        copy_path = args.output / 'resume-probe.json'
        save(copy_path, checkpoint)
        second = next_update(args, json.loads(copy_path.read_text()), np)
        if canonical(first) != canonical(second):
            raise AssertionError('Serialized resume differs from uninterrupted next update')
        save(args.output / 'resume-check.json', dict(identity=ident, passed=True,
             next_update_sha256=hashlib.sha256(canonical(first).encode()).hexdigest()))
    limit = args.train_fights if args.stop_after is None else min(args.stop_after, args.train_fights)
    while checkpoint['next_fight'] < limit:
        checkpoint = next_update(args, checkpoint, np)
        save(args.output / 'checkpoint.json', checkpoint)
    if checkpoint['next_fight'] < args.train_fights:
        return
    initial = check_export(checkpoint['initial_model'])
    trained = check_export(checkpoint['model'])
    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / 'initial.tai4').write_bytes(initial)
    (args.output / 'trained.tai4').write_bytes(trained)
    report = evaluate(args, initial, trained)
    report.update(identity=ident, training=checkpoint['results'],
                  export_reference_check='passed; existing Python reference, not new ROM oracle run',
                  claim='Plumbing only; no convergence, ordinary-game fidelity, or strength claim')
    save(args.output / 'evaluation.json', report)


if __name__ == '__main__':
    main()
