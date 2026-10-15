#!/usr/bin/env python3
"""Small single-thread complete-fight benchmark for the conditional headless profile."""
import argparse
import dataclasses
import hashlib
import json
import pathlib
import statistics
import time

from battle_env import NativeFight, MoveAction


def policy(observation):
    """Fixed rough physical/special preference, not a learned or exact-damage actor."""
    side = observation['side']
    user = observation['teams'][side][0]
    foe = observation['teams'][side ^ 1][0]
    preferred = 0 if 3 * user['Attack'] * foe['SpDef'] >= 2 * user['SpAtk'] * foe['Defense'] else 1
    if not user['pp'][preferred]:
        preferred ^= 1
    return MoveAction(preferred)


def run(args):
    start = time.perf_counter()
    fight = NativeFight(args.rom, args.sym, args.source_root, args.build_dir)
    cold = time.perf_counter() - start
    durations = []
    turns = attacks = 0
    winners = [0, 0]
    action_counts = [0, 0]
    start = time.perf_counter()
    for episode in range(100):
        begin = time.perf_counter()
        fight.reset(seed=123 + episode, orientation=episode % 2)
        while not fight.terminal and not fight.truncated:
            actions = tuple(policy(fight.observation(side)) for side in (0, 1))
            for action in actions:
                action_counts[action.slot] += 1
            fight.step(actions)
        assert fight.terminal and not fight.truncated
        winners[fight.winner] += 1
        turns += fight.turn
        attacks += len(fight.events)
        durations.append(time.perf_counter() - begin)
    elapsed = time.perf_counter() - start
    result = dict(scope='conditional instrumented headless fights, no frontend/distribution fidelity claim',
                  episodes=100, threads=1, gpu=False, cold_verified_engine_seconds=cold,
                  reused_engine_seconds=elapsed, fights_per_second=100 / elapsed,
                  median_fight_ms=1000 * statistics.median(durations),
                  min_fight_ms=1000 * min(durations), max_fight_ms=1000 * max(durations),
                  turns=turns, attacks=attacks, winners=winners,
                  chosen_action_counts=action_counts,
                  includes='reset, observations, two fixed heuristic decisions, all actual native order/move calls, event generation and frontend boundary handling',
                  excludes='cold source/ROM verification and dynamic loading; emulator is never called',
                  environment_sha256=hashlib.sha256(pathlib.Path(__file__).with_name('battle_env.py').read_bytes()).hexdigest())
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result))


if __name__ == '__main__':
    p = argparse.ArgumentParser()
    p.add_argument('--rom', type=pathlib.Path, required=True)
    p.add_argument('--sym', type=pathlib.Path, required=True)
    p.add_argument('--source-root', type=pathlib.Path, required=True)
    p.add_argument('--build-dir', type=pathlib.Path, required=True)
    p.add_argument('--output', type=pathlib.Path, required=True)
    run(p.parse_args())
