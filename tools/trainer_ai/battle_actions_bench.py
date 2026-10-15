"""One bounded serial native-only complete-fight throughput sample."""
import argparse
import hashlib
import json
import pathlib
import statistics
import time
from battle_actions import SwitchAction, ItemAction
from battle_env import MoveAction
from battle_actions_edges import make_env


def main(root):
    started = time.perf_counter()
    env = make_env(root)
    cold = time.perf_counter() - started
    durations, episodes = [], []
    for seed in range(20):
        env.team_indices = ((0, 1), (0, 2), (1, 2))[seed % 3]
        started = time.perf_counter()
        env.reset(seed=193 + seed, orientation=seed % 2)
        while not env.terminal and not env.truncated:
            # Both public observations and legal sets precede either action.
            observations = [env.observation(side) for side in (0, 1)]
            legal = [env.legal_actions(side) for side in (0, 1)]
            if env.fight.turn == 0:
                selected = (SwitchAction(1), ItemAction(0))
            else:
                selected = tuple(MoveAction((env.fight.turn + side) % 2) for side in (0, 1))
            actions = tuple(action if action in legal[side] else legal[side][0]
                            for side, action in enumerate(selected))
            env.step(actions)
        durations.append(time.perf_counter() - started)
        assert env.terminal and not env.truncated
        episodes.append(dict(seed=seed + 193, teams=env.team_indices,
                             orientation=seed % 2, turns=env.fight.turn, winner=env.winner))
    result = dict(profile='experimental-headless-iid-div-v1', cpu_workers=1, gpu=False,
                  cold_verification_seconds=cold, fights=len(episodes), episodes=episodes,
                  total_rollout_seconds=sum(durations), median_fight_seconds=statistics.median(durations),
                  fights_per_second=len(episodes) / sum(durations),
                  includes='reset, observations, legal masks, scripted action selection, complete native fights',
                  excludes='learning, policy inference, oracle, ordinary frontend equivalence')
    (root / 'build/battle-actions/benchmark.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    main(parser.parse_args().artifact_root)
