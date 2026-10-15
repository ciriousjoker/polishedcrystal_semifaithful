#!/usr/bin/env python3
"""Bounded synthetic rollout scheduling, with no learner integration."""
# Set limits before importing worker or optional learning libraries.
import os
for variable in ('OMP_NUM_THREADS', 'OPENBLAS_NUM_THREADS', 'MKL_NUM_THREADS',
                 'VECLIB_MAXIMUM_THREADS', 'NUMEXPR_NUM_THREADS'):
    os.environ[variable] = '1'
import argparse
import concurrent.futures
import hashlib
import json
from pathlib import Path
import subprocess
import threading

_ACTIVE = 0
_PEAK = 0
_ACTIVITY_LOCK = threading.Lock()


def run_batch(executable, count, seed):
    global _ACTIVE, _PEAK
    with _ACTIVITY_LOCK:
        _ACTIVE += 1
        _PEAK = max(_PEAK, _ACTIVE)
    try:
        result = subprocess.run([executable, str(count), str(seed)], check=True,
                                capture_output=True, text=True, timeout=120)
        return dict(seed=seed, fights=count, outcomes=json.loads(result.stdout))
    finally:
        with _ACTIVITY_LOCK:
            _ACTIVE -= 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--simulator', required=True)
    parser.add_argument('--workers', type=int, default=1)
    parser.add_argument('--max-inflight', type=int, default=1,
                        help='must be at least --workers to fill every worker')
    parser.add_argument('--batches', type=int, default=4)
    parser.add_argument('--fights-per-batch', type=int, default=250)
    parser.add_argument('--seed', type=int, default=1)
    parser.add_argument('--learner-backend', choices=['none', 'cpu', 'cuda', 'mps'], default='none',
                        help='record intended learner backend; no learner is loaded')
    args = parser.parse_args()
    if not (1 <= args.workers <= 64 and 1 <= args.max_inflight <= 64
            and 1 <= args.batches <= 1000000
            and 1 <= args.fights_per_batch <= 1000000 and args.seed > 0
            and args.seed + args.batches * args.fights_per_batch < 2**32):
        parser.error('invalid resource or seed bounds')
    executable = str(Path(args.simulator).resolve(strict=True))
    digest = hashlib.sha256(Path(executable).read_bytes()).hexdigest()
    print(json.dumps(dict(kind='synthetic-scheduler-provenance',
                          executable_sha256=digest, workers=args.workers,
                          max_inflight=args.max_inflight, learner_backend=args.learner_backend,
                          learner_enabled=False,
                          rules_profile='17-move synthetic slice; no full ROM parity',
                          objective='whole-fight outcomes; no training',
                          seed=args.seed)), flush=True)
    # Threads supervise single-thread native children. Pending tasks are capped,
    # and completed output streams immediately instead of accumulating in RAM.
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.workers) as pool:
        pending = set()
        submitted = 0
        while submitted < args.batches or pending:
            while submitted < args.batches and len(pending) < args.max_inflight:
                seed = args.seed + submitted * args.fights_per_batch
                pending.add(pool.submit(run_batch, executable,
                                        args.fights_per_batch, seed))
                submitted += 1
            done, pending = concurrent.futures.wait(
                pending, return_when=concurrent.futures.FIRST_COMPLETED)
            for future in done:
                print(json.dumps(future.result(), sort_keys=True), flush=True)
    print(json.dumps(dict(kind='scheduler-summary', peak_active_batches=_PEAK)), flush=True)


if __name__ == '__main__':
    main()
