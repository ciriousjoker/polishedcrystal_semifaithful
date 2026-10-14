# Trainer AI feasibility tools

Exploratory desktop tools for a hardware-aware neural trainer controller. These
are not connected to the game build and contain no trained battle policy. The
provisional observation width is a numerical test dimension, not an approved
selection of game information. The deployment target is at most 10 KiB total
added ROM for weights, inference, multiplication tables and integration data.

## Exact integer inference

`inference.c` implements a 64 -> 96 -> 12 numerical fixture: signed packed
4-bit weights, unsigned 4-bit activations, bounded signed biases, explicit
floor/clip semantics and a portable binary format. Twelve output slots map to
four moves, six party destinations and two trainer consumables. An explicit
legal-action mask controls selection; battle-correct masks still need an
adapter. Replacement uses only destination slots. See `inference.md` for ROM,
RAM, latency and unimplemented integration costs.

`inference_sm83.asm` is an actual assembled dot-product routine using 512 bytes
of signed-product tables. `inference_check.py` executes the routine's bounded
opcode subset with SM83 instruction timings, checks products/carries and
compares the desktop network with an independent integer oracle. It is not a
complete emulator or a physical hardware benchmark, and it is never used for
training rollouts.

From the repository root, using Python 3, RGBDS and a C compiler:

```sh
python3 -B tools/trainer_ai/inference_check.py --build-dir /tmp/polished-ai-inference
cc -std=c11 -O3 -Wall -Wextra -Werror tools/trainer_ai/inference.c -o /tmp/polished-ai-inference/native
/tmp/polished-ai-inference/native bench
/tmp/polished-ai-inference/native export /tmp/polished-ai-inference/untrained.tai
/tmp/polished-ai-inference/native load /tmp/polished-ai-inference/untrained.tai
```

These commands require no learning libraries or GPU. Microbenchmarks use an
untrained fixture and cannot establish strategic quality. A complete GBC
network driver, feature extractor, banking and integration are still pending.

## Native simulation slice

`extract_simulator_data.py` checks a deliberately narrow default non-FAITHFUL
source profile and generates `simulator_data.h` with all 255 move entries, a
support mask, type table and a digest of its source inputs. This is not an
arbitrary assembly parser. Regenerate when those data change; compile-time
variants need separate extraction and validation.

`simulator.c` runs native synthetic 6-vs-6 fights without an emulator. It supports
17 ordinary damaging moves, source-aware integer damage/STAB/type/critical
stages and PP. Synthetic states have no abilities, items, statuses, stages or
weather; switches only replace a fainted member with the next alive slot.
Unsupported moves and PP exhaustion are rejected. A turn limit reports
truncation separately from wins. No real-fight importer exists. This program
must not be used as a faithful full-game training environment.

```sh
python3 -B tools/trainer_ai/extract_simulator_data.py
cc -std=c11 -O3 -Wall -Wextra -Werror tools/trainer_ai/simulator.c -o /tmp/polished-ai-simulator
/tmp/polished-ai-simulator --self-test
/tmp/polished-ai-simulator --features
/tmp/polished-ai-simulator 200000
```

Reported fights/s are requested synthetic rollouts, including initialization;
inspect completed, rejected and truncated counts. Timings omit neural policy
evaluation and cannot predict full-mechanics training throughput. The RNG has
independent native streams and does not reproduce ROM seeds or draw order.

## Next correctness boundary

Before learning a policy, define the approved observation visibility and exact
scenario/mechanics scope, implement all needed rules and verify against ROM
transition fixtures with controlled random events. Reward the complete fight
outcome. Start with the native CPU engine as the fidelity reference and GPU
learning; consider a GPU simulation port only against a matched synchronized
CPU/GPU workload. Game Boy inference must be checked numerically and measured
with the complete extractor/layers/action adapter before deployment.

The `--features` demo exposes exact rational expected raw/useful damage, KO
probability and min/max damage for the supported profile, without changing
state or consuming RNG. Per slot it evaluates normal/critical base damage once
each, then aggregates 32 spread outcomes with denominator 384. This is a
provisional condensed-observation experiment; GBC extraction cost and the
final observation schema are unmeasured and are not part of neural timing.

## Source-driven native arithmetic lifting

`lifting.py` generates native C from the actual DamagePass1..4 and Multiply/Divide
assembly instructions, using assembled UNION symbols for shared math memory.
`lifting_check.py` compares native execution with separately assembled byte
execution and independent arithmetic formulas, tests a copied-source update, and
rejects unsupported instruction/operand mutations. The fixture abstracts farcall
bank effects and native return addresses; it is not full-ROM parity.

```sh
python3 -B tools/trainer_ai/lifting_check.py --build-dir /tmp/polished-ai-lifting
```

See `lifting.md` for exact scope, benchmark commands, remaining architecture
risks and why a source-derived arithmetic slice is not yet a full battle engine.
These are host tools and add no bytes to the game ROM.

## Stateful real-ROM translation

`stateful_lifting.py` extends the experiment to actual assembled game ROM bytes,
real RAM return stacks, MBC3 bank wrappers, actor stats and recorded DIV events.
`stateful_fixture_check.py` compares bounded routines and a chained confusion
transition with independent mGBA execution. `stateful_fixture_updates.py` rebuilds
copied game sources and checks supported updates and explicit rejection paths.
These host tools do not alter the game ROM or provide a whole-fight simulator.

See [stateful_lifting.md](stateful_lifting.md) for the supported memory profile,
source verification gate, reproducible commands and limitations. The old
arithmetic fixture remains a separate, narrower experiment.

## Evaluation ratings

`ratings.py` fits scenario-balanced batch Bradley-Terry ratings on an Elo scale,
with a frozen reference at 1000, fixed policy hashes, rules/suite/protocol
provenance and pairwise coverage. Disconnected pools are labeled incomparable;
reported score uncertainty is not an Elo posterior. `ratings_check.py` uses only
synthetic fixtures. A faithful evaluator and trained policies still need to
produce real whole-fight outcomes. See `ratings.md` for JSON schema and protocol.

```sh
python3 -B tools/trainer_ai/ratings_check.py
python3 -B tools/trainer_ai/ratings.py /path/to/evaluation.json --output /tmp/ratings.json
```

## Bounded storage and CPU scheduling

`training_store.py` writes bounded framed shards under a cooperating-writer
application quota. The selected data root is
`/Volumes/Shared/polished_ai/training_data`; the default ceiling is
100000000000 bytes including conservatively charged metadata and crash residue.
No full-size allocation or replay corpus is created by initialization. Filesystem
allocation assumptions, interrupted writes and recovery limits are in `training.md`.

`training_runtime_native.c` exposes explicit seed ranges for the restricted
synthetic simulator. `training_runtime.py` launches bounded single-thread native
children with two workers/two in-flight batches by default. CPU library thread
limits are set before imports and inherited. Future backend metadata includes
CPU/CUDA/MPS; no learner or GPU kernel is implemented or started. Current tests
must stay within the user's coordinated two-to-three CPU-task limit.

```sh
cc -std=c11 -O2 -Wall -Wextra -Werror tools/trainer_ai/training_runtime_native.c -o /tmp/polished-ai-runtime
python3 -B tools/trainer_ai/training_runtime.py --simulator /tmp/polished-ai-runtime --workers 2 --max-inflight 2 --batches 4 --fights-per-batch 250
python3 -B tools/trainer_ai/training_check.py --artifact-dir /tmp/polished-ai-storage-check
```

Rating/storage/resource tooling does not remove the simulator's current fidelity
boundary. Do not train or announce game strength using unsupported mechanics.
