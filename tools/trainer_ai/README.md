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
