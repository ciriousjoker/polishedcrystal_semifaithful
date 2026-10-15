# Bounded storage and rollout runtime

These standard-library prototypes schedule the existing synthetic 17-move
slice. They do not train or evaluate a policy, certify ROM parity, import actual
fights, or establish trained strength. The objective metadata names whole-fight
outcomes. Unsupported and truncated outcomes remain distinct from wins.

## Storage

`training_store.py` initializes `/Volumes/Shared/polished_ai/training_data` with
an empty manifest and configuration. The conservative ceiling is
100,000,000,000 bytes, including directory, metadata, sealed, temporary and
unknown file occupancy. No bulk fixture data is put into this selected path.
Each file is charged the larger of reported allocated bytes and logical size
rounded up to 128 KiB, matching the observed Shared volume allocation scale.
This floor is explicit in the persisted configuration. Select a larger unit if
the target filesystem needs it. Physical allocation and per-directory expansion
can differ across filesystems; this is a cooperating-writer application budget,
not a hard operating-system or cross-host quota. Initialization itself creates
the root and lock before checking occupancy, so choose a ceiling that can hold
the filesystem's minimum directory footprint.

One exclusive `flock` covers each entire shard write and manifest update. This
serializes writers, so no independent outstanding reservations can overbook the
budget. All writers must use this API and this same local lock. Occupancy is
rescanned once when the lock is acquired, including any crash residue. A
conservative in-memory ledger then charges allocation growth and metadata
replacement during that transaction, avoiding filesystem scans per record.
Directory-entry slack stays charged until the next locked rescan. File writes
use bounded standard-library buffering, with flush/fsync at seal time. Checks reserve
allocated growth and a conservative directory allocation unit before creating
files. Shards default to at most 8 MiB and stream length-prefixed byte records;
callers choose compact encoding/compression and provide source/rules/model/
scenario/seed/objective provenance. Oversize writes fail and retain bounded
partial files. A caller should bound each input record in its own streaming
pipeline rather than construct large record objects in memory.

Files are flushed and fsynced before atomic rename. Only sealed files enter the
atomically replaced manifest. A crash can leave a temp file, metadata temp file,
or an unlisted sealed shard; all are charged on subsequent writes. There is no
automatic reclamation or deletion. Reconciliation must inspect checksums and
provenance before explicitly deciding what to retain. Directory fsync and
power-loss durability are not promised on the exFAT volume. A system crash may
lose a recent manifest entry, but orphan file occupancy is still charged after
restart. Unsupported locks/renames fail with filesystem errors; symlinks and
special files fail closed. Concurrent unrelated writers and remote hosts are
outside the guarantee. Existing source files outside this directory are never
modified.

For online/on-policy learning, consume current rollouts promptly and retain only
bounded reproducibility/evaluation shards or a bounded replay window. The
writer intentionally stops when full; retention policy needs an explicit later
design, rather than silently discarding unrelated data.

## Runtime

```sh
cc -std=c11 -O2 -Wall -Wextra -Werror tools/trainer_ai/training_runtime_native.c -o /tmp/training-runtime
python3 -B tools/trainer_ai/training_runtime.py --simulator /tmp/training-runtime --workers 1 --max-inflight 1 --batches 4 --fights-per-batch 250
python3 -B tools/trainer_ai/training_check.py --artifact-dir /tmp/training-storage-check
```

The native adapter includes `simulator.c` without altering it, using its existing
fixture and rollout functions. The adapter accepts an explicit seed range per
batch. Adjacent nonoverlapping ranges avoid duplicating seeds across workers;
this is native RNG provenance, not ROM RNG parity or a proof of statistical
independence. The executable SHA-256 and seed/count accompany streamed outputs.
Thread supervisors launch at most the explicit worker count of single-threaded
native processes. Pending and completed-but-unconsumed batches together stay
within `--max-inflight`; output is streamed immediately. To fill N workers, set
`--workers N --max-inflight N` or a larger inflight limit. Default workers and
inflight batches are both 1. Increasing workers is an explicit future choice;
current experiments must use only one CPU-heavy process/thread total, including
any learner, evaluation and oracle jobs.

OMP, OpenBLAS, MKL, Accelerate and NumExpr thread limits are set to 1 before
worker infrastructure or optional learning libraries could load, and children
inherit them. Native code itself is single-threaded. `--learner-backend` records
`none`, `cpu`, `cuda` or `mps` (Apple Silicon, including the current M2)
as a future configuration choice, always reporting
`learner_enabled=false`. No learner or GPU kernel is loaded; GPU execution remains disabled in tests. Future learning
must load libraries only after resource settings are applied, isolate the
learner budget from rollout workers, and bound device queues and memory.
GPU core count does not map to a user-configurable worker count. Tiny actors
can favor batched CPU inference because process/device transfers and launch
latency dominate small networks. GPU learning/inference needs measured batches
against the same validated workload before choosing the backend. Real learning
remains blocked on adequate simulator fidelity and approved observations.
