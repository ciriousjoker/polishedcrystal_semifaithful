# Merge retained conditional battle coverage

`battle_coverage_merge.py` joins explicit, independently valid retained collections
without rerunning fights or rewriting original evidence. Each input supplies
`ledger.json`. The existing strict gate must accept its evidence, configuration
and source inventory. Only failures for still-missing required whole-fight rules
are permitted. All inputs must agree on current inventory, configuration and
`conditional-native-gameplay` scope. Other gate failures reject the merge.

The output directory must be empty and outside every input. The tool copies only
referenced retained artifacts and original ledgers/adapters under separate
`collections/<index>/` prefixes. Original reports, raw cases, manifests, binaries,
WRAM snapshots, runner sources, case reports and adapters retain their exact bytes
and hashes. It does not copy build caches or scan entire directories.

New `derived/` case reports rebase retained WRAM and raw-case references. New
adapters rebase artifact, case-report, execution-artifact, oracle-artifact and
excluded-case paths. They keep hash-bound `origin` and `origin_ledger` references
to the unmodified originals. Only these derived JSON documents receive new
hashes. All referenced paths must be canonical, relative and contained in their
evidence root; conflicting output paths and changed source bytes reject.

Evidence names receive unique collection prefixes. A duplicate rule retains its
strongest already validated phase and only the references that establish that
phase. No phase is inferred from adapter totals and no unclaimed rule is promoted.
Excluded and unknown corpus cases remain in the retained original reports;
explicit `excluded_corpus_cases` entries are also rebased and visible in
`merge.json`. The final ledger is checked again by the unchanged strict gate.
Missing mechanics remain listed in `acceptance.json` and `COVERAGE.md`, and M2
remains incomplete until its required rules pass.

The actual prepared inputs contain 19 accepted first-family cases with 20 keys
and 35,339 comparisons, plus 24 accepted stat-stage cases with 24 keys. The
expected join has 44 distinct claimed keys. Counts describe retained cases,
not newly executed fights or independent frontend equivalence.

Exact next command, from the worktree after the coordinator allocates the CPU:

```sh
python3 -B tools/trainer_ai/battle_coverage_merge.py \
  --collection /Volumes/Storage/tmp/polished-ai/build/battle-corpus/oracle-provenance-retained-o3 \
  --collection /Volumes/Storage/tmp/polished-ai/build/battle-corpus/stat-stage-retained-o3 \
  --output /Volumes/Storage/tmp/polished-ai/build/battle-corpus/combined-retained-o3
```

Then check the combined ledger independently:

```sh
python3 -B tools/trainer_ai/battle_coverage.py \
  --output /Volumes/Storage/tmp/polished-ai/build/battle-coverage/combined-retained-o3 \
  --ledger /Volumes/Storage/tmp/polished-ai/build/battle-corpus/combined-retained-o3/ledger.json \
  --evidence-root /Volumes/Storage/tmp/polished-ai/build/battle-corpus/combined-retained-o3
```

Collection success exits 0; the strict whole-M2 gate is still expected to exit 1
because required coverage is incomplete. Invalid evidence exits 2 without a final
ledger. For a bounded negative check, copy one retained input to a disposable
external directory, remove a referenced raw case or change one byte in it, and
repeat the merge using that copy plus the other original collection and a fresh
empty output. Each mutation must reject as invalid retained evidence. Never
modify the original retained input for this check. No battle, build or native
library execution is needed for merging or these integrity checks.

## Executed checkpoint

The coordinator merged the current first-family and stat-stage collections.
The result retains 43 positively observed complete fights, 44 distinct rule
keys and the one excluded Thunder Wave case without changing original bytes.
The independent unchanged gate reports 809 remaining missing rules, with no
invalid-evidence failures. M2 is still incomplete.

Both actual negative checks reject: a removed original case artifact and an
original case whose bytes changed. Neither rejection writes a ledger. Evidence
is retained under `build/battle-corpus/combined-retained-o3` and
`build/battle-corpus/merge-negative/report.json` in the external project.
No source fight was rerun to perform this reference-only merge.
