# Retaining witnessed corpus coverage

`battle_corpus_collect.py` converts the actual `battle_corpus_run.py` report into
an evidence directory accepted by the existing strict `battle_coverage.py` gate.
It does not execute, build or import the native battle controller. It registers
only accepted cases with positive semantic activation, an actual witness frame,
a complete terminal source fight, nonzero ROM differential comparisons and exact
native replay. Candidate rules alone never establish coverage.

The collector requires explicit inventory, report, every report-listed case,
manifest, raw ROM, SYM, generator, original build receipt and runner source paths.
If the receipt records a build-log digest, the original log is required too.
The collector copies those bytes and initial WRAM snapshots into a new empty
evidence directory. It verifies source inventories, runner module hashes,
manifest identity, execution bytes and runtime configuration before writing the
ledger. It never reconstructs an assembler invocation or changes a receipt.

Original reports and cases remain byte-identical under `raw/`. `cases.json` is
explicitly a derived case report, referencing each original case and semantic
frame index. `adapter.json` retains all required original artifacts and execution
provenance. All output artifact references are relative to the evidence root.
Absolute paths inside unchanged original reports describe original locations;
the gate uses retained relative paths, never those old locations.

The runner does not yet emit comparison-region and choice-contract metadata.
The collector therefore pins six reviewed runner source identities in
`REVIEWED`, retains them, and verifies every other source digest actually recorded
by each case. The reviewed Oracle compares CPU registers, C000-DFFF, HRAM and,
when banked, all additional physical WRAM plus raw SVBK. Its comparison rows are
appended only after equality assertions. The reviewed runner submits scripted
choices for both actors. The controller offers `BagAction` through its action ABI;
`player_bag: true` records this capability, without claiming a bag mechanic was
activated. Options come from actual initial WRAM through the existing gate and
the manifest's explicit capabilities. Any change to a reviewed boundary rejects
until the collector mapping is reviewed. Supply the original matching runner
source directory for old evidence; do not alter hashes to make current sources
appear to have produced an old case.

Every blocked, unsupported, unsatisfied or truncated case is retained and listed
in `collection.json` and the adapter's `excluded_corpus_cases`. It contributes no
claims. A failed or unsupported case can never enter the adapter's passed case
list. A report with no eligible case fails collection. Invalid or mismatched
accepted evidence also fails, leaving any already copied raw bytes for diagnosis
and no ledger. Use another empty output directory after repairing inputs.

A successful collection returns zero even while the whole M2 gate remains false.
`acceptance.json` keeps every still-missing required rule visible. It is expected
that a single Tackle fight registers only `move:TACKLE` and
`effect:EFFECT_NORMAL_HIT`; it cannot establish the remaining ordinary trainer
mechanics. The only claim scope is `conditional-native-gameplay`. Ordinary
frontend projection is still unproven.

Example for the retained first case, run only when the coordinator allocates the
single CPU slot:

```sh
python3 -B tools/trainer_ai/battle_corpus_collect.py \
  --inventory /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-coverage/inventory.json \
  --report /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/first-native-o3/report.json \
  --case /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/first-native-o3/move-tackle-side0-seed193.json \
  --manifest /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/native-o3/manifest.json \
  --rom /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game/polishedcrystal-3.2.3.gbc \
  --symbols /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game/polishedcrystal-3.2.3.sym \
  --generator tools/trainer_ai/stateful_lifting.py \
  --build-receipt /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-coverage/original-build-receipt.json \
  --build-log /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game-build.log \
  --runner-sources tools/trainer_ai \
  --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/first-retained-o3
```

Then independently invoke the existing gate with the retained ledger and evidence
root. Its nonzero exit remains expected while required rules lack coverage:

```sh
python3 -B tools/trainer_ai/battle_coverage.py \
  --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-coverage/first-retained-o3 \
  --ledger /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/first-retained-o3/ledger.json \
  --evidence-root /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/first-retained-o3
```

The collector also resolves the case's unique move row in the current inventory,
requires the source move ID and actor side in the witness, and requires exactly
that move key and its current effect key. It reloads the retained, hash-pinned
pure semantic observer, rebuilds frames and source-derived bindings, and requires
that recomputed activation, witness frame index and rule keys match the original
report. This does not execute a battle or import the native controller.

## Manual negative check: declared rule without observation

After the positive collection, prepare a disposable report that consistently
substitutes Growl's rule declarations into the Tackle report. The case and its
actual execution frames remain Tackle. Run this only in the allocated CPU slot:

```sh
python3 -B - <<'PY'
import json
from pathlib import Path
source = Path('/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/first-native-o3')
target = Path('/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/collector-negative-substitution')
target.mkdir(exist_ok=False)
case = json.loads((source / 'move-tackle-side0-seed193.json').read_text())
report = json.loads((source / 'report.json').read_text())
rules = ['move:GROWL', 'effect:EFFECT_ATTACK_DOWN']
case['case']['candidate_rules'] = rules
case['semantic']['candidate_rules'] = rules
case['result']['accepted_rules'] = rules
report['cases'][0]['accepted_rules'] = rules
report['cases'][0]['evidence'] = str(target / 'case.json')
(target / 'case.json').write_text(json.dumps(case))
(target / 'report.json').write_text(json.dumps(report))
PY
```

Repeat the positive collector command with `--report` and `--case` pointing at
these two negative files and a fresh empty `--output`. Expected: exit 2 with
`Invalid positively observed rule keys`, and no `ledger.json` written. Consistent
JSON declarations cannot promote a mechanism absent from the actual source move.
To exercise observer recomputation separately, keep all original Tackle rule
keys but replace the witness frame's target HP after the move with its before HP.
Expected: exit 2 with `Retained observations do not reproduce declared semantic
activation`. Preserve the original execution outputs unchanged.

## Executed evidence before the Encore observer refinement

The coordinator executed the first collector against the original frozen runner:
`/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/first-retained-o3` retains
one accepted case and two rules. The ten-family, both-side batch collected
17 accepted cases and 18 rules under
`/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/first-families-retained-o3`.
Three cases remain visibly excluded: Thunder Wave side 1 had no unconfounded
opposing Splash execution, and both Splash cases lacked a positive projection
witness under the original observer.

The independent strict gate output is retained at
`/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-coverage/first-families-retained-o3`.
It reports no invalid evidence, with 835 required rules still missing full-fight
coverage. M2 remains incomplete. The coordinator also executed the unobserved-rule,
removed-HP-witness and stale-manifest negative checks; all three rejected with
exit 2 and no ledger. Results are retained at
`/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/collector-negative/report.json`.

A subsequent source-reviewed observer refinement now compares only the low
nibble of `EncoreCount` in Splash's no-effect projection. The actual source's
`HandleEncore.do_encore` tests `and $f` for remaining duration and extracts the
upper nibble using `swap a` for the last move index. Updating that index from
zero to one after ordinary move use changes raw `EncoreCount` from 0 to 16
without activating Encore. Source bindings require these exact reviewed
instructions, including `EndturnEncoreDisable`'s duration mask. All other field
flags, counters and stages remain unchanged in the comparison. A low-nibble
change from 0 to 1 must reject activation; synthetic negative controls also cover
both sides' stat stages, volatile flags and another counter.

The collector's reviewed observer identity was advanced to this precise new
source version. Existing evidence retains its original observer under
`first-families-retained-o3/runner/battle_corpus_observers.py`; it is not relabeled
or recomputed as new execution. The documented first-case collection commands
above describe the original execution and need the original collector revision
when reproducing that old report. New observer claims require a new actual runner
execution and a fresh retained directory. Thunder Wave's blocked case remains
blocked. The new refinement is code preparation pending coordinator validation.

## Execution-time oracle provenance

A subsequent independent review found that the historical collections above did
not record the separate C ROM oracle, loaded oracle dylib or direct State/Hook
ABI modules. They remain useful historical conditional comparisons with this
provenance limitation. No execution-time identity is reconstructed for them.
The fresh Splash run already succeeded on both sides, retained under
`/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/splash-retained-o3`, with
only `move:SPLASH` and `effect:EFFECT_SPLASH`; this collection has the same
historical oracle-provenance limitation.

The metadata-only update to `Oracle.__init__` now exposes the actual selected
`battle_controller_oracle.c` source and loaded dylib paths and SHA256 identities.
It resolves the actual loaded mGBA dependency using `dladdr` on the oracle's
`GBCoreCreate` symbol, requiring the known configured mGBA prefix. The source
and dylib are recorded separately: this does not invent a fresh compiler receipt
for a reused library. Direct imported `State` and `Hook` module identities are
also recorded. Each new raw corpus case contains this `oracle_provenance`;
runner hashes now include `stateful_fixture_check.py` and `battle_env_check.py`.

The current collector requires additional explicit `--oracle-source`,
`--oracle-library`, and `--mgba-library` inputs. It retains their exact bytes,
checks each new case's execution-time identities, and checks the actual ABI
module identities against retained runner sources. The C oracle's reviewed
source digest is pinned separately. Missing execution-time identity rejects old
cases. A dependency resolution error is visible in raw evidence and prevents
collection. No build or native library launch is performed by the collector.

Next fresh execution command, with the coordinator's sole CPU slot:

```sh
python3 -B tools/trainer_ai/battle_corpus_run.py \
  --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai \
  --inventory /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-coverage/inventory.json \
  --native-build /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/native-o3 \
  --optimization O3 --moves TACKLE SPLASH --sides 0 1 \
  --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/oracle-provenance-native-o3
```

Collect that fresh result:

```sh
python3 -B tools/trainer_ai/battle_corpus_collect.py \
  --inventory /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-coverage/inventory.json \
  --report /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/oracle-provenance-native-o3/report.json \
  --case /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/oracle-provenance-native-o3/move-tackle-side0-seed193.json \
  --case /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/oracle-provenance-native-o3/move-tackle-side1-seed193.json \
  --case /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/oracle-provenance-native-o3/move-splash-side0-seed193.json \
  --case /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/oracle-provenance-native-o3/move-splash-side1-seed193.json \
  --manifest /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/native-o3/manifest.json \
  --rom /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game/polishedcrystal-3.2.3.gbc \
  --symbols /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game/polishedcrystal-3.2.3.sym \
  --generator tools/trainer_ai/stateful_lifting.py \
  --build-receipt /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-coverage/original-build-receipt.json \
  --build-log /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game-build.log \
  --runner-sources tools/trainer_ai \
  --oracle-source tools/trainer_ai/battle_controller_oracle.c \
  --oracle-library /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/controller-oracle-7840c8b7a31bc643.dylib \
  --mgba-library /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/mgba/lib/libmgba.0.10.5.dylib \
  --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-corpus/oracle-provenance-retained-o3
```

These exact binary inputs are the existing configured files. The new raw cases
must confirm their actual loaded identities. If they differ, use the actual
recorded files and diagnose the discrepancy rather than editing case identities.
The earlier negative-check procedure requires fresh cases under the current
collector so that rejection tests reach their intended assertion boundary.

## Fresh execution acceptance

The coordinator executed all ten initial move families on both sides again
with the current Oracle metadata. All twenty fights reached source terminal
and exact native replay; nineteen provided unconfounded positive activation
for twenty distinct move/effect keys. The remaining Thunder Wave case is
retained without coverage because paralysis prevented the opposing Splash
required by the controlled-round observer. No case was relabeled.

The new collector passed with retained C oracle, loaded oracle dylib, actual
libmgba and State/Hook identities. Actual six negative checks reject an
unobserved rule, removed damage witness, stale native manifest, absent
execution-time oracle identity, changed oracle-library identity and changed
libmgba identity, writing no ledger in each rejection. Evidence lives under
`build/battle-corpus/oracle-provenance-families-o3`,
`oracle-provenance-retained-o3`, and `collector-provenance-negative/report.json`
in the external project. The independent strict gate retains all other missing
mechanics and the frontend limitation; M2 remains incomplete. The independent
review found the provenance issue resolved in source before this fresh run.

## Prepared local secondary-effect witness retention

The new `source-action-span` phase has a separate witness gate. It does not gain
coverage from its phase name or source hashes alone. The collector reconstructs
both existing source callsites using the retained, manifest-checked ROM and SYM
bytes and current inventory-bound source scaffolds. The runner and collector
share the pure `execution_boundaries` binder in the reviewed observer module;
no controller or native library is imported for collection.

A local frame must identify a turn inside the explicit prelude/probe window,
have empty individual-command diagnostics and verified read-only observations,
and match exactly one actual execution-trace row for that turn and side. Actual
move, miss result, both complete callsite contracts, and both complete before/
after observations must agree with the trace. The callsite contracts must also
equal the independently reconstructed source/ROM/SYM contracts. The existing
semantic observer then independently recomputes the status/confusion transition,
including secondary damage and neutral-fixture restrictions. Complete source
terminal, independent Oracle comparisons, execution-time provenance and exact
RecordedRun replay remain mandatory. Existing controlled-round witnesses keep
their previous semantic checks and opposing-Splash restriction.

This extension is code preparation only. The coordinator passed the original
synthetic observer checks before the pure binder was factored; the current
factored version and collector changes have not been imported or executed.
Reviewed source hashes must be refreshed after the remaining controller writer
freezes its final source. Old retained evidence is not rewritten or relabeled.

After the bounded actual secondary run and positive retention, the next negative
controls should each use disposable reports/cases and a fresh empty output:

- Remove or duplicate the matching execution-trace row. Expected rejection:
  `Local witness has missing or ambiguous actual execution`.
- Change only the frame's target HP/status observation while leaving its trace
  untouched. Expected rejection: `Local witness observation differs from actual
  execution trace`.
- Change both frame and trace callsite return PCs together. Expected rejection:
  `Local callsites disagree with actual source/ROM/SYM boundaries`.
- Remove secondary damage or the matching new status/confusion from both frame
  and trace, preserving their equality. Expected rejection by independently
  recomputed semantic activation.

Each negative command must exit 2 and leave no ledger. These checks are planned,
not executed evidence; absent actual secondary activation still contributes no
claim and is retained as unsatisfied or blocked.

## Executed local retention and rejection checks

The reviewed local extension has now retained the corrected sixteen-case batch
under `build/battle-corpus/secondary-local-retained-o3-01` in the external project.
Two DynamicPunch fights contribute `move:DYNAMICPUNCH` and
`effect:EFFECT_CONFUSE_HIT`; fourteen unsatisfied cases remain raw exclusions.
The accepted cases contain 3,426 full source/Oracle comparisons and exact native
replay. Collection exited zero. Its standalone strict gate reports 851 remaining
missing-rule failures and no invalid evidence; this collection does not replace
or merge the earlier retained collections, and M2 remains incomplete.

All four planned local negative checks were executed against a positive case.
Removing its trace row, changing only the frame observation, changing the same
callsite in frame and trace, and consistently removing secondary confusion each
rejected at the intended assertion with exit 2 and no ledger. Exact logs and
return codes are retained in
`subagents/m2-move-spans-01/artifacts/negative-results.json`. The pure observer
checks passed after the shared binder refactor. The reviewed runner pin includes
the narrowly corrected handling of the aliased end-of-move grammar scratch byte;
old failed and retained artifacts were preserved unchanged.

## Prepared secondary stage witness extension

The observer now has a prepared extension for recognized secondary stat-stage
hits and the source's five-stat `EFFECT_ALL_UP_HIT` recipe. The existing local
collector gate applies unchanged: a complete source-bound interval and exact
frame/trace observation equality are required before semantic recomputation.
The observer additionally requires actual target damage and the correctly named
stage changes on the intended side. No phase-only acceptance is introduced.

This code preparation is unexecuted. The observer's REVIEWED digest intentionally
still pins the preceding accepted version until coordinator review and the
currently active controller/profile work are frozen. New native/controller
identities also need independent review before refresh; no old collection is
recomputed as new evidence. Planned semantic negative controls consistently
remove stage change or damage from both frame and trace, keeping them equal,
so rejection must reach semantic recomputation. Frame-only changes continue to
reject at trace equality. None of these controls have run for the stage change.

## Executed secondary stage retention and combined ledger

The secondary stage collector retained twelve accepted fights and all twelve
excluded cases from the 24-case batch. Accepted Flame Charge, Mud-Slap and Icy
Wind cases add six move/effect keys with 21,907 independent Oracle comparisons
and exact terminal replay. Eight Metal Claw/AncientPower nonactivations remain
unsatisfied; four Rock Smash cases remain unsupported because the current source
inventory maps that move to Brick Break rather than the inactive faithful
branch's Defense-down-hit effect.

Both semantic negatives changed the frame and matching trace consistently:
removing stage changes while keeping damage, and removing damage while keeping
stage changes. Both rejected at semantic recomputation with exit 2 and no
ledger. Details are in
`subagents/m2-secondary-stages-01/artifacts/negative-results.json`.

The existing merger then combined the previous 68-key collection, the two-key
confusion collection, and this six-key stage collection without rewriting their
original artifacts. New `build/battle-corpus/combined-through-secondary-stages-o3`
contains 76 distinct tested-full-fight keys and 28 retained exclusions. The
unchanged strict gate reports 777 missing required-rule failures and no invalid
evidence. Native manifests remain attached to their original adapters; no
manifest identity was forced across collections. M2 remains incomplete.

## Executed active-inventory breadth retention

The 113-case ready-supported breadth batch retained 69 positive fights and 44
excluded cases in `build/battle-corpus/ready-moves-retained-o3-01`. Accepted
cases contain 123,776 independent Oracle comparisons and claim 92 distinct keys,
of which 86 are new relative to the preceding 76-key combined ledger. The
collector used the exact fifteen frozen host-source snapshots and actual
execution-time Oracle/library/ABI provenance. Collection exited zero in 1.20
seconds. No original collection was rewritten.

The existing merger combined the preceding ledger with the new collection into
`build/battle-corpus/combined-through-ready-moves-o3`. It has 162 distinct
whole-fight-tested keys, 72 retained exclusions, 691 missing required-rule
failures and no invalid evidence. The 86 added keys comprise 69 move identities
and seventeen previously unproved effect categories. This pass adds ordinary
hit variants, fixed/level damage, primary status/confusion, selected primary
stat changes, weather, safeguard and healing effects. Probabilistic secondary
activations still require their own positive observations; M2 remains incomplete.

## Prepared local first-action-faint damage admission

The observer is prepared to admit its existing damage-only families in the
same strict local phase used for status and secondary-stage effects. The
collector's source/ROM/SYM boundary reconstruction and exact complete frame/
trace matching remain unchanged. The existing miss-zero plus target-HP-loss
semantic predicate permits the still-active target to reach zero HP before
`ResolveFaints`. The historical Hyper Voice/Aeroblast blocked cases stay
unchanged; four fresh two-sided fights and a consistently removed-damage
semantic negative are still required. Reviewed observer hash refresh awaits
coordinator source review. This section records unexecuted preparation.

## Executed local damage retention

The reviewed observer identity was advanced only after source freeze. Four
fresh Hyper Voice/Aeroblast fights, both sides with seed 271, were retained under
`build/battle-corpus/local-damage-retained-o3-01`. They contain 4,385 independent
Oracle comparisons, source terminal and exact replay, with both move identities
and the already covered normal-hit effect. A disposable case consistently
restored target HP in frame and matching trace; semantic recomputation rejected
it with exit 2 and no ledger.

The existing merger combined this collection with the 162-key ledger into
`build/battle-corpus/combined-through-local-damage-o3`: 164 distinct tested keys,
689 missing required-rule failures, no invalid evidence and 72 preserved earlier
exclusions. Original blocked Hyper Voice/Aeroblast reports remain unchanged;
only fresh positive execution supplies their new claims. M2 remains incomplete.
