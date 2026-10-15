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
  --inventory /Volumes/Storage/tmp/polished-ai/build/battle-coverage/inventory.json \
  --report /Volumes/Storage/tmp/polished-ai/build/battle-corpus/first-native-o3/report.json \
  --case /Volumes/Storage/tmp/polished-ai/build/battle-corpus/first-native-o3/move-tackle-side0-seed193.json \
  --manifest /Volumes/Storage/tmp/polished-ai/build/battle-controller/native-o3/manifest.json \
  --rom /Volumes/Storage/tmp/polished-ai/build/stateful/game/polishedcrystal-3.2.3.gbc \
  --symbols /Volumes/Storage/tmp/polished-ai/build/stateful/game/polishedcrystal-3.2.3.sym \
  --generator tools/trainer_ai/stateful_lifting.py \
  --build-receipt /Volumes/Storage/tmp/polished-ai/build/battle-coverage/original-build-receipt.json \
  --build-log /Volumes/Storage/tmp/polished-ai/build/stateful/game-build.log \
  --runner-sources tools/trainer_ai \
  --output /Volumes/Storage/tmp/polished-ai/build/battle-corpus/first-retained-o3
```

Then independently invoke the existing gate with the retained ledger and evidence
root. Its nonzero exit remains expected while required rules lack coverage:

```sh
python3 -B tools/trainer_ai/battle_coverage.py \
  --output /Volumes/Storage/tmp/polished-ai/build/battle-coverage/first-retained-o3 \
  --ledger /Volumes/Storage/tmp/polished-ai/build/battle-corpus/first-retained-o3/ledger.json \
  --evidence-root /Volumes/Storage/tmp/polished-ai/build/battle-corpus/first-retained-o3
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
source = Path('/Volumes/Storage/tmp/polished-ai/build/battle-corpus/first-native-o3')
target = Path('/Volumes/Storage/tmp/polished-ai/build/battle-corpus/collector-negative-substitution')
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
`/Volumes/Storage/tmp/polished-ai/build/battle-corpus/first-retained-o3` retains
one accepted case and two rules. The ten-family, both-side batch collected
17 accepted cases and 18 rules under
`/Volumes/Storage/tmp/polished-ai/build/battle-corpus/first-families-retained-o3`.
Three cases remain visibly excluded: Thunder Wave side 1 had no unconfounded
opposing Splash execution, and both Splash cases lacked a positive projection
witness under the original observer.

The independent strict gate output is retained at
`/Volumes/Storage/tmp/polished-ai/build/battle-coverage/first-families-retained-o3`.
It reports no invalid evidence, with 835 required rules still missing full-fight
coverage. M2 remains incomplete. The coordinator also executed the unobserved-rule,
removed-HP-witness and stale-manifest negative checks; all three rejected with
exit 2 and no ledger. Results are retained at
`/Volumes/Storage/tmp/polished-ai/build/battle-corpus/collector-negative/report.json`.

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
`/Volumes/Storage/tmp/polished-ai/build/battle-corpus/splash-retained-o3`, with
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
  --artifact-root /Volumes/Storage/tmp/polished-ai \
  --inventory /Volumes/Storage/tmp/polished-ai/build/battle-coverage/inventory.json \
  --native-build /Volumes/Storage/tmp/polished-ai/build/battle-controller/native-o3 \
  --optimization O3 --moves TACKLE SPLASH --sides 0 1 \
  --output /Volumes/Storage/tmp/polished-ai/build/battle-corpus/oracle-provenance-native-o3
```

Collect that fresh result:

```sh
python3 -B tools/trainer_ai/battle_corpus_collect.py \
  --inventory /Volumes/Storage/tmp/polished-ai/build/battle-coverage/inventory.json \
  --report /Volumes/Storage/tmp/polished-ai/build/battle-corpus/oracle-provenance-native-o3/report.json \
  --case /Volumes/Storage/tmp/polished-ai/build/battle-corpus/oracle-provenance-native-o3/move-tackle-side0-seed193.json \
  --case /Volumes/Storage/tmp/polished-ai/build/battle-corpus/oracle-provenance-native-o3/move-tackle-side1-seed193.json \
  --case /Volumes/Storage/tmp/polished-ai/build/battle-corpus/oracle-provenance-native-o3/move-splash-side0-seed193.json \
  --case /Volumes/Storage/tmp/polished-ai/build/battle-corpus/oracle-provenance-native-o3/move-splash-side1-seed193.json \
  --manifest /Volumes/Storage/tmp/polished-ai/build/battle-controller/native-o3/manifest.json \
  --rom /Volumes/Storage/tmp/polished-ai/build/stateful/game/polishedcrystal-3.2.3.gbc \
  --symbols /Volumes/Storage/tmp/polished-ai/build/stateful/game/polishedcrystal-3.2.3.sym \
  --generator tools/trainer_ai/stateful_lifting.py \
  --build-receipt /Volumes/Storage/tmp/polished-ai/build/battle-coverage/original-build-receipt.json \
  --build-log /Volumes/Storage/tmp/polished-ai/build/stateful/game-build.log \
  --runner-sources tools/trainer_ai \
  --oracle-source tools/trainer_ai/battle_controller_oracle.c \
  --oracle-library /Volumes/Storage/tmp/polished-ai/build/battle-controller/controller-oracle-7840c8b7a31bc643.dylib \
  --mgba-library /Volumes/Storage/tmp/polished-ai/build/stateful/mgba/lib/libmgba.0.10.5.dylib \
  --output /Volumes/Storage/tmp/polished-ai/build/battle-corpus/oracle-provenance-retained-o3
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
