# Delayed and volatile evidence collection

Status: metadata collection, merge, strict gate and eight rejection checks passed
on 2026-10-15 after the coordinator granted the CPU slot. No battle was rerun and
no build was performed. Future validation requires the coordinator's CPU grant.
The original fight driver and all shared helpers, gate and merger remain frozen.

`battle_delayed_collect.py` retains the exact completed 16-case family from three
successful reports under
`/Volumes/Storage/tmp/polished-ai/subagents/m2-delayed-residual-01/artifacts`:
`first-corrected`, `remaining-side0` and `remaining-side1`. Failed first attempts
using an incorrect Leech Seed healing expectation are excluded. The executed
23-file project host map is pinned in the collector, including driver SHA
`ea5db14843349a4cb339099512b8f371a8bc3648fc593391fe95abd5c69e4d1d`.
The pinned manifest is
`dec3cb17a34aeeb9e0f36e44590d0890b7d0816ddff98fc7bb0346b9b32737b0`.

This is pure typed evidence verification. It imports coverage and the accepted
small `battle_family_evidence` helper, never the battle driver or native engine.
It uses `artifact_sink`, `bind_execution` and `finish`, without changing the
shared acceptance schema or introducing rollout mechanics.

Every raw case and witness, report, successful run log, standalone initial
8192-byte WRAM and full all-bank/HRAM/SVBK memory snapshot is retained. The
collector pins original executed host bytes, checks every retained input hash,
source hashes and compiled move/effect records, and binds actual ROM, symbols,
manifest, assembler receipt/log and Oracle C/library/mGBA/State/Hook identities.
Identical generated C, native libraries and other input snapshots are retained
once across the three batches. Each copied report still references its original
snapshot locations, while the adapter artifact list hashes the deduplicated
retained bytes. The helper binds real initial runtime options from WRAM.

The witness checks reconstruct only the bounded evidence conditions: source
fractions and operation ordering, both views of every probe, active party
indices, actual cast/Splash/switch actions, initial and subsequent legal masks,
PP expenditure, permanent identity/stat stability, absence of items/statuses,
explicit replacement choices and real source terminals. Exact event/state replay
is bound to the pinned executed driver and complete independent comparison tape.
Full before/after observations are not separately recreated by another engine.

Source Leech Seed drains one eighth maximum HP and heals half that drain through
`GetHPAbsorption` and `HalveBC`. Ghost Curse pays half maximum HP and causes
quarter-HP residual; non-Ghost Curse raises Attack/Defense and lowers Speed.
Perish Song starts at four, shows three/two/one at successive decision points,
then both active leads faint and both explicit reserves replace them. Future
Sight's packed caster/tick count reaches its third endturn tick after the target
switches and the reserve takes positive delayed damage. Wrap samples four/five,
decrements before residual damage, expires without another tick and restores
its target's source legal switch mask. These source clauses are pinned and
checked again; changing a source input fails closed.

Fourteen positive cases can support seventeen existing required inventory keys:
five move keys, five corresponding effect keys, five corresponding endturn
handler keys, `volatile:SUBSTATUS_LEECH_SEED` and `volatile:SUBSTATUS_CURSE`.
The two Grass nonactivation cases remain retained supporting controls without
positive rule claims. Positive cases contain 6,896 comparisons; all sixteen
executed cases contain 7,628 comparisons over 89 rounds. No blanket controller,
neutral ability, immunity pseudo-key or full M2 claim is made.

After the CPU grant, the exact metadata-only validation command is:

```sh
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 MKL_NUM_THREADS=1 \
python3 -B /Volumes/Storage/tmp/polished-ai/subagents/m2-delayed-collection-01/artifacts/validate_packaging.py \
  --canonical /Volumes/Storage/tmp/polished-ai/build/battle-corpus/combined-through-abilities-hazards-o3 \
  --output /Volumes/Storage/tmp/polished-ai/subagents/m2-delayed-collection-01/artifacts/validation-01
```

The coordinator may replace `--canonical` with its latest merge base at grant.
The validator collects the exact sixteen cases, checks fourteen positive records
and seventeen keys, merges with the supplied canonical corpus and invokes the
unchanged strict gate. Only missing-rule failures are acceptable, with zero
invalid evidence. No battle rerun or build is required.

Eight verified negative checks use wrong Leech healing, Curse cost, Perish
counter, Future Sight delay, Wrap switch mask, missing initial WRAM, missing
executed host bytes and missing Oracle bytes. Semantic negatives update both
observation views, the raw/report witness pair and evidence hash consistently.
They must reach typed semantic rejection instead of a stale wrapper digest.
Staged files use hardlinks with atomic replacements; originals are never
mutated. Each rejection must exit two without a ledger. The validator verifies
original report/case hashes remain unchanged.

Accepted evidence is under
`/Volumes/Storage/tmp/polished-ai/subagents/m2-delayed-collection-01/artifacts/validation-01`.
`retained/` contains the standalone family. `merged/` combines it with
`combined-through-drain-recoil-o3`; `strict-gate/acceptance.json` reports 227
tested keys, 626 missing required rules and zero invalid evidence. All seventeen
family keys are net new against that base. The two Grass controls remain raw
supporting evidence without positive claims. `result.json` records all eleven
commands and confirms original bytes unchanged. All five semantic negatives
failed at their intended fraction, counter or mask checks; missing WRAM, host and
Oracle bytes also rejected with exit two and no ledger. The validation parent
exited zero. No additional fight or routine broad test was needed.
