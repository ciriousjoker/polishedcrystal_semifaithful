# Ability display projection

The controller projects exactly `PerformAbilityGFX` and
`PerformAbilityReplacementGFX`. Actual Show/Begin/End callers keep ability mode,
visibility/pending flags, temporary-map save/restore, Trace's saved AF, cache
replacement, and replayed entry handlers. No ability is substituted and no
FF4F/VRAM/font hardware support is claimed.

The normal graphics wrapper finishes with `XOR A`, giving A=0, F=$80. Replacement
finishes at the VWF string terminator with `CP '@'`, giving A=$53, F=$c0 in the
current source charmap. The WRAM-bank return helper preserves AF. Exact source
hash guards require review if the bodies or callers supporting this boundary
change. Other caller registers are restored by their source wrappers.

The bounded runner requires two complete ordinary trainer fights:

- Porygon uses its real Trace against Gyarados's Intimidate. Its live ability
  cache must change to Intimidate and both sides' Attack stages must drop.
  Porygon then uses actual nested Conversion to select zero-PP Water Gun before
  completing the fight with Tackle.
- Gyarados's normal Intimidate lowers Snorlax's Attack, then Gyarados switches
  out and returns. The second activation must lower Attack again, followed by
  a physical Tackle that damages the opponent.

Both require actual source terminal continuation, restored ability mode,
fixed levels/XP, complete same-hook mGBA segment comparisons, and exact native
replay. Initial first-wrapper state and its real outer source caller stack are
retained for a separate unhooked graphics continuation through later Trace. Host, runner, ROM,
symbols, manifest, and actually loaded oracle identities are retained.

Run only after the coordinator grants the sole CPU slot and regenerates the
existing O3 native profile:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 python3 tools/trainer_ai/battle_controller_ability_check.py --artifact-root /Volumes/Storage/tmp/polished-ai --output /Volumes/Storage/tmp/polished-ai/build/battle-controller/ability-projection
```

Same-hook comparisons alone do not validate the new presentation boundary.
Ordinary mGBA must execute the wrappers without internal hooks, check their
actual return AF and restored bank, then continue the surrounding entry-ability and Trace code, followed by
entry effects. All physical WRAM/HRAM differences must be retained. Ability
scratch overlaps decompression, weather, and scratch-map storage, so the union
must not be globally masked. Pixel/frame equality is outside this proof.

Implementation and runner are pending their first validation. No M2 completion
or unhooked-wrapper equivalence is claimed by this document.

The bounded proof passed: Trace/Conversion took 16 turns with 1,258 actual-ROM
comparisons, and normal Intimidate with switch/reentry took 9 turns with 798.
Both exact native replays passed. A private VWF-source modification was rejected
by the exact source guard.

The separate mGBA continuation executed five ability wrappers without internal
hooks and 173 real VBlanks. Actual normal returns were A=0/F=$80; replacement
returned A=$53/F=$c0. Restored SVBK, final CPU, both Intimidate Attack effects,
ability cache replacement, `wInAbility=0`, and all compared gameplay regions
matched at the first source policy decision. Both trajectories had zero logical
Random calls in this bounded entry-ability continuation.

All 432 differing physical bytes are retained individually in the external
report. They include ability graphics/name scratch with explicit union aliases,
inactive stack contents, sound state, frontend transfer/text/timer scratch, and
the seven bytes of hardware/VBlank RNG state. No region was erased or repaired.
The hardware RNG differences remain an explicit limit: this comparison uses
matched logical Random outputs and does not establish hardware-state or future
randomness-distribution equivalence. The continuation stops at the first policy
decision; later full fights remain conditional on the declared presentation.

Reports: `/Volumes/Storage/tmp/polished-ai/build/battle-controller/ability-projection/report.json`
and `/Volumes/Storage/tmp/polished-ai/build/battle-controller/ability-wrapper-continuation/report.json`.
The latter directory also holds complete physical states and
`raw-difference-review.json`, with every difference's nearest source aliases.

New runs also retain each case's mapped initial WRAM after actual reset and
before either actor commits, with its byte hash and replay equality check. This
recording passed a fresh two-fight run: 25 rounds, 2,056 independent comparisons
and exact replay, including equal post-reset WRAM. The new records are under
`build/battle-controller/ability-projection-initial-wram` in the external run;
all 16 executed host sources are hash-verified and preserved beside them.
Earlier accepted fights and their separate unhooked display continuation remain
unchanged; missing initial bytes are never assigned to those old records.
