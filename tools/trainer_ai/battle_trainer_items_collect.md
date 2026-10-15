# Trainer inventory evidence retention

`battle_trainer_items_collect.py` is a metadata-only semantic collector for the
reviewed 28 actual source trainer inventory fights. It imports coverage and the
existing small artifact helper. It never imports battle drivers, loads native
fight libraries, compiles, runs battles or repairs old evidence.

Status: metadata retention verified for all 28 positive cases and 17 source
keys, with all eight negative controls rejected. The merged corpus has zero
invalid evidence. Reruns require the coordinator's exclusive CPU grant. The
accepted input checkpoint contains 14 positive enemy
trainer inventory items in slots 0 and 1, seed 193, with 11,200 independent
CPU/all-bank WRAM/HRAM/SVBK comparisons and exact retained native replay.

## Claim boundary

The collector emits at most seventeen existing required keys:

- Fourteen `trainer-item:*` keys for actual source item effects and selected
  inventory consumption under typed trainer action selection.
- `field:GUARD_MIST` and `endturn:HandleMist` only from the positive Guard Spec
  counter sequence 4, 3, 2, 1, 0, 0 and the bound actual end-turn source path.
- `volatile:SUBSTATUS_FOCUS_ENERGY` only from Dire Hit's observed false-to-true
  flag transition.

`AI_Items.*` inventory rows name the source desirability wrappers. Their table
mapping and calls to the executed effect handlers are audited against source
and ROM, but the desirability policy is intentionally replaced by typed host
selection. This family claims effect execution and inventory consumption. It
does not claim the wrapper policy executed, `controller:CustomTrainerActions`,
positive `status:PAR` behavior, player bag symmetry or stage-cap refusal.
Paralysis cured by Full Heal or Full Restore is supporting status evidence.

## Semantic and identity checks

The reviewed checkpoint pins 23 executed host files and 18 relevant game source
files. Each of the three reports must share those exact identities, the known
O3 manifest, actual Oracle/mGBA/State/Hook provenance and all 52 original retained
input identities. The source/ROM item table and wrapper mappings, literal enemy
Potion/Super/Hyper amounts 20/50/200, item-attribute stat selectors, high-nibble
plus one stage amount, Guard Spec duration 5 and Dire Hit focus bit are
reconstructed independently of driver metadata. Source changes fail closed.
Hyper Potion's actual positive restoration is capped by legitimate Horsea
MaxHP 90; the collector makes no uncapped 200-HP observation claim.

All 28 unique item/slot/side/seed tuples must be present. Successful run logs,
raw complete cases, original `.probe.json` observation files and derived
witnesses must agree. Probes check exact selected-slot consumption, preservation
of the other slot, opposing Splash PP, own item-turn PP preservation, actual
HP/status/stage/guard/focus effects, unchanged opposing stages and correct
source party/active caches. Complete cases must reach source terminal without
failure or truncation and retain the full independent comparison and replay
records. The fixed level/XP/base-stat initial and final contracts must agree.

Initial standalone WRAM is exactly 8192 bytes at the first frozen actor
boundary. Its actual source fields must match initial party/active health,
status, move/PP, stages, flags, inventory, level and XP observations. The full
32896-byte image must contain the same canonical WRAM plus all eight banks,
HRAM and SVBK. The existing runtime-option gate checks NO_EXP and ordinary
trainer options from the retained image.

The explicit `shared_snapshot_root` must be the reviewed sibling
`../trainer-item-inputs` relative to each case-report folder. Every input must
be a content-addressed direct child with its actual expected SHA256 and
basename. This binds the accepted shared location without assuming snapshots
live inside individual report folders. Native generated C/library and other
input copies are deduplicated across all three batches. Reports, logs, cases,
source probes, initial images and input bytes are retained unchanged. The
existing strict gate and merger remain unchanged.

## Metadata validation

The task's frozen `artifacts/validate_packaging.py` passed all eleven metadata
subprocesses after the explicit CPU grant: positive retention, merge with the
latest canonical corpus, unchanged strict gate and eight rejection controls.
The resulting corpus contains 263 tested keys, 590 missing required rules and
zero invalid evidence. The original execution artifacts remained unchanged.
A complete M2 claim remains outside this bounded family.

The eight controls are wrong positive healing, wrong target stat, wrong
selected-slot consumption, wrong Guard Spec countdown, wrong Dire Hit focus
flag, missing initial WRAM, altered executed host bytes and altered actual
Oracle bytes. Semantic edits update both observation views, caches and derived
raw/report witnesses consistently, then rehash cases and successful log rows.
Staging uses hardlinks and atomic file replacement so original accepted bytes
remain unchanged. No battle or build reruns belong to this validation.
