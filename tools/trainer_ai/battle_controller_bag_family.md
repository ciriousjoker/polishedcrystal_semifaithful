# Positive restorative player-bag family

All 18 positive cases passed: 129 source rounds, 15,991 exact reference
comparisons and identical native whole-fight replays. No existing controller,
profile, compiler or oracle code was changed and no native rebuild was needed.
The first six handler representatives passed before the full family: Potion,
Full Restore, Full Heal, Revive, Revival Herb and Ether. The complete family
covers four Potion potencies, Full Restore, six status healers, three revival
items and four PP restorers. The repeated first-stage cases are not counted
again in the 18-case total.

Item IDs come from actual source constants. The runner verifies actual assembled
battle-party context, reads the assembled `ItemEffects` byte and dispatch pointer
and requires that pointer to identify the intended source handler. HP potency
comes from the assembled item parameter. This matters because player Super
Potion and Hyper Potion use their actual item data, which differs from the
trainer inventory's hardcoded effect routines. No trainer healing amount is
copied into the player-bag expectations. Base PP comes from assembled move data.

Initial scenarios contain the low HP, status, fainted reserve or depleted PP.
There are no midfight state patches. On the probe turn the player submits a real
BagAction, including party/move target, while the opponent uses Splash. The
first post-turn observation must prove both source consumption and the relevant
positive change. It also checks unselected party HP/status/PP and the untouched
opponent's HP/status, avoiding incoming damage as a confound. Revival Herb must
execute its separate happiness penalty. Afterwards both sides attack until the
actual ordinary trainer terminal; permanent stats and levels remain fixed.

Each case retains initial WRAM, source/ROM/SYM/native-manifest identities and the
loaded oracle's source, library, ABI and libmGBA provenance. Every segment uses
the independent all-bank oracle and an actual logical random-event recording,
then the complete fight is replayed natively with identical segment states and
outcome. Unknown paths, wrong activation, truncation and replay differences fail
closed. Presentation/input projections remain conditional as in the controller.

One no-effect/retry control is explicitly blocked, not counted as passed. A
rejected restorative can remain inside the source party or bag menu. The public
ControllerFight retry policy runs only after the outer battle menu reopens.
This runner does not mutate a private BagInput or invent a source cancellation.
That adapter gap remains visible for a later targeted fix. No acceptance gate
or item inventory row is relaxed or promoted by this preparation.

The executed first stage uses the six handler representatives against the
existing O3 native artifact:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/battle_controller_bag_family.py --artifact-root /Volumes/Storage/tmp/polished-ai --output /Volumes/Storage/tmp/polished-ai/build/battle-controller/bag-family --seeds 193
```

Use `--cases` to select additional named positive item cases. Other healing
items, PP Ups, confusion-only cures and additional interactions remain outside
this bounded family until separately activated and checked.

The subsequent complete execution used `--cases POTION SUPER_POTION
HYPER_POTION MAX_POTION FULL_RESTORE ANTIDOTE BURN_HEAL PARALYZEHEAL AWAKENING
ICE_HEAL FULL_HEAL REVIVE MAX_REVIVE REVIVAL_HERB ETHER MAX_ETHER ELIXIR
MAX_ELIXIR`, one seed (193), and the same O3 artifact. Actual player Super Potion
restores 60 and Hyper Potion 120, as encoded in their assembled parameters.

The final aggregate report is `<run>/build/battle-controller/bag-family/report.json`.
`report-first-stage.json` preserves the six-case feedback checkpoint. Reports
retain the actual loaded oracle C, native oracle library, State/Hook ABI files
and loaded libmGBA bytes. These successful positive conditions do not resolve
the explicitly blocked no-effect inner-menu retry or establish all restorative
interactions, PP Ups or confusion-only cures. No blocked case is counted as
passed and no broad M2 completion claim follows from this family.

The subsequent dedicated extension in `battle_controller_bag_retry_check.md`
now separately proves explicit no-effect retarget/cancellation for HP and PP
items. The historical 18-case report and its blocked-control description remain
unchanged. With that new host adapter, Potion, Ether and Revive were rerun as
positive regressions: three terminal fights, 21 turns, 2,606 source comparisons
and exact native replays, retained in
`<run>/build/battle-controller/bag-retry-positive-regression/report.json`.
