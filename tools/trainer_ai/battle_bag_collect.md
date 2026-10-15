# Retain positive restorative player-bag coverage

`battle_bag_collect.py` converts the actual restorative-family report to the
unchanged strict coverage ledger. It recognizes only the 18 item variants in the
reviewed driver and derives their `player-bag-item` and six `player-bag-effect`
keys from the current source inventory. It accepts no caller-provided rule keys
and promotes no controller, status or unrelated mechanism from these fights.

The collector requires the exact 14-source execution checkpoint. It never reads
mutable current fight/bag hosts as if they produced the retained runs, and never
imports or executes a native controller, emulator or build. The exact driver,
Oracle, replay checker, bag decoder and direct ABI sources are hash-pinned and
retained. Report and raw case host identities must match that checkpoint.
Actual per-case oracle source, loaded dylib, libmgba and State/Hook ABI identities
must match the retained bytes. Missing identities cannot be backfilled.

Each positive case must have source terminal success, report-confirmed exact
whole-fight replay, nonzero recorded source comparison segments, a matching
first typed BagAction party/move target and opposing Splash intent, and the
reviewed isolated fixture. The collector re-reads assembled item IDs, attributes,
parameters and effect dispatch pointers from ROM/SYM, checks source inventory
mapping and recomputes the positive target HP/status/revival/happiness/PP outcome.
Status probes must exercise the intended item-specific initial status. PP probes
must match assembled move maximum PP and the actual selected move.

The raw format retains the selected target's before/after observations and a
`consumed: 1` witness. It does not retain complete pre/post bag observations or
unselected party snapshots. Exact consumption and unchanged other-party/opponent
checks therefore rely on the retained, reviewed driver's executed assertions;
the collector does not invent independent inventory counts or missing snapshots.
Likewise the successful exact replay flag is emitted by the original report only
after all retained segment states and whole-fight results matched. This is a
trusted source-bound evidence transformation, not a new execution or an
independent reconstruction of all missing raw observations.

Original report/case/checkpoint/execution/WRAM bytes are kept under the new empty
evidence directory. Derived case and adapter reports reference only root-relative
artifacts. Runtime options are independently read from retained initial WRAM by
the existing gate. The currently known Set/banked-WRAM/LCD-disabled execution
profile is checked explicitly. Blocked ineffective inner-menu retry remains
visible in the adapter and `collection.json`, contributes no claims and survives
later coverage merges. Positive branch evidence does not establish every failed
use, rejection or retry behavior for these items, or overall item completeness.
Ordinary frontend equivalence and M2 completion remain unproven.

The existing bag-family run contains 18 positive item variants, 129 source rounds
and 15,991 comparison segments plus exact replay. Expected collection: 18 item
keys and six effect-family keys. Root owns execution and validation of this new
adapter. Exact next command after allocation of the single CPU slot:

```sh
python3 -B tools/trainer_ai/battle_bag_collect.py \
  --inventory /Volumes/Storage/tmp/polished-ai/build/battle-coverage/inventory.json \
  --report /Volumes/Storage/tmp/polished-ai/build/battle-controller/bag-family/report.json \
  --host-checkpoint /Volumes/Storage/tmp/polished-ai/build/battle-controller/bag-family/host-source-checkpoint \
  --manifest /Volumes/Storage/tmp/polished-ai/build/battle-controller/native-o3/manifest.json \
  --rom /Volumes/Storage/tmp/polished-ai/build/stateful/game/polishedcrystal-3.2.3.gbc \
  --symbols /Volumes/Storage/tmp/polished-ai/build/stateful/game/polishedcrystal-3.2.3.sym \
  --build-receipt /Volumes/Storage/tmp/polished-ai/build/battle-coverage/original-build-receipt.json \
  --build-log /Volumes/Storage/tmp/polished-ai/build/stateful/game-build.log \
  --oracle-library /Volumes/Storage/tmp/polished-ai/build/battle-controller/controller-oracle-7840c8b7a31bc643.dylib \
  --mgba-library /Volumes/Storage/tmp/polished-ai/build/stateful/mgba/lib/libmgba.0.10.5.dylib \
  --output /Volumes/Storage/tmp/polished-ai/build/battle-controller/bag-family-retained
```

Then invoke the independent strict gate with that ledger/evidence root. Collection
returns zero when its evidence is valid even while the M2 gate fails for missing
rules. An invalid positive witness, typed target, executed-host checkpoint,
oracle identity, ROM dispatch or missing raw artifact rejects with exit 2 and no
ledger. Integrity checks should mutate only disposable copies, preserving the
original bag-family evidence and checkpoint.

## Executed checkpoint

The coordinator collected the actual 18 positive cases into 24 item/effect keys.
Two actual negative checks reject a wrong healing witness and a wrong party
target without writing a ledger. Merging with the existing first-family and
stat-stage evidence preserves 68 distinct keys and both excluded controls.
No fight was rerun and no original source/evidence byte was changed.

Evidence: `build/battle-controller/bag-family-retained`,
`build/battle-controller/bag-collector-negative/report.json` and
`build/battle-corpus/combined-with-bag-o3` in the external project. This checkpoint
still leaves inner retry and overall M2 coverage open; it proves the listed
positive restorative uses only.
