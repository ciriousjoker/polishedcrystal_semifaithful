# Focused rejected-item input checks

The four focused complete fights passed against the retained optimized native
artifact and actual-ROM oracle: 28 turns and 3,551 source comparisons, with exact
recorded native replay in every case. Existing successful positive-item host
sources were first preserved byte-for-byte under the bag-family evidence
checkpoint. No core, profile, oracle, decision or scenario code changed.

The new input point is the actual source `PrintText` call for
`WontHaveAnyEffectText`. The adapter checks its current bank, text pointer,
source call operand and return into `ItemNotUsed_ExitMenu`. Normal scrolling or
repeated low-level callbacks cannot trigger a retry. The source then continues
its own no-success flag, party/bag loops, inventory and turn control.

`bag_retry_chooser(side, observation, choices)` receives a detached player-only
input with the failed own BagAction. Its mask contains only same-item,
same-stack retargets and an explicit `CancelBagAction`. A retarget changes the
pending player input only. Cancellation uses the existing `BagInput.cancel()`
and source B-button handling to unwind. The old outer `retry_chooser` chooses a
new turn action only when the real battle menu reopens. The pending enemy
intent remains hidden and unchanged. Missing policy, invalid choices and more
than 32 inner retries reject; cancellation is not a top-level turn action.

The focused runner executed four complete source/oracle/replay cases:

- Potion on a full-HP active Pokemon, explicitly retargeted to an injured reserve.
- All-full Potion, explicitly cancelled, then a player attack at the real outer
  menu while the enemy's original Splash intent stays pending.
- The same cancellation with a different hidden enemy attack; inner actor input
  must be identical, while source eventually executes that retained attack.
- Ether on a move with maximum PP, explicitly retargeted to the depleted other
  move of that same Pokemon.

At rejection, item quantity, HP/status/PP and the completed-turn counter must
still match initialization. Actor input is deliberately mutated to check that
it is detached. A live source/host diagnostic checks enemy-intent preservation
before and after the policy input and, for cancellation, at the outer retry.
Final consumption and actual restoration or cancellation are checked before
normal attacks complete the full fight. Missing-policy and invalid-choice
checks also passed failure poisoning, unchanged live inventory/HP and explicit
fresh-reset recovery. State comparison rejects nested cancellation as an ordinary turn
action before any source transition.

Retained report: `/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/bag-retry/report.json`.
It includes initial WRAM, host-source hashes, ROM/SYM/native manifest identities
and actual oracle C/library/libmGBA identities. The two cancellation cases had
identical detached actor inputs despite different hidden enemy current actions.

Run only after the coordinator grants the project's single CPU slot:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/battle_controller_bag_retry_check.py --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/bag-retry
```

The detector covers this declared ineffective-target scaffold. Other menu
rejections, eggs, unsupported contexts and general UI interaction are not
implicitly classified. Existing presentation/input projections remain
conditional, and no coverage ledger or acceptance gate is changed here.
