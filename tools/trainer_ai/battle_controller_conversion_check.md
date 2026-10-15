# Player Conversion nested type choices

Three complete source fights passed: 23 turns, 2,610 actual-ROM comparisons
and exact recorded native replay. Missing policy and current-type invalid
choices also failed closed and required reset, which recovered correctly.
The actor receives an explicit `conversion_chooser`
input at the real move menu's type-2 branch. The nested slot mask includes known
moves even at zero PP and excludes UNKNOWN_T/current active types. It is not an
ordinary usable-attack mask. The adapter checks the source scaffold, assembled
farcall operand and actual live return frame into player Conversion, then
supplies the selected cursor and A input. Source continues its own validation,
restores the original attack fields and changes types. No enemy choice or RNG
state enters the detached actor observation.

The bounded full-fight runner executed:

- Porygon with its source Analytic ability chooses Conversion in slot 0, whose Normal type is already active;
  the nested actor explicitly chooses zero-PP Water Gun in slot 1.
- The same own input and initial state with a different hidden enemy attack;
  the nested actor-input hash must remain identical.
- A Water actor explicitly chooses the Normal Conversion slot as its type
  target, exercising the opposite conversion direction.

Each case requires actual type changes, unchanged zero target PP, exactly one
nested decision, retained hidden enemy intent, terminal continuation, complete
actual-ROM segment comparisons and exact recorded native replay. Missing policy
and a current-type choice must reject and poison until reset. Initial WRAM,
host-source hashes and actually loaded oracle identities are retained.

Run after the coordinator grants the sole CPU slot:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/battle_controller_conversion_check.py --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/conversion
```

No core, profile, Oracle, scenario or source-game code is changed. Existing
presentation/input projections remain conditional. Enemy Conversion continues
through the actual source AI and is not covered by this nested player adapter.

Evidence is retained at
`/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/conversion/report.json`.
The two Normal-to-Water cases have identical nested actor-input hashes despite
different hidden enemy current actions. Source SetChoiceLock stores that private
slot in the high nibble of EncoreCount before execution; the nested observation
therefore conservatively retains the predecision high nibble and the actual
live low countdown. This transformation is source-scaffold guarded and writes
no machine state.

The initial default Trace fixture correctly rejected an unsupported FF4F VRAM
bank write in the ability-overlay display before the first decision. The final
Conversion fixture uses Porygon's actual alternative Analytic ability to avoid
that unrelated unsupported display path. Trace presentation remains an explicit
M2 gap. A broader pivot/replacement observation privacy follow-up is also needed:
those fresh callbacks can reach the same packed current-action nibble before
the opposing actor has performed its move. This extension does not silently
claim that separate callback family covered.
