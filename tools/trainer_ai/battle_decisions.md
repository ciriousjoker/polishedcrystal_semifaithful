# Full controller decisions

`battle_decisions` supplies `observation(machine, side)` and
`legal_actions(machine, side)`. The controller must capture both actors before
committing either current action, including simultaneous replacement choices.
This API does not itself enforce the controller phase or cache observations.

`QUERY_ROOTS` must be included in the native manifest. Queries copy CPU, RAM,
stack and random state, then execute actual assembled `CheckLockedIn`,
`CheckUsableMove`, `UserCanSwitch`, `GetAbility` and `GetGender`. A suspension or random draw
rejects those queries. They cannot run a policy callback or a presentation callback.
Unknown native instructions, memory operations and event exhaustion retain the
controller's rejecting behavior.

Stat trainer items additionally query actual `EnemyUsedXItem` on an isolated
machine with A set to its item ID and enemy turn selected. Only this query may
return through the manifest's declared presentation boundaries, using the same
explicit conditional projection as the controller. Selection, terminal and
unknown boundaries and random draws still reject. Carry filters actual source
refusal, including the source's Contrary and stage-limit behavior. Guard Spec
and Dire Hit handlers execute unconditionally even when already active, so they
remain selectable. No original AI first-turn or desirability filter is applied.

Move, switch and enemy trainer item actions retain the earlier public dataclass
types. `TrainerItemAction` aliases `ItemAction`, with two inventory slots.
`ContinueAction` represents source-enforced recharge, charge, rampage or rollout.
`StruggleAction` requests the actual no-usable-moves source path. These intents
do not write move data or implement effects. Replacement is a separate phase;
the controller must restrict its choices to living, non-egg reserves.

Bag choices use the existing `BagAction`. Assembled item attributes identify
selectable battle contexts. Party-context actions include party and move target
intents; the actual item routine decides which target fields matter and whether
use succeeds, retries or consumes the turn. The mask does not apply the original
trainer AI's desirability thresholds, nor does it promise every selectable item
has an effect on its selected target. Ball use in a trainer fight must reach the
real source rejection. Learned action heads can later factor item and targets
instead of allocating one output per tuple.

Observations contain both full rosters, current HP and computed stats, moves and
PP, held items, source-resolved abilities, status, active types, stages, named
volatile flags and counters, field durations and hazards, and the actor's own
inventory. For side zero, `player_bag` contains the player bag and `trainer_items`
is empty. For side one, `trainer_items` contains the two trainer item slots and
`player_bag` is empty. Opponent unseen inventory is not exposed. Active battle records take precedence over stored
party records. The source's type, ability and temporary transformation state are
therefore preserved. Reserve abilities use actual `GetAbility`. Gender uses actual
`GetGender` with the stored true party species, form and index, matching the
source `CheckOppositeGender` callers even under Transform. Fixed-gender and
genderless species therefore follow source rules rather than a raw form-bit
guess. The isolated query sets `wMonType`, `wCurPartyMon` and `wCurPartySpecies`
only in its private copy. The public value is male, female or genderless.
The coordinator's twelve actual-ROM query comparisons passed on both party
layouts, including variable, fixed and genderless cases and contradictory active
form flags. Query CPU and memory results agree and the original state/RNG stays
unchanged. This is routine evidence, not full-fight Attract or Rivalry coverage;
report: `build/battle-controller/gender/report.json` in the external project.

Observations exclude XP, EV progress, next-level progress, pending current
actions, future random events, CPU state and scratch buffers. Scenario EVs remain
an initial stat-construction choice. Battle levels and base stats stay fixed;
combat stat changes still follow the game. Observation fields are candidates,
not a final neural-network feature schema. Later ablations should remove fields
whose removal does not hurt held-out full-fight quality.

`battle_decisions_check.check_machine` checks query isolation and detached
observations on an initialized machine. It is not a differential ROM test.
The source query entries and controller choices still require actual-ROM
validation. No checks have been executed during the initial code-preparation
handoff because another worker owns the single CPU slot.
