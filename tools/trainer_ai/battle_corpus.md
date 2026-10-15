# Full-fight mechanic corpus

`battle_corpus.py` creates scenario **plans**, not coverage evidence. Its inventory
binding is the source inventory hash emitted by `battle_coverage.py`. No row is
accepted merely because a move name or an effect ID exists in the catalog.

The initial factory enumerates every source move, both actor sides, and two
recorded seeds. For the current 255-move inventory this is 1,020 plans. This is a
finite first pass, not enough seeds to guarantee activation of secondary effects.
The catalog explicitly lists all other required rule keys as a backlog. Ability,
held-item, player-bag, status and field coverage cannot be inferred from the move
plans. No catalog has been executed or validated yet.

## Runner contract

The proposed `ControllerFight` API supplies `observation(side)`,
`legal_actions(side)`, and `step((action0, action1), choose=...)`. Resolve both
intents from frozen predecision observations before calling `step`. Typed move,
switch, trainer-item, continuation and Struggle actions remain controller-owned.
A policy must not inspect the other player's submitted action or event tape.

`scenario(case, status_values=...)` creates the existing `Scenario` and `Pokemon`
records lazily. Derive status masks from the same source as the runner. The
three-turn sleep counter is a count in the source's low three status bits.
Levels remain fixed and the actual no-experience option remains set. Ordinary
stat stages, status, Transform and source state changes still execute normally.
Both parties have a reserve and trainer inventory is empty for isolated moves.
The baseline Horsea has Swift Swim in slot zero and pure Water typing in the
bound default source, avoiding passive immunity to any attacking type or primary
status. Weather may activate Swift Swim, which the source must execute.

`desired_moves(case, turn)` supplies the named setup/probe intents. A missing
requested probe move must fail or be classified explicitly. It must not silently
fall back to an ordinary attack and thereby manufacture a successful probe.
After the probe the finishing policy may choose a legal attack, including forced
continuation and Struggle, and first living reserve. This allows Sketch,
Transform, Encore, Disable and PP depletion to affect the legal action set.
A turn limit is a truncation and never successful full-fight evidence.

## Meaningful evidence

Each accepted case must preserve the initial scenario, seed, source/ROM/SYM/native
manifest provenance, submitted typed actions, semantic observations and actual
full-fight terminal state. Compare source execution with the independent game
oracle throughout the complete fight. Also replay natively from the same seed.

Check separately that the probe really ran and that the intended rule activated.
PP depletion alone is insufficient: sleep, immunity, missed attacks, disabled
moves and failed preconditions can all consume turns without testing the effect.
Use retained state transitions and/or real executed source branch identities.
Do not replace source battle commands with hooks that fabricate their effects.
A damaging move needs a successful hit as well as any desired miss control.
Secondary-effect cases need at least one seed which actually triggers the effect.
Probabilities and damage boosts need contrasting cases, rather than one sample.
Splash is explicitly a positive test of the source's intended no-effect path.

The generic recipes include injured healing/draining users; sleeping Dream Eater
and Sleep Talk states; poisoned Heal Bell parties; prior opposing moves for
Disable, Encore and Sketch; category-matched retaliation for Counter and Mirror
Coat; damaging opposing intent for Sucker Punch; removable berries for item
manipulation; differing HP for Pain Split; and stat setup before Baton Pass.
Multi-turn moves, delayed damage and Perish Song require multiple observed turns.
These are executable plan preconditions, not proof that a semantic assertion has
passed. For example a speed tie can defeat the intended Counter precondition and
must remain a failed/unsatisfied case rather than accepted coverage.

## Explicit unprepared prerequisites

`Case.ready` means that scenario materialization has no known fixture API gap.
It does not mean that a case is correct, tested or guaranteed to activate. Cases
with `fixture_requirements` stay blocked. Current blockers include typed nested
Conversion selection; explicit genders for Attract; switching into hazards and
Pursuit; source-established screens for Brick Break; Dig/Fly/Minimize conditional
power interactions; and deliberately lethal incoming attacks for prevention and
Destiny Bond. Ghost Curse needs an additional Ghost fixture. Transform requires
switch-out restoration assertions. Weather healing needs contrasting weather
variants. All are visible rather than counted by ordinary move execution.

The next corpus increment should make setup a sequence of typed source actions,
including explicit switches and harmless waiting moves, with per-turn predicates
which abort as soon as a prerequisite is not established. A fixture may set
reviewed valid initial party state; it must never manually execute battle rules
or edit HP/status/RNG after the battle has begun.

## Catalog generation

After the controller owns no heavy CPU slot, the coordinator may run this cheap
catalog command and the focused factory checks with one CPU:

```sh
python3 tools/trainer_ai/battle_corpus.py \
  --inventory /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-coverage/inventory.json \
  --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-coverage/move-corpus.json
```

Native compilation, emulator comparison and actual full fights require the
coordinator's separate serial CPU handover. Catalog generation itself runs no
native library, training, simulator or emulator.

## Semantic activation observers

`battle_corpus_observers.py` adds executable predicates for clear first families:
normal/priority/always-hit and static/level damage; primary poison, Toxic, burn,
paralysis, sleep and freeze; selected corresponding secondary statuses;
confusion; direct healing and weather healing; named single-stat and compound
stage changes; weather start; screens/guards; and intentional Splash no-effect.
An unknown effect is `blocked`. Normal HP damage cannot certify an arbitrary
move's secondary effect, multi-hit/recoil, charge, switching or ability branch.
The exact arithmetic and complete state still require ROM differential checks.

The validation-only `MoveFrame` protocol contains `before` and `after` public
observations, `actor_side`, actual `executed_move` ID, genuinely executed
`source_commands`, and actor-local `attack_missed`. `single-source-move` spans
must include actual `BattleCommand_usedmovetext` execution and relevant source
handlers. The runner must collect these diagnostics from source execution,
rather than manufacture them from the submitted action or PP change.
Observers never receive a future RNG tape and these diagnostics are not model
inputs. A faint followed by replacement conservatively leaves the generic HP
predicate unsatisfied, because a new entrant's HP must not be mistaken for the
original target's damage.

For practical first validation without a new instruction tracer, an alternative
`controlled-source-turn` frame can prove clear mechanisms. It requires verified
actual actor execution, actual opposing Splash, unchanged active indices, only
neutral Swift Swim abilities, no starting status or held items anywhere, no
weather, hazards, screens, guards, damaging volatile conditions or queued damage.
Those restrictions are derived from observations, not an arbitrary success flag.
This permits ordinary damage, primary status, healing, stage and weather tests.
Secondary effects stay blocked in this mode, requiring a finer isolated span.
A whole-turn record with an attacking opponent, poison, weather damage or a held
trigger cannot pass this shortcut. Existing source-backed provenance and full
ROM comparison must also hold. Actual source execution verification includes
disambiguating sleep, forced continuation, incapacitation and failed selection.

`source_bindings(machine, inventory)` checks relevant source hashes and resolves
status/ability/weather constants through the actual runner. Predicate results
are `activated`, `unsatisfied`, or `blocked`. They never confer full-fight
coverage by themselves. Retain all sampled frames, including earlier misses,
when looking for a later positive multi-turn transition. Delayed/locked moves
without an explicit reviewed observer remain blocked even if ordinary damage
occurs later.

`battle_corpus_observers_check.py` is an unexecuted synthetic regression suite
for rejecting false positives: PP-only changes, missed/different moves, unknown
or confounded spans, stale source bindings, initial poison/items/weather,
nontriggered secondaries and unknown effects. It also prepares positive examples
of damage, primary status, weather, healing, stages and Splash. Synthetic tests
validate the predicate implementation and are never game-mechanic evidence.
