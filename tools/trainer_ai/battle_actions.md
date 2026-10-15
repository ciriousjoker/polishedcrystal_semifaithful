# Bounded voluntary actions and heterogeneous native battles

`battle_actions.py` and `battle_teams.py` extend the conditional source-backed
battle experiments. This is an experimental headless IID-DIV profile, not an
ordinary graphical battle or general Polished Crystal simulator.

## Supported decisions

- `MoveAction(0|1)` selects Tackle or Water Gun with remaining PP.
- `SwitchAction(index)` selects a living, non-egg inactive member of a two-member
  team. Actual `UserCanSwitch` determines trapping legality on a temporary full
  CPU/RAM/RNG copy. Querying legal actions leaves the fight unchanged.
- Enemy `ItemAction(0|1)` consumes the selected trainer inventory slot. There are
  exactly two source trainer slots. The player bag is a separate unsupported
  action path. `inventory=(name_or_none, name_or_none)` accepts Potion,
  Super Potion, Hyper Potion, Max Potion, Full Heal and Full Restore by their
  uppercase underscore names. Duplicate items and empty slots are allowed. Occupied
  supported slots remain action-legal even when their effect would be wasteful;
  the policy replaces the original AI selection heuristics, including HP and
  highest-level checks.

Both actors choose from public observations and masks before either action is
committed. Observations expose both complete fixture rosters and trainer
inventory, including individual reserve types and abilities. They exclude
current action RAM, RNG state and future DIV input. Illegal choices reject before
mutation; execution failures poison the environment until reset.

Three fixed two-member teams are available: Psyduck/Pidgey, Sentret/Spearow and
Smeargle/Hoothoot. Per-member types and ability slots are read from assembled
`BaseData`; source `SendInUserPkmn` resolves the actual active caches. The selected
abilities include Technician, exercising a meaningful indirect damage handler.
Level-50 zero-IV/EV neutral stats are constructed from that same table. These
are synthetic battle-start fixtures, with Tackle and Water Gun on every member.
They do not establish natural learnset or team-builder legality.

## Actual source execution and controller scope

Moves run actual `DetermineMoveOrder` and `DoTurn`. Source `DoTurn` records the
explicit deferred switch flags; actual `DeferredSwitch` performs withdrawal,
Pursuit checking, switch abilities, outgoing party synchronization, send-in,
hazards and entry ability callers. Source `ResetAbilityIgnorance` runs after each
actor. The host records `wEnemyGoesFirst` exactly as `BattleTurn` does. Source
party synchronization and `ResolveFaints.check_battle_over` decide termination;
win/loss routines are terminal boundaries before progression and rewards UI.

Trainer item effects run before move ordering, matching `ParseEnemyAction`.
The policy chooses an item instead of invoking the original AI heuristic.
Every occupied supported slot remains selectable even if the effect is wasteful;
this policy contract replaces the old highest-level, HP and status selection
heuristics and does not implement the player bag's no-effect rejection rules.
Actual `EnemyUsed*` routines and `AI_TryItem.used_item` execute the effect,
party synchronization, inventory removal and turn flags. `AIUpdateHUD` is not a
presentation no-op: its party updates, item-state decrement and flags execute.
`BattleAnimateHPBar` also executes its palette write; only its lower
`AnimateHPBar` callee is a rendering return. The initial item-state byte is the
source trainer initialization value `$ff`.

The complete `PerformMove` postlude and `HandleBetweenTurnEffects` controller
are not yet translated into this host phase sequence. Full fights therefore
start with neutral weather, hazards, held items, status, trapping and delayed
effects. The selected moves cannot introduce them. Direct Full Heal/Full Restore
status probes validate those effects without claiming residual-status battles
in this combined environment. The earlier independent status slice remains
separate. Pursuit as an attack, arbitrary species, other move effects, simultaneous
replacement, Struggle and frontend frame scheduling remain unsupported.

Explicit identity-return rendering hooks are listed in
`battle_actions_profile.PRESENTATION`. They extend the earlier party projection
with speech box/HUD clearing, lower HP animation, sound and text rendering.
Meaningful battle callers execute unchanged. This projection and independent
uniform DIV bytes are deliberate semantics; parity does not prove hardware DIV
independence or ordinary frontend fidelity. Unknown instructions, PCs, MMIO,
unsupported branches and event exhaustion fail closed.

## Provenance and Assembly updates

`ActionEngine` loads one independent, exactly verified native profile; it never
borrows baseline libraries for a changed ROM. The generator binds ROM, symbols,
all tracked game inputs, entries, stops, exclusions and generator identity.
Every cold construction checks that profile before fixture execution. Reset
reuses verified handles without compilation or source hashing.

The profile additionally pins the whole ROM and symbol layout, plus a normalized
aggregate of all tracked game inputs. Only these exact source/ROM computation
fields can vary automatically:

- Tackle power, 2 through 255, inherited from the earlier experiment.
- Super Potion's actual `ld b, amount`, 50 through 100.
- Non-faithful Pidgey base HP, 40 through 60, with matching BST.

Each source value must agree with its assembled byte. All other source/ROM
changes, including transitive skipped renderer changes, require renewed audit.
This is conservative update safety, not arbitrary game-update compatibility.

`battle_actions_updates.py` copies the existing incremental game build once,
changes actual Assembly Super Potion 50 to 75 and Pidgey HP 40 to 55, and builds
with `make -j1`. It rejects the old native artifact, regenerates for the changed
ROM and compares changed callers plus a complete fight against headless mGBA.
It also rejects unrelated controller source, skipped rendering code and
source/ROM disagreement. Existing verified experiment caches are reused.

## Serial verification

Run only one command at a time. No GPU or numerical worker pool is used.

```sh
python3 -B tools/trainer_ai/battle_actions_profile.py \
  --rom "$GAME/polishedcrystal-3.2.3.gbc" \
  --sym "$GAME/polishedcrystal-3.2.3.sym" \
  --source-root "$GAME" --build-dir "$ARTIFACT_ROOT/build/battle-actions/full"
python3 -B tools/trainer_ai/battle_actions_check.py --artifact-root "$ARTIFACT_ROOT"
python3 -B tools/trainer_ai/battle_actions_edges.py --artifact-root "$ARTIFACT_ROOT"
python3 -B tools/trainer_ai/battle_actions_updates.py --artifact-root "$ARTIFACT_ROOT"
python3 -B tools/trainer_ai/battle_actions_bench.py --artifact-root "$ARTIFACT_ROOT"
```

The first command creates a single serial C compilation. mGBA is used only for
validation; native complete-fight rollouts and the throughput sample do not use
an emulator. Evidence is written below `build/battle-actions` outside Git.

## Recorded bounded results

The retained run validates 36 complete fights across all three team pairings,
both orientations and six opening action combinations with 5,340 exact caller
comparisons. Another 24 item-effect cases cover HP clamping, high-byte carry,
status/confusion clearing, party synchronization and the item-state byte.
Two native complete trajectories replay identically after resetting the same
verified handles. The changed-Assembly experiment includes 163 comparisons,
including five direct updated callers and a complete ten-turn fight.

A single serial 20-fight native sample measured 12.78 fights/s, median 79.45 ms
per complete fight; cold verification took 0.831 s separately. It includes reset,
public roster observations, legal masks and scripted action selection. It does
not include learning or network inference, and it is not a scale estimate for
arbitrary full-roster battles. These broader callers and copied-state legality
queries are slower than the earlier small 1v1 fixture; the runtime is still an
instruction dispatcher with Python orchestration and per-call DIV previews.
Evidence and exact source/dependency fingerprints are in
`build/battle-actions/final-identity.json` under the external artifact root.
