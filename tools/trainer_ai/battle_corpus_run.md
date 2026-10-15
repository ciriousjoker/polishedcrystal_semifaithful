# Bounded mechanic corpus execution

`battle_corpus_run.py` bridges source-bound move plans and semantic observers to
`ControllerFight`. Its first actual damage case has passed; broader mechanic coverage remains
pending. Run it only after the typed controller slice is stable and the project's
single heavy CPU slot is available. The runner uses existing native/controller
artifacts and the mGBA oracle; it does not build a ROM. Existing loader/oracle
helpers may compile missing native artifacts, so schedule the command as heavy
work even for a single case.

Start with one damage case and one side:

```sh
python3 tools/trainer_ai/battle_corpus_run.py \
  --artifact-root /Volumes/Storage/tmp/polished-ai \
  --inventory /Volumes/Storage/tmp/polished-ai/build/battle-coverage/inventory.json \
  --output /Volumes/Storage/tmp/polished-ai/checks/corpus-tackle-side0 \
  --moves TACKLE --sides 0 --seeds 193
```

Then use a fresh output directory and expand to the first bounded list:

```sh
python3 tools/trainer_ai/battle_corpus_run.py \
  --artifact-root /Volumes/Storage/tmp/polished-ai \
  --inventory /Volumes/Storage/tmp/polished-ai/build/battle-coverage/inventory.json \
  --output /Volumes/Storage/tmp/polished-ai/checks/corpus-first-families \
  --moves TACKLE THUNDER_WAVE SWORDS_DANCE GROWL RECOVER \
          REFLECT LIGHT_SCREEN RAIN_DANCE SUNNY_DAY SPLASH \
  --sides 0 1 --seeds 193 --max-turns 256
```

To use the separately built optimized artifact, add
`--native-build /Volumes/Storage/tmp/polished-ai/build/battle-controller/native-o3
--optimization O3`. The default remains the existing `native` directory with
`O0`. The selected directory and optimization are recorded in the report; the
existing verified loader binds that configuration to its manifest.

These are candidate cases, not a promise of activation or coverage. Missing move
names reject the request. Explicitly requested unsupported effects/fixtures remain
blocked in the report. Further supported effects can be requested with `--moves`;
confounded rounds remain blocked under the existing observer's fixture predicate.
Seeds are deterministic scenario/RNG initialization, while actual hardware DIV
samples are recorded from the reference and supplied to native replay.

## Evidence contract

The actor policies read only their detached observation and legal mask. Probe
intents come from `desired_moves`; unavailable requested probe moves reject the
case. After the probe, both sides explicitly prefer legal Tackle, then Water Gun,
then the existing first-attack policy, including typed forced continuation and
Struggle. Faint replacements use the explicit first-reserve chooser. Source code
owns all mechanics, terminal results, NO_EXP behavior, fixed levels and permanent
base stats. There are no post-reset mechanic or RNG edits.

Actual execution identity is read at the existing `StdBattleTextbox` suspension
only when the immediate return address matches the unique source/ROM near call
in `DisplayUsedMoveText` before its `ApplyTilemapInVBlank` tail jump. The source
scaffold, bank and assembled call/jump operands must all match. Unknown or
ambiguous scaffolds reject execution. The diagnostic reads `hBattleTurn` and
`wMoveGrammar`; submitted actions and PP changes never establish move identity.

The existing `LoadTileMapToTempTileMap` suspension at the end of `PerformMove`,
before fallthrough into `ResolveFaints`, records `wAttackMissed` for that same
actor. Its exact source/ROM callsite is separately bound. This avoids reading the
opponent's overwritten miss flag after a round. Neither diagnostic is given to an
actor, and neither requires new source hooks or changes to the controller profile.

Before/after observations span a **whole source round**, including end-turn
processing. They are explicitly `controlled-source-turn`, never single-move
frames. A candidate frame needs exactly one matched move identity and matching
end-of-move miss capture per side. Existing semantic predicates additionally
require neutral baseline fixtures and actual opposing Splash, and reject
unsupported/confounded secondary effects. A missed or skipped opposing Splash
can leave status cases blocked even when another visible transition occurred.

The prepared `source-action-span` path additionally retains complete detached
source observations at the actual used-text call and its matching end-of-
`PerformMove` call, for the selected actor during the bounded probe turns. It
reads the live observation accessor directly, because the fight's public actor
observation remains frozen at the previous decision. Every capture asserts that
the complete live State bytes and RNG state are unchanged. These diagnostic
observations are retained in the execution trace and frames, and exact replay
must reproduce them. They are never added to actor inputs.

Local spans preserve actual turn, side, move, miss result and both source/ROM
callsite contracts. Nested starts, identity changes and spans still pending at
the next round boundary reject execution. No individual BattleCommand execution
is invented: `source_commands` stays empty for this phase. The reviewed local
observer permits only existing primary status/confusion and their secondary-hit
families, requires neutral Horsea with Swift Swim, no items, baseline status,
volatiles or active field effects, and unchanged participants. The used-text to
end-of-`PerformMove` interval excludes the opponent action and end-turn residual
processing. Neutral abilities/items exclude on-hit attribution conflicts.
Secondary effects need both actual target HP loss and newly applied matching
status or positive confusion duration. Local acceptance does not require the
opponent to complete Splash later in the round. Controlled-round observers
retain their existing opposing-Splash restriction and reject secondary effects.

This path and the added synthetic false-positive checks are code preparation
only. They have not been imported, run or accepted against actual game evidence.
After coordinator review, collector hash refresh and CPU-slot allocation, the
next bounded actual command is:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/battle_corpus_run.py \
  --artifact-root /Volumes/Storage/tmp/polished-ai \
  --inventory /Volumes/Storage/tmp/polished-ai/build/battle-coverage/inventory.json \
  --native-build /Volumes/Storage/tmp/polished-ai/build/battle-controller/native-o3 \
  --optimization O3 \
  --output /Volumes/Storage/tmp/polished-ai/checks/corpus-secondary-local-o3-01 \
  --moves DYNAMICPUNCH POISON_STING SCALD SPARK \
  --sides 0 1 --seeds 193 194 --max-turns 256
```

These four move names are present in the current inventory. DynamicPunch has
100% secondary confusion chance after a successful 50%-accuracy hit; the other
three have 30% secondary chance. Two fixed seeds give sixteen bounded full-fight
cases. An absent hit or secondary activation remains unsatisfied or blocked,
and no result is promoted without terminal/oracle/exact-replay evidence.

A row is accepted only when the semantic observer reports activation, the full
fight reaches an actual source terminal without truncation/fault, all source
segments compare with independent mGBA CPU/memory execution, and taped native
replay reproduces the full segment sequence, diagnostic trace, observations,
normalized state at reset and every round, and public fight record. State
normalization excludes only unused event-tape transport capacity, following
`RecordedRun.digest`; it does not drop executed CPU or memory state. Raw hashes
that include unused tape bytes are retained as original diagnostic provenance.

Each case JSON retains its scenario plan, exact source inputs, inventory digest,
runner/fixture/helper hashes, ROM/symbol/manifest hashes, snapshots, actual move
trace, semantic result, comparison records, consumed event tapes and source fight
record. Immediately after actual reset, the runner captures the initial WRAM
bytes at `C000:DFFF`. A successful execution retains them separately as
`<case>-initial-wram.bin`, with SHA-256, address range and boundary in the case
evidence. Native replay must reproduce those exact initial bytes. This enables
a later gate to verify actual options, including NO_EXP, abilities and battle
style. No raw state dump is retained for every round. Host controller, decisions,
actions, environment, bag and controller-profile code are also hashed in provenance.
Failures retain available partial diagnostics. If reset or execution fails before
a complete fight record is available, a minimal CPU position, fault and raw
state digest are retained without claiming a completed reset or complete provenance. The report retains
blocked, unsatisfied and truncated outcomes, returns a nonzero exit status unless
all requested cases are accepted, and never automatically edits a coverage ledger.
The existing presentation-hook abstraction still limits ordinary frontend claims.

## Executed checkpoint

The coordinator executed Tackle for actor side 0 with seed 193 against the
verified O3 controller. The actual damage mechanism activated, the complete
fight reached source terminal after 18 rounds, all native segments matched the
independent ROM oracle, and recorded-event native replay was identical. Evidence
is retained in the external project at
`build/battle-corpus/first-native-o3/report.json`. The complete validation command
took 4.36 seconds wall time, including oracle execution and replay; this is not
native-only training throughput.

Only the observed `move:TACKLE` and `effect:EFFECT_NORMAL_HIT` are candidate
coverage keys from this checkpoint. No other move, ordinary frontend projection
or complete M2 coverage follows from this result. The broader command above and
coverage-registry integration remain pending.

## Executed local secondary checkpoint

The bounded four-move, two-side, two-seed run has now executed against the frozen
O3 controller. All sixteen fights reached source terminal and exact RecordedRun
replay. DynamicPunch activated on side 0 with seed 193 and side 1 with seed 194;
the other fourteen cases remain unsatisfied. Only `move:DYNAMICPUNCH` and
`effect:EFFECT_CONFUSE_HIT` gained positive witnesses. Poison Sting, Scald and
Spark gained no coverage from this run.

The accepted action-local snapshots show target HP 90 to 65 with confusion
count 0 to 2, and target HP 90 to 64 with confusion count 0 to 4, respectively.
Both have actual miss byte zero. The source-controlled full fights each lasted
sixteen rounds. The retained accepted fights contain 3,426 independent Oracle
comparison rows in total, with terminal and exact replay evidence.

An initial attempt rejected every case because it incorrectly assumed
`wMoveGrammar` remained a move identifier at the final boundary. The source
aliases that byte with `wApplyStatLevelMultipliersToEnemy` in `ram/wramx.asm`;
`engine/battle/core.asm` writes the latter during battle processing. The corrected
runner preserves move identity only at the bound used-text call and retains the
later scratch byte explicitly as `end_move_grammar_scratch`. Matching side, turn,
source-bound end and absence of nested starts still enforce interval identity.
Original failed outputs remain under `checks/corpus-secondary-local-o3-01`.
The corrected immutable outputs are `checks/corpus-secondary-local-o3-02` in the
external project. No source mechanics, hooks, native artifact or Oracle changed.

## Prepared secondary stat-stage batch

The local observer is prepared to admit already recognized secondary stage-hit
families, while retaining its neutral Horsea fixture and exact source-interval
requirements. Activation needs target HP loss plus the named stage changing on
the correct side in the correct direction. Primary stat moves retain their
existing controlled-round path. `EFFECT_ALL_UP_HIT` adds the source's five-stat
self-increase recipe; all five stages must increase together with actual damage.
Accuracy and evasion are excluded from that five-stat recipe.

Source `AttackUpHit`, `DefenseUpHit`, `SpeedUpHit`, `AllUpHit` and target-down
scripts place used-move text before damage and their secondary stage operations.
The source's `raisestathit` applies to the user, while `loweroppstathit` includes
`STAT_TARGET | STAT_LOWER`. All effects finish before the existing final
`PerformMove` tilemap call. No new trace or move-effect implementation is added.

After current native/controller source is frozen and reviewed, source hash pins
are refreshed, and the coordinator grants CPU, the prepared batch is 24 cases:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B tools/trainer_ai/battle_corpus_run.py \
  --artifact-root /Volumes/Storage/tmp/polished-ai \
  --inventory /Volumes/Storage/tmp/polished-ai/build/battle-coverage/inventory.json \
  --native-build /Volumes/Storage/tmp/polished-ai/build/battle-controller/native-o3 \
  --optimization O3 \
  --output /Volumes/Storage/tmp/polished-ai/checks/corpus-secondary-stages-o3-01 \
  --moves FLAME_CHARGE MUD_SLAP ICY_WIND ROCK_SMASH METAL_CLAW ANCIENTPOWER \
  --sides 0 1 --seeds 193 194 --max-turns 256
```

The current source/inventory maps Flame Charge to user Speed up, Mud-Slap to
opponent Accuracy down, and Icy Wind to opponent Speed down, each at 100% effect
chance on a successful hit. Metal Claw has 10% user Attack up and AncientPower
10% all-five-stat self increase. The preparation incorrectly read Rock Smash's
inactive faithful source branch as 50% Defense down. Its current non-faithful
inventory effect is `EFFECT_BRICK_BREAK`, which needs an unsupported screen
fixture and is retained as blocked. The executed checkpoint below supersedes
these preparation assumptions. Existing confusion fights remain separately
retained.

## Executed secondary stage checkpoint

The bounded 24-case batch completed: twelve positive cases, eight unsatisfied
cases and four blocked cases. Flame Charge, Mud-Slap and Icy Wind activated on
both sides for both seeds. Metal Claw and AncientPower did not activate their
secondary stages in this bounded probe window. Rock Smash's current source
inventory requires the separate Brick Break screen fixture, so all four cases
were blocked before game execution. The unsupported cases were preserved.

All twenty executed fights reached source terminal with exact native replay.
The twelve accepted fights contribute 21,907 independent source/Oracle
comparisons and six move/effect keys. There is no actual activation claim for
Attack-up-hit, five-stat-up-hit, or Defense-down-hit from this batch. Synthetic
observer controls passed, and both consistently removed-stage and removed-damage
collector negatives rejected. Exact cases live under
`checks/corpus-secondary-stages-o3-01`; retained originals are under
`build/battle-corpus/secondary-stages-retained-o3-01` in the external project.
