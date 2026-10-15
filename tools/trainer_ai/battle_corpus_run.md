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

## Executed active-inventory breadth checkpoint

The next batch was generated from current inventory using existing `move_cases`
and `supported_effect`, selecting ready side-0/seed-271 cases whose move key
lacked a positive claim in the 76-key ledger. It selected 113 moves, below the
150-case limit. The exact names, active effects, prerequisites, input ledger
hash and fifteen frozen host-source identities are retained in
`subagents/m2-ready-moves-01/artifacts/selected-plan.json` in the external run.

All 113 fights reached source terminal and exact native replay. Sixty-nine cases
activated, forty-two were unsatisfied and two were blocked by the existing
observer fixture. The accepted fights contain 123,776 independent Oracle
comparisons. The measured full command took 294.10 seconds including Oracle and
replay; this is no native-only throughput or model-quality measurement.

Both blocked moves, Hyper Voice and Aeroblast, knocked the target from 90 HP to
zero in their first action. Their actual local snapshots exist, but the current
normal-damage observer still requires a controlled round with actual opposing
Splash. That opponent action was absent after the knockout, so no coverage was
claimed. There were no controller/native faults or truncations. Secondary-hit
nonactivations remain visible rather than being inferred from move use.

Exact output lives under `build/battle-corpus/ready-moves-o3-01`, with original
fifteen host modules preserved under the task's `artifacts/frozen-runner`.
Collection retained all accepted and excluded cases separately. Source inputs
were checked unchanged before and after the batch. No engine, observer, runner,
controller, profile or native artifact changed for this breadth pass.

## Prepared local damage witness for first-action faint

The local observer now has a prepared admission for the existing `DAMAGE_ONLY`
effect set. It retains the exact action-local source boundaries, unchanged
participants, read-only observation assertion and neutral Horsea/Swift Swim
fixture. Existing damage semantics require miss byte zero and intended target
HP loss. Zero target HP remains a valid state at the bound end-of-`PerformMove`
call because `ResolveFaints` has not replaced that active participant yet.
No numerical damage model or new diagnostic hook is added.

This preparation does not alter the historical 113-case report. Fresh evidence
must come from Hyper Voice and Aeroblast on both sides with seed 271, four full
fights, after source review, hash refresh and CPU allocation. Planned command:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B tools/trainer_ai/battle_corpus_run.py \
  --artifact-root /Volumes/Storage/tmp/polished-ai \
  --inventory /Volumes/Storage/tmp/polished-ai/build/battle-coverage/inventory.json \
  --native-build /Volumes/Storage/tmp/polished-ai/build/battle-controller/native-o3 \
  --optimization O3 \
  --output /Volumes/Storage/tmp/polished-ai/build/battle-corpus/local-damage-o3-01 \
  --moves HYPER_VOICE AEROBLAST --sides 0 1 --seeds 271 --max-turns 256
```

Synthetic positive target-knockout and negative user-only damage, no damage,
miss, participant replacement, nonneutral context and missing source/read-only
identity checks are prepared but have not run. All existing terminal, Oracle
comparison, replay and collector trace-equality gates remain mandatory.

## Executed local damage checkpoint

The fresh Hyper Voice/Aeroblast batch passed on both sides with seed 271: four
positive terminal fights and exact native replay, with 4,385 independent Oracle
comparisons. The narrow local-damage admission observed first-action damage
before faint resolution without requiring the later opposing Splash. Existing
source, identity, read-only and neutral-fixture gates remained active. Synthetic
observer controls passed. Raw output is retained under
`build/battle-corpus/local-damage-o3-01`; the earlier 113-case report and its two
blocked outcomes remain unchanged.

## Executed bounded rare-secondary seed selection

A disposable first-turn experiment used the existing actual-Oracle controller,
recorded segments, action-local observations and semantic observer to search
side-0 seeds 1 through 32, stopping at each target's first positive transition.
All five targets were found in 32 total probes: Poison Sting seed 2, Scald seed
1, Spark seed 2, Metal Claw seed 13 and AncientPower seed 14. The prefixes took
4.925 seconds and contained 5,982 source/Oracle comparisons. Prefix records have
empty accepted-rule claims; they are fixture selection, never coverage or
held-out evaluation.

The unmodified full-fight runner then executed both sides at each selected seed.
All ten fights reached source terminal and exact native replay. The five side-0
cases activated; all five opposite-side cases remain unsatisfied. The positive
full fights contain 7,190 independent Oracle comparisons. Existing collectors
retained each target separately, then the existing merger combined them with
the 168-key ledger into `build/battle-corpus/combined-through-rare-secondary-o3`.
That ledger has 178 tested keys, 675 missing required-rule failures and no
invalid evidence. New keys cover these five move identities and their five
secondary effects, including all five AncientPower stage increases.

Exact search provenance, seed outcomes, prefix observations, full-run commands,
frozen fifteen host modules and measured timings live under
`subagents/m2-rare-secondary-01/artifacts`. Full runner commands took 25.807
seconds combined; collection took 1.835 seconds and merge 2.929 seconds. Runtime
sources and native artifacts were unchanged. These deliberately selected
positive fixtures do not measure training throughput or battle strength.

## Executed opposite-side rare-secondary checkpoint

A separate bounded side-1 prefix search found actual positive transitions for
Poison Sting seed 1, Scald seed 3, Spark seed 1, Metal Claw seed 12 and
AncientPower seed 16. It used 33 first-turn probes, 6,177 Oracle comparisons and
5.868 seconds; the same per-move 1-through-32 cap was retained. Prefixes remained
fixture-selection diagnostics with no coverage claims.

The unmodified runner then executed only side 1 at those five selected seeds.
All five full fights activated, reached source terminal and reproduced exact
native replay, with 7,183 independent Oracle comparisons. Separate retained
collections were merged with the previous ledger into
`build/battle-corpus/combined-through-rare-secondary-both-sides-o3`. Its 178 keys
and 675 remaining missing-rule failures are unchanged, with no invalid evidence;
the additional evidence closes these five orientation gaps. Earlier opposite-
side nonactivations remain in their original reports.

Full runners took 16.887 seconds, collection 2.015 seconds and merge 4.023 seconds.
Exact source snapshots, prefix outcomes, commands and provenance are retained
under `subagents/m2-rare-secondary-side1-01/artifacts`. Selected positive fixtures
are not held-out evaluation or model-strength evidence. Runtime sources and
native artifacts were unchanged.

## Prepared local drain and recoil families

The source-local observer is prepared for `EFFECT_LEECH_HIT` and
`EFFECT_RECOIL_HIT`. Both require actual target damage; drain additionally needs
actor HP gain, and recoil actor HP loss. Both actors' observed HP must stay
within unchanged source MaxHP bounds. The existing source-call identity,
read-only observation, exact trace equality and neutral Horsea/Swift Swim/no-item
fixture remain unchanged. Full-HP drain without healing is unsatisfied; ordinary
damage without the actor transition cannot establish either effect.

These checks observe direction and bounds. Exact source ratios, rounding,
minimum recoil, target-damage clamping and healing saturation remain the
independent full CPU/memory Oracle's responsibility. No host damage formula is
introduced. Source `LeechHit` drains after applying damage; `RecoilHit` applies
recoil before post-faint effects. The source skips ordinary recoil under Rock
Head or Magic Guard, which the neutral fixture excludes. Struggle has a distinct
quarter-MaxHP source branch and needs an exhausted-PP forced action. The frozen
named probe resolver cannot select that typed action, so its case now records
known Tackle with zero PP and remains explicitly unsupported rather than giving
Struggle as a learned move. Composite Flare Blitz and sleeping-target Dream Eater
are outside this preparation.

Drain recipes now start at 85 HP so normal level-50 Horsea fixtures can reach
the actual source MaxHP cap. The observer reads that cap from source observations;
it does not hardcode the maximum. Family acceptance additionally requires at
least one actual positive drain witness ending at observed MaxHP. All numerical
correctness still requires complete differential execution and exact replay.

After source review and CPU allocation, first run synthetic observer checks and
one canary:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B tools/trainer_ai/battle_corpus_run.py \
  --artifact-root /Volumes/Storage/tmp/polished-ai \
  --inventory /Volumes/Storage/tmp/polished-ai/build/battle-coverage/inventory.json \
  --native-build /Volumes/Storage/tmp/polished-ai/build/battle-controller/native-o3 \
  --optimization O3 \
  --output /Volumes/Storage/tmp/polished-ai/build/battle-corpus/drain-recoil-canary-o3-01 \
  --moves GIGA_DRAIN --sides 0 --seeds 193 --max-turns 256
```

If the canary passes, the bounded family command requests 22 cases, of which
Struggle's two forced-action fixture gaps stay visibly blocked:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B tools/trainer_ai/battle_corpus_run.py \
  --artifact-root /Volumes/Storage/tmp/polished-ai \
  --inventory /Volumes/Storage/tmp/polished-ai/build/battle-coverage/inventory.json \
  --native-build /Volumes/Storage/tmp/polished-ai/build/battle-controller/native-o3 \
  --optimization O3 \
  --output /Volumes/Storage/tmp/polished-ai/build/battle-corpus/drain-recoil-o3-01 \
  --moves ABSORB MEGA_DRAIN LEECH_LIFE DRAINING_KISS DRAIN_PUNCH GIGA_DRAIN \
          TAKE_DOWN DOUBLE_EDGE BRAVE_BIRD WILD_CHARGE STRUGGLE \
  --sides 0 1 --seeds 193 --max-turns 256
```

The prepared synthetic controls and commands have not run. Existing raw and
retained evidence stays unchanged. New full-fight outcomes must be retained
with matching host sources before any claims or merged ledger updates.

## Executed drain and recoil checkpoint

Synthetic observer checks and the Giga Drain canary passed. The canary observed
actor HP 85 to source MaxHP 90 and target HP 90 to zero, then reached source
terminal after nine rounds with exact native replay. The family batch retained
20 accepted full fights covering six drain moves and four ordinary recoil moves
on both sides with seed 193. All reached source terminal and exact replay,
with 32,316 independent Oracle comparisons. Struggle's two unsupported forced-
action cases remain blocked and contribute no claims.

All twelve drain family witnesses ended at observed MaxHP after positive healing,
so the required actual healing-cap check passed. Recoil witnesses required both
target damage and actor damage. Numeric source formulas, rounding, cap and
clamping behavior were compared by the independent CPU/memory Oracle. No host
formula replaced that comparison. Two disposable cases consistently removed
drain healing or recoil target damage from both frame and matching trace;
semantic recomputation rejected both with exit 2 and no ledger.

Exact raw, source snapshots and retained provenance live in
`build/battle-corpus/drain-recoil-o3-01`, `drain-recoil-retained-o3-01` and
`subagents/m2-drain-recoil-01/artifacts`. The existing merger combined the new
twelve keys with the latest weather-inclusive 198-key ledger into
`build/battle-corpus/combined-through-drain-recoil-o3`: 210 tested keys, 643
missing required-rule failures and no invalid evidence. Historical artifacts
were preserved unchanged. M2 remains incomplete.

## Prepared real forced-Struggle follow-up

The named resolver is prepared to accept requested Struggle only when the actual
source legal mask contains exactly one typed `StruggleAction`. It never maps a
learned Struggle move slot to a normal `MoveAction`. The fixture continues to
start with known Tackle at zero PP; only its previous resolver-support blocker
is removed. All move identity, target damage, recoil, source/Oracle comparison
and exact replay gates remain unchanged. Old blocked reports are preserved.

The prepared cheap resolver controls reject a request with no legal forced
Struggle, duplicate forced actions or an invented learned Struggle slot. Fresh
both-side seed-193 fights must additionally retain initial own Tackle/zero-PP
observations and actual source Struggle execution. Source quarter-MaxHP recoil
correctness remains the independent Oracle's responsibility. This follow-up
has not been executed or added to coverage.

## Executed real forced-Struggle checkpoint

The typed resolver controls passed, including requests without legal forced
Struggle and invented learned-move fallback rejection. Two fresh source fights,
one per side with seed 193, then activated actual Struggle from initial known
Tackle with zero PP. Both records contain typed `StruggleAction`, source move ID
255, actor HP 90 to 68 at source MaxHP 90, and target HP 90 to 77 or 76. The
14-round and 13-round fights reached source terminal and exact native replay,
with 3,124 independent Oracle comparisons. Source quarter-MaxHP recoil was
verified by the full comparison; no host mechanic was introduced.

A separate actual-Oracle reset with legitimate Tackle at one PP produced a
source legal mask without Struggle. The named request rejected before any fight
step, retaining 113 source comparisons and the actual mask. Fresh source and
retained evidence live under `build/battle-corpus/forced-struggle-o3-01` and
`forced-struggle-retained-o3-01`; the original 20-positive/two-blocked reports
remain unchanged. The existing merger added only the new Struggle move key to
the latest 227-key ledger, producing `combined-through-forced-struggle-o3` with
228 tested keys, 625 missing required-rule failures and no invalid evidence.
