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
