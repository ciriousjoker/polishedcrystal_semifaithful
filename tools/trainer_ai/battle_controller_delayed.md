# Delayed and volatile actual-source fights

Status: all 16 listed fights passed on 2026-10-15. Importing or running this driver requires the
coordinator's exclusive CPU grant. No engine, profile, manifest, observation,
scenario or shared runner changes belong to this family.

`battle_controller_delayed.py` covers eight cases in both setter orientations,
seed 193, for exactly 16 complete fights. Each case casts once, submits Splash
through the mechanic's bounded observation interval, then explicitly selects
Water Gun until an actual source terminal. Pokemon and moves are supplied engine
fixtures, with no natural learnset claim. Fixtures have no held items, happiness
zero, fixed levels and the actual no-experience option. Horsea has Swift Swim;
the Ghost branch uses Gastly with Levitate and Water Gun to avoid Normal immunity;
the Grass control uses Oddish with Chlorophyll. Future Sight uses level-100
Horsea targets, including its reserve, to keep the delayed hit observable before
fainting. Other participants use level 50.

| Case | Isolated witness | Probe rounds |
| --- | --- | --- |
| `leech` | Seeded target loses one eighth maximum HP each round, low-HP caster gains half the drained amount | 3 |
| `leech-switch` | Positive first drain, target switches, seed flag clears, no further drain | 3 |
| `leech-grass` | Observed Grass type, no seed flag, no drain or healing | 3 |
| `curse-ghost` | Ghost caster pays half maximum HP, cursed target loses one quarter each round | 3 |
| `curse-other` | Attack and Defense rise, Speed falls, no target curse or HP damage | 3 |
| `perish` | Both counters show 3, 2, 1, both active leads faint, both explicit reserves replace them, clear round | 5 |
| `future-switch` | Packed caster counter shows 0x12, 0x11, 0; switched reserve takes positive damage on tick three only | 4 |
| `wrap` | Initial source hit and count 3 or 4, repeated residual, expiry, source switch-mask recovery and clear round | 6 |

The actual sources inspected are `engine/battle/move_effects/leech_seed.asm`,
`curse.asm`, `perish_song.asm`, `future_sight.asm`, the trap-target command in
`engine/battle/effect_commands.asm`, `engine/battle/endturn.asm`, switch status
reset and `UserCanSwitch` in `engine/battle/core.asm`, and fractional HP helpers
in `home/battle.asm`. Leech Seed checks Grass before setting the volatile.
Curse branches on the caster's type. Perish Song initializes four and decrements
at the casting round's end. Future Sight stores the caster's party index in the
high nibble and initializes three low-nibble ticks. Wrap samples four or five
without an item, then decrements before applying residual; a zero low count
releases without damage. The driver observes the sampled duration rather than
requiring a fixed RNG outcome. Move records retain compiled effects and accuracy.
An accuracy failure or nonactivation fails closed. There is no chance override,
post-reset RAM patch, numerical battle model or future-RNG feature.

The runner uses existing ControllerFight, Oracle and RecordedRun interfaces.
Each probe retains both observations and legal masks, submitted typed actions,
PP use, active identity, HP, statuses, item and fixed move checks. Perish Song
requires both explicit replacement choices. Every executed source segment is
compared independently against mGBA for CPU, all eight WRAM banks, HRAM and SVBK,
then replayed natively using exactly its captured events. The replay compares
normalized machine state per segment and all fight choices/events/fixed-party
contracts. Only unused event transport capacity is excluded, consistently with
RecordedRun's existing digest. Raw transport-sensitive hashes remain diagnostic.

Evidence includes per-case initial all-bank memory, its hash, normalized initial
state hash, initial boundary/options/fixed-party contract, complete fight record,
Oracle comparisons, native replay result and controller audit. Hash-verified
snapshots retain all loaded project Python dependencies, native generated C and
library, Oracle C/library, actual loaded mGBA, State/Hook sources, ROM, symbols,
manifest and inspected game sources. If present, the original assembler receipt
must match the ROM and symbols. Source changes invalidate the batch. The driver
never promotes central coverage or claims full M2 completion. Held items,
interaction abilities, switching the Future Sight caster and other trap moves
remain outside this bounded family.

After static coordinator review, the proposed first discriminating run is three
fights, with no concurrent heavy process:

```sh
python3 tools/trainer_ai/battle_controller_delayed.py \
  --artifact-root /Volumes/Storage/tmp/polished-ai \
  --output /Volumes/Storage/tmp/polished-ai/subagents/m2-delayed-residual-01/artifacts/first \
  --cases leech perish wrap --sides 0 --seeds 193
```

If that run supplies trustworthy evidence, the full bounded invocation is:

```sh
python3 tools/trainer_ai/battle_controller_delayed.py \
  --artifact-root /Volumes/Storage/tmp/polished-ai \
  --output /Volumes/Storage/tmp/polished-ai/subagents/m2-delayed-residual-01/artifacts/full \
  --seeds 193
```

Use the existing verified native build or pass `--native-build` explicitly. Any
new controller, profile or hardware fault is a concrete blocker to report with
its bank/PC/boundary. Widening presentation boundaries or editing the engine to
make this family pass is outside this driver's authorization.

For collection compatibility, every case also retains a standalone 8192-byte
`initial-wram.bin`, path, SHA and post-reset frozen-decision boundary, plus
`host_sources` using explicit project dependency basenames and `game_sources`
using relative game paths. The same maps and Oracle provenance are in the batch
report. The dependency list includes the transitive legacy action/party/status
imports, without scanning the directory or retaining standard-library modules.

The first runtime probe exposed an actual-source distinction: Leech Seed drains
one eighth maximum HP, then `GetHPAbsorption` in effect_commands.asm calls
`HalveBC`. Without Big Root, the caster therefore heals half the drain, rounded
down with a minimum of one. The initial fixture observed target HP 90 to 79 to 68
to 57 and caster HP 10 to 15 to 20 to 25. The witness follows this source behavior;
no engine code was changed. Failed first and diagnostic outputs are retained.

Accepted evidence lives under
`/Volumes/Storage/tmp/polished-ai/subagents/m2-delayed-residual-01/artifacts`:
`first-corrected/report.json` contains the initial three side-0 cases,
`remaining-side0/report.json` contains the other five side-0 cases, and
`remaining-side1/report.json` contains all eight side-1 cases. These three serial
invocations exited zero and share identical executed host and game identities.
The resulting 16 unique fights total 89 turns and 7,628 independent segment or
instruction comparisons, followed by exact native event/state replay. The
lightweight `audit.py` and `audit.json` verify unique case keys, evidence hashes,
all 150 retained input snapshots, current source bytes, standalone 8192-byte
WRAM hashes, full-memory hashes, fixed-party contracts and comparison counts.
No controller fault or presentation-boundary change was required.
