# Bounded controller source updates

This helper extends the source-backed lifting experiment without changing the
default frozen controller audit. It is not a general approval mechanism for a
new game version. Three reviewed deltas are admitted separately:

| Mutation | Actual Assembly edit | Required observable check |
| --- | --- | --- |
| `move-power` | Tackle power 40 to 55 | Source-loaded power 40 to 55 and updated-ROM full-fight parity |
| `burn-toxic-rule` | Conditional burn/Toxic call from sixteenth to eighth max HP | A burned active battler loses different HP at end of turn in a complete fight |
| `hram-layout` | Swap adjacent `hBattleTurn` and `hBattlePalFadeMode` declarations | Symbol addresses move and both sides still complete a differential full fight |

The caller copies the frozen incremental game cache into one private directory
per mutation. `build_update` checks the baseline with the unchanged action audit,
requires all tracked source inputs and inherited game objects to match, applies
one exact edit, deletes its affected object and old ROM, and invokes actual
`make -j1`. It retains the build log and a receipt containing actual assembler
and linker invocations, the complete source inventory and raw ROM/SYM hashes.
No Assembly or compilation happens on import.

`BoundedUpdateAudit.verify` checks the original baseline again, permits only the
exact source delta, checks the retained build log and artifact bytes, rejects
build defines, and verifies expected HRAM relocation where applicable. Its
`configuration()` record must be retained by the controller profile/report.
Pass this explicit audit object into profile build and machine construction.
The default audit remains unchanged. Never patch the previous audit function or
replace module globals.

After an update, the native closure must be generated from the new assembled
bytes. The standard verified loader continues to bind the native library to
the current ROM, symbols, generator and every tracked game input. The old
library must reject the changed ROM/source before a new build is used. A new
source inventory invalidates old coverage evidence. A build receipt establishes
trusted runner provenance, not gameplay correctness or coverage by itself.

Actual validation completed serially with the O0 debug native configuration:

| Mutation | Direct source result | Complete fight | Exact fight comparisons |
| --- | --- | ---: | ---: |
| `move-power` | `UpdateMoveData` reads power 40 before and 55 after | 7 turns | 817 |
| `burn-toxic-rule` | At 110 max HP the real full endturn loses 6 before and 13 after | 7 turns | 812 |
| `hram-layout` | `hBattleTurn` moves from FFCF to FFD0 and source still reads power 40 | 8 turns | 917 |

Every complete fight reached an actual source terminal and replayed its retained
logical random events natively with identical states and outcomes. The data and
layout fights open with a voluntary switch and trainer healing item. The burn
fight retains an actively burned Pokemon and uses the trainer item. Fixed level
and permanent-stat assertions remain active; XP is disabled through the game's
ordinary option. The six direct before/after probes add 686 exact comparisons,
separate from the 2,546 complete-fight comparisons.

All three builds reject the previous native artifact and reject runtime audit
configuration mismatch. The old frozen audit already admits the narrow Tackle
power parameter, so that source edit is deliberately accepted by that older
audit; executable burn and HRAM layout changes are rejected by it. This does
not permit loading stale native bytes for any mutation.

Six actual-input rejection guards pass for each mutation, with every input
restored and reverified: unsupported ROM instruction byte, unreviewed source
delta, changed presentation instruction, source/ROM mismatch, invalid receipt
build defines, and altered retained build log. The frozen baseline is unchanged.
Actual `make -j1` logs, assembler/linker commands, source hashes, ROM/SYM bytes,
new native artifacts, reports and recorded fight evidence are retained under
`<run>/build/battle-controller/source-updates/`. The aggregate report is
`report.json`; `battle_controller_mutations.py` is the serial validation driver.

These are three reviewed update experiments. They demonstrate regeneration for
changed data, an executable rule and relocated state addresses. They do not
approve arbitrary future updates, prove every branch of Toxic, replace broad
mechanics coverage, or prove the projected presentation equivalent to an
unhooked physical-input frontend. No training-throughput claim follows from
these O0 validation runs.
