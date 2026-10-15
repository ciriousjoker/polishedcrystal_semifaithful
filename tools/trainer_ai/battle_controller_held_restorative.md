# Actual held restorative family

All 22 distinct fights passed: 100 source turns, 12,219 exact Oracle
comparisons, and identical native whole-fight event replays. The executed family
covers Oran, Sitrus, Leftovers, Cheri, Chesto, Pecha, Rawst,
Aspear, Lum and Leppa, plus one full-HP Leftovers no-trigger control, on both
sides with seed 193. The driver rejects batches larger than 24 fights.

The actual reserve starts with its held item and a legitimate low-HP, status or
zero-PP scenario condition. A healthy lead occupies the first slot during
reset. At the first frozen decision the holder switches to the reserve while
its opponent uses Splash. The first post-switch frozen decision supplies the
after-state. This avoids assuming that an active item's effect waits until the
first round: source entry and automatic preparation are allowed to execute
normally. The reserve must still have the exact item and status before the
switch. Missing activation is blocked, never counted as a passed case. No RAM
is patched after scenario initialization to make an effect happen.

Horsea uses ability slot zero, its source Swift Swim ability. The witness checks
the actual ability before and after entry. Splash is an explicit scripted
non-damaging engine probe, not a claim about Horsea's natural learnset. Both
sides subsequently select the legal attacking move Water Gun through the typed
action mask until the real ordinary trainer terminal. Existing controller
checks retain fixed levels, no experience, detached actor observations and
secrecy of current opposing actions and future random events. Full rosters are
part of the established observation contract.

The witness requires exact positive HP, status or PP transitions and actual
consumption for each berry. Oran uses the assembled item parameter. Sitrus uses
a quarter of source MaxHP, Leftovers a sixteenth, with the source minimum of
one. Status fixtures cover paralysis, sleep, toxic poison, burn and freeze;
Lum's probe covers toxic poison, not confusion. Leppa starts with zero Splash
PP and full Water Gun PP. It must restore only Splash by ten, capped by the
assembled move's base PP. The switch itself uses no move PP. Leftovers must
remain held after both the positive healing probe and the full-HP no-trigger
control. The active cache and party record must agree on item, HP, status and
PP at the observed boundary. Other party members must retain item, HP and
status through the isolated probe.

The source runs poison and burn residuals before its general endturn healing
item sweep. The toxic and burn holders therefore must lose exactly one
sixteenth of MaxHP, with minimum one, before their berry cures them. This is an
explicit source-order expectation, not incoming move damage or a tolerated
arbitrary HP change. Other status probes preserve HP.

Item identifiers are read from source constants. The existing item decoder
checks the source attribute macro before the driver reads the assembled held
effect and parameter. Those bytes must match the selected source effect and
expected parameter. Sleep's literal source mask is checked explicitly. Relevant
item constants, item data, core, endturn, consumption, HP helper and Horsea
ability source files are hashed alongside the host drivers. Existing native
manifest guards remain authoritative: a source update must be rebuilt and
reviewed normally, and an unknown source revision must be rejected by those
guards. This preparation adds no adapter, manifest exception or automatic
coverage approval.

Every actual source segment must pass the existing independent Oracle's CPU,
all WRAM banks, HRAM and SVBK comparison. The runner requires banked comparison
mode and retains oracle provenance. RecordedRun captures actual per-segment
events and complete normalized state digests, then replays the whole fight
natively and requires identical activation witness, segment states, terminal,
turn count and winner. Source inputs are hashed before and after execution.
The retained initial WRAM file contains the mapped C000-DFFF window; all-bank
comparison and replay evidence come from the segment records, not from that
single diagnostic file.

A complete 22-fight invocation, requiring the exclusive CPU slot and a current
source-matched artifact, is:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/battle_controller_held_restorative.py --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/held-restorative --seeds 193
```

The actual execution preserved the first five side-zero cases, then ran only
the six remaining side-zero cases and all eleven side-one cases. There were no
repeated accepted cases. Reports and totals under `<run>/build/battle-controller`:

| Report | Fights | Turns | Comparisons |
| --- | ---: | ---: | ---: |
| `held-restorative-first/report.json` | 5 | 24 | 2,963 |
| `held-restorative-player-rest/report.json` | 6 | 29 | 3,626 |
| `held-restorative-enemy/report.json` | 11 | 47 | 5,630 |

The source-order toxic and burn witnesses were confirmed on both sides: HP
changed from 90 to 85 before status cleared. Every berry was consumed;
Leftovers remained held in both its positive and full-HP control cases.

Each successful batch retained `report.json`, per-case JSON with source,
ROM, SYM and native manifest identities, oracle provenance, recorded segments,
comparison rows, controller audit and the concrete witness, plus initial WRAM
and its hash. A failed future batch writes `failure.json` with `passed: false` and
`status: blocked`; any per-case JSON written before replay is provisional until
the aggregate report confirms replay success. Preserve the process exit status
and log with those artifacts. The three listed reports are executed proof pointers; the complete invocation
above remains a convenient rerun command, not an additional executed batch.

Flame Orb, Toxic Orb, Lum confusion, status application timing, Unnerve, item
theft, other restorative items and combinations remain outside this batch.
This family does not update the central coverage ledger or establish M2
completion. If a source update changes thresholds, ordering, consumption or
cache synchronization, review the family witness and rerun affected actual
cases rather than mechanically accepting an item name or held-effect enum.
