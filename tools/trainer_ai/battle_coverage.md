# Source-derived trainer battle coverage

`battle_coverage.py` inventories the default non-FAITHFUL game's actual move IDs,
move effect pointer scripts, abilities, held-effect families, trainer inventory
handlers, status/volatile flags, field masks and weather, switch flags, actual
endturn calls and meaningful initialization/turn/experience/learning controllers.
Every row starts `unproven`. Presence in assembly, a translated opcode or a
reachable handler is never enough to mark a rule supported.

The inventory currently has 1,110 rows: 255 moves, 152 effect scripts, 160 ability
IDs (including NO_ABILITY), 77 nonzero held-effect families, 14 trainer items,
six statuses, 30 volatile flags, 11 field rules, eight switch flags, 23 endturn
calls, 19 controllers, 85 player bag items, 16 player bag effect families and 254 informational item rows. Numeric IDs come from
constant order and table positions; source file and original line accompany each
row. Unknown conditional forms, table/constant count mismatch and duplicate rule
keys reject rather than silently guessing. FAITHFUL and VIRTUAL_CONSOLE are
explicitly unset. This is a source inventory, not a general RGBDS parser.

Generate the ledger and visible missing-rule list:

```sh
python3 -B tools/trainer_ai/battle_coverage.py --output /outside/repo/coverage
```

Exit status 1 means the M2 acceptance gate fails, as expected with the empty
ledger. `inventory.json`, `acceptance.json`, `COVERAGE.md` and
`ledger-template.json` are written before exiting. No game build, simulation or
historical test rerun is performed by this command.

An evidence ledger uses the generated `inventory_sha256`, `claims` keyed by
exact inventory rule IDs, and `evidence` keyed by stable report names. Claims
contain a `phase` and a list of evidence names. Phases are `unproven`,
`present-executable`, `tested-routine`, and `tested-full-fight`. The latter is
required for M2's ordinary trainer rules; routine tests remain visible without
being promoted to complete-fight coverage. Raw item rows are informational:
required held-effect families and trainer item handlers are tracked separately.

Each evidence entry contains a relative `path` and SHA256 of a retained JSON
adapter report. That report must contain:

- The exact current `inventory_sha256`.
- `rom_sha256`, `generator_sha256` and `manifest_sha256`.
- Nonzero `full_state_comparisons`, and `complete_fights` for full-fight claims.
- Zero `failed_cases` and `unsupported_cases`.
- `rule_keys` naming rules actually exercised by the retained cases.
- `artifacts`, each with a root-relative path and SHA256, retaining original
  execution reports and the actual stateful manifest.

The gate hashes all retained artifacts. It parses the matching stateful manifest
and compares its ROM and generator identities and every inventory source hash.
Adapters cannot update only a textual source label to reuse stale evidence.
Paths outside the evidence root, unknown rule IDs, missing comparisons, failures,
unsupported cases and missing complete-fight evidence fail acceptance.

```sh
python3 -B tools/trainer_ai/battle_coverage.py \
  --output /outside/repo/coverage \
  --ledger /outside/repo/ledger.json \
  --evidence-root /outside/repo
python3 -B tools/trainer_ai/battle_coverage_check.py
```

The ledger is an explicit evidence registry, not automatic execution tracing or
a proof that an adapter's declared rule keys were semantically exercised. A
reviewer must bind each promoted rule to actual case inputs, trace/handler
observations and outcomes in the retained original report. Passing every rule
still does not establish every pairwise interaction, exact frontend register
projection, or hardware DIV independence. Those remain separate acceptance
requirements. Existing narrow experiments are not silently imported or relabeled
as source-complete coverage.

Ordinary trainer battles are the required scope. Wild, Safari, link, Tower
progression and rewards UI are explicit mode exclusions. The source's commented
Wish, Aqua Ring, Ingrain, Nightmare, Taunt and terrain are listed as absent rules,
not missing implementations in this simulator. The user explicitly requires
fixed levels and base stats without experience simulation. Experience awards,
level/stat progression and progression move learning are informational exclusions
with a visible reason. Battle initialization/stat loading remains required, as do
combat stat stages, ability modifiers, Transform and other temporary battle effects.

Evidence must explicitly name its acceptance scope and game configuration.
`conditional-native-gameplay` with `profile_kind: headless-iid-div` records the
training environment's conditional rule semantics. It does not approve ordinary
frontend projection. The latter scope requires `profile_kind` and `oracle_kind`
of `ordinary-frontend-projection` and `real-frontend-gameplay-projection`
respectively. `configuration` also records `mode: ordinary-trainer`,
`ruleset: default-non-faithful`, both actor `roles`, and explicit game `options`
such as abilities, experience and battle style. Configuration equality is required
across the ledger, adapter report and original case report.

An adapter report's `case_reports` list names retained original JSON files. Each
contains the exact `manifest_sha256`, `configuration` and a nonempty `cases` list.
A case records its stable `case_id`, `passed: true`, `unsupported: false`, nonzero
`full_state_comparisons`, `complete_fight` boolean and actually exercised
`rule_keys`. Every declared rule must occur in an original passed case, and every
full-fight claim must occur in an original complete-fight case. Older report
formats without per-rule case records require a new execution collector or a
reviewed transformation retaining trace evidence. Merely writing adapter totals
and declaring coverage cannot pass this gate.

Player bag actions are required independently of enemy trainer items. Every
regular item whose source battle menu differs from `ITEMMENU_NOUSE` receives a
required `player-bag-item` row, with its actual item ID, effect table entry and
battle-menu action. Its effect family receives a required `player-bag-effect`
row. `_DoItemEffect` is also a required controller. This includes correct
ordinary-trainer rejection/no-effect behavior for otherwise selectable wild-only
items; a ball's presence does not mean capturing another trainer's Pokemon is
legal. Party targets, PP move targets, revival, status clearing, unsuccessful use
and inventory consumption remain source-driven semantics to exercise.

## Execution configuration and retained build bytes

The user-selected fixed-level ordinary M2 configuration requires boolean
`abilities: true`, `experience: false`, `fixed_levels: true` and `player_bag: true`,
plus `battle_style` in `set`, `switch`, `predict`. Both actor roles must be `policy`, `scripted` or `source-ai`.
Missing keys, truthy integers/strings, null or unknown roles, enabled experience,
disabled fixed levels, abilities or bag support cannot establish this scope. Restricted profiles can retain
an unproven ledger, but cannot waive required ordinary-game rows.

A report's `execution_artifacts` map names retained `rom`, `symbols`, `generator`
and `build_receipt` files. Their actual bytes are hashed against the stateful
manifest and report. The manifest must have schema 1, one supported flat or CGB
banked execution profile with matching LCD capability, and nonempty source entries.
The optional capability sections below define the exact supported profile names.
The build receipt records schema 1, exact `inventory_sha256`, `rom_sha256`,
`symbols_sha256`, `.gbc` target, `effective_defines: []` and nonempty
`assembler_invocations` argv lists. Every invocation must identify `rgbasm`
without a `-D` flag. FAITHFUL, VIRTUAL_CONSOLE and MONOCHROME targets/defines are
unsupported; these symbol names, if exported, also reject. Source hashes alone
cannot distinguish faithful/default builds. A trusted build runner must capture
these actual invocations and output identities rather than reconstructing an
assertion from a desired ruleset label.

Every original case names a retained `initial_wram` snapshot containing exactly
C000-DFFF. The gate resolves `wInitialOptions`, `wInitialOptions2` and `wOptions2`
from retained SYM bytes, and derives ability, no-experience and Set/Switch/Predict
bits from the current source constants. These actual initial bytes must agree
with declared options. A case's executed `choice_contract` additionally records
`roles` and `player_bag` support, which are host callback capabilities rather than
RAM options. Case inputs and callback behavior still require semantic review.
This binds a consistent trusted collector chain; it is not a cryptographic proof
that a third-party JSON document represents a real execution.

The actor observation contract excludes experience and progression metadata. The
source's actual `NO_EXP_OPT` must be set in every retained starting WRAM snapshot;
merely declaring `experience: false` does not pass. This is a deliberate training
ruleset selected by the user, rather than a claim of experience-enabled gameplay
fidelity. Level/base-stat initialization from the team is still checked.

## Optional CGB banked WRAM capability

The legacy `bounded-mbc3-flat-wram-div-event-tape` manifest profile remains valid
with `banked_wram` absent or explicitly false, including legacy configurations
whose options omit that flag. The optional
`bounded-mbc3-cgb-banked-wram-div-event-tape` profile instead requires explicit
boolean `banked_wram: true` in both the manifest and configuration options.
Truthy integers and profile/capability mismatches reject.

For banked execution every original case must explicitly include
`full_state_components: ["cpu", "hram", "wram-banks-0-7", "svbk"]`. Full-state
oracle comparisons must include the complete CGB WRAM backing, every physical
bank and raw SVBK selection, including bank-zero aliasing to bank one, rather
than only the currently mapped C000-DFFF window. `initial_wram` remains the
canonical 8 KiB bank-zero/bank-one snapshot used to decode runtime options; it is
not a substitute for comparing all banked state. The gate checks consistent
capability/evidence metadata; actual native and oracle bank semantics require
separate executed proof. This capability does not promote any combat mechanism
or change the fixed-level/no-experience contract.


## Optional disabled LCD capability

The generator's actual banked profile contains `cgb-banked-wram`. The previously
listed `bounded-mbc3-banked-wram-div-event-tape` spelling was not emitted by the
generator and is rejected. This corrects a gate integration defect; earlier
synthetic fixture acceptance did not establish actual generator compatibility.

Both flat and CGB banked profiles may additionally contain `-lcd-disabled` before
`-div-event-tape`. Such a profile requires explicit Boolean `lcd_disabled: true`
in the manifest and configuration. Without that suffix the capability is false;
legacy absence remains false. Truthy integers, omitted true capabilities and
profile mismatches reject. The capability models only a constant-zero FF40 read
with writes still rejected. Its CPU/MMIO semantics and the ordinary frontend
projection require separate actual-game evidence. It cannot promote mechanics
coverage from configuration metadata alone.
