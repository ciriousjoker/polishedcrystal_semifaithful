# Actual-source trainer inventory full fights

`battle_controller_trainer_items.py` prepares fourteen positive enemy trainer
inventory cases, each selectable in inventory slot 0 or 1. Inventory slots are
not battle sides. The source exposes these handlers only for the enemy trainer,
so the driver makes no player-side symmetry claim. The existing controller,
profile, source ROM and independent Oracle remain unchanged.

Status: CODEPREP, unexecuted. Importing or running this driver requires the
coordinator's exclusive CPU grant. It reuses the existing O3 manifest
`dec3cb17a34aeeb9e0f36e44590d0890b7d0816ddff98fc7bb0346b9b32737b0`.
It refuses a changed prepared input ledger or absent Oracle library instead of
building a replacement. No central coverage promotion is performed.

## Positive witnesses

Both teams contain one level 50 Horsea with Swift Swim, Splash and Water Gun,
neutral weather, no held item, and the real NO_EXP option. The enemy submits a
typed `ItemAction(slot)` while the player submits Splash. Both actors then use
legal attacks until the actual source terminal. The unselected inventory slot
contains Dire Hit, including in the Dire Hit case, so consumption must preserve
the correct other slot even when the two item identifiers are equal.

| Source handler family | Initial condition | Required effect after the first round |
| --- | --- | --- |
| Potion, Super Potion, Hyper Potion | Enemy HP 1 | Restore 20, 50, 200, capped at source MaxHP |
| Max Potion | Enemy HP 1 | Restore to source MaxHP |
| Full Heal | Enemy paralysis | Clear actual status |
| Full Restore | Enemy HP 1 and paralysis | Restore MaxHP and clear actual status |
| Guard Spec | No Mist | High nibble starts from source duration 5, observed as 4 after the first end turn; decrement through zero and observe one clear round |
| Dire Hit | No Focus Energy | Set the actual Focus Energy bit |
| Six X items | Neutral stages | Raise the stat named by the source item parameter, preserving all other stages and the opposing side |

The trainer healing handlers overwrite the wrapper's proposed amount. In this
source, Super Potion and Hyper Potion restore **50 and 200**, although their
player-bag attributes and AI desirability wrappers use 60 and 120. The driver
parses the actual enemy handler literals and binds their assembled `ld a` and
`ld b` bytes. Horsea's legitimate level 50 MaxHP caps Hyper Potion's positive
restoration. This is not an uncapped 200-HP observation.

The X item source parameter is `$10 | STAT`. `DoChangeStat` adds one to the
high-nibble value, so these items raise **two stages**. Each parameter is parsed
from the correct source table row and checked against the assembled item record.
The decoder scaffold, Guard Spec immediate and Dire Hit bit are also checked.
The neutral Horsea ability is checked in its compiled base-data record.

Every probe checks HP, status, stages, Mist, Focus Energy, exact selected-slot
consumption, unselected-slot preservation, opposing Splash PP use and no enemy
move PP on the item turn. It compares the actual party and active caches for
both sides. Existing `ControllerFight` checks enforce fixed party level, XP and
base stats throughout the complete fight. A refused or unsupported handler fails
closed. No mechanics or RNG writes occur after reset.

## Evidence and boundaries

The explicit transitive host dependency allowlist uses basename-to-SHA256
records. Game source records use relative paths. Inputs are frozen before
machine initialization, retained before fights, and checked again after the
batch. Actual loaded Oracle C, dylib, mGBA, State and Hook identities, native
library, generated C, ROM, symbols, manifest, assembler receipt and original
build log are retained. Large retained files use a content-addressed sibling
`trainer-item-inputs` directory shared by the staged batches. Each complete case
and report explicitly records its absolute `shared_snapshot_root`, allowing a
later collector to bind retained inputs to that root.

Each case preserves its initial standalone 8192-byte WRAM file, all eight WRAM
banks plus HRAM and SVBK, file hashes, and the exact first frozen actor decision
boundary. Full independent CPU, memory and actually consumed DIV comparisons
cover source execution. `RecordedRun` then replays the same consumed event tape
natively and checks normalized CPU/event/memory equality at every segment,
initial-memory equality, witness equality and full fight event/action equality.
Only unused event-tape transport capacity is excluded from raw state hashes.

The case evidence preserves host and game source hashes and the exact executed
host bytes. It is conditional on the existing presentation, input and ability
graphics boundaries. It proves neither the original AI desirability policy,
player bag equivalence, ineffective use behavior, stage-cap refusals nor ability
interactions. It is not emulator training or a performance measurement.

## Deferred execution

The exact ledger and commands live in the bounded task's `artifacts` directory.
Run the initial three fights first. If they pass without source/profile changes,
run the remaining eleven slot-0 fights and fourteen slot-1 fights serially.
This is 28 distinct full fights total, with a native replay per fight. Do not
rerun the initial three as an additional complete 28-case batch. Preserve fresh
output directories and stop at any source discrepancy instead of changing the
witness or extending frozen machinery without coordinator review.
