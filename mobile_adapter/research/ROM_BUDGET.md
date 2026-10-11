# Multiplayer ROM budget and optimization audit

Audited 2026-10-11 against production checkpoint `0dcc9022f`. This is an analysis
and acceptance-gate change. Production assembly, ROMs, gameplay and transport
remain unchanged. Size-only candidate ROMs are not runtime-tested releases.

## Binding budget

The user set a 10 KB limit for the complete multiplayer ROM component. Interpret
this conservatively as **10,000 bytes**, not 10,240 bytes. Count executable code,
embedded feature data and the net cost of all integration seams. Rust, Flutter,
Python and standalone test fixtures are outside the ROM budget. Measure against
the disabled counterpart with the same game revision and other build options.
Also cap the feature-owned section sum: reducing ordinary game code must not
fund an oversized addon. An unmet budget is an acceptance blocker.

The current 2 MiB outputs have these measured costs:

| Item | Bytes |
| --- | ---: |
| Free ROM in disabled counterpart | 13,961 |
| Free ROM in production multiplayer build | 2,849 |
| Net whole-ROM addon | 11,112 |
| Feature-owned ROM sections | 10,938 |
| Net integration difference | 174 |
| Required reduction to meet the budget | **1,112** |

The required reduction is about 10% of the addon. A 10,240-byte interpretation
would require 872 bytes instead, but it is not the selected budget. Meeting the
10,000-byte limit would leave 3,961 bytes of aggregate space at this game revision,
not unlimited future content capacity or necessarily a contiguous allocation.
ROM0 currently has 39 free bytes. It needs its own placement/timing checks.

| Production section | Bytes |
| --- | ---: |
| Driver, wire protocol, native actors/leases, LZ decoder, prototype menu | 8,011 |
| Private palette handling and emergency input | 1,258 |
| Generated NPC lease policy | 1,005 |
| Save isolation | 257 |
| Raw tile backups | 237 |
| Incremental map fingerprint | 170 |

## Executed size experiments

`probe_rom_savings.py` links cumulative candidates in an isolated object/output
directory and restores feature sources in `finally`. It verifies that production
ROM, map and symbol hashes remain unchanged. No original game helper is manually
relocated and no mapper, save allocation, user process or battery file is changed.

| Cumulative candidate | Net addon | Incremental saving | Qualification |
| --- | ---: | ---: | --- |
| Unused routines and host-only HELLO fixture | 10,996 | 116 | 93 unreferenced routine bytes; 23 fixture bytes move out of production ROM |
| Shorten 39 statically in-range JP/JMP sites | 10,957 | 39 | Branches link successfully; executed behavior/timing still needs the native gates |
| Bitmap/prefix representation of NPC policy | 10,954 | 3 | All 611 map masks match in host checks; decoder and metadata erase almost all apparent data savings |
| Place all six feature sections in bank 127 | 10,930 | 24 | 24 internal far calls/jumps become near operations; bank/register/stack contracts need runtime verification |
| Move one-shot echo diagnostic into a test fixture | 10,697 | 233 | Changes the developer diagnostic path; persistent protocol/recovery source retained in candidate only |
| Move prototype Select+A controls to Bridge | 10,173 | 524 | Retains chord consumption and Select+B path; removes current in-game prototype UI |

The final candidate saves **939 bytes**, still **173 bytes over budget**. The
menu/diagnostic changes are options for discussion, not adopted behavior changes.
Future interaction menus and the requested connection indicator would still
consume ROM. These experiments do not establish that the finished feature set
already fits. Raw table compression ratios are particularly misleading here.

The current 1,005-byte NPC policy consists of 49 reader bytes, 78 group-pointer
bytes, 611 map-index bytes and 267 shared-mask bytes. The constant-access prototype
uses 94 chunks with bitmap/prefix pairs, 304 nonzero indices and 156 group-pointer
bytes. Its extra reader code leaves just three bytes saved. Sequential/RLE lookup
could use less storage, but adds scan work to the lease path and needs a separate
bounded timing design; it is not a free substitute for the current lookup.

## Larger research opportunity: ROM-only NPC metadata

The disabled ROM's **2,882 object type bytes across all 611 maps** never use bit 7.
A host-only transform reproduced all **775 audited replaceable actor flags** in
that bit and recovered every original type by masking it off. The map record
width stays unchanged, adding **zero map-record bytes**. This proves an available
encoding at this revision, not a working ROM implementation.

A possible design would generate the existing audited allowlist as compile-time
metadata, mark only the corresponding ROM object records, and replace the
956-byte policy data with a bounded reader of those ROM flags. Feature assembly
would stay in the dedicated directory. Likely integration seams are the
`object_event` macro and `home/map.asm::CopyMapObjectHeaders`, with conditional
calls/macros only; do not hand-edit all map sources. Estimated net savings are
approximately **800 to 900 bytes** after replacement reader/normalization code,
but that estimate has not been compiled or timing-validated.

Mandatory conditions before adoption:

- Strip the ROM-only tag before any ordinary engine consumer or save path can
  observe it. Never persist it in native map objects or SRAM. Prove ordinary
  primary/backup saves remain byte-identical in tagged/untagged map loads,
  connections, reloads and interrupted/recovered sessions.
- Preserve every ordinary object type, trainer/dialogue behavior and protected
  actor. The allowlist review policy and original-state restoration do not change.
- Keep reads bounded, validate map/record boundaries, preserve banks/registers,
  and pass actual four-core cadence, palette, save and fault tests. Avoid a new
  long scan inside a normal game frame; any additional load cost must be measured.
- Fail source/build audits if upstream claims the bit, changes record/type
  semantics, or invalidates the reviewed actor policy. Mask feature-only metadata
  appropriately in the eventual base-game compatibility calculation.
- Keep the disabled build exactly hash-identical. Any extra integration must be
  reviewed against the minimal-footprint requirement before implementation.

This could create meaningful room without removing game content, save isolation
or packet validation. It is the most promising specific next investigation.

## Assessment and next work

A 10,000-byte target is credible for the current four-player foundation, but
is not yet achieved. The current source has concrete redundancy, development-only
UI/diagnostics and a potentially avoidable policy table. Merely shortening jumps
will not recover the required 1,112 bytes. Prioritize verified cleanup, internal
bank contracts and the ROM-only policy research, then measure again.

Do not promise that battles, trades, gifts, calls, travel, the unresolved scanline
policy and every future game revision will all fit in the remaining budget.
Reuse existing game routines and Bridge-owned lobby/compatibility behavior, budget
each future feature before implementing it, and report conflicts. The hard cap
remains binding; further requirements cannot silently bypass it.

There are no duplicated remote-avatar graphics to remove: the ROM already reuses
native assets. The 545-byte private resumable decoder and private palette/save
handling protect timing and ordinary game state. They must not be removed just
to hit a byte count. WRAM and save SRAM are not spare permanent program storage.
Downloading code or modifying emulator RAM would change the hardware model and
adapter-free recovery requirements.

Moving to a multiplayer-only 4 MiB MBC30 target remains a separate migration.
Changing a header/file size alone does not fix upper-bank selection, emulator
support, RTC, save compatibility or build tools. The pinned DoubleCherryGB core
fails the recorded upper-bank probes. A larger cartridge does not enlarge ROM0,
RAM or sprite budgets. This audit retains the current 2 MiB target and does not
waive the 10,000-byte cap.

## Reproduction and acceptance

```sh
make test-mobile-budget RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/
python3 -m unittest discover -s mobile_adapter/tools -p 'test_rom_budget.py'
python3 mobile_adapter/tools/probe_rom_savings.py --output /path/to/fresh/probe-directory
```

Run the source-mutating size probe serially with other builds. Normal `make mobile`
remains available for development; `test-mobile-budget` and the full ROM acceptance
target fail while the budget is exceeded. These failures must not be described as
successful acceptance. The independent scanline blockers also remain open.

The compact evidence file [rom-budget-evidence.json](rom-budget-evidence.json)
contains production hashes, linked byte measurements and host metadata checks.
Candidates have link/data evidence only, **no executed runtime acceptance**.
Both production variants were rebuilt and retain their previous hashes. Seven
new budget regression tests, eight existing lease tests and the host assembly/
cross-language contracts pass. The strict budget target fails as expected at
11,112 bytes; no emulator timing/save tests were run on the experimental ROMs.
