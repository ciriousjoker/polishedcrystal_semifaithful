# Production MBC30 migration

Updated: 2026-10-12. The user supplied a forked macOS core and explicitly
replaced the old 10,000-byte addon limit with a 4 MiB MBC30 target. The original
lower half must remain minimal, and disabled builds must stay hash-identical.
The user also requested returning every new build/test artifact to Storage.

## Layout and core

`make mobile` and `make faithful mobile` now produce 4,194,304-byte ROMs.
All six banked feature sections are in bank 128, starting at file offset
2,097,152. Original game sections remain in banks 0..127. No ordinary helper was
relocated. Every mobile link validates the split and rejects original sections
upstairs or banked feature sections downstairs. Repeated builds also validate
cached output; a deliberately invalid cached map was rejected without relinking.
This uses a feature-owned Python
guard; the original `tools/bankends` is unchanged and still handles ordinary
builds. Future feature sections may occupy other banks in 128..255, subject to
explicit call/bank contracts and the existing RAM/timing/section-size guards.

The header remains cartridge type `0x10` (timer + RAM + battery); size code is
`0x07` for mobile and `0x06` for disabled builds. MBC30 has no distinct header
cartridge type here. Battery size code `0x03` remains 32 KiB in both variants.
This requires an MBC30-capable emulator or physical board. Ordinary MBC3 boards
and the old DoubleCherryGB core cannot run the expanded feature ROM. Physical
cartridge hardware has not been tested. More ROM does not expand RAM or OAM.

The final standard feature owns **10,946 upper ROM bytes** and costs **174 net
lower integration bytes** against its matching disabled counterpart. Lower
headroom is **13,787 bytes**, including **39 in ROM0**; upper headroom is
**2,086,206 bytes**. Faithful lower headroom is 13,850 bytes. The counterpart
budget regression guard caps lower net integration at 256 bytes; this is an
engineering ceiling, not a promise that any future upstream content will fit
an arbitrarily full native ROM. Existing game content is retained.

DoubleCherryGB fork revision: `ef247c6545c61358cd8679a78c95c2e92daae13d`.
The supplied universal library SHA-256 is
`e26383c5151d68716e973c4fd540b43b7c476d267f4078058a0f7bdb5d28e96d`;
matching source archive SHA-256 is
`61d92544eb0bd22a4b52802505c5fcd597bffdf689710d1f05d2b82d8a0d6865`.
It executes natively on ARM64, with no Rosetta or Wine. The mapper probe reads
`0x31`, `0xa5`, `0x5a` from banks 1, 128 and 255. Both the read-only palette
observer build and the instruction observer build pass all six fork CTest cases.

The Mac launcher uses the exact delivered release library. Integration tests use
separate builds from its matching source, with debugger-only palette and CPU
observations. The default paths and revision are centralized in core_runtime.py.
An explicit `--core` / `--profile-core` lets the acceptance tools select a reviewed
alternative; the manual launcher verifies the supplied release checksum.

## Graphics race exposed by migration

The initial expanded ROM failed the existing connected-menu emergency-exit test:
restored sprite 124 in native slot 8 had mismatched standing/walking VRAM hashes.
The exact previous 2 MiB ROM passed the same test with the new fork, while the
expanded ROM already passed offline snapshot recovery and actual disconnection.

Feature code selected VBK before disabling interrupts. Native VBlank background
attribute/menu work can overwrite VBK in that interval, so a chunk can access
the wrong VRAM bank. The bank decision now stays in one private, unbanked byte;
read and GDMA copy loops select VBK only after DI. RawChunk returns the selected
bank explicitly instead of changing mutable hardware state ahead of the lock.
This changes feature assembly only, adds eight ROM bytes and one private WRAM0
byte, and does not alter native save/RAM addresses or native interrupt code.
The host assembly contract checks the actual DI/VBK-selection instruction prefix.
The connected-menu and compatible online-state recovery tests now pass.

## Reproduction

Install the archive from Downloads and prepare the matching observers once:

```sh
python3 mobile_adapter/tools/prepare_mbc30_core.py --profile
```

Build and check the focused mapper/layout/runtime suite:

```sh
make test-mobile-mbc30 RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/ MOBILE_CARGO=/Volumes/Storage/Cache/cargo/bin/cargo
```

Use `MOBILE_TEST_CASES` to select finite runtime groups and `MOBILE_TEST_OUTPUT`
to choose a fresh Storage directory. This target is migration acceptance; it
does not waive the separate strict all-map and protected-scanline gates in
`test-mobile-rom`. Normal host unit/ASM tests do not start an emulator.

The existing human command remains `python3 mobile_adapter/tools/dev_session.py`.
Use `--mac-only --scenario bedroom` without a phone. The launcher patches the
endpoint in the upper bank, retains port 17423 and stable filenames, backs up
existing batteries, and regenerates snapshots with the new core. Android builds
now use this fork's source and a separate build directory; the old installed
phone core must be replaced using `--reinstall-core`. Android and its NDK output
have not been validated during this migration, and no device was contacted.
Old snapshots from a different ROM/core are not acceptance inputs. Ordinary
in-game `.sav` fixtures remain unchanged.

## Final ROM hashes and finite runtime checks

- Standard mobile (4 MiB):
  `cfc57e531f839c2a58bc202fe730ec74832c0d16a923421e9f2833a8329a8a20`.
- Standard disabled (2 MiB, pristine baseline):
  `314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e`.

Four actual clients at 100/200/300/400 percent complete 800/800 world-update
frames each, with zero duplicates, serial/tick register failures or background
work crossing the next VBlank. Per-figure native controls replay movement and
verify the same ten-actor inventory, peer positions and two reclaimed actors.
Their ordinary timelines contain 783..784 update frames and 16..17 duplicates;
the multiplayer run adds no gaps. These are actor-load controls, not identical
cold-join animation/RNG histories or a proof of every frame in every map.

All five final-build Save scenarios pass: connected, peer disconnect, server
close, malformed response and emergency disconnect. Primary/backup checksums,
fresh offline Continue, ordinary movement, matching-counterpart compatibility,
native palette equality and instruction-level save-copy projection checks pass.
The exact supplied release library passes reconnect/echo and no-adapter tests.
The launcher creates an ordinary battery and fresh start snapshot successfully
in an isolated `--prepare-only --mac-only` run, without opening a GUI or phone.
Start menu, running, warp, rejoin and both local/hosted latency scenarios pass
with four clients. Hosted mode uses two Bridges and adds 100ms RTT. Measured
additional motion p95 is 169.187ms local and 298.500ms hosted; those finite host
measurements do not guarantee WAN latency or remove the existing thin margin.

## Validation boundaries

The current run's machine-readable evidence is in mbc30-migration-evidence.json;
large runtime files and failure traces remain under
`/Volumes/Storage/Cache/mobileadapter/mbc30-tests/`. The initial regression and
previous-ROM control are preserved, rather than discarded after the fix.
The disabled standard is freshly rebuilt and matches the pristine baseline
SHA-256 `314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e`.
All 23,373 checked native RAM symbols remain unchanged.

The map capacity audit still checks all 611 maps. Strict scanline review remains
open: 698 conservative pairs on 269 maps, including 355 protected pairs. The
native Route 37 pressure diagnostic still detects the known missing remote
figure; additional ROM capacity does not fix that hardware presentation limit.
Do not treat this migration as unrestricted gameplay or milestone-3 acceptance.
Cloudflare deployment, phone execution and real MBC30 cartridge execution remain
outside this run. Finite frame/save/fault tests are evidence, not a universal
proof of flawless execution in every possible future game state.
