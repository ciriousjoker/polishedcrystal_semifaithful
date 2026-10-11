# Mobile Adapter development

The current implementation adds three-player lobbies and live overworld avatars
to the tested transport foundation. Milestone 2 now requires four-player support
and the acceptance work below. Battles, calls, gifts and trades follow in
milestones 3 to 5. The persistent scope is in [PLAN.md](PLAN.md).
Four-player coexistence is the absolute gameplay feature requirement; the other
gameplay goals are a wishlist subject to feasibility and discussion. Foundational
quality requirements remain binding. The current server, state ownership, hosting
options and test gaps are explained in [SERVER_ARCHITECTURE.md](research/SERVER_ARCHITECTURE.md).
The binding requirements and explicit acceptance gaps are in
[REQUIREMENTS.md](REQUIREMENTS.md). Strict frame parity remains open; 400% speed,
priority escape and connected-state restoration without libmobile need additional
acceptance tests. The required capacity is now four total players on the same map;
the implementation currently supports three. Gifts/trades will use ordinary
user-managed saves, without a transaction journal or durable recovery requirement.
Historical milestone-2 acceptance and measured limits are in
[research/MILESTONE_2.md](research/MILESTONE_2.md).
The implementation is under `rom/`, the Rust workspace under `server/`, and native
acceptance tools under `tools/`. Existing game code has small conditional hooks;
no existing helper was moved into another ROM bank. No ROM is committed; portable
ordinary-save test fixtures are in `tests/scenarios/`.

For manual Mac and Android testing, use the single command in
[MANUAL_TESTING.md](MANUAL_TESTING.md). It builds and starts the Rust server on
fixed port 17423, starts the Mac game at 5% volume, updates the phone's stable
Download files through ADB, and directly starts its game with a freshly generated
start state. No additional Android app is built or installed.

## Build and developer proof

Use RGBDS 1.0.4, Rust/Cargo 1.85 or later, and the pinned native DoubleCherryGB
build described in [research/HOST_ENVIRONMENT.md](research/HOST_ENVIRONMENT.md).
BGB, Wine and Rosetta are not required. On the configured development host:

```sh
adapter_cache=/Volumes/Storage/Cache/mobileadapter
make -j4 mobile RGBDS="$adapter_cache/rgbds-1.0.4/"
/Volumes/Storage/Cache/cargo/bin/cargo build --locked --manifest-path mobile_adapter/server/Cargo.toml
mobile_adapter/server/target/debug/mobile-server
```

The server prints `READY 127.0.0.1:17423`. Load
`polishedcrystal-mobile-3.2.3.gbc` with DoubleCherryGB's Mobile Adapter enabled,
blue adapter and telephone relay disabled. Continue a normal save into a safe
idle overworld scene. Select+B joins or leaves lobby 1. Select+A opens status,
players and connect/disconnect controls. Select+B+A runs the echo diagnostic.
These are the current build's controls; the required independent, disconnect-only
Select+B path is not implemented yet.
Three clients can join; each sees two peers in a compatible map instance.
Initial saves share coordinates, so move a tile to separate overlapping avatars.
Avatars use each player's selected Chris/Kris/Crys/Beta figure, movement sprite
and the native palette allocator. Runtime
symbols expose success, error, ownership and guard state for automated oracles.

The only currently supported feature builds are standard `make mobile` and
`make mobile faithful`; cosmetic/debug/VC/Pocket feature combinations fail with
an explicit message. Ordinary disabled builds retain their upstream behavior.
Feature object files and generated version graphics are isolated by variant.
Switching supported mobile variants does not contaminate the disabled standard
ROM. The original compiler output remains reproducible:

```text
polishedcrystal-3.2.3.gbc
SHA-256 314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e
```

## Host-only tests

```sh
make test-mobile-host \
  RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/ \
  MOBILE_CARGO=/Volumes/Storage/Cache/cargo/bin/cargo
```

This launches no emulator. Twenty Rust tests cover codec bounds/endian fields,
fragmented and coalesced streams, malformed/truncated input and three independent
TCP clients, lobby capacity, generations/resume, expiry, compatibility and
latest-state coalescing. A real TCP push test also preserves partial inbound
frames while sending deltas. RGBDS additionally compiles the production adapter and complete RAM
layout with the actual LCD/OAM LOAD blocks, then checks emitted protocol vectors
and memory bounds. It does not execute LR35902 instructions. Existing stack and HRAM
addresses are explicit contract assertions.
The same target runs eighteen host deployment tests and six portable-save selection
and integrity tests, including validation of the chosen scene and figure in
each freshly prepared start snapshot.
See [protocol/transport-v1.md](protocol/transport-v1.md) and
[protocol/lobby-v2.md](protocol/lobby-v2.md).

## Binding isolation and failure requirements

All feature assembly remains in `mobile_adapter/rom`, with only conditional
integration hooks in game files. `make mobile` enables MOBILE_ADAPTER; disabled
output must match the corresponding pristine upstream SHA-256. The adapter owns
separate work buffers and allocates no SRAM. Native Save logic stays unchanged.
Malformed packets and transport failures must release the adapter and retain
local play, without reset or writes to saved game data.

World-update timing must match the same input sequence without the feature.
Profiling now counts frames with updates, duplicate updates and every gap,
rather than accepting a high total that hides missed frames. Pass a directory
of matching baseline client profiles with `probe_lobby.py --profile
--cadence-baseline PATH` to enforce this requirement. The current NPC performance
follow-up still has additional missed frames and does not meet strict parity.
Do not treat older milestone cycle budgets as proof of identical frame timing.

## Explicit native integration tests

Build the tiny frontend as documented in HOST_ENVIRONMENT.md. First regenerate a
real battery fixture through New Game and the ordinary in-game Save menu:

```sh
python3 mobile_adapter/tools/probe_emulator.py \
  --core "$adapter_cache/build/doublecherry/DoubleCherryGB_libretro.dylib" \
  --runner "$adapter_cache/smoke-libretro" \
  --output "$adapter_cache/new-baseline-run"
```

Then run the actual feature ROM transport suite. Output directories must be new:

```sh
python3 mobile_adapter/tools/probe_transport.py \
  --core "$adapter_cache/build/doublecherry/DoubleCherryGB_libretro.dylib" \
  --runner "$adapter_cache/smoke-libretro" \
  --server mobile_adapter/server/target/debug/mobile-server \
  --battery "$adapter_cache/new-baseline-run/create/ingame.sav" \
  --output "$adapter_cache/new-transport-run"
```

The suite owns its server process, refuses to stop another listener on port
17423, and runs 14 native game processes across 12 scenarios: reconnect, three
concurrent clients, split response, bad magic, silence/timeout, remote close,
missing server, missing adapter, cancellation, recovery, oversized and truncated
responses. Each instance starts with an isolated configuration directory,
imports ordinary SRAM at boot, and runs instructions and button inputs. It never
loads an emulator save state or writes test values into game RAM. Matching build
symbols supply read-only observers. Game state, server transcripts, movement,
crash markers and buffer guards are semantic success conditions. PNGs at
intermediate state changes and completion support visual diagnosis; there is no
brittle golden image requirement.

The frontend accepts one-shot `ON MIN_FRAME SYMBOL=VALUE[,SYMBOL=VALUE] BUTTON
DURATION` actions, fails when required states are never reached, and paces live
network frames. Fixed boot inputs are specific to the baseline. For later UI,
add semantic phases rather than guessing arbitrary global frame delays.

The separate save-failure integration suite exercises real online Save followed
by peer disconnect, server loss and malformed application packets, then boots
its exported battery in fresh offline instances:

```sh
python3 mobile_adapter/tools/probe_save_safety.py \
  --core "$adapter_cache/build/doublecherry/DoubleCherryGB_libretro.dylib" \
  --runner "$adapter_cache/smoke-libretro" \
  --server mobile_adapter/server/target/debug/mobile-server \
  --battery mobile_adapter/tests/scenarios/bedroom/chris.sav \
  --peer-battery mobile_adapter/tests/scenarios/bedroom/kris.sav \
  --output "$adapter_cache/new-save-safety-run" --port 17510
```

Both primary and backup checksums, save stability after failure, post-failure
movement and fresh Continue are required. This tests ordinary battery saves.
Emulator snapshots include CPU and RAM addresses, so manual auto-start snapshots
are regenerated for the current ROM/core. A finite suite cannot prove absence
of every possible defect or protect against frontend disk-flush failures.

## Optional runtime profiling

Keep the normal core untouched. Prepare a separate copy using the committed
read-only instruction observer:

```sh
python3 mobile_adapter/tools/prepare_profile_core.py \
  --source "$adapter_cache/sources/doublecherry" \
  --output "$adapter_cache/sources/doublecherry-profile-new"
cmake -S "$adapter_cache/sources/doublecherry-profile-new" \
  -B "$adapter_cache/build/doublecherry-profile-new" -DCMAKE_BUILD_TYPE=Release
cmake --build "$adapter_cache/build/doublecherry-profile-new" -j4
```

Pass `--profile-core PATH_TO_PROFILE_DYLIB` to the transport suite. It verifies
registers, stack, physical ROM/WRAM banks, bank shadows, audio/video callbacks and
instruction-cycle ceilings during the reconnect scenario. Pinning checks the
core revision and unchanged CPU source before copying. The source already needs
the documented CMake dependency patch. Profiling changes no CPU state, networking
or emulated timing. The source-audited interrupted Bankswitch case is recorded
separately, because a far call can complete an already pending mapper write.
The actual-VBlank metric includes a separate 20-cycle hardware IRQ-entry charge.
Instruction-span and frontend-batch metrics remain available for comparison.
This is emulator evidence, not a physical hardware timing certification.

Measured placement, cycle figures, remaining budgets, limitations and test
artifacts are in [research/MILESTONE_2.md](research/MILESTONE_2.md). Recheck them
when the driver or hooks change. Menus/battles and transaction recovery receive
their own acceptance scenarios in the subsequent milestones.

## Three-player overworld acceptance

Run the actual ROM with three independent native core processes:

```sh
python3 mobile_adapter/tools/probe_lobby.py \
  --runner "$adapter_cache/smoke-libretro" \
  --core "$adapter_cache/build/doublecherry/DoubleCherryGB_libretro.dylib" \
  --server mobile_adapter/server/target/debug/mobile-server \
  --battery "$adapter_cache/m1-baseline-regression/create/ingame.sav" \
  --latency --port 17425 --output "$adapter_cache/new-lobby-test"
```

Add `--test-one-way-delay-ms 50` for the declared RTT100 experiment. The probe
measures captured local motion to remote four-quad OAM motion, with 40 samples
and enforced p95 limits of 200 ms local or 300 ms RTT100. It also reports the
full input-to-remote delay, including the unchanged vanilla turn script.
Add `--scenario speed` for 24 direction changes with one client at 300% and
two at 100%; combine it with RTT100 to exercise independent delivery credits.
`--scenario menu`, `rejoin`, `resume` and `warp` use the ordinary bedroom fixture.
`resume` inserts a transparent TCP proxy and drops one live socket.

`create_scene_save.py` generates other ordinary saves through New Game/in-game
Save from a disposable spawn/clock producer ROM. Use Snowtop Outside (15,22,
hour 20) for `camera`; Route17North (9,12) for bicycle mode;
PalletTown (8,1) for `connection` and (8,6) for `running`;
NewBarkTown (22,8) for surf mode; LakeOfRage (18,24) for the large Gyarados
exception; CinnabarVolcano1F (14,15) for rocks; Goldenrod
Game Corner (14,5) with `--exercise-pressure` for crowding. A Snowtop fixture
with `--set-initial-event EVENT_OLIVINE_CITY_HIDDEN_RARE_CANDY` provides a
different ordinary layout for `--scenario instance --different-battery PATH`.
All initialization substitutions are recorded; battery SRAM is never edited.

Pass the freshly prepared profiling core as `--core ... --profile` to check
registers/banks, stack, serial VBlank budgets and every NPC restore return.
Existing output directories are refused. ROM/SYM/save/core hashes, intermediate
traces, server logs, PNGs and input events survive failures. No emulator save
states or injected game RAM are used in acceptance.

See [research/MILESTONE_2.md](research/MILESTONE_2.md) for measured evidence,
rendering limits and protected-scene exceptions.
Current avatar/scenario acceptance and build hashes are in
[research/AVATAR_SCENARIOS.md](research/AVATAR_SCENARIOS.md).

## Committed manual scenarios and selected figures

The one-command launcher now asks for bedroom, crowded Game Corner or moving-NPC
Goldenrod City saves. It defaults to Chris on Mac and Kris on Android. Pass
`--scenario game-corner --mac-avatar crys --android-avatar beta` to choose directly.
All twelve portable ordinary batteries and their hashes are committed in
[tests/scenarios](tests/scenarios/README.md). Current ROM/core snapshots are
regenerated per platform, never reused across builds.

The live protocol is now schema 3 with an explicit four-choice figure byte.
Two independent bank-2 graphics caches hold native sprite data; four VRAM0 tiles
per peer display the current frame without touching the local player's sprite.
The renderer uses native character/movement tables and allocates appropriate
native palettes without recoloring required NPCs. If the eight hardware palettes
are exhausted and no exact match exists, that peer is temporarily deferred.
Start-menu drawing remains intentionally paused and resumes after menu close.

`probe_lobby.py` accepts `--second-battery` and `--different-battery` for distinct
figures, `--scenario start-menu` for the normal Start menu, and
`--require-npc-motion` for the wandering-NPC fixtures. The read-only observer
checks decompressed sprite-cache hashes, actual displayed VRAM frame hashes,
remote figure IDs and loaded palette identities against native assets.

```sh
python3 -m unittest discover -s mobile_adapter/tools -p test_save_scenarios.py
```
