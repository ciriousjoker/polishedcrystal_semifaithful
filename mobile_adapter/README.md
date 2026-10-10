# Mobile Adapter development

Milestone 1 is a working, tested transport foundation. This is not yet multiplayer
overworld gameplay. The persistent scope and next milestones are in [PLAN.md](PLAN.md).
The implementation is under `rom/`, the Rust workspace under `server/`, and native
acceptance tools under `tools/`. Existing game code has small conditional hooks;
no existing helper was moved into another ROM bank. No ROM or save is committed.

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
idle overworld scene. Select+B performs a connect/echo/disconnect proof;
pressing the chord again while active cancels. Repeat after release to reconnect.
This temporary developer gesture has no production lobby/menu UI. Runtime
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

This launches no emulator. Eight Rust tests cover codec bounds/endian fields,
fragmented and coalesced streams, malformed/truncated input and three independent
TCP clients. RGBDS additionally compiles the production adapter and complete RAM
layout with the actual LCD/OAM LOAD blocks, then checks emitted protocol vectors
and memory bounds. It does not execute LR35902 instructions. Existing stack and HRAM
addresses are explicit contract assertions.
See [protocol/transport-v1.md](protocol/transport-v1.md).

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
Core instruction cycles exclude hardware interrupt-entry cycles; this is emulator
evidence, not a physical hardware timing certification.

Measured placement, cycle figures, remaining budgets, limitations and test
artifacts are in [research/MILESTONE_1.md](research/MILESTONE_1.md). Recheck them
when the driver or hooks change. Menus/battles and transaction recovery receive
their own acceptance scenarios in the subsequent milestones.
