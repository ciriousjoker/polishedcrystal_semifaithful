# Mobile Adapter development

The normal mobile build supports four-player lobbies and native overworld avatars.
Flutter provides the Bridge UI; native Rust owns transport and a shared household
session. Several local emulator connections can play through an embedded lobby
owner without an upstream service. Internet mode multiplexes those same players
through one WebSocket to a hosted lobby owner. The socket-independent Rust rules
are shared by the embedded owner, native hosted simulator and WASM Durable Object.

Scope and binding requirements are in [PLAN.md](PLAN.md) and
[REQUIREMENTS.md](REQUIREMENTS.md). Current measured acceptance, hashes and finite
coverage boundaries are in [research/MILESTONE_2.md](research/MILESTONE_2.md).
Native rendering assurance is currently open: a new protected-trainer fixture
finds a missing remote figure, and strict map scanline gates flag unresolved
conflicts. Use `make test-mobile-maps` and `make test-mobile-scanlines`; their
current failures are recorded in
[research/SCANLINE_AND_MBC30_FOLLOWUP.md](research/SCANLINE_AND_MBC30_FOLLOWUP.md).
Public Cloudflare deployment and phone testing are deferred. Calls, interaction
menus, battles, gifts, trades and travel remain later milestones.

## Human development command

From this worktree:

```sh
python3 mobile_adapter/tools/dev_session.py
```

The launcher asks for bedroom, crowded Game Corner or Goldenrod City saves and
uses Chris on Mac and Kris on Android by default. It builds the ROM and Rust
Bridge, starts the Mac at 5% volume, updates stable Android filenames through ADB,
and starts RetroArch with the correct core, intent and freshly generated state.
Ctrl+C cleans up; an occupied port requires approval before its listener is stopped.
See [MANUAL_TESTING.md](MANUAL_TESTING.md) for device prerequisites and shortcuts.

Use `--mac-only --scenario bedroom` without a phone. Local mode embeds the owner.
`--mode hosted --test-one-way-delay-ms 50` starts the separate local router and
lobby model with approximately 100ms added RTT. `--mode remote --upstream
wss://your-origin.example` selects a hosted origin. The development port is 17423.
Phone development currently reaches the Mac over LAN; a device-local Android
Bridge and background-service packaging are future delivery work.

## Build and Bridge app

Use RGBDS 1.0.4, Cargo 1.85 or later and the pinned native ARM64 DoubleCherryGB
core in [research/HOST_ENVIRONMENT.md](research/HOST_ENVIRONMENT.md).
BGB, Wine and Rosetta are unnecessary. On the configured host:

```sh
make mobile RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/
cargo build --locked --manifest-path mobile_adapter/server/Cargo.toml
mobile_adapter/server/target/debug/mobile-bridge
```

The Bridge prints `BRIDGE_READY 127.0.0.1:17423 mode=local`. Enable the emulator's
Mobile Adapter, use the blue adapter and disable telephone relay. Continue an
ordinary save. Select+B joins or leaves; Select+A opens players and status;
Select+B+A requests the echo diagnostic. Physical Select+B precedes native input
locks and exits locally without waiting for a server. Initially overlapping
players need to move apart. Remote players use the actual saved figure and native
movement assets. Native NPC leases are protected by a reviewed source policy.

The feature builds are `make mobile` and `make faithful mobile`. Unsupported
cosmetic/debug/VC/Pocket combinations fail explicitly. All feature assembly lives
in `rom/`; existing game code has small guarded integration hooks. No native
helper is relocated. Feature object files are isolated by variant. Without the
MOBILE_ADAPTER flag, the standard output remains:

```text
SHA-256 314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e
```

Build the Flutter app with its native Rust library using:

```sh
python3 mobile_adapter/tools/build_bridge_app.py --flutter /path/to/flutter --debug
```

Add `--run` to open it. App/CLI listeners must use different ports when concurrent.
See [bridge_app/README.md](bridge_app/README.md) and
[research/SERVER_ARCHITECTURE.md](research/SERVER_ARCHITECTURE.md).
The macOS bundle is built and signed; other platform packaging and background
lifecycle are not yet verified, especially iOS foreground-emulator coexistence.

## Autonomous tests

Host-only unit and assembly contract tests launch no emulator:

```sh
make test-mobile-host RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/
cargo clippy --locked --manifest-path mobile_adapter/server/Cargo.toml --workspace --all-targets -- -D warnings
```

They cover 35 Rust test functions, 19 launcher tests, six ordinary-save fixture
tests and eight lease-policy tests, plus emitted protocol vectors and production
assembly layout. The Rust suite checks bounded framing, arbitrary byte/event
sequences, scoped identity, capacity, expiry, connection churn, aggressive clients,
room isolation, multiplexed households and reconnect. Actual local workerd tests
exercise the Worker/DO adapter and forced hibernation; Flutter tests exercise the
UI and actual Rust FFI listener. Their commands are in the component READMEs.

The actual headless four-core acceptance suite is separate:

```sh
make test-mobile-rom RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/
```

It requires the normal core, read-only profiling core and native frontend. It
generates fresh ordinary-NPC controls from the measured four-player cadence run
as described in MILESTONE_2.md, including real control-specific in-game saves. MOBILE_TEST_CACHE,
optional MOBILE_TEST_CONTROL, MOBILE_TEST_OUTPUT and MOBILE_TEST_CASES configure these
explicit runtime dependencies. Fixture builds temporarily change map source and restore its exact baseline;
do not run concurrent builds. Every run owns isolated test ports and fresh
artifacts, leaving the human listener on 17423 untouched. Failures retain logs,
input events, intermediate PNGs, cycle observations and semantic state snapshots.

Tests boot ordinary batteries and execute real instructions/controller inputs;
matching build symbols and video callbacks provide read-only success conditions.
They do not patch game RAM, battery bytes or frame timing. The compatible-state
recovery case intentionally captures an actual online emulator state and restores
it without an adapter. Manual startup snapshots are regenerated for each ROM/core.
Portable save fixtures and provenance are committed under [tests/scenarios](tests/scenarios/README.md).

Current gates compare four connected cores at 100/200/300/400% against corresponding
ordinary native NPCs, with no additional world-update gaps. They also verify native
avatar tiles and actual hardware colors, local and delayed hosted visible latency,
menu exit, map transitions, rejoin, save-copy isolation and independent native
palette-save controls. Specialized camera and icon restoration and transport
fault suites remain separate. Every new ROM change must retain these gates.

The wire currently checks transport 1, lobby schema 4, capacity 4 and
standard/faithful mechanics. It does not prove arbitrary cross-revision gameplay
compatibility. Prototype room IDs are nonzero 16-bit IDs, not secure invite codes.
Generated semantic compatibility metadata and secure room creation remain later
work. Finite tests do not prove absence of every possible defect or every WAN path.
