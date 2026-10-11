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

## MBC30 core and ROM layout

The mobile flag now selects a **4 MiB MBC30** ROM. All banked feature sections
occupy the upper half, starting at bank 128. The ordinary game stays below 2 MiB;
only minimal guarded integration remains there. Battery allocation is unchanged
at 32 KiB. Disabled builds retain their original 2 MiB target and exact hash.
The previous 10,000-byte cap was explicitly replaced by the user. See
[research/MBC30_MIGRATION.md](research/MBC30_MIGRATION.md) for current evidence.

Install the supplied `DoubleCherryGB-mbc30-macos-universal.zip` from Downloads
and build its separate read-only headless observers once:

```sh
python3 mobile_adapter/tools/prepare_mbc30_core.py --profile
```

The core and all test builds live in `/Volumes/Storage/Cache/mobileadapter/`.
The release library is checksum-pinned to fork revision `ef247c6`. Launchers and
acceptance tools select this fork by default; the old core cannot read bank 128.
Fresh start snapshots are regenerated for the new ROM/core. An Android core must
also be rebuilt from this fork and installed with `--reinstall-core`; the old
phone installation is incompatible. Device validation remains deferred.

## Human development command

From this worktree:

```sh
python3 mobile_adapter/tools/dev_session.py
```

The launcher asks, in order, for a starting scenario and the number of **Mac**
game windows (1..4, default 2). It detects authorized ADB devices and asks whether
to update/open Android only when a device is available. Android adds one player,
so at most three Mac instances may accompany it. Offline/unauthorized devices do
not trigger the question. `--serial` selects among multiple authorized devices.

All instances use the same scenario's progress, party, name, clock and position.
The development fixtures change only the selected figure, trainer ID and ordinary
main/backup checksums. Mac figures cycle Chris, Kris, Crys, Beta, starting at
`--mac-avatar`; Android takes the next figure unless `--android-avatar` is given.
IDs 10001..10004 differ for convenience, but equal trainer IDs are fully supported
and are never network identities. Every instance gets its own save and fresh
core snapshot in a new timestamped run directory, so an older open emulator
cannot overwrite a newly prepared instance's save.

By default, the command builds the ROM, native Rust engine and Flutter shell;
starts the local Worker/lobby simulator on **8787**; auto-connects the separate
Flutter Bridge on **17423**; opens the requested RetroArch windows with the
checksum-pinned custom MBC30 DoubleCherryGB core at 5% volume; and optionally
updates/opens Android with the existing stable filenames and ES-DE-style intent.
Missing/outdated Android cores are installed as part of the approved update;
unchanged core/app stamps skip installation. `--reinstall-core` forces it.

Server logs and Bridge connection/join/disconnect logs appear in the terminal.
**Ctrl+C stops only the local server and frees port 8787.** Bridge and emulator
windows remain open. The Bridge's emulator TCP port 17423 remains active until
you stop/close the Bridge. With the server stopped, it waits to reconnect. Close
emulator windows yourself. An occupied port still requires interactive approval
before its identified listener is stopped.

Use `--mac-only --instances 2 --scenario bedroom` for an unattended Mac launch.
`--prepare-only` generates artifacts without opening GUIs/listeners.
`--mode hosted --test-one-way-delay-ms 50` adds approximately 100ms RTT.
`--mode local` embeds the lobby in the Bridge without a separate server;
`--mode remote --upstream wss://your-origin.example` uses an external server.
In these two modes Ctrl+C ends terminal monitoring only.
Phone development reaches the Mac over LAN. A device-local Android Bridge and
background-service packaging remain future delivery work.
See [MANUAL_TESTING.md](MANUAL_TESTING.md) for device prerequisites and shortcuts.

## Build and Bridge app

Use RGBDS 1.0.4, Cargo 1.85 or later and the pinned native ARM64 DoubleCherryGB
core in [research/MBC30_MIGRATION.md](research/MBC30_MIGRATION.md).
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
