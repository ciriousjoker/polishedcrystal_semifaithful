# Development host readiness

Checked: 2026-10-10. Host: Apple Silicon, macOS 27.0.1 (26A434), ARM64.
Branch: `mobileadapter`. No production game source was changed in this setup.

Decision: use native DoubleCherryGB as the primary emulator. The user explicitly
rejects Rosetta installation. Wine and BGB are not prerequisites or acceptance
gates. This host can execute the development and automated test workflow without
a graphical frontend or manual game input.

## What works now

| Component | Installed or prepared | Verification |
| --- | --- | --- |
| Repository | Attached worktree on `mobileadapter` | Baseline source commit `8f3c7e9651e988d876e32a4c7ad9ef1bb76702a8` |
| RGBDS | Default 1.0.1; separate required 1.0.4 | Complete standard ROM rebuild with 1.0.4; exact official SHA-256 |
| Rust / Cargo | 1.85.0, native ARM64 | Temporary binary, `rustc --test`, and offline `cargo test` succeeded |
| C/C++ toolchain | Xcode clang 21, CMake, pkg-config, make | Built the library, bridge, core, and headless frontend |
| libmobile | Pinned native static library and headers | CMake build/install succeeded |
| libmobile-bgb | Pinned native ARM64 `mobile` executable | Build/install and `mobile --help` succeeded |
| DoubleCherryGB | Pinned native ARM64 libretro core, embedded libmobile | Three independent boots; automated real Save, fresh Continue, and verified tile movement |
| BGB | Official 1.6.6 x64 archive downloaded and extracted | PE executable inspected; not executed |
| Wine | Absent | No Wine executable or installed Wine/CrossOver/Whisky app found |
| macOS Rosetta | No working installation detected | `arch -x86_64 /usr/bin/true` fails with `Bad CPU type`; no installation receipt |

Native development, assembly/link checks, Rust host tests, and headless ROM
integration work without Wine, Rosetta, a VM, or a container. The frontend supplies
inputs and captures native video. Read-only observers use the current `.sym`
file and core memory descriptors, avoiding hard-coded runtime addresses and old
CPU/WRAM snapshots. The normal host unit-test command will not launch an emulator.

Verified end to end by `tools/probe_emulator.py`:

1. Boot the unchanged ROM, select New Game, complete the introduction, and save
   through the ordinary in-game menu. Export its raw 32-KiB battery SRAM.
2. Verify save version 10, both 97/127 sentinel pairs, and both game-data checksums.
3. Start a fresh core process, load that battery file, and Continue to the same
   map (group 24, number 5) and position (3, 3).
4. In another fresh process, Continue and complete a one-tile step right to (4, 3).
   Observe matching player-object coordinates (7, 7) then (8, 7), a changed rendered
   image, CGB mode, and a zero crash marker. The source battery fixture is unchanged.

All three stages passed without manual input. Logs, raw saves, PNG/PPM images,
and state results stay in the cache. A machine-readable report with hashes is
committed as [native-automation-probe.json](native-automation-probe.json).
These three sequential scenarios supplement the earlier three independent boot
processes; neither is a connected multiplayer session.

The adapter option is enabled, but the unchanged ROM sends no adapter commands.
ISP login, TCP connectivity, network latency, serial ISR behavior, and multiplayer
features are not proven by this probe. There is no Rust server crate or feature
unit-test suite yet. Earlier Rust tests were temporary toolchain checks.
The necessary native tools exist; implementing and verifying that transport is
normal development work, not a reason to install BGB.

## Cache locations and provenance

Reusable tools and external sources live outside the Git worktree:

```text
/Volumes/Storage/Cache/mobileadapter/
  sources/libmobile/
  sources/libmobile-bgb/
  sources/doublecherry/
  rgbds-1.0.4/
  build/libmobile/
  build/libmobile-bgb/
  build/doublecherry/DoubleCherryGB_libretro.dylib
  install/lib/libmobile.a
  install/include/
  install/lib/pkgconfig/libmobile.pc
  install/bin/mobile
  bgb/bgb64.exe
  smoke-libretro
  smoke/a/, smoke/b/, smoke/c/
  automation-20261010/
```

Rust and Cargo are in `/Volumes/Storage/Cache/cargo/bin/`. Worktree:
`/Volumes/Storage/Cache/codex_worktrees/mobileadapter/polishedcrystal`.
Keep caches out of commits; keep source pins, patches, and probe source in Git.
Do not assume these paths exist on another host.

Pinned external sources:

- [libmobile](https://github.com/REONTeam/libmobile/tree/0704f56902f23b7ebf05c82c222e0e145e3140b6):
  `0704f56902f23b7ebf05c82c222e0e145e3140b6`.
- [libmobile-bgb](https://github.com/REONTeam/libmobile-bgb/tree/4dfc19f69a8ccb6f2ada55e49765419f76d03e2c):
  `4dfc19f69a8ccb6f2ada55e49765419f76d03e2c`.
- [DoubleCherryGB](https://github.com/TimOelrichs/doublecherryGB-libretro/tree/03f58ca3dfb4b716f7e66a0e0467f9e85ef82abb):
  `03f58ca3dfb4b716f7e66a0e0467f9e85ef82abb` plus the recorded CMake patch.
  Its embedded libmobile is a separate compatibility target from the standalone
  library. Do not replace it silently with another revision.

Downloaded archive hashes:

```text
RGBDS v1.0.4 macOS archive, verified against GitHub release asset digest:
62d1669bbe3b2bf2cfc2e72076d3c8c00fc023a9cc15d458f0c723bd9d0312c4

BGB bgbw64.zip, locally recorded hash of official download:
38b97e4496ad85106f59c87a6b0386b7405fbebb3bccc90650279762bd10478c

Standard baseline ROM rebuilt using RGBDS 1.0.4:
314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e
```

The BGB URL is rolling. Its hash records this download, not a separately
published vendor checksum. The RGBDS default installation was not replaced.

## Rebuild the native dependencies

Use the existing pinned checkouts on this host. On a new host, clone with `gh`
and check out the revisions above before building. Run these from the worktree:

```sh
adapter_cache=/Volumes/Storage/Cache/mobileadapter
cmake -S "$adapter_cache/sources/libmobile" \
  -B "$adapter_cache/build/libmobile" \
  -DCMAKE_INSTALL_PREFIX="$adapter_cache/install" \
  -DLIBMOBILE_BUILD_SHARED=OFF -DLIBMOBILE_BUILD_STATIC=ON
cmake --build "$adapter_cache/build/libmobile" -j4
cmake --install "$adapter_cache/build/libmobile"

PKG_CONFIG_PATH="$adapter_cache/install/lib/pkgconfig" \
  cmake -S "$adapter_cache/sources/libmobile-bgb" \
  -B "$adapter_cache/build/libmobile-bgb" \
  -DCMAKE_INSTALL_PREFIX="$adapter_cache/install" \
  -DWITH_SYSTEM_LIBMOBILE=ON -DCMAKE_C_FLAGS=-D__unix__
cmake --build "$adapter_cache/build/libmobile-bgb" -j4
cmake --install "$adapter_cache/build/libmobile-bgb"
"$adapter_cache/install/bin/mobile" --help
```

The bridge's pinned source rejects Darwin because its POSIX branch is guarded by
`__unix__`, which Apple clang does not define. The build flag selects that existing
POSIX branch without modifying its source. Only compilation and CLI startup have
been verified. This bridge is optional historical preparation, not a current
development prerequisite; its exchange has not been tested.

DoubleCherryGB's pinned CMake list omits three required libretro-common C sources,
causing undefined symbols on macOS. The small recorded build-list patch adds
them without changing emulation behavior. Apply it once to a clean pinned source
checkout; the cached checkout already has it applied:

```sh
git -C "$adapter_cache/sources/doublecherry" apply \
  "$PWD/mobile_adapter/tools/patches/doublecherry-cmake.patch"
cmake -S "$adapter_cache/sources/doublecherry" \
  -B "$adapter_cache/build/doublecherry" \
  -DCMAKE_BUILD_TYPE=Release -DLIBMOBILE_BUILD_SHARED=OFF
cmake --build "$adapter_cache/build/doublecherry" -j4
```

Upstream compiler warnings remain; the link and runtime probe succeeded. The
patch belongs to the external emulator checkout, not Polished Crystal's engine.

## Repeat the headless boot probe

```sh
make -j4 RGBDS="$adapter_cache/rgbds-1.0.4/"
shasum -a 256 polishedcrystal-3.2.3.gbc

clang++ -std=c++17 -O2 -Wall -Wextra \
  -I "$adapter_cache/sources/doublecherry/libretro/DoubleCherryEngine" \
  mobile_adapter/tools/smoke_libretro.cpp -o "$adapter_cache/smoke-libretro"
mkdir -p "$adapter_cache/smoke/a"
"$adapter_cache/smoke-libretro" \
  "$adapter_cache/build/doublecherry/DoubleCherryGB_libretro.dylib" \
  "$PWD/polishedcrystal-3.2.3.gbc" \
  "$adapter_cache/smoke/a/boot.ppm" "$adapter_cache/smoke/a"
```

The frontend makes 1,200 `retro_run` calls, presses Start for calls 700 through
709 to skip the intro, and leaves the title running. It checks native 160x144
video, at least 90% of requested video callbacks, a nonuniform final image, and that the core
queries the enabled adapter option after loading. LCD-off transitions can omit
callbacks, so callbacks and emulated frame calls are distinct.

All three isolated processes passed: 1,198 video callbacks, 22 final colors,
zero exit status. The final image was also inspected and shows the Polished
Crystal title. Separate system/config directories avoid adapter EEPROM sharing.
These are three independent boot probes, not a connected three-player session.
PPM output and a PNG conversion are cached under `smoke/{a,b,c}/`.

## Repeat the native save and movement automation

Build the frontend as above, then run this explicit integration command with a
new output directory (existing directories are refused):

```sh
python3 mobile_adapter/tools/probe_emulator.py \
  --core "$adapter_cache/build/doublecherry/DoubleCherryGB_libretro.dylib" \
  --runner "$adapter_cache/smoke-libretro" \
  --output "$adapter_cache/automation-new-run"
```

The three processes execute 12,200, 3,500, and 4,200 `retro_run` calls. Each gets
an isolated system/save/config directory. Battery fixtures are generated from
scratch using the committed scripts in `tools/fixtures/`; no copyrighted ROM or
binary save is committed. The command requires the exact baseline ROM hash and
matching symbols. It runs normal game instructions; the host only imports the
ordinary SRAM save at boot and observes runtime memory without modifying it.

The frontend accepts `--frames`, `--input`, `--symbols`, `--state-out`,
`--save-in`, and `--save-out`. Save import requires the exact core SRAM size;
RTC trailers are rejected. Do not silently import a BGB file with an RTC trailer.
Current observer layouts are specific to the pinned core: its physical WRAM
allocation and memory descriptors were audited, and its erroneous generic
SYSTEM_RAM/VIDEO_RAM accessors are avoided. See the feasibility report.

The capability probe uses baseline-specific frame scripts and final semantic
assertions. It is not the full future acceptance harness. Add bounded state-driven
phases, intermediate render snapshots, real-time pacing, server transcripts,
concurrent clients, and disconnect/fault injection as multiplayer is developed.
Fast offline startup is useful, but accelerated emulation must not be used to
claim socket latency because the adapter uses host wall time. Separate emulator
processes can connect through their embedded libmobile to the same local Rust
TCP listener; they do not need a separate BGB bridge or RetroArch netplay.

## BGB status

The earlier audit downloaded official BGB 1.6.6 and built the native bridge.
BGB was not executed; Wine is absent and Intel macOS execution is unavailable.
The user has since rejected Rosetta and selected native DoubleCherryGB. No
Rosetta, Wine, VM, or container was installed. BGB testing is optional future
work on a compatible external host if explicitly requested. It does not block
this branch. Previous prospective Rosetta/Wine installation instructions have
been removed from this plan to reflect that decision.

## Minimal upstream changes

The preferred capacity experiment now fits a 12-byte ROM0 dispatch without
relocating any existing helpers. Results and limitations are in
[FEASIBILITY.md](FEASIBILITY.md) and `memory-banked-isr-probe.json`.
Production feature code is still absent. Keep the new module gated, centralize
feature code/data, and prove the disabled build's exact upstream hash. Execute
and measure the real banked ISR before relying on this layout.
