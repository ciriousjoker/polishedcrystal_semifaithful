# Development host readiness

Checked: 2026-10-10. Host: Apple Silicon, macOS 27.0.1 (26A434), ARM64.
Branch: `mobileadapter`. No production game source was changed in this setup.

## What works now

| Component | Installed or prepared | Verification |
| --- | --- | --- |
| Repository | Attached worktree on `mobileadapter` | Baseline source commit `8f3c7e9651e988d876e32a4c7ad9ef1bb76702a8` |
| RGBDS | Default 1.0.1; separate required 1.0.4 | Complete standard ROM rebuild with 1.0.4; exact official SHA-256 |
| Rust / Cargo | 1.85.0, native ARM64 | Temporary binary, `rustc --test`, and offline `cargo test` succeeded |
| C/C++ toolchain | Xcode clang 21, CMake, pkg-config, make | Built the library, bridge, core, and headless frontend |
| libmobile | Pinned native static library and headers | CMake build/install succeeded |
| libmobile-bgb | Pinned native ARM64 `mobile` executable | Build/install and `mobile --help` succeeded |
| DoubleCherryGB | Pinned native ARM64 libretro core, embedded libmobile | Three separate headless processes booted the baseline and reached the title screen |
| BGB | Official 1.6.6 x64 archive downloaded and extracted | PE executable inspected; not executed |
| Wine | Absent | No Wine executable or installed Wine/CrossOver/Whisky app found |
| macOS Rosetta | No working installation detected | `arch -x86_64 /usr/bin/true` fails with `Bad CPU type`; no installation receipt |

Native development, assembly/link checks, Rust host tests, and headless ROM
execution can proceed without Wine. BGB compatibility testing remains unavailable
until Wine and working macOS Rosetta exist. Neither a Windows VM nor a Linux
container is required for the intended Wine route.

The boot probe enables the adapter option but sends no Mobile Adapter commands
from the unchanged ROM. It proves native emulator availability, not ISP login,
TCP connectivity, network latency, or multiplayer behavior. There is no Rust
server crate or feature unit-test suite yet. The Rust tests were temporary
toolchain checks, not tests of future server behavior.

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
been verified; a BGB link exchange on Darwin remains a test gate.

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
video, at least 1,100 video callbacks, a nonuniform final image, and that the core
queries the enabled adapter option after loading. LCD-off transitions can omit
callbacks, so callbacks and emulated frame calls are distinct.

All three isolated processes passed: 1,198 video callbacks, 22 final colors,
zero exit status. The final image was also inspected and shows the Polished
Crystal title. Separate system/config directories avoid adapter EEPROM sharing.
These are three independent boot probes, not a connected three-player session.
PPM output and a PNG conversion are cached under `smoke/{a,b,c}/`.

This small frontend is the starting point for a native integration harness.
Later add battery `.sav` import/export, current-symbol state oracles, input
scripts, transcript capture, and fault injection. Do not reinterpret this smoke
as coverage of save persistence or interrupt timing. Normal host unit tests
will continue to run without starting an emulator.

## Optional BGB through Wine

[BGB's manual](https://bgb.bircd.org/manual.html) explicitly lists its x64 build
as tested with Wine on OS X. That establishes a supported route, not a successful
test on this particular macOS 27 host.

Wine is a compatibility layer installed as an app. A Wine prefix is an ordinary
directory containing a simulated C drive and registry. It does not require a
Windows installation, VM, Docker image, or Linux container. Each BGB process
still uses an ordinary local socket to the native `mobile` bridge.

For this Apple Silicon host, the currently packaged Intel Wine needs Rosetta.
[Apple documents Rosetta installation](https://support.apple.com/en-us/102527)
on macOS 27 and earlier:

```sh
softwareupdate --install-rosetta
```

This command was not executed. It asks the user to follow the installation and
license prompts. The presence of `/Library/Apple/usr/libexec/oah/RosettaLinux`
alone does not prove macOS Intel binaries can run. Verify afterwards with
`arch -x86_64 /usr/bin/true`. Apple has announced restricted Rosetta availability
from macOS 28, so native DoubleCherryGB is preferable for long-lived automation.

At the time of this check, [Homebrew's wine-stable cask](https://formulae.brew.sh/cask/wine-stable)
is disabled, dated 2026-09-01, with reason `fails_gatekeeper_check`. Its old install
command is therefore not a working setup recipe. The package maintainer offers
[manual Wine app bundles](https://github.com/Gcenx/macOS_Wine_builds). The stable
11.0_1 archive recorded by the cask is:

```text
https://github.com/Gcenx/macOS_Wine_builds/releases/download/11.0_1/wine-stable-11.0_1-osx64.tar.xz
SHA-256 b50dc50ec7f41d58b115a6b685d4d1315ba3c797bd3aa0f49213f2703cb82388
```

That archive was not downloaded or installed here. Manual setup would unpack
the verified app bundle, put `Wine Stable.app` in Applications, and complete the
normal macOS first-open approval if requested. Check package runtime dependencies
if Wine fails to launch; the cask lists GStreamer runtime. Gatekeeper acceptance
and macOS 27 compatibility remain unverified. Do not globally disable Gatekeeper.

Once Wine actually runs, use a project-specific prefix and the existing BGB:

```sh
adapter_wine='/Applications/Wine Stable.app/Contents/Resources/wine/bin/wine'
WINEPREFIX="$adapter_cache/wine-prefix" "$adapter_wine" --version
WINEPREFIX="$adapter_cache/wine-prefix" "$adapter_wine" \
  "$adapter_cache/bgb/bgb64.exe" "$PWD/polishedcrystal-3.2.3.gbc"
```

These are prospective commands, not commands executed successfully during this
audit. Before automated acceptance tests, verify BGB baseline boot, its headless
and demo CLI behavior, and an actual bridge listener exchange. Use separate
battery saves, adapter configs, and listener ports 8765/8766/8767 for instances
A/B/C. The future Rust TCP game server is separate from those BGB link ports.

## Minimal upstream changes

The preferred capacity experiment now fits a 12-byte ROM0 dispatch without
relocating any existing helpers. Results and limitations are in
[FEASIBILITY.md](FEASIBILITY.md) and `memory-banked-isr-probe.json`.
Production feature code is still absent. Keep the new module gated, centralize
feature code/data, and prove the disabled build's exact upstream hash. Execute
and measure the real banked ISR before relying on this layout.
