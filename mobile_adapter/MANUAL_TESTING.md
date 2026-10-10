# One-command Mac and Android development session

On the configured development host, run this from any directory:

```sh
python3 /Volumes/Storage/Cache/codex_worktrees/mobileadapter/polishedcrystal/mobile_adapter/tools/dev_session.py
```

The command builds the current feature ROM and Rust server, detects the Mac's
LAN IPv4 address, prepares an isolated Mac game, updates the connected Android
phone through ADB, and starts both RetroArch instances with DoubleCherryGB.
The server binds the detected LAN address on fixed TCP port **17423** and prints
`SERVER tcp://<address>:17423`. Both ROM copies use this same endpoint. The ROM's
small feature-owned endpoint table and cartridge checksum are adapted in a copy
of the compiler output; the standard build and existing game routines stay intact.
The phone and Mac must be able to reach each other through the local network.
No RetroArch netplay, DNS or P2P telephone relay setting points to this server.

The Mac instance starts at 5% emulator volume, using -26.0206 dB. This does not
change macOS system volume. Keyboard controls are arrows, X for A, Z for B,
Enter for Start and right Shift for Select. In an idle overworld scene,
right Shift + Z runs the milestone 1 transport proof. The terminal should show
`ECHO session=... sequence=... bytes=4`. On Android use the corresponding
Select and B touch controls together. Release before repeating the test.

## Android files and direct start

ADB must list one authorized device. Enable USB debugging and accept the Mac's
debugging connection once. ARM64 RetroArch must be installed and have been opened
once so its configuration, storage access and touch controls are initialized.
The current tested device uses package `com.retroarch`; `com.retroarch.aarch64`
is also supported. Use `--serial SERIAL` or `--package PACKAGE` if selection is
ambiguous. Nothing installs a new Android app.

The stable folder is Android's **Downloads/polishedcrystal_mobile_adapter**, whose
filesystem path is:

```text
/storage/emulated/0/Download/polishedcrystal_mobile_adapter/
  polishedcrystal-mobile.gbc
  polishedcrystal-mobile.sav
  polishedcrystal-mobile.srm
  polishedcrystal-mobile.state.auto
  retroarch.cfg
  core-options.cfg
  Polished Crystal Mobile.lpl
```

`.sav` is a real 32 KiB in-game battery save. `.srm` contains the same initial
battery data under RetroArch's required filename. The auto-state skips the title
and Continue menus. Every update generates a new auto-state by booting the current
ROM with the battery save and ordinary menu inputs, checking idle overworld state,
and serializing through the current core. The Android state is generated on
Android using the Android core, not copied from the Mac. Old CPU/WRAM snapshots
are never reused with a new build. The normal acceptance suite continues to boot
from battery saves without snapshot input.

The update intentionally resets both test clients to the prepared bedroom save.
Previous `.srm` files are backed up in the host cache's `dev-session/backups`.
Only this workflow's named files and game-specific settings are replaced. Other
files in the Download folder and other RetroArch games are preserved.

The script force-stops RetroArch before updating, installs the pinned Android core
through RetroArch's own core sideload activity, stages the files, and launches
`RetroActivityFuture` with explicit `ROM`, `LIBRETRO` and `CONFIGFILE` extras.
Its configuration auto-loads the newly generated state. After the command, the
game is open on the phone; no launcher tap or manual Continue is required.
For a separately created ROM shortcut, always select the custom installed core
`/data/data/<package>/cores/DoubleCherryGB_libretro.so`. The script also writes
per-game overrides/options and a separate playlist fixing that core selection.
Your shortcut can keep pointing at the stable ROM path after every update.

For diagnosis, the exact generated direct-start ADB command is in the host
cache's `dev-session/android/launch-adb.txt`. Android frontend logs are in the
Download folder's `retroarch.log`; Mac preparation and frontend logs are in
`dev-session/mac`. The script accepts `--server-ip 192.168.x.y` or
`--interface en6` to override automatic LAN detection. A DHCP change requires
rerunning the command so both ROM copies get the current address. Port 17423
does not change.

## Shutdown and verification

Keep the terminal open while testing. Ctrl+C stops the server and this Mac game
instance. Stop the previous session before rerunning the command or the automatic
integration suite. The command refuses to kill another process using its port.
`--mac-only` runs without Android; `--prepare-only` prepares artifacts without
starting the Mac GUI or server.

Verified on 2026-10-10 with native Apple M2 macOS and a real Pixel 9 Pro using
ARM64 RetroArch 1.19.1. Both platforms booted their regenerated states. Separate
native Mac and Android game processes completed the actual adapter TCP echo over
the Mac's LAN address, accepted the reply, released serial ownership without an
error and completed a subsequent tile step. Repeated complete ADB deployment runs
also verified updating the same stable paths and backing up the previous save.
The pinned Android core was built with NDK 29.0.14206865, API 23, static C++ runtime
and embedded libmobile. It needs only Android's system libc/libm/libdl libraries.

Host-only deployment checks:

```sh
python3 -m unittest discover -s mobile_adapter/tools -p test_dev_session.py
```

This is still milestone 1: the playable clients can prove connectivity, but
remote overworld avatars, lobbies, phone interactions, battles and transfers
are subsequent milestones. A server ECHO log alone is not a complete gameplay
acceptance condition.
