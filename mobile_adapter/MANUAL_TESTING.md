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
Enter for Start and left Shift for Select. In an idle overworld scene,
left Shift + Z joins the lobby. On Android hold B, then press Select while B
remains held. Release both afterwards. Join on both clients, then move one tile:
the prepared saves initially place both players at the same coordinates.
Select+B again leaves. Select+A opens status, players and connection controls;
on Mac that is left Shift + X. Select+B+A retains the short echo diagnostic.
Close ordinary dialogs before connecting.

The terminal distinguishes frontend startup from actual network membership:

```text
ANDROID START confirmed: core initialized; fresh state loaded. Select+B joins the three-player lobby.
CLIENT_CONNECTED session=1 peer=192.168.2.23:51000 client=android
LOBBY_JOIN session=1 lobby=1 player=1 generation=1
LOBBY_JOIN session=2 lobby=1 player=2 generation=1
LOBBY_LEAVE session=1 player=1
CLIENT_DISCONNECTED session=1 peer=192.168.2.23:51000 reason=closed client=android
```

Numbers and source ports vary. CLIENT_CONNECTED confirms a real TCP socket;
LOBBY_JOIN confirms compatibility and player membership. LOBBY_OFFLINE records
a lost connection, and a later LOBBY_JOIN with the same player and a higher
generation confirms token resume. Explicit leave removes identity, so rejoining
gets a new player ID. Mac/Android labels are inferred from source IPs. They are
not authentication. Errors appear as SESSION_ERROR or LOBBY_REJECTED.

Three players can share lobby 1. They see the approved Chris avatars on the same
map with compatible physical block layouts. Remote players do not block movement
or run NPC scripts. World progress, items, puzzles and encounters remain local.
Scripts, menus and Snowtop's camera temporarily pause rendering. Large Gyarados
and Exeggutor sprites, or unavoidable required-NPC sprite pressure, also pause
avatars. Select+A reports the reason and keeps the player roster accessible in
a safe idle scene. Leaving the protected situation restores avatars.

The Mobile Adapter popup describes the emulator peripheral. Its initialization
log can appear with emulation disabled, so the popup alone does not prove a Rust
server connection. Check LOBBY_JOIN and the in-game online/player status. Calling,
interaction menus, battles, trading and gifts follow in milestones 3 to 5.

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

The script force-stops RetroArch before updating, stages the files, and launches
`RetroActivityFuture` with explicit `ROM`, `LIBRETRO` and `CONFIGFILE` extras.
These are the activity and string extras used by ES-DE's
[RetroArch discovery rules](https://gitlab.com/es-de/emulationstation-de/-/blob/master/resources/systems/android/es_find_rules.xml)
and [Android launch configuration](https://gitlab.com/es-de/emulationstation-de/-/blob/master/resources/systems/android/es_systems.xml).
It stops the app again immediately before the one game launch, clears its previous
task and waits for the requested activity. Normal launches never open an installer.
The server is listening before either playable client can test connectivity.
Its configuration auto-loads the newly generated state. The script checks a fresh
Android log for adapter initialization and successful state loading, reporting
failure in the terminal if this cannot be confirmed. After the command, the
game is open on the phone; no launcher tap or manual Continue is required.
The isolated configuration sets `global_core_options = "true"` and
`game_specific_options = "false"`, making its explicit `core_options_path`
authoritative. Without the first setting, an inherited per-core preference can
silently load `DoubleCherryGB/DoubleCherryGB.opt` with the adapter disabled.
The user's global RetroArch configuration and per-core options are preserved.
For a separately created ROM shortcut, always select the custom installed core
`/data/data/<package>/cores/DoubleCherryGB_libretro.so`. The script also writes
per-game overrides/options and a separate playlist fixing that core selection.
Your shortcut can keep pointing at the stable ROM path after every update.

The pinned core is already installed on the configured Pixel. Its SHA-256 and
app installation identity are recorded in `.core-install.json` in the test
folder. If this core changes or RetroArch is replaced, the command requests an
explicit `--reinstall-core` run. That opt-in installation uses RetroArch's core
sideload activity and needs an extra brief start. If you manually remove or
replace this core, use the same flag once. No automatic installer restart is
part of the normal command.

For diagnosis, the exact generated direct-start ADB command is in the host
cache's `dev-session/android/launch-adb.txt`. Android frontend logs are in the
Download folder's `retroarch.log`; Mac preparation and frontend logs are in
`dev-session/mac`. Core debug logging is enabled in these isolated profiles,
including libmobile's TCP connect, outgoing request and incoming reply bytes.
The script accepts `--server-ip 192.168.x.y` or
`--interface en6` to override automatic LAN detection. A DHCP change requires
rerunning the command so both ROM copies get the current address. Port 17423
does not change.

## Shutdown and verification

Keep the terminal open while testing. Ctrl+C stops the server and this Mac game
instance. Stop the previous session before rerunning the command or the automatic
integration suite. The command refuses to kill another process using its port.
`--mac-only` runs without Android; `--prepare-only` prepares artifacts without
starting the games or server. Only explicitly requesting `--reinstall-core`
opens the Android installer in preparation mode.

Verified on 2026-10-10 with native Apple M2 macOS and a real Pixel 9 Pro using
ARM64 RetroArch 1.19.1. Both platforms booted their regenerated states. Separate
native Mac and Android game processes completed the actual adapter TCP echo over
the Mac's LAN address, accepted the reply, released serial ownership without an
error and completed a subsequent tile step. Repeated complete ADB deployment runs
also verified updating the same stable paths and backing up the previous save.
The pinned Android core was built with NDK 29.0.14206865, API 23, static C++ runtime
and embedded libmobile. It needs only Android's system libc/libm/libdl libraries.
The logging follow-up verified both actual source IPs in the new server logs,
successful reply acceptance and movement in both ROMs. The Pixel's one-intent
start also confirmed adapter initialization and fresh state loading. Host cache
`dev-session/log-proof` contains the server log and symbol results.

A subsequent real RetroArch GUI check reproduced the Android adapter being Off
despite the generated options file specifying On. Enforcing the isolated options
path fixed the direct-intent launch. After one force-stop and one game intent,
the real Pixel frontend loaded its fresh state and completed TCP connect, the
`PC` transport-v1 `TEST` request, its matching echo reply and clean session teardown
against the running Rust server at `192.168.2.25:17423`. This was tested through
the running GUI's inputs, not the headless runner. The raw frontend trace is in
`dev-session/gui-proof/android-retroarch.log` in the host cache. Eight host-only
contracts passed, including inherited per-core options and the single-intent start.

Host-only deployment checks:

```sh
python3 -m unittest discover -s mobile_adapter/tools -p test_dev_session.py
```

Milestone 2 adds the three-player lobby and live overworld avatars. The native
three-core acceptance suite checks actual movement, map transitions, reconnects,
rendering pressure and visible latency. Phone interactions, battles and transfers
remain milestones 3 to 5. A server ECHO log alone is not gameplay acceptance.


On 2026-10-11, the M2 ROM also ran simultaneously in the actual Mac RetroArch
GUI and physical Pixel GUI. Both joined lobby 1 with distinct server-assigned
IDs, displayed the other avatar and accepted normal arrows/touch movement.
The GUI automation used a disposable Mac F2 mapping to send the Select+B chord;
the shipped launcher profile uses left Shift+Z. GUI observer config was isolated
from normal launch profiles. The normal Mac profile keeps running when the
terminal is focused, so switching apps does not expire the presence lease.
Proof is retained in the host cache as m2-gui-observer-server.log,
m2-gui-observer-input.json and m2-gui-phone3.png.

The same physical GUI check was repeated with the final 16-byte parser budget
and final compiler ROM `725d94ee2e8adec0817a9398e9fa3a1056908943e429ccbdaa335384f3a7e3e8`.
After an unlocked Pixel cold start, both real frontends joined and displayed
each other. The LAN endpoint copy is
`73203c64a9bbb6d1f44692c70b61c55947cc6cdab3d556d510ee2887ecd7900f`.
Final proof: `m2-final16-gui-server.log`, `m2-final16-gui-input.json` and
`m2-final16-phone-visible.png` in the host cache. The full automatic acceptance,
including the separate 300% speed test, is recorded in
[research/MILESTONE_2.md](research/MILESTONE_2.md).
