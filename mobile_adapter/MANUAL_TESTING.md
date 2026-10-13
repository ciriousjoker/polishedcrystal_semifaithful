# One-command Mac and Android development session

On the configured development host, run this from any directory:

```sh
python3 /Volumes/Storage/Cache/codex_worktrees/mobileadapter/polishedcrystal/mobile_adapter/tools/dev_session.py
```

At startup, choose the ordinary starting scenario (bedroom, crowded Game Corner,
or Goldenrod City), then the number of **Mac** game windows (1..4, default 2).
If an authorized phone is detected through ADB, the launcher asks whether to
update and open Android too. Android adds one player, leaving at most three Mac
slots. With no phone, the Android question is omitted.

Figures cycle Chris, Kris, Crys, Beta. `--mac-avatar` chooses the first one and
`--android-avatar` optionally overrides the next unused phone figure. All session
batteries share the same progress, name, party, clock and initial position. Only
figure, trainer ID and ordinary save checksums differ. Different trainer IDs are
a fixture convenience; duplicate IDs must work too. Fresh emulator snapshots
are regenerated from these batteries with the current ROM and exact device core.
Every run gets independent timestamped Mac profiles.

To skip prompts, use `--scenario game-corner --instances 2 --mac-only`, or replace
`--mac-only` with `--android` to include an authorized device. The portable source
saves are committed in [tests/scenarios](tests/scenarios/README.md). Deployed
Android filenames stay stable. Each update resets the chosen test scenario.

The command builds the current MBC30 feature ROM, Rust server/Bridge and Flutter
shell. It starts the local Worker/lobby server on loopback port **8787**, opens
one shared Flutter Bridge, and launches all selected RetroArch windows using the
custom checksum-pinned MBC30 DoubleCherryGB core. Android's matching custom core
is installed if its installation stamp changed; ordinary launches skip the
installer. `--reinstall-core` forces reinstallation.
The Bridge binds the detected LAN address on fixed TCP port **17423** and prints
`BRIDGE tcp://<address>:17423`. Both ROM copies use this same endpoint. The ROM's
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
ANDROID START confirmed: core initialized; fresh state loaded. Select+B joins the four-player lobby.
BRIDGE_EMULATOR_CONNECTED stream=1 peer=192.168.2.23:51000
BRIDGE_JOINED stream=1 player=1 generation=1
BRIDGE_JOINED stream=2 player=2 generation=1
BRIDGE_DISCONNECTED stream=1 result=Ok(())
```

Numbers and source ports vary. BRIDGE_EMULATOR_CONNECTED confirms a real TCP
socket; BRIDGE_JOINED confirms validated player membership. BRIDGE_UPSTREAM
confirms the shared upstream WebSocket in hosted mode. BRIDGE_UPSTREAM_LOST and
BRIDGE_PLAYER_ERROR expose failures. Disconnecting one local stream leaves its
siblings active. These logs identify transport sessions, not authenticated people.

Four players can share one lobby, including three remote avatars on the same map.
They use each player's actual selected figure, movement sprite and hardware colors.
Remote players do not block movement or run NPC scripts. World progress, items,
puzzles and encounters remain local. Remote players use private records outside
the native object pool. NPCs remain simulated and interactable; hardware pressure
can temporarily omit complete figures from drawing, without removing their state.

The Start menu preserves complete static native avatars while other clients keep
moving independently. Closing it refreshes positions. Protected camera/cutscene
and resource-critical drawing may temporarily defer avatars; ordinary exploration
must show all three peers when they share the viewport. Select+A keeps the roster
and rendering status available in a safe idle scene. Detailed finite coverage
boundaries and remaining acceptance failures are recorded in the external
[project run](/Volumes/Storage/tmp/mobileadapter/README.md).

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

The update intentionally resets both test clients to the selected scenario's ordinary saves.
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
cache's `dev-session/runs/<run>/android/launch-adb.txt`. Android frontend logs are in the
Download folder's `retroarch.log`; Mac preparation and frontend logs are in
`dev-session/runs/<run>/mac-<number>`. Core debug logging is enabled in these isolated profiles,
including libmobile's TCP connect, outgoing request and incoming reply bytes.
The script accepts `--server-ip 192.168.x.y` or
`--interface en6` to override automatic LAN detection. A DHCP change requires
rerunning the command so both ROM copies get the current address. Port 17423
does not change.

## Shutdown and verification

Keep the terminal open to see server logs and Bridge connection/join/disconnect
logs. **Ctrl+C stops only the local server and frees its port 8787.** It leaves
the Flutter Bridge and emulator windows running. The Bridge keeps TCP port 17423
until you stop/close its window; it waits to reconnect after the server stops.
Close the emulator windows yourself when finished.

If a port is occupied on the next launch, the command lists its listener PID/name
and asks before stopping it with SIGTERM. The default is No; noninteractive runs
never kill an existing listener. Each run's profiles are separate, so old open
emulators cannot overwrite a newly prepared Mac instance's save.
`--prepare-only` prepares the artifacts without opening games or listeners.
`--mode local` uses only the Bridge's embedded lobby; `--mode remote --upstream
wss://your-origin.example` uses an external server. Ctrl+C in those modes ends
terminal monitoring and leaves the GUI apps running.

Historical Mac/Pixel verification logs and build hashes have moved to the
external project archive. They do not validate the current MBC30 phone build;
physical-device validation remains deferred.

Host-only launcher checks:

```sh
python3 -m unittest discover -s mobile_adapter/tools -p test_dev_session.py
```

The current headless four-client acceptance commands and observation boundaries
are documented in [tests/README.md](tests/README.md). A server ECHO log alone is
not evidence of four-player overworld acceptance.

## Shared Bridge and hosted simulation

The default command uses one Rust Bridge with an embedded local owner. Mac and
phone emulator streams belong to the same household session; no cloud connection
is required. To simulate the public router/lobby split locally:

```sh
python3 mobile_adapter/tools/dev_session.py --mode hosted --test-one-way-delay-ms 50
```

This starts a local hosted router and lobby owner, then the Bridge's shared
WebSocket. The explicit delay adds 50ms in each direction. Public Cloudflare
account deployment and the phone's native Flutter Bridge packaging are deferred.
