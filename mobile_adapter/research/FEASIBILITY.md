# Follow-up feasibility investigation

Investigated: 2026-10-10.
Baseline: `8f3c7e9651e988d876e32a4c7ad9ef1bb76702a8`, standard build.
Scope: capacity experiments, all-map static audit, latency design, and integration
test design. No production ROM feature or Rust server is implemented here.

## Findings and decisions

- A 12-byte resident dispatch fits without moving existing helpers or changing
  the cartridge mapper. A simultaneous initial mobile budget fits. Prefer this
  fork-friendly design; interrupt runtime and timing safety remain untested.
- Two additional ordinary avatars look practical in normal overworld scenes.
  Neither one nor two free runtime slots are guaranteed throughout every map,
  story state, effect, and movement history. Use a capacity-aware allocator from
  the first playable experiment, with explicitly replaceable decoration.
- Live movement with a delay of a few tenths of a second is a plausible target.
  Three-second movement delay is not imposed by the adapter protocol. Measure
  the complete emulated path before promising an achieved latency.
- Boot each current ROM from ordinary battery-save fixtures. Resolve observer
  addresses from its own symbols. Check state, timing, and intermediate rendered
  frames; a final screenshot or a successful process exit alone is insufficient.

## Memory: measured capacity experiments

The baseline contains only 18 free ROM0 bytes, 13,943 free ROMX bytes, and 300
free WRAM0 bytes. ROMX bank `$7f` has a contiguous 13,926-byte tail. WRAMX bank 2
has a contiguous 2,837-byte tail at `$d4eb..$dfff`. These are linker holes,
not idle UNION members that can be borrowed while networking is active.

### Preferred experiment: small dispatch and banked handler

The user prioritizes minimal changes to upstream game code. No helper was moved
in the actual game sources during either experiment. The earlier relocation
occurred only in disposable temporary assembly files.

`tools/probe_memory.py --strategy banked-isr` demonstrates an alternative:
after the existing Serial interrupt's register saves, add a WRAM0 ownership
check, branch to the existing serial path when inactive, and otherwise `farcall`
an adapter handler in ROMX. Return through the existing register restores and
`reti`. The added dispatch occupies exactly 12 ROM0 bytes, leaving six free.
The original helper implementations and serial logic remain in place. A
one-byte banked `ret` is only a placement placeholder, not a functioning driver.

The same simultaneous data/ROMX reservations still link:

| Region | Reservation | Remaining free space |
| --- | ---: | ---: |
| ROM0 | 12-byte dispatch, no extra resident placeholder | 6 bytes |
| ROMX bank `$7f` | 8,192 bytes plus one-byte handler placeholder | 5,750 bytes across ROMX |
| WRAM0 | 96 bytes | 204 bytes |
| WRAMX bank 2 | 1,536 bytes | 1,301 bytes in bank 2 |
| HRAM | 1 byte | 4 bytes |
| SRAM bank 1 | 128 bytes | 1,244 bytes in bank 1 |

Exact results are in `memory-banked-isr-probe.json`, reproduced with RGBDS 1.0.4
and the exact baseline ROM hash. This removes helper relocation as an initial
requirement. It does not prove the complete driver fits or executes correctly.
The existing printer interrupt already uses a banked `farcall`, which is a
useful integration precedent, not a serial throughput proof.

Keep production dispatch under a feature flag and its implementation under
`mobile_adapter/`. Add only narrow includes/hooks to existing source and build
files. A disabled feature build must reproduce the upstream ROM hash. Maintain
an explicit hook list and compile-time budgets to make upstream changes visible.
Foreground protocol work belongs in ROMX; the interrupt must perform bounded
byte handling, preserve ROM/WRAM banks and registers, and avoid waits or `ei`.
Measure banked dispatch cycles, stack headroom, maximum serial rate, and effects
on VBlank/audio before accepting this strategy. Six spare bytes also mean future
upstream ROM0 growth could break the feature build; fail explicitly and revisit
the driver rather than silently moving maintainer code.

### Comparison experiment: helper relocation

`tools/probe_memory.py` reads an already built baseline without editing its
source or objects. With `--strategy helper-relocation`, it creates temporary assembly files and moves the implementations
from these files into ROMX bank `$7f`, and preserves their global entry points
in ROM0 using `farcall` and `ret` trampolines:

- `home/item.asm`
- `home/print_bcd.asm`
- `home/restore_music.asm`

The banked implementations total 354 bytes; the replacement entry points cost
90 bytes. ROM0 free space therefore rises from 18 to 282 bytes. The existing
far-call machinery preserves argument registers, returned registers/flags, and
the caller's ROM bank. Nested calls use the ordinary existing machinery.

A second link adds inert placeholders for all of the following simultaneously:

| Region | Reservation | Remaining free space |
| --- | ---: | ---: |
| ROM0 | 256 bytes | 26 bytes |
| ROMX bank `$7f` | 8,192 bytes | 5,397 bytes across ROMX |
| WRAM0 | 96 bytes | 204 bytes |
| WRAMX bank 2 | 1,536 bytes | 1,301 bytes in bank 2 |
| HRAM | 1 byte | 4 bytes |
| SRAM bank 1 | 128 bytes | 1,244 bytes in bank 1 |

Both variants assemble and link successfully. The experimental ROMs contain
capacity placeholders, are not playable implementations, and are discarded.
Exact results, baseline hash, and assembler version are in `memory-probe.json`.
This is a placement proof for an illustrative budget, not proof that every
planned feature fits within 8 KiB or that the relocated code executes correctly.

The intended allocation is a small serial dispatch in ROM0, a handler in ROMX,
and ownership state in WRAM0, plus bounded packet buffers, reassembly, and two peer records in the
dedicated bank-2 tail. A 1,536-byte budget can illustrate two 272-byte adapter
buffers, two 64-byte peer records, and 864 remaining bytes for framing/session
state and queues. These are ceilings to refine against an implemented protocol.
Cap payloads and queues rather than assuming an entire Pokemon party fits in
one network buffer. Never retain networking data in `wLinkData` or other menu,
battle, or graphics UNION storage.

The bank-2 tail is outside Sound Stack, Pic Animations RAM, Sprites Backup, and
Used Storage. A source search found the whole-bank wipe in `engine/init.asm:
ClearWRAM`, which is expected during initialization. Before adopting the layout,
test menu/battle/music transitions, reset/reconnect initialization, ISR bank
save/restore, and any future bulk operation against the dedicated reservation.
Pin the section's bank and assert its bounds in the feature build.

The 128-byte SRAM example fits after existing data in bank 1's `$baa4..$bfff`
tail, outside the current main save and Pokemon database regions. It only proves
physical space. A transaction journal still requires its own version, checksum,
redundant-record/recovery policy, and coordination with normal and backup saves.
It is not approved as a complete gift/trade persistence implementation.

Do not relocate arbitrary `home/` files wholesale. `home/names.asm:GetName`
switches ROM banks inside its own execution; `home/pokemon.asm:GetPokedexNumber`
uses `StackCallInBankA`; trainer helpers also switch banks or expose implicit-bank
script pointers. Resident bank-switching machinery and interrupt paths must stay
valid. The three experiment candidates still need register/bank/timing regression
tests, especially BCD output with text delays and music backup/restore. Extra
far-call stack depth and execution cost must be measured. Keep relocation as a
fallback requiring a concrete justification, and prefer the small dispatch
experiment above. Do not assume a larger MBC3 ROM is an easy fix.

## Maps: every assembled map audited

`tools/audit_map_slots.py` reads the assembled ROM and matching symbol file.
It follows all 611 entries in `data/maps/attributes.asm`, including macro-generated
object records, rather than counting only textual `object_event` lines.
Per-map results are in `map-slots.csv`; aggregate results and limitations are in
`map-slots-summary.json`.

There are two separate limits:

- 21 map records including the player, hence 20 nonplayer records. These describe
  potential NPCs, Pokemon, trees, rocks, item balls, and decoration across a map.
- 13 runtime structures including the player, hence 12 nonplayer structures of
  34 bytes. Only allocated objects occupy these, but temporary effects also use
  this pool. A spare map record does not imply a spare runtime structure.

The highest declared map count is 19 on Lake of Rage, followed by Route 32 with
18, Cerulean Cape and Route 39 with 17. Every map has at least one unused static
record. Lake of Rage has only one, so appending two fake remote map events is not
a general solution. Keep remote identity/bindings in separate feature data.

The conservative count of *all* declared objects on a map gives:

| Required spare runtime structures | Maps whose total declared count fits | Remaining maps needing visibility/state analysis |
| --- | ---: | ---: |
| 1, for one remote avatar | 542 / 611 | 69 |
| 2, for two remote avatars | 528 / 611 | 83 |
| 3, for two remotes and one effect reserve | 519 / 611 | 92 |

These counts exclude additional effects and script-created objects. They are
count-based bounds for map records, not guarantees over complete gameplay.
The remaining maps are not necessarily full: most objects are offscreen or hidden.

`InitializeVisibleSprites` initially considers a 12 by 11 tile rectangle, wider
than the LCD's 10 by 9 background-tile pairs. The tool reproduces stored-coordinate
offsets and time masks, tests rectangle membership at every occupancy boundary,
and deliberately ignores story flags. Its initial-position diagnostic has at
least one spare structure on 610 maps, two on 608, and three on 607. An independent
exhaustive evaluation of the engine's INC/SUB/carry/CP rectangle agreed for all
611 maps over 1,078,304 tile/time positions. This verifies the geometry calculation,
not collision reachability, callbacks, or executed gameplay.

The raw initial-window outliers and source inspection are:

| Map | Raw simultaneous demand ignoring story flags | Source-level explanation |
| --- | ---: | --- |
| Route36NationalParkGate | 12 | Ten contestants and two officers are declared. Contest selection hides five contestants; callbacks switch the officers. A normal results scene has five contestants and one officer. |
| BattleTower2F | 11 | Six special trainers and their six generic replacements are alternatives, plus Pikachu. Quest completion sets the departed companion event and clears the Battle Tower companion event. Normal completed quest states fit well below this raw demand. |
| CinnabarLab | 11 | Four substitute player avatars share one location; only the selected gender/avatar appears. That alone reduces the raw demand to at most eight. Scripts also switch scientists and Mewtwo stages. Protect this cutscene. |
| TinTower1F | 10 | Two alternative trios of sages are event-controlled. Only one trio normally appears alongside the three beasts and Eusine, at most seven from these records. |

The random-movement envelope additionally flags Azalea Town at demand 10. It
includes the engine's enlarged walking radius and deliberately ignores collision
and story exclusions. It does not simulate scripted movement.

### Why the initial-window figures are not a universal guarantee

`_CheckObjectStillVisible` keeps an object if its current *or initial* coordinates
are in range, and never deletes `WONT_DELETE` objects solely for being offscreen.
The extended audit therefore also counts current-time retained objects as
potentially occupying slots anywhere. With movement envelopes, this flags eleven
maps at demand ten or more. Examples beyond the initial outliers are Cianwood
City, Route 39, Ruins of Alph Outside, Azalea Gym, and:

- **SnowtopMountainOutside:** eleven evening/night window decorations plus a
  substitute player record have `WONT_DELETE`. The initial window sees at most
  nine, but panning/history can retain more. The special camera view needs an
  explicit policy; it must not broadcast camera movement as ordinary avatar
  movement or blindly evict the substitute player.
- **CinnabarVolcano1F:** four strength boulders and seven smash rocks can retain
  offscreen structures. Together with nearby trainers, conservative demand
  reaches thirteen, exceeding the existing twelve-slot pool even before peers.
  Event flags, reachability, and the actual path can reduce that demand, but this
  is a concrete reason to inspect allocation history rather than screenshots.

These are conservative demand estimates, not a claim that the game allocates
thirteen structures or that every retained object can legally coexist. Runtime
flags, story states, camera mode, effects, and transitions need integration cases.

### Replaceable objects and allocation thresholds

The proposed replacement mechanism is realistic and should be planned from the
first two-player implementation. Use a feature-owned table keyed by map ID and
stable map-object ID with an explicit replaceability priority. Do not consume a
new bit in already-used object flags or move map-object IDs when a peer appears.

Allocate only for peers actually visible in the same compatible map instance.
An online player in another map must not hide local NPCs. Prefer free structures,
then eligible offscreen decoration, then explicitly replaceable idle decoration
in view. Suppress automatic respawn with a separate temporary mobile mask and
restore the local object when the demand disappears. A lonely/offline scene
keeps all normal NPCs. Avoid repeated eviction/spawn flicker at the viewport edge.

Do not use `Script_disappear`: it calls `ApplyEventActionAppearDisappear` and
changes the object's event flag. `DeleteMapObject` frees the runtime binding
without this story mutation, but blindly calling it still loses runtime state.
Preserve reconstructible position/state, clean palette/follower/interaction
references, and keep a mask that ordinary spawn paths consult. Strength boulders
can have moved from their original coordinates, so a generic offscreen eviction
must preserve their current state or leave them protected. Protect plot actors,
active trainers, shops, puzzle objects, followers, and active script targets.

Using the raw runtime pool, with `N` occupied ordinary/effect structures and `R`
visible remotes, the requirement is `N + R <= 12`. Thus:

- One remote requires replacement when existing demand exceeds eleven.
- Two remotes require replacement when existing demand exceeds ten.
- Reserving one additional effect structure lowers those thresholds to ten and
  nine respectively. One effect reserve is an initial policy, not a proof that
  every animation needs at most one temporary structure.

If no permitted object can be reclaimed, defer the avatar and keep that player
available through the roster. A mandatory visible-avatar guarantee on every map
would need a deeper renderer/resource redesign or explicit map-specific changes.
Ordinary scenes look feasible for two and three total players, but the audit
does not justify removing the fallback even for two players.

### Graphics limits beyond runtime structures

`GetSpriteVTile` derives a 12-tile range from the runtime structure index;
indices 8 onward use VRAM bank 1. Big Gyarados, sailboat graphics, and Alolan
Exeggutor use the last structure's tile range independently of their actual
runtime index. Do not reserve slots 11 and 12 for peers globally. Avoid the
special shared tile range when those graphics are present, including tiny window
graphics using `SPRITE_SAILBOAT` on Snowtop Mountain.

The hardware has forty OAM sprites and a ten-sprite scanline limit. The ordinary
walking facings here use four 8 by 8 sprites per avatar, two per scanline.
Three total ordinary avatars consume twelve OAM sprites and six on a shared
scanline before nearby NPCs/effects. They fit alone; crowded scenes can still
lose sprite rows even with free structures. Dynamic object palettes are another
independent budget. Cover dense horizontal lineups, large sprites, effects,
palette changes, and late object-slot allocation in rendering tests.

## Latency: plausible live movement, still to measure

`StepVectors` uses sixteen updates for ordinary walking, eight for running, and
four for biking. At approximately 59.7 frames per second these are 268, 134,
and 67 ms per tile, before special animation states. Sending a position only
after a step ends adds a visible delay even on localhost. Publish step starts,
direction, duration/progress, and correction snapshots during movement instead.

Raise the first experiment's target to twenty presence exchanges per second
(50 ms intervals), subject to serial/frame-cost measurements. Each client keeps
one TCP connection regardless of whether it sees one or two peers. Compact
snapshots at that rate need only a few KiB/s per client for three players.
The documented fast serial clock offers enough nominal wire bandwidth; it does
not establish the achieved ISR, command, bridge, or network throughput.

The approximate latency budget is:

```text
sender service wait
  + sender serial/adapter/bridge transfer
  + sender-to-server network delay
  + server processing
  + server-to-recipient network delay
  + recipient service wait and transfer
  + render-frame wait
  + short interpolation delay
```

At twenty exchanges per second, the two service waits contribute about 50 ms
on average and up to 100 ms combined if the schedule is actually achieved.
Add up to roughly 17 ms for a render frame and a small 25-50 ms interpolation
buffer, plus measured transfer and network costs. An initial engineering target
is local visible movement in 50-150 ms most of the time, with p95 below 200 ms;
for clients each seeing approximately 100 ms RTT to the same server, target
p95 below 300 ms. These are hypotheses and acceptance goals, not results.

Avoid these artificial multi-second delays:

- One byte per game frame gives about sixty bytes/s: a 128-byte transfer already
  takes over two seconds. Continue transfers asynchronously within/across frames.
- libmobile `command_transfer` can wait one second on an Internet transfer with
  no outgoing or incoming data. Use a nonempty application `POLL` request.
- Adapter serial inactivity of about three seconds causes a disconnect, rather
  than imposing a three-second movement update interval. Keep servicing it
  through menus and long animations or explicitly suspend/reconnect presence.
- Unbounded queues replay old movement. Coalesce unsent movement, bound parser
  work, and discard stale map epochs/sequences. Reliable actions retain their
  own ordered queue. TCP packet loss can still cause head-of-line delay.

Render every game frame from recent snapshots. Interpolate briefly, allow only
short bounded prediction, and stop predicting on a stale stream. Local movement
never waits for a server acknowledgement. Measure input/step onset in client A
to the first corresponding rendered pixels in client B, not merely socket RTT.
Run two and three clients with walking, running, biking, warps, NPC-heavy maps,
menus, jitter, disconnects, and explicitly labeled one-way/round-trip delay.
Keep live network tests at real speed; uncontrolled fast-forward invalidates
wall-clock polling and timeout measurements.

## Integration tests: battery saves, symbols, and explicit oracles

An ordinary in-game save produces battery SRAM, commonly `.sav`. An emulator
save state stores CPU registers, PC, stack, WRAM/VRAM, bank state, and emulator
internals. The latter becomes stale when code or runtime layouts change.
Do not keep an old full snapshot as the starting state across ROM builds.

Ordinary saves are not automatically compatible with every future change either:
Polished Crystal currently has `SAVE_VERSION = 10`, fixed save/checksum regions,
and migration/load logic. Preserve or migrate that schema. Relocating code alone
does not require a new fixture, but changing saved fields or their interpretation
may. BGB can append 48 RTC bytes to MBC3 battery saves; the harness must handle
each frontend's actual SRAM/RTC storage format rather than blindly copying a
32-KiB file to every emulator.

The intended scenario workflow is:

1. Prepare small fixtures by actually playing to the required map/story state
   and saving through the normal in-game menu. Record ROM/save-schema revision,
   map/coordinates, party/items, RTC/time settings, and fixture hash. The native
   baseline probe now regenerates a player-room save using committed input scripts;
   the generated binary and screenshots stay in the external cache.
2. Copy fixtures into fresh per-client directories. Use isolated adapter configs,
   server storage, ports, and identities. Start the current ROM, select Continue,
   and wait for an explicit ready condition before multiplayer inputs.
3. Resolve symbols and banks from the current build's `.sym`; interpret observed
   values using versioned test layouts. This reads moving runtime addresses
   correctly without injecting an old CPU or memory snapshot.
4. Drive bounded input phases: join, wait for roster/avatar, start movement,
   wait for rendering, interact, decline/accept, disconnect, and reconnect.
   Advance on observed conditions with timeouts; do not assume a rapid fixed
   menu click sequence remains valid under all timings.
5. Assert independent server facts, ROM state, and rendered output. On timeout
   or mismatch, preserve input trace, ROM/fixture hashes, logs, symbols, state
   diagnostics, and screenshots.

Example presence oracles: three distinct IDs, exactly two remote bindings per
client where visible, correct coordinates and map epochs, required NPCs intact,
no palette/tile overlap, and a measured input-to-render latency under the stated
budget. Menu success means the correct target's interaction opens and closes
while the third player remains connected. Gifts/trades later need restart/save
reload and conservation/exactly-once checks at every durable failure boundary.

Golden PNGs are useful at native 160 by 144 resolution with fixed palettes,
time/weather, settings, and controlled input. Check start, mid-step, final stance,
and crowded-scene sprite rows, not just the final picture. Frames 3, 17, and 32
can be checkpoints relative to a recognized movement phase. For a network test
with jitter, also use event-triggered visual checkpoints and timing intervals;
one rigid global frame number would reject correct network scheduling.
Use tight, justified masks for irrelevant nondeterminism and combine visual
checks with semantic assertions. A correct server packet does not prove the
avatar was drawn; a correct final picture does not prove the intervening path.

### Emulator tooling and remaining implementation work

The primary backend is native DoubleCherryGB. The user rejects Rosetta, so Wine
and BGB are not prerequisites or acceptance gates for this branch.

BGB supports battery loading (`-loadbatt`), link listeners, input demos,
headless runs, SYM files, break/watchpoints, and screenshots/state export on exit.
This gives a practical independent compatibility-test path through libmobile-bgb
on an external compatible host if later requested. Normal exit is not a success oracle: a
demo ending or debugger break can exit normally. Completion/failure markers
and captured results must be checked. BGB does not expose a general console
debugger API in its documented CLI, so do not promise a nonexistent memory-read
command. Its CLI automation and a custom result reader still need prototyping.

For automated frame-by-frame control, the native libretro harness around the
pinned DoubleCherryGB core now works: supply input callbacks, run frames,
capture video, and isolate each instance in its own process. The inspected core
exports `retro_run` and SRAM/system/video memory functions. However, its current
single-game `retro_get_memory_data` uses the memory-type `id` as `v_gb[id]`, and
`retro_get_memory_size` does likewise. Memory types are not emulator instance
indices. The frontend avoids those accessors and reads WRAM through the C000
descriptor supplied by `RETRO_ENVIRONMENT_SET_MEMORY_MAPS`, plus the FF80 HRAM
descriptor. The pinned CPU uses 4-KiB physical WRAM banks; banked symbol offsets
are resolved against that buffer. SRAM uses the valid SAVE_RAM (id 0) accessor.
This was checked against the CPU source and actual save/reload/movement states;
no runtime observer patch in the core or production ROM is necessary.

`tools/probe_emulator.py` now drives three fresh processes: New Game plus normal
Save, Continue from the resulting battery file, and Continue plus one tile step
right. It asserts CGB mode, a zero crash marker, save existence/version, player
room map and coordinates, agreement between the player object and map position,
both save sentinels and game-data checksums, and changed rendered output. The
source fixture is unchanged. It retains logs, per-process state, PPM/PNG images,
and hashes, with a nonzero result on failure. The ROM retains its official hash.

This is a verified automation capability, not completed multiplayer testing.
The input timing scripts are restricted to the exact baseline hash. Extend the
frontend with bounded observation-driven phases and intermediate visual checks
for production scenarios. Offline startup can run quickly; socket latency tests
need real-time pacing because the adapter uses host time. Adapter commands,
ISP login, TCP exchange, ISR timing, and interactions remain acceptance work.

An optional test-only ROM observation block may expose sequence/frame counters,
readiness, binding records, and a completion marker. Observation must not replace
the production parser, movement decision, or transaction logic with host code.
Measure the feature build as well as any instrumented test build, since debug
hooks can change timing and memory requirements.

A freshly generated full snapshot may cache the offline startup phase only for
the exact ROM hash, emulator/core version, configuration, and fixture hash that
created it. Invalidate it on any mismatch and establish new network sockets
after restoring; snapshots cannot restore external Rust/socket state. Battery
boot remains the canonical correctness path.

Normal host tests remain separate and do not launch a Game Boy emulator. They
cover Rust domain/codecs, real TCP scenarios, pinned libmobile transcripts, and
static assembly/layout contracts. Executed ASM and actual serial/render timing
remain the job of a separate optional integration command.

## Reproduction and evidence

Build the matching standard baseline first, then run from this worktree:

```sh
python3 mobile_adapter/tools/audit_map_slots.py \
  --rom /path/to/baseline/polishedcrystal-3.2.3.gbc \
  --sym /path/to/baseline/polishedcrystal-3.2.3.sym \
  --output mobile_adapter/research
python3 mobile_adapter/tools/probe_memory.py \
  --build-dir /path/to/baseline \
  --strategy helper-relocation \
  --output mobile_adapter/research/memory-probe.json

python3 mobile_adapter/tools/probe_memory.py \
  --build-dir /path/to/baseline \
  --strategy banked-isr \
  --output mobile_adapter/research/memory-banked-isr-probe.json
```

Use a matching checkout, objects, map, ROM, and SYM; the tools target this baseline's
record/build formats. The initial local run used RGBDS 1.0.1 and the already verified ROM
SHA-256 `314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e`.
Both memory strategies have since been reproduced with RGBDS 1.0.4 and the
matching standard build; the recorded JSON now uses schema version 2. The
faithful variant remains a separate adoption gate. No emulator execution of
either capacity-placeholder ROM or multiplayer latency benchmark has occurred.
The separate native baseline boot smoke and host setup are recorded in
[HOST_ENVIRONMENT.md](HOST_ENVIRONMENT.md).

Primary external references:

- [libmobile command transfer and serial timeout](https://github.com/REONTeam/libmobile/tree/0704f56902f23b7ebf05c82c222e0e145e3140b6):
  `commands.c:command_transfer`, `mobile.c:mobile_loop`.
- [libmobile-bgb](https://github.com/REONTeam/libmobile-bgb/tree/4dfc19f69a8ccb6f2ada55e49765419f76d03e2c):
  bridge byte transfers, separate config, socket callbacks.
- [DoubleCherryGB libretro implementation](https://github.com/TimOelrichs/doublecherryGB-libretro/blob/03f58ca3dfb4b716f7e66a0e0467f9e85ef82abb/libretro/libretro.cpp):
  `retro_run`, memory accessors and their current indexing limitation.
- [BGB manual](https://bgb.bircd.org/manual.html): command-line automation,
  battery-save RTC format, debugger and sprite-line behavior.
- [Dan Docs, Mobile Adapter GB](https://shonumi.github.io/dandocs.html#magb):
  hardware serial speed and protocol, distinct from achieved emulator throughput.

Repository source anchors include `constants/map_object_constants.asm`,
`ram/wramx.asm`, `layout.link`, `home/farcall.asm`, `home/map_objects.asm`,
`engine/overworld/player_object.asm`, `engine/overworld/map_objects.asm`,
`engine/overworld/scripting.asm`, `data/sprites/map_objects.asm`,
`data/sprites/facings.asm`, the dense maps above, and
`engine/events/bug_contest/contest.asm:Special_SelectRandomBugContestContestants`.
