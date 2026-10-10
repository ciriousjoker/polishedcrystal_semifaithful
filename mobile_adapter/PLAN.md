# Mobile Adapter multiplayer plan

Last updated: 2026-10-10.
Status: planning, source investigation, and reproducible capacity-audit tools.
No production ROM or server implementation yet.
Working branch: `mobileadapter`.
Base commit: `8f3c7e9651e988d876e32a4c7ad9ef1bb76702a8`.

This document is the persistent working memory for the project. Keep decisions,
evidence, unresolved questions, and the next concrete step current as work
continues. Communicate findings directly to the user in German; repository
content stays in English. Do not create an upstream PR or push this branch unless
requested. Commit completed changes locally. Do not use the GitHub plugin.

## User intent and scope

- Develop ROM changes and a Rust server together on this branch.
- Start with a server on the local PC and two independent game instances.
- Design for multiple connected players from the beginning. The first supported
  lobby capacity is three total players, including the local player, so each ROM
  tracks up to two remote players. Player collections and explicit target IDs
  must exist even in the first two-player demonstration.
- Make remote players visible in the overworld.
- Facing another player and pressing A opens a shared interaction menu.
- Calling an online player through the Pokegear plays the existing call sound,
  closes the call screen, and opens that same interaction menu across any map
  distance. This is a game action, not audio calling.
- Support battle challenges, Pokemon trades, and gifts of ordinary items.
- Keep new adapter and multiplayer code together under `mobile_adapter/` as far
  as practical. Engine hooks elsewhere are expected.
- Normal Rust unit tests and assembly contract checks must run without starting
  a Game Boy emulator. Be precise about what those checks actually verify.
- A desktop app, hosted deployment, unrestricted lobby sizes, and battles with
  more than two participants are later work.

Interpretation: the user said "GBC" when describing an emulator using
"libmobile.bgb". The likely intended emulator is BGB with `libmobile-bgb`.
DoubleCherryGB is the second intended adapter implementation. This interpretation
does not change the requested Game Boy Color ROM target.

## Feasibility assessment

This is technically plausible, but it is a substantial ROM feature rather than
a server-side wrapper around the existing cable link. A persistent connection
to one server can carry presence and interactions for several players. The
adapter does not need one physical connection per player.

The most important risks, in implementation order, are:

1. Fitting an asynchronous serial driver into an almost full ROM and preserving
   bank registers, interrupt behavior, and frame timing.
2. Keeping adapter activity alive through menus, scripts, animations, and battles.
3. Rendering two remote avatars without corrupting NPC slots, sprite tiles, or
   palettes on crowded maps.
4. Replacing cable-specific waiting and acknowledgement behavior in battle and
   trade paths with explicit network sessions.
5. Recovering interrupted trades and gifts without silently losing or duplicating
   inventory in normal supported clients.

Presence, calls, and menus are a reasonable first playable milestone. Battles
require a deeper transport adaptation. Persistent transfers should come after
connection recovery and durable transaction recovery are demonstrated.
No calendar estimate is justified before the serial and memory experiments.

## Verified baseline and resource limits

The base commit built successfully with `make -j4` using local RGBDS 1.0.1.
The repository and CI request RGBDS 1.0.4. The 2 MiB standard ROM nevertheless
matches the official development asset for this exact commit.

```text
SHA-256: 314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e
```

Measured from the baseline linker map, rather than estimated:

| Region | Free bytes | Consequence |
| --- | ---: | --- |
| ROM0 | 18 | A new interrupt handler will require reclaiming resident code space. |
| ROMX, across 127 banks | 13,943 | Most banks are full; total free bytes do not imply arbitrary placement. |
| ROM, total | 13,961 | Importing Crystal's complete mobile subsystem is not a practical starting point. |
| WRAM0 | 300 | Keep resident IRQ state small. |
| WRAMX bank 1 | 13 | Do not append the player table to normal game data. |
| WRAMX bank 2 | 2,837 | Candidate for dedicated packet buffers and peer records after a lifetime audit. |
| WRAMX bank 5 | 2,062 | Fragmented and shares graphics/battle data; not automatically safe storage. |
| HRAM | 5 | Use at most a minimal ownership flag or pointers after auditing. |
| SRAM, across four banks | 1,832 | Fragmented; preserve existing saves and checksums. |

`Makefile` currently selects `MBC3+TIMER+RAM+BATTERY`. Do not assume that increasing
ROM size is a harmless fix. Any mapper or extended bank support change requires
verification in both BGB and DoubleCherryGB and an explicit compatibility
decision. First measure a minimal driver and reclaim or relocate suitable ROM0
helpers into ROMX. Fixed-bank callers and interrupted banked code must remain
valid. Existing `layout.link` placements need deliberate updates.

Follow-up experiments are recorded in [research/FEASIBILITY.md](research/FEASIBILITY.md).
A disposable assembler/linker experiment relocated three foreground helper files
through existing far-call entry points and recovered 264 ROM0 bytes. It then
successfully reserved 256 ROM0 bytes, 8 KiB ROMX, 96 WRAM0 bytes, 1.5 KiB in WRAMX
bank 2, one HRAM byte, and 128 SRAM bytes simultaneously. This demonstrates a
plausible initial placement, not runtime correctness or the final driver size.
Exact results and repeatable tooling live in `research/` and `tools/`. Keep the
production baseline unchanged until relocation regression tests are available.

The overworld has 13 runtime object structures, including the local player,
and 21 map object records. Each runtime structure is 34 bytes. Objects starting
at slot 8 use VRAM bank 1. `home/map_objects.asm:GetSpriteVTile` assigns sprite
tile ranges from runtime slot indexes, with special handling for large sprites.
Adding peers is therefore constrained by object slots, VRAM allocation, palettes,
and hardware OAM limits, not simply by the background tileset.

`ram/wram0.asm` and `ram/wramx.asm` contain large UNION regions reused by menus,
link communication, and battles. Their apparent idle capacity is not persistent
network storage. In particular, `wLinkData` overlaps a 1,300-byte scratch region.

## Adapter boundary and transport decision

The layers are:

```text
Polished Crystal ROM
  -> Game Boy serial registers and Mobile Adapter command packets
  -> emulator adapter implementation / libmobile
  -> ordinary TCP socket
  -> Rust game server
  -> other clients through their own TCP sockets
```

In BGB there is one extra boundary: BGB's link protocol connects to the separate
`libmobile-bgb` process. The Rust game server does not implement BGB's link
protocol and does not receive raw emulated serial interrupts.

Local setup, with intended game-server port 17423:

```text
BGB A, link listener 8765 -> libmobile-bgb A -> 127.0.0.1:17423
BGB B, link listener 8766 -> libmobile-bgb B -> 127.0.0.1:17423
BGB C, link listener 8767 -> libmobile-bgb C -> 127.0.0.1:17423

DoubleCherryGB A/B/C -> built-in libmobile -> 127.0.0.1:17423
```

Give every BGB bridge a different adapter configuration file using `-c`.
Give every game instance a separate save path. The bridge defaults to BGB host
`127.0.0.1`, port `8765`, and config path `config.bin`.
The Rust server accepts all client sockets on one listener; client ports need
not be assigned manually. A local server on a different device uses that
device's reachable IP instead of localhost.

### Recommended production shape for this branch

Use the adapter's ISP/Internet mode and one TCP connection per ROM. The relevant
command sequence is adapter wakeup, start session (`0x10`), dial a supported ISP
number (`0x12`), PPP login (`0x21`), TCP connect (`0x23`), repeated data transfer
(`0x15`), and orderly close. DNS (`0x28`) is unnecessary for the initial numeric
loopback endpoint. Session startup and cleanup must handle partial failures.
Initial development can use the blue adapter and libmobile's recognized `#9677`
ISP number with the minimum accepted PPP payload, subject to the compatibility
experiment. A generic empty configuration must be tested; do not make a prior
run of Mobile Trainer an undocumented prerequisite or rewrite adapter EEPROM.

Verified in libmobile: TCP connect takes four IPv4 bytes and a big-endian port,
then returns a connection ID. The server receives only TCP application bytes,
not Mobile Adapter magic bytes, packet checksums, or connection IDs.
The library supports two network connection slots; one is sufficient for our
multiplayer session. This is not a limit of two players per lobby.

### Distinguish relay configuration from game-server configuration

The historical telephone/P2P mode and the Internet/TCP mode are distinct.
`libmobile-bgb --relay` speaks the REON telephone relay protocol. It cannot be
pointed at an arbitrary Rust game server just because both use TCP.
DoubleCherryGB's custom `dns_ip` and `relay_ip` settings similarly configure
adapter services, not our game's TCP endpoint. That endpoint must be supplied
by ROM code, initially as development build constants and later through a
small in-game connection setup if needed.

Alternative experiment only: with relay disabled, dialing `127000000001` in
libmobile's P2P mode connects to loopback TCP port 1027 by default. A server
could accept several such sockets and speak our application protocol, but that
would repurpose telephone mode and its timing/configuration rules. Prefer the
explicit Internet/TCP route and do not build the main design around an adapter
relay implementation or around historical Pokemon service endpoints.

The Rust server need not link libmobile. A host test harness may link a pinned
copy to validate adapter transcripts separately from ROM execution.

## Runtime transport design

- One owner of `rSB` and `rSC`: idle, cable link, printer, or mobile. Switching
  owners must cancel outstanding transfers and restore existing serial state.
- A short resident serial ISR records received bytes and advances bounded
  byte-transfer state. It must preserve registers and any bank it touches.
  Packet interpretation, UI work, and server-event handling stay outside the ISR.
- Do not send only one byte per overworld frame. The serial pump must continue
  packet transfers asynchronously while the main loop remains responsive.
- Start with 8-bit adapter transfers and GBC fast serial clock. Avoid the
  adapter's GBA-oriented 32-bit transfer mode.
- Tentative memory placement: small ownership/cursor/state data in WRAM0,
  larger adapter buffers and the peer table in a dedicated section in WRAMX
  bank 2. Exact sizes and safe ISR access are an experiment, not an allocation
  already proven safe. Prefer a small resident staging area if it avoids
  switching WRAM banks for every interrupt.
- Networking starts only when explicitly enabled. Adapter absence leaves normal
  offline gameplay usable and exits the connection attempt with a bounded timeout.
- Target 20 presence exchanges per second in the first latency experiment,
  subject to measured transfer/frame cost. Publish step starts and progress,
  not only completed tile positions. Local movement responds immediately;
  remote rendering uses short interpolation and bounded prediction. Initial
  goals are local p95 below 200 ms and p95 below 300 ms with each client's server
  RTT near 100 ms. These are unmeasured acceptance targets, not achieved results.
- Coalesce unsent movement into the most recent snapshot. Reliable events have
  a separate bounded queue; they must not be silently dropped with movement.
- The adapter is driven by ROM transfers. Server-side push still requires the
  ROM to poll and clock data out. TCP reads can split or combine messages.
- libmobile's Internet-mode `0x15` implementation can wait roughly one second
  on an empty send/empty receive. Send an explicit application `POLL` request
  rather than relying on empty receive calls for normal presence updates.
- libmobile drops an active adapter connection after about three seconds with
  no serial activity. Maintain activity through long menus and animations, or
  explicitly report paused/disconnected presence and reconnect afterward.
- Never add a blocking network wait or expensive parser to VBlank. Use a bounded
  service hook in relevant foreground loops. A global `DelayFrame` hook needs
  a reentrancy and bank audit before use; it is not automatically safe.
- Disconnect cancels noncommitted invitations, removes stale avatars, and
  returns control to ordinary gameplay. Inventory recovery follows the durable
  transaction state rather than a blanket rollback.

libmobile exposes up to 254 network bytes per transfer, while connection-ID and
adapter framing bytes consume additional space. Begin with logical application
frames of at most 128 bytes and conservative outgoing chunks. Incoming adapter
buffers must tolerate the implementation's maximum packet length, including
the connection ID, rather than assuming each TCP read contains one short frame.
Negotiate bounded server responses and define chunk/reassembly limits before
exchanging parties. Do not confuse a TCP write with a packet boundary.

## Multi-player model and protocol

Use explicit `LobbyId`, `PlayerId`, connection generation, `InteractionId`,
`BattleId`, and `TransactionId`. A ROM peer slot is a rendering/cache location,
not a persistent identity. A reconnect must invalidate delayed events from the
old connection generation.

The server maintains a collection of players per lobby and routes each action
to an explicit target or participant list. A ROM keeps a bounded collection of
remote-player records. Never introduce a global "the remote player" for lobby
state. Within a two-person battle or trade, it is valid to populate the existing
single-opponent game buffers with the selected peer's data.

Separate these limits:

- Supported lobby capacity: initially 3 total players.
- ROM peer cache and visible avatar capacity: initially 2 remote players.
- Interaction participants: initially 2, explicitly identified.
- Battle participants: 2 under the existing battle engine.
- One exclusive battle/trade/gift session per player at a time.

The server collections can support a larger configurable lobby limit later, but
the current ROM advertises its capacity and rejects incompatible lobbies.
Future larger lobbies require paged rosters and a bounded visible interest set;
they are not promised by simply raising one constant. Multiplayer battles would
require separate battle mechanics and are outside this plan.

Proposed message families, not a frozen wire specification:

| Family | Purpose |
| --- | --- |
| `HELLO`, `WELCOME`, `ERROR` | Protocol/build capabilities, identity, resume result. |
| `JOIN`, `LEAVE`, `ROSTER` | Lobby membership and full resynchronization. |
| `POLL`, `HEARTBEAT`, `PRESENCE` | Bounded event delivery and position/status snapshots. |
| `INTERACTION_REQUEST`, `ACCEPT`, `DECLINE`, `CANCEL` | Explicit caller/target/session and busy states. |
| `BATTLE_READY`, `BATTLE_ACTION`, `BATTLE_ABORT` | Party setup, ordered turns, timeout cleanup. |
| `TRANSFER_OFFER`, `PREPARED`, `COMMIT`, `APPLIED`, `STATUS` | Trade/gift agreement and durable recovery. |

Keep framing binary and explicitly serialized: protocol version, message type,
bounded payload length, request/sequence ID, relevant session identity, and
payload. Define byte order and the Polished Crystal character encoding.
Do not copy Rust structures or arbitrary in-memory assembly structures directly
onto the wire. IDs, sequence wrap, unknown commands, bad lengths, and stale
generations require defined behavior. Large Pokemon or party payloads use
bounded chunks with transfer ID, chunk index, total length, and integrity checks.

The handshake checks gameplay compatibility, not just the displayed release
version. Faithful and standard builds have different mechanics and must be
rejected for incompatible interactions unless a compatibility policy is later
implemented. Include a protocol revision, gameplay/schema revision, and build
capabilities. Parties and gifts use Polished Crystal's own schema.

## Overworld behavior

Share presence, not the entire single-player save state. Initial snapshots include
map group/number, a map-instance or visibility key, map-entry epoch, tile position,
facing, movement state, approved avatar, sequence number, and availability.
Map-instance rules must hide incompatible story versions of a location.
Story flags, NPCs, encounters, time of day, pickups, and puzzles remain local in
the first version. This is a social overworld, not shared quest progression.

Remote avatars are nonblocking and never run trainer scripts or NPC movement AI.
Apply snapshots to a dedicated peer record, then interpolate a short animation
or snap after a warp. Warps and connected-map transitions invalidate stale
coordinates. A position snapshot and map identity must describe one consistent
state; account for the engine's object-coordinate offsets.

For the first sparse-map experiment, use available runtime object slots plus an
explicit object-slot-to-player binding table. `TEMP_OBJECT` or another special
marker alone is insufficient: existing interaction code calls `GetMapObject`
with the stored map-object index. Remote interaction must be intercepted before
that lookup, and deletion/visibility routines must clear bindings safely.
Automatic NPC spawning must not overwrite remote objects.

Do not globally increase `NUM_OBJECT_STRUCTS` without redesigning the tile-slot
mapping, WRAM layout, iteration loops, and special large-sprite reservations.
Audit allocator behavior on crowded maps before declaring general overworld
support complete. Preserve required NPCs; a capacity failure may temporarily
hide a peer and expose them through the online roster with a visible indication.
If this fallback is common, a dedicated remote rendering allocation is needed.
Three lobby members does not by itself guarantee three visible avatars on every
map. Start with an approved small avatar set and standing/walking states, then
add surfing, biking, stairs, ledges, weather, palette changes, and large sprites
after checking their tile budget.

The all-map audit covers 611 assembled maps. Whole-map record counts leave room
for one/two/three extra runtime structures on 542/528/519 maps respectively,
excluding additional effects. Initial-window geometry looks less crowded but
is not a lifetime guarantee: story alternatives, ordinary movement, retained
`WONT_DELETE` objects, temporary effects, and special tile ranges matter.
Snowtop Mountain's windows/camera substitute and Cinnabar Volcano's retained
rocks/boulders are concrete reasons to include capacity management immediately.
See the report and `research/map-slots.csv` for every map and the audit's limits.

Use a feature-owned replaceability table and temporary suppression mask. Restore
eligible decoration when visible-peer demand disappears; preserve stable map
IDs, position/state, event flags, and required script actors. Do not use the
story-mutating `Script_disappear` command for multiplayer eviction. With twelve
nonplayer runtime slots, one/two remotes require ordinary demand at most eleven/
ten; a one-effect reserve lowers that to ten/nine. Special last-slot sprite
graphics can impose a tighter allocation rule. Define a protected camera/cutscene
policy and defer rendering if no eligible object can be reclaimed. This policy
is necessary even for a robust two-player release, although ordinary maps mostly
fit without hiding NPCs.

Facing interaction uses current map/instance and adjacent coordinates. If
multiple players overlap the same tile, select a target by ID in a short menu
instead of always taking the first array entry. The server checks an accepted
interaction against current participants and availability; movement may update
during invitation transit.

## Calls and the shared interaction menu

Expose a separate online-player roster in the Pokegear. Existing phone contacts
are static NPC indexes stored as a bitset, so network identities must not be
inserted into `wPhoneList` as fake trainer numbers.

Both entry routes call one `OpenPlayerInteraction(PlayerId, InteractionId)` path:

1. Face a remote avatar and press A, or select an online player and call them.
2. Create an invitation with explicit initiator and target IDs.
3. For calls, reuse `SFX_CALL`; restore/close the Pokegear call screen.
4. Recipient accepts, declines, or returns busy/offline. Do not interrupt an
   active script, animation, save, battle, or another exclusive session.
5. Open the same player interaction menu: challenge, trade, gift item, leave.

Calls work across map distances and initially do not depend on NPC phone-service
restrictions. An incoming call is queued until a safe foreground point or expires.
If A calls B while C calls B, the server serializes acceptance and returns a
defined busy result for the other request. Simultaneous crossed invitations also
need deterministic resolution.

"Talk" is initially the interaction/menu flow described by the user. Free-text
chat, voice, and new message-entry UI are not assumed requirements. Add canned
messages only if later requested.

## Battles, trades, and gifts

### Battle sessions

Reuse battle mechanics and opponent buffers after selecting one explicit peer.
Replace cable transport boundaries, not the game with a second battle simulator
inside the Rust server. The server coordinates session readiness and ordered
actions; clients initially execute the existing battle engine.

`Gen2ToGen2LinkComms` exchanges random numbers, parties, and other link data.
`LinkBattleSendReceiveAction` repeatedly calls `LinkTransfer` and uses frame-based
acknowledgement delays. Implement a mobile path for setup and turn exchange,
using battle ID, turn index, explicit participant IDs, timeout, and cancellation.
Preserve deterministic RNG ordering and define a desynchronization check before
continuing the next turn. Do not relay old link bytes with guessed timing.

The current `LinkBattleError` uses `SoftReset`; mobile disconnect needs a safe
exit restoring prebattle state instead. Presence continues as busy/in battle.
The third lobby member remains connected and can walk or see busy status; no
spectator mode or three-person fight is implied. Audit all `wLinkMode` branches
and give the new transport a distinct mode rather than accidentally selecting
an old cable path.

### Durable inventory operations

Item gifting is feasible using `ReceiveItem` and `TossItem`, but only after
capacity, quantity, target acceptance, and save recovery are handled. Begin with
ordinary allowed bag items; exclude key items and special pockets until their
semantics are designed. Use an explicit allowlist and bounded quantities.

Pokemon trading can reuse party-selection, display, animation, and appropriate
save helpers. It needs an explicit transfer schema including species/form,
personality, moves, held item, names, OT data, and any supported mail state.
Preserve last-usable-Pokemon rules and trade evolution behavior. Do not reuse
vanilla Crystal's Pokemon serialization without auditing Polished Crystal's
changed structures and storage format.

Both operations need a durable protocol before changing either inventory:

- Server validates the offer and reserves both participants.
- Both clients validate current source and destination capacity, write and save
  a pending transaction record, and lock the relevant party/inventory operations.
- Server persists a single commit or abort decision before replying.
- Clients apply that decision once and durably save inventory and the applied
  transaction marker in a recoverable order. A journal must distinguish intent,
  partially applied state, and completed state; a lone "done" flag is insufficient.
- Reconnect/load consults the journal and server decision to finish recovery.
  A timeout after durable commit is not permission to restore the source item.
- Tests inject crashes around every persistent write on each client and server.

The exact journal layout and save ordering are unresolved and block this feature.
`SaveAfterLinkTrade` saves Pokemon data and related checksums; it is not a generic
bag-plus-network-journal save transaction. Checksum/backup selection on load must
recover a consistent inventory/journal pair. Existing SRAM contains reserved
unused save padding worth auditing, in addition to linker-reported free space;
preserve save sentinels, checksums, backup offsets, and migration behavior.

Promise recovery for supported clients, not cheat prevention. Local save
restoration, duplicated save files, and a malicious ROM can defeat economic
ownership without an authoritative server inventory. Do not claim a central
lobby coordinator alone prevents such duplication.

## Planned code organization and integration seams

Keep the feature under one root directory named `mobile_adapter`:

```text
mobile_adapter/
  PLAN.md
  research/             evidence, capacity results, complete map-slot audit
  tools/                reproducible host-side capacity/audit scripts
  protocol/             versioned schema, vectors, generated-file rules
  rom/
    constants.asm       feature constants and bounded structure layouts
    ram.asm             dedicated WRAM/HRAM declarations
    sram.asm            versioned identity/journal data when needed
    serial.asm          resident ISR portion and serial ownership
    adapter.asm         Mobile Adapter packet/command state machine
    protocol.asm        application framing and bounded reassembly
    session.asm         connect, lobby, peer table, reconnect
    overworld.asm       remote avatar bindings and snapshots
    interaction.asm     common menu and invitation state
    phone.asm           online roster and call presentation
    battle.asm          selected battle-session bridge
    trade.asm           Pokemon transfer integration
    gift.asm            item transfer integration
    data/               menus, text, tables
  server/
    Cargo.toml          Rust workspace, added during implementation
    crates/protocol/    binary codecs and limits
    crates/core/        pure lobby, interaction, and transaction state
    crates/server/      TCP runtime, persistence, configuration
  tests/
    contracts/          assembler fixtures and byte/layout checks
    host_adapter/       pinned libmobile transcript tests
    scenarios/          two/three-client TCP scenarios and fault injection
```

The implementation directories are proposed; planning, research, and audit tools
exist now. Source location does not determine memory bank. `serial.asm` can be
included in a resident section even though its file lives under this directory.
Generate cross-language constants
from one small schema, or validate synchronized definitions if generation proves
unnecessarily complex. Commit meaningful shared vectors. Do not vendor the
entire upstream mobile subsystem or external emulator repositories by default.

| Existing location | Proposed narrow hook |
| --- | --- |
| `home/serial.asm:Serial` | Dispatch to the mobile ISR when it owns serial; preserve cable/printer behavior. |
| `home.asm`, `main.asm`, `includes.asm`, `ram.asm` | Include the grouped feature sources and declarations in explicit sections. |
| `Makefile`, `layout.link` | Feature build flag/target, code placement, resource assertions, test targets. |
| `engine/overworld/events.asm:HandleMap` | Bounded foreground network service and coherent local snapshots. |
| `engine/overworld/events.asm:TryObjectEvent` | Remote binding lookup before ordinary `GetMapObject`; enter the common menu. |
| `engine/overworld/map_objects.asm` | Remote step handling, visibility/deletion cleanup, allocator cooperation. |
| `engine/overworld/player_object.asm` | Map entry and visible object allocation/rebinding. |
| `engine/overworld/npc_movement.asm` | Separate interaction hit detection from nonblocking remote collision behavior. |
| `home/map_objects.asm:GetSpriteVTile`, `engine/overworld/overworld.asm:GetUsedSprite` | Audit object-slot tile allocation, avatar reloads, and palette/VRAM capacity. |
| `engine/pokegear/phone.asm` | Online roster and call sound/screen restoration. |
| `engine/phone/phone.asm` | Incoming network invitation scheduling at a safe point, separate from random NPC calls. |
| `engine/link/link.asm:Gen2ToGen2LinkComms` | Mobile-specific setup and party/RNG exchange. |
| `home/serial.asm:LinkTransfer`, `engine/battle/core.asm:LinkBattleSendReceiveAction` | Explicit mobile battle action exchange instead of cable nybbles and fixed delays. |
| `engine/link/link.asm:LinkTrade`, `engine/menus/save.asm:SaveAfterLinkTrade` | Selected-peer trade path and durable completion/recovery. |
| `home/item.asm`, `engine/items/items.asm` | Existing inventory operations called by gift logic after validation. |
| `engine/menus/save.asm`, `engine/events/checksave.asm`, `ram/sram.asm` | Safe persistent journal, backup/checksum integration, migration/load recovery. |

Start with a `MOBILE_ADAPTER` build flag and a separate ROM artifact. With the
feature disabled, the baseline ROM should remain byte-identical where practical.
Check standard and faithful builds before expanding to cosmetic/debug variants.
VC/printer/cable compatibility must be explicit, not silently assumed.

## Rust server design and tests

Use Rust with an asynchronous TCP runtime such as Tokio. Keep the domain core
independent of the runtime and sockets. It consumes typed events and emits typed
effects. Inject time and ID generation rather than sleeping in unit tests.

One lobby owner processes membership and interaction transitions sequentially;
connection tasks perform I/O through bounded channels. This avoids a collection
of locks and makes crossed invitations deterministic. Movement may be coalesced;
transaction and battle events may not. Capacity violations and slow clients
have defined error/disconnect behavior. Lobby ownership is separate from network
connection count and can later be partitioned by lobby.

Local presence can initially be in memory. Before gifts or trades are enabled,
add durable transaction decisions, for example with SQLite. Bind loopback by
default. Configurable bind address/port supports later hosting. Public deployment
needs its own identity, abuse, persistence, and transport design; libmobile's raw
TCP path does not magically provide TLS. No cloud resources are created now.

Required normal unit tests:

- Binary round trips, exact vectors, endian handling, length limits, unknown
  versions/types, partial frames, combined frames, malformed chunks.
- Three-member roster, capacity limits, leave/reconnect, stale generations,
  target lookup independent of slot order, and presence map/epoch ordering.
- A challenges B while C contacts B; crossed calls; busy, declined, expired,
  canceled, and disconnected sessions; no events leaking to C's interaction.
- Battle readiness, duplicate/out-of-order turn submission, wrong participant,
  timeouts, and compatibility rejection.
- Gift/trade preconditions, full bag/party, duplicate messages, durable commit
  versus abort, restart/replay, and every persistence failure boundary.

Add loopback integration tests using two and three lightweight TCP clients,
without a Game Boy emulator, to exercise the real framing and runtime. Adapter
tests should drive libmobile's serial C API with virtual time and socket callbacks,
checking connect/poll/disconnect transcripts and empty-poll behavior. These
tests prove host compatibility and the specified transcript, not execution of
the ROM serial driver.

## What can be tested in assembly without an emulator

RGBDS assertions and standalone assembly/link fixtures can verify constants,
field offsets, counts, table sizes, bounds, bank placement, save sentinels, and
static packet bytes. Assembly macros can produce protocol fixtures that host
tests compare with the Rust codec. Build/map checks can reject ROM or RAM budget
regressions and feature-disabled checksum changes.

These are compile-time/contract tests, not runtime unit tests of an assembled
parser or interrupt handler. A Rust reference state machine tested against the
same vectors verifies that model; it does not prove the assembly matches it.
Actual execution of LR35902 instructions requires hardware or a CPU emulator.
A headless CPU test runner would avoid launching a graphical emulator but would
still be emulation, so it must not be described as meeting a literal ban on
emulation. It is an optional later improvement, not required by the normal
unit-test target.

Keep the future command split clear:

```text
make test-mobile-host       Rust tests, static ASM contracts, adapter transcripts
make mobile                build the adapter-enabled ROM with resource checks
make test-mobile-rom       optional CPU execution or emulator integration tests
```

These targets do not exist yet. BGB and DoubleCherryGB manual integration remains
necessary for actual serial timing, rendering, menu responsiveness, and end-to-end
battles/trades. The ordinary unit-test command must not start either emulator.

Integration fixtures must be ordinary in-game battery saves loaded by the current
ROM, followed by boot/Continue and bounded state-driven input phases. Never reuse
a full CPU/WRAM emulator snapshot across ROM hashes. Preserve or migrate the
normal save schema, handle frontend-specific RTC data, and resolve test observers
from the matching current `.sym` instead of fixed addresses. Check semantic ROM
state, server facts, input-to-visible latency, and native-resolution PNGs at start,
intermediate movement, and final state. Frame checkpoints should be relative to
recognized phases, with event/timing conditions for jittered networking.

BGB's documented battery/headless/demo/breakpoint/export options make it a
compatibility-test candidate; process exit alone does not prove success. A native
libretro harness for DoubleCherryGB can offer tighter frame control, but its
inspected system/video-memory accessor indexing requires validation or repair
before using it as an observer. Neither harness nor save fixtures exist yet.
The detailed workflow and remaining backend work are in the feasibility report.

## Milestones and acceptance gates

1. **Memory and adapter proof.** Pin tool/reference versions, implement only the
   minimal feature build and asynchronous adapter connection. Measure ISR size,
   RAM, transfer latency, and frame cost. Reach a loopback Rust echo/test endpoint
   from BGB and DoubleCherryGB with clean configurations. Unsupported adapter,
   timeout, and cancellation must leave offline gameplay usable. Gate: memory
   layout and Internet/TCP path are proven before feature UI work grows.
2. **Server core and three-client model.** Implement protocol and pure Rust lobby
   state with two/three-client host tests, bounded queues, reconnect, and explicit
   target IDs. Gate: three clients are distinct; no global other-player state.
3. **Two-player overworld demonstration.** In a sparse map, show a remote avatar,
   position/facing, warps, and disconnect cleanup. Keep a two-entry remote table
   even with only one remote connected. Gate: no blocking gameplay or NPC/script
   collisions, plus measured sprite/bank budgets.
4. **Three-player presence and interaction.** Show two peers where resources
   permit, handle crowding explicitly, select overlapping targets, and exercise
   crossed invitations. Gate: C remains connected while A and B interact.
5. **Pokegear calls and common menu.** Ring, accept/decline/busy/expire, restore
   the screen, and open the same action menu across maps. Gate: calls do not
   interrupt unsafe scripts or steal an ongoing interaction.
6. **Battle bridge.** Adapt setup/RNG and ordered actions with safe abort and
   resynchronization checks. Gate: two-person battles under latency and disconnect,
   with a third connected lobby member and continued adapter activity.
7. **Durable transfer foundation and gifts.** Specify and test journal/save
   recovery before enabling inventory mutation, then deliver allowed bag items.
   Gate: fault-injected restarts recover consistently on supported clients.
8. **Pokemon trades.** Add the Polished Crystal-specific schema, party UI,
   evolution/mail rules, and durable transfer flow. Gate: interrupted and replayed
   transactions, full parties, and backup saves behave correctly.

Revisit the memory budget at every milestone. If the minimal adapter plus
presence cannot fit safely, resolve code relocation/resource/mapper choices
before continuing into battles and trades. Do not assume the complete original
Crystal mobile code can be pasted into this ROM.

## Evidence and external references

Investigated on 2026-10-10. External source checkouts used for investigation are
temporary references, not dependencies committed into this repository.

- [libmobile](https://github.com/REONTeam/libmobile/tree/0704f56902f23b7ebf05c82c222e0e145e3140b6):
  `commands.h`, `commands.c`, `mobile.h`, `serial.c`, `mobile.c`. Verified command
  IDs, ISP numbers, TCP bytes/port, transfer limits, poll wait, and idle timeout.
- [libmobile-bgb](https://github.com/REONTeam/libmobile-bgb/tree/4dfc19f69a8ccb6f2ada55e49765419f76d03e2c):
  README and `source/main.c`. Verified BGB listener usage, bridge/config arguments,
  relay versus Internet distinction, and the direct-dial IPv4 convention.
- [DoubleCherryGB](https://github.com/TimOelrichs/doublecherryGB-libretro/tree/03f58ca3dfb4b716f7e66a0e0467f9e85ef82abb):
  README and `gb_core/linkcable/Mobile_Adapter_GB.cpp`. Verified embedded libmobile,
  DNS/relay configuration, and ordinary IPv4 socket connect handling. Its vendored
  libmobile version is a separate compatibility target from upstream master.
- [Dan Docs, Mobile Adapter GB](https://shonumi.github.io/dandocs.html#magb):
  hardware protocol, fast GBC serial clock, packet framing, commands, and timings.
  Resolve discrepancies between hardware documentation and adapter implementations
  with pinned transcript tests rather than guessing.
- [pret/pokecrystal](https://github.com/pret/pokecrystal/tree/3bc8daa4173e96a7f4011dad3922eb6fa5dad5c6):
  `home/serial.asm`, `home/mobile.asm`, and `mobile/`. Reference for bank-aware
  mobile interrupt dispatch and original mobile code; not assumed compatible
  with Polished Crystal's memory layout or data model.
- [Official baseline ROM](https://github.com/Rangi42/polishedcrystal/releases/download/do-not-checkout/polishedcrystal-dev-8f3c7e96.gbc):
  GitHub asset SHA-256 matched the local baseline build. The rolling release URL
  may later disappear; the commit and verified hash are recorded above.

## Open decisions and continuation checklist

- [x] Create and attach a separate managed worktree and the `mobileadapter` branch.
- [x] Confirm a reproducible baseline ROM and inspect actual memory usage.
- [x] Identify serial, overworld, phone, link, battle, item, and save integration seams.
- [x] Distinguish libmobile telephone relay configuration from our TCP game server.
- [x] Record a multi-player model, feature order, test limits, and durability risks.
- [x] Demonstrate an initial ROM0/ROMX/RAM reservation with disposable links.
- [x] Audit all 611 maps and distinguish initial visibility from retained demand.
- [x] Specify replaceable-decoration policy, latency targets, and battery-save
  integration-test oracles.
- [ ] Prove minimum ISP/TCP startup on both adapter implementations, including
  fresh configuration and disabled or irrelevant telephone relay behavior.
- [ ] Allocate resident serial code and stable RAM with measured budgets and
  audit bank/interrupt safety.
- [ ] Freeze the first binary schema and response-credit/chunking rules only
  after the transport experiment.
- [ ] Resolve crowding behavior, map-instance rules, and avatar allocation.
- [ ] Design safe foreground service points through menus, saves, and battles.
- [ ] Specify journal placement, save compatibility, and client crash recovery
  before coding any inventory-changing multiplayer feature.

Next concrete implementation step, when requested: milestone 1, a minimal
adapter connection proof with a feature build and small loopback endpoint.
Continue using this worktree and branch; do not create another branch or PR.
