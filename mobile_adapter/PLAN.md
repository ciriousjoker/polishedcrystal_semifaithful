# Mobile Adapter multiplayer plan

Last updated: 2026-10-11.
Binding requirements: [REQUIREMENTS.md](REQUIREMENTS.md), including the latest
four-player target, matched-NPC performance at up to 400% speed, priority local
Select+B disconnect, independent saves and compatible state recovery without
libmobile. Durable transfer recovery and save journals are explicitly out of
scope. These requirements supersede older proposals below. Feasibility boundaries
are explicit and are not waivers.
The gameplay wishlist and architecture outlook below were clarified on 2026-10-11:
four-player coexistence is its sole absolute feature requirement. Other gameplay
features remain desired, subject to feasibility and discussion. Foundational
requirements remain binding. This update is planning only, not an instruction to
implement the new wishlist or deploy cloud infrastructure.
The user subsequently selected a local TCP-to-WebSocket bridge on each device
plus Cloudflare Workers/Durable Objects as the critical hosting direction. Keep
the working direct-TCP development workflow until its replacement is tested.
The current milestone-2 checklist explicitly includes the remaining adversarial
server and four-client performance/recovery tests below.
Follow-up: the user accepts the documented Free-tier quota as the initial budget
and approves local handling of frequent ROM polls. Lobby management is now
preferred in the Bridge app. A build-generated, embedded compatibility hash is
desired; the stronger claim that all semantically compatible versions necessarily
hash alike remains an unresolved feasibility goal, not an established property.
Status: milestone 1 complete; milestone 2 functionality is implemented, but its
strict frame-cadence acceptance remains open. Three-player lobbies and live
overworld presence are tested at 300%/100%/100% emulator speeds. Extra missed
world-update frames and the new four-player/matched-NPC acceptance work block
milestone 3. The current implementation still supports three total players.
Selected figures and three selectable manual-save scenes are complete. Save and
transport recovery checks pass.
Current follow-up evidence: research/stability-cadence-followup.json.
Earlier measured evidence: research/AVATAR_SCENARIOS.md and
research/avatar-scenarios-evidence.json. Historical milestone-2 acceptance:
research/MILESTONE_2.md and research/milestone2-evidence.json.
Working branch: `mobileadapter`.
Base commit: `8f3c7e9651e988d876e32a4c7ad9ef1bb76702a8`.

Manual testing workflow: `python3 mobile_adapter/tools/dev_session.py` now builds
and starts the local server on fixed TCP port 17423, launches the Mac client at
5% emulator volume, pushes stable ROM/save/config paths to Android's
Downloads/polishedcrystal_mobile_adapter and directly launches RetroArch there.
Every build gets a fresh start snapshot derived from an ordinary in-game save;
Android generates its own snapshot with the Android core. No Android launcher
app is wanted or produced. Verified on native M2 macOS and a physical Pixel 9 Pro:
both clients joined the same lobby through the Mac's LAN IPv4, displayed each
other and moved. See MANUAL_TESTING.md for commands, prerequisites and limitations.

The following workflow follow-up records the earlier milestone 1 behavior;
milestone 2 replaces the short-lived echo gesture with persistent lobby joining.

Manual workflow follow-up: terminal logs now separate local adapter readiness
from server CLIENT_CONNECTED / ECHO / ECHO_REPLY / CLIENT_DISCONNECTED events,
including peer IP and source-IP-based Mac/Android labels. The Android launcher
validates a fresh frontend log. Per user correction, normal Android commands
force-stop the app, publish the files, then launch the game exactly once using
ES-DE's RetroActivityFuture component and ROM/LIBRETRO/CONFIGFILE extras. The
previous task is cleared. Automatic core sideloading is disabled because it
caused an unwanted open/close cycle. Explicit --reinstall-core remains for core
updates; a SHA-256 plus app-installation marker detects when this is needed.
The normal game connection remains a Select+B-triggered short-lived echo;
there are no automatic joins, persistent lobbies or remote avatars until M2.
Follow-up verification: eight host deployment tests and nine Rust tests passed,
including a real CLI logging test. On the Pixel, the force-stop plus one explicit
intent loaded the fresh state and initialized the adapter. Separate native Mac
and Pixel ROM probes exercised the new server on an isolated test port; both
produced connect/request/reply/disconnect logs, accepted the echo and moved.
Evidence: host cache dev-session/log-proof/{server.log,mac.json,android.json}.
Always regenerate snapshots for the exact test ROM, including endpoint changes.

This document is the persistent working memory for the project. Keep decisions,
evidence, unresolved questions, and the next concrete step current as work
continues. Communicate findings directly to the user in German; repository
content stays in English. Do not create an upstream PR or push this branch unless
requested. Commit completed changes locally. Do not use the GitHub plugin.

## User intent and scope

- Develop ROM changes and a Rust server together on this branch.
- Start with a server on the local PC and two independent game instances.
- Design for multiple connected players from the beginning. The required
  lobby capacity is four total players, including the local player, so each ROM
  tracks three remote players. All four can share the same map and viewport.
  Player collections and explicit target IDs must exist even in the first
  two-player demonstration.
- Make remote players visible in the overworld.
- Facing another player and pressing A opens a shared interaction menu.
- Calling an online player through the Pokegear plays the existing call sound,
  closes the call screen, and opens that same interaction menu across any map
  distance. This is a game action, not audio calling.
- Support battle challenges, Pokemon trades, and gifts of ordinary items.
- Keep all feature assembly under `mobile_adapter/rom`, with only surgical
  conditional integration hooks elsewhere. Any exception needs explicit human
  agreement based on evidence of necessity. Tooling may live outside this folder.
- Treat this as a maintainable fork with minimal upstream changes. Prefer small
  conditional hooks and feature-owned tables. Do not relocate existing helpers
  as the default solution to the ROM0 limit.
- Normal Rust unit tests and assembly contract checks must run without starting
  a Game Boy emulator. Be precise about what those checks actually verify.
- Use native DoubleCherryGB as the primary emulator and integration backend.
  The user explicitly rejects installing Rosetta. Wine and BGB are not required
  for development or acceptance; do not install them or leave them as blockers.
- Local bridge applications for Android, macOS, Windows and Linux are now in the
  infrastructure plan. Full app packaging and hosted rollout are later delivery
  work; their transport/lifecycle feasibility must be proven early. Unrestricted
  lobby sizes and battles with more than two participants remain outside scope.

### Gameplay wishlist and architecture direction

Only the four-player row is an absolute gameplay feature requirement. Preserve
extension points for the remaining goals now, without building their UI or
speculative infrastructure. Any proposed compromise should be explained with
measured limits rather than silently removing a goal. The foundational quality
requirements in REQUIREMENTS.md continue to apply to all implemented features.

| Feature | Priority and architectural consequence |
| --- | --- |
| Up to four players at once, including the same map | MUST. Generic peer collections and three remote actor leases per client. |
| Start-menu connection indicator in the top-right corner | Desired. Three ascending vertical bars based on Material Symbol `signal_cellular_alt`, adapted to the GBC tile grid. Expose connection status independently of UI; exact offline/connecting/online/error visuals remain to be designed. Adapter attachment alone must not imply a joined lobby. |
| Move/run together in the overworld and talk | Desired. Preserve native movement, distinct peer identity and an explicit interaction target. Talking currently means the interaction menu, not free-text or voice chat. |
| Compile-time overworld hitbox toggle, potentially an in-game option later | Desired. Initially interpreted as collision with remote avatars, not disabling native terrain/NPC collision. Keep collision policy separate from transport/rendering; clarify any broader noclip intent before implementation. Default remains nonblocking. All combinations with multiplayer disabled preserve the pristine build. |
| Temporarily remove NPCs when peers need sprite resources; restore them on departure | Desired resource strategy. Allocate only for local visibility demand; use audited optional actors, preserve local state and event identity, and restore safely. Same-map membership need not reserve an offscreen sprite. |
| Adjacent-player actions: Battle, Trade, Gift item | Desired. Share typed, validated action requests and explicit participant IDs. Keep inventory and save authority local. |
| Call a player from anywhere | Desired. Reuse the common action menu and add context-specific actions without duplicating the transport/session model. |
| Quick travel to a called player | Desired, new. Separate a travel request from presence updates and ordinary movement. The receiving game validates map/story compatibility, destination freshness, a safe entry tile and native warp lifecycle. No packet may force an arbitrary warp or mutate progression flags. Consent and exact restrictions need design before implementation. |

Do not make local collision or travel rely on frame-perfect shared simulation.
Mixed emulator speeds, delayed snapshots and different local story states remain
supported constraints. Rendering bindings must not double as player identity or
action authority. Shared protocol capability/version checks should allow later
features and reject incompatible actions without changing the ordinary save.

### NPC eligibility and restoration policy

User-approved on 2026-10-11: protected-by-default eligibility, an audited
feature-owned allowlist, free/offscreen/distant selection and safe restoration.

The current generated `rom/replaceable.asm` table supports frame-local rendering
suppression only. `tools/audit_render_policy.py` recognizes pure-text scripts,
generic trainers and no-op decoration; runtime guards protect nearby actors and
unsafe scenes. This is not evidence that those actors can safely be deleted from
runtime object storage. Do not automatically reuse it as a resource-eviction list.

For native resource leasing, default every actor to protected. Keep a feature-owned
allowlist keyed by map and stable map-object identity, with a reason and priority.
Use analysis to propose candidates and detect script references; verify each
accepted rule and exception. Decorative/no-op and pure-dialogue actors can qualify
only if no external script or gameplay role depends on their continued presence.
Keep plot actors, active or triggerable trainers, shops/healers, followers,
puzzle objects, camera proxies and active movement/script targets protected unless
an explicit safe lifecycle has been demonstrated. A simple script prefix, sprite
type or offscreen position is insufficient proof.

At runtime, prefer an empty eligible slot, then an approved offscreen actor, then
the lowest-priority approved distant visible actor. Recheck current interactions,
script ownership and special graphics before leasing. Hold a stable lease to avoid
NPC flicker near viewport edges. Preserve the actor's current position, facing and
other required runtime state, plus its map/event identity. Suppress native respawn
temporarily without changing story/save flags; restore on release only if the
current native map/event conditions still permit that actor to exist. Never
resurrect an actor legitimately removed by game progression.

The build audit must detect changes to allowlisted object definitions, references
or map ordering and require review rather than silently reassign eligibility.
Unknown/new actors remain protected. If no safe allocation can display four
players, report the exact map/state and solve that case; do not silently weaken
four-player support. No map event data needs new multiplayer flags.

### Development experience and autonomous testing

Keep `tools/dev_session.py` as the single human entry point. The current workflow
is accepted for now; retain stable Android paths, scenario selection, correct
intent/config/save selection, 5% Mac volume, visible connection logs and clean
Ctrl+C/occupied-port handling as it evolves. The future version also starts the
local bridges and local backend. An Android bridge service is now planned; a
separate app merely to launch RetroArch is still unnecessary. Do not replace the
current working workflow during this planning-only follow-up.

Every multiplayer feature needs an agent-runnable test path with bounded waits,
semantic success/failure oracles, pinned tool/core versions, reproducible input
and fault seeds, and machine-readable artifacts. Agent acceptance must not depend
on someone unlocking a phone. Use native headless cores for automatic execution
and retain ADB as a separate real-device compatibility path.

The foundation already includes `make test-mobile-host`, native ROM probes,
ordinary `.sav` scenarios, current-symbol memory observations, images, transport
fault injection and cadence measurement. Improvements still needed: a unified
ROM-suite command, four-client/400% scenarios, the matched-NPC control, priority
disconnect and compatible-state recovery coverage, and server property/fuzz plus
resource-isolation tests. Add scenarios when implementing each future action.
Coverage of current probes must not be called coverage of the entire future game.

### Server ownership, lobby UX and hosting outlook

See [research/SERVER_ARCHITECTURE.md](research/SERVER_ARCHITECTURE.md) for source
evidence, compatibility policy, transport design, cost assumptions and the test
matrix. Keep Rust and a runtime-independent lobby state machine. The selected
target path is the ROM through libmobile to a device-local TCP bridge at
`127.0.0.1:17423`, then secure WebSocket to a routing Worker and a Durable Object
per lobby. The same ROM endpoint works on Android and desktop without patching
each device's LAN IP. The remote URL belongs in bridge configuration.

Treat routing and lobby ownership as separate modules now, with one Cloudflare
Worker project exporting both its public handler and lobby DO class. The Worker
resolves a room and forwards the WebSocket upgrade; the DO owns its accepted
connection events, membership and rules. Cloudflare activates objects on demand;
no per-room deployment or separate publicly addressed server is needed. Bridge
keeps the original upgraded socket; no per-message Worker relay or second direct
DO connection is planned. The creator determines the placement reference: use
Cloudflare's automatic default placement on the creator's initial activation,
before publishing the invitation. No custom regions, location hints or placement
optimization are planned. Avoid deployment-time room precreation. All members
share that room location; later joins do not relocate it. Measure latency before
acceptance. Extract
runtime-independent lobby rules from the current Tokio server and share them
between native and Cloudflare adapters. Test the actual Worker/DO path locally
with pinned Wrangler/workerd during milestone 2, not only a WebSocket mock.

The completed backend must have a single repository deployment command,
provisionally `make deploy-mobile-cloudflare MOBILE_ENV=production`, after one-time
authentication/configuration. It builds/checks and jointly deploys the Worker and
DO class/bindings, then reports the Bridge's HTTPS/WSS origin and verifies a smoke
test. Version toolchains/configuration, isolate staging/production namespaces and
test safe session recovery across deployment. This command is not implemented yet;
the current request records its delivery contract, not an immediate deployment.

Share bridge framing, limits and reconnect behavior in a Rust core; use thin
platform wrappers for Android service lifecycle and desktop startup. Retain the
native backend for existing development/tests and add a local WebSocket/Worker
test backend. Bridge and backend listeners must use distinct ports in local tests;
do not bind both to 17423. The one-command script orchestrates them. A publicly
hosted raw-TCP gateway and Spectrum are unnecessary for this selected path.
The user accepts the documented Free-tier quota as sufficient for the initial
deployment target. Measure usage across rooms, keepalives, poll handling and 400%
traffic; handle quota exhaustion safely. The architecture report records the
assumptions. No paid upgrade or public deployment is authorized by this decision.
WebSocket setup consumes one Worker request and one DO request. Ongoing messages
consume no new Worker requests; DO incoming-message request accounting uses the
documented 20:1 ratio. Outgoing DO messages have no request charge. Track DO active
duration, API calls and reconnects separately. The public Worker address does not
imply a double per-message request bill; see the architecture report's breakdown.

The bridge is a critical compatibility boundary, not a stateless byte copier.
TCP reads may split or combine application frames; bound reassembly, WebSocket
messages and both directions' queues. Preserve ordered actions and the ROM's
delivery credits. The user approves terminating frequent local POLL requests in
the bridge so every emulated poll does not become a billed cloud message, without
fabricating server liveness or acknowledgements. Coalesce only obsolete presence, and measure
end-to-end latency and behavior through 400% speed.

The user approved this state model: clients own ordinary game state. The server
keeps only reconstructible session
coordination such as membership, routing, latest presence and pending interactions.
No database of authoritative saves/inventory is desired. A server crash interrupts
multiplayer, requires fresh session identity/resynchronization, and must leave
ordinary local play and saving intact. Active interactions may cancel safely.

Lobby creation, selection and joining are now preferred in the Bridge app instead
of requiring a new in-ROM options screen. The user runs the emulator and opens
Bridge; the local handshake identifies the running client. Optional local ROM
import/share can display embedded build/compatibility metadata, but must not be
mistaken for proof that this exact file is running. Compare imported metadata
with the live handshake and require a real server join acknowledgement before
showing online state. Ordinary game saves remain local.

A six-digit shared connection code in Bridge remains a candidate UX, not an
implemented password system. Keep a code distinct from
internal room/session identity, validate capacity, expire unused room incarnations
and limit attempts/creation. An idle-room timeout around ten minutes is a proposal,
separate from short peer liveness deadlines. Decide before implementation whether
users choose a shared code that creates/joins, or one user creates a unique room
code and others join. Address accidental collisions and guessing without treating
a million possible PINs as a strong authentication secret. Do not store new
multiplayer settings in the ordinary save without revisiting save independence.

Desired build metadata: generate and embed a compatibility digest only in enabled
ROMs, with format, protocol and game-profile identifiers readable by Bridge and
reported by the ROM. The requested inputs cover incompatibility-relevant game
code/data, not just multiplayer code. Define the actual coverage before claiming
the digest is sufficient. Raw ASM hashing cannot meet the stronger request that
every compatible revision necessarily has the same digest; includes, generated
data, binary assets and build flags also affect semantics. See the architecture
report for the proposed canonical manifest, conservative fallback and open limits.
Keep all embedded assembly under `mobile_adapter/rom`, avoid self-referential hash
inputs and retain the disabled-build identity. Lobby admission must validate the
declared profile at the server too; a client-controlled digest is no anti-cheat proof.

The initial emulator discussion included BGB with `libmobile-bgb`. The current
decision is DoubleCherryGB with embedded libmobile, running natively on ARM64.
Keep the ROM's adapter protocol libmobile-compatible. An additional BGB target
can be considered on another host later if explicitly requested.

## Feasibility assessment

This is technically plausible, but it is a substantial ROM feature rather than
a server-side wrapper around the existing cable link. A persistent connection
to one server can carry presence and interactions for several players. The
adapter does not need one physical connection per player.

The most important risks, in implementation order, are:

1. Fitting an asynchronous serial driver into an almost full ROM and preserving
   bank registers, interrupt behavior, and frame timing.
2. Keeping adapter activity alive through menus, scripts, animations, and battles.
3. Rendering three remote avatars without corrupting NPC slots, sprite tiles, or
   palettes on crowded maps.
4. Replacing cable-specific waiting and acknowledgement behavior in battle and
   trade paths with explicit network sessions.
5. Validating live gifts/trades and applying consistent local inventory updates,
   with ordinary user saving and no persistent multiplayer recovery journal.

Presence, calls, and menus are a reasonable first playable milestone. Battles
require a deeper transport adaptation. Inventory features should follow
validated live-session transfer handling. Reliable recovery across crashes or
independent save histories is explicitly not required.
No calendar estimate is justified before the serial and memory experiments.

## Verified baseline and resource limits

Terminology: feature-owned or "adapter" working memory means ordinary Game Boy
Color WRAM reserved for our ROM module. It is not memory inside the Mobile Adapter
and adds no hardware RAM. Packet buffers, peer records and graphics caches use
audited unused native WRAM ranges; see `rom/ram.asm` and the four-player report.

The base commit built successfully with local RGBDS 1.0.1 and was rebuilt with
the repository-required RGBDS 1.0.4. Both 2 MiB standard ROMs match the official
development asset for this exact source commit.

```text
SHA-256: 314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e
```

Measured from the baseline linker map, rather than estimated:

| Region | Free bytes | Consequence |
| --- | ---: | --- |
| ROM0 | 18 | A 12-byte dispatch hook fits; keep the handler in ROMX and measure interrupt cost. |
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
verification in DoubleCherryGB and an explicit compatibility
decision. First measure a minimal banked handler with a small resident dispatch.
Fixed-bank callers and interrupted banked code must remain valid. Keep new
sections and memory definitions feature-owned, with only the required includes
and conditional hooks in existing engine files.

Follow-up experiments are recorded in [research/FEASIBILITY.md](research/FEASIBILITY.md).
The preferred disposable experiment adds a 12-byte dispatch after Serial's
existing register saves and calls a banked placeholder through `farcall`. It
leaves six ROM0 bytes free without moving any existing helper. Simultaneous
reservations of 8 KiB ROMX, 96 WRAM0 bytes, 1.5 KiB WRAMX bank 2, one HRAM byte,
and 128 SRAM bytes still link. No production game source was changed. This
proves placement only; handler size, stack use, bank safety, and serial timing
remain adoption gates. The earlier 264-byte helper-relocation experiment is
retained as comparison evidence, not the preferred architecture.

Host readiness and exact rebuild commands are in
[research/HOST_ENVIRONMENT.md](research/HOST_ENVIRONMENT.md). A native ARM64
DoubleCherryGB core boots the baseline in three independent headless processes.
The native harness also creates a normal in-game save, loads it through Continue
in a fresh process, and verifies a completed tile step using current symbols,
save checksums, and captured video. No game engine changes are needed for these
observers. The adapter option is enabled, but an ISP/TCP exchange and multiplayer
behavior remain to be implemented and tested. BGB is not a prerequisite.

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

In the optional BGB setup there is one extra boundary: BGB's link protocol connects to the separate
`libmobile-bgb` process. The Rust game server does not implement BGB's link
protocol and does not receive raw emulated serial interrupts.

Local setup, with intended game-server port 17423:

```text
DoubleCherryGB A/B/C -> built-in libmobile -> 127.0.0.1:17423
```

One independent core process and separate save/adapter configuration directory
per player. The Rust server accepts every client on the same listener. This does
not use RetroArch netplay or the core's multi-Game-Boy cable mode.
Optional future BGB setup on a compatible external host:

```text
BGB A, link listener 8765 -> libmobile-bgb A -> 127.0.0.1:17423
BGB B, link listener 8766 -> libmobile-bgb B -> 127.0.0.1:17423
BGB C, link listener 8767 -> libmobile-bgb C -> 127.0.0.1:17423
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
  returns control to ordinary gameplay. Transfers use volatile session state and
  valid local application; no persistent reconciliation or rollback is required.

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

A single authoritative maximum-capacity setting must drive ROM and server
constants and bounded storage. Use generic iteration and resource leases for
zero through three remote peers; no numbered-player implementation branches.
Investigate using eligible native NPC object/graphics resources with exact actor
restoration, rather than manually assigning a new fixed VRAM window per peer.
See research/FOUR_PLAYER_FEASIBILITY.md for the placement proof and remaining
allocator/renderer acceptance.

The server maintains a collection of players per lobby and routes each action
to an explicit target or participant list. A ROM keeps a bounded collection of
remote-player records. Never introduce a global "the remote player" for lobby
state. Within a two-person battle or trade, it is valid to populate the existing
single-opponent game buffers with the selected peer's data.

Separate these limits:

- Required lobby capacity: 4 total players, superseding the implemented limit of 3.
- Required ROM peer cache and visible avatar capacity: 3 remote players.
- Interaction participants: initially 2, explicitly identified.
- Battle participants: 2 under the existing battle engine.
- One exclusive battle/trade/gift session per player at a time.

The server collections can support a larger configurable lobby limit later, but
the current ROM advertises its capacity and rejects incompatible lobbies.
Capacity changes must pass resource assertions. Beyond the fixed packet/render
budgets, larger lobbies would need paged rosters and a bounded visible interest
set; changing a constant must not silently overrun a resource. Multiplayer battles
would require separate battle mechanics and are outside this plan.

Proposed message families, not a frozen wire specification:

| Family | Purpose |
| --- | --- |
| `HELLO`, `WELCOME`, `ERROR` | Protocol/build capabilities, identity, resume result. |
| `JOIN`, `LEAVE`, `ROSTER` | Lobby membership and full resynchronization. |
| `POLL`, `HEARTBEAT`, `PRESENCE` | Bounded event delivery and position/status snapshots. |
| `INTERACTION_REQUEST`, `ACCEPT`, `DECLINE`, `CANCEL` | Explicit caller/target/session and busy states. |
| `BATTLE_READY`, `BATTLE_ACTION`, `BATTLE_ABORT` | Party setup, ordered turns, timeout cleanup. |
| `TRANSFER_OFFER`, `PREPARED`, `COMMIT`, `APPLIED`, `CANCEL` | Volatile trade/gift agreement, validation and live-session status. |

Keep framing binary and explicitly serialized: protocol version, message type,
bounded payload length, request/sequence ID, relevant session identity, and
payload. Define byte order and the Polished Crystal character encoding.
Do not copy Rust structures or arbitrary in-memory assembly structures directly
onto the wire. IDs, sequence wrap, unknown commands, bad lengths, and stale
generations require defined behavior. Large Pokemon or party payloads use
bounded chunks with transfer ID, chunk index, total length, and integrity checks.

The target handshake must check gameplay compatibility, not just the displayed release
version. Faithful and standard builds have different mechanics and must be
rejected for incompatible interactions unless a compatibility policy is later
implemented. Include a protocol revision, gameplay/schema revision, and build
capabilities. Parties and gifts use Polished Crystal's own schema.
The current handshake only checks transport/lobby schema, capacity and the
standard/faithful flag. Identical multiplayer code does not establish safe
cross-revision maps, battles or transfers. Define and test compatibility profiles
as described in research/SERVER_ARCHITECTURE.md before supporting mixed revisions.

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

For the generic resource-leasing prototype, use available runtime object slots
plus an explicit object-slot-to-player binding table. `TEMP_OBJECT` or another special
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
The four-player target requires actual same-map coexistence; lobby membership
alone is insufficient. Per-map resource pressure must be tested, not hidden by
silently retaining a two-peer renderer. Start with an approved small avatar set
and standing/walking states, then
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
nonplayer runtime slots, one/two/three remotes require ordinary demand at most
eleven/ten/nine; a one-effect reserve lowers that to ten/nine/eight. Special
last-slot sprite graphics can impose a tighter allocation rule. Define a protected camera/cutscene
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
The two uninvolved lobby members remain connected and can walk or see busy
status; no spectator mode or multi-person fight is implied. Audit all `wLinkMode` branches
and give the new transport a distinct mode rather than accidentally selecting
an old cable path.

### Live-session gifts and Pokemon trades

The user has explicitly rejected a transaction journal in `.sav` and removed the
requirement for reliable cross-client recovery after interruption or unsaved-state
loss. Users are responsible for ordinary saving after receiving a gift or finishing
a trade. Do not add a sidecar, mandatory reconciliation on load, authoritative
server inventory or durable transaction database to restore that dropped scope.

Use explicit participant and operation IDs within the live session. Validate
recipient consent, current capacity, item quantities, allowed item types and the
complete incoming Pokemon representation before any local mutation. Preserve
last-usable-Pokemon rules and native inventory invariants. Use Polished Crystal's
actual Pokemon schema, including supported forms, moves, held items and mail.

Stage the complete change, then perform a bounded, consistent local application.
Reject duplicate or stale messages using volatile session state. An unaccepted or
invalid request must not mutate inventory. Disconnect must remain locally escapable
at every phase; it does not require proving that both clients saved the same
transaction. Do not guess a persistent rollback or replay transfers on reconnect.
After restart, each client's ordinary `.sav` determines its inventory.

Keep ordinary save functions and layout unchanged. Users may lose unsaved transfer
results or produce inconsistent histories by restoring old saves; this is accepted
scope, not a reason to compromise save-file validity or packet safety. Test consent,
validation, duplicate delivery, consistent local state and cancellation. Persistent
exactly-once application, global conservation after failures and recovery after
independent save rollback are not acceptance requirements.

## Planned code organization and integration seams

Keep the feature under one root directory named `mobile_adapter`:

```text
mobile_adapter/
  PLAN.md
  REQUIREMENTS.md        binding requirements, feasibility limits, acceptance gaps
  research/             evidence, capacity results, complete map-slot audit
  tools/                reproducible host-side capacity/audit scripts
  protocol/             versioned schema, vectors, generated-file rules
  rom/
    constants.asm       feature constants and bounded structure layouts
    ram.asm             dedicated WRAM/HRAM declarations
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
    scenarios/          two/three/four-client TCP scenarios and fault injection
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
| `engine/link/link.asm:LinkTrade`, `engine/menus/save.asm:SaveAfterLinkTrade` | Selected-peer trade path; preserve ordinary save semantics without a journal. |
| `home/item.asm`, `engine/items/items.asm` | Existing inventory operations called by gift logic after validation. |
| `engine/menus/save.asm`, `engine/events/checksave.asm`, `ram/sram.asm` | Audit save independence and compatibility; a multiplayer SRAM journal is not authorized. |

Start with a `MOBILE_ADAPTER` build flag and a separate ROM artifact. With the
feature disabled, the baseline ROM must remain byte-identical.
Check standard and faithful builds before expanding to cosmetic/debug variants.
VC/printer/cable compatibility must be explicit, not silently assumed.

## Rust server design and tests

The deployment/ownership explanation and assurance gaps are in
[research/SERVER_ARCHITECTURE.md](research/SERVER_ARCHITECTURE.md). Full coverage
means tested invariants and adversarial behaviors, not a line-count target.
Volumetric DDoS is infrastructure scope; an individual client's malformed or
abusive traffic must remain bounded and must not disable unrelated rooms.

Use Rust with an asynchronous TCP runtime such as Tokio. Keep the domain core
independent of the runtime and sockets. It consumes typed events and emits typed
effects. Inject time and ID generation rather than sleeping in unit tests.

One lobby owner processes membership and interaction transitions sequentially;
connection tasks perform I/O through bounded channels. This avoids a collection
of locks and makes crossed invitations deterministic. Movement may be coalesced;
transaction and battle events may not. Capacity violations and slow clients
have defined error/disconnect behavior. Lobby ownership is separate from network
connection count and can later be partitioned by lobby.

Presence and live transfer coordination can remain in memory. No durable
transaction database is required by the revised gift/trade scope. Bind loopback by
default. Configurable bind address/port supports later hosting. Public deployment
needs its own identity, abuse, persistence, and transport design; libmobile's raw
TCP path does not magically provide TLS. No cloud resources are created now.

Required normal unit tests:

- Binary round trips, exact vectors, endian handling, length limits, unknown
  versions/types, partial frames, combined frames, malformed chunks.
- Four-member roster, fifth-client rejection, leave/reconnect, stale generations,
  target lookup independent of slot order, and presence map/epoch ordering.
- A challenges B while C contacts B and D remains independent; crossed calls;
  busy, declined, expired, canceled and disconnected sessions; no event leakage.
- Battle readiness, duplicate/out-of-order turn submission, wrong participant,
  timeouts, and compatibility rejection.
- Gift/trade consent and preconditions, full bag/party, malformed payloads, stale
  or duplicate live-session messages, consistent local application and cancellation.

Add loopback integration tests using two, three and four lightweight TCP clients,
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

`make test-mobile-host` and `make mobile` exist. The unified `test-mobile-rom`
target remains planned; the current probes run individually. Automated DoubleCherryGB integration will cover
executed serial logic, modeled timing, rendering, menus, and end-to-end scenarios.
The baseline automation probe already runs through an explicit Python command;
see the host report. Add new scenarios with each feature. Human interaction is
not a prerequisite for running this workflow. The ordinary unit-test command
must not start an emulator.

Integration fixtures must be ordinary in-game battery saves loaded by the current
ROM, followed by boot/Continue and bounded state-driven input phases. Never reuse
a full CPU/WRAM emulator snapshot across ROM hashes. Preserve or migrate the
normal save schema, handle frontend-specific RTC data, and resolve test observers
from the matching current `.sym` instead of fixed addresses. Check semantic ROM
state, server facts, input-to-visible latency, and native-resolution PNGs at start,
intermediate movement, and final state. Frame checkpoints should be relative to
recognized phases, with event/timing conditions for jittered networking.

The native DoubleCherryGB frontend controls inputs, captures video, imports and
exports raw battery SRAM, and reads current-symbol WRAM/HRAM through published
memory descriptors. It avoids the pinned core's incorrect system/video-memory
accessor indexing. `tools/probe_emulator.py` regenerates its save through the
normal Save menu and verifies a fresh Continue and movement without injecting
CPU/WRAM snapshots. It checks both game-data checksums and an unchanged input
fixture. Timings are pinned to the baseline; feature scenarios need state-driven
phases, intermediate checkpoints, and real-time pacing for socket latency.
The detailed workflow and remaining work are in the feasibility report.

## Five milestones to complete the agreed project

These five milestones supersede the earlier eight-step breakdown. Execute them
in order on this worktree and branch, with small local commits. Each milestone
includes its implementation and relevant automated tests; do not defer all
verification until milestone 5. A checklist item is complete only when its
acceptance evidence exists. Five milestones are an organization of the work,
not proof that the full feature already fits or a calendar estimate.

The current target roadmap is the Mobile Adapter-enabled Polished Crystal ROM
plus a locally runnable Rust server for up to four connected players. Four-player
same-map support is the absolute gameplay requirement; adjacent interaction,
Pokegear calls, battles, trades, gifts and the wishlist above are desired outcomes
subject to feasibility and explicit discussion of compromises. Milestones 3 to 5
describe how to deliver and test those desired features if retained, not a claim
that the user refuses every feature tradeoff. Foundational quality requirements
are binding throughout. Use collections and explicit participant identities from
the beginning. Companion bridge apps and Cloudflare hosting are now part of the
infrastructure direction; their full rollout follows the early bridge proof.
Cloud deployment is not authorized by this planning request. Free-text/voice chat, shared
story progression, unrestricted lobby capacity, and battles with more than two
participants remain outside the previously agreed scope.

### 1. Working adapter connection, memory ownership, and test foundation

Status reaffirmed on 2026-10-11: complete for the recorded transport-foundation
scope. The acceptance evidence is in [research/MILESTONE_1.md](research/MILESTONE_1.md)
and its JSON summary.
The later four-player, frame-parity, priority-recovery, Bridge and compatibility
requirements are open milestone-2 work. This historical acceptance does not claim
that those expanded requirements or all future failure states are already covered.

Implement the `MOBILE_ADAPTER` build, feature-owned code/RAM sections, serial
ownership, bounded asynchronous Mobile Adapter commands, and an initial framed
protocol. Add the Rust workspace, codec/domain test separation, and a minimal
TCP endpoint. Connect from the actual game ROM through DoubleCherryGB's embedded
libmobile to that endpoint, receive a response, disconnect, and reconnect.
A standalone host library transcript or a synthetic test ROM alone cannot
satisfy this milestone.

Measure the real driver's ROM/RAM size, dispatch/ISR cycles, stack headroom,
bank preservation, and frame/audio cost. Keep existing helpers in place. The
12-byte dispatch and 8-KiB reservation are starting evidence, not a completed
implementation or a promise that all later UI and transfer logic fits. Establish
budgets for adapter buffers, peer records, interactions, battle data, and future
transient session records. Audit their lifetimes through existing menu/battle unions.
Recheck actual placement and budgets throughout subsequent milestones.

Extend the native frontend with bounded state-driven inputs, intermediate video
and symbol snapshots, real-time pacing, and isolated config/save directories.
Provide the distinct host-only and ROM integration commands, adapter transcript
vectors, and baseline boot/save/reload regression scenarios. Use normal battery
fixtures and the current build's symbols. Identify all narrow upstream hooks.

**Acceptance:** the feature-enabled game performs a verified loopback TCP
request/response with a clean adapter configuration and repeated reconnects;
normal controls remain responsive. Adapter absence, cancellation, bad packets,
and timeout safely return to offline play. Banks/registers and buffers pass
runtime checks. The disabled standard ROM reproduces its exact baseline hash;
existing build variants are checked for build and compatibility effects. No
helper relocation, mapper change, or borrowing unsafe UNION storage is silently
introduced to overcome a failed budget. Failed placement/timing is resolved here
before building substantial feature UI.

### 2. Four-player lobbies and live overworld presence

Expand the Rust server into the lobby/session service with bounded TCP queues,
explicit player IDs, connection generations, compatibility negotiation, and
reconnect/resynchronization. Add the ROM's connection/lobby controls, online
state, a three-entry remote peer table, coherent movement snapshots and avatar
bindings. The existing two/three-player proofs are intermediate evidence. Finish
this milestone with four actual emulator processes connected to one server,
including simultaneous 100%/200%/300%/400% speeds and a matched native-NPC timing
control. See research/FOUR_PLAYER_FEASIBILITY.md for placement evidence and the
generic object/graphics resource allocation to prototype.

Handle walking, facing, relevant movement modes, warps, connected-map transitions,
map-instance differences, and disconnect cleanup. Remote avatars are nonblocking
and do not execute NPC scripts. Keep story, inventory pickups, encounters, and
puzzles local. Implement the feature-owned replaceability table and temporary
NPC suppression/restoration; preserve required actors and story flags.

Use the all-611-map audit to guide capacity checks. Cover the crowded retained-
object and special-sprite cases dynamically, including Snowtop Mountain and
Cinnabar Volcano. Audit object slots, VRAM tiles, palettes, and per-scanline OAM.
Normal supported overworld scenes must actually display all three remote players;
a permanently hidden peer in an ordinary scene is not a completed solution.
If safe NPC suppression cannot meet that requirement, implement the necessary
rendering allocation here. Protected cutscenes or resource-critical effects may
temporarily defer avatars with a defined visible status and roster access; record
those exceptions explicitly rather than silently dropping maps from coverage.

**Acceptance:** four distinct players can join, move, change maps, leave and
reconnect; each sees all three peers in the same supported map instance/viewport.
The host model and real four-core scenarios demonstrate bounded queues and no
single-global-peer assumption. Report both full input-to-visible delay and
additional local-world-motion-to-remote-visible delay. The original full-input
200/300-ms targets failed because unchanged vanilla turning precedes movement.
The revised additional-delay gate is p95 below 200 ms locally or 300 ms with
approximately 100-ms client/server RTT. This is a changed acceptance definition,
not evidence that the original targets passed. Static all-map checks and representative dynamic
crowding/transition scenarios preserve required NPCs, restore suppressed objects,
and avoid tile/palette corruption.

**Current milestone-2 TODO and acceptance additions (2026-10-11):**

- [ ] Four real clients simultaneously at 100%, 200%, 300% and 400%, including
  the same viewport, crowded maps, joins/leaves, transitions and fifth-client
  rejection. Recheck generic peer/resource bounds and explicit target identity.
- [ ] Matched ordinary-NPC control: equal actor count, movement, geometry, camera,
  palette/effect load and input sequence. Compare per-frame world updates, gaps,
  input/audio timing and remote freshness; retain the pristine control separately.
- [ ] Codec fuzz/property tests and arbitrary model event sequences with
  reproducible seeds and fake time. Cover partial/coalesced frames, invalid fields,
  stale generations, wrong participants, identifier exhaustion and time advance.
- [ ] Bounded resources and fair service with one aggressive client: valid and
  invalid messages, slow readers/writers, socket admission, log pressure and queue
  saturation. Healthy clients behind the same NAT and at 400% must remain usable.
- [ ] Repeated connection churn, expiry, server restarts and fresh room/session
  incarnations. Verify recovery, priority disconnect and native save independence;
  an explicit exit must not silently reconnect or replay an old action.
- [ ] Cross-lobby isolation: one room's hostile traffic, errors and disconnects
  neither reach nor starve another room. Distinguish logical isolation from
  process-wide outages and test both at their intended boundaries.
- [ ] Bridge foundation proof: bounded TCP/WSS framing, ordering, credit handling,
  local POLL semantics and loss/reconnect in a headless local multi-client suite.
  Establish the shared lobby-core/native/Cloudflare boundaries and exercise the
  real local Worker-routing/DO adapter with pinned Wrangler/workerd. Record the
  joint deployment configuration and one-command delivery path early.
  Demonstrate same-device loopback with Mac and Android libmobile, then Android
  service survival while RetroArch is foreground. Record feasible service type,
  launch/timeout rules and remaining desktop packaging work. No public deployment
  is needed for this milestone's proof.
- [ ] Define transport/gameplay compatibility profiles and reject unknown unsafe
  combinations. Existing schema/mechanics checks are not a cross-ROM-version proof.
  Investigate the desired generated/embedded hash, coverage and reproducibility;
  test Bridge-visible metadata and live-client matching without claiming automatic
  recognition of all semantically compatible source changes.

These are open acceptance work for the current milestone, not a later backlog.
Bridge installation/UX for all four platforms and the hosted rollout remain
delivery work after the early proof. The other recorded emergency-exit and save
acceptance requirements also remain open; this list does not replace them.

### 3. Adjacent interaction, Pokegear calls, and the common action menu

Implement target selection for adjacent or overlapping peers, invitations,
accept/decline/cancel/timeout, online roster, call sound, screen restoration, and
one shared interaction-menu path. Calls work across map distances. The menu
routes challenge, trade, and gift actions by explicit player/session IDs; those
feature bodies are completed in milestones 4 and 5 rather than represented as
working features at this stage. Talking means this menu flow, as agreed.

Keep the adapter serviced in menus and scripts through safe bounded hooks.
Queue incoming calls until a safe foreground state, report busy/offline, and
resolve crossed invitations deterministically. Each player can have one
exclusive interaction, but lobby membership and the peer collection remain
independent of the selected interaction partner.

**Acceptance:** facing a peer and calling that same peer both open the same menu
for the correct ID; calls ring and close/restore their screen correctly. In a
four-player scenario, simultaneous requests, refusal, cancellation, expiration,
map changes and disconnects do not target the wrong player, steal an active script,
leave a stuck menu or disconnect either uninvolved player. Unit and
emulator scenarios verify intermediate UI states as well as completion.

### 4. Complete two-player battles through the server

Adapt the selected-peer battle setup, party/RNG exchange, and ordered turn
actions to explicit mobile sessions. Reuse Polished Crystal's existing battle
mechanics; the Rust server routes and coordinates events rather than becoming
a second battle simulator. Validate build/gameplay compatibility, sequence
numbers, readiness, turn progression, and state agreement.

Replace cable-specific waits and mobile error/SoftReset paths with bounded
network waits, clear cancellation, and safe cleanup. Keep adapter activity and
busy presence through battle animations; both uninvolved players' lobby
connections and ordinary gameplay continue. Preserve game state according to existing link-
battle rules and restore a consistent offline state after an aborted session.

**Acceptance:** two selected clients complete a real battle with consistent
outcomes while two others remain connected and can move. Party setup, supported
battle mechanics, voluntary exit, latency, duplicate/stale events, disconnects,
and desynchronization have automated checks. Abort returns both participants to
usable gameplay without a soft reset or persistent party/inventory damage.

### 5. Gifts and Pokemon trades, then complete-project acceptance

Implement consent, validated transfer payloads, volatile operation IDs and
consistent local inventory application. Follow the revised live-session transfer
semantics above. Save journals, durable distributed transactions and reliable
recovery after unsaved-state loss are explicitly excluded. Each user is responsible
for ordinary saving; no multiplayer metadata enters `.sav`.

Implement ordinary allowed-item gifts with quantity/capacity checks and recipient
consent. Implement Pokemon selection, both parties' confirmation, Polished Crystal's
supported Pokemon schema, held items/mail rules, trade evolution, last-usable-
Pokemon restrictions and animations. Exercise every selected pair in a four-player
lobby while the uninvolved players remain connected. These must be usable ROM
features rather than disabled menu entries or host-only simulations.

Inject malformed/stale/duplicate messages and disconnect at each live phase.
Require coherent local inventory, safe exit and valid ordinary saves. Check that
saving and reloading preserves completed local changes, and that reconnect/load
does not replay or reconcile old transfers. Do not require global conservation,
cross-client atomicity or exactly-once recovery across independent save histories.

Finally run the four-client suite: connect, meet on the same map, walk, interact,
call across maps, battle, gift, trade, priority disconnect, automatic reconnect,
server/client restart and compatible connected-state restore without an adapter.
Recheck matched native-NPC frame timing through 400%, memory/graphics budgets,
ordinary save compatibility and the disabled-build hash. Cover the supported build
matrix and reject incompatible peers.

**Acceptance:** every requested action is usable in the actual ROM. Invalid input
and ROM-controlled failures preserve coherent local game structures and valid
saves; unsaved transfer outcomes follow the user's normal saving responsibility.
Required host-only tests and separate native integration scenarios pass. Deliver
reproducible builds, server/client commands, configuration, documentation, source
pins and evidence. Fresh-checkout instructions work with the native dependencies.
Deliver the one-command Cloudflare backend deployment interface and joint Worker/DO
configuration, including reproducible build/checks, environment setup and endpoint
output. Local adapter tests must pass; an actual cloud rollout remains a separate
user-triggered action. Device Bridge packaging uses that deployed origin.
Commit locally on `mobileadapter`; no upstream PR, cloud publication or external
deployment is part of this task. Only then is the agreed project complete.

## Execution policy and current blockers

Milestone 1's actual-ROM transport, disabled hash and failure checks have passed.
Current acceptance blockers are milestone 2's four-player expansion, matched-NPC
frame timing through 400% and priority cancellation/recovery requirements. Record
new evidence here before compaction. Historical three-player results do not prove
the new acceptance criteria.

The existing native proof needs no Rosetta, Wine, BGB, cloud account or companion
app. The selected future path adds local bridge wrappers and eventually a
Cloudflare deployment; its core tests must also run locally. Full ROM fit,
VRAM/palette placement, crowded-scene rendering and native timing remain technical
work. Durable transfers are no longer required. If a measured hardware budget
conflicts with a requirement, report the concrete case and design tradeoff instead
of silently weakening the requirement.

Work on this branch, implement and test each vertical slice, update this plan
with results, and commit it locally. Preserve milestone 1's actual ROM-to-Rust
round trip. Reuse the verified native harness and feature-owned section layout.

## Evidence and external references

Investigated on 2026-10-10. Pinned external source checkouts and host builds are
cached outside the repository. Their revisions and rebuild steps are documented
in `research/HOST_ENVIRONMENT.md`; they are not vendored into this branch.

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
- [x] Demonstrate a 12-byte resident dispatch without relocating existing helpers.
- [x] Rebuild the exact baseline hash with RGBDS 1.0.4.
- [x] Build libmobile, the BGB bridge, and a native DoubleCherryGB core.
- [x] Boot three independent native emulator processes through a headless probe.
- [x] Select DoubleCherryGB as the primary backend; exclude Rosetta/Wine/BGB
  from current prerequisites at the user's request.
- [x] Automate New Game, ordinary Save, fresh Continue, symbol observations,
  checksum validation, and a completed tile step without modifying game code.
- [x] Audit all 611 maps and distinguish initial visibility from retained demand.
- [x] Specify replaceable-decoration policy, latency targets, and battery-save
  integration-test oracles.
- [x] Prove minimum ISP/TCP startup on native DoubleCherryGB, including
  fresh configuration and disabled or irrelevant telephone relay behavior.
- [x] Extend the native frontend with bounded state-driven phases, intermediate
  render checkpoints, real-time pacing, and server fault injection for features.
- [x] Allocate resident serial code and stable RAM with measured budgets and
  audit bank/interrupt safety.
- [x] Freeze transport-v1 framing and bounded single-request echo reassembly
  after the actual-ROM experiment. Add lobby message credits/chunking in milestone 2.
- [x] Establish initial crowding behavior, map-instance rules and avatar allocation
  for the existing three-player implementation.
- [x] Complete three-player presence, reconnection and 300% speed acceptance.
- [ ] Meet strict native cadence using existing idle time and measured bounded
  hooks, including the new 400% mixed-speed requirement.
- [ ] Implement and measure independent priority exit through menus, saves and
  later battles, including compatible connected states restored without libmobile.
- [ ] Prove corresponding enabled/disabled save compatibility and equivalent-state
  byte equality under injected transport failures.
- [ ] Expand capacity to four clients and three simultaneously visible peers with
  one capacity setting and generic resource leasing; audit RAM, VRAM, palettes,
  native actor restoration and packet bounds.
- [ ] Complete milestone 2's explicit fuzz/model, resource/fairness, connection
  churn, server-restart and cross-lobby isolation checklist above.
- [ ] Prove the selected device-local TCP/WSS bridge and define safe compatibility
  profiles as part of milestone 2; retain the existing human launcher meanwhile.
- [ ] Implement live-session gifts/trades with user-managed ordinary saving.
  Save journals and reliable cross-restart transfer recovery are out of scope.

Current implementation step: complete milestone 2's checklist, including four
players, matched-NPC cadence, adversarial server tests, the bridge proof and
priority recovery acceptance gaps in REQUIREMENTS.md. Milestone 3 remains
blocked. Its later scope is target selection,
invitation/acceptance/cancellation/timeout state and the shared adjacent/call
interaction menu. Use the existing peer collection and connection generations;
keep lobby presence independent of the selected action partner. Audit service
points through menus and scripts before introducing invitations. Recheck ROM
budgets: standard ROM0 has 7 bytes free and ROMX 5945 bytes overall. Continue in
this worktree and branch; do not create another branch or PR. Milestone 1 is
complete; the revised milestone-2 acceptance is open.

## Milestone 1 implementation checkpoint

Completed 2026-10-10. The production game connects through native DoubleCherryGB
and embedded libmobile to the Rust server, checks an echo, disconnects and
reconnects. Final standard acceptance passes 14 native processes across 12
scenarios, plus faithful reconnect and disabled Save/Continue regression.
Host-only tests pass without an emulator. No upstream helper relocation or
mapper change was needed. New use: 16 bytes ROM0, 1,402 bytes ROMX, 34 bytes
WRAM0 and 676 bytes WRAMX bank 2; all 23,373 compared existing RAM symbols retain
their addresses. Disabled standard hash still matches exactly. A read-only
profiling core measured 620 serial interrupts, zero preservation failures and
190-byte observed stack headroom. Future UI/battle paths need their own scenarios.

Read [README.md](README.md) for commands, [research/MILESTONE_1.md](research/MILESTONE_1.md)
for budgets/limits and [protocol/transport-v1.md](protocol/transport-v1.md) for the
wire contract. Raw final artifacts are under
`/Volumes/Storage/Cache/mobileadapter/m1-complete/`; the baseline battery regression
is under `m1-baseline-regression/`. Use the normal core for functional acceptance
and `build/doublecherry-profile-v2/` only for optional observation. Do not install
Rosetta/Wine/BGB. The adapter timeout callbacks use emulated CPU clocks; actual
sockets use host time. Developer Select+B runs the proof only in a safe idle
map state and consumes that chord to avoid the ordinary registered-item script.

## Android frontend configuration checkpoint

On 2026-10-10 the real Pixel RetroArch frontend revealed a configuration bug that
headless adapter tests did not cover. Its base configuration had
`global_core_options = "false"`, selecting an existing
`DoubleCherryGB/DoubleCherryGB.opt` with the adapter disabled instead of our
explicit `core_options_path`. Manually loading the ROM worked because the
game-specific options were enabled in that workflow. The isolated development
profile now forces `global_core_options = "true"` together with
`game_specific_options = "false"`; existing global and per-core user files stay
intact. RetroArch 1.19.1's `runloop_init_core_options_path` confirms this precedence.

Do not infer attachment or network readiness from `[Mobile] Adapter initialized`:
the core constructs libmobile even when its option is Off. The attachment popup
is OSD-only and need not appear in logs. Startup validation reports only core
initialization and successful fresh state loading. Isolated profiles also enable
core debug logging to preserve actual libmobile protocol evidence.

After the fix, a single force-stop and explicit game intent loaded the fresh
state. Holding the real GUI's B control while sending Select completed TCP connect
to `192.168.2.25:17423`, transmitted the transport-v1 `TEST` echo, received its
matching reply and completed TCP/PPP/session teardown. Raw evidence is in the
host cache's `dev-session/gui-proof/android-retroarch.log`. Eight host-only
deployment contracts pass. Keep testing the actual GUI option-loading path as
well as the headless ROM harness when changing deployment.

## Milestone 2 implementation history

Historical checkpoints below retain intermediate failures and superseded figures.
The final acceptance record belongs in research/MILESTONE_2.md and its evidence
JSON. At the time of this initial checkpoint M2 was in progress. Native ARM Mac, branch
`mobileadapter`, no Rosetta/Wine/BGB or upstream publication. No M2 commit yet.

Implemented: Rust rooms with three independent players, nonzero IDs, generations,
resume tokens, bounded latest-state push/delta updates, explicit leave and lease
expiry. ROM controls: Select+B persistent join/leave, Select+A status/roster,
Select+B+A echo diagnostic. Two feature-owned peer records and approved Chris
avatars use dedicated tiles and OAM, no NPC slots/collision objects/scripts.
Physical block-layout keys are recomputed after map entry and paused scenes.
All 611 maps have an assembled policy audit; 407 maps qualify for optional
rendering. Required and nearby actors are protected. Temporary facing changes
are restored, and final OAM also has a ten-sprites-per-scanline capacity check.
Large Gyarados/Exeggutor, protected scenes and unavoidable required-NPC pressure
have visible status and retain roster membership.

Networking: independent TCP reader/writer, one watch notification and latest
state per connection, 8-ms coalescing, full membership roster and changed-peer
DELTA. Empty application POLL avoids pinned libmobile's one-second empty receive
wait. POLL is ignored without a reply or presence lease extension. Changed
snapshots and 500-ms keepalives supply presence. 48-byte serial burst budget,
feature-owned WRAM trampoline, IRQ clock gating at VBlank boundary and a bounded
32-byte foreground parser. Profiling observes real instruction preservation,
stack and actual VBlank intervals, including a separate 20-cycle IRQ-entry charge.

Acceptance evidence in host cache: `m2-local-final-v2` measures 40 actual input
poll to native video OAM samples, p95 165.332 ms; `m2-rtt100-final` measures p95
281.585 ms at declared 50-ms uplink + 50-ms downlink application-hop delays.
Profiling passes registers/banks/stack and serial below half a double-speed
physical frame. Menu, explicit rejoin, warp and dropped-socket token resume passed
in `m2-accepted-*`; resume keeps IDs 1,2,3 and increments exactly one generation.
The source has since received a held-chord fix: keep physical joypad input intact,
filter mirrored input, and pump at MobileMenuInput after GetJoypad. Recheck final
latency/menu/echo tests with this fix. A prior Snowtop test exposed an unintended
registered-item dialog, and the initial fix revealed cancellation in a dense map.
Do not count those failed runs as acceptance. Ordinary save fixtures live under
`m2-scene-*`, including forced bicycle mode on Route17North and a Snowtop progress
variant. Save creation changes disposable producer ROM initialization only and
uses real New Game/in-game Save; no RAM/SRAM edits or emulator snapshots.

Passed with the final source: dense Goldenrod Game Corner pressure and exact
restore-boundary profiling, Snowtop camera and retained night windows, same-map
progress isolation, Cinnabar rocks, bicycle, connected maps, running, surfing,
large Gyarados deferral, menu, explicit rejoin, warp and token resume. Full 12-case
Echo regression passed across the primary run and the five fast-path cases.
The fast no-adapter exchange completes within one video observation: its oracle
uses attempt/error/IRQ count/cleanup/movement and two image checkpoints instead
of requiring a sampled intermediate ownership state.

Remaining: final-source local/RTT100 latency and faithful native three-core run,
then actual Mac/Android one-command deployment. Rust fmt/clippy, 18 host Rust tests,
RGBDS contracts, eight deployment contracts, standard/faithful/disabled builds,
23373-symbol RAM layout and exact disabled hash already passed. Record final
metrics, update evidence and locally commit after all gates pass. M3 calls and
interactions, M4 battles and M5 gifts/trades remain out of scope for that historical proof.

### Strict positional latency and fast-forward gate

The user's 300% emulator-speed requirement is part of M2 acceptance. The
absolute snapshot/session model must preserve identity, map and final positions
with one client at 3x speed; visual jumps are allowed. The native runner now
supports a 3x paced client and the lobby probe has a speed scenario.

The earlier 171/277-ms final latency figures included sprite facing changes.
A strict four-quad bounding-box observer excludes pure turns. On the same raw
traces, actual first motion is p95 242.265 ms local / 334.762 ms RTT100, failing
the agreed positional targets. Do not declare M2 complete from the old numbers.
A one-frame upload cooldown alone still fails local p95 245.449 ms. A reduced
32-byte serial burst is currently under test to reduce foreground frame pressure.
No new actual GUI deployment or M2 commit has happened yet. Most functional
scenes passed with the prior 48-byte/four-frame-cadence source; rerun relevant
acceptance after the final transport choice. Fresh output directories retain
all failed traces. GUI deployment and updated final evidence remain mandatory.

The 32-byte experiment also failed strict local p95 (247.375 ms). Current source
restores the 48-byte budget and packs schema-2 snapshots into 10 wire bytes:
map/instance/epoch stay explicit, X/Y use 12 bits each, facing/movement/status
share one byte and approved avatar 0 is implicit. Peer wire records are 16 bytes;
the ROM expands them to its existing 20-byte internal records. A separate
14-byte feature-owned last-upload snapshot supports change detection, raising
WRAMX allocation to 1473 bytes. Compact roster expansion uses the last 16 bytes
of the private 128-byte APP buffer only after validating its maximum 45-byte
packed roster. No existing game RAM or helper moves. The packed local profiler
run is in progress; all scene/transport/fast-forward gates must be repeated or
revalidated with the final format before claiming completion.


### Slow receivers and measured latency semantics

Packed local strict input-to-motion still failed (p95 249.237 ms). Bounded idle
IRQ chaining did not improve it (261.738 ms under a heavily loaded host) and was
reverted. Do not claim these input-to-remote tests pass the original 200/300-ms
target. A local direction turn consumes several unchanged vanilla frames before
world motion. The observer now reports both input-to-remote time and additional
local-world-motion-to-remote-visible-motion time. The latter has explicit p95
200 ms local / 300 ms RTT100 gates. This measurement change was explained to
the user, not hidden as a transport improvement.

The first actual 3x/1x/1x run passed (`m2-first-speed3`), but used the intermediate
idle-chain source. Final acceptance extends this to 24 actual direction changes
and 100-ms declared RTT. The server now bounds output with one coalescing receive
credit per client's PRESENCE/POLL, preventing a fast sender from accumulating
old movement in a slow client's TCP stream. A new host test sends 2000 updates
to a receiver without credit, verifies no queued output, then receives the exact
latest coordinates after a credit. Control responses retain their write timeout
and priority. Final source and GUI acceptance still remain outstanding.


Latest final-source gates: `m2-settle-local` passed 40 strict motion samples with
network/render p95 88.042 ms and input-to-remote p95 224.826 ms.
`m2-final-speed3-rtt100` passed 24 turns plus a return to central view, using
one 3x and two 1x actual cores, bounded TCP propagation delay (50 ms each way),
matching final IDs/generations/maps/absolute positions, guards and IRQ/bank/stack
checks. A sequential 50-ms application delay incorrectly throttled upstream
updates and is not used in acceptance. Both old failures remain diagnostic.
The finish barrier now pumps clients until all are ready rather than freezing
the earliest client and expiring its lease. One failed speed scene ended on the
room edge with a legitimately offscreen peer; the final input returns centrally.

Final standard hash 8260d333036e79d03fb3e8a0ba6f61b2efbf01277b7cc3dd24208f87be3fd200;
faithful 01427d6483fd31c62dd5577d2dfa443fcdc16516e9573a323b2e2570be2ed34f.
RAM symbols unchanged, resident 121, buffers 1473, driver 6926 bytes.
20 Rust tests and fmt/clippy passed. Final profile scene batch (port 17427),
functional scene batch (17429), RTT100 latency (17425) and full Mac/Android
deployment (17423) are running. Pixel reconnected after the user confirmed USB.
Remaining: collect all outputs, rerun full Echo suite, actual GUI join/visibility,
final documentation/evidence and local commit. No M2 commit yet.


### Intermediate M2 source checkpoint with 32-byte parser budget, 2026-10-11

The final foreground split keeps adapter checksum verification, APP parsing and
next-request preparation in separate pumps. APP consumes at most one completed
frame and 32 bytes per pump. Dense Game Corner passed the original 32768-cycle
Tick gate: max 23800 cycles in the first final-source run, no bank/register or
NPC-restore failures. This replaces the failed 34632/36848-cycle experiments.
Standard ROM SHA-256 24defd27ef3ab5fd54390c31c4d798c1828caf66dab22a8489ed27b11de637a9;
faithful 0dbd013de1a9a2ff67215a0cb3f2345aac9babceaebab43d309e5268bc54b55b.
Driver 6922 bytes, resident 121, WRAMX 1473, existing 23373 RAM symbols unchanged.

A live RetroArch Mac + physical Pixel GUI session on this source produced two
LOBBY_JOIN IDs and visible avatars on both screens. Actual arrows and Android
touch D-pad moved the players. The Mac GUI observer used a disposable F2 mapping
to send both Select+B because the UI automation cannot reliably hold a modifier
as an emulator button. Normal launcher controls remain left Shift+Z, and the
regular config uses RetroArch's correct key name "shift". The isolated Mac
profile now disables pause_nonactive so terminal use does not expire presence.
Proof artifacts: m2-gui-observer-{server.log,input.json,mac.log}, m2-gui-phone3.png.

The camera profiling gate exposed vanilla SP-as-copy-pointer use in home/video.asm
and home/gfx.asm, not a game stack overflow. The read-only observer now excludes
below-c000 SP only with IME disabled and PC in four exact ROM0 ranges configured
from the matching SYM. It records excluded sample count/minimum separately.
All other SP values, register/bank restoration and original stack floor remain
checked. Profile scene reruns and full Echo regression are running. Normal-core
menu, rejoin, socket resume, warp, connected border, running, bike, surf and
faithful scenarios have passed final-source three-core acceptance. Latency must
be repeated after these batches with the normal core. Still no M2 commit.

### Accepted M2 checkpoint, 2026-10-11

Milestone 2 is complete. This checkpoint supersedes all implementation-history
hashes, budget figures and remaining-test lists above. Authoritative acceptance:
research/MILESTONE_2.md and research/milestone2-evidence.json. No PR or push.

Final standard SHA-256
725d94ee2e8adec0817a9398e9fa3a1056908943e429ccbdaa335384f3a7e3e8;
faithful 81e0664967741a11c30143379985df231bb97b2ef00fdde9049349664bbf36d7.
Fresh disabled build retains exact baseline 314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e.
Driver 6924 bytes, WRAM0 121, WRAMX bank 2 1473; 23373 existing RAM symbols
unchanged. Standard free ROM0 7 and ROMX 6983. Existing helpers stay in place.

Final pump consumes at most 16 APP bytes and one complete application frame;
adapter checksum, APP parse and next request preparation are separate phases.
The profiler excludes native graphics copy cursors only in ten exact PC/bank
ranges with IME disabled and retains raw excluded metrics. Final maxima: ISR
1432 cycles, Tick 25268, serial work per VBlank 67840, lowest true SP c091.
All original runtime timing, preservation and stack gates passed. No NPC facing
restore failures. Nineteen native processes have instruction profiles.

Fifteen three-core profile/functional scenarios, two three-core latency scenarios
and twelve Echo regression scenarios passed: 65 native processes. The 3x case
uses one core at 300%, two at 100%, 24 movement bursts and 100-ms client/server
RTT. Absolute positions/maps/identities/generations converge. Additional motion
delay p95 is 93.879 ms local, 198.846 ms at RTT100. Full input delay p95 is
244.252/350.064 ms; the original 200/300 full-input targets failed and were
explicitly revised to additional delay after native movement begins.

Twenty Rust and eight deployment tests passed, as did RGBDS/layout/vector and
renderer audits, Cargo fmt and Clippy. Generated policy covers 611 maps, 407
with optional actors. Current test-ROM endpoint/checksum copies were normalized
and matched the final standard or faithful compiler output, excluding stale
intermediate acceptance artifacts.

Final real Mac and Pixel RetroArch GUI proof: both joined lobby 1 and visibly
displayed the other player after arrows/touch movement. Pixel cold start worked
after unlock. Final host cache artifacts are m2-final16-gui-{server.log,input.json,
mac.log} and m2-final16-phone-visible.png. Temporary F2 GUI automation mapping
remains isolated; normal controls are left Shift+Z / hold B then Select.
The command prepares fresh ROM/core snapshots, fixed port 17423, 5% Mac volume
and stable Android paths. Owned GUI/server test processes were stopped afterwards.

Next work is milestone 3 only when requested: adjacent/Pokegear target selection,
invitations and one shared interaction menu. Presence can be reused, but battle
and gift/trade behavior are not implemented or covered by the 3x proof.


### Selected-avatar and manual-scenario follow-up, 2026-10-11

User requested actual selected local/remote figures, three selectable manual
scenes committed with ordinary saves, and an explanation/test of Start-menu
avatar disappearance. Follow-up complete after M2 commit 33aecff32. Keep working
in the same worktree and branch. Detailed current acceptance is in
research/AVATAR_SCENARIOS.md and research/avatar-scenarios-evidence.json.

Current changes: lobby schema 3 (11-byte snapshot, 17-byte wire peer), explicit
wPlayerGender 0..3, native PlayerStateSprites table including running/bike/surf,
independent 24-tile caches at bank 2 dac0..ddbf, four current-frame VRAM0 tiles
per peer at 64..67 and 68..6b. Native MarkUsedPal is exported (no code moved) and
used only when there is free palette capacity or an exact loaded match. Palette
exhaustion defers a peer rather than overwriting a required actor's palette.
Ordinary saves are in mobile_adapter/tests/scenarios with a hash/provenance
manifest. Launcher prompts for bedroom, game-corner or goldenrod-city and supports
--scenario, --mac-avatar and --android-avatar; default Mac Chris / Android Kris.
Auto-states remain regenerated from the selected ordinary saves per current
ROM/core. Starter spawn was corrected to 3,3 so the downward test step is clear.

Start-menu pause is deliberate: vanilla maintains a still local background and
clears sprites under its panel. Remote players have independent movement. The
menu uses wScriptRunning=255 in this path, not hInMenu=1. Actual paused frames and
two restored figures pass in both bedroom and Game Corner. Eleven final-source
cases pass with 33 native processes: mixed four-choice figures, crowded/moving
NPC scenes, Start menus, 3x speed at RTT100, native running/bike/surf, faithful
gameplay and 40 local latency samples. NPC motion is checked per stationary
client with unchanged actor membership. Six profiled cores show zero preservation
or NPC-restore failures, max Tick 25392 / ISR 1432 / VBlank 67840 cycles, minimum
SP c0ac. Local movement p95 is 100.643 ms; input-to-remote p95 276.534 ms includes
the native turn delay. Rust 20, deployment 8, scenario 6 host tests, Clippy/fmt,
production RGBDS vectors and renderer audit pass. make test-mobile-host now also
includes the deployment/scenario suites without starting an emulator.

All accepted endpoint/checksum test copies normalize to current standard SHA256
3bafac80ecea0d7da588f0b42ec7bc52ff1a703a60bb8708841ca03b791d1f87 or faithful
6cd3a3997c33bb04bee19abbeb70c55d2c13e42077a9b8df7d1498e4966ee657.
The fresh disabled rebuild still matches the exact baseline. Existing 23373 RAM
symbols are unchanged; driver 7196, resident 122, protocol/OAM 1473, new graphics
768 bytes. Bank-2 graphics end ddc0, leaving 576 tail bytes. Current Mac/Pixel
Game Corner preparation has Chris/Kris respectively; read-only live screenshots
also show their correct local and remote figures. The user's manual session
was left running. No upstream PR/push. Next work remains M3 when requested.

### Binding project invariants, clarified 2026-10-11

The expanded, current wording and the compatible-savestate clarification are in
[REQUIREMENTS.md](REQUIREMENTS.md). In particular the required speed target is now
400%, the player count is four, and frame parity uses an equivalent native-NPC
control. Durable transfer recovery is no longer required; historical evidence
below does not satisfy the new acceptance criteria.

- All feature assembly stays in mobile_adapter/rom. Existing game assembly may
  contain only surgical conditional integration hooks; do not move existing
  game helpers or introduce multiplayer implementation outside that directory.
- One build switch, MOBILE_ADAPTER (make mobile), includes the feature. A build
  without it must match the corresponding pristine upstream build byte for
  byte, including SHA-256. Tooling may be elsewhere. Compare equivalent standard
  variants: only baseline/disabled and enabled should be distinct ROM outputs.
- Connected gameplay must retain native world-update/frame cadence compared
  with equivalent ordinary NPCs at the same rendering load. CPU overhead is
  acceptable only within the existing frame budget. Report every observed
  deviation to the user, including isolated
  missed frames. Do not accept a relaxed FPS threshold silently.
- Save integrity is independent of multiplayer connection state. Adapter
  work buffers must never alias ordinary save data or native save scratch RAM;
  invalid packets, disconnects and transport failures must degrade to offline
  gameplay without resetting/crashing the game or writing a damaged save.
  Save while connected and disconnect/malformed-packet recovery require explicit
  integration tests. Later gifts/trades use validated live-session state and
  ordinary user-managed saves. Reliable transfer recovery across interruption or
  restart is not required. Never claim absolute crash freedom from a finite test
  suite; document what is protected and tested.
- Maximize error tolerance and performance while keeping the repository footprint
  and native integration surface minimal.
- The current performance follow-up is not complete. A strict matching-input
  baseline comparison now detects additional missed world frames, including
  individual gaps masked by catch-up updates. Do not accept total update counts
  alone or claim timing parity until this gate passes. Cold joins, dense maps,
  3x emulation, menus and RTT100 remain explicit timing cases.

### NPC cadence and development-session follow-up, 2026-10-11

Active user request after 28e9257b7: NPCs visibly stutter while moving online;
Ctrl+C must stop normally; an occupied fixed port should prompt before stopping
its owner. Work remains in the same mobileadapter worktree, no upstream PR.

Launcher changes are implemented: SO_REUSEADDR bind probe, lsof PID/name listing,
default-No interactive approval, revalidate listener ownership after approval,
SIGTERM only and five-second free-port wait. Noninteractive calls never kill.
Server/Mac children have isolated process groups so terminal Ctrl+C reaches the
parent, whose existing cleanup reaps both. Normal SIGINT/SIGTERM exits are not
failures. Eighteen host deployment tests pass, including a real listener restart
and real process-group Ctrl+C with two owned stand-in children.

Read-only profiling now observes MobilePrepareSprites/MobileRenderSprites,
DelayFrame within preparation and HandleNPCStep cadence in display frames
3600..4399. Core source/build: external cache sources/doublecherry-profile-cadence
and build/doublecherry-profile-cadence; runner smoke-libretro-cadence. An offline
Game Corner control has 800 world updates in 800 frames, max gap 1. The old-ROM
three-core speed reproduction (cache/cadence-before) has 452/408/407 updates,
max gap 3, plus 7/5 preparation DelayFrame calls on the two 1x clients. It also
exposes one old Tick cycle-budget failure and is diagnostic, not accepted.

Implemented renderer optimizations: bounded four-tile HBlank/early-VBlank
copies and post-copy frame publication; audited bank-local native OAM counts;
retained native OAM with a separate peer prefix; exact accepted Y-byte keys and
a page-aligned scanline delta table; optional classification only on overflow;
fast exact palette reuse; persistent graphics across menus; bounded four
foreground pumps per frame; fast restoration when no NPC facings were suppressed; final
actual native OAM-size guard; at most one cold avatar decompression per frame.
Foreground snapshot capture stays lobby-only; the duplicate tick at that hook
was removed. No native helper was relocated.

Timing remains unresolved. Full protocol work inside the VBlank wait reached
p95 739 ms and was rejected. Byte-only idle polling and foreground byte budgets
also failed cadence/latency gates. These are diagnostic experiments, not accepted
implementations. The HALT hook was reverted. Current code keeps normal serial
interrupts, a 24-byte budget and four foreground pumps. The parser now copies
bounded chunks up to the validated header/frame boundary instead of rebuilding
pointers for every byte. It copies at most 32 bytes and publishes one frame per
pump. Native save logic remains unchanged.

The serial-RAM experiment at de91 was removed. Its first prototype tripped the
native ERR_EXECUTING_RAM guard. A DI foreground wrapper fixed it, and all three
save-failure cases passed, with millions of checked IME=0/WBK=2 RAM instructions.
However it saved only 168 worst-case cycles per interrupt, consumed 311 extra
RAM bytes, and still failed cadence. It is not retained in the current source.

The VBlank GDMA queue experiment was also removed. An initial late hook starved
tile publication; the early hook fixed avatars and passed local p95 166.526 ms,
but still failed cadence (737/676/672 populated frames). It added a native hook
without sufficient benefit. The retained source has no new VBlank/HALT hook,
uses bounded foreground four-tile copies, and removes only the duplicate
HandleMapObjects tick from existing native integration.

The retained standard ROM is byte-identical to the chunk-parser save and latency
candidates after normalizing only endpoint and global checksum. Canonical SHA256
996e9f92541b83f08066c63e488b112872ae418283a93495093802cf874b66bd.
Local motion p95 164.775 ms, max 197.920 ms; dense 3x cadence 729/677/679 versus
797/800/800 baseline. Strict parity FAILS. No M3 work should start until the
world-cadence architecture is resolved; do not describe this follow-up as a
complete NPC performance fix. Earlier 27 fps reproduction improves, but the
user requires matching cadence, not an average-FPS threshold.

Matching-input pristine controls (cache/cadence-baseline-final) have 797/800/800
frames containing a world update per 800 frames, max gaps 2/1/1. Chris has three
pre-existing gaps and two catch-up updates. The user was told both the baseline
and every observed regression. The chunk-parser candidate passed local p95 164.775 ms but failed strict
cadence (729/677/679 populated frames). The removed RAM candidate passed local
p95 183.942 ms, with one 575.984-ms outlier, but failed cadence
(743/701/692). Final transport probes use final-transport-a,b,c (17614/17616/17618).
The retained source passed all three save cases in save-safety-chunks. Earlier failed experiments are
diagnostic only, including cadence-capacity which accidentally used a previous
ROM after a failed build. Do not include them as accepted evidence.

Current resident allocation is 125/128 bytes; buffers 1232, graphics 768 and
render scratch 190 bytes. Total dedicated RAM is 48 bytes smaller than
28e9257b7. No extra serial RAM code or DMA fields remain. OAM scratch is
reduced from 160 to 64 bytes; weather/scanline scratch share a 145-byte table in
separate passes. Native RAM/SRAM must not move. check_layout additionally checks scratch
aliasing and both OAM prefix blocks.

New probe_save_safety.py boots two ordinary bedroom saves, selects the real
Save menu online, then injects peer disconnect, server closure or invalid packet
magic. It validates both native checksums, unchanged saved data after failure,
continued gameplay, and fresh offline Continue/movement in new core processes.
All three cases pass on frozen occurrence and chunk-parser ROM copies in
cache/save-safety-occurrence, cache/save-safety-chunks and
cache/save-safety-ram-atomic (removed RAM experiment). No RAM/save edits or emulator states are used.
The frozen chunk candidate exactly matches the retained ROM. Existing deployment already backs up Mac/Android
ordinary battery files before replacing a test scenario; do not duplicate it.

Final validation of retained code: 44 host tests (20 Rust, 18 deployment, six
scenario), production assembly contracts and renderer audit pass. Standard and
faithful enabled builds pass; disabled SHA256 still matches 314001ec5a2dde...
check_layout preserves all 23373 native RAM/SRAM/HRAM symbols, allocates no new
SRAM/HRAM, and measures 7966 ROM bytes, 125 resident bytes, 1232 buffer bytes,
768 graphics bytes and 190 render-scratch bytes. Existing helpers stay in place.

All 12 transport fault cases pass in 14 native core processes in
cache/final-transport-a,b,c: reconnect, three-clients, split, bad-magic, silent,
close, no-server, no-adapter, cancel, recover, oversize and truncated. Profiles
check register/bank/stack and IME preservation; there are no failures or resets.
Start-menu, optional rendering pressure, ordinary NPC motion and reconnect with
resume identity pass in final-start-menu and final-resume. RTT100 movement p95
274.362 ms, max 275.571 ms, 40 directional video samples. Local p95 164.775 ms,
max 197.920 ms. Both latency gates pass. Both ordinary save slots remain valid
after all three online-save failure cases, and fresh offline Continue succeeds.
The saved-data tests use the exact retained ROM after endpoint/checksum
normalization; do not rerun merely because source comments changed.

Strict frame parity remains FAILED: dense three-core speed case has 729/677/679
frames with updates versus 797/800/800 matching-input pristine controls. Do not
mark this performance task complete. Next work must budget the combined serial,
parser and rendering costs, measure work per individual frame and move/avoid
work without trading away latency or native story rendering. No M3 until this
requirement is met. Experimental HALT, RAM-code and VBlank-queue implementations
are removed. Keep their rejected results as diagnostic history only.

Launcher fix committed as 1ae47d85a. ROM/tooling/evidence changes belong to a
separate local checkpoint with strict cadence explicitly left open. No upstream
PR or push. The existing manual
Mac/Pixel session on port 17423 was left running; no phone saves were overwritten.
