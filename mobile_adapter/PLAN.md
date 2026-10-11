# Mobile Adapter multiplayer plan

## 2026-10-12: hidden remote figures must survive native turns

A two-player Game Corner regression reproduced the reported flash: after the
remote figure became offscreen, each local facing-change event could resurrect
its last accepted native facing. The static menu preservation hook also runs
for ordinary turn events. `STANDING` hid only the immediate OAM build; the lease
still lacked `INVISIBLE`, so that hook treated it as a displayed frozen actor.

Both background hide paths now set the native invisible flag as well as the
standing facing. Normal successful presentation clears the flag; original NPC
restoration copies the original record. No native game assembly or foreground
hook changes are required. This adds twelve upper-bank bytes, with the same
174-byte net lower integration and original RAM layout.

The `warp-turn` acceptance case boots normal Game Corner batteries, walks a
player through the real exit, and repeatedly turns the remaining players before,
during and after departure. Read-only hardware OAM observations must show an
initial displayed figure, its disappearance, and no later resurrection while
hidden. The departed player's map and final released leases are also checked.
The default acceptance suite runs this case with both two and four players.
The old ROM fails at frame 3934; the final two/four-player gate passes. Native
Start menu, rejoin, offscreen return, connected Save and offline Continue pass.
All four 100..400% profiles contain 800/800 world updates, without duplicates or
background VBlank crossings, and beat their matched ordinary-NPC controls.
The disabled target retains the exact original hash.
See `research/REMOTE_TURN_VISIBILITY.md` and its evidence JSON for final results.
The separate protected-NPC scanline blocker remains open.

## 2026-10-12: interactive multi-window development launcher

The single Python command now asks for scenario, Mac instance count (1..4,
default 2), and an Android update/open choice only when ADB reports an authorized
device. Android adds a player; selecting four Macs plus Android re-prompts for
at most three Macs. Explicit CLI switches keep autonomous runs noninteractive.

The default is the local Worker/lobby simulation: the terminal owns the Rust
hosted server on loopback port 8787. A detached Flutter window embeds the native
Rust Bridge, listens on the selected LAN address at stable TCP port 17423, and
automatically joins the hosted origin. It manages all selected emulator streams
in one household session. Mac windows use the checksum-pinned custom MBC30 core
through RetroArch, each at 5% volume. Logs from server and Bridge are visible in
the terminal; each GUI keeps its own log file after the terminal command exits.

Ctrl+C stops only the owned hosted server and releases its port. Bridge and
emulator windows remain open until the user closes them. The Bridge's separate
TCP listener remains active while its window is open. Local embedded and remote
modes have no terminal-owned server; Ctrl+C stops monitoring only. Existing port
owners still require interactive approval. A new timestamped profile per run
and Mac instance protects saves belonging to older open emulator windows.

Session batteries clone the same validated Chris scenario, preserving party,
progress, name, clock and coordinates. Allowed changes are trainer ID, seven
native appearance bytes and ordinary main/backup checksums. Appearance includes
`wPlayerGender`, `wPlayerSprite`, `wPlayerObjectSprite`, `wPlayerObjectPalette`,
`wPlayerPalIndex`, `wLoadedObjPal0` and `wNeededPalIndex`. Changing only gender
leaves stale native colors in a persisted player object; native palette tests
caught that and require the full appearance mapping. This transformation is
host-side test-fixture preparation. Production save code and committed original
scenario batteries are unchanged. Every snapshot is regenerated with the current
ROM and the exact platform core. Android core installation is automatic only
when the approved update detects a missing/changed core/app stamp, or when forced.

New **R6 MUST**: ordinary trainer IDs may collide between any or all players.
Network identity, authorization and routing must remain independent of them.
The explicit `duplicate-ids` native acceptance case uses four figures with trainer
ID 12345 and checks distinct server-assigned player identities, remote native
sprite/color equality and the Start-menu lifecycle. It is part of the default
milestone-2 suite. `probe_session_saves.py` boots all 12 normalized batteries and
compares their native player sprite and hardware colors with the original ordinary
character-choice saves, while host tests enforce the exact allowed byte delta.

Validation: host/assembly/Rust suite passes; Flutter widget autostart and native
FFI tests pass on loopback and the actual LAN interface; all 12 scenario/figure native comparisons pass; the four-identical
trainer-ID acceptance passes. Four real Mac frontend windows and one real Flutter
Bridge launched together, with four separate lobby joins visible in logs. Terminal
Ctrl+C exited successfully and left all five GUI processes alive while releasing
the server socket. The final appearance fixture revision was also launched with
two real frontend windows, both reporting successful current-core auto-state load.
No feature or native game assembly changed; enabled and disabled ROM hashes remain
those of the MBC30 migration. Evidence is in `research/dev-launcher-evidence.json`.
Physical Android remains unavailable; no new device validation is claimed. This
launcher work does not resolve the existing protected-NPC scanline blocker.


Last updated: 2026-10-12.

Latest footprint directive (2026-10-12): the user approves a 4 MiB MBC30
multiplayer target and explicitly replaces the 10,000-byte total addon cap.
The ordinary disabled target remains 2 MiB and hash-identical. All six banked
feature sections now share bank 128; original game sections remain below 128.
Every mobile link checks those rules and the unchanged 32 KiB RTC battery header.
The counterpart budget gate protects a 256-byte net lower integration ceiling;
current integration is 174 bytes. No original helper is relocated. Additional
feature capacity does not relax RAM, OAM, frame timing or save constraints.
Use the supplied DoubleCherryGB fork revision ef247c6, installed with checksum
verification by tools/prepare_mbc30_core.py. Mac launchers use the supplied
universal library; headless color/instruction observations use separate native
builds from its matching pinned source. Old cores cannot run the expanded ROM.
All new artifacts use the Storage volume, as requested by the user.
Current standard mobile SHA-256, after the turn-visibility fix:
73c113958b7b5207efeabe9c7141bcb3749fed8db9c640f08cefb207254308ed.
Feature storage is 10,958 upper bytes; lower integration remains 174 bytes. A
migration test exposed a VBK/VBlank race in tile restore. Bank selection now
occurs under DI, with an explicit private latch (eight ROM bytes, one WRAM byte).
Priority/offline snapshot recovery, all five Save fault scenarios, exact native
save-copy observations, four-client 100..400% cadence and matched native controls
pass. Six four-client interaction/latency groups and supplied-library transport
pass. The manual launcher prepares a fresh snapshot with the supplied core.
Read [research/MBC30_MIGRATION.md](research/MBC30_MIGRATION.md) for the current
migration checkpoint and retained validation artifacts before future work.

Rendering assurance remains open: the protected Route 37 fixture loses a remote
figure and the strict map scanline gate flags 698 conservative pair candidates
on 269 maps, including 355 protected pairs. Whole-character display suppression
for approved replaceable NPCs is accepted but not implemented. Do not start
milestone-3 ROM features or treat a mapper migration as closing these blockers.
The previous 2 MiB budget and mapper feasibility reports below are historical.

The milestone-2 local implementation passed its earlier recorded finite suite. Assurance is reopened for native scanline crowding and comprehensive
future-map gates; do not extend the earlier completion claim to those properties.
The user's new no-content-choice ROM requirement also needs a capacity strategy.
Read [research/ROM_AND_RENDER_LIMITS.md](research/ROM_AND_RENDER_LIMITS.md) before
starting milestone 3. The normal `make mobile` build uses schema 4, capacity four
and a generic native object lease pool. Flutter presents a native Rust household
Bridge: embedded local hosting or one WebSocket per household to the routed room
owner. The shared Rust owner runs embedded, in the native hosted simulator and
in the locally tested Rust/WASM Durable Object adapter. Public deployment and the
unavailable phone are deferred by the user.

Historical 2 MiB standard mobile SHA-256:
fdb237ff7a3bf88d20236ca18a33f239a158b6ca0b9b0edd8193ee6d62bde138.
Current 4 MiB hashes are recorded in research/MBC30_MIGRATION.md.
Disabled standard remains hash-identical to the baseline:
314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e.
No original helper was relocated; all feature assembly stays in rom/ with small
conditional integration seams. All 23,373 checked native RAM symbols stay fixed.
Historical 2 MiB mobile headroom was 2,849 bytes. The MBC30 migration now
leaves 13,787 bytes in the original half, including 39 in ROM0, and almost 2 MiB
in the upper half. Budget RAM, ROM0 and graphics before implementing later features.

The historical 2 MiB milestone-2 run used four actual cores at 100/200/300/400
percent and passed the strict cold 800-frame window
with 800/800 world updates, zero duplicates and no VBlank overruns. Fresh native
controls verify equal ten-actor inventory, actual reclaimed objects, correct peer
figures/final positions and measured movement inputs. They are an actor-load
comparison, not identical cold-join animation, random history or palette slots.
The ten main groups, immutable Save/fault repeat, special scenes, faithful,
echo transport, shared-owner/host tests, local workerd hibernation and native
Flutter FFI/bundle checks pass. Ordinary save-copy projection and private remote
palettes avoid both runtime-object and native palette-cache contamination.
Physical Select+B works through menus and an online compatible state restored
without an adapter. Later changes must retain these gates.

Historical additional movement p95 was 188.997ms local and 298.842ms with 100ms added
RTT. Full input p95 is 306.721ms and 417.990ms; hosted margin is thin. Finite tests
are not universal fault freedom, all-story-state coverage or a WAN guarantee.
Read research/MILESTONE_2.md and research/milestone2-evidence.json before historical
notes below. They supersede old blockers/completion claims and invalid controls.
Secure invites, semantic compatibility metadata and other-platform packaging/
background lifecycle remain future work; calls/actions are milestone 3.

Latest execution directive: implement milestone 2 autonomously, using native
headless DoubleCherryGB on macOS, without the unavailable phone or cloud account.
The Bridge foundation now includes a Flutter UI backed by native Rust, one shared
session for all attached local emulators, embedded local lobby hosting, and a
single multiplexed WebSocket for joining an Internet lobby. Two households can
combine distinct emulator players through two Bridges without making each player
a separate household connection. The local and hosted owners share Rust rules.
Include bounded test-only latency injection at the transport boundary. Older
per-emulator WebSocket proposals below are intermediate scaffolding, not the
accepted final architecture. Platform packaging and hosted deployment follow
later; iOS background execution must be assessed separately from Rust support.
New work remains experimental and uncommitted until the revised milestone-2
acceptance gates pass. Commit completed changes locally, as requested. The earlier
quoted Bridge UI example does not override that Git workflow.
Binding requirements: [REQUIREMENTS.md](REQUIREMENTS.md), including the latest
four-player target, matched-NPC performance at up to 400% speed, priority local
Select+B disconnect, independent saves and compatible state recovery without
libmobile. Durable transfer recovery and save journals are explicitly out of
scope. These requirements supersede older proposals below. Feasibility boundaries
are explicit and are not waivers.
The gameplay wishlist and architecture outlook below were clarified on 2026-10-11:
four-player coexistence is its sole absolute feature requirement. Other gameplay
features remain desired, subject to feasibility and discussion. Foundational
requirements remain binding. That wishlist clarification was planning only;
later execution directives authorize M2, not the future gameplay wishlist or public deployment.
The user subsequently selected a local TCP-to-WebSocket bridge on each device
plus Cloudflare Workers/Durable Objects as the critical hosting direction. Keep
the working direct-TCP development workflow until its replacement is tested.
The revised milestone-2 checklist records the completed adversarial-server and
four-client performance/recovery tests, with deferred delivery work explicit.
Follow-up: the user accepts the documented Free-tier quota as the initial budget
and approves local handling of frequent ROM polls. Lobby management is now
preferred in the Bridge app. A build-generated, embedded compatibility hash is
desired; the stronger claim that all semantically compatible versions necessarily
hash alike remains an unresolved feasibility goal, not an established property.
Status: milestone 1 complete. Milestone 2's implemented local path passes its
finite suite, with scanline/future-map assurance reopened by the latest audit.
The normal mobile build now uses schema 4, four total players, native actor leases,
the Flutter/native Rust Bridge and the shared embedded/hosted/WASM Owner.
Historical three-player completion statements below do not establish this result.
The final native build isolates original leased objects and palette state from
ordinary primary/backup saves. Final-source results and resource measurements
are recorded in research/MILESTONE_2.md and research/milestone2-evidence.json.
No milestone-3 gameplay feature has started.
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

**Milestone-2 acceptance and retained delivery work (2026-10-11):**

- [x] Four real clients at 100%, 200%, 300% and 400%, including the same
  crowded viewport. Separate join/leave/rejoin, movement modes and transition
  scenarios pass. Shared-owner/native/workerd tests reject a fifth player.
- [x] Fresh native actor-load controls: equal actor count and remaining native
  identities, correct other-player figures/final geometry and replayed movement
  inputs. All mobile profiles have 800/800 world-update frames, no duplicates
  or extra gaps. Cold-join animations, NPC random history and palette-slot
  assignment are explicitly not identical; these are finite resource controls.
- [x] Deterministic arbitrary-byte codec and typed model-event sequences,
  partial/coalesced frames, invalid fields, stale generations, participant checks,
  identifier exhaustion and injected time. Run the current 35 Rust tests.
- [x] Bounded resources, socket admission and fairness against aggressive
  clients, invalid messages, slow transport and queue saturation. Healthy
  household siblings and separate rooms are covered at their intended boundary.
- [x] Churn, expiry, upstream loss/reconnect, server restart and fresh generation
  handling. Physical exit cancels local intent without an acknowledgement.
  Native saving and adapter-free compatible-state recovery pass independently.
- [x] Cross-room and cross-stream isolation in the shared model, native adapter
  tests and actual local workerd. Logical isolation does not isolate a host crash.
- [x] Bridge foundation: native Rust inside Flutter, embedded local owner,
  household multiplexing, bounded WS/TCP/credit and local poll semantics. Native
  loopback, routed room ownership, actual local Worker/DO and four forced
  hibernations pass. Mac bundle and real FFI listener/port-release tests pass.
- [x] Current compatibility admission rejects unsafe transport/schema/capacity
  and standard/faithful combinations. This is not cross-revision gameplay proof.
- [ ] Deferred by the user's unavailable-phone instruction: Android loopback and
  background service survival with foreground RetroArch. Other-platform packaging
  and iOS lifecycle remain delivery work; native compilation alone is insufficient.
- [ ] Optional semantic compatibility extension: generated/embedded metadata,
  reproducibility, relevant/irrelevant data changes and imported-versus-live ROM
  matching. The stronger compatibility goal is retained, not implemented here.
- [ ] Deferred by the user: public Cloudflare account/deployment and one-command
  authenticated rollout with namespace-upgrade and endpoint smoke tests. Joint
  Worker/DO configuration and local WASM tests are already in the repository.

The binding save, emergency, stability and performance requirements remain active
for every later change. The approved local milestone is complete; the explicitly
listed delivery work and optional compatibility extension are not claimed complete.

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

Current work in progress: extract the runtime-independent Rust lobby core, add
the native Bridge and public Worker/per-lobby DO adapters, then prove grouped
local and Internet paths with headless cores before completing the four-player
ROM and matched-NPC cadence gates. The shared model passes its existing native
tests. WASM type checking passes; bundling and adapter tests are still in progress.
No cloud deployment, physical phone use or milestone completion is claimed.

Bridge checkpoint: native embedded local mode and grouped Internet mode are
implemented. One multiplexed upstream WebSocket carries distinct local emulator
streams. Shared Owner rules isolate malformed player messages, bound membership
and delivery credit, and route to independent room owners. Native transport tests
pass for two household Bridges, full-lobby rejection, sibling survival and server
loss. A 50ms one-way test delay produces at least 100ms handshake RTT without
serial sleeps per message. Flutter widget tests and a real Dart-to-native FFI
listener test pass. The macOS Flutter app builds and includes its signed native
Rust library without Rosetta. Other platform packaging remains unverified.
Three actual headless DoubleCherryGB clients passed both the embedded and hosted
Bridge paths using ordinary Chris/Kris/Crys bedroom batteries, movement and avatar
assertions. Artifacts: external cache m2-bridge-local-first and
m2-bridge-hosted-first. This is still the schema-3, three-player ROM checkpoint;
four-player runtime and strict matched-NPC cadence are not accepted yet.
WASM Worker/DO bundling succeeds with worker-build 0.8.7's supported classic shim
flag, --no-panic-recovery. The experimental module-target catch-wrapper generator
failed on the pinned toolchain; no experimental panic recovery is claimed.
Future parser/resource work must retain native/local-hosted/DO model equivalence.
The actual Rust/WASM routing/DO path also passes local workerd tests for upgrade,
capacity, presence, room isolation, wrong-room/malformed rejection and leave.
This used no cloud account and performed no deployment. The Bridge now coalesces
POLL credits as well as positions at a bounded wall-clock rate, and caps incoming
local messages per emulator. Re-run actual ROM latency after these later changes;
the two first headless proofs preceded the POLL-coalescing update.

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

### Physical emergency exit and shared admission checks, 2026-10-11

The guarded four-byte VBlank farcall samples physical JOYP outside native input
locks. The handler restores the prior JOYP selection and never enables IME or
waits for transport. A pending cancel prevents an interrupted foreground parser
from publishing a successful join afterwards. Resident memory is 127 bytes.
The disabled build again has SHA-256
`314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e`.

`probe_priority.py` passed an actual connected Start-menu exit and a real online
Start-menu emulator snapshot restored into the same core with the adapter disabled.
Select+B cleared local owner/intent/online/peers, menus closed, movement resumed,
no crash marker appeared and the ordinary input battery remained unchanged.
Artifacts: `/Volumes/Storage/Cache/mobileadapter/m2-priority-3`. The explicit
`--test-snapshot-out` option permits this integration fixture only; ordinary human
snapshot preparation retains its offline/idle guards.

Shared Rust admission now limits each Bridge group separately to 1024 messages
per second and expires idle groups after 10 seconds. Native routing and DO use
this same budget. DO alarms expire silent members without needing traffic from
another group. Identity churn evicts only disposable offline resume records;
live players survive bounded storage exhaustion and identifier wrap. Checkpoint
validation rejects out-of-range snapshots and unmatched live sessions. All 28
native Rust tests pass. The latest Worker bundle and local workerd checks are
being repeated after this change; no cloud deployment occurred.

A separately flagged native actor experiment is now in
`rom/native_actors_probe.asm`. It initially leases only free native actor slots,
loads complete standing/walking graphics in bounded idle-time chunks and avoids
the independent OAM/scanline renderer. It is NOT the release allocator and does
not yet evict NPCs, support four players, or establish cadence acceptance. The
first build needed JR-range repairs. Keep the normal renderer available until
runtime evidence selects or rejects this approach. Latest production ROM/symbols
before this experiment were copied to the external cache under
`m2-native-actor-backup`. Never publish this experiment as completed M2.

### Native actor experiment results, 2026-10-11

Three real headless ROMs passed the free-slot native actor experiment. Every
peer's complete cache and currently displayed native VRAM frame matched its
selected vanilla figure. A read-only observer now follows leased native object
slots and actual hardware OAM, instead of assuming fixed tile windows. Three
selected figures were visible in the bedroom (`m2-native-actors-fourth`).

The first measured experiment still missed world updates: 768/768/768 of 800,
max gap 2. It is not a performance fix. The foreground native renderer hook was
small (48-cycle render hook), but peer preparation still performed substantial
work before normal engine updates. The next experiment moves that preparation
into bounded HALT wait opportunities, alongside transport, retaining the last
remote appearance when no suitable window remains. This is distinct from the
previous failed whole-parser experiment because the per-facing upload and
independent OAM/scanline pipeline have been removed. Measure before accepting.

`build_native_controls.py` creates a disposable baseline bedroom with two normal
native avatar NPCs through a temporary map-source include. All fixture assembly
lives under `rom/fixtures`. It restores the original map source in a finally
block, rebuilds the disabled ROM and checks its exact hash. The first ordinary
control ROM is under external cache `m2-native-control-1`; its scene must still
be checked for unintended dialogue from boot inputs before using the result.
No source map modification remains.

Latest actual local workerd tests pass household multiplexing on one WebSocket,
two households sharing a room, malformed one-stream payload isolation and
single-group message-budget rejection, in addition to routing/capacity/leave.
These are local native/workerd checks, not a cloud deployment.

### Four-player native free-slot slice, 2026-10-11

The native actor experiment now has three generic peer records/caches, schema 4
and four total players. Full rosters and deltas validate all records, uniqueness
and identity bindings before publishing any record. Reverse packed expansion is
generic and bounded. The native diagnostic labels are generated from the pool.
Experiment objects/ROMs use an isolated `MOBILE_PROBE=1` Make configuration, so
normal `make mobile` cannot reuse experimental objects.

Four actual selected-character ROMs connected to one embedded Bridge, displayed
all three selected peers and passed graphics/cache/OAM checks in the bedroom.
All four profiles had 800/800 world-update frames, max gap 1, zero preservation
failures, serial max 1448 and tick max 15876 cycles. Artifacts:
`/Volumes/Storage/Cache/mobileadapter/m2-four-native-bedroom-2`.
This is not all-map or speed acceptance and does not yet implement NPC eviction.
The first crowded trial never joined: its normal game work had already passed
the conservative idle-window cutoff. The cutoff/budget is being measured and
revised; do not report this trial as a multiplayer or crowd acceptance.

The shared Rust core now admits four and rejects a fifth. All 29 Rust tests
pass, including two native Bridges with two emulators each, local embedding,
latency and upstream-loss isolation, checkpoint state-sequence fuzzing and
identifier churn. Host launcher tests are now 19 and pass the local/hosted/remote
command paths. DO schema/capacity is being rebuilt and tested locally; no cloud
account or deployment is involved. The normal production ROM remains schema 3
until the native actor experiment passes enough evidence to replace it.

### Crowded idle scheduling investigation, 2026-10-11

The crowded scene's first full four-player allocation shifted native frame wait
entry to LY about 124. The initial LY<120 transport gate then starved maintenance
and peers expired. The revised slice stages packed expansion, whole-roster
validation and publication across bounded pumps, updates one pool actor per idle
unit and avoids scanning leases for ordinary map objects. Four connected crowded
cores now retain all identities and each reports 800/800 update frames, max gap 1
(`m2-four-native-corner-5`). Functional acceptance still FAILED: two remote actors
were absent from actual OAM in several clients. Native OAM submission can fill
before all high-priority actors are emitted. Allocation must budget OAM as well
as runtime slots and replace audited optional actors when needed. Correcting
native priority alone does not establish this gate.

The current experiment raises the byte budget back to eight with a separate
LY guard before starting each serial byte, retaining an ISR guard window. The
first hosted four-ROM latency trial is running; its independently injected delay
must be declared separately from the Python transparent-hop delay in evidence.
Do not confuse an endpoint declaration with actual injected propagation delay.
The ordinary production ROM still uses the retained renderer; the new shared
server's schema 4 will require promotion of the validated native ROM or an
explicit compatibility strategy before the human launcher is declared usable.

### Household transfer latency and resource leases, 2026-10-11

The first four-ROM hosted trial kept correct identity/graphics, but did not pass
latency: the first remote movement arrived about 1.2 seconds after input and
some directional samples had no matching change. The unchanged-roster delivery
credit left a libmobile TCP receive with no immediate response. The experiment
now introduces identity-bound local transfer receipts, with authority receipts
from accepted presence messages and a three-second authority freshness bound.
Receipts do not change membership or positions; WELCOME still comes exclusively
from the actual room owner. Retest measured motion; this diagnosis is not yet an
accepted latency fix.

The scoped Game Corner lease prototype saves a complete native object in free
bank-5 WRAM and temporarily reuses audited cosmetic script actors 6 through 11.
It also reserves four OAM entries per remote actor before using a free slot.
Restoration, palette/camera refresh, map-transition cleanup and all-map policy
are not accepted yet. The new allocator is still experimental and isolated by
MOBILE_PROBE, not promoted to the human launcher.

### Receipt and native observer follow-up, 2026-10-11

Thirty-one Rust tests now pass, including silent-authority expiry and coalesced
local delta delivery. Local transfer replies return one latest roster/delta or
an identity receipt, never both per poll. Pending join polls are allowed without
fabricating WELCOME. The faster ROM trial exposed that pending-poll ordering bug;
its repeated unjoined sessions are not accepted connectivity evidence.

The native OAM observer was incorrect: InitSprites writes each avatar's four
quads backwards, but the observer read forwards from the lowest tile. It could
include adjacent NPCs/zeros and falsely report missing figures or directional
motion. A corrected observer requires a coherent complete four-tile group and
uses its actual hardware facing. Earlier native cache evidence remains useful,
but complete four-quad visibility/latency must be rerun with the corrected runner.

The first scoped Game Corner lease profile with a 16-byte burst failed cadence:
three clients had only about 693 through 695 update-bearing frames out of 800,
with duplicate updates in other frames; the fourth also had gaps. Total update
counts alone are insufficient. The burst is reverted to eight; this failure
is explicitly not an accepted performance tradeoff. The candidate allocator
leased two audited dialog actors and one free native slot in that scene.

### Coherent native OAM and frame-wait boundary, 2026-10-11

The corrected OAM observer also skips coherent stale groups whose Y bytes were
hidden by vanilla cleanup, and preserves the last complete physical OAM image
as hardware-oam.bin. Four real Game Corner clients pass selected cache/VRAM/
complete-avatar visibility with two audited leases (map actors 6 and 8) and one
free slot (`m2-four-native-corner-lease-3`). Its profiling still had one missed
update-bearing frame in two clients, so that run is not cadence acceptance.

A feature-owned replacement HALT wait now checks whether the awaited VBlank has
already occurred during background work before sleeping. Its integration replaces
HALT/NOP with a four-byte farcall; existing native helpers are not moved. All four
Game Corner profiles subsequently measured 800/800 update-bearing frames, max gap
1 and zero duplicate updates (`m2-four-native-corner-waitguard-1`). That run failed
the separate randomized NPC-motion oracle in one client; do not label the entire
run accepted. Extend the ordinary observation and compare native motion behavior.

A disposable matched Game Corner ROM replaces audited actors 6 and 8 with normal
Crys/Beta objects and adds a normal Kris object in the otherwise free slot. A
native control with the same selected Chris save and movement measured 800/800,
max gap 1 and no duplicates. The original map file and exact disabled ROM hash
were restored afterwards. Evidence: m2-four-native-control-corner-1/run. The
normal production ROM is still schema 3 and remains incompatible with the new
schema-4 Bridge until promotion; do not claim the human launcher is ready yet.

The harness now supports two household Bridges and per-player 1/2/3/4 speed
multipliers. These new real-ROM paths still need execution. The latest four-ROM
100-ms hosted RTT latency trial uses the corrected complete-OAM observer and
is running under m2-four-native-receipt-latency-5. All earlier native latency
trials failed and must not be used as accepted timing evidence.


### Native wait-window scheduling checkpoint, 2026-10-11

The shared owner now replenishes roster delivery credit for accepted PRESENCE
messages as well as POLL. A dedicated regression runs two moving clients through
20 successive updates without polls and verifies fresh peer state each time.
All 32 native Rust tests pass. This does not establish real-ROM latency acceptance.

With the feature-owned VBlank-before-HALT guard, the 16-byte burst passes a longer
Game Corner run: all four profiles have 800/800 update-bearing frames, no duplicate
updates and max gap 1. Every selected avatar has coherent visible native OAM,
matching VRAM/cache/palette; ordinary NPC motion and protected story flags pass.
Artifacts: m2-four-native-waitguard-burst16-corner. The earlier failed 16-byte trial
predated that wait-boundary fix and is retained as failed evidence.

The refreshed 100-ms hosted RTT trial still fails: 60 visible-motion samples have
p95 837.05ms, max 843.327ms after first local world motion. Input-to-remote p95 is
954.902ms. Artifacts: m2-four-native-receipt-latency-7. No latency target was raised
and no failed directional samples were dropped. A follow-up permits only cheap
late transport transitions, excludes in-flight byte waits from foreground credit,
and investigates bounded phase progress inside the native frame wait. This stays
behind MOBILE_PROBE until all timing and recovery gates pass.


The bounded wait-window phase loop also passes Game Corner's four-client native
cadence: 800/800 update frames, zero duplicates, max gap 1 in all four clients,
with NPC motion and actual selected-avatar graphics. It never waits for a network
answer past the scanline deadline. In-flight serial bytes do not consume a
foreground transition credit. Expensive request/application work retains the
earlier LY admission boundary; only small transport transitions use late idle.
Artifacts: m2-four-native-idle-phases-corner. Hosted latency retest is still open.


### Cadence gate and early-window admission, 2026-10-11

The 32-byte budget on every map FAILED: two Game Corner clients had 799/800 update
frames, one duplicate update and a max gap of 2 (frames 3921 and 4091). Functional
avatar/NPC checks had passed, exposing an overly weak harness success condition.
Native basic/speed profiles now explicitly reject any non-800 update count,
duplicate update or max gap above one, even without a supplied control directory.
Artifacts: m2-four-native-idle32-corner. That unconditional budget was reverted.

The current experiment permits 32 bytes only when native wait entry is before
LY96, otherwise retaining 16, with the unchanged per-byte LY136 deadline. The
four-client bedroom profile passes 800/800, zero duplicates and max gap 1 in all
clients. Hosted RTT100ms latency still FAILED: p95 478.802ms, max 727.252ms;
input-to-remote p95 597.128ms. Artifacts: m2-four-native-receipt-latency-9.

Single-peer roster/delta changes now use one fixed bounded validation/publication
unit; full three-peer updates remain staged and atomically validated. This reduces
unnecessary phase waits, but its new timing/recovery gates are still running.
No human launcher ROM was promoted and no new acceptance target was weakened.


### Shared-owner push scheduling and local read admission, 2026-10-11

The Owner now emits bounded latest-state pushes without requiring a WAN POLL
credit round trip. Per-receiver pushes are separated by at least 50ms; dirty
latest state is retained implicitly in the bounded room model, with an explicit
flush_after deadline. The native host ticks the owner; the Durable Object schedules
a short alarm only for dirty state and otherwise retains its idle deadline alarm.
Attachment schema is now 4. The local Rust/WASM workerd suite passes routing,
capacity, presence, room isolation, household multiplexing, scoped invalid streams
and single-group traffic admission. This is local evidence, not cloud deployment.

The Bridge coalesces each emulator's upstream PRESENCE to at most 20Hz. Local
POLL delivery credit stays in the Bridge, with at most one pending latest roster.
An unchanged POLL produces no TCP frame: libmobile's nonempty send completes its
adapter command without its empty-receive one-second wait. A fresh cached roster
can satisfy that local credit as soon as it arrives. Authority liveness depends
only on validated upstream WELCOME/ROSTER/RECEIPT, never local polling. No join or
remote state is fabricated. Native integration tests keep sending presence into
a silent authority and verify that the local stream still expires within 3s.

33 native Rust tests pass after automatic-push admission. The real four-ROM
RTT100ms trial with immediate local receipts failed at p95 407.662ms, max425.647ms
(m2-four-native-push-latency-1). Removing empty POLL receipt traffic improved a
subsequent trial to p95 303.099ms, max312.286ms, but it still FAILS the unchanged
300ms gate (m2-four-native-push-latency-2). Empty PRESENCE receipts have now also
been removed in the diagnostic candidate; its next ROM test is pending. The ROM
itself remains the native actor experiment, not yet promoted for the human launcher.


### Four-speed matched-control investigation, 2026-10-11

The updated Bridge/Owner passes 60 actual-ROM hosted RTT100ms visible-motion
samples: p95 269.586ms, max327.422ms; input-to-remote p95 386.037ms includes the
unchanged native turn delay. All four profiles have 800 update-bearing frames,
zero duplicates and max gap1 (m2-four-native-push-latency-3). Do not reuse the
failed prior trials as acceptance. Local latency still needs final-source proof.
The Bridge no longer fabricates any local receipt; only real cached state can
satisfy a local read credit. Upstream authority receipts remain internal liveness
signals. Native tests and Clippy pass; FFI pointer lifetime is explicitly documented.

A four-client, two-household Game Corner run at 1/2/3/4 speed FAILED source cadence:
793 update-bearing frames, one duplicate, maxgap2; other clients each800/0/1
(m2-four-native-households-speeds-corner-1). Its seven gaps occur during long tile
scroll steps. An actual matched ordinary-NPC control has797 frames, two duplicates
and three gaps (m2-native-matched-control-speeds-1); the source has four additional
missed frames. This is a real regression, reported to the user, not a waived FPS
threshold. Native graphics-request waits now skip Mobile work; new expensive
transport starts require LY<116 and actor work requires LY<104. That candidate is
being retested against the actual matching control. A supplied control now replaces
the blanket800 gate, while still rejecting extra missing frames, larger gaps or
more duplicates. Without a control, the strict800/0/1 gate remains unchanged.

Authoritative four-player capacity now lives in rom/capacity.asm. Rust derives
its constant through a small build script; all generic actor/peer collections use
that setting. The historical schema3 production path has not yet been promoted.
The new crowded disconnect harness also needs --expected-online=0 for its final
barrier; its first failed run was an observer readiness error, not recovery proof.
A compatible-state recovery test now supports three actual peer processes and
requires a real NPC lease in its crowded saved RAM state.

### Native wait and delayed household checkpoint, 2026-10-11

The maximum-delay Rust workflow passes four concurrently active emulator
streams, two household Bridges and a 1000-ms delay in each direction. A malformed
additional local stream closes independently. The scenario keeps issuing local
presence/read credits while waiting for delayed remote state, as libmobile does;
stopping all local requests before the delayed roster arrives is not that flow.

The native graphics-priority experiment found an invalid admission predicate:
`hNbTilesToCopy` aliases the persistent BG-map row count. Treating any nonzero
value as an active sprite request starved joins. Use actual pending 1bpp/2bpp
request counts. The 104-line actor boundary then passed the native cadence
comparison but starved avatar publication; that is a failed functional result.
A 124-line actor boundary restored avatars, with one extra source-world gap
versus the ordinary-NPC control. Lowering transport admission to 112 starved
ongoing response phases in crowded clients. All trials are retained.

Response checksums are now sliced into at most 32 bytes per foreground pump,
with seven feature-owned scratch bytes and publication only after full trailer
validation. This alone did not satisfy cadence. The native wait loop also must
check `hVBlankOccurred` before beginning any new idle work: once the awaited
VBlank arrives, the regular game immediately owns the frame. The final-source
mixed-speed measurement of that correction is pending. Milestone 2 is open.

### Native sprite transfer and menu recovery checkpoint, 2026-10-11

The real crowded compatible-state recovery fixture passed in
`m2-four-native-crowded-state-recovery-3`: one local player and three actual
headless peers, two genuine displaced NPC leases, Start menu, physical Select+B,
normal movement after exit, and a real compatible RAM snapshot restored without
an attached adapter. Ordinary input batteries remained unchanged. No game memory
was injected. Native menu stationary actions set temporary actors to STANDING;
the feature-owned existing sprite hook now restores their last accepted facing
while menus freeze presentation. Offline lease restoration remains active even
while the menu is open. This candidate must be rerun after the transfer changes.

The previous four-tile Get2bpp upload invoked DelayFrame during cold joins. It was
replaced with four aligned single-tile GDMA blocks in four fresh HBlanks, exclusively
inside the native idle wait, without a pending DMA or frame-wait call. Admission
checks actual pending native graphics requests, hDMATransfer, and DMA activity.
This is different from the abandoned VBlank GDMA queue: no additional native
VBlank upload hook, no independently rendered OAM pipeline. A partial chunk stays
hidden and is retried before publishing an actor. Pan Docs specifies single-block
VRAM DMA takes about 8 microseconds and GDMA must respect safe VRAM windows:
https://github.com/gbdev/pandocs/blob/master/src/CGB_Registers.md

Native helper decompression is currently called with ordinary VBlank handling
through a feature-owned five-byte ROM0 stub. No native helper was relocated.
ROM0 has zero bytes free; this is a pressure point rather than accepted final
allocation. Full cold decompression still needs a stricter bounded-work proof.

The mixed-speed two-household trial 11 observed 797/800/800/800 update frames,
matching the equal ordinary-NPC controls, no extra gaps, and no upload DelayFrame
calls. The overall trial failed a diagnostic mode marker removed by the menu fix.
Trial 12 with that marker restored failed source-player parity (793 vs 797
populated frames). Thus parity is not reproducibly accepted. Both failed trials
are preserved; do not cite only trial 11 as a complete performance solution.
The current candidate removes no-op rendering farcalls in the native path, limits
the existing menu sprite hook to active scripts, and sets the diagnostic marker
once at join. Trial 13 is pending. The observer now supports an explicit 800-frame
window so cold joins can be measured against corresponding native controls;
comparing different windows is rejected.

The Rust maximum-delay workflow passed four active clients through two Bridges
with 1000 ms delay in each direction and an isolated malformed extra local stream.
Local workerd acceptance passed twelve cases, including repeated complete
four-player position rosters, room isolation, household multiplexing and scoped
malformed-stream handling. The cloud account and deployment remain untouched.
The current Durable Object WebSocket attachment limit documented by Cloudflare
is 16384 bytes, not the older 2048-byte figure. No forced hibernation recovery
acceptance has yet been recorded.

The read-only native idle/upload instruction observer is now in
`tools/profile_core.h`, not just the local build cache. It records upload
frame waits, span cycles and VBlank crossings. The invalid disk-exhausted trial
10 is diagnostic only. Completed trace JSONL artifacts were losslessly gzip
compressed to preserve evidence and recover cache space.

### Idle scheduling and hibernation checkpoint

The four actual ROMs in `m2-four-native-households-speeds-corner-18` passed the
matched ordinary-NPC cadence check and the existing functional oracle at
100/200/300/400% across two household Bridges with 50 ms upstream one-way delay.
The primary moving client had exactly the control's three normal frame gaps;
all stationary clients had 800 update frames, no duplicate updates and no gap.
Maximum measured transport pump was below 10,000 cycles. This is an 800-frame
warm-window result, not all-scene or cold-join acceptance.

Serial dispatch is now suspended during background capture, parsing and graphics
jobs and native game work. The enclosing idle wait resumes bounded byte IRQs.
Native position capture moved from the normal overworld frame into idle time.
The wait preserves BC as well as AF and yields immediately when VBlank completes.
This removed additional cadence gaps caused by serial bursts extending job cost.
Earlier trials 13, 14 and 16 failed cadence; 15 starved joining; 17 passed cadence
but exceeded the transport-pump budget. Keep these failure artifacts.

Whole-avatar decompression remains unacceptable for cold work. A private,
resumable LZ decoder now emits at most eight cache bytes per job and leaves
native VBlank mode unchanged. The first experiment (trial 19) passed warm cadence
but failed the exact vanilla graphics-cache oracle. Inspection identified the
project's special minimum repeat/alternate counts (two/three rather than one).
That correction is under test in trial 20. Do not promote the ROM until the
byte oracle and cold-window cadence/recovery checks pass. The former feature-owned
five-byte ROM0 decompression trampoline is no longer referenced.

The actual Rust/WASM lobby owner passed four forced workerd hibernations with
four players on two household WebSockets, preserving accepted sockets, identities
and full position rosters. Reproducible test: `server/cloudflare/tests/hibernation.mjs`.
Miniflare is pinned directly for that test. No Cloudflare account was accessed.

Remaining milestone-2 work includes cold-window comparison, generic all-map
protected-default leasing and lifecycle coverage, final save/recovery/latency/fault
checks, ordinary-build flag/protocol promotion, host contracts and Flutter bundle
verification. Do not claim milestone 2 complete on the warm-window result alone.


## Generic native lease checkpoint

The native-actor experiment remains separate from the legacy default mobile ROM.
Do not promote it or call milestone 2 complete yet.

- `m2-four-native-cold-speeds-corner-2/probe` passed real four-core cold joining,
  two household Bridges, 100/200/300/400 percent speeds and 100 ms injected RTT.
  Window 3300..4099 has 798 source updates, no duplicate updates, and only the
  two vanilla control gaps; the other three cores have 800 updates. All four
  have zero observed background-job VBlank crossings. The earlier cold-1 run
  was invalid because disk space exhausted; it is not acceptance evidence.
- The corrected incremental native LZ decoder passed the four selected-avatar
  cache and displayed-tile byte oracles. It emits eight bytes per job and uses
  native minimum counts of two for repeats and three for alternates.
- Map-instance hashing now runs in bounded 16-cell slices. The latest
  `m2-four-native-crowded-state-recovery-4` passed physical menu disconnect and
  restoration of a real online RAM snapshot with no adapter attached.
- Actual Rust/WASM workerd survived four forced Durable Object hibernations
  with two household WebSockets and four distinct authorized players. Socket
  identity and exact full rosters/positions survived wake-up. See local
  `m2-workerd-hibernation-2.log`; deployment is still deferred.
- Trace output supports direct gzip, avoiding hundreds of megabytes per core.
  The runner checks output errors. Failed experiments remain preserved.
- `audit_native_leases.py` and `native_lease_policy.asm` provide a distinct
  protected-default leasing policy, independent from the older visual-only
  suppression policy. Version 2 identifies 717 finite dialogue graphs across
  all 611 maps. They can read event flags, face the speaker and print dialogue;
  all branches must terminate without state-writing commands. Unknown commands,
  cycles, external object references, numeric references, special graphics,
  trainer interactions and puzzle objects remain protected. Source fingerprints
  and reference-count checks gate experimental mobile builds.
- The runtime chooses free slots first, then approved offscreen actors, then
  the most distant approved actor. It preserves the 34-byte native state and
  camera anchor, and restores original graphics/palette before publishing the
  NPC again. It verifies that a frozen temporary lease still owns a slot before
  updating/restoring it, so a script-deleted or reused slot is not overwritten.
- Slot 12 is available on ordinary maps. It remains reserved when the declared
  map assets contain Big Gyarados, Alolan Exeggutor, Sailboat or variable sprites.
  This avoids wasting a native slot on every map while protecting shared graphics.
- `m2-four-native-generic-cold-1/probe` passed cadence and four native avatars
  with generic selection. Its largest observed idle job was 31,344 cycles and
  no VBlank was crossed. A subsequent all-work admission guard at LY 96 failed:
  fast cores starved their avatar work even though their native world cadence
  remained correct. This is a failed prototype, not a supported configuration.
- The corrected guards admit bounded uploads before LY 116 and the larger
  actor-preparation job before LY 108. `m2-four-native-generic-cold-3/probe`
  passed four real cores, all avatars, matched cold cadence and zero observed
  background VBlank crossings. The later expanded policy still needs final
  cadence/recovery reruns.
- Pure whole-map retained-object counts overestimate some resource demands.
  Cinnabar Volcano has a declared out-of-map rock and separated walk regions;
  directional walls and warps matter. Do not assert that it fits from a naive
  FLOOR-only flood fill: LAVA_GLOW has native LAND permission. The new crowd-route
  tool follows native side-wall permissions and excludes warps, jump tiles and
  actor anchors. Its routes are fixture proposals, not reachability proofs.
- `build_crowd_route.py` and the runner's `WALK` command are being validated
  against an ordinary Cinnabar Volcano save made by New Game and the Save menu.
  The first controller prototype overshot a waypoint because global coordinates
  update later than native next-tile coordinates. The corrected controller
  releases input on the native next-tile coordinate and waits between waypoints.
  Preserve this failure; do not bypass it with RAM writes.

Next: finish the real crowd walk, test all four clients after retained-slot
accumulation, cover other dense/special maps, then run final latency, mode changes,
map transitions, save equivalence and malformed-transport recovery. Rerun Rust,
Flutter and both local workerd suites. Promote ROM/launcher/contracts together,
verify disabled baseline hashes and native symbol layout, and commit stable work.


### Movement presentation checkpoint

- The final ordinary Cinnabar route traversed 44 real controller waypoints and
  returned to its starting tile without RAM injection. Four actual ROMs then
  displayed three remote figures with eleven physical object slots occupied;
  the protected retained rocks remained intact. Artifact:
  `m2-four-native-cinnabar-retained-1`. Azalea Gym's southern special-object
  fixture also passed four native clients (`m2-four-native-azalea-special-1`).
  Other disconnected walk regions still need separate fixtures.
- Rust workspace assurance passed 34 tests, including bounded malicious state
  sequences. Python deployment passed 19 tests. Both actual local workerd suites
  passed: twelve routing/room cases and four forced hibernations with two
  household WebSockets and four player identities. The Cloudflare account
  remains untouched. Logs: `m2-rust-workspace-final-1.log`,
  `m2-python-deployment-final-1.log`, `m2-workerd-local-final-1.log`.
- A running fixture exposed presentation gaps that the final-state-only oracle
  missed. In-flight graphics now retain their last complete visible frame and
  accept live position work independently of the bounded decoder. The current
  four-tile frame is uploaded last. Same-map offscreen leases retain their cache;
  different maps, instances and unavailable participants still release safely.
  A separate per-frame preparation credit prevents uploads from starving all
  position updates. Native menu restoration stores native facing bits separately
  from the six-frame graphics index, and verifies slot ownership.
- `m2-four-native-running-4` passed all four real ROMs plus intermediate complete
  selected-avatar tile and visibility checks. A partially clipped native viewport
  edge does not provide a complete four-quad group, so that byte oracle requires
  the full figure inside the viewport. Earlier running-1 never entered running;
  running-2 exposed gaps; running-3 failed at the expected clipped edge. Preserve
  all those artifacts. The new scheduler still requires matched cold/warm cadence
  and recovery acceptance before promotion.


### Native lifecycle and capacity evidence

- `m2-four-native-generic-cold-4/probe` passed the new independent upload/position
  scheduler against the matched ordinary-NPC control at 100/200/300/400 percent
  and 100ms injected RTT: 798/800/800/800 world update frames, no duplicates,
  zero background VBlank crossings. Largest idle span was 28,288 cycles.
- Four-core bicycle, surf and Big Gyarados map probes passed exact selected-avatar
  cache/VRAM oracles (`m2-four-native-bike-1`, `m2-four-native-surf-1`,
  `m2-four-native-gyarados-1`). Start-menu presentation also passed
  (`m2-four-native-start-menu-1`).
- The first camera probe failed its explicit deferral oracle. Unsafe tiny-map
  panning now overrides menu freezing for cleanup, marks deferral and releases
  social leases. `m2-four-native-camera-2` passed real pan entry, retained native
  window objects, exit and restored four-player presentation. Review then found
  the new projection carry contract had been changed in the legacy file instead
  of the active native copy. It is now corrected in the active native function,
  with the legacy file restored; map/instance lifecycle tests must validate it.
- `audit_native_capacity.py` computes a conservative protected-actor bound after
  approved leasing. `m2-generic-capacity-1.json` fits three remote slots on 605
  of 611 maps under its stated static model. Six require additional evidence:
  Route39, Route36NationalParkGate, CinnabarVolcano1F, CinnabarLab, AzaleaGym and
  SnowtopMountainOutside. This is not an all-story/effect, OAM or timing proof.
  The contest gate selects only five contestants and one officer in native code;
  Cinnabar Lab has mutually exclusive event actors. Geometry-only counts include
  impossible combinations and unreachable retained actors. Other regions and
  runtime slot preservation still need actual controller fixtures.


### Default native ROM promotion and exact recovery checkpoint

The normal Mobile Adapter build now selects the native path with its single
MOBILE_ADAPTER flag. Schema 4, authoritative capacity 4, generic three-peer
storage and the Python Bridge launcher are promoted together. Experimental
MOBILE_PROBE/MOBILE_NATIVE_ACTOR_PROBE switches and the duplicate independent
OAM renderer are removed. All feature assembly remains in mobile_adapter/rom.

- Native NPC leases preserve the complete 34-byte object, four camera bytes and
  exact 384 standing/walking graphics bytes in private bank 5. Restore graphics
  before making the native object visible, then let native palette binding run.
  Pokemon icon tests exposed a GDMA source alignment error: low address bits are
  ignored by hardware. The raw tile pool is now 16-byte aligned with an assertion.
  Actual Game Corner and Route39 icon restoration compare original and restored
  VRAM bytes, including an online menu RAM state loaded without an adapter.
- Four actual retained-slot Route39 cores passed after 52 real controller
  waypoints. One displaced a real Miltank icon under OAM pressure. Native OAM
  subtraction is conditionally bounded to prevent an out-of-ShadowOAM write when
  a native actor enters between background admission jobs.
- Actor preparation is admitted before LY 115, preserving the per-byte LY 136
  hard stop and bounded idle work. The default final ROM passed the dense cold
  matched-NPC comparison at 100/200/300/400 percent, two households and 100ms
  added RTT. All four had 800 update frames, zero duplicates and zero measured
  NativeIdle/Upload VBlank crossings. Largest idle span: 29,348 cycles. Artifact:
  m2-default-cold-cadence-1/probe. Earlier raw-cold-1 had 798/800/800/800 matching
  the control's two ordinary gaps.
- A source-pinned capacity audit now fits all 611 maps under its stated ordinary
  exploration model (607 raw static bounds, 611 reviewed bounds). Contest gates
  select five contestants; the bound includes both officers. Lab gender actors
  are exclusive. Azalea retains all carts across native scripted paths. Native
  walls/warps separate volcano lifetimes. Snowtop panning is explicitly deferred;
  afterward, approved offscreen shared-atlas window records may be leased without
  touching their graphics. The long full-camera-tour-2 passed pan entry, travel,
  exit and restored four-player display. These are slot bounds and finite
  lifecycle evidence, not an all-story/OAM/full-playthrough proof.
- The default ROM passed four-client native Save menu fault cases: peer departure,
  server shutdown and corrupted application magic. Faults start at frame 4050
  while the native Save menu is active. Checksums, backup and primary saves agree;
  fresh adapter-free Continue and movement pass. Artifact: m2-default-save-faults-1.
  No save engine, SRAM allocation or save journal was added.
- Standard disabled hash is exact. check_layout preserves all 23,373 existing
  RAM/SRAM/HRAM symbols. Driver 8,084 bytes, auxiliary code 407 bytes, policy 1,005
  bytes; resident 128 bytes; bank-2 buffers 1,336 bytes; raw backup 1,152 bytes.
  Standard mobile leaves 4,404 ROM bytes and 956 aggregate WRAMX bytes free.
  Faithful mobile also compiles. Existing native helpers were not relocated.
- Actual Rust workspace tests, strict Clippy, static ASM vectors/layout contracts,
  Python deployment/scenario/policy tests, the signed native macOS Flutter bundle,
  real Dart FFI and local workerd routing/hibernation tests pass. No cloud account
  or phone was accessed. The human launcher prepared an isolated fresh Game Corner
  snapshot, without touching the manual session's port, saves or frontend.
- Local visible latency initially failed at p95 217.048ms, then 206.775ms after
  removing one Bridge throttle. The embedded owner now flushes the same bounded
  dirty state immediately, while cloud delivery retains 20Hz coalescing. A new
  unit test checks eager delivery against the same identity/capacity/checkpoint
  rules. The post-change actual-ROM latency measurement is pending. Do not claim
  M2 complete until the unchanged local<200ms/RTT100<300ms p95 gates pass.

### Native save span correction and final-source retest

A paired baseline save exposed a mistaken earlier assumption: wPlayerData includes
wObjectStructs and native palette scratch. Merely allocating no mobile SRAM was
insufficient. Before this correction, TEMP_OBJECT leases and frozen remote fields
could enter a normal save copy. Earlier fault-save results checked checksums and
Continue but did not prove independence of those records; they are superseded.

The feature now captures exact original empty records as well as reclaimed NPCs.
Free leases restore their original bytes. Two guarded farcalls after native
SavePlayerData/SaveBackupPlayerData CopyBytes project still-owned object records
from private bank-5 backups directly into the ordinary save copies. No live game
RAM is changed and native checksum/backup handling remains intact. Palette calls
preserve all five native scratch fields. Every assembly implementation stays in
mobile_adapter/rom/native_save.asm; no save journal or new SRAM is introduced.

A read-only instruction observer retains the native same-state payload, substitutes
the verified original records and checks every byte, register and bank after each
projection. Tests also reject serialized remote TEMP_OBJECT records, boot the
exported batteries with the disabled counterpart, and exercise physical exit
mid-save. Five save cases include an uninterrupted online save, peer disconnect,
server close, malformed data and Select+B.

The first post-change crowded test had no randomly wandering NPC motion in its
short final stationary window. Extending only the motion-observation duration to
5500 frames made this fixture pass; the strict 3300..4099 cadence comparison and
all cycle limits remain unchanged. Final-source four-core cadence passed, followed
by local visible p95 181.258ms. The remaining suite is running under
m2-save-isolation-suite-2. Older latency results remain historical.


## Final revised milestone-2 acceptance, 2026-10-11

Final evidence and reproduction are in research/MILESTONE_2.md and
research/milestone2-evidence.json. The native palette-cache Save blocker was
resolved with private hardware colors, original native actor views and complete
native palette scratch preservation. Native fades retain complete remote colors
and defer new color construction to avoid the normal long catch-up loop.
Five Save/fault cases are repeated with immutable disabled-ROM inputs and both
full same-state save-copy and independent native palette-cache oracles.

Earlier ordinary-NPC comparison fixtures were invalid: Continue restored cached
original map objects, and the replacement inventory/input sequence differed.
The final verifier generates actual reclaimed map indexes, correct figures,
positions and control-specific normal New Game/Save batteries for each measured
run. It checks remaining native actor identities, counts and replayed movements.
Fast source restoration explicitly forces the map dependency, then verifies the
exact baseline hash. Final four-player cold profiles are 800/800 at all speeds;
controls are 783/784. No extra world gap or idle/upload VBlank crossing is observed.
The boundaries of this actor-load comparison are retained in the evidence.

All ten main cases, five immutable Save faults, native camera/bike/surf/volcano/
connection cases, actual icon restoration, faithful and 14-process echo transport
pass with the final ROM. Host tests and immutable final layout/renderer audits
pass, and the human preparation path produces the same ROM and a fresh normal
Continue snapshot in an isolated cache. No manual listener, user profile, phone
or public cloud was changed. The milestone is complete for the latest authorized
scope; future features must preserve the gates and address the small ROM budget.


## Native scanline and MBC30 follow-up, 2026-10-11

The user approved whole-figure temporary hiding/restoration only for reviewed
replaceable NPCs. Required/protected pairs must be flagged. The native selection
and final visibility checks now run in current headless acceptance, independent
of DoubleCherryGB's missing hardware selection cap. Route 37 demonstrates an
unresolved missing figure before the theoretical twelve-object line can be
measured; Game Corner's four-core menu control passes at maximum eight objects.
The all-map strict gate fails on unreviewed candidates. New source coverage is
611 map ASM files, 457 block inputs and collision/alias/facing/renderer definitions.

The user's multiplayer-only 4 MiB proposal passed an isolated link-placement
probe without moving original game sections above bank 127. Upper bank access
fails in the stock pinned core. No production mapper switch, required-NPC hiding,
cartridge claim or rendering acceptance was introduced. Results and actionable
remaining work are in research/SCANLINE_AND_MBC30_FOLLOWUP.md. Completed tooling
and reviewable findings are committed locally; known-failing acceptance gates
stay visible instead of being waived.
