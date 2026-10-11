# Binding project requirements

Updated: 2026-10-12. This document records the user's requirements and subsequent
clarifications in English. It takes precedence over older implementation proposals
in PLAN.md. An unmet requirement is an acceptance blocker, not permission to
silently weaken it. Feasibility notes below are engineering assessments, not
user-approved exceptions.

Scope clarification (2026-10-11): four simultaneous players, including on the
same map, is the only absolute requirement within the gameplay feature wishlist.
The other gameplay features in PLAN.md are desired directions, subject to measured
limits and discussion with the user. This does not relax the foundational
footprint, save, input recovery, performance, stability or testability requirements.
Do not implement wishlist features merely because they have been recorded.

## Minimal footprint

- **F1 MUST:** Minimal integration surface with Polished Crystal.
- **F2 MUST:** Minimal impact on ROM space.
- **F3 MUST:** The feature can be completely switched off with one compilation
  flag, producing a hash-identical GBC file to the corresponding pristine build.
- **F4 MUST:** All feature assembly lives in `mobile_adapter/rom`, apart from
  tooling and minimal integration code. Any deviation requires an explicit human
  decision after demonstrating why it is absolutely necessary and why alternatives
  do not suffice. Existing game helpers must not be relocated as a shortcut.
- **F5 MUST:** Preserve the corresponding original game content and its ordinary
  2 MiB physical-cartridge build. The user approved a separate **4 MiB MBC30**
  cartridge target for enabled multiplayer builds on 2026-10-12. That target
  needs an MBC30-capable emulator or board; an ordinary MBC3 board is insufficient.
  Do not remove game content to make the addon fit. This solves feature storage
  within the upper 2 MiB, not arbitrary growth of the original game or RAM/OAM.
- **F6 MUST:** MBC30 **replaces** the previous 10,000-byte total addon limit,
  explicitly confirmed by the user on 2026-10-12. All banked feature code/data
  must occupy banks 128..255, and original game sections must remain below bank
  128. The lower half contains only minimal conditional integration. The build
  guard checks placement; the matching-counterpart budget gate additionally
  enforces a 256-byte net lower integration ceiling as an engineering regression
  guard. Current measured integration is 174 bytes. Ordinary game helpers must
  not be relocated to fund that integration.

Read [research/MBC30_MIGRATION.md](research/MBC30_MIGRATION.md) for current mapper,
core and validation evidence. [research/ROM_BUDGET.md](research/ROM_BUDGET.md) is a
historical 2 MiB savings investigation, not the current acceptance cap.
The strict `test-mobile-budget` gate remains included in full ROM acceptance.

The latest measured ROM budget, scanline limits and missing future-map gates are
recorded in [research/ROM_AND_RENDER_LIMITS.md](research/ROM_AND_RENDER_LIMITS.md).
The earlier finite milestone-2 run does not prove those stronger properties.

## Multiplayer capacity

- **G1 MUST:** Support four simultaneous players in total, including the local
  player, with three remote peers per client. All four must be able to share the
  same map instance and appear together within the viewport in ordinary supported
  overworld play. Lobby membership alone does not satisfy this requirement.
  Battles and transfers still select two explicit participants; the other two
  players remain independent lobby members.
- **G2 MUST:** Use a generic bounded peer/resource pool. One authoritative maximum
  capacity setting determines peer records, protocol limits and generated/shared
  ROM/server constants. Runtime behavior handles zero, one, two or three remote
  peers through the same code paths. Do not implement separate numbered-player
  branches or manually add a graphics/cache block whenever capacity changes.
  Raising capacity may legitimately fail a documented resource assertion; an
  advertised supported value must not require hand-editing several allocators.

This replaces the original three-player target. The normal mobile build now
supports four total players through native NPC object/graphics leases. Current
acceptance and its finite coverage boundaries are recorded in
[research/MILESTONE_2.md](research/MILESTONE_2.md). The original memory exploration
remains in [research/FOUR_PLAYER_FEASIBILITY.md](research/FOUR_PLAYER_FEASIBILITY.md).
The implemented architecture leases ordinary NPC object/graphics
resources through the native renderer: use free slots first, then only explicitly
replaceable NPCs, and restore them when the visible-peer demand ends. Actual resource
release is required; hiding an NPC's OAM alone does not release its object slot or
tile allocation. Connected peers on other maps need records but no local sprite
lease. Preserve script actors, map/event identity and NPC state. Three remote
players require at most three such actor leases, not four extra actors.
The user additionally approved temporarily omitting an explicitly replaceable
NPC's entire displayed figure under scanline pressure, then automatically
restoring its display when resources return. This does not permit hiding protected
actors or changing NPC/game/save state. Required/protected conflict pairs must be
flagged for review. The current native path still needs this presentation policy;
its new runtime/strict map gates expose unresolved failures. See
[research/SCANLINE_AND_MBC30_FOLLOWUP.md](research/SCANLINE_AND_MBC30_FOLLOWUP.md).
The user approved the protected-by-default, audited NPC allowlist and runtime
selection/restoration policy in PLAN.md on 2026-10-11. The generated native lease
allowlist is separate from the removed legacy render-suppression table. Source
fingerprints and finite script-graph checks gate every mobile build.

## Save files and recovery

- **S1 MUST:** Ordinary `.sav` files are entirely independent of multiplayer.
- **S2 MUST:** Saves are fully compatible in both directions between multiplayer
  and single-player counterparts built from the same source revision and matching
  build options, except for the mobile flag.
- **S3 MUST:** Select+B disconnects whenever the running game can physically
  receive and process the input. It takes priority regardless of menus, scripts,
  animations, save activity, battle state, engine input locks or multiplayer
  errors. It must not depend on the server, adapter, a successful protocol exchange
  or valid session state. Game code must not obstruct this path.
- **S4 MUST:** Whenever a save occurs, it must match the save produced in a
  single-player situation regardless of multiplayer state, including unexpected
  error states, malformed or malicious server packets, connection loss and an
  attacker flooding the emulator.
- **S5 DECISION:** No multiplayer transaction journal is stored in `.sav`. Reliable
  transfer recovery across interruption, restart or unsaved-state loss is not
  required. After a gift/trade changes normal in-memory inventory, each user is
  responsible for ordinary saving. This does not waive packet validation, valid
  local inventory structures, save integrity or safe local cancellation.

## Performance

- **P1 MUST:** Normal gameplay is indistinguishable between single-player and
  non-malicious multiplayer at equivalent actor/rendering load: replacing the
  additional ordinary local NPCs with real remote players must not change
  performance. Match actor count, geometry, movement, camera, effects and palette
  pressure, including peers at up to 400% speed. Extra sprites are expected;
  their existence is not an excuse for extra multiplayer-specific delay. The
  DDoS exception applies to performance only; save integrity and recovery remain
  requirements.
- **P2 MUST:** The regular game always takes priority. Multiplayer work must never
  block a regular game frame. Background work uses time the game would otherwise
  spend waiting for the next frame.

## Stability

- **R1 MUST:** Different emulator speeds coexist across the network, including
  100%, 200%, 300% and 400%.
- **R2 IMPORTANT:** Try to support arbitrary emulation speeds. If an upper bound
  is necessary, support at least 400%.
- **R3 MUST:** Clients handle broken or malicious server packets gracefully.
  Ignore invalid input when safe; abandon an untrustworthy stream locally when
  framing cannot be recovered safely.
- **R4 MUST:** Temporary server disconnects recover through automatic reconnect.
- **R5 MUST:** A compatible emulator state captured during multiplayer must remain
  escapable through the same button combination when restored without a usable
  Mobile Adapter, including an emulator build without libmobile.

- **R6 MUST:** Multiple players may have the same ordinary trainer ID, including
  all four members of one lobby. Trainer IDs are game data, never network
  identities, authentication credentials, routing keys or unique player keys.
  Duplicate IDs must not merge players, misroute messages or invalidate saves.

The user explicitly clarified R5: state-format compatibility is already assumed.
The receiving emulator must successfully load the state; libmobile/adapter
availability may differ. Generic conversion between incompatible emulator state
formats is outside this requirement. Test the compatible ROM/core combinations,
including any supported builds with libmobile omitted, rather than assuming
that builds with different device support have compatible serialization.

The stability contract covers everything controlled by ROM code. Emulator,
libmobile, host and physical hardware defects are outside this contract, as
explicitly clarified by the user. Malformed data delivered to the ROM, internal
ROM mistakes and software-created input starvation remain within scope.
The user explicitly confirmed the matched-NPC performance comparison, priority
Select+B path, ordinary user-managed transfer saving and this ROM-control boundary.

## Development experience and server assurance

- **D6 MUST:** One Bridge owns a shared local session containing multiple distinct
  emulator players, up to the four-player lobby limit. Local mode embeds the
  native Rust lobby owner and needs no Internet service. Internet mode connects
  that same local group to one remote lobby over a multiplexed WebSocket. A second
  Bridge can add its own local players to that lobby. A transport connection is
  not a player identity; disconnecting one player must not remove its siblings.
- **D7 MUST:** The Bridge application uses Flutter for the cross-platform UI and
  native compiled Rust for transport and embedded lobby logic. Reuse the same
  socket-independent Rust lobby core in the native Bridge and WASM Durable Object.
  Do not reimplement lobby rules in Dart. macOS, Linux, Windows, Android and iOS
  are delivery targets; platform lifecycle feasibility is checked explicitly.
- **D8 MUST:** Autonomous local tests can inject bounded latency on the server
  transport boundary. The delay is a test configuration, never an untrusted
  client-controlled production setting. Describe one-way delay versus round-trip
  delay, and preserve bounded queues and independent player progress under delay.
- **D9 EXECUTION:** Finish milestone 2 autonomously using isolated native headless
  DoubleCherryGB instances on macOS. The unavailable phone and actual Cloudflare
  deployment are not acceptance prerequisites for this run. Keep physical Android
  lifecycle testing and hosted deployment as explicitly deferred follow-ups.

- **D1 MUST:** Make every multiplayer component autonomously testable by an agent.
  Deterministic host tests must run without a Game Boy emulator; actual ROM
  execution has separate headless integration tests. No manual phone interaction
  may be required for the core automated acceptance suite. Physical-device
  verification remains a separate compatibility check.
- **D2 MUST:** Preserve a single Python entry point for the human Mac/Android
  development session: start the Rust server, launch the Mac emulator, update
  stable Android files and launch the configured Android emulator via ADB.
  The existing script is sufficient for now and is intended to evolve. Keep its
  scenario selection, low Mac volume, connection logs and clean lifecycle behavior.
- **D3 DECISION:** A local TCP-to-secure-WebSocket bridge is a critical component
  of the selected Cloudflare hosting direction. Each supported device runs its
  own bridge; the ROM uses the stable loopback endpoint `127.0.0.1:17423`.
  Plan a shared Rust core with Android, macOS, Windows and Linux integration.
  Cloudflare Workers route to a Durable Object per lobby. This supersedes the
  earlier deferral of companion bridge applications, not the request to avoid
  an unnecessary standalone Android launcher. Record and test transport, platform
  lifecycle and resource limits before claiming compatibility. No implementation
  or deployment is requested in this documentation follow-up.
- **D4 DIRECTION:** Prefer lobby creation/selection/joining in the Bridge app.
  Identify running emulator clients through a local handshake and optionally read
  embedded metadata from a locally imported ROM. A build-generated compatibility
  digest is desired, including incompatibility-relevant game inputs. The user's
  stronger desired equivalence rule, that every compatible revision yields the
  same digest, is not established by raw-source or binary hashing. Record actual
  input coverage and limitations; do not present a conservative fingerprint as
  fulfilling that stronger rule. Preserve protocol checks, live-session identity,
  server admission validation and the feature-disabled hash.
- **D5 MUST:** Design public Worker routing and per-lobby Durable Object ownership
  as distinct modules now, while delivering them as one Cloudflare project.
  Share runtime-independent Rust room rules between the native and Cloudflare
  adapters and test the real Worker/DO path locally. The finished cloud backend
  must build/check and deploy through one repository terminal command after
  one-time authentication/configuration, with versioned toolchains/bindings,
  isolated environments and reported endpoint. The device Bridge connects to
  that public origin. This is a delivery requirement, not an immediate rollout.
- **T1 MUST:** Server assurance is based on behavior, invariants and adversarial
  input coverage, not a line-coverage percentage. Malformed frames, invalid action
  sequences or bounded traffic from an individual malicious client must not crash
  the service, create unbounded work/memory, exhaust shared resources permanently
  or corrupt another lobby. Use deterministic tests, property/fuzz tests and real
  transport tests, with explicit resource budgets and evidence.
- **T2 SCOPE:** Volumetric/distributed network attacks belong to deployment
  infrastructure. Application-level denial of service from one client remains
  in scope. Provider protection for the chosen transport must be verified;
  ordinary Cloudflare HTTP proxying does not automatically protect our raw TCP.
- **T3 MUST:** Clients retain authoritative ordinary game/save state. Server
  membership, routing, presence and interaction coordination may be transient
  and reconstructible. Loss of that server state must not destroy or invalidate
  local game/save state. No durable inventory ownership or save-recovery service
  is introduced. The user explicitly approved this transient-state model.
  Cloudflare with a local bridge is the selected direction. The user accepts the
  documented Free-tier quota for the initial target; actual consumption remains
  to be measured. Bridge is preferred for lobby management; exact joining UX is
  open. No paid upgrade or deployment is authorized by this planning decision.

The user also approved local handling of frequent ROM polls to reduce cloud
traffic, while preserving real server liveness, acknowledgements and session
semantics. This is an accepted design direction, not completed transport proof.

See [research/SERVER_ARCHITECTURE.md](research/SERVER_ARCHITECTURE.md) for the
current implementation, public-hosting options, cost assumptions and assurance
gaps. Existing tests are a foundation, not evidence of complete hostile-client
coverage or proof that unimplemented features already work.

## Definitions and unresolved feasibility boundaries

### Saves and exact equality

"Same hash" in S2 means the same source revision, not identical hashes for enabled
and disabled ROM binaries. The enabled binary necessarily contains different
code. Ordinary battery saves and full emulator states are distinct artifacts.

The proposed precise S4 comparison is: identical native save input state, initial
battery contents, RTC/time inputs and save operation produce identical cartridge
save bytes, for every multiplayer connection/error state. Connection data, peer
records, protocol buffers, tokens and recovery metadata must never enter the
battery save. A fresh offline load must not require the server. Emulator-specific
RTC wrappers/file metadata require separately verified import/export handling.

Different playthroughs can have different game time, random outcomes and
inventory. Legitimate trades or gifts intentionally
change normal game state. They cannot produce a byte-identical save to a
playthrough in which that inventory change never happened. They can preserve the
ordinary save format and serialize an equivalent native inventory identically.

The user explicitly rejected the earlier save-journal and durable-recovery
proposal. Remove that requirement from milestone 5, rather than replacing it with
a sidecar, mandatory recovery service or server-authoritative inventory. During a
live session validate the complete transfer, recipient consent and capacity before
local mutation. Keep local application consistent and reject duplicate/stale
messages using volatile session/operation IDs. A failed or ambiguous exchange
must remain locally escapable; it does not require persistent reconciliation of
both clients. After exiting/reloading, the ordinary saved inventory is authoritative
for that client. Exactly-once application across crashes, rollback or independent
save histories is not promised. The previous durability-design blocker is removed.

### Emergency exit and priority

The target is a bounded local escape path checked independently of ordinary menus,
scripts, input suppression flags and protocol parsing. It must not wait for a
graceful network shutdown. Select+B must disconnect every active/stale multiplayer state and must not
toggle back into joining while teardown is pending or the chord remains held.
The existing human development workflow also uses a fresh press to join from a
fully offline state. Joining is available through the Select+A menu. Existing diagnostic key combinations
must yield to disconnect when Select+B is held.
Explicit user exit must disable automatic reconnect until the user joins again;
temporary involuntary disconnects retain automatic recovery.

The implementation now samples physical JOYP at the common VBlank entry before
native input locks, and stops local intent/serial ownership without a network
acknowledgement. Actual connected-menu, Save-menu and compatible adapter-free
RAM-state tests verify escape and subsequent ordinary movement. These finite
cases do not establish every possible native critical-section response bound.
Native interrupt masking and direct-poll opportunities still require lifecycle
audits whenever new feature code is added.

The recovery path must not tear down a native save halfway through its critical
write sequence. Recognize/latch cancellation at a reachable input service point,
stop adapter activity safely, and complete native critical work before any game
state restoration that would interfere with it. Canceling multiplayer must not
mean aborting the normal save. Input detection has finite instruction/interrupt
latency even when it is given the highest software priority.

The earlier phrase "hardware-disabled interrupts" was too broad: `di` and the
interrupt masks are normally controlled by software. A game-created input lock,
indefinite interrupt suppression, blocking loop or unreachable cancellation path
is an implementation defect, not an exclusion from S3. Audit native critical
sections and every VBlank/input mode; service the chord as soon as processing is
physically possible. An interrupt being masked does not itself prevent direct
joypad-register polling. External failure to execute ROM code or deliver input is
outside scope. A bounded input-service latency still needs measurement.

For R5, treat the restored session as potentially stale: external sockets are not
restored by game RAM. Missing responses must not be required to release serial
ownership, discard volatile peers and continue locally. A successfully loaded
compatible state with absent libmobile is an explicit acceptance case, including
states captured mid-command and inside menus. A raw state cannot generally be
reused with a different ROM binary merely because both ROMs accept the same `.sav`.

### Frame priority and deferred remote presentation

The Game Boy has no second general-purpose CPU for multiplayer tasks. "Background"
means small resumable units of work placed in verified spare time before a native
frame deadline. Serial interrupts, bank switching, graphics publication and hooks
also consume cycles and must be included in the timing budget. A literal promise
of zero instructions outside existing waits is not currently established; any
necessary publication/input hook must remain surgical and be measured. Do not
treat a background label or byte-count cap as proof of matching frame timing.
Additional visible sprites also shorten HBlank, but the matched native-NPC control
has the same sprite geometry and therefore the same hardware effect. It cannot
justify a difference between local-NPC and remote-player performance.

Example: the last accepted remote position is tile A. The peer moves through B
to C while a busy local frame has no spare time. Native player/NPC updates and
display deadlines proceed normally. The remote world position remains A until a
bounded update fits; then the latest valid state C replaces it. B need not be
replayed. Old remote world coordinates must still be projected through the local
camera so the avatar does not become stuck to the screen. Rendering that cached
pose has a nonzero cost which must also fit the budget. Expensive animation/tile
updates may be deferred independently of position updates.

The native lease implementation now uses bounded idle jobs and retains complete
cached appearance while a later update waits. The finite four-core cadence and
latency measurements are in MILESTONE_2.md. This may reduce remote update frequency;
it does not promise a fixed delay on every scene or WAN path. If a scene has no sufficient spare time for an
extended period, perfect native cadence and a fixed minimum remote freshness
cannot both be assumed. Report that case to the user; do not silently accept
seconds of delay or drop required local rendering. Coalescing applies to presence
snapshots, not to battle choices, confirmed trades or gifts.

Native timing parity means no additional missed world-update frames or longer
gaps than a matching single-player scene with equivalent ordinary NPCs, geometry,
movement and input. Keep the original pristine control as a separate regression
reference. A control that merely disables networking while retaining the adapter's
renderer does not replace the native-NPC comparison. Any fixture that cannot
represent the same actor load must disclose that limitation rather than claim
parity. Existing baseline gaps must remain visible. Match input/audio timing too;
identical instruction timing between different binaries is not implied. The
current four-core dense-room gate passes at 100/200/300/400% against matched native
NPC controls, with no additional world-update gaps or background VBlank crossings.
That evidence does not prove P1/P2 across every possible map/story/input state.
The same strict gates remain mandatory for future feature changes.

### Speed, hostile input and physical limits

Mixed 100%-400% speeds are realistic: use session generations, sequence numbers,
latest-state coalescing and bounded per-recipient work, independently of peers'
frame counters. The server owns real-time leases. Frame-based ROM deadlines must
be audited because 120 frames last four times less wall-clock time at 400%.
Arbitrary producer speed requires dropping superseded presence updates instead
of attempting to replay every intermediate state. It does not grant unbounded
throughput or immunity to a host too slow to run its selected emulator speed.

All wire-controlled lengths, types, indexes, counts and state transitions require
validation before use, with fixed memory bounds and bounded work even for valid
packet floods. Silence, partial frames and untrusted framing must permit local
recovery. The goal is to handle every input byte sequence safely; the present
finite fault suite is not proof of that property. Expand host fuzz/property tests,
assembly contract checks and real-ROM fault tests. Shared address space is a reason
for stronger ROM validation and isolation discipline, not permission for internal
errors to corrupt the game. External emulator/libmobile/host failures are outside
the user's explicitly defined ROM-code guarantee.

ROM space and sprite resources remain finite. At checkpoint 4aff2149d, standard
enabled ROM0 has 39 free bytes and ROMX has 2,810 free bytes across all banks;
the net feature cost is 11,112 bytes. Fragmentation limits placement. Future
interactions/battles/transfers and upstream changes need explicit budgets.
Hardware OAM has forty entries but displays at most ten objects on a scanline.
Four ordinary avatars require eight per line; a second overlapping ordinary NPC
exceeds that limit. The current native renderer has no dedicated whole-character
scanline admission policy. Its global buffer guard and object leases do not
prove full scanline visibility. Stronger acceptance is open, as documented in
ROM_AND_RENDER_LIMITS.md; perfect CPU scheduling cannot remove this display limit.

## Required acceptance additions

- Bidirectional native-save loading between the corresponding enabled/disabled
  ROMs; exact save-byte comparison with equivalent native/RTC inputs, including
  main and backup saves. Existing save-failure tests are useful but insufficient.
- Priority exit during connection setup, established play, malformed/partial
  input, silence, menus, scripts, save activity and later battles/transfers. Check
  engine input locks, every VBlank mode, extra held buttons, partial session state,
  native state preservation, bounded response and explicit-exit reconnect policy.
- Restore connected compatible emulator states at multiple protocol phases with
  adapter disabled, unavailable, and a state-compatible libmobile-free build.
  Check local exit and continued movement without relying on server responses.
- Matched native-NPC timing tests at 100%/200%/300%/400%, with four clients, busy
  maps, scrolling, new avatar graphics, menus and controlled network delay. Include
  combined serial/parser/render costs and remote freshness, not just average FPS.
- Bounded behavior under malformed frames and sustained valid/invalid floods;
  reconnect after temporary failures without renewing an explicitly canceled
  session. Preserve normal save bytes under all injected transport conditions.
- Recheck disabled hashes, native memory layout and footprint after changes.
- Four-player wire bounds, three distinct peers per client, correct avatars and
  palettes, same-map on-screen coexistence, transitions and crowded scenes. Add a
  fifth-client capacity rejection test. A raised server limit alone is insufficient.
- Gift/trade consent, validation, live-session duplicate rejection, consistent
  local application and disconnect behavior; ordinary manual saving determines
  subsequent persistence. No journal or cross-restart transfer recovery tests.

Current status: documentation only in this follow-up. No new 400%, emergency-exit,
save-byte-equivalence or connected-state-restore acceptance result is claimed.
The four-player layout test proves RAM placement only. Milestone 2 now additionally
requires four-player implementation and matched-NPC timing; milestone 3 remains
blocked by that acceptance work. Milestone 5 no longer requires durable recovery.

## Sources

- Local `home/joypad.asm`, `home/delay.asm`, `engine/menus/save.asm`,
  `mobile_adapter/rom/adapter.asm` and the committed layout/cadence evidence.
- [Pan Docs: interrupts](https://eldred.fr/pandocs/Interrupts.html),
  [HALT](https://eldred.fr/pandocs/Reducing_Power_Consumption.html),
  [rendering](https://eldred.fr/pandocs/Rendering.html) and
  [OAM](https://eldred.fr/pandocs/OAM.html).
- [Libretro core serialization API](https://docs.libretro.com/development/cores/developing-cores/#retro_serialize_size-retro_serialize-and-retro_unserialize)
  and the pinned DoubleCherryGB `libretro.cpp` serialization implementation.
