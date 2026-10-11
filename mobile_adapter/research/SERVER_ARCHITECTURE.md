# Server ownership, hosting options and assurance

Updated: 2026-10-11. Inspected source: `0fa37627d`; this follow-up changes
documentation only. Public deployment, a provider migration and new gameplay
features are not being implemented. The user wants the current architecture to
leave room for the desired features in [PLAN.md](../PLAN.md).
The selected hosting direction is now a device-local TCP/WSS bridge with
Cloudflare Workers and one Durable Object per lobby. This is a critical planned
component, superseding the earlier optional-bridge/unspecified-provider discussion.

## What runs today

```text
ROM -> emulated Mobile Adapter / libmobile -> ordinary TCP
    -> native Rust process on the Mac, port 17423
    -> another client's TCP connection -> libmobile -> its ROM
```

The server uses Tokio with two runtime worker threads and async connection tasks,
not one process per lobby or one operating-system thread per player. It accepts
at most 16 simultaneous sockets across the process. A shared `Service` owns a
mutex-protected `Lobbies` model and a shared change notification. The model has
one bounded map of player records, with lobby membership stored on each record.
This is a development server, not the final public multi-tenant architecture.

The current ROM sends a HELLO whose prefix hardcodes public test lobby 1 in
`rom/session.asm:MobileHelloPrefix`. A valid join creates a player record tagged
with that lobby number. No external matchmaking service or per-room server is
spawned. The wire/model can distinguish room IDs, but the game has no room-code
entry UI. Capacity is still three total players, pending the four-player work.

The server retains player ID, connection generation, reconnect token, mechanics
variant, last-seen time, sequence and latest presence snapshot. A presence snapshot
contains map/instance, position, direction, avatar and movement/status. Outgoing
presence is coalesced; an adapter polling credit limits delivery to a slow client.
The current model has a three-second live lease and a thirty-second reconnect
retention period, evaluated while servicing model operations. These differ from
the proposed ten-minute empty-room lifetime.

There is no database or authoritative save/inventory copy. A server restart loses
all these records. Client recovery must form a fresh valid session and publish
current presence; it must not depend on recovering old server memory.

Source anchors: `server/crates/server/src/main.rs`, `src/lib.rs`, `src/lobby.rs`,
`server/crates/protocol/src/lobby.rs`, and `rom/session.asm`.

## State ownership

The user explicitly approved this transient server-state model on 2026-10-11.

| State | Owner and intended failure behavior |
| --- | --- |
| Local world, story flags, party, inventory and current native simulation | Each ROM's native game memory. Ordinary local saving determines persistence. A network failure must not reset or overwrite it. |
| Ordinary cartridge `.sav` | Native save code and emulator storage. No multiplayer metadata, journal or authoritative cloud inventory. |
| Membership, socket routing, session generation, latest position cache and pending invitations | Temporary server coordination, reconstructible or safely canceled after failure. Losing it interrupts multiplayer but must not invalidate local game state. |
| Battle/transfer operation progress | Explicit live-session coordination; clients validate and apply allowed local effects. Abort/restart is safe locally. Cross-crash atomicity and recovery of unsaved transfers are not required. |
| Room-code mapping and idle lifetime | Hosting metadata if that UX is adopted. Distinct from save data and disposable under a defined expiry/rejoin policy. |

A useful relay is not literally stateless: even sockets, room membership and
recipient selection are state. The appropriate goal is no irreplaceable server
game state. A restarted room must have a fresh incarnation/session identity so
old packets and operation IDs cannot mutate a new session. Do not infer success
or replay an inventory operation because a server has restarted.

An opaque byte forwarder would hide client addresses from each other, but would
not satisfy the assurance requirements. The service should validate framing,
versions, membership, claimed sender identity, targets, action ordering, sizes
and resource budgets before relaying. Clients independently validate incoming
actions, including local inventory and warp constraints. The server does not
simulate the whole overworld or own offline story progression. Cheat prevention
against modified ROMs is not implied by save safety. The relay itself sees
source IPs; it should not expose them in game messages or tokens.

## Runtime and isolation recommendation

Keep Rust. The existing typed codec and time-injected model are a useful base for
fast deterministic tests and bounded binary handling. Memory safety does not
prevent invalid state transitions, panics, identifier exhaustion or unbounded
resource consumption. Those require design and testing in any language.

Make the lobby rules independent of sockets, OS threads, the clock, entropy and
Cloudflare APIs. One logical room owner serializes typed events through bounded
queues and emits effects. Share the tested codec/model across deployment adapters:

1. Native TCP/Tokio for the current libmobile clients and local development.
2. The planned Worker/Durable Object adapter reached through the local bridge.

One process may host many four-person rooms. Separating room ownership and work
budgets first avoids requiring a dedicated VM/container per tiny lobby. Later,
route rooms to different processes/hosts for a smaller process-failure impact and
regional placement. Async tasks do not isolate a process crash; a shared gateway,
host or provider incident can still affect multiple rooms. An independent logical
room is not a claim of physically independent infrastructure. Native multi-host
deployment remains an architectural alternative, not the selected public path.

## Protocol versions versus game compatibility

A protocol version defines the format and interpretation of network messages;
it is independent of the Polished Crystal release number. The current code checks
transport version 1, lobby schema 3, capacity 3 and the standard/faithful mechanics
flag. An incompatible schema or capacity is rejected; a room cannot mix mechanics
flags. There is no Polished Crystal source revision or full gameplay compatibility
fingerprint in the current HELLO. Being accepted today is not proof that different
ROM revisions are safe together.

Identical multiplayer assembly is insufficient if native game content changed.
Separate compatibility dimensions and conservative acceptance rules:

| Dimension | Examples that need matching semantics |
| --- | --- |
| Wire protocol | Header/message layout, lengths, field interpretation, session and sequence rules. |
| Overworld presence/travel | Map IDs, coordinate systems, map identity/layout, avatar/movement enums, collision and allowed travel destinations. |
| Battles | Species/move/ability/item meanings, formulas, turn rules, RNG contract and build options. |
| Trades/gifts | Serialized Pokemon fields, forms, item IDs, limits and local validation rules. |

The existing 16-bit map-layout key is a visibility hint and can collide. It is not
a gameplay compatibility fingerprint. Never treat a matching protocol number,
map key or standard/faithful byte as authorization for arbitrary cross-version
battles, transfers or travel.

Different ROM revisions may eventually interoperate if their relevant compatibility
profiles are explicitly equal and tested. A text-only change is a plausible
compatible case; a changed map numbering or battle formula is not automatically
compatible. Define compact versioned profile/capability identifiers, initially
reject unknown combinations, and retain exact build identity for diagnostics or
as a conservative fallback. Broader compatibility is an opportunity, not a new
absolute feature requirement or something already implemented. Do not require
identical binary hashes forever when only irrelevant assets/text differ.

Bridge/cloud protocol compatibility is a separate boundary. A bridge update must
not silently translate game semantics or claim an incompatible ROM is compatible.
Positive and negative compatibility fixtures belong in milestone 2; feature-specific
mechanics/serialization checks grow when those features are implemented.

### Desired build-generated compatibility digest

The user would like the enabled build to derive and embed a compatibility hash
from all incompatibility-relevant game assembly outside the multiplayer feature.
The desired equivalence rule is stronger than exact-build matching: every pair
of compatible versions should necessarily produce the same hash. This is a
nice-to-have goal, not an implemented or proven property.

Deterministic generation and embedding are feasible. Automatically recognizing
all compatible source changes is a different problem that hashing does not solve.
Comments, equivalent refactors and code relocation can change source/binary hashes
without changing network-visible semantics. Hashing only `.asm` also misses inputs
such as `INCBIN` assets, generated tables and build options. A hash cannot repair
an incomplete choice of inputs or distinguish an irrelevant change on its own.

Investigate a versioned, canonically encoded compatibility manifest covering the
dimensions above. Derive resolved IDs, tables, layouts, schemas and mechanics
options from build inputs where possible. Audit code dependencies and any excluded
inputs; retain conservative fingerprints for relevant routines whose equivalence
is not established. Unknown changes fail conservatively until reviewed. Keep exact
artifact/build identity separate for diagnostics. This can avoid some unnecessary
mismatches, but does not satisfy the universal equivalence rule automatically.
The manifest's actual coverage and false-rejection limits must remain explicit.

Generate a bounded metadata block with magic, schema, length and compatibility
identifiers under `mobile_adapter/rom`, only for the enabled build. Measure the
digest/metadata ROM and wire cost. Avoid a hash cycle: derive the manifest before
embedding its digest and exclude the generated block from its own inputs. The
final ROM checksum belongs in external build artifacts. Feature-disabled output
must remain hash-identical to the native build.

The live ROM handshake reports metadata to Bridge. Bridge can compare it before
joining; the server must independently enforce compatible lobby admission. A
declared digest is untrusted client input, not attestation or anti-cheat. Optional
local ROM import can read the block, but cannot prove that imported file is running.
Network compatibility across revisions also does not imply cross-revision `.sav`
compatibility; the existing same-base enabled/disabled save guarantee is separate.

Milestone 2 should test reproducibility, relevant/irrelevant input changes,
mechanics flags, malformed/unknown metadata and imported-versus-live mismatches,
alongside the disabled-ROM hash gate. Feature-specific compatibility coverage
must grow with battles, transfers and travel. Preserve the stronger user goal as
unresolved rather than silently substituting exact-source equality for it.

## Cloudflare assessment

Rust/WASM is supported through `workers-rs`, but the current Tokio TCP listener
cannot become a Worker just by changing its compilation target. Workers expose
event-driven platform I/O. The official TCP API currently supports outgoing TCP,
not incoming raw TCP listeners. Our current ROM/libmobile protocol is raw TCP,
not an HTTP/WebSocket handshake. [Rust support][rust], [TCP sockets][tcp].

The selected Cloudflare model for the proposed room-per-worker idea is a routing
Worker plus one named Durable Object per room. An object is created on access
and coordinates its room's connections/events. It is not a separately deployed
server binary for every PIN. Durable Objects may use transient memory without
making the server authoritative over game saves. [Durable Objects][objects].

The user selected the local-bridge solution:

```text
Each player's device:
  ROM -> libmobile -> TCP 127.0.0.1:17423 -> local bridge
                                              |
                                      secure WebSocket (WSS)
                                              |
Cloudflare: routing Worker -> room Durable Object -> other players' WSS connections
```

Each device runs its own bridge. The phone's localhost means the phone, not the
Mac. The normal host network stack, including Android's standard shared host
networking, should allow the emulator to reach the local listener; verify this
with the actual supported core/app combinations before claiming acceptance.
No emulator fork or ROM WebSocket/TLS implementation is assumed. A wrapper using
a separate network namespace would need explicit loopback routing configuration.

The endpoint remains fixed in the ROM, while the bridge owns the remote WSS URL
and TLS certificate validation. This avoids per-device LAN-IP ROM patches. One
local listener can serve multiple emulator sockets with isolated bridge sessions,
initially one upstream WSS per emulator connection. No network message may supply
an arbitrary upstream destination. Bind to loopback, bound local admission too,
and do not expose a public proxy. Other local applications are not automatically
trusted merely because they can reach localhost.

A shared Rust bridge core handles bounded protocol framing and queues; thin
Android/macOS/Windows/Linux wrappers handle installation and lifecycle. On Android,
plan a user-started foreground service with the required notification while
RetroArch is foreground. Select and verify a valid service type and permissions.
`dataSync` is not an assumed permanent relay mechanism: Android 15+ documents a
six-hour background budget in 24 hours for relevant targets. No misuse of unrelated
service types to bypass restrictions is selected. Test startup, long sessions,
network changes, process death, restart and clean shutdown. [Android launch][android-launch],
[service types][android-types], [timeouts][android-timeout].

This extends the earlier deferred app scope to necessary bridge wrappers; it does
not require a standalone Android launcher replacement. Desktop starts can initially
use a CLI. Evolve the existing Python command to start the backend and bridges,
publish stable ROM/save files and launch both emulators only after readiness.
Keep the current working direct-TCP command unchanged until the new path passes.
For offline automated tests, use a local WebSocket/Worker backend on a separate
port, pinned runtime and ephemeral room IDs; public Cloudflare access must not be
required for the core acceptance suite. No app code or deployment is added now.

TCP-to-WebSocket conversion must respect application framing, not assume one TCP
read is a packet. Preserve ordered action messages, session identity and delivery
credits, with bounds on payloads, fragments, compression/decoding and queues.
Prefer binary uncompressed frames initially. The user approved local handling of
frequent delivery POLLs from a bounded receive cache, so each ROM poll does not
cross the cloud. Prove its exact session/credit behavior. Coalesce only superseded
presence, using real time independently of emulator speed. This adaptation is
accepted as a direction, but its transport behavior is not already proven.
Local handling must not invent server acknowledgements, hide upstream failure,
renew a dead ROM's presence forever or replay old actions after reconnect.

An upstream WebSocket failure must invalidate the affected transport/session:
close or fail its local socket in a defined way, discard stale pending data and
let the ROM establish a fresh valid session. Avoid reconnect storms with bounded
retry/backoff. Select+B remains a ROM-owned local escape even if the bridge is
missing or broken. Log adapter readiness, local TCP connection, upstream WSS
connection and confirmed lobby join as distinct states, without logging secrets.

Do not assume Cloudflare's ordinary HTTP proxy protects an arbitrary TCP listener.
The documented Spectrum product for custom TCP/UDP requires Enterprise plus a
paid add-on; it does not turn a Worker into a raw-TCP listener. Infrastructure
attack protection can remain outside game code. With the selected bridge, public
ingress is WSS and raw TCP stays on loopback, so neither a publicly hosted TCP
gateway nor Spectrum is required. Application abuse limits still apply to the
Worker/object. [Spectrum][spectrum].

Worker/object lifecycle is controlled by the platform, not a guarantee that a
process stays alive until ten minutes after its last packet. Hibernation can
discard in-memory object fields while keeping eligible WebSockets connected.
Connection attachments can retain limited connection metadata across hibernation;
fresh client snapshots can rebuild presence. Design reconstruction explicitly.
Idle-room expiration is a separate application policy, not reliable evidence
that the provider destroyed a worker. [Lifecycle][lifecycle], [WebSockets][ws].

## Lobby codes and reconnect

The preferred lobby UI now lives in the Bridge app. The emulator runs the ROM,
which identifies its client and compatibility metadata through the local handshake.
Bridge displays the client and lets the user create, select or join a lobby.
Optional local ROM import/share can display embedded metadata and compare it with
the running client's declaration. Only metadata needs to travel to the cloud.
Show confirmed online membership only after an actual server join acknowledgement.
An imported file or an open local TCP socket alone is not a confirmed lobby join.

Changing rooms requires a fresh session and discarding stale room-specific pending
events. Bridge must respect an explicit ROM disconnect instead of automatically
rejoining it. Select+B remains available locally regardless of Bridge UI state.
The earlier candidate in-ROM Multiplayer Options menu is superseded by this
preferred app flow; exact UI and discovery implementation remain open.

A six-digit code is a plausible Bridge UX. It has at most 1,000,000 values
and is an invitation code, not strong authentication on its own. The current
16-bit room field cannot represent every six-digit value; a future protocol must
expand it or map a code to a separate opaque room identifier. Preserve leading
zeroes in the UI and never silently truncate or hash into the current field
without collision handling.

Two candidate flows remain open: users pick the same code and the first join
creates a room, or a host creates a room and receives a unique random live code.
The latter avoids accidental merging of independently created rooms. Either way,
coordinate concurrent creation, enforce four-member capacity, rate-limit guesses
and creation, expire unused incarnations, and avoid leaking reconnect secrets.
Per-session unguessable credentials and room incarnations remain distinct from
the human code. Six-digit codes alone cannot promise private rooms against a
determined guesser. The planned bridge provides certificate-validated WSS on the
Internet segment; the current direct-TCP implementation has no such encryption.
Do not confuse a human invitation code with transport encryption or a session token.

Ten minutes after the last member leaves could expire a room, but that value is
only a proposal. Live players need much shorter liveness checks, and idle connected
players may legitimately send heartbeats. After expiry/restart, do not let stale
packets or stale reconnect credentials attach to a different room incarnation.
No new code/setting should enter `.sav` just for convenience.

## Latency and indicative cost

Assessment: four-player presence and turn-based interactions are plausible on a
regional server or Durable Object, but there is no measured Cloudflare result.
The input-to-remote-display path includes local game timing, adapter transfer,
client-to-room travel, room processing, room-to-peer travel and peer rendering.
The selected device-local bridge adds local processing, not a separately hosted
public TCP gateway hop. That overhead and Android scheduling still need measurement.
A worldwide edge entry point does not
place a room's coordinator next to all four players. Location hints can influence
initial placement but are not guarantees. [Data location][location].

Benchmark real clients, including four mixed 100%-400% speeds, with same-region
and cross-region paths. Preserve the existing additional-presence-latency targets
and matched-NPC frame gate; do not use a fast HTTP ping or low server CPU as proof
of playable end-to-end latency. No lockstep movement or global frame clock is
required; superseded presence can be replaced, unlike accepted actions.

Provider prices checked 2026-10-11, USD, before tax. No hosted gateway cost is
required by the local-bridge design; application distribution/maintenance still
requires work. Workers Paid
has a $5/month minimum. Durable Objects include 1 million request units and
400,000 GB-seconds monthly, then $0.15/million requests and $12.50/million
GB-seconds. Incoming WebSocket messages use a 20:1 billing ratio; outgoing messages
do not add request charges. Duration uses 128 MB per billed object, not its actual
tiny application state. Billable excess is rounded per the provider's rules.
[Workers pricing][workers-pricing], [Object pricing][object-pricing].

Illustrative calculation, not a benchmark or deployment quote:

- 100 total lobby-hours per month, four clients each sending 10 WebSocket
  application messages per second: `100 * 3600 * 4 * 10 = 14,400,000` messages.
- At 20:1: 720,000 object request units, plus handshakes/other operations.
- If every lobby were billable for its whole connected duration:
  `100 * 3600 * 0.128 = 46,080 GB-seconds`.
- With modest other account usage, these assumed object totals fit the paid
  included quotas. A small deployment could therefore fit the $5 base platform
  charge, before storage, routing-Worker extras or other services.

That example assumes WebSockets; one HTTP/RPC call per update has different request
accounting. Hibernation eligibility may reduce duration, but a tiny/no-database
room is not automatically free. WASM is not a discount category. Actual cost
depends on aggregate lobby-hours, message rate and runtime design. Cloudflare is
selected as the direction; no paid plan or public deployment is selected here.

### Free-tier planning bound

The free Durable Object quotas currently include 100,000 request units and 13,000
GB-seconds per day, shared across the account. SQLite-backed objects are available
on Free; using that object class does not require storing game data. Excess
operations fail rather than automatically obtaining unlimited capacity.
[Object pricing][object-pricing], [Object limits][object-limits].

At the illustrative ten upstream application messages per second for each of four
players, one room generates `4 * 10 * 3600 / 20 = 7,200` request units per hour.
The request allowance therefore covers about `100,000 / 7,200 = 13.89` aggregate
lobby-hours per day before other requests. Ten simultaneous rooms consume that
allowance in about 1.39 hours at that rate. This is a request-only bound, not a
promised capacity. At continuously billed duration, the duration quota independently
covers `13,000 / 0.128 / 3600 = 28.21` aggregate lobby-hours. Hibernation can improve
the duration side, not erase application-message usage. Handshakes, heartbeats,
unfiltered polls, retries, routing-Worker/CPU/storage limits and abusive requests
also matter. The ten-message rate is an example, not the measured ROM rate or a
selected update-frequency limit.

The user accepts these documented quotas as sufficient for the initial target.
Free is the starting hosting budget; this does not authorize a paid upgrade or
public deployment. Measure total messages and runtime cost, including 400%
emulation, and expose quota exhaustion as a recoverable disconnect.
Use hibernation-aware connections and reconstructible state; do not keep objects
active unnecessarily with polling timers. Actual provider integration/latency and
metering should be checked before public rollout, without weakening frame or
presence-freshness requirements to fit an unmeasured cost assumption.

## Current assurance and missing tests

This is a focused source inspection for planning, not an exhaustive security audit.
Existing protections/tests include bounded 120-byte payloads, early header
rejection, exact typed-message lengths, version/capacity checks, session/generation
checks, sequence/map-epoch ordering, read deadlines, bounded lobby writer queues,
latest-state coalescing, reconnect/expiry and real TCP fragmentation tests.

Visible gaps to close before claiming public hostile-client readiness:

- Global 16-socket admission has no per-source/client fairness policy. One sender
  must not indefinitely monopolize admission through many maintained connections.
  Room creation, valid-message rate, logging and session churn need budgets too.
  Account for several legitimate players behind one NAT; verify normal four-client
  traffic at up to 400% speed against the limits instead of rejecting it as abuse.
- The model's monotonically increasing 16-bit `next_player` permanently rejects
  new identities after exhaustion, even when old players are gone. Test reuse or
  wider identities with safe generations, plus repeated joins/leaves and expiry.
- A single global model mutex and notification channel couples rooms. Test and
  bound the effect of one busy room on another before scaling the development
  model. Remove panic/poison chains reachable through untrusted inputs.
- The pre-lobby echo/diagnostic path has writes without the lobby writer's timeout.
  Audit every exposed path, bound echo/logging work and disable or explicitly limit
  development-only behavior for a public service.
- No dedicated fuzz/property harness or declared per-client CPU/memory test budget
  is currently present. Passing normal test vectors is not adversarial completeness.

Required test families as the system develops:

The existing codec/model, resource/fairness, connection churn, restart and
cross-room cases below are explicitly current milestone-2 work in PLAN.md, not
deferred until later feature milestones. Include the selected bridge foundation
and compatibility boundary in that milestone's transport proof.

| Family | Observable requirement |
| --- | --- |
| Codec fuzz/property tests | Arbitrary bytes, malformed lengths, unknown fields and fragmented/coalesced streams never panic or allocate beyond a fixed bound. |
| Model sequence/property tests | Arbitrary join/leave/resume/action order, clock advance and identifier wrap preserve membership, ownership, capacity and session freshness. |
| Admission/resource tests | Slow readers, partial writers, valid floods, repeated joins/leaves, empty rooms and log pressure stay bounded and leave healthy clients serviceable. |
| Cross-room tests | A malicious or failed room cannot route actions to or starve another room. Task/process/gateway failure boundaries are measured separately. |
| Restart/reconnect tests | Drop all transient server state; clients resume ordinary play, cancel uncertain actions and establish fresh identities without touching save layout. |
| Feature action tests | Consent, targets, busy states, inventory bounds, collision policy and travel validation have positive and negative cases when implemented. |
| Real-ROM tests | Invalid server input, four-player speed mismatch, priority exit, native save integrity, stale compatible states and matched-NPC frame cadence remain valid. |

Use fake time and injected randomness for fast deterministic model tests, replayable
fuzz seeds, ephemeral TCP ports and machine-readable results. A separate headless
ROM suite must need no physical phone; ADB verifies real-device compatibility.
Keep the human one-command Python session independent from destructive test setup.
Measure behavior coverage against these invariants, not a cosmetic 100% line score.

[rust]: https://developers.cloudflare.com/workers/languages/rust/
[tcp]: https://developers.cloudflare.com/workers/runtime-apis/tcp-sockets/
[objects]: https://developers.cloudflare.com/durable-objects/concepts/what-are-durable-objects/
[spectrum]: https://developers.cloudflare.com/spectrum/
[lifecycle]: https://developers.cloudflare.com/durable-objects/concepts/durable-object-lifecycle/
[ws]: https://developers.cloudflare.com/durable-objects/best-practices/websockets/
[location]: https://developers.cloudflare.com/durable-objects/reference/data-location/
[workers-pricing]: https://developers.cloudflare.com/workers/platform/pricing/
[object-pricing]: https://developers.cloudflare.com/durable-objects/platform/pricing/
[object-limits]: https://developers.cloudflare.com/durable-objects/platform/limits/
[android-launch]: https://developer.android.com/develop/background-work/services/fgs/launch
[android-types]: https://developer.android.com/develop/background-work/services/fgs/service-types
[android-timeout]: https://developer.android.com/develop/background-work/services/fgs/timeout
