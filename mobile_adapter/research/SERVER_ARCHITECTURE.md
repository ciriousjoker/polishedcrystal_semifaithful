# Server ownership, hosting options and assurance

Updated: 2026-10-11. Inspected source: `f8bf5339a`; this follow-up changes
documentation only. Public deployment, a provider migration and new gameplay
features are not being implemented. The user wants the current architecture to
leave room for the desired features in [PLAN.md](../PLAN.md).

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
2. Optionally a Worker/Durable Object adapter for a future compatible transport.

One process may host many four-person rooms. Separating room ownership and work
budgets first avoids requiring a dedicated VM/container per tiny lobby. Later,
route rooms to different processes/hosts for a smaller process-failure impact and
regional placement. Async tasks do not isolate a process crash; a shared gateway,
host or provider incident can still affect multiple rooms. An independent logical
room is not a claim of physically independent infrastructure.

## Cloudflare assessment

Rust/WASM is supported through `workers-rs`, but the current Tokio TCP listener
cannot become a Worker just by changing its compilation target. Workers expose
event-driven platform I/O. The official TCP API currently supports outgoing TCP,
not incoming raw TCP listeners. Our current ROM/libmobile protocol is raw TCP,
not an HTTP/WebSocket handshake. [Rust support][rust], [TCP sockets][tcp].

The closest Cloudflare model to the proposed room-per-worker idea is a routing
Worker plus one named Durable Object per room. An object is created on access
and coordinates its room's connections/events. It is not a separately deployed
server binary for every PIN. Durable Objects may use transient memory without
making the server authoritative over game saves. [Durable Objects][objects].

However, that model needs a transport path:

- A TCP-to-WebSocket gateway keeps current emulator compatibility but remains
  running infrastructure, adds cost and an extra hop, and needs its own limits,
  protection and failure recovery. Multiple gateways can reduce shared impact.
- A WebSocket-capable emulator/libmobile bridge avoids that gateway but is not
  currently implemented or assumed available on Mac and Android.
- Implementing a web transport in the ROM is another substantial compatibility,
  authentication and footprint project; it is not a free consequence of using
  libmobile. Do not silently add it to the current ROM scope.

Do not assume Cloudflare's ordinary HTTP proxy protects an arbitrary TCP listener.
The documented Spectrum product for custom TCP/UDP requires Enterprise plus a
paid add-on; it does not turn a Worker into a raw-TCP listener. Infrastructure
attack protection can remain outside game code, but the actual transport needs
a compatible provider arrangement before public release. [Spectrum][spectrum].

Worker/object lifecycle is controlled by the platform, not a guarantee that a
process stays alive until ten minutes after its last packet. Hibernation can
discard in-memory object fields while keeping eligible WebSockets connected.
Connection attachments can retain limited connection metadata across hibernation;
fresh client snapshots can rebuild presence. Design reconstruction explicitly.
Idle-room expiration is a separate application policy, not reliable evidence
that the provider destroyed a worker. [Lifecycle][lifecycle], [WebSockets][ws].

## Lobby codes and reconnect

A six-digit code is a plausible GBC-friendly UX. It has at most 1,000,000 values
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
determined guesser. Public transport confidentiality also needs a design; the
current raw TCP is not encrypted by merely choosing a private-looking code.

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
Adding a gateway adds another segment. A worldwide edge entry point does not
place a room's coordinator next to all four players. Location hints can influence
initial placement but are not guarantees. [Data location][location].

Benchmark real clients, including four mixed 100%-400% speeds, with same-region
and cross-region paths. Preserve the existing additional-presence-latency targets
and matched-NPC frame gate; do not use a fast HTTP ping or low server CPU as proof
of playable end-to-end latency. No lockstep movement or global frame clock is
required; superseded presence can be replaced, unlike accepted actions.

Provider prices checked 2026-10-11, USD, before tax and any gateway. Workers Paid
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
  charge, before gateway, storage, routing-Worker extras or other services.

That example assumes WebSockets; one HTTP/RPC call per update has different request
accounting. Hibernation eligibility may reduce duration, but a tiny/no-database
room is not automatically free. WASM is not a discount category. Actual cost
depends on aggregate lobby-hours, message rate, runtime design and the missing
TCP transport solution. No vendor, paid plan or deployment is selected here.

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
