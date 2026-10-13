# Lobby schema 4

The historical filename is retained for links. This is the current schema-4
four-player wire contract. Schema 2/3 clients must update with their Bridge.
Each ROM opens one TCP stream through libmobile's ISP path to the local Bridge.
Transport-v1 has an eight-byte PC header, version 1 and a maximum 120-byte payload.
Words are unsigned big endian. Reads and adapter DATA responses may split/combine
frames; parsing never assumes a socket read equals a complete application frame.

| Kind | Value | Payload |
| --- | ---: | --- |
| HELLO | 16 | schema u8, mechanics u8, capacity u8, lobby u16, resume ID u16, token 8 bytes |
| WELCOME | 17 | ID u16, generation u16, lobby u16, token 8 bytes, capacity u8 |
| PRESENCE | 18 | ID u16, generation u16, snapshot 11 bytes |
| ROSTER | 19 | receiver ID u16, generation u16, count u8, count times 17-byte peer record |
| LEAVE | 20 | ID u16, generation u16 |
| LEFT | 21 | ID u16, generation u16 |
| ERROR | 22 | error code u8 |
| POLL | 23 | empty, sequence zero |
| DELTA | 24 | ROSTER layout, changed existing peers only |
| RECEIPT | 25 | receiver ID u16, generation u16 |

Schema is 4; mechanics is standard 0 or faithful 1. Capacity is read from the
single `rom/capacity.asm` declaration by both assembly and the Rust build script.
The supported value is four total players, with three generic remote pool entries.
This knob is subject to ROM, RAM, graphics and audited layout bounds; increasing
it beyond the supported value requires new acceptance, not just changing a number.

The ROM initially names lobby 1. Bridge configuration selects the household's
actual lobby and validates/translates the local HELLO lobby field. Prototype room
IDs are canonical nonzero 16-bit numbers, not secure invitations or passwords.
Server IDs are independent of trainer IDs, object slots and TCP sessions.
Generations/identifiers fail closed on exhaustion rather than silently wrapping.
The shared room bounds retained identities to 128, active players to four,
households to eight and emulator stream IDs to 1..16.

A peer record contains ID, generation and revision (u16 each), then its snapshot.
A full three-peer roster is 56 payload bytes or 64 bytes with its transport header.
It replaces membership; an absent peer is removed. Membership/generation changes
send a full roster. DELTA cannot create a member. The ROM validates receiving
identity, duplicates, self records, unknown delta IDs and every field before
publishing the bounded pool. Internal records are separate from packed wire bytes.

| Snapshot offset | Size | Meaning |
| --- | ---: | --- |
| 0 | 1 | Map group |
| 1 | 1 | Map number |
| 2 | 2 | Physical map block-layout key |
| 4 | 2 | Scene-entry epoch |
| 6 | 3 | Packed X/Y: X[11:4], X[3:0] with Y[11:8], Y[7:0] |
| 9 | 1 | Facing bits 7..4; movement bits 3..1; availability bit 0 |
| 10 | 1 | Saved figure: Chris 0, Kris 1, Crys 2, Beta 3 |

Coordinates are 0..4095, facing 0..15 and movement normal 0, run 1, bike 2,
skate 3, surf 4 or surf Pikachu 5. Availability is paused 0 or safe overworld 1.
Native capture combines logical coordinates, step-timer progress and jump offset;
projection follows the local camera. Epoch/generation changes reset presentation.
Old complete avatars remain visible until a bounded idle update fits; obsolete
intermediate presence may be skipped. Remote actors have no scripts or collision.

Map instance keys hash interior blocks and tileset in bounded idle phases. A
scripted layout change invalidates the visual key. The 16-bit key can collide and
is only a visibility hint. It never synchronizes story, items, puzzles or encounters.
Transport/schema/capacity/mechanics checks are not complete cross-revision game
compatibility. The current 15-byte HELLO carries no clean-game build digest.
The build embeds a clean-counterpart digest, but live digest negotiation and
Bridge/server enforcement remain open work. Protocol identity must be checked
separately from game content identity.

## Household envelope and owner

Several emulator streams share one Bridge session and one upstream WebSocket.
Binary WebSocket messages have a six-byte envelope: ASCII MB, version 1, operation
1 for frame or 0 for stream close, then stream u16. Operation 1 contains exactly
one complete bounded PC frame; operation 0 has no trailing data. Maximum message
size is 134 bytes. Closing one stream leaves its siblings intact. Invalid envelope
framing may close the household; invalid one-player payloads are scoped to that
stream. No envelope contains an address belonging to another player.

Local mode embeds the Rust Owner with no upstream. Internet mode routes the
upgrade at `/lobbies/<id>/ws` to one owner per lobby. Native hosted simulation and
WASM Durable Object share this same socket-independent owner; subsequent DO
messages are handled by the DO, not a Worker relay per packet. The DO's validated
hibernation attachments retain transient owner state and accepted socket identities.
They do not contain cartridge saves, inventory or any irreplaceable game state.

Internet presence and authority rosters coalesce at 20Hz per player. Embedded mode
flushes dirty state immediately. Local POLL grants one delivery credit and stays
local; repeated credits coalesce. A changed roster supersedes the pending one.
Validated identity-bound authority RECEIPTs confirm accepted presence without
copying a roster each time. They refresh liveness; local polls/movement cannot
hide a dead authority. Local replies never fabricate a join or remote freshness.
No pending roster means no TCP response; the adapter can complete an empty
nonblocking exchange. Control output, write queues, socket admission, deadlines,
room counts and test delay queues are bounded. See the source and tests for limits.

WELCOME/LEFT repeat the request sequence. Revisions/epochs use modulo-65536
serial arithmetic with forward difference 1..32767. Map/key changes need a newer
epoch. Generations reset revision tracking. Server push sequences are independent
of outstanding requests. Presence expires after three wall-clock seconds; detached
identities remain resumable for thirty seconds. Resume requires the opaque token
and advances generation. Old cleanup cannot remove a replacement session.
Server restart discards disposable membership; unavailable resume credentials cause
a fresh join. Explicit local exit disables recovery until a later explicit join.

ERROR codes are malformed 1, incompatible 2, full 3, stale 4, resume unavailable 5,
service capacity 6. Code 5 clears credentials and retries fresh admission. Other
semantic errors stop joining. Tokens come from host entropy; WSS protects remote
transport, while loopback development permits WS. Modified clients are not attested.

## ROM priority and acceptance

Physical Select+B exits active/stale multiplayer intent before engine input locks;
a fresh press in the fully offline development workflow can join. Select+A opens
status/roster/controls; Select+B+A requests the offline echo diagnostic. Exit never
waits for an adapter acknowledgement and does not interrupt native save writes.
The ROM keeps network state out of SRAM and projects original leased actors into
native save copies before their ordinary checksums. Native palette scans use the
original actor views; remote hardware colors have private bounded storage.

Serial activity is restricted to native wait time. Heavy parser/CRC/map-hash/LZ/
upload jobs have measured admission deadlines. Application publication copies at
most 32 bytes per job, native decompression at most eight bytes per job, map hashing
16 bytes per job and at most four transport pumps per frame. Limits alone are not
proof of cadence: real instruction profiling checks every awaited VBlank boundary.

Current four-core acceptance uses 100/200/300/400% speeds through two Bridges,
matching ordinary native NPCs and a 100ms injected hosted RTT. The separate visible
latency gate measures local world motion to directional movement of a complete
remote avatar: p95 below 200ms local or 300ms RTT100. Full input-to-visible delay
is also retained, including unchanged native turning time. Tests are loopback
experiments, not guarantees for every WAN. Later battles/transfers must synchronize
explicit actions and bounded acknowledgements rather than emulator frame counts.
