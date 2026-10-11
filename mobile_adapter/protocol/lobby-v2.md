# Lobby schema 3

One TCP stream per ROM connects through libmobile's ISP path. Frames use
transport-v1's eight-byte PC header, version 1 and maximum 120-byte payload.
All application words are unsigned big endian. Socket reads and adapter DATA
responses can split or combine frames. The server publishes the latest state using coalescing delivery credits from
each client, independently of which peer produced the update.

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
| DELTA | 24 | ROSTER layout, containing changed existing peers only |

Schema is 3, mechanics 0 standard or 1 faithful, capacity 3. The ROM selects
public test lobby 1. IDs are nonzero, assigned by the server independently of
trainer IDs, runtime objects and socket sessions. Rooms permit three connected
players. The service retains at most 128 identities globally and accepts 16
sockets. IDs are not reused in a server lifetime; generations never wrap.

A peer record contains ID u16, generation u16, revision u16, then its snapshot.
A full roster contains up to two peers and has a maximum 39-byte payload. It
replaces membership and removes absent peers. Membership/generation changes
always send a full roster. DELTA updates existing records by explicit ID and
cannot create membership. The ROM checks receiving identity, duplicates, self
records, unknown delta IDs and all snapshot bounds before publishing records.

| Snapshot offset | Size | Meaning |
| --- | ---: | --- |
| 0 | 1 | Map group |
| 1 | 1 | Map number |
| 2 | 2 | Physical map block-layout key |
| 4 | 2 | Scene-entry epoch |
| 6 | 3 | Packed absolute X/Y: X[11:4], X[3:0] with Y[11:8], Y[7:0] |
| 9 | 1 | Facing bits 7..4; movement bits 3..1; availability bit 0 |
| 10 | 1 | Chosen figure: Chris 0, Kris 1, Crys 2, Beta 3 |

Coordinates retain all 12 bits (0..4095), facing is 0..15, movement is normal 0,
run 1, bike 2, skate 3, surf 4 or surf Pikachu 5. Availability is 0 paused or
1 safe overworld. Figure is read from the actual saved wPlayerGender. Values above 3 are rejected. Older schema-2 fixed-Chris clients are incompatible. The ROM expands peer
records to its own 20-byte internal layout after validating packed frame size.
Expansion scratch occupies the last 17 bytes of the private APP buffer; a packed
full roster occupies at most 47 bytes including its header.

Capture follows the player-object step and combines logical tile coordinates,
fractional progress from the walking timer and vertical jump offset. Slow,
normal, run and bicycle steps have separate durations. Camera projection uses
the same fractional progress. A negative border position is clamped until the
connected map loads. Generation/epoch changes reset interpolation; ordinary
rendering approaches the latest target, snapping beyond 32 pixels.

Entry and paused menus/scripts invalidate capture. The next safe scene increments
its epoch and hashes interior map blocks, excluding connection padding and
including tileset. Scripted block changes therefore invalidate the visual key.
The 16-bit key is a conservative visual hint, can collide, and does not synchronize
story flags, pickups, puzzles or encounters. Rendering requires matching map/key
and safe availability. Remote avatars have no collision or NPC scripts.

WELCOME/LEFT repeat the request sequence. Increasing PRESENCE revisions and
scene epochs use modulo-65536 serial arithmetic with difference 1..32767. A
changed map/key needs a newer epoch. Generation changes reset revision tracking.
Push sequences belong to server notifications, not outstanding requests.

Each connection keeps one latest model snapshot, one watch notification and one
previous normalized roster. The writer coalesces for 8 ms and skips heartbeat-only
and self-only changes. One pending delivery credit per adapter DATA request additionally bounds
slow receivers: PRESENCE and POLL grant a credit, repeated grants coalesce, and
the writer keeps a held credit until the latest peer state changes. At most one
write plus one pending credit can exist. Credit is replenished by each client's
own requests, independently of the sender's emulator speed. Movement never
becomes an event backlog in the model or an unbounded outgoing TCP queue. A one-entry
control channel prioritizes LEFT/ERROR. Independent reader/writer tasks preserve
partial incoming frames while pushing outgoing state. Writes have a two-second
deadline. Later reliable actions need separate bounded queues and credits.

The ROM sends changed snapshots at a minimum one-frame interval plus 500-ms
keepalives. Otherwise adapter DATA carries POLL. Pinned libmobile waits up to one
second for an empty TCP receive; nonempty POLL avoids that wait without changing
libmobile. POLL has no reply and does not renew the presence lease. The foreground
parser copies at most 32 application bytes and publishes one completed frame per
pump. Header/body chunks stop at the validated frame boundary; lengths cannot
spill into another frame or the dedicated buffer. Adapter checksum validation,
application parsing and preparation of the next request run in separate pumps.
Serial bursts share at most 24 bytes per VBlank. At most four foreground pumps
run per frame across the existing hooks. No network wait runs on the game CPU.
These caps bound work; the strict matching-input world-cadence check remains a
separate requirement and must pass before claiming timing parity.

After a broken socket, automatic retry waits 240 frames and submits the previous
ID/token. Disconnected identities remain resumable for 30 seconds. Resume advances
generation; old writes/cleanup cannot remove the replacement. Presence expires
after three seconds without a valid update. Explicit LEAVE immediately removes
identity and LEFT clears ROM credentials.

ERROR codes: malformed 1, incompatible 2, full 3, stale 4, resume unavailable 5,
service capacity 6. Error 5 clears old credentials and retries a fresh join,
including after server restart. Other semantic errors stop automatic joining.
Tokens use OS randomness through pinned getrandom 0.3.4. This local protocol has
no encryption, accounts or identity persistence across restarts.

Select+B toggles online intent. Select+A opens status/roster/controls. Select+B+A
retains transport-v1's echo diagnostic. Physical joypad bytes remain intact for
debouncing; accepted chords filter mirrored game input. Cable/printer use releases
mobile ownership. Offline play remains available without a server.

Latency acceptance observes the first captured local world motion and the
first directional change in remote four-quad avatar bounds at the native video
callback. Facing flips alone do not count. The network/rendering p95 budget is
200 ms local or 300 ms RTT100. The separate input-to-remote statistic includes
the unchanged vanilla turn script, which often takes roughly 130 ms before
movement begins. Earlier input-to-remote 200/300-ms experiments failed; those
results remain recorded, rather than being presented as accepted measurements. RTT100 uses a transparent TCP test bridge with independent 50-ms upstream and
downstream delivery deadlines. Its queues are capped at 64 KiB per direction.
Multiple packets share the propagation delay rather than each imposing a new
serial wait. This is a reproducible loopback-delay experiment, not a full WAN
loss/bandwidth simulation or a real mobile network measurement.


Fast-forward acceptance runs one actual core at 3x and two at 1x. Presence uses
absolute world positions, map/instance/epoch and monotonic session revisions,
never an assumption of shared frame counts or equal walking speed. The server
uses wall-clock leases; client frame counters only pace local transport. Slow
receivers may skip intermediate positions and snap, but must converge on the
latest identity, generation, map and position. Future battles and transactions
must synchronize explicit actions/acknowledgements rather than emulator frames.
