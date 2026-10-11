# Milestone 2 implementation and acceptance

Completed on 2026-10-11 on branch `mobileadapter`. Three-player lobbies and live
overworld presence passed the gates below. Calls, battles and durable transfers
remain milestones 3 through 5. Machine-readable evidence with hashes and per-core
observations is in [milestone2-evidence.json](milestone2-evidence.json).

The final compiler outputs, before adapting endpoint/checksum in test copies:

| Build | SHA-256 |
| --- | --- |
| Standard mobile | `725d94ee2e8adec0817a9398e9fa3a1056908943e429ccbdaa335384f3a7e3e8` |
| Faithful mobile | `81e0664967741a11c30143379985df231bb97b2ef00fdde9049349664bbf36d7` |
| Disabled standard | `314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e` |

Every accepted test ROM was normalized back to localhost port 17423 and its
global checksum recomputed. Its resulting hash matches the appropriate final
compiler output. Earlier intermediate hashes in PLAN.md are superseded.

The implementation uses feature-owned ROMX bank 7f, 121 bytes of resident WRAM0
at cec4 and 1473 bytes of WRAMX bank 2 at d4eb. It occupies 6924 driver bytes
under the 8192-byte cap. All 23373 existing RAM symbols remain at baseline
addresses. The standard feature build leaves seven ROM0 bytes and 6983 ROMX
bytes free. No existing helper was relocated, no mapper changed, no new SRAM
or HRAM was allocated. The disabled ROM's exact SHA-256 remains
314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e.

The renderer does not allocate runtime NPC slots. Two private peer records and
reserved sprite tiles produce local+peer OAM ahead of ordinary objects. The
approved Chris appearance reuses the local player's palette. Standing tiles
are VRAM0 64..6b and VRAM1 7c..7f; walking tiles are VRAM0 e0..eb. Ordinary
12-tile sprite allocations, emotes at 60..63 and effects at 6f..7f remain intact.
The 15-tile final-slot Gyarados/Exeggutor walking allocation overlaps 7c..7e,
so those active large sprites explicitly pause social drawing. Sailboat,
Ho-Oh and Lugia types fit the ordinary tile ranges.

Generated rendering masks cover all 611 assembled maps. 407 have eligible
optional actors: generic trainers, no-op decoration, pure text commands and
complete text-only scripts ending in a cosmetic turn of LAST_TALKED. Custom
story/service scripts, cameras, rocks and boulders remain protected. Nearby
actors within two tiles stay visible. Total OAM pressure may suppress temporary
facing bytes, restored immediately after vanilla InitSprites. A second sweep
checks the ten-sprites-per-scanline limit and may hide whole optional actors in
that frame's OAM without touching their object state. If required actors still
cannot fit, the exact vanilla OAM is restored and status reports a pause.
Weather fills remaining scanline space after avatars. No story flags or collision
objects are changed by rendering.

Protected exceptions are scripted/camera/menu scenes, overlapping large sprite
tiles and unavoidable required-actor OAM pressure. Select+A reports the reason
and exposes the roster when normal input is available. These are temporary
rendering exceptions; connection membership remains independent. Every peer
must also share map group/number, a physical block-layout key and safe availability.
World progress stays local. The 16-bit key is a visual hint, not collision-free
instance identity or story synchronization.

Integration tests boot ordinary 32-KiB in-game saves and perform actual inputs.
Observer addresses come from matching SYM files. No game RAM/SRAM editing or
emulator state injection occurs. The read-only profiling core checks physical
banks, registers, stack, actual VBlank serial budget and NPC facings immediately
before MobileRestoreFacings returns. Arbitrary video observations can occur
inside the temporary hide window, so those are not the exact restoration oracle.
Manual-play start snapshots are a separate deployment feature and are regenerated
from ordinary saves with each current ROM/core build.

The latest-state server uses a bounded model, watch notification and previous
roster per connection. It coalesces for 8 ms, sends full membership rosters or
changed-peer deltas, and never queues walking events. Each receiver grants a coalescing delivery credit through PRESENCE or POLL;
one held write plus one pending credit bounds old movement even with a 3x sender.
POLL avoids libmobile's one-second empty TCP receive wait. It neither produces replies nor refreshes
presence. Each client has a three-second presence lease and 30-second token
resume window. Reconnect advances generation and invalidates old writes/cleanup.
Calls/interactions, battles and durable gifts/trades remain milestones 3 to 5.


The read-only profiler retains the original ISR (<2048 cycles), foreground
(<32768 cycles), stack floor (c040), bank/register and half-VBlank serial gates.
The APP parser copies at most 16 bytes and publishes at most one complete frame
per pump. Adapter checksums, APP parsing and next-request preparation are split.
Observed foreground elapsed time still includes preempting vanilla interrupts.
Stack measurements exclude SP used as a graphics copy iterator only with IME
disabled and PC/bank in ten source-audited video/gfx/tileset copy ranges from the
current SYM. Excluded sample count and raw minimum remain separate metrics.
All true stack values and routine return state remain checked. Earlier 94a0/
45fd raw minima belonged to these native copy routines, not mobile stack writes.

A two-phase final observation barrier keeps all three cores running until every
client is ready, then captures all states before allowing any frontend teardown.
This prevents the test harness from expiring or disconnecting its earliest client
before the last client's assertions. No game state is altered by this barrier.
Latency uses the ordinary core with real-time pacing and native video callbacks;
profiling is a separate instruction observer and introduces extra host work.

## Final measured gates

Twenty Rust tests, eight host deployment tests, RGBDS contracts and emitted
protocol vectors passed without starting an emulator. Cargo formatting and
Clippy with warnings denied passed. The renderer audit checks 16 native facing
definitions, tile allocations, emotes/effects and six special sprite headers.
The generated policy covers all 611 maps; 407 contain eligible optional actors.

Fifteen three-core functional/profile scenarios passed: dense Game Corner,
Snowtop camera and differing map layout, Cinnabar Volcano, large Gyarados,
300%/100%/100% with 100-ms RTT, menu suspension, explicit rejoin, socket token
resume, warp, connected border, running, bike, surf and faithful. Each uses
three independent native game processes and ordinary battery saves. The speed
case performs 24 alternating movement bursts and checks final absolute position,
map, identity and generation agreement. A further two three-core latency cases
and all 12 Echo regression scenarios passed: 65 native processes in total.
This is representative dynamic coverage, not gameplay of every audited map.

| Runtime measurement across accepted profiling cases | Result | Gate |
| --- | ---: | ---: |
| Longest serial handler | 1,432 cycles | <2,048 |
| Longest foreground tick, including preempting interrupts | 25,268 cycles | <32,768 |
| Maximum serial work per actual VBlank interval | 67,840 cycles | <70,224 at double speed |
| Lowest true stack pointer | c091 | >=c040 |
| Register/bank preservation failures | 0 | 0 |
| NPC facing restoration failures | 0 | 0 |

Nineteen native processes were profiled. The lowest observed stack pointer is
145 bytes above the c000 stack bottom. These are emulator measurements of these
fixtures, not a hardware timing certification or a future battle budget.

Latency uses 40 movement samples per case and an independent bounded TCP bridge
with 50 ms of propagation in each direction for the 100-ms RTT case. Four-quad
position bounds reject mere facing flips as motion. Both timings are retained:

| Timing, p95 | Local | Added 100-ms client/server RTT |
| --- | ---: | ---: |
| Local world motion to remote visible motion | 93.879 ms | 198.846 ms |
| Initial input to remote visible motion | 244.252 ms | 350.064 ms |

The original full-input 200/300-ms targets failed. Acceptance explicitly changed
to additional delay after local world motion, below 200/300 ms respectively;
unchanged vanilla turning precedes movement. This does not claim that the original
targets passed. These are controlled loopback measurements, not mobile/WAN latency.

The final ROM also passed a real Mac RetroArch plus physical Pixel GUI check.
Both source IPs joined lobby 1 with distinct player IDs and both screens displayed
the other avatar after normal movement. The Pixel cold-started the generated ROM
and current-core snapshot after unlocking. A disposable Mac F2 mapping provided
Select+B for UI automation; normal deployment controls remain left Shift+Z.
The hardware pair ran at normal speed; the three-speed proof uses native headless
cores. See [../MANUAL_TESTING.md](../MANUAL_TESTING.md) for the one-command workflow.

Raw artifacts remain outside Git under `/Volumes/Storage/Cache/mobileadapter/`:
`m2-accepted-final-{crowding,camera,instance,volcano,gyarados,speed3-rtt100}`,
`m2-complete-final-{menu,rejoin,resume,warp,connection,running,bike,surf,faithful}`,
`m2-final16-latency-{local,rtt100}`, `m2-final16-echo`, and
`m2-final16-gui-{server.log,input.json,mac.log}` / `m2-final16-phone-visible.png`.
Other similarly named directories are historical experiments and are excluded
from the final evidence summary.
