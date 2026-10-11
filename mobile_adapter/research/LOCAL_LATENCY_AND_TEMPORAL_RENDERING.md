# Local latency and proposed temporal figure rendering

Date: 2026-10-12. Investigation only; production ROM/server code is unchanged.

## Measured local path

The active user-owned hosted process was started with `--test-one-way-delay-ms 0`.
The human launcher defaults to hosted simulation, but injected delay defaults to
zero. The Flutter Local multiplayer selection starts the embedded Rust owner
without a WebSocket backend. Its UI status timer is 250 ms, but game data is
processed natively and does not wait for that UI timer.

A fresh two-core, embedded-local test with the corrected production ROM and normal
bedroom batteries measured 20 ordinary inputs: motion-to-remote p50 160.137 ms,
p95 163.035 ms, maximum 163.432 ms. Input-to-remote p95 was 280.976 ms, including
unchanged native turning before local movement. No simulated RTT was added.
These are paced native video-callback observations, not a measurement of the
user's phone, Wi-Fi, frontend display queue or arbitrary movement patterns.

Frame cadence and movement latency are separate contracts. The latest four-client
100..400% instruction profiles produce 800 world updates in 800 frames with no
background VBlank crossings. This does not make remote motion instantaneous.
The previous additional-latency acceptance target below 200 ms locally passes,
but it is visibly looser than the user's desired movement responsiveness.

Local bridge TCP uses NODELAY, and its service tick is 5 ms. The shared owner
bypasses the 50 ms dirty-roster throttle for local recipients. Internet mode
coalesces outgoing movement to 20 Hz. ROM capture runs on alternating frames;
serial commands, polling, bounded validation and peer presentation add scheduled
stages. Attribution of the full ~163 ms requires a timestamped stage breakdown.
Do not attribute this total to network RTT or claim a hardware minimum.

Remote projection currently calls `MobileApproachPixel` separately for X and Y.
Each call halves a small remaining error, snapping errors larger than 32 pixels.
A missed right-angle corner can therefore become a diagonal visual shortcut.
Independent-axis smoothing needs replacement with movement-segment-aware
presentation, or a bounded snap when the actual corner is unavailable. The ROM
must not invent traversable paths or delay the local game to reconstruct history.

Next performance work:

1. Trace local input/world motion, capture, serial update completion, owner
   receipt, receiver poll/commit and actual remote presentation in one measurement.
2. Reduce avoidable idle-stage and poll delays while retaining bounded processing,
   latest-only queues, fault handling and zero extra native world gaps.
3. Remove diagonal interpolation artifacts without accepting unbounded history.
4. Repeat two/four-client local and hosted latency, 100..400% cadence, malformed
   messages, Save isolation and dense-map tests after changes. Do not promise a
   numerical lower target before profiling it.

## User's rendering proposal

Prefer temporal sharing of complete character figures only when scanline pressure
requires it. Keep the local player continuously visible normally. Cycle two
competing remote figures across alternate frames, or three across three frames.
The user proposes permitting the local player to participate only as a last-resort
exception when ordinary NPCs already occupy the remaining line capacity. NPCs
should preferably remain active and present rather than being fully removed.
This is a proposal under evaluation, not an implemented renderer or accepted
proof that the old resource allocator can be deleted.

At ~60 emulated frames/s, those figures appear at ~30 Hz or ~20 Hz respectively.
Temporal omission is flicker, not hardware alpha blending. A fair whole-figure
scheduler must account for every scanline touched by each figure, both total OAM
capacity and reserved effects, and differently sized sprites. It must avoid
partial heads/bodies, indefinite starvation and oscillation around thresholds.
Native NPC simulation, scripts, collision and saves must remain independent of
which visual frames are omitted.

Recommendation: retain the local figure continuously and, in the full-NPC case,
time-share one deterministic NPC presentation with the remote figures. This is
visual admission only, so it does not require classifying that NPC as safely
removable from game logic. The user's local-player fallback remains an explicit
alternative, not a default that silently overrides local visibility priority.

A simple odd/even scheduler can alias with frontend frame skipping at 400% speed:
a frontend presenting every fourth emulated frame may always sample the same
phase. Test actual presentation behavior; all-frame headless OAM traces alone
cannot establish user-visible fairness. Do not assume guaranteed visibility with
arbitrary external frame dropping.

Temporal OAM admission solves scanline overflow only. Current native runtime has
13 object records including the local player. A full twelve-NPC map plus three
remote figures requires sixteen records if all actors remain resident. Hiding
figures every other frame does not release a record, VRAM tiles or palettes.
Avoiding NPC resource leases completely requires separate bounded remote visual
records and verified graphics allocation or virtualization. Retain the reviewed
lease protections until such a replacement passes timing and Save gates.

Current static map audits should remain, with temporal liveness/maximum absence
and complete-figure assertions replacing unconditional every-frame peer visibility
only after that policy is implemented and validated. Effects, partial overlaps,
active scripts and real frontend fast-forward belong in the expanded acceptance.
