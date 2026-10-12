# Milestone 2: independent four-player presence and household Bridge

The final independent-renderer acceptance run is in progress on 2026-10-12.
Do not treat successful intermediate cases as milestone completion. This report
supersedes the retired native-object leasing implementation and its acceptance
numbers. Historical experiments remain in PLAN.md and Git history.

## Architecture

`make mobile` selects a 4 MiB MBC30 ROM. All banked feature code and avatar assets
live in the upper 2 MiB. The lower half contains only surgical integration, with
184 net additional bytes. Removing `MOBILE_ADAPTER` produces the exact original
2 MiB ROM. No existing helper was relocated. The former renderer switch,
NPC leasing/eviction, NPC state restoration and save-copy hooks are removed.

Three compact private records hold remote players. They consume zero ordinary
object slots on every map. NPC object records, movement, interactions, collision,
script references and story flags remain native. Four players can coexist on the
same map without NPC permissions or map redesign.

The renderer admits complete figures within the 40-entry OAM buffer and ten-entry
scanline limit. The local player takes priority. Under actual hardware pressure,
remote and ordinary figures share presentation over successive frames; NPCs remain
simulated and interactable. The common canonical rectangle path preserves native
OAM groups in place. Irregular and clipped figures use exact bounded row admission
and rollback. Hardware observations validate selected quads independently of the
emulator's own sprite-limit setting.

Each remote has a fixed four-tile GPU pose window. Two use VRAM bank 0 tiles
$64..$67 and $68..$6b; the third uses $6c..$6e and bank 1 $3f. Native atlases and
effect tiles remain intact. Pose preparation reads build-validated native assets
and queues an immutable private job. The existing early VBlank seam commits the
whole pose, before ordinary graphics work. A movement-mode change retains the
last complete pose until the new graphics commit. Loaded movement metadata
prevents an equal frame index from reusing tiles from the wrong movement mode.

Exact current native colors are reused first. Native avatar sources are reserved
before a private guest chooses a free color, avoiding an unnecessary conflict
with another guest that already has an exact ordinary source. Otherwise private colors borrow a
currently unused hardware palette. A generic full admission path handles palette
pressure. Native palette buffers/cache and serialized construction scratch remain
unchanged. Weather/effect palettes retain their native roles. Weather quads use a direct
clipped-row check and single copy. Admission stops at LY126 to reserve the existing
complete publication deadline. Particles that exceed OAM/scanline capacity or the
remaining presentation time are omitted for that frame; ordinary simulation and
NPC state are unchanged. The four-speed snow replay passes the unchanged-native timing
control and the three-frame hardware visibility gate. Emitted weather instructions
pass 18,152 clipping, pressure, ownership and output-bound cases (maximum 1,268 cycles). Fades defer new
private construction while retaining complete existing appearance.

## Scheduling and observations

Heavy work runs in the native idle wait. Capture, graphics and projection receive
at most one optional job per frame, leaving time for coherent scene publication
and bounded transport work. Native game frames take priority. Deadlines cover the
combined instructions, serial work, bank changes, graphics and OAM publication;
background work never calls an additional frame wait.

Presence is latest-state data, not synchronized emulator lockstep. Fast senders
can skip intermediate positions; their frame counters do not control the server's
wall-clock leases. Tests use actual clients at 100/200/300/400 percent. Visual
perfection in the accelerated sender's own frontend is outside the agreed scope.

The hardware trace records every connected video frame. A change-only trace could
omit several repeated visible frames and falsely report a long absence on the
next hidden frame. Sparse connected traces now fail temporal acceptance. Readiness
requires the committed avatar to match the current accepted avatar; a stale
Stage=7 alone is insufficient during the initial appearance update.

The three-frame absence bound applies to a ready eligible figure while an admitted
native idle window exists. Native sprite reloads and waits starting at/after LY136
are counted explicitly and reset that bound. They are not reported as drawn frames.
Frozen Start-menu occlusion is checked separately. The unchanged native cadence
oracle remains mandatory. A running replay with the exact disabled ROM checks the
native graphics-load pauses independently; this does not authorize additional
multiplayer-induced world-update gaps.

## Bridge and hosted model

Flutter calls a native Rust engine. One Bridge owns one household session with
several independent emulator streams. Local mode embeds the shared lobby owner
and needs no upstream connection. Internet mode multiplexes that household over
one WebSocket. Two Bridges can contribute two players each to the same four-player
room; disconnecting one emulator does not disconnect its siblings.

The native hosted simulator routes the initial WebSocket upgrade to a room owner.
The Cloudflare Worker adapter routes it to that lobby's Durable Object. Both use
the same socket-independent Rust rules as local Bridge mode. Placement follows
Cloudflare's normal object behavior. Lobby membership, generations, leases and
latest presence are disposable coordination state. Ordinary game progression is
owned by the clients.

The hosted simulator provides bounded test-only delay queues. 50 ms per direction
adds 100 ms client/server RTT. Local ROM polls remain local. Embedded mode flushes
changed state immediately; hosted presence is coalesced at approximately 30 Hz. Receipts and
connection freshness are never fabricated. Local rendering is not intentionally
delayed to simulate a bad lobby. Cloud-bound presence and authority use one shared
33ms minimum interval, replacing the previous 50ms interval. Flush-boundary tests
still enforce latest-state replacement, rate bounds and eventual delivery.

## Saves, recovery and identity

There is no multiplayer SRAM, save metadata, transaction journal or save hook.
The ordinary Save and backup routines run unchanged. Private data lies outside
the native serialized player span. A read-only instruction observer captures the
source of the native RST CopyBytes before copying and compares every byte afterward,
including native bank/stack preservation. It does not derive its expectation from
the feature's post-copy output. Independent disabled-ROM saves additionally compare
native palette-cache ranges. Checksums and fresh Continue are checked in both
corresponding ROMs, without RAM injection or hand-edited battery bytes.

Physical Select+B is sampled at the common VBlank seam before ordinary input locks.
Local cancellation does not wait for an adapter or server acknowledgement. Tests
cover a connected Start menu, save activity, faults and a real compatible online
emulator state restored with the adapter disabled. Trainer IDs are cosmetic game
data, not network identity; four clients with the same trainer ID remain distinct.

## Repeatability and current evidence

Host-only tests do not start a Game Boy emulator:

```sh
make test-mobile-host RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/
cargo clippy --locked --manifest-path mobile_adapter/server/Cargo.toml --workspace --all-targets -- -D warnings
```

Actual ROM acceptance is separate:

```sh
make test-mobile-rom RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/
```

The default cases include cadence, equal ordinary-avatar load, local and delayed
hosted motion, priority escape, five fault-save cases, running, menu, warp,
turn-after-peer-exit, rejoin, automatic interrupted-socket resume, save-cache
isolation, duplicate trainer IDs, protected
Route37 trainers, large Alolan Exeggutor graphics, night, snow and a synthetic
full native pool with all six native object palettes occupied. Each run uses
fresh artifacts and isolated ports. The pinned release core is unchanged;
profiling cores add read-only instruction/hardware observations. Night/weather
fixtures additionally persist the external cartridge clock offset produced by
ordinary RTC writes. The explicit frontend sidecar restore runs before the first
emulated frame and never restores CPU, game RAM, SRAM or instruction state. The
pinned core's ordinary RTC getter exposes the wrong instance/value for this mode;
the isolated test frontend uses a separate cartridge-clock persistence hook.
The source save and clock sidecar are checksum-pinned in both mobile and native
replays. Native night starts at 21:00 in this game; the fixture chooses 22:00. Current artifact root:
`/Volumes/Storage/Cache/mobileadapter/independent-final-all-20261012`.

The all-map contract checks the current compiled map inventory dynamically,
including future additions: native objects, script targets, tilesets, environment,
location/music/palette, dimensions and movement data must match the disabled ROM.
It requires zero remote native slots. New graphics formats must pass emitted
facing/VRAM contracts and actual-assembly bounds execution. There is no map-specific
allowlist. These are static data and resource contracts, not exhaustive playthroughs
of all reachable story states.

Matched controls preserve the full native NPC inventory and replay ordinary
movement with the same batteries in the hash-verified disabled ROM. The unsaturated
bedroom additionally fits three ordinary avatar NPCs for equal figure-load control.
Controls create ordinary New Game/Save fixtures and restore their temporary map
source and the exact baseline. Builds must not run concurrently with these fixture
mutations. Cold join, native NPC random history and palette-slot assignment can
differ; the controls do not claim identical instruction histories.

## Scope boundaries

Finite fault, property, geometry and cycle tests are evidence, not a mathematical
proof of universal defect freedom. Additional scripts/cutscenes and later gameplay
features must keep the same native performance, save and recovery gates. Overworld
presence does not establish battle, call, gift/trade or quick-travel behavior.

Phone testing and public Cloudflare deployment are excluded by the user for this
run. Other platform packaging/background lifecycle, especially iOS, remain future
work. The macOS Flutter bundle contains a Rust engine and executable built for the
same native host architecture. Protocol/schema/capacity/mechanics checks exist;
semantic compatibility hashes and secure invitations are later work.

For the human workflow:

```sh
python3 mobile_adapter/tools/dev_session.py
```

The launcher selects an ordinary save scenario and instance count, detects Android,
uses stable deployment names, the pinned MBC30 core and 5% Mac volume. It starts
the Bridge and hosted simulator, displays connection logs and asks before stopping
a conflicting listener. Ctrl+C stops the terminal server and frees its port;
Bridge/emulator windows remain user-controlled. `--mac-only` works without a phone.
