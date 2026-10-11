# Milestone 2: native four-player presence and household Bridge

The recorded finite acceptance run completed 2026-10-11 on branch `mobileadapter`
for the approved autonomous macOS/local-host scope. A subsequent audit reopened
scanline-crowding and comprehensive future-map assurance; see
[ROM_AND_RENDER_LIMITS.md](ROM_AND_RENDER_LIMITS.md). The new no-content-choice
ROM requirement also has an unresolved capacity strategy. Do not read the earlier
completion as proof of arbitrary player geometry or future map compatibility.
The newer native selector and strict all-map gate now expose Route 37's missing
remote figure beside protected trainers. Whole-character admission and protected
crowding still require work; this is a failing acceptance case. See
[SCANLINE_AND_MBC30_FOLLOWUP.md](SCANLINE_AND_MBC30_FOLLOWUP.md) for new source
coverage, diagnostics and the multiplayer-only MBC30 investigation.
Phone and public deployment tests remain deferred.
This report supersedes the original three-player renderer and the selected-figure follow-up. Historical
experiments and failures remain in PLAN.md and Git history.

## Implemented behavior

The normal `make mobile` build supports four players, including three remote
avatars on the same supported map instance. Remote players use their saved Chris,
Kris, Crys or Beta choice and the ordinary native walking, running, bicycle and
surf assets. A generic bounded peer pool leases native object/graphics resources.
Free slots are preferred; only reviewed replaceable NPCs can be displaced.
Their original object, camera and raw graphics state is retained and restored.
Story flags, protected actors, services and puzzles remain local.

The Flutter Bridge uses a native Rust engine. Several emulator connections belong
to one household session. Local mode embeds the shared lobby owner and needs no
upstream service. Internet mode uses one multiplexed WebSocket for that household;
two Bridges can contribute two players each to one four-player lobby. Disconnecting
one stream does not remove its siblings. The native hosted simulator routes the
upgrade to a room owner; the WASM Worker routes it to that lobby's Durable Object.
All three hosts use the same socket-independent Rust rules.

The remote transport coalesces presence and changed rosters at 20Hz. Local polls
stay local and grant bounded delivery credit; they do not fabricate authority
receipts or freshness. Embedded mode flushes dirty state immediately through the
same owner. The hosted simulator supports bounded test-only one-way delay queues.
50ms in each direction adds approximately 100ms client/server RTT.

Heavy ROM work runs in native idle waits, with explicit admission deadlines and
bounded parser, checksum, map-hash, decompression and upload phases. Current native
actors retain their last complete appearance while updates wait. Urgent positions
alternate with ordinary pool jobs, so a fast sender cannot starve other peers.
No native helper was relocated.

## Save and emergency recovery

Physical Select+B is sampled from JOYP at the common VBlank entry path, before
engine input locks. An active session exits locally without an adapter/server
acknowledgement. The foreground rechecks cancellation before publishing work.
Offline Select+B requests a join; Select+B+A consumes one echo diagnostic request.
The same escape works through a connected Start menu, normal Save menu and a
compatible online emulator state restored with the adapter disabled.

The native wPlayerData save span includes object records. Allocating no mobile
SRAM alone was insufficient: the previous implementation could serialize remote
TEMP_OBJECT data. The corrected feature captures exact originals for both empty
and reclaimed slots. Two guarded farcalls after the existing primary/backup
CopyBytes project still-owned original records into those ordinary save copies.
Live game RAM is not rewritten. Native checksum, backup and disk handling remain
in control. Five serialized native palette scratch fields are preserved across feature calls.
Remote colors no longer allocate entries in the ordinary palette manager. Fast
feature-owned macros give its scans the unbanked original actor view. Matching
ordinary colors are reused; otherwise a still-unused hardware palette is borrowed
with feature-private color storage. Native palette buffers/cache bytes remain
unchanged. The guarded object-palette uploader selects verified private colors
only for a live owned remote actor. Slots 6/7 retain their native effect roles.
Native palette endpoint/fade scratch is preserved too. Existing complete colors
are retained during fades; new private colors wait until the native fade ends,
avoiding the constructor's potentially long catch-up loop. This is a deliberate
presentation boundary, not permission to delay a native game frame.
There is no multiplayer save journal, new save metadata or new SRAM allocation.

A separate read-only instruction observer starts with the complete same-state
native save payload, projects the verified original records and compares every
byte after the feature call. It also verifies registers, stack return and banks.
This avoids comparing independently booted games whose RTC, NPC movement and daily
random data differ. Fresh Continue and ordinary movement are checked with both
the mobile ROM without an adapter and its hash-verified disabled counterpart.
An independent disabled-ROM Save additionally compares native palette-cache
ranges in both primary and backup saves, in the bedroom with four different colors
and in the crowded fault scenarios. It catches remote-induced writes made before
the copy observer starts. No RAM writes, patched battery bytes or prebuilt emulator
states seed these tests. The compatible-state recovery test intentionally captures
a real online RAM state.

## Footprint and compatibility

One MOBILE_ADAPTER compilation flag controls every feature hook and allocation.
All feature assembly lives in mobile_adapter/rom. Integration consists of small
conditional calls or a feature-owned macro at existing native seams, including
the two save-copy calls. Existing game helpers and mapper are unchanged.
All 23,373 checked native RAM/SRAM/HRAM symbols retain their baseline addresses.
Removing unused legacy renderer buffers recovered 456 WRAMX bytes. ROM0 has 39
bytes free; total ROM headroom is approximately 2.8KiB. This is a material risk
for later interaction/battle features and must be budgeted before adding them.
Exact final hashes and allocation sizes are recorded in milestone2-evidence.json.
A static audit checks known near calls/jumps between banked feature symbols: a
moved emergency helper once crashed because a JP did not switch banks. The corrected
path uses farjp, and live physical-disconnect regressions are mandatory.

The current wire boundary checks transport 1, lobby schema 4, capacity 4 and
standard/faithful mechanics. It does not establish compatibility between arbitrary
Polished Crystal revisions. Generated semantic compatibility metadata and secure
invite creation remain later design work. Prototype room IDs are nonzero 16-bit
values, not an authentication/password guarantee.

## Acceptance and repeatability

Host-only checks require no Game Boy emulator:

```sh
make test-mobile-host RGBDS=/path/to/rgbds-1.0.4/
cargo clippy --locked --manifest-path mobile_adapter/server/Cargo.toml --workspace --all-targets -- -D warnings
```

These cover 35 Rust test functions with bounded framing, deterministic arbitrary
bytes/state sequences, token/generation checks, capacity, churn, aggressive-client
budgets, upstream loss and room/stream isolation. Additional tests cover the human
launcher, ordinary-save fixtures, reviewed lease policy and production assembly
vectors/layout. Formatting and renderer/capacity audits are separate checks.

The native emulator acceptance command on the configured development Mac is:

```sh
make test-mobile-rom RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/
```

It requires the pinned native core, profiling core and runner in the configured
cache. The suite builds four ordinary-NPC controls from the measured cadence run
and validates actor identity/count, peer coordinates and replayed movement inputs.
MOBILE_TEST_CACHE, optional MOBILE_TEST_CONTROL,
MOBILE_TEST_OUTPUT and MOBILE_TEST_CASES expose those dependencies. Each run uses
fresh artifacts and isolated ports, never the manual listener on 17423. Failures
retain diagnostics. The suite runs real instructions with ordinary battery saves,
controller input and matching-symbol read-only observations.

The crowded cadence case uses four actual cores at 100/200/300/400 percent through
two household Bridges and the delayed hosted simulator. The strict 800-frame cold
window is compared with corresponding ordinary native NPC controls. The controls
replace the map objects actually leased in that run, then create their own normal
New Game/Save batteries: Continue would otherwise restore the original cached
actors and silently bypass the compiled control scene. Fixture builds force the
map dependency and restore the exact disabled hash even with same-second file
timestamps. Builds must not run concurrently while source fixtures are active.
Standing peers model final geometry; networking chords and cold-join animations
have no ordinary-NPC equivalent. NPC random history and hardware palette-slot
assignment can differ. These are actor-load controls, not identical whole-game
executions. Historical controls with mismatched figures or actor inventory are
excluded from the final evidence. All four connected profiles have 800/800 update-bearing frames, zero duplicates
and maximum gap one; the equal ten-actor controls have 783/784 update-bearing
frames, with 16/17 native duplicates and maximum gap two. Multiplayer adds no
world-update gap and never crosses the awaited VBlank in this measured window.
NPC wandering
has a longer stationary observation because random movement need not occur in a
short final interval. That extension does not change the cadence or cycle gates.

Other final-source cases cover local and hosted visible movement, connected-menu
escape, adapter-free compatible-state recovery, uninterrupted online Save plus
peer disconnect/server close/malformed response/physical Save-menu exit, running,
Start-menu static sprites, map transitions and rejoin. Separate native cases cover
special shared graphics, the full Snowtop camera lifecycle and exact raw Miltank
icon restoration. Echo transport regressions cover 14 native processes in 12 cases.
Each latency case records 60 movements and checks all three receivers. The
current final-source p95 is 188.997ms locally and 298.842ms with 100ms added RTT;
full input-to-visible p95 is 306.721ms and 417.990ms respectively. The hosted
result is close to the 300ms gate and provides little margin for host scheduling
or a slower WAN. These are finite loopback results, not a production latency
guarantee.

Latency is measured from first local world movement to first directional movement
of a complete remote four-quad avatar at the native video callback. The p95 gates
remain below 200ms locally and 300ms with an added 100ms RTT. Full input latency is
also retained; the game's unchanged turn delay precedes world movement. Neither
these loopback measurements nor the static resource audit promise every WAN path.

Canonical final evidence is in milestone2-evidence.json. The accepted main suite
is host cache m2-accepted-final-guard; its old cadence controls are superseded by
m2-accepted-final-matched-cadence, which generates and validates its own controls.
The five Save faults are repeated under m2-accepted-final-immutable-saves with
immutable hash-verified disabled inputs. Special scenes, faithful and transport
are in m2-accepted-final-guard-special and m2-accepted-final-guard-other. The
isolated human startup preparation is m2-human-final-guard-preparation; it builds
the same final ROM and regenerates a real Continue snapshot without touching the
manual listener or user profiles.

The actual local workerd tests cover Worker routing, WebSocket upgrade, two-household
multiplexing, capacity, malformed traffic, isolation, budgets and four forced
hibernations with identity/position recovery. Flutter widget and actual native FFI
listener/port-release tests pass; the native ARM64 macOS bundle is built and signed.

## Explicit boundaries and follow-ups

The generated protected-default lease policy covers all 611 assembled maps. The
raw conservative slot bound resolves 607; reviewed fingerprinted story/terrain/
camera contracts resolve the remaining four ordinary-exploration bounds. This is
a slot bound, not a full playthrough or an all-story-state OAM/timing proof.
Script-created actors and changed dynamic assets require further lifecycle fixtures.
Protected panning/cutscene scenes may temporarily defer remote drawing; menus keep
complete static native avatars while other clients continue independently.

Current finite tests do not prove the absence of every possible ROM, Rust or host
defect. Every new feature must keep the hash, footprint, save, recovery and cadence
gates. The binding requirements remain active; coverage boundaries are not waivers.

The phone is unavailable for this run. Public Cloudflare deployment/account tests,
Android/iOS/Linux/Windows packaging and background service survival are deferred.
Native Rust support alone does not establish iOS background feasibility. Calls,
interaction menus, battles, gifts, trades and quick travel remain later milestones.

For the existing human Mac/Android workflow, run from this worktree:

```sh
python3 mobile_adapter/tools/dev_session.py
```

It keeps scenario selection, stable Android paths/filenames, 5% Mac volume, the
correct restart intent, connection logs, Ctrl+C cleanup and occupied-port approval.
`--mac-only` supports local development without a phone; `--mode hosted
--test-one-way-delay-ms 50` selects the local hosted model. Phone development may
use the Mac's LAN endpoint until a device-local Android Bridge is packaged.
Current snapshots are regenerated from ordinary saves for each exact ROM/core.
