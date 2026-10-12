# Independent remote actor presentation

Date: 2026-10-12. Architecture analysis with an experimental implementation.
Read [VIRTUAL_RENDERER_PROGRESS.md](VIRTUAL_RENDERER_PROGRESS.md) for the current
checkpoint. The normal launcher retains the native comparison path; its separate
OAM bounds safety fix changes the enabled hash. The disabled hash is unchanged.

## Existing foundation

`rom/ram.asm` already allocates three generic 64-byte peer records in private
multiplayer WRAM. Each record includes network identity/generation, map/instance
and epoch, coordinates, facing, avatar/movement, projected position and rendering
state. There are also three private 384-byte avatar caches in WRAM bank 2. This is
Game Boy WRAM reserved for the feature, not memory provided by the Mobile Adapter.

The native comparison renderer couples those independent records to ordinary object slots:
`MobileNativeUpload` maps a leased native slot to sprite tile addresses, and
`MobileRenderSprites` leaves emission to the native renderer. That historical allocator may
borrow reviewed NPC resources and preserve their native records and graphics.
G3 supersedes this policy. The experimental renderer removes that coupling and
does not compile eviction code or map-specific lease masks.
Unused lease backups can potentially be reclaimed after the replacement passes,
but no extra free RAM or final graphics budget is established by this analysis.

## What the proposal resolves

Remote actors consume zero ordinary object records. A full twelve-NPC map can
retain those NPC records and the local player while three independent remote
actors exist. NPC movement, scripts, collision and story state need no removal
or restoration solely to provide a remote object slot. Private actor state also
reduces the ways multiplayer can contaminate native saved structures, but does
not itself prove save safety.

Adjacent interactions and optional collision need explicit bounded queries over
the three private peer records. Resolve a selected peer by validated session
identity/generation and map/instance, never by a native NPC slot or trainer ID.
Rendering omissions must not disable interaction or native NPC simulation.

## What it does not resolve by itself

The Game Boy still permits forty OAM entries and ten hardware objects per
scanline. A standard sixteen-pixel-wide figure consumes two hardware objects on
its occupied scanlines, so four players and two overlapping NPCs still exceed
that line budget. Private WRAM records do not expand it. Complete-figure temporal
admission can rotate competing presentations when necessary, retaining normal
local-player priority and the user's explicit exceptional fallback. Omitted
figures must not leave on-line OAM entries that still count against the limit.

VRAM tile addresses and eight OBJ palettes also remain shared. WRAM avatar caches
are not displayable directly by the PPU. Replace slot-based tile addressing with
an audited private/virtual graphics allocation policy that preserves backgrounds,
NPC graphics, effects and native palette transitions. Emitting figures on fewer
frames only saves graphics storage if a verified tile/palette reuse strategy
actually accompanies it. MBC30 ROM expansion does not enlarge these resources.

Sources: [Pan Docs OAM source](https://github.com/gbdev/pandocs/blob/master/src/OAM.md)
and [Pan Docs tile-data source](https://github.com/gbdev/pandocs/blob/master/src/Tile_Data.md).

## Performance design and acceptance

Prepare decompression, graphics uploads, projection and admission decisions in
bounded idle work. Publish validated presentation state safely; do not expose
half-written records or stale figures across map changes. Per-frame composition
of the final OAM buffer still needs a bounded measured budget and coordination
with the native renderer/DMA. Independent actor storage does not make that work
free. Preserve priority Select+B even during incomplete jobs or invalid packets.

An earlier independent OAM/scanline pipeline missed strict frame cadence before
the current native path was selected. That history rules out treating a return
to private rendering as already proven; it does not establish that an optimized
private renderer is impossible. Keep the working path for comparison until the
prototype passes, without adding large native-engine changes.

Remaining replacement acceptance gates:

1. A full twelve-NPC Game Corner with three independent remote actors. Confirm
   all ordinary NPC records, scripts and movement remain active.
2. The protected Route 37 pair and four overlapping players: complete figures,
   bounded absence/fairness and no partial bodies under scanline/OAM pressure.
3. Different avatars, walking/special forms, camera movement, menus, fades and
   effects: no VRAM/palette corruption or stale map-departure resurrection.
4. Equivalent native-NPC cadence comparisons and local/hosted latency profiling.
   Normally running receivers must handle sources at 100%, 200%, 300% and 400%.
5. Existing packet-fault, cancel/reconnect, ordinary-save isolation, offline
   Continue, adapter-free compatible snapshot and disabled-ROM hash gates.
6. Updated map/resource audits for all current maps and future CI additions.
   Finite scenario tests are evidence, not a universal proof of every script path.

All new assembly remains under `mobile_adapter/rom`; integration stays surgical
and conditional. Banked feature code stays in the upper MBC30 half. There is no
change to `.sav` layout and no trainer-ID uniqueness assumption.

## User clarifications

Diagonal remote smoothing is acceptable. Improve measured latency first and
revisit smoothing only if needed; do not turn elimination of diagonal visual
shortcuts into a new mandatory feature.

The 400% requirement protects other normally running players and protocol/game
safety. The user choosing fast-forward accepts degraded local appearance. Perfect
visual fairness under arbitrary frame dropping on that fast client is not a
requirement. Fairness on normal receivers, regular frame cadence, bounded work
and save/recovery guarantees remain required.
