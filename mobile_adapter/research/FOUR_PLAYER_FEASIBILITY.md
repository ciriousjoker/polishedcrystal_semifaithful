# Four-player feasibility and generic resource allocation

Date: 2026-10-11. Source checkpoint: `48676b25c` (production assembly unchanged).
This is planning and a linker placement experiment, not four-player runtime
acceptance. The current executable still advertises three total players.

Terminology: "feature-owned WRAM" is ordinary Game Boy Color working RAM reserved
for this ROM feature, not RAM inside the physical or emulated Mobile Adapter.
The declarations are WRAM0 and WRAMX sections in `mobile_adapter/rom/ram.asm`.
They hold packet buffers, peer records and graphics caches in previously unused
native address ranges. The serial adapter is not a memory expansion for these
allocations. Its internal implementation buffers are not directly addressable
WRAM and were not used in this experiment.

## Required design

Support four total players, including the local player, in the same map and
viewport. Each client tracks three peers through generic bounded collections.
One authoritative capacity setting must drive ROM/server limits, peer storage,
message bounds and rendering allocation. Runtime zero/one/two/three-peer cases
must not use separate numbered-player implementations. Higher configured limits
need resource checks and tests; four is the required supported capacity.

The user prefers reclaiming NPC resources where necessary. Investigate a generic
lease of ordinary object/sprite resources, using the native renderer and tile
allocator. Allocate an empty eligible slot first; otherwise suspend only an
explicitly replaceable actor and restore its state afterwards. Remote actors need
validated positions and avatar state, not ordinary NPC AI/scripts. Keep peer ID
and connection generation independent of the temporary object slot. Connected
players elsewhere on the map/world do not each permanently reserve a visible
actor slot. Protect required scripted actors, special sprites and event identity.

This approach can eliminate bespoke per-peer VRAM windows and persistent graphics
caches. It needs a real lifecycle/timing prototype, including safe skipping of
ordinary NPC behavior for a leased remote actor and restoration of displaced NPCs.
All new implementation stays in `mobile_adapter/rom` with surgical native hooks.
Raising `NUM_OBJECT_STRUCTS` or changing original RAM layout is not assumed.

The current renderer uses independent OAM and fixed graphics windows. Temporarily
hiding an NPC's rendering does not release its object/tile resources. Reclaiming
those resources must therefore be implemented explicitly, not assumed from the
fact that the actor disappeared from the screen.

## Capacity evidence

The current approved avatar geometry uses four 8x8 OAM entries arranged as a
16x16 figure. Four such avatars use 16 of the hardware's 40 entries and eight of
the ten entries allowed on a shared scanline. NPCs and effects consume the
remaining space. Equivalent ordinary local NPCs have the same hardware limits;
they must be the performance comparison, with matching geometry and movement.
These figures establish no fundamental four-avatar limit, not universal fit with
arbitrary additional NPC/effect demand.

The existing runtime has 13 object slots including the local player. Leasing
three slots for remote actors leaves nine slots for ordinary NPCs/effects.
Optional replacement can make room, but protected actors and special last-slot
sprites still require a lifecycle audit. Existing map-count evidence is static
and does not prove runtime availability on every map.

The full three-peer roster fits the existing wire limits without larger packets:

| Field | Bytes |
| --- | ---: |
| Receiver ID, generation, peer count | 5 |
| Three existing 17-byte peer records | 51 |
| Payload | 56 |
| Frame including transport header | 64 |

The protocol already limits payloads to 120 bytes. Both implementations must
advertise the new capacity consistently and reject an incompatible peer. Three-
client test cases and the existing fourth-client rejection test must be updated
and extended with fifth-client rejection; they are not a four-player proof.

## Conservative standalone-cache placement experiment

Before the generic NPC-resource clarification, a disposable RGBDS 1.0.4 experiment
checked the more expensive alternative of retaining independent graphics caches.
It assembles all original RAM declarations and LCD/OAM LOAD contracts, with only
feature-owned declarations modified in a temporary file. No production driver,
native game source or manual test session was changed.

The additional peer needs 64 record bytes, 384 bytes for a 24-tile cache, and 32
bytes of additional output/rollback-prefix scratch: 480 bytes in total. This fits
by repacking only the existing feature-owned bank-2 tail:

| Feature region | Start | End, exclusive | Bytes |
| --- | --- | --- | ---: |
| Protocol, peer and rendering buffers | `$d4eb` | `$da1b` | 1,328 |
| Three full avatar caches | `$da1b` | `$de9b` | 1,152 |
| Rendering cache | `$de9b` | `$dec8` | 45 |
| Page-aligned scanline table | `$df00` | `$df91` | 145 |

All 23,373 existing RAM/SRAM/HRAM symbols retain their baseline bank and address.
No new SRAM or native RAM relocation is needed for this reservation. There are
167 unallocated bytes in that bank-2 tail, split between two gaps. Resident
declarations were unchanged. This proves RAM placement for this specific layout,
not future code size, frame timing, graphics correctness or the final allocator.
The generic NPC-resource design is preferred for investigation; these hardcoded
experimental addresses are not the required production architecture.

The current independent VRAM0 windows are `$64..$67` and `$68..$6b`. Simply adding
`$6c..$6f` would overlap native effects, which start at `$6f`. One possible ordinary-
sprite-only alternative is the VRAM1 gap `$3c..$3f`, but special last-slot sprites
can occupy part of it. It is not an accepted general allocation. This concrete
collision explains why changing the current capacity constant alone is unsafe;
generic native resource leasing is intended to remove such manual choices.

Machine-readable results: [four-player-layout.json](four-player-layout.json).
Temporary fixture and linker outputs are in the evidence's artifact directory.
To reproduce the reservation: override capacity/peer count to 4/3 in the existing
RAM contract fixture, add one 64-byte peer and one 384-byte cache, let OAM output
scale by peer count, apply the table above, link against the native RAM layout,
and compare all baseline native RAM symbols. No game code runs in this test.

## Remaining acceptance

- Prototype generic native resource leasing, actor lifecycle and restoration;
  preserve local NPC/script behavior and correct selected avatars/palettes.
- Allocate from the capacity setting; replace fixed peer1/peer2 logic with bounded
  iteration, and audit every zero/one/two/three-peer path and memory bound.
- Exercise four real clients in the same viewport, including 100%/200%/300%/400%,
  dense maps, map transitions, joins/leaves, weather, menus and special sprites.
- Compare against equivalent native NPCs, not a scene with fewer drawn actors.
  Keep transport-only and renderer-only runs as diagnostic isolation controls.
- Check ROM placement, palette/tile ownership, OAM/scanline limits, input priority,
  invalid packets and compatible-state recovery. Four-player feasibility does not
  waive the unresolved performance requirement.

Sources: `home/map_objects.asm:GetSpriteVTile`,
`engine/overworld/overworld.asm:LoadOverworldGFX`, `mobile_adapter/rom/ram.asm`,
`mobile_adapter/rom/constants.asm`, `mobile_adapter/tools/audit_renderer.py`,
the Rust lobby codec, and [Pan Docs OAM](https://eldred.fr/pandocs/OAM.html).
