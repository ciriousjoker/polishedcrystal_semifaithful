# Native actor implementation work

Date: 2026-10-11. Native actor leasing is now the normal four-player mobile build,
using schema 4 and one MOBILE_ADAPTER compiler flag. The separate experiment
flag and legacy independent OAM renderer were removed. Current acceptance is in
MILESTONE_2.md; failed experiments remain recorded in PLAN.md. The intended
experiment and early infrastructure notes below are historical design context.

## Verified relevant native behavior

- Native object storage is bank 1 at d4cc, 13 records of 34 bytes. Feature records
  are in bank 2 starting d4eb. Cross-bank copies require deliberate bank switching;
  a normal CopyBytes cannot copy between these two regions directly.
- GetSpriteVTile assigns graphics by runtime object slot. Slots 0..7 use VRAM0;
  slots 8..12 use VRAM1, with 12 standing tiles and walking tiles 128 tiles later.
  Four normal avatars therefore fit by leasing native slots, without a third
  hardcoded window that collides with effects at 6f.
- Native Facings uses four quads for ordinary avatar facings 0..15. Standing
  direction offsets are 0, 4, 8; walking uses those offsets plus 128. Mirroring
  remains native. GetUsedSprite uploads both standing and walking graphics and
  waits through Request2bpp, so it cannot be called as foreground multiplayer work.
- Native InitSprites renders object records and applies palette, priority,
  weather and OAM limits. HIGH_PRIORITY actors are placed ahead of normal actors
  in hardware order. IsNPCAtCoord skips EMOTE_OBJECT actors. That flag can keep
  remote leases nonblocking without accessing an invalid map-script index.
- TEMP_OBJECT associations are explicitly safe in DeleteMapObject and movement
  restoration. A malformed virtual map-object index must never reach GetMapObject.
- FROZEN alone is not a complete remote-step implementation: native HandleObjectStep
  still checks visibility, may update palettes and runs its frozen action handler.
  Examine this lifecycle or use a surgical feature-owned early handler. Native
  engine behavior must remain unchanged in the disabled build.

## Intended experiment

Replace the independent peer OAM/scanline pipeline with a bounded generic native
object lease pool. Each peer has one record, one optional slot and an audited
original-actor backup. Use free slots first. Reclaim only a new reviewed allowlist;
the old render-suppression table includes trainers and is not an eviction list.
Preserve ordinary object state and map identity; temporarily keep the map object's
runtime association occupied to prevent respawn. Restore only when the current
map/event lifecycle still permits it. Never mutate event flags or SRAM.

Keep all new assembly in mobile_adapter/rom. All loops use the authoritative
capacity and generic peer/resource tables. Generate diagnostic numbered labels
for observers rather than writing numbered-player logic. Generic full-roster
validation must check every record and duplicate identity before publishing any
record; expand packed records backwards using bounded private scratch. Membership
changes and deltas must invalidate leases safely.

Load a leased sprite's complete standing/walking tiles once, then movement/facing
uses native rendering without per-animation VRAM uploads. Populate graphics in
bounded idle-time chunks and publish a lease only after its graphics are ready;
partially overwritten old NPC graphics must never remain visible. Cold decompression
also needs a measured budget. Preserve the native priority/order rules and verify
all three remote actors are actually visible under crowded OAM/scanline pressure.

Repeated prior experiments already failed: moving the existing whole parser into
the HALT wait caused p95 739ms; byte-only idle polling, a serial RAM handler and
VBlank GDMA also failed cadence. Do not repeat them unchanged. Native rendering
should remove most bespoke sprite/scanline work and per-facing HBlank copies before
reassessing transport scheduling. Measure against equal native actors, then also
retain pristine controls. Report any additional world-update gaps honestly.

Emergency exit should sample physical JOYP independently of Joypad's input locks.
The current Joypad returns early for wInputFlags and wGameLogicPaused, so relying
on hJoypadDown does not meet the accepted priority requirement. ROM0 has seven
free bytes. Inspect the actual farcall encoding and VBlank's saved-bank/register
boundary before adding a guarded emergency hook. Do not relocate native helpers.
The current MobileRelease enables IME, so it must not be called unmodified inside
an interrupt. Select+B must clear local intent immediately, irrespective of network
acknowledgement, and recovery of a compatible emulator state without libmobile must
not require working serial hardware.

## Current infrastructure evidence

Native embedded and hosted Bridge paths passed three actual headless ROMs using
ordinary selected-character batteries. Rust group tests, Flutter widget tests,
native Dart FFI listener/port-release test, macOS bundle build and real local
workerd routing/DO tests pass. POLL coalescing was added after the first ROM proofs;
rerun latency and recovery. No phone or cloud deployment is needed for this run.

Known remaining assurance work includes typed-state/property sequences, identifier
wrap/churn, per-room malicious-client isolation and transport resource budgets,
four clients at 100/200/300/400 percent, same-map crowds, save equivalence and
compatible-state restore without adapter, plus the strict cadence gate.

## Historical save-cache isolation blocker, final-source review

The nine-group final suite passed with ROM SHA-256
198794f08a9a07f0a0f07d56713b4eae0517fe154284749cb6f88e1ec339a7b0
(the canonical hash is in m2-final-layout-6.json). A separate ordinary disabled-ROM
comparison then exposed a missing save oracle: bedroom saves retain native palette
cache entries allocated solely for remote avatars. wUsedObjectPals is 1 offline
and 15 online; wLoadedObjPal1..3 are FF offline and 3/1/2 online. These are harmless
on fresh Continue, but violate the strict S4 requirement. The previous same-state
copy observer starts after these earlier writes and cannot detect their provenance.
Do not mark the revised milestone complete or publish the old evidence as final.

The next implementation must preserve the ordinary palette manager's save state.
Investigate feature-private hardware palettes and a bounded native palette source
view of the original leased objects. Keep all implementation in rom/, with only
small guarded palette-scan seams. Original game helpers must not move. Measure
native cadence again after any new hook; do not hide additional frame gaps.
Diagnostic artifacts: host cache m2-native-save-bedroom-cache-review-1 and
m2-native-save-cache-review-1/bedroom-offline/palette-comparison.json.


## Final palette isolation and test-oracle correction

The native palette-cache contamination above was fixed with feature-private
colors and verified original-actor views. The native palette manager retains its
serialized buffers and scratch fields. Reusing complete colors during active
native fades also avoids the ordinary constructor's long fade catch-up loop.
Five connected Save/fault cases and an independent disabled-ROM palette oracle
pass on the final ROM. Physical exit and adapter-free online-state recovery pass.
See MILESTONE_2.md and milestone2-evidence.json for final hashes and artifacts.

Old ordinary-NPC controls were invalid: they replaced different native figures
and Continue restored cached actors from the original battery. Final controls
use the actual reclaimed map-object indexes, correct other-player figures/final
positions, equal remaining native actor inventory and measured movement inputs.
They generate real New Game/Save batteries for each control ROM. Same-second
source timestamps also required forcing the map dependency during restoration.
The repeatable verifier now generates controls for its own measured run and
rejects stale actor/input metadata. Standing native peers are an actor-load
counterfactual; cold joins, NPC random history and palette slot allocation are
not identical to the network run. Old comparison claims must not be reused.
