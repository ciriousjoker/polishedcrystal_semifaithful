# Independent renderer implementation checkpoint

Date: 2026-10-12. Experimental, not accepted M2 completion.

## Active follow-up, 2026-10-12

The latest active plan entry supersedes the older implementation description
below. Three private pose windows no longer overlap the native large-sprite
atlas: bank 0 $64..$67, $68..$6b and $6c..$6e plus bank 1 $3f. Pending pose source,
identity and appearance are private; transfer and OAM flip/bank attributes commit
in the existing early VBlank hook. The retired active-scanout copier is excluded.

A preserved native OAM base and bounded guest scatter avoid repeatedly copying
ordinary NPCs. Canonical shape classification is captured at the existing native
metadata hook. The dense virtual-geometry-credit-corner-20261012 run passes all
four strict 800-frame windows, speeds 1..4, native NPC motion, actual palettes/
poses, hardware row selection and the unchanged three-frame absence bound.
The early-transfer bedroom run passes four clients and strict cadence.

Actual emitted irregular row accounting exposed a reverse-template clipping
bug, fixed by rechecking the selected minimum Y before the rectangle fast path.
2,144 host instruction cases now validate clipping, private bounds and exact
rollback. Its measured 21,752-cycle count/rollback bound requires generic work
before LY 110. 13,920 interval cases and 65,536 native OAM bounds cases also pass.

These runs do not yet establish final-source acceptance. Local motion and hosted
motion are being measured after transfer cleanup and source validation. The
remaining graphics/resource, recovery/save and matched-cadence gates listed in
the plan remain mandatory. Default launcher migration is deliberately pending.

## Historical user contract and rollout

The replacement must work without map redesign, replaceable-NPC labels, native
NPC eviction, story/script changes or NPC state restoration. G3 supersedes the
old lease policy. The virtual build excludes the lease allocator, lease masks,
lease backup/restore routines and runtime LZ decoder. Its map/recovery hook only
clears private presentation ownership. The live save-projection hook is `ret`.
Ordinary NPC objects remain simulated and interactable even when a displayed
figure is omitted for hardware capacity.

`make mobile MOBILE_RENDERER=virtual` builds the candidate separately. The normal
Python development launcher stays on `make mobile`, the legacy comparison path,
until the replacement passes. That default still leases NPC resources and does
not satisfy G3. This temporary comparison flag must disappear after migration;
it does not alter the final single multiplayer on/off compilation contract.

All new assembly is under `mobile_adapter/rom`. Conditional native hooks retain
immutable draw metadata and group boundaries, without changing NPC data. Both
Standard and Faithful prototypes compile. The Standard layout check preserves
all 23,373 original RAM symbols, and neither build allocates multiplayer SRAM. The Standard prototype uses
16,400 upper-half bytes and 156 net lower integration bytes, leaving 13,805 bytes
free below bank 128. Native comparison integration is 199 bytes. MBC30 capacity
does not change Game Boy RAM or GPU limits.

## Resource handling

- Objects: three fixed 64-byte private peer records in WRAM bank 2. Native object
  indices and trainer IDs are not remote identities.
- OAM: whole figures are admitted to 40 hardware entries and at most ten objects
  per visible row. Ordinary 16x16 figures use four entries and two per row. The
  local player is admitted first; remote/native presentation order rotates.
  A viewport whose emitted native OAM leaves insufficient space for all three
  remote figures admits one rotating remote guest. This is presentation sharing,
  not removal of an NPC. Special shapes/weather use bounded exact row accounting.
- VRAM: only the current four-tile pose is uploaded. Two windows use bank 0 tiles
  $64..$67 and $68..$6b; one uses bank 1 $7c..$7f. Native standing/walking atlases
  and common emote/effect tiles are preserved. A loaded large last-slot sprite
  that overlaps the bank-1 window disables it and shares the second window.
  This audits sprite format rather than a map permission list.
- Assets: a build tool derives 24 gender/movement entries from native state and
  sprite tables. Seventeen deduplicated native raw images live in the upper ROM.
  Only 64 bytes are copied per pose; cold runtime decompression is unnecessary.
- Colors: cache construction uses the isolated native palette constructor and
  restores its temporary native buffers/scratch. Exact ordinary hardware colors
  can be reused. Otherwise private hardware color ownership is coordinated with
  complete-figure admission. A merged 64-byte palette is prepared in private WRAM
  before OAM publication and uploaded with a fixed native-style VBlank routine.

Alternation alone does not solve VRAM or color capacity. A figure must own its
uploaded pose and matching hardware color for the actual displayed frame. At
30/20 Hz, alternating figures visibly flicker; this is not real alpha blending.
Eight OBJ palettes contain four colors each, with color zero transparent. Source
references: [OAM](https://github.com/gbdev/pandocs/blob/master/src/OAM.md),
[palettes](https://github.com/gbdev/pandocs/blob/master/src/Palettes.md), and
[tile data](https://github.com/gbdev/pandocs/blob/master/src/Tile_Data.md).

## Idle scheduling and the dense-map blocker

Every background entry checks the awaited native VBlank and an LY deadline.
Serial interrupts are masked during native game work. Graphics, projection and
composition never call a frame wait, and the replacement wait never enters HALT
if its awaited VBlank already occurred. Frame credit is replenished before work.
Capture runs on even frames, projection on odd frames and one graphics job every
four frames. The ordinary game keeps priority; stale remote presentation may be
retained or omitted when there is no safe graphics budget.

The prototype still reconstructs too much native presentation each frame. On the
Game Corner fixture native waiting often starts near LY 111, leaving only a short
part of visible scanout before VBlank. Measured composition steps reach about
5,700 CPU cycles, native group admission about 2,800, and remote group admission
about 5,400. The sum cannot be inferred from individual maxima, but traces show
composition repeatedly reaching the deadline before publication. Reducing remote
admission to one guest under total-OAM pressure does not resolve this. Native
records continue to move while no valid completed remote list is published.

The final dense pre-cleanup run additionally has a 799/800 world-update window,
one duplicate and a gap of two frames in client 3. This is an acceptance failure
even though observed idle/upload calls do not cross VBlank. Do not relax the
cadence gate or interpret that narrower check as complete frame safety.

A boundary-only foreground capture experiment reaches more composition steps but
still misses dense acceptance. It was reverted because it added 40 lower-half
bytes without resolving the blocker. The next change should preserve/reuse
existing native OAM and avoid rebuilding every NPC group. Any staged plan must
still use current NPC/camera coordinates and validate actual scanline demand;
lagging native NPC positions or stale ownership is not an acceptable shortcut.

## Current finite evidence

- Native comparison: `native-bounds-corner-20261012` passes four actual clients
  at speeds 1,2,3,4, Game Corner movement, native sprite/color checks and all
  strict 800/800 world-update windows after the bounds fix.
- Candidate before dead-code removal: `virtual-final-bedroom-20261012` passes
  four actual clients at speeds 1,2,3,4. Hardware selection never exceeds eight
  objects per line; all three peers appear and maximum absence is two frames.
  Actual hardware palette and pose bytes match native avatar fixtures.
- Clean candidate: `virtual-clean-bedroom-20261012` repeats that finite pass
  after removing NPC eviction/allowlist dependencies. Exact hashes and results
  are recorded in `virtual-renderer-evidence.json`.
- Best motion experiment: `virtual-direct-pose-latency-20261012/local-latency`
  measures 60 samples without injected RTT: p50 184.313 ms, p95 203.832 ms,
  maximum 215.933 ms. Input-to-remote p95 323.612 ms includes native turning.
  The unchanged 200 ms local-motion gate fails. This is an intermediate-source
  result, not final-source latency acceptance or an improvement to the default
  human launcher.
- More frequent graphics work regresses to p95 314.047 ms; earlier projection
  alone regresses to 246.525 ms. Both scheduling changes were reverted. Diagonal
  interpolation remains allowed by the user.
- `virtual-checkpoint-corner-20261012` and `virtual-oam-sharing-corner-20261012`
  fail dense cadence/publication. Joined clients and correct cached assets are
  not presentation acceptance. `virtual-boundary-corner-20261012` is the rejected
  foreground-capture experiment.
- Actual emitted OAM bounds instructions pass all 65,536 input combinations in
  each renderer's host-only assembly test. Eleven host resource/selection tests
  pass, including independent PPU selection, bounded absence and sorted-interval
  overlap checks. Their arithmetic proof does not replace executing the complete
  assembly renderer on real emulator frames.
- Disabled Standard ROM SHA-256 is exactly
  `314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e`.

Artifacts are under `/Volumes/Storage/Cache/mobileadapter/`. Tests use isolated
ports 17800/17830 and ordinary test batteries. Test ROM copies change only the
embedded port and required global checksum; all executable code is unchanged. User listeners on 17423/8787, GUI
sessions and personal saves are untouched.

## Remaining acceptance and known risks

1. Complete publication on dense maps with equal native-NPC frame cadence,
   continuous local player priority and bounded absence for every remote and NPC
   figure. Actual protected Route 37/scanline-pressure fixtures remain required.
2. Dynamic tile-window sharing with a large native sprite. Shared-window uploads
   currently run too infrequently to prove the three-frame absence bound. Check
   all native effects, weather, special player forms and map/menu transitions.
3. VRAM scanout ownership. Current in-place pose uploads have an early whole-job
   deadline preflight, but this does not prove no active sprite reads partially
   replaced tiles. Coordinate committed hardware OAM, current pose ownership and
   uploads before considering the candidate stable.
4. Palette fairness and transitions. Reserve available native color demand before
   taking occupied slots; ensure omissions occur only under real resource pressure.
   Correct finite ordinary-avatar colors do not prove night/day, fades, glowing
   actors, special forms or all eight-palette conflicts. The observer checks frame
   boundaries; mid-frame palette/VRAM state remains a separate proof obligation.
5. Final-source local/hosted latency, source speeds 100..400%, malformed packet
   sequences, priority cancel, reconnect, map departures/turns, menus, connected
   saves, offline Continue and compatible snapshots without an adapter. Retained
   native-suite success cannot be inherited by the new renderer.
6. Generic all-map resource contracts and future additions. Existing native facing
   geometry contracts flag unsupported sprite-format changes; historical protected
   NPC pair inventories are stress inputs, not demands on map designers.

M2 remains reopened. Do not start M3 ROM features, promote the candidate into the
human launcher, or claim universal graphics/performance/save acceptance.
