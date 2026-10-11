# ROM capacity and scanline crowding audit

Audited 2026-10-11 against implementation checkpoint `4aff2149d`. This report
qualifies the earlier milestone-2 completion claim: its finite local acceptance
results remain valid, but scanline crowding and comprehensive future-map gates
are unresolved. No rendering policy or cartridge mapper was changed in this audit.

## Subsequent implementation

The user's subsequent 10 KB limit, exact whole-ROM gate and executed size
experiments are recorded in [ROM_BUDGET.md](ROM_BUDGET.md). The production addon
still costs 11,112 bytes and exceeds the conservatively selected 10,000-byte
budget by 1,112. Its ROM-only NPC metadata opportunity is research, not an
implemented or save/timing-validated optimization.

The follow-up in [SCANLINE_AND_MBC30_FOLLOWUP.md](SCANLINE_AND_MBC30_FOLLOWUP.md)
adds current native selection checks, all-map/geometry guards and failing
post-link map gates. It records the Route 37 visibility failure and the actual
MBC30 layout/core probes. The missing-test and partial-source-coverage statements
below describe the earlier audit state and are superseded by that follow-up.
Production ROM code and hashes remain unchanged; unresolved rendering policies
and MBC30 adoption remain open.

## New binding direction

The user requires physical-cartridge playability and no forced choice between
future upstream content and multiplayer. Do not fund multiplayer by removing
upstream content or silently dropping supported revisions. Treat this as a
binding direction with an unresolved capacity strategy, not a demonstrated
guarantee for arbitrary future upstream growth.

## Measured cartridge footprint

Both standard outputs occupy exactly 2,097,152 bytes. The current header selects
MBC3 with RTC and 32 KiB battery RAM. Native helpers were not relocated.

| Resource | Disabled counterpart | Mobile build |
| --- | ---: | ---: |
| ROM0 free | 18 bytes | 39 bytes |
| ROMX free, summed over banks | 13,943 bytes | 2,810 bytes |
| Total ROM free | 13,961 bytes | 2,849 bytes |
| WRAM0 free | 300 bytes | 51 bytes |
| WRAMX free, summed over banks | 4,912 bytes | 1,359 bytes |
| SRAM free | 1,832 bytes | 1,832 bytes |

The net ROM cost is 11,112 bytes, approximately 10.85 KiB or 0.53% of the
cartridge. It consumes approximately 79.6% of the disabled build's remaining
space. Free bytes are fragmented; the summed total is not a promise that another
routine of that size fits. ROM0 and unbanked RAM are separate tight constraints.

The feature's measured ROM sections are:

| Section | Bytes |
| --- | ---: |
| Driver, wire protocol, actors, leases, decoder and prototype menu | 8,011 |
| Private native palette logic | 1,258 |
| Generated native lease policy | 1,005 |
| Save isolation, raw tile backup and map fingerprint helpers | 664 |

The section total is 10,938 bytes; the net whole-ROM cost also includes
conditional integration changes and replaced native instruction sequences.
Within the main section, the prototype menu occupies 535 bytes and the resumable
decoder 545 bytes. These are measured code ranges, not promised savings.
The lease policy already uses one index per map and 89 shared three-byte masks.
Repeated code and optional diagnostic paths deserve optimization, but there is
no measured multi-kilobyte saving yet. Removing save isolation, bounds checks,
recovery or timing limits is not an acceptable optimization.

## Capacity strategies and limits

1. Measure byte and cycle changes for every optimization. Moving a routine among
   existing ROM banks helps placement but does not reduce its total storage.
   Removing the entire temporary menu could save at most its 535-byte range,
   before replacement UI, hooks and behavior are accounted for.
2. WRAM is not extra cartridge program storage. Code loaded into WRAM still needs
   its original ROM representation unless downloaded; compression additionally
   needs decoding, working space and time. Existing WRAM pressure, timing and
   adapter-free recovery make either approach an explicit design tradeoff.
   Ordinary SRAM must remain save-owned. The Mobile Adapter does not provide
   generally addressable cartridge program memory to the game.
3. Bridge/server logic already owns household routing, lobby membership and
   Internet transport. Further offloading may reduce ROM policy, but cannot
   eliminate native input, serial handling, actor publication and save isolation.
   Requiring an emulator to inject game RAM would change the hardware model.
4. A multiplayer-only MBC30 build is a candidate for a larger physical cartridge:
   4 MiB ROM with RTC. It is not compatible with every ordinary MBC3 cartridge.
   Keep the disabled build byte-identical, preserve the ordinary 32 KiB save
   allocation, and verify bank addressing, RTC, cartridge boards/flashcarts and
   emulator support before adopting it. The pinned DoubleCherryGB core currently
   masks MBC3 ROM bank writes with `0x7f` in `gb_core/mbc.cpp::mbc3_write`; merely
   producing a 4 MiB file would alias upper banks in this backend. No MBC30
   compatibility claim is established. More ROM also does not enlarge ROM0 or
   native WRAM.

With a fixed 2 MiB capacity, a nonzero addon and unrestricted future upstream
growth, the no-content-choice guarantee is mathematically impossible. A larger
verified cartridge target, measured optimizations and upstream resource gates
can give practical headroom. They do not prove compatibility with every future
engine change. Do not advertise that stronger guarantee as implemented.

Hardware references: [Pan Docs MBC3/MBC30](https://github.com/gbdev/pandocs/blob/master/src/MBC3.md)
and [OAM selection](https://github.com/gbdev/pandocs/blob/master/src/OAM.md).

## Four players on one scanline

Native ordinary 16x16 characters use four 8x8 hardware objects. Each covered
scanline intersects two of those objects. The PPU can select only ten objects
per scanline, independently of the total forty-entry OAM capacity:

| Ordinary characters with overlapping vertical spans | Objects per scanline |
| --- | ---: |
| Four players | 8 |
| Four players plus one NPC | 10 |
| Four players plus two NPCs | 12, exceeds hardware selection |

Offsets during movement can create the overlap without actors sharing a map
tile row. Weather, emotes, large figures and other effects also consume objects.
Using 8x16 objects would not remove the two-objects-per-line width of a 16-pixel
character. An offscreen X position still consumes scanline selection; a hidden
object must use an appropriate offscreen Y or be omitted from OAM.

Current behavior follows native OAM priority construction in
`engine/overworld/map_objects.asm::InitSprites`. Remote actors receive
`HIGH_PRIORITY`; the renderer fills OAM in reverse order with high-priority
groups last, making them earlier in hardware OAM. The PPU selects the first ten
entries whose Y spans intersect the line. Later entries lose pixels on that
line. This can produce incomplete NPC figures or changing clipping as actors
move. Neither the event nor the NPC state is removed by this hardware effect.
Other high-priority native actors can also compete; this is not a universal
four-player visibility guarantee.

The native lease allocator reclaims reviewed NPC resources for object-slot or
total-OAM pressure. Its `.demand` counts complete facing quads globally. The
`mobile_native_oam_bounds` integration macro prevents forty-entry buffer
overflow. Neither implements per-scanline whole-character admission. The old
independent renderer's temporary suppression policy was removed and must not
be cited as current native behavior.

## What existing tests prove

- The regenerated policy reviews 611 compiled maps and approves 775 candidate
  map actors. The current static capacity audit resolves 607 raw bounds and all
  611 bounds with four additional reviewed cases. This is an ordinary native
  object-slot model, not a proof of every scripted actor lifecycle or scanline.
- The accepted four-core cadence traces at 100/200/300/400% contain 2,143,
  4,379, 6,580 and 8,848 online observations respectively. Their maximum
  scanline demands are 6, 8, 8 and 6. They never exercise the twelve-object
  four-player-plus-two-NPC case.
- `probe_lobby.py` has a ten-per-line assertion only behind
  `not args.native_actor_probe`. Current native acceptance takes the other path.
  Native tests observe actual OAM entries and tile/color groups, but entry
  presence alone does not prove that the PPU selected every avatar pixel.
- Build-time source guards cover the lease generator, candidate maps,
  relevant definitions and reviewed capacity contracts. The lease manifest
  directly hashes only 304 of the 611 map ASM files. Some additional maps are
  protected by the capacity-contract hashes. New map inventory changes are
  guarded by enum/list hashes and the generator's explicit count check, but not
  every existing map edit is guarded.
- `make mobile` checks those source fingerprints; it does not run the complete
  post-link capacity calculation. The full capacity CLI currently reports
  unresolved maps without making that condition a failing exit status. The
  current audit has none, but this remains a future-change gate gap.

These results cannot support a claim that every existing or future map and
arbitrary player arrangement is proven compatible.

## Required follow-up before stronger acceptance

1. Add an adversarial native scene with all four players and zero, one, two or
   more ordinary NPCs on overlapping scanlines. Include partial-row overlap,
   movement, effects and protected native actors. Check actual PPU selection
   and complete rendered figures, not only OAM membership, against ordinary-NPC
   controls with matching geometry and the strict cadence gate.
2. Design bounded whole-character presentation admission. Only audited
   replaceable NPCs may lose their complete display for a frame, with automatic
   restoration and no event, save or gameplay-state mutation. This presentation
   decision is distinct from leasing an object slot or graphics storage.
3. Decide how protected native actors and four simultaneous player figures are
   handled when their physical scanline demand exceeds ten. Hiding only optional
   NPCs cannot solve a line containing four players plus two protected NPCs.
   Do not silently hide required actors, defer required players or accept
   flicker as fulfillment of the four-player guarantee. Report the conflict to
   the user before choosing a policy. Background rendering or different graphics
   would require a separate footprint/timing/integration design.
4. Make full post-link all-map resource auditing a failing acceptance gate;
   cover every map, geometry, movement/facing input and reviewed lifecycle
   assumption. Reject unknown or unresolved cases until reviewed. Combine
   structural bounds with targeted native fixtures for script-created actors
   and dynamic assets. A source fingerprint is a change detector, not a proof.
5. Introduce per-bank ROM and native-RAM budgets plus disabled-hash checks on
   upstream updates. Measure optimizations and assess the MBC30 path separately
   before adding milestone-3 ROM features.

## Audit verification and small repair

A final whitespace cleanup in the previous commit changed the raw lease
generator fingerprint after its manifest had been generated. Regeneration
changed only `generator_sha256`; the generated assembly table is byte-identical.
The reviewed source checks, compiled-policy check and eight lease unit tests
pass again. Both ordinary builds were rerun and retain their exact hashes:

- Disabled: `314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e`
- Mobile: `fdb237ff7a3bf88d20236ca18a33f239a158b6ca0b9b0edd8193ee6d62bde138`

The fresh full capacity report is retained in host cache as
`rom-render-limit-capacity-20261011.json`. ROM code, allocation and protocol
behavior did not change; existing runtime measurements remain measurements of
the same binaries. No user listener, running emulator or battery save was altered.
