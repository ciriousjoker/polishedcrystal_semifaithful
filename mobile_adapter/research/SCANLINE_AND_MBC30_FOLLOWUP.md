# Native scanline gates and multiplayer-only MBC30 investigation

> MBC30 adoption was implemented on 2026-10-12 using the supplied fork.
> See [MBC30_MIGRATION.md](MBC30_MIGRATION.md) for current mapper/core evidence.
> Scanline blockers below remain open; mapper feasibility notes are historical.

Date: 2026-10-11. Branch: `mobileadapter`.
This follow-up supersedes the missing-test/source-guard statements in the earlier
ROM/render audit. It does not establish complete four-player rendering acceptance.
Production ROM code, mapper, user listeners and user saves were not changed.

## Approved presentation rule

The user approves omitting an explicitly replaceable NPC's complete figure when
scanline resources are needed, then automatically showing it when capacity returns.
Preserve NPC state, scripts, story flags and save isolation. Presentation admission
is separate from native object/tile leasing. This rule is not implemented yet.
Required trainers, shops, services and other protected actors cannot silently be
made replaceable to obtain a passing test. A protected pair plus four overlapping
ordinary players demands twelve hardware objects per scanline; the hardware limit
is ten. Such cases must be flagged and reviewed before choosing a rendering rule.

## All-map conflict inventory

`native-scanline-conflicts.csv` retains all 698 conservative pair candidates on
269 of 611 compiled maps. 355 pairs contain two protected actors; 123 of those
start on the same row without event flags. Classification distinguishes reviewed
replaceable actors, required trainers/shops and protected actors needing review.
Protection is a safety default, not proof that every protected actor is essential.
Time masks and ordinary wandering envelopes are included. Collision/reachability,
mutually exclusive story states, special assets, effects and scripted positioning
are not fully modelled. A missing candidate is not a universal compatibility proof.

A concrete required pair exists on Route 37: trainer map indexes 2 and 3 at `(6,12)`
and `(7,12)`. Both have no event flag and remain protected even after a battle.
The fixture places four separate live players at `(8,12)`. Their overlap is legal
with remote hitboxes disabled; horizontal overlap never reduces OAM selection.

Every map ASM source is now fingerprinted, including maps without lease candidates.
All 457 source block files, shared-block aliases, collision sources, map attributes
and relevant facing/renderer definitions are covered. Changed/new maps or geometry
require regeneration and review. Fingerprints detect changes; they do not prove
that the changed map is safe. Unit tests reject edits to a protected-only map,
shared block data and collision definitions.

`test-mobile-maps` now runs full post-link slot-capacity and strict scanline audits.
Unresolved slot bounds produce failure instead of a successful exit status. The
scanline gate currently fails on the flagged inventory, including optional pairs
whose whole-character admission is still pending. Do not turn these flags into
approved exceptions merely to obtain a green run.

## Current native runtime gate

The observer captures all forty real hardware OAM Y coordinates at each video
callback. `scanline_selection.py` independently selects the first ten Y-matching
8x8 objects on every visible line, checks peak demand and checks remote object rows
that would be discarded. This applies to `--native-actor-probe` in the current
acceptance path. A fresh observer is compiled by `verify_milestone2.py` so a cached
old runner cannot silently miss the new fields. An empty observation cannot pass.
The final stable view separately requires every declared visible peer to have a
complete visible hardware group; unfinished join uploads are not treated as final
visibility failures. These are frame-boundary selection checks, not proof of every
mid-frame DMA state or background/pixel priority.

The pinned DoubleCherryGB color renderer iterates all forty OAM entries without
the real hardware's ten-object selection cap. Its screenshot alone would conceal
some hardware overflow. The independent selector therefore remains necessary.

A four-core Game Corner Start-menu control passes: peak demand eight for every
client, complete final peer groups. The Route 37 fixture fails: all clients report
three rendered remote peers but one complete remote figure is hidden in actual
OAM. Peak demand is only ten because that figure is already absent. This exposes
an earlier visibility failure, not a successful solution to the twelve-object
conflict. The cause remains unresolved. Fix it, then repeat the protected-pressure
fixture and matched ordinary-NPC cadence controls before accepting rendering.

Reproduce from the configured host with fresh output directories:

```sh
make test-mobile-host RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/
make test-mobile-maps RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/
make test-mobile-scanlines RGBDS=/Volumes/Storage/Cache/mobileadapter/rgbds-1.0.4/
python3 mobile_adapter/tools/probe_scanline_pressure.py --diagnostic \
  --output /Volumes/Storage/Cache/mobileadapter/route37-diagnostic-new
```

The two new acceptance commands currently fail intentionally on unresolved
compatibility findings. `test-mobile-rom` also requires both map audits and the
protected-pressure fixture; the full acceptance command cannot bypass those
known failures. Direct `verify_milestone2.py --cases ...` remains available for
selected diagnostic/control runs, not complete acceptance. `--diagnostic` retains the known failure and reports
`acceptance_passed: false`; it is not an acceptance waiver. Battery fixtures are
made through normal New Game and in-game Save, without game-RAM or save injection.
Host-only unit/ASM tests remain separate from emulator tests.

## Multiplayer-only upper ROM half

Ordinary MBC3 provides 2 MiB ROM, RTC and 32 KiB save RAM. MBC30 provides 4 MiB ROM
with RTC. It can retain the ordinary 32 KiB save allocation. There is no separate
MBC30 header type; ROM size plus supported mapper behavior is relevant. Source:
[Pan Docs MBC3/MBC30](https://github.com/gbdev/pandocs/blob/master/src/MBC3.md).

An isolated layout probe successfully linked all six banked feature sections into
bank 128 in a 4 MiB ROM. All original game ROM sections stayed below bank 128;
ordinary feature-disabled ROMs and production source files were restored exactly.
Resident RAM code and guarded ROM0 integration still use scarce lower resources.
A larger cartridge adds ROMX capacity, not ROM0, WRAM or sprite capacity. This is
a layout feasibility result, not runtime/RTC/save acceptance or a production flag.

The native `tools/bankends` checker rejects bank 128. The probe reports that
post-link limitation explicitly and verifies the expanded map independently;
it does not bypass the production checker or claim the ordinary build passed.

The unmodified pinned DoubleCherryGB core is **not MBC30-compatible for upper ROM
banks**. A standalone 4 MiB ROM executed reads of banks 1, 128 and 255. Bank 1's
positive control returned `0x31`; upper banks returned `0xf0` and `0x7f` instead
of `0xa5` and `0x5a`, consistent with the `0x7f` bank mask. The pinned revision is
`03f58ca3dfb4b716f7e66a0e0467f9e85ef82abb`, also the observed remote HEAD.
Source: [DoubleCherryGB mbc3_write](https://github.com/TimOelrichs/doublecherryGB-libretro/blob/03f58ca3dfb4b716f7e66a0e0467f9e85ef82abb/gb_core/mbc.cpp#L560).

```sh
python3 mobile_adapter/tools/probe_mbc30.py \
  --output /Volumes/Storage/Cache/mobileadapter/mbc30-core-new
python3 mobile_adapter/tools/probe_mbc30_layout.py \
  --output /Volumes/Storage/Cache/mobileadapter/mbc30-layout-new
```

Run the layout probe serially with other builds: it temporarily changes only
feature-owned section placement and restores it in a finally block, using isolated
objects/output. The core probe reports unsupported banks normally; add
`--require-supported` when making it an adoption gate.

The user's multiplayer-only cartridge proposal is a practical capacity direction.
Before adoption: add a guarded production mapper/layout switch, update bank budget
checks, maintain hash-identical disabled builds, implement and verify MBC30 in the
supported emulator fork, and test RTC, saves, nested bank calls, state restoration
and real cartridge support. Do not silently ship a 4 MiB ROM to an unsupported core.

## Evidence and remaining work

Compact immutable results are in `scanline-mbc30-evidence.json`. Full traces and
producer logs remain in host cache. Host Rust/Python/ASM checks pass, including
fourteen scanline/map-source tests. Production disabled and mobile hashes remain
unchanged. M2 rendering assurance is still open, with these next steps:

1. Diagnose Route 37's missing complete remote figure.
2. Implement bounded whole-figure admission for approved replaceable NPCs.
3. Review the protected conflict inventory and obtain a rendering decision where
   four players plus required actors cannot all fit. No protected actor is hidden
   or reclassified by this change.
4. Add partial-overlap, effect and scripted-lifecycle fixtures plus matching
   ordinary-NPC cadence controls. The present audit is not universal proof.
5. Treat MBC30 as a separate verified capacity migration before new ROM features.
