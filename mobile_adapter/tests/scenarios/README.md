# Portable starting saves

Three scenarios are committed with ordinary 32 KiB in-game battery saves for
each character choice: Chris, Kris, Crys and Beta. They are shared by the manual
launcher and native integration tests.

| Scenario ID | Location | Purpose |
| --- | --- | --- |
| `bedroom` | Player's upstairs bedroom, 3,3 | Simple avatar, movement and Start-menu test |
| `game-corner` | Goldenrod Game Corner, 14,5 | Crowded room, wandering patrons and sprite pressure |
| `goldenrod-city` | Goldenrod City, 25,18 | Outdoor layout with wandering and patrolling civilians |

`manifest.json` records hashes, main/backup checksums, save version, producer ROM
hashes and every initialization substitution. Creation uses a disposable ROM
with a chosen spawn, clock and initial character-menu selection, then normal
New Game and the in-game Save menu. The selected-character initialization replaces
only the producer's initial cosmetic fade before GenderMenu. No game RAM or
battery bytes are edited. Production ROMs load these ordinary saves through
Continue. The files contain no copied third-party save or real player progress.

The launcher asks which scene and how many Mac windows to use, then optionally
includes a detected Android. For the manual session it clones the validated
Chris save for every player and changes only the ordinary figure byte, trainer
ID and main/backup checksums. This host-side fixture preparation does not change
the ROM's save logic. Committed originals retain their in-game provenance above;
other integration tests can still use those unmodified originals.

Mac figures cycle through the four choices; Android defaults to the next one.
The party, progress, name, clock and position remain byte-identical between the
session batteries outside those whitelisted fields. Every deployment regenerates
the emulator auto-state using the current ROM and exact device core. Each Mac
instance has a separate timestamped profile. CPU/WRAM snapshots remain in the
external host cache and are not reused across builds.

Trainer IDs need not be unique. Duplicate IDs, including four equal IDs, must
not affect connection identity or lobby routing. The server assigns separate
session player IDs. The host-only scenario tests check the exact battery delta;
the native duplicate-ID probe boots four such saves through normal Continue.

Regenerate all twelve batteries after a save-format change or fixture update:

```sh
python3 mobile_adapter/tools/generate_save_scenarios.py \
  --runner /Volumes/Storage/Cache/mobileadapter/smoke-libretro \
  --core /Volumes/Storage/Cache/mobileadapter/mbc30-ef247c6/DoubleCherryGB-mbc30-macos-universal/DoubleCherryGB_libretro.dylib \
  --artifacts /Volumes/Storage/Cache/mobileadapter/new-scenario-producers
```

Choose a fresh external artifact directory. Review the updated ordinary saves
and manifest together. Generated producer ROMs, screenshots and traces remain
outside the repository.
