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

The launcher asks which scene to use. Its defaults are Chris on the Mac and Kris
on Android. `--mac-avatar` and `--android-avatar` select another existing figure.
Every deployment regenerates the emulator auto-state using the current ROM and
the exact core on that device. CPU/WRAM snapshots are generated in the external
host cache, not committed or reused across builds. Integration acceptance boots
the ordinary `.sav` files directly and observes the matching build's symbols.

Regenerate all twelve batteries after a save-format change or fixture update:

```sh
python3 mobile_adapter/tools/generate_save_scenarios.py \
  --runner /Volumes/Storage/Cache/mobileadapter/smoke-libretro \
  --core /Volumes/Storage/Cache/mobileadapter/build/doublecherry/DoubleCherryGB_libretro.dylib \
  --artifacts /Volumes/Storage/Cache/mobileadapter/new-scenario-producers
```

Choose a fresh external artifact directory. Review the updated ordinary saves
and manifest together. Generated producer ROMs, screenshots and traces remain
outside the repository.
