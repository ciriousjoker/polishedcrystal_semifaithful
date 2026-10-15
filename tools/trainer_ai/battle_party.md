# Conditional two-member party fixture

`PartyFight` runs a fixed two-Smeargle roster on each side. Actual source
`DetermineMoveOrder` and `DoTurn` execute attacks. Actual party synchronization
and `ResolveFaints.check_battle_over` decide whether the trainer battle ends.
An active KO with a living reserve does not end the battle. A validated host
choice selects a reserve, then actual `GetUserSwitchTarget`, whole
`SendInUserPkmn`, `SpikesDamage` and `RunEntryAbilities` execute replacement.

This is explicit host phase orchestration of independently verified source
callers, not a proof of the complete `HandleBetweenTurnEffects` or graphical
battle controller. The source replacement controller is in
`engine/battle/endturn.asm:HandleBetweenTurnEffects.endturn_loop`. Faint
animations, experience, progression, trainer rewards and money do not run.
WinTrainerBattle/LostBattle are stop events before progression, never successful
identity returns. Voluntary switching, random forced displacement, simultaneous
faints, nonneutral status, other abilities, held items, hazards, weather, Baton
Pass, Substitute and Struggle remain outside the public constructor.

## Fixed start and source loading

Only seed, orientation and the existing bounded turn budget vary. Constructor
and reset supply complete fixed parties without accepting arbitrary initial RAM.
Active and reserve HP/stats differ, and reserve PP starts at 7/8. Source copies
party fields to battle fields, loads base types and regenerates abilities.
No host stat-copy function implements a switch. Source `BattleConsumePP`
updates active party PP, while source UpdateBattleMonInParty/UpdateEnemyMonInParty
updates level/status/current HP. The party field layout is pinned through the
source manifest and ROM audit.

Both party counts and the two actual PartyMon species fields are initialized.
This source snapshot has no `wPartySpecies` or `wOTPartySpecies` array: player
count is followed by seven unused bytes, and enemy count by Mirror Herb state
before its PartyMon structures. No separate terminated species list is invented.
All four real nickname structures use the fixed PokemonNames ROM table entry,
the source MON_NAME_LENGTH, and the source charmap's `@` terminator, following
GetPokemonName's table offset/length/terminator convention. Actual SendInUserPkmn
copies these names into the active battle structures.

The old no-switch fixture directly sets the ability cache to Own Tempo.
Its current actual move/ability getters read that cache, which is coherent at
the declared initialized-battle boundary. It does not establish ordinary source
initialization from personality and option bits. This new fixture enables
abilities with `wInitialOptions = $02` and sets party/active personality to
`ABILITY_1 = $20`. Actual `GetAbility` selects Own Tempo for Smeargle from that
personality. Zero ability selector would select hidden Moody when abilities are
enabled; disabled ability options would produce no ability. Existing factories,
core hook lists and their evidence remain unchanged.

The public observation contains both members of both rosters, with inactive
HP, max HP, stats, status, item, moves and PP from actual party RAM. Because
every member is the same species/form/ability selector, types and ability are
the profile established by the initial actual source send-in. This reuse is
specific to homogeneous fixed rosters. It is not an implementation of general
inactive ability calculation. Observations omit committed opponent actions,
future DIV events, controller scratch and arbitrary RAM.

Replacement callbacks receive `(side, observation, legal_indices)` only. Legal
indices are zero-based and exclude the active member, zero HP and eggs. The
source target fields receive one-based choices. Callback failure after an attack
poisons the partially executed fight; `reset(seed, orientation)` reconstructs
the closed fixture through actual source sends and reuses verified handles.
No compiler, library reload or source hashing runs per reset. Events and
replacements are cleared; each episode is bounded by at most 64 turns.

## Separate presentation projection

The existing six headless hooks remain frozen. This module adds nine named
identity-return hooks in its own standalone verified native library:

| Hook | Explicitly omitted frontend work |
| --- | --- |
| BattleTextbox | Tilemap textbox construction and text rendering |
| GetCGBLayout | Palette selection and layout |
| SetDefaultBGPAndOBP | Display palette conversion |
| LoadTileMapToTempTileMap | Banked frontend tilemap snapshot |
| ApplyTilemapInVBlank | Frontend transfer request and frame wait |
| GetBackpic | Back sprite decompression and upload |
| GetFrontpicOrGhostpic | Front sprite/ghost rendering |
| BattleAnimateFrontpic | Cosmetic front sprite animation and cry |
| PlayStereoCry | Audio engine and sound wait |

The ABI preserves all registers/flags and pops the actual return address.
Conditional mGBA uses exactly the same named identity returns. This removes
frontend scratch and timing behavior and does not prove their ordinary hardware
projection. Full supported CPU state is compared under this declared profile.
The full-ROM audit and source manifest bind the bodies and transitive source;
only the existing fixed-layout Tackle-power/checksum update is admitted.

Computational callers remain actual source. In particular, UserSentOutText
updates the enemy HP snapshot, SendOutPlayerMon resets action/move counters and
checks Amulet Coin, enemy send-in copies TempMon and maintains base data, and
SendInUserPkmn updates seen state, base experience, ability, active index and
target clearing. These functions are not rendering identity returns. Source
hazard/ability dispatch executes even though the fixture contains no hazards
and Own Tempo has no confusion to cure.

Unknown PCs/instructions/MMIO reject. Named exclusions for LoadAnim, trainer
final-speech graphics, PlayMusic and SlideEnemyPicOut reject optional paths.
Random displacement is rejected separately from KO replacement. Conservative
static closure also records rejected unsupported/non-ROM paths in the manifest;
none is a successful hook. Dynamic source continuation roots include
GetAbility.Function, _ReturnFarCall and _CopyBytes, and the OwnTempoAbility target
is a source root for the actual ability jumptable.

## Evidence and reproduction

Serial, one numerical thread, no GPU. Run only with the coordinator's sole CPU
slot. Prefix and full libraries are independent of the frozen baseline library.

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B tools/trainer_ai/battle_party_profile.py --rom /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game/polishedcrystal-3.2.3.gbc --sym /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game/polishedcrystal-3.2.3.sym --source-root /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game --build-dir /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-party/prefix
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B tools/trainer_ai/battle_party_probe.py --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B tools/trainer_ai/battle_party_profile.py --rom /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game/polishedcrystal-3.2.3.gbc --sym /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game/polishedcrystal-3.2.3.sym --source-root /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game --build-dir /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-party/sendin --full
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B tools/trainer_ai/battle_party_check.py --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B tools/trainer_ai/battle_party_check.py --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai --guards-only
```

The accepted run produced:

* 10 prefix/choice-boundary full-state comparisons: both sides, alive/KO active
  members, source choice return, dirty stages/volatiles, and unselected controller
  boundaries. No frontend hook is needed for this prefix.
* 4 complete SendInUserPkmn full-state comparisons with actual returns.
* 6 complete 2v2 battles and 573 per-call full-state mGBA comparisons: 90 order,
  173 attacks, 90 each player sync/enemy sync/source terminal decision, and 10
  each source target/send-in/hazard/entry-ability calls. Both sides replace.
* 12 native replay episodes: two identical seeded resets for each of six
  orientation/strategy combinations, comparing full State bytes, observations,
  events, replacements and winner.
* Explicit reserve active/fainted/egg guards, invalid-action atomicity, invalid
  replacement callback failure and full-state restoration by reset after a failed fight.
  Party counts, real PartyMon species fields, all four source-table nicknames,
  active nickname copying and the absence of separate species arrays are also checked.

Files in `build/battle-party`: prefix-proof.json, sendin-proof.json,
fight-proof.json and guard-proof.json. The mGBA checks record its concrete DIV
reads and replay them natively. Native replays use the private IID byte stream.
Those event laws can produce different turn counts; neither establishes the
ordinary frontend/hardware DIV law or model strength. No learner or model input
schema was changed in this slice.

The final acceptance reran prefix, whole-party and guard checks after the nickname
fixture correction, reusing unchanged native libraries. All proof files record
the executed fixture source SHA256 in `fixture_source_sha256`; final source
identities are listed in `build/battle-party/final-source.sha256`.
