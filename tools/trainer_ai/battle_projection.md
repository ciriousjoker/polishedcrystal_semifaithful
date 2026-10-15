# Real frontend projection discriminator

`battle_projection_check.py --project /Volumes/Storage/tmp/polished-ai` compiles
one independent mGBA adapter and runs six actual `UpdateHPBar` calls. There are
**no presentation hooks**, patched return instructions or injected RNG outputs.
LCD, VBlank interrupts, the actual VBlank0 handler and source OAM DMA code execute.
The fixture enables CGB state, normal background tile transfer, supplies the
source ROM checksum and a valid source stack, and copies the ordinary OAM DMA
routine from the ROM to HRAM. No buttons are pressed. Sound, timers and graphics
start from neutral fixture state; this is not a boot-to-battle integration test.

Both battle sides cover HP changes 150 to 140, 20 to 0, and 10 to 150. The active
mon's already-committed HP matches the new value and its maximum HP is 150. The
existing small Smeargle fixture supplies ordinary initialized active structs;
only its data constructor runs, without loading a lifted engine. Individual
initial and final CPU/RAM snapshots and source/ROM identities are retained.

## Observed result

All six calls returned without fault. They executed 11, 16 and 94 VBlanks,
respectively, and 33, 48 and 282 actual DIV reads. The complete two active battle
structs remained unchanged. Source stack balance and BC preservation passed.

The actual frontend is observably different from an identity-return hook:

- AF, DE and HL change. `UpdateHPBar` preserves BC explicitly, not every register.
- `wCurHPAnimOldHP` becomes the new HP. This aliases `wHPBuffer2`, so the effect is
  visible through a shared computational scratch buffer as well.
- HP palettes, tilemaps, graphics scratch and interrupt state change.
- Actual VBlank refreshes change RNG state and divider counters.

This is a successful discriminator, not a proof that all current identity
hooks are safe. For `_SubtractHP`, the source explicitly pushes and restores AF
around the update, protecting its immediate faint branch. This local observation
does not establish caller liveness for every rendering hook or every shared
buffer. `UpdateHPBarBattleHuds` also runs the HUD caller after the animation;
that broader caller is not executed by this probe.

A general simulation must either execute the source computational part of each
rendering caller, or justify a narrower replacement ABI using actual caller
liveness and gameplay-state projection tests. Exact register or scratch equality
with the existing identity-return headless profile must not be claimed. Random
contract validation remains distinct from the hardware distribution of human UI
and DIV timing. No complete turn or complete ordinary frontend battle equivalence
is established here, and no emulator was introduced into training rollouts.

## Computational caller continuation

`battle_projection_callers.py` adds sixteen actual `_SubtractHP` and `RestoreHP`
caller cases, including both real HUD wrappers and their source descendants.
The independently executed conditional path uses identity returns only for
UpdateHPBar, UpdatePlayerHUD and UpdateEnemyHUD. The actual path has no such hooks.

Both sides cover ordinary damage, exact KO, overkill, zero damage, ordinary
healing, capped healing, full-health healing and zero healing. All sixteen cases
passed the declared projection: complete active structs, current damage,
first-faint marker and BC. `_SubtractHP` AF matches as expected from its source
save/restore. Calls execute 14 to 97 real VBlanks and 42 to 291 actual DIV reads.
Every actual and conditional call completes without a fault.

The broader caller reveals more scratch differences, including wMonType,
wCurSpecies and wTempMonLevel. RestoreHP AF differs and is not a documented
preserved return. DE/HL also differ. These are recorded rather than suppressed.
Both relevant computational entries initialize their HP buffers before reading
them, which establishes local independence from the preceding animation's old-HP
alias. This does not prove global scratch liveness or complete turn equivalence.

Separate V2 evidence is in `build/battle-projection/callers-v2/report.json` with
48 initial/actual/conditional snapshots. Original R1 sources/report are retained
in `build/battle-projection/revision-1`; R1 was not freshly rerun for this extension.
