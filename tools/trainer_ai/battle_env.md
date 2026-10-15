# Restricted actual-caller fight experiment

This is a runnable **conditional headless** 1v1 environment. It is not accepted
as equivalent to an ordinary graphical battle and is not a complete-game
simulator. It must not establish training quality or ordinary-game fidelity.

The fixture constructs both Smeargle with Own Tempo, level 50, no item or status,
normal typing, neutral stages, no affection option, and zero initialized field,
hazard, delayed-effect, switch and volatile state. Both know actual ROM move IDs
33 (Tackle) and 55 (Water Gun). Attack/defense/special stats and speeds differ
between actors. The second actor's Attack is 25, giving it a meaningful special
move preference. No arbitrary RAM fixture is accepted by the public constructor.
Combat stats are supplied at the battle-start boundary; their EV/DV derivation
is not covered.

The native generated code runs actual `DetermineMoveOrder` and `DoTurn`, including
all normal-hit effect commands and full damage application. The normal-hit
program is checked against the assembled ROM tables before execution. There
are no hand-written damage, hit, critical, STAB, PP or HP rules. The host commits
both legal actions, clears the neutral per-attack damage accumulator as in
`PerformMove`, invokes the actual callers, cancels the defeated actor's action,
and reports a 1v1 terminal winner. Experience, progression and the game's faint
UI/controller are outside this projection. No reserves or replacements exist.
PP exhaustion truncates explicitly as `unsupported-struggle`; a turn budget also
truncates. Neither condition is reported as a win.

## Frontend boundary and remaining fidelity gap

The precise experimental profile returns immediately at `StdBattleTextbox`,
`PlayBattleAnim`, `UpdateHPBar`, `UpdatePlayerHUD`, `UpdateEnemyHUD` and
`DelayFrame`, preserving registers/flags and popping the real return address.
This identity-return ABI is an explicitly changed frontend. It does not claim
these functions actually preserve every register. All combat commands, including
`usedmovetext` and `moveanim`, still execute, retaining their mechanical writes.
Real `DelayFrames`, `ApplyTilemapInVBlank` and `PlayFXAnimID` wrappers also execute.
Unknown paths and excluded instructions fault and cannot be treated as UI events.

The independent mGBA wrapper applies exactly this same headless profile. Complete
CPU and supported WRAM/HRAM equality therefore establishes conditional native
translation correctness, not equivalence to an unmodified frontend. In the real
game, VBlank invokes `UpdateDividerCounters` and `AdvanceRNGState`. Omitting
frames changes shared RNG state. Uniform private DIV bytes define an explicit
experimental event law and do not establish the real hardware-time distribution.
A faithful ordinary-game pilot remains gated on this boundary and RNG audit.

The profile pins all ROM bytes and exact symbol layout, masking only the fixed
Tackle power byte plus header checksum bytes. Thus only fixed-layout Tackle power
data changes can propagate automatically in this headless experiment. Any other
ROM change, including a transitive presentation callee, requires a new audit.
Additional conservative source-file hashes reject known frontend source changes.
The general M1 lifter keeps its previous default behavior and update coverage.

## API and lifecycle

```python
fight = NativeFight(rom, sym, source_root, build_dir, seed=1, orientation=0)
while not fight.terminal and not fight.truncated:
    own = fight.observation(0)
    opposing = fight.observation(1)
    events = fight.step((MoveAction(0), MoveAction(1)))
winner = fight.winner
fight.reset(seed=2, orientation=1)
```

Construction verifies source/ROM/native identity once. `reset` reuses the verified
engine and cached fixture constants under a frozen input identity. Create a new
engine after changing inputs. The observation exposes full fixture combat stats,
level, types, ability, item/status, known moves and PP, without freezing a learned
feature selection. It excludes committed opposing actions and future RNG input.
The private event stream generates only environment randomness. Exceptions after
committing actions poison the instance until `reset`; illegal actions reject
atomically before committing. Native rollouts do not call mGBA.

## Reproduction

Use one process/thread, no GPU, with numerical thread-pool limits set to one.
The existing built ROM/SYM/source and isolated mGBA can be reused.

```sh
python3 -B tools/trainer_ai/battle_env_check.py --build \
  --rom /path/to/game.gbc --sym /path/to/game.sym \
  --source-root /path/to/game-source --build-dir /tmp/battle-env \
  --mgba-prefix /path/to/mgba
```

The checker asserts six complete fights, both orientations, three fixed policies,
full CPU/RAM replay equality at both real callers, native-only complete fights,
legality, boundary configuration, ROM0 trap alias, presentation/transitive body
mutation rejection, and poisoned-instance behavior. Nonzero exit is a failure.

`battle_env_updates.py` clones an existing built disposable source snapshot,
changes actual Tackle power 40 to 80, rebuilds only the affected object and ROM,
rejects old artifacts, regenerates the native library and repeats the full fight
suite. It also rejects a source change adding a combat write inside a skipped
presentation body. Use a fresh output directory and preserve the original game.
`battle_env_bench.py` runs 100 complete native fights using one reusable verified
engine, including reset, observations and a fixed heuristic. Its throughput is
only for this conditional headless scope, not a training-duration estimate.
