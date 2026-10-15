# Conditional status residual fixtures

`StatusFight` composes the existing verified `NativeFight` actual move callers
with a separate verified native `HandlePoison` / `HandleBurn` leaf. Exactly one
initially affected Smeargle is accepted, using `status="poison"`, `"burn"` or `"toxic"`.
The legacy `poisoned_side` argument names the affected actor in all three modes.
Orientation, affected side, status mode, private seed and the existing turn
budget are the constructor inputs. No arbitrary initial RAM,
other statuses, items, weather or replacement rosters are supported.

Actual `SetFastestTurn` selects residual order inside both status handlers. Actual
status ability checks, minimum damage, maximum-HP division and HP subtraction
execute from the ROM. A dynamic `CallBattleCore` target requires the explicit
`SubtractHP` root, as in the existing move profile. Unknown instructions and PCs
remain faults. The source/ROM audit and six existing presentation identity-return
hooks remain unchanged. No new frontend hook is introduced for these status modes.

After each nonterminal attack pair and selected status call, a second verified leaf
runs actual `UpdateBattleMonInParty`, `UpdateEnemyMonInParty` and
`ResolveFaints.check_battle_over`. This synchronizes party HP/status through source
code and selects trainer battle completion from actual party checks. Execution
stops **before** `WinTrainerBattle` or `LostBattle` as an explicit terminal event.
These events do not return, and are not presentation identity-return hooks.
Rewards, experience, faint animations, money, progression and menus do not run.
Both actors fainted at this boundary reject. A partial execution error makes the
instance unusable until reset. Invalid actions reject before mutation.

This is declared host orchestration of independently checked source callers,
not a proof of the complete `HandleBetweenTurnEffects` or `ResolveFaints`
controller. The other ordinary endturn handlers, affection, abilities and other
omitted effects are not inferred from the selected status parity. Attack KOs retain the base
fixture's host terminal rule. The status extension does not change that rule.
All ordinary frontend and hardware RNG fidelity gaps in `battle_env.md` remain.
Uniform private DIV bytes without frames are a conditional experimental event
law. This module does not establish ordinary-game equivalence or model strength.

```python
from battle_env import MoveAction
from battle_status import StatusFight
fight = StatusFight(rom, sym, source_root, base_build, status_root / 'leaf',
                    seed=193, orientation=0, poisoned_side=0)
while not fight.terminal and not fight.truncated:
    fight.step((MoveAction(0), MoveAction(1)))
```

The terminal library is loaded from the sibling `terminal` directory. Each
library independently uses `load_verified`, exact entry/boundary configuration,
source identities and the existing full-ROM headless audit. Base engine/profile,
stateful lifter, policy and RNG modules are unchanged.

Reproduce with serial compilation, one numerical thread and no GPU:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B \
  tools/trainer_ai/battle_status_profile.py \
  --rom /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game/polishedcrystal-3.2.3.gbc \
  --sym /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game/polishedcrystal-3.2.3.sym \
  --source-root /Volumes/Storage/Development/agentic_projects/polished_ai/build/stateful/game \
  --build-dir /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-status
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B \
  tools/trainer_ai/battle_status_check.py \
  --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai
```

The checker reuses the baseline native library and mGBA oracle built by
`battle_env_check.py`. It checks full registers, SP/PC, bank, supported WRAM and
HRAM at each actual call. At terminal events it compares the native boundary
state with mGBA stopped at that exact address, without a fabricated return.
Routine-only probes also include both poisoned actors and HP edge inputs; those
raw validation fixtures are not accepted by the public fight constructor.

Burn executes the source damage modifier inside actual `DoTurn`; special moves
retain their source behavior. Toxic initializes PSN and TOX together, increments
the actual counter, and caps it through source code at 15. `observation()` adds
the current `toxic_count` for each actor because it affects future residuals.
The public constructor does not accept an arbitrary initial Toxic counter.

The poison-only evidence and executed sources are preserved under
`subagents/status-slice-01/poison-r1`. That report is historical evidence for its
exact source revision. The separate bounded continuation checker adds Burn and
Toxic branch evidence and a small three-mode fight regression without claiming a
fresh rerun of the complete original poison matrix:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B \
  tools/trainer_ai/battle_status_extended_check.py \
  --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai
```

`extended-report.json` records this continuation separately.
