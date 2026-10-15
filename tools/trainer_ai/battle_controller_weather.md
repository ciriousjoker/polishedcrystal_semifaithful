# Source weather duration, residuals and entry abilities

Status: **20/20 complete source fights passed**, 187 rounds and 13,606 independent CPU/all-bank WRAM/HRAM/SVBK comparisons, followed by exact native event/state replay. Execution used the coordinator's exclusive CPU grant. No game, engine or runtime profile changes were needed.

`battle_controller_weather.py` adds ten bounded cases, each in both setter orientations, with seed 193 by default. The maximum invocation is twenty complete source-controller fights. Existing engine, Oracle, scenarios, profile, observer and coverage files remain frozen. This is a category evidence runner, not a replacement battle engine or a coverage promotion.

| Case | Source creation | Fixture | Isolated witness |
| --- | --- | --- | --- |
| rain-move | Rain Dance | Horsea, Swift Swim | Counter expiry, no HP residual |
| sun-move | Sunny Day | Horsea, Swift Swim | Counter expiry, no HP residual |
| sand-move | Sandstorm | Horsea, Swift Swim | Positive residual on both ordinary Water actors |
| hail-move | Hail | Horsea, Swift Swim | Positive residual on both ordinary Water actors |
| drizzle | Actual reserve switch | Politoed, slot 2 | Rain creation and duration |
| drought | Actual reserve switch | Plain Ninetales, slot 2 | Sun creation and duration |
| sand-stream | Actual reserve switch | Tyranitar, slot 0 | Rock immunity and opposing positive sand damage |
| snow-warning | Actual reserve switch | Glaceon, slot 1 | Ice immunity and opposing positive hail damage |
| damp-rock | Rain Dance holding Damp Rock | Horsea, Swift Swim | Actual item duration 8 versus ordinary rain duration 5 |
| cloud-nine | Sandstorm opposite Cloud Nine | Horsea opposite Psyduck, slot 1 | Raw weather persists and expires while both Water actors avoid residual |

The active default build has no Abomasnow. Glaceon's Snow Warning is slot 1 in the non-FAITHFUL branch; its FAITHFUL branch instead gives Snow Cloak. Psyduck's Cloud Nine is slot 1. Politoed is Water/Grass and plain Ninetales is Fire/Ghost in this build. The driver reads active source declarations using the existing default-build conditional reader, reads linked BaseData record widths and field offsets, and checks compiled type and selected ability bytes. Every active observed type and ability must then match those values. No inactive branch is accepted as fixture evidence.

## Source order and policy

`engine/battle/move_effects/weather.asm` sets `wBattleWeather` and obtains its duration from `GetItemBoostedDuration`. The default duration is 5; Damp Rock's compiled item effect is `HELD_PROLONG_RAIN`, parameter 8. `engine/battle/abilities.asm` dispatches Drizzle, Drought, Sand Stream and Snow Warning through these same actual weather commands.

`HandleWeather` in `engine/battle/endturn.asm` decrements the counter before weather effects. When the counter reaches zero it clears weather and returns before residuals. Ordinary five-turn weather therefore has four damaging end-turn intervals, not five, in this source. Creation-round observation is count 4, followed by 3, 2, 1 and zero; the sixth probe round remains clear. Damp Rock observations are 7 through zero plus a ninth clear round. The runner asserts this exact sequence and fails if actual behavior differs. It does not change the game to fit another generation's rules.

Sandstorm exempts Ground, Rock and Steel types; Hail exempts Ice. Damage is the source sixteenth-MaxHP amount, rounded down with a minimum of one. Both source HP and party/active caches must agree each round. Rain and sun actors retain full HP. Cloud Nine is checked after timer handling, so raw sand and its timer remain observable while residuals are suppressed. Separate ordinary sand cases provide the positive damage comparison.

All actors use Splash during the probe after creation or the requested actual reserve switch. There is no incoming damage confound. Only after expiry and the next clear round do both actors select their legal Water Gun slot or the existing deterministic legal fallback until a real source terminal. Fixed-level and permanent-stat invariants remain enforced by `ControllerFight`. The policy reads the ordinary frozen observation and legal actions, never the opponent's secret submitted action or future RNG. There are no post-reset RAM writes.

## Retained evidence

Each fight retains a standard standalone 8192-byte initial WRAM file, path, hash and post-reset decision boundary, plus memory for all eight WRAM banks, HRAM and SVBK, its hash and path, the actual initial controller boundary, each weather/counter/HP/type/ability transition, source and host dependency snapshots, and Oracle identity for its C source, loaded dylib, actual loaded mGBA dependency, State and Hook ABI sources. Native generated C, loaded library, manifest, ROM and symbols are hash checked and snapshotted. The existing assembler receipt is retained and must match the ROM/symbols when available.

`Oracle` compares independent source CPU execution, all WRAM banks, HRAM and SVBK. `RecordedRun` retains actual DIV events and exact normalized native state per executed segment. The second run replays every segment and verifies identical initial memory, normalized initial CPU/event state, probe witnesses and the complete controller record, including terminal, choices, fixed-party values and boundaries. The raw initial State hash is retained diagnostically; equality uses the existing RecordedRun normalization of unconsumed event-tape capacity. A mismatch fails closed and emits failure evidence. Source/hardware faults require coordinator review; this driver never expands the runtime profile.

The script snapshots an explicit allowlist of its actual project host execution dependencies, including the imported profile and party/status modules, then verifies those identities again at completion. Oracle ABI/native dependency paths are retained separately; unrelated writers and the standard library tree are excluded. Python version and executable path are recorded. Inputs must remain frozen until evidence is retained and reviewed.

## Bounded invocations after a CPU grant

From the selected worktree, the first feedback batch is three fights:

```sh
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 MKL_NUM_THREADS=1 \
python3 tools/trainer_ai/battle_controller_weather.py \
  --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai \
  --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/weather-feedback \
  --cases rain-move sand-stream snow-warning --sides 0 --seeds 193
```

After reviewing feedback, use separate serialized groups. The whole family can be run as one twenty-fight batch if the first feedback supports it:

```sh
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 MKL_NUM_THREADS=1 \
python3 tools/trainer_ai/battle_controller_weather.py \
  --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai \
  --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/weather \
  --seeds 193
```

The script requires the current verified native build, default `build/battle-controller/native-o3`, without modifying it. No ROM build is requested. The Oracle constructor retains the existing repository behavior of compiling its C library if absent; the coordinator must confirm it is already available before granting this batch when builds remain prohibited.

## Open limits

The ten listed cases passed in both orientations with seed 193. Prefix compatibility and source terminal success are evidenced for these cases. Unhooked weather animations/presentation fidelity remain unverified. Declared presentation/input projections and previously accepted ability graphics boundaries remain conditional. This does not claim unhooked rendering proof.

Coverage is limited to these weather durations, isolated residuals, Rock/Ice type immunity and the listed Cloud Nine control. Damage/stat modifiers, other weather abilities, weather replacement, all three sand-immune types, Safety Goggles, Utility Umbrella, overworld/permanent weather, item theft or suppression, faint-triggered weather interactions and weather ability collisions remain outside this family. This closes no M2 gate on its own and promotes no central coverage rows automatically.

## Accepted execution evidence

Three serialized batches passed on the unchanged `dec3cb17a34aeeb9e0f36e44590d0890b7d0816ddff98fc7bb0346b9b32737b0` native manifest:

- `/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/weather-feedback/report.json`: first three cases.
- `/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/weather-remainder14/report.json`: fourteen remaining move, ability, Damp Rock and Cloud Nine cases.
- `/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/weather-remainder3/report.json`: mirrored first three cases.

`/Volumes/Storage/Development/agentic_projects/polished_ai/subagents/m2-weather-01/artifacts/summary.json` binds the reports and lists all twenty unique `(case, setter, seed)` keys. Aggregation independently verified every case evidence hash and every standalone initial-WRAM file's 8192-byte size and hash. Each executed report includes the conventional `host_sources` and `game_sources` mappings alongside retained input snapshots. Python driver bytes remain exactly those snapshotted by the executed reports.
