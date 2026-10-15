# Nested actor privacy at actual pivot input

Six complete actual Baton Pass fights passed: 78 turns, 8,747 actual-ROM
segment comparisons and exact recorded native replay. Two hidden opposing
move slots are paired with a faster player pivot
and a faster enemy pivot. The first replacement actor observation and mask
must match across each pair, with identical initial WRAM. Actual source
EncoreCount writes must witness the differing pending slots before the slower
actor spends PP. Source must switch to the requested reserve and reach terminal.

A third pair first executes ordinary Tackle actions to establish nonzero frozen
locks, then has both actors use Baton Pass. The slower replacement callback
must observe the actual new opposite party participant with a zero projected
high nibble, instead of inheriting the former active member's frozen lock.
The slower actor uses duplicate Baton Pass slots for the paired secret choice.
Every callback checks all other fresh observation fields against live queries,
including own lock and opposing low duration. Detached input mutation must leave
live RAM and pending intents unchanged. No callbacks or source mechanics are
simulated by the check.

The shared controller helper also covers fresh bag retry, outer retry and
Conversion input. Existing Conversion three-fight and bag cancel paired-input
regressions are required after the candidate freezes. Existing projection limits
remain conditional; this bounded proof does not establish arbitrary actor
noninterference or all presentation behavior.

Run only after the coordinator allocates the sole CPU slot:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/battle_controller_pivot_privacy_check.py --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/pivot-privacy
```

The report retains source identities, ROM/SYM/manifest hashes, actually loaded
Oracle provenance, initial WRAM and per-fight recorded comparisons. No game
assembly, profile, Oracle, scenario initializer or core changes are needed.

Evidence: `/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/pivot-privacy/report.json`.
The affected three Conversion fights also passed 2,610 comparisons and exact
replay with the same controller hash, including hidden-input equality and
missing/invalid-policy poisoning/reset guards. Their report is at
`/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/pivot-privacy-conversion/report.json`.
All four focused bag retry fights passed another 3,551 comparisons and exact
replay, including the paired hidden-action cancel inputs and negative/reset
guards. Report:
`/Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/pivot-privacy-bag-retry/report.json`.
