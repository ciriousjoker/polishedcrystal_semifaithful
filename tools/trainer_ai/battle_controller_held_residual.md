# Actual held residual family

All twelve distinct fights passed: 76 source turns, 7,436 independent Oracle
comparisons, and identical native whole-fight event replays. The six cases ran
on both holder sides with seed 193, with no repeated accepted cases or fixture
repairs. The family covers Flame Orb and
Toxic Orb timing, Fire-type Flame Orb immunity, Poison-type Toxic Orb immunity,
and Black Sludge healing for Poison types versus harm for other types.

The healthy lead and preconditioned holder reserve follow the proved held
restorative setup. The first actual action switches in the holder while its
opponent uses Splash. Both sides then use Splash for two more rounds. The
three observed source boundaries must show:

| Case | Switch round | Next round | Third round |
| --- | --- | --- | --- |
| Flame Orb, Horsea | Burn applied, HP unchanged | MaxHP/16 damage | MaxHP/16 damage |
| Toxic Orb, Horsea | Toxic poison applied, HP unchanged | MaxHP/16 damage | 2 times MaxHP/16 damage |
| Flame Orb, plain Ponyta | No status or HP change | No change | No change |
| Toxic Orb, Tentacool | No status or HP change | No change | No change |
| Black Sludge, Tentacool | MaxHP/16 healing | MaxHP/16 healing | MaxHP/16 healing |
| Black Sludge, Horsea | MaxHP/8 damage | MaxHP/8 damage | MaxHP/8 damage |

Each division floors with a source minimum of one. Healing caps at source
MaxHP. Black Sludge's healing holder starts at ten HP; the other holders start
healthy. The actual source implements the effects. These arithmetic checks are
positive witnesses at source boundaries, not host-side battle simulation.

`HandleBetweenTurnEffects` processes poison and burn before
`HandleEndturnBlockB` applies status orbs. The switch round must therefore show
status application without premature residual damage. Toxic progression must
advance from the first tick to twice that damage on the third round; an orb
reapplication that incorrectly resets toxic progression cannot silently pass.
`HandleEndturnBlockA` runs Black Sludge's shared `HandleLeftovers` handler before
that round's poison and burn logic. No fixture patch occurs after reset.

Horsea must resolve to its source Swift Swim ability and Water typing. Plain
Ponyta must resolve to Run Away and Fire typing. Tentacool must resolve to Clear
Body and Water/Poison typing. The driver checks actual runtime ability and type
bytes at every probe boundary. It does not count inactive faithful build
branches as active evidence. Item identifiers and held effect/parameter bytes
are bound to source constants and assembled item attributes through the
existing guarded decoder. All three source attribute parameters must be zero;
the relevant denominators and ordering belong to source routines, not that
parameter byte.

Every item must remain held through all probes. The active and party
cache must agree on item, HP, status and PP at every probe boundary, with each
cache snapshot retained in the witness. The reserve must preserve its
observed initial ability/item/status condition before switching. Holder Splash
PP must decrease once in each subsequent probe, and moves and PP Ups must stay
fixed. Other party members preserve item, HP and status during every isolated
probe interval. Afterwards both sides select a legal attacking move through
the actual action mask until the ordinary source trainer terminal. Water Gun
is the attack for Horsea and Tentacool; Ponyta uses Ember. Splash is an explicit
engine probe, not a claim about each species' natural learnset.

The existing controller retains fixed levels, no experience, full-roster
observations, and secrecy of simultaneous opposing actions and future random
events. No adapter, core, profile, generator or Oracle change is made by this
family. Unknown source revisions remain subject to existing manifest guards.
No item table entry or enum automatically promotes a coverage row.

Every source segment must pass independent Oracle CPU, all-bank WRAM, HRAM and
SVBK comparisons. Banked Oracle mode is required. RecordedRun retains actual
segment event tapes and normalized state digests for an exact native whole-fight
replay, including the same three-round witness, terminal, turn count and winner.
The reports retain actual Oracle C/library/libmGBA/State/Hook provenance,
executed host/source hashes, ROM/SYM/native manifest identities, options,
controller audit, initial mapped WRAM path/hash after reset and before the first
action, and complete segment records. The WRAM diagnostic file contains mapped
C000-DFFF; all-bank proof comes from the Oracle segment comparisons. All sixteen executed
host dependency files were copied and hash-verified against the unchanged first
report into `held-residual-first/host-source-checkpoint`. Its `identity.json`
binds every snapshot and the originating report hash.

The actual first checkpoint used these three side-zero cases:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/battle_controller_held_residual.py --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/held-residual-first --cases flame toxic sludge-other --sides 0 --seeds 193
```

After that checkpoint passed unchanged, the remaining three player cases and
all six enemy cases ran in separate output directories:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/battle_controller_held_residual.py --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/held-residual-player-rest --cases flame-fire toxic-poison sludge-poison --sides 0 --seeds 193
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/battle_controller_held_residual.py --artifact-root /Volumes/Storage/Development/agentic_projects/polished_ai --output /Volumes/Storage/Development/agentic_projects/polished_ai/build/battle-controller/held-residual-enemy --sides 1 --seeds 193
```

The three retained aggregate reports under `<run>/build/battle-controller` are:

| Report | Fights | Turns | Comparisons |
| --- | ---: | ---: | ---: |
| `held-residual-first/report.json` | 3 | 18 | 1,883 |
| `held-residual-player-rest/report.json` | 3 | 20 | 1,914 |
| `held-residual-enemy/report.json` | 6 | 38 | 3,639 |

Both sides showed identical isolated item transitions. Flame Orb applied burn at
90 HP, then HP fell to 85 and 80. Toxic Orb applied toxic poison at 90 HP, then
HP fell to 85 and 75, confirming the second tick doubled. Black Sludge healed
Tentacool from 10 to 16, 22 and 28 HP, while Horsea lost HP from 90 to 79, 68 and
57. Fire Ponyta remained at 110 HP without status and Poison Tentacool remained
at 100 HP without status across all three immunity probes. Every held item
remained unconsumed, and all per-boundary party/active cache checks passed.

The task's external `artifacts/summary.json` verifies twelve unique case keys,
matching host/source/native/oracle identities across reports, and hashes of
every referenced per-case evidence and initial WRAM file. Original reports are
unchanged. Future per-case JSON written before replay remains provisional until
a successful aggregate report confirms it. Blocked activation, type/ability
mismatch, differential failure, truncation or replay divergence fails the
batch; failure output records `passed: false`, `status: blocked`.

Corrosion, ability immunity combinations, existing status interactions, theft,
held-item faint boundaries and other residual items are outside this batch.
Presentation/input projections retain their established conditional status.
No automatic ledger promotion or broad M2 completion follows from this family.
