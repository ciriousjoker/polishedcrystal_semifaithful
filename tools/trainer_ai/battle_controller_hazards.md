# Source hazard entry family

`battle_controller_hazards.py` prepares nine related full-fight conditions in
both actor orientations. All 18 cases have now passed: 117 source rounds, 11,946 exact banked
reference comparisons and identical recorded native whole-fight replays. They
used the existing O3 artifact without recompilation or changes to the core,
profile or host controller. CPU execution remained serial.

The setup is deliberately narrow. One Pokemon actually uses Spikes or Toxic
Spikes while the other uses Splash. After the requested number of layers, the
setter uses Splash and the opponent submits a real voluntary switch. Thus no
incoming attack, held item or seeded status can explain the entry HP/status
change. Source execution supplies every effect. There are no midfight RAM
patches or host-written hazard/status/HP effects. Both policies then use attacks
until the ordinary source controller reaches its terminal.

The cases are:

- Spikes with one, two and three layers, requiring the actual source amounts.
- One and two Toxic Spikes layers, requiring poison versus bad poison and
  actual residual HP loss after the entry turn.
- Flying entrants for both hazard types, preserving HP/status and hazard layers.
- A grounded Poison entrant, actually absorbing both Toxic Spikes layers.
- A one-HP Spikes entrant, fainting on entry and requesting a real replacement;
  that new entrant must also receive its own Spikes damage.

Placement must change the correct side's field by exactly one layer without
changing either active Pokemon's HP/status. Entry checks bind the requested
party index to the entrant's before/after HP/status and surviving field state.
The entry-KO case additionally records the actual source replacement callback
and requires its selected reserve to become active. These witnesses support
only these activated conditions; they do not establish every hazard interaction
or all branches of the underlying move handlers.

Each fight uses the existing independent banked mGBA comparison and retained
source-event recording, followed by a native whole-fight replay. Initial WRAM,
source identities, precise witnesses and fight evidence are retained per case.
The runner fails closed on wrong activation, source failure, truncation or
replay mismatch. It does not edit the coverage ledger or relax its gates.
Presentation/input projections remain conditional exactly as in the controller.

The first discriminating execution was one grounded Spikes case. After it
passed, the full nine-condition, both-side family passed with this command:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/battle_controller_hazards.py --artifact-root /Volumes/Storage/tmp/polished-ai --output /Volumes/Storage/tmp/polished-ai/build/battle-controller/hazards --seeds 193
```

Default execution selects all nine conditions, both setter sides and one seed.
The aggregate report is `<run>/build/battle-controller/hazards/report.json`;
individual records retain initial WRAM, semantic witnesses and source identities.
This is one seed per condition, not a statistical confidence bound. Immunity and
absorption controls are observed during the isolated entry interval; subsequent
combat is allowed to apply its ordinary effects. Existing fixed-level and
permanent-stat guards remain active throughout every complete fight.
No universal observer or new game engine is introduced.
