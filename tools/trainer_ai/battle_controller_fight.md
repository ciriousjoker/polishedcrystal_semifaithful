# Typed source controller

`ControllerFight` adapts a verified `ControllerMachine` to complete battle
episodes. It prepares the actual `DoBattle` entry, executes automatic turn
preparation, freezes both actor observations and action masks before either
actor chooses, and runs the actual source controller to the next decision or
terminal boundary. It does not
schedule moves, apply damage, consume inventory, or resolve faints in Python.

The user selected fixed levels and no XP simulation. The adapter requires the
actual source `NO_EXP_OPT`, ordinary trainer mode, no link mode, and no Battle
Tower mode. Stored levels and XP must remain unchanged from scenario
initialization. Stored MaxHP, Attack, Defense, Speed, SpAtk and SpDef remain
fixed after actual `DoBattle` initialization. That boundary allows the real
initial `FixPlayerEVsAndStats` routine to normalize supplied EV records before
freezing permanent stats. Both baselines and their source boundaries are
retained in diagnostic replay metadata. Temporary combat stat changes, including
Transform and Power Trick, remain actual game mechanics. XP is absent from
actor observations.

Actions are typed move, switch, trainer item, player bag, locked continuation,
and Struggle intents. Actor masks query assembled source routines. Trainer item
selection replaces AI desirability only; the actual item handler and actual
`AI_TryItem.used_item` caller commit effects and slot consumption. Bag effects
and retry behavior remain in actual source menus and item handlers. An intent
that reopens the battle menu without taking a turn invokes an explicit
`retry_chooser` for the player only. The submitted enemy intent is retained and
hidden. Retry choices are source-mask validated, bounded and recorded. Missing
retry policy rejects instead of inventing an action. Trainer X item failure
currently also rejects rather than silently substituting a move.

Replacement input takes fresh detached snapshots for both actors at every
actual request. Pending targets and current enemy decisions are absent from
those snapshots. There is no whole-turn replacement cache that could survive a
later faint or repeated entry-hazard KO. A separate explicit replacement policy
chooses living reserves. Source callers still decide when and why to replace.

Actor timing is at the actual `UpdateBattleMonInParty` call immediately after
`HandleBerserkGene` and `CheckMirrorHerb` in `BattleTurn`. The adapter derives the
exact banked callsite by matching both source scaffold and symbol-resolved ROM
operands. Changed or ambiguous scaffolds reject for review. No source assembly
or automatic held-item effects are replaced. After each completed round, source
preparation for the next round runs before its observations and masks freeze.
This prevents accepted item actions from becoming illegal due to a held-item
effect that should already have happened. The exact stop contract is retained
in replay metadata, including the initial preparation's recorded DIV events.
Unknown menu
owners, rules, source faults, and unavailable intents reject. Turn limits
truncate with no winner. The adapter has no successful unsupported fallback.

Optional observer callbacks receive detached diagnostics. They are not actor
features and do not prove individual move effects from whole-turn differences.
Replay records retain source identities, options, seeds, typed actions, actual
requested replacements, consumed DIV events, source boundary names, and final
state digests. The serial check runner first compares each native segment
against the independent mGBA oracle, retains the actual DIV tapes, then replays
those tapes natively and compares every source segment and terminal result.
Fresh hardware divider values are not assumed reproducible from a seed. These checks
must be run before support claims. Same-hook checks alone do not prove that
omitted presentation code has no later gameplay effect.

The scenario initializer accepts optional starting PP for each occupied move
slot, plus female and egg flags encoded through reviewed source masks. Omitted
PP uses the actual assembled base PP. Explicit PP is bounded by that base value;
PP Ups are not configured by these fixtures. All changes apply only before the
source fight starts. Existing default teams, options and initialization calls
remain the defaults.

Targeted serial cases include a two-turn Fly continuation, both actors starting
with exhausted PP, opposite-gender Attract, and two six-member teams that first
switch to slot six and then fight through replacement exhaustion. These cases
reject if their intended condition never appears. Activation observations are
fixture checks, not independent proof of all interactions or local effect
attribution. Each case still requires an actual terminal battle, independent
ROM comparisons and recorded-event replay before it counts as executed evidence.

Example after acquiring the sole heavy CPU slot and building the native profile:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/battle_controller_fight_check.py \
  --artifact-root /Volumes/Storage/tmp/polished-ai \
  --output /Volumes/Storage/tmp/polished-ai/build/battle-controller/typed-fights \
  --cases neutral switch trainer-item player-bag --seeds 193
```

The additional bounded cases can be requested with `--cases forced-charge
struggle attract six-party`. They are prepared fixtures until their runner has
actually passed against the current native artifact and ROM.
