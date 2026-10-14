# Whole-fight policy comparison scaffold

This standard-library tool compares recorded whole-fight wins, draws and losses. It does not run battles or establish simulator fidelity. All retained checks use synthetic outcomes. There are no trained-model league results yet.

Run:

```sh
python3 tools/trainer_ai/ratings.py outcomes.json --output ratings.json
python3 tools/trainer_ai/ratings_check.py
```

## Input contract

The JSON root contains:

- `dataset_kind`: `synthetic` for fixtures or `evaluation` for actual recorded battles.
- `provenance`: `rules_sha256`, `suite_sha256`, `protocol_sha256` and `policy_format: "exported-integer"`. Each hash is 64 lowercase hexadecimal characters. Hash the battle rules and generated simulator manifest, complete held-out suite, and evaluation protocol respectively.
- `models`: object keyed by stable model IDs. Each value has `policy_sha256` for the exported integer policy artifact and a nonempty `checkpoint` identifier. Additional metadata such as architecture, training seed, training data lineage and baseline implementation identity is preserved.
- `reference`: a model ID naming a frozen reference baseline, anchored at 1000.
- `matches`: list with `a`, `b`, `result` (`a_win`, `draw`, `b_win`), `scenario_id`, `provenance`, `policy_a_sha256`, `policy_b_sha256`. Results describe completed fights. `a` is the physical first side, so exchanging sides requires exchanging policy hashes and reversing wins/losses.

Every match must repeat exactly the root provenance and the appropriate immutable policy hashes. Mixed rules, suites, protocols and model revisions are rejected. These are integrity checks on supplied metadata, not verification of the claimed content hashes or suite membership. The eventual evaluator must generate and verify manifests and emit these records itself. Changing any provenance field starts a separate comparison report. Include timeout and invalid-action adjudication in the protocol; never silently drop losses, failures or incomplete fights.

Use one `scenario_id` for all seeds, repeats, side swaps and roster swaps derived from the same underlying held-out scenario. Repeated seed identifiers alone are unsuitable cluster IDs. A suite hash binds the exact roster/state/seed manifest, not only the suite name. The collector must reject duplicate fight IDs and verify the planned schedule before analysis; this scaffold deliberately permits repeats and prevents them from masquerading as independent scenario evidence.

## Fair evaluation protocol still required

Freeze checkpoints before evaluation. Evaluate the exact exported integer policy and action mapping intended for the game, including feature quantization, legality and tie breaking. Floating-point training-network performance does not establish exported-policy performance. Use a held-out full-fight scenario suite separate from training, feature selection, tuning and checkpoint selection. If selecting models with this suite, retain another untouched final suite.

For every scheduled model pair and scenario, balance both physical sides, assignments of the two rosters, and corresponding seeds. Record terminal outcomes for the whole fight. Side and roster assignment are separate dimensions. `both_a_b_orientations` is only an observed orientation count; it does not prove roster balance, completed schedule or evaluator correctness. The protocol must specify team distribution, starting states, matchup weights, seed generation, draw rules and paired RNG handling. Scenario repeats can reduce Monte Carlo noise but do not add new scenario coverage.

Include frozen scripted baselines, the current game AI and previous accepted exported policies where supported by the same faithful evaluator. A fixed baseline's identity and implementation belong in its immutable artifact manifest. Self-play-only ratings move when the pool changes and cannot prove progress. Compare fixed checkpoints against the same reference and suite. A reference gives a scale anchor, not a universal absolute measure of strength.

Full team knowledge must not expose the opponent's secret current action or future RNG. No rating result compensates for leaked observations or missing battle mechanics.

## Fit and uncertainty

The fit uses batch Bradley-Terry logistic likelihood on an Elo scale: `P(a scores) = 1 / (1 + 10 ** ((rating_b - rating_a) / 400))`. Draws contribute half a score, rather than fitting a separate draw-probability model. Matches are canonicalized by model ID; each pair/scenario cell contributes its mean score with weight one. Sorting and deterministic coordinate Newton updates make fit results invariant to record order and orientation. The fit uses a unit Gaussian prior on each non-anchor logit relative to its component anchor. This prior keeps all-win separation finite, and shrinks sparse results strongly. Finite ratings are not proof of finite statistical uncertainty. The solver fails explicitly on nonconvergence.

Disconnected pools are labeled and get independent local anchors at 1000. Their ratings cannot be compared across components. A model with no outcomes has a null rating. Adding opponents changes the likelihood and therefore the ratings even within one connected pool. Coverage includes raw fight counts, pair/scenario cells, unique scenario clusters, opponents, pair scores and fitted pair scores. Compare observed and fitted pair scores to find poor scalar fits and cyclic matchups such as A beating B, B beating C, C beating A. Preserve that matchup matrix when a scalar ranking hides a strategically important cycle.

The reported interval is a deterministic percentile bootstrap over per-model scenario-average scores, with 500 resamples and seed 0. Within a scenario it averages the model's pair-cell scores equally; the fit instead assigns one unit per pair/scenario. The interval describes observed score against that opponent pool, not an Elo posterior or a universal win probability. It assumes independent scenario clusters, while retaining within-scenario dependence. Correlated scenarios need coarser cluster IDs or a more suitable analysis. Fewer than two clusters returns null. Small suites, repeated identical outcomes and all-win samples can produce deceptively narrow or collapsed intervals; `boundary_scores_only` flags the all-win/all-loss boundary case. Report more coverage and held-out outcomes before drawing strength conclusions. No minimum credible suite size is established by these synthetic tests.

## Missing evaluator

The current scaffold is ready to consume provenance-bound results, but a faithful whole-battle native simulator, complete policy adapters, scenario generator and scheduler, manifest verification, source-update parity gates, and real exported-policy runs remain required. A hand-written `evaluation` label does not prove those requirements. Do not train or publish strength claims from unsupported mechanics or numerical inference fixtures. The synthetic checks establish comparison-tool behavior only.
