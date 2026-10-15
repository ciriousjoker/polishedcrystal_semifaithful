# Experimental IID-DIV/no-frame whole-fight learner

Completed one bounded run: 256 terminal training fights, 160 terminal heldout
fights, exact checkpoint/resume check, and final learned-model SM83 oracle guard.
There were no unsupported or truncated outcomes and no hyperparameter search.
The trained export won 22/32 against initial. Eight private event seeds are the
paired scenario units, with both stat orientations and actor roles within each
cluster; descriptive score-bootstrap interval [0.5,0.875]. This small result is
not statistical proof of improvement or ordinary-game strength. Execution is
finished and all numeric Python sources are frozen.

The required profile is `experimental-headless-iid-div-v1`. It describes synthetic
IID private DIV bytes without frame/VBlank RNG advancement. Omitted VBlank changes
RNG counters. This is not ordinary-game faithful and cannot justify ordinary-game
quality, convergence, or trained strength. Architecture review permits only
bounded learner plumbing under this explicit profile.

The sole environment is the new restricted `battle_env.NativeFight`: one Own
Tempo Smeargle per side, Tackle and Water Gun, two asymmetric stat orientations,
no additional mechanics. There is no old synthetic simulator fallback. Construct
one verified NativeFight at startup and call its provided `reset(seed,orientation)`
per fight. The reset reuses verified frozen inputs. The coordinator must freeze
source, ROM, symbols, and native build throughout execution and instantiate a new
engine after any input change.

## Executed protocol

The provisional network is 16 x 16 x 12. Features are own and opponent HP
fractions, own Attack/Defense/Speed/SpAtk/SpDef, opponent equivalents, then own
and opponent PP for slots 0 and 1. Stats divide by 255; PP divides by 63; HP by
MaxHP. Multiply each ratio by 15, floor, clip to [0,15]. Only outputs 0 and 1
are legal. Capture both observations and legal masks before committing either
action. Features exclude private RNG, opposing current actions, transition
damage, and future state. There is no empirical ablation claim.

Sampling uses masked softmax of exact quantized logits at temperature 16.
REINFORCE assigns each actor only final full-fight outcome +1 or -1. Unsupported
and truncated attempts are separate, produce no update, and never count as wins.
Latent floating parameters and SGD momentum 0.9 use an explicitly approximate
straight-through gradient. Forward inference is exact integer. Weights round
with floor(x+0.5), clip to signed int4; biases clip to +/-16384. Hidden activation
is floor shifted by 3 and clips to 0..15. Accumulators fit int16 at this width.
TAI4 exports use the existing writer and are decoded back for all evaluation.
The executed reference check compared separate integer inference with existing
`network_runtime_check.reference` over inputs, masks, and ties. A separate final
artifact check also executed the learned export on the real mGBA SM83 oracle.

At most 256 fight attempts, no GPU or parallel workers. NumPy/BLAS thread
variables are set to 1 before NumPy import. Only current trajectories, small
checkpoints, and bounded outcome records are stored. No persistent dataset.

Checkpoints contain source/profile/schema/ROM/symbol/native-manifest identities,
model, optimizer momentum, sampler RNG, next fight index, initial model, and
outcomes. Resume requires identical identities/protocol. `--resume-check` compares
one next attempt from memory against the serialized checkpoint, including all
RNG/optimizer/result/model state. It costs two fights; inspect outcomes because
an unsupported/truncated next attempt does not verify a gradient update.

Frozen exported integer argmax evaluates heldout odd event seeds, disjoint from
even train seeds. Every heldout seed runs both stat orientations and policy sides.
Initial and trained face fixed-slot-0 and fixed-slot-1 scripts; trained also faces
initial. Script provenance stays script provenance, with no invented model hash.
Raw results retain unsupported/truncated outcomes. The Elo-scale fit below is
limited to this synthetic experimental pool; no ordinary-game Elo or strength claim.

## Reproduction commands

Use the bundled Python 3.12 with NumPy. Supply verified artifacts and the worker's
external `/Volumes/Storage/Development/agentic_projects/polished_ai/build/policy-pilot` output area. The
original execution received the sole CPU slot after independent fight review;
any future heavy run needs resource orchestration, not a product approval file:

```sh
PY=/Users/ciriousjoker/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3
"$PY" tools/trainer_ai/policy_pilot.py \
  --profile experimental-headless-iid-div-v1 \
  --rom ROM --symbols SYM --source-root SOURCE --build-dir NATIVE_BUILD \
  --output PILOT_OUTPUT --identity-only
```

Remove `--identity-only`, add `--resume-check --stop-after 8` for the first bounded
checkpoint. Continue with the same parameters and
`--resume PILOT_OUTPUT/checkpoint.json`, removing `--stop-after` and
`--resume-check`. Identity changes intentionally invalidate resume. Existing
network ROM oracle checks remain separate from the pilot reference check.

## Final exported artifact SM83 guard

Evaluation retains at most eight public observations (first two turns, both
orientations and sides), their encoded inputs and actual masks. After a completed
pilot, the single learned 312-byte model was checked by the existing real
mGBA SM83 oracle and runtime, compiled only at 16x16x12. The exact exported file
is INCBIN-linked into that ROM. This runs actual masks plus the empty-mask return
ABI, comparing hidden values, logits, action and stack with the decoded export
and existing reference. No new emulator or broad multi-shape suite is introduced.
Any unknown/runtime failure prevents reporting the artifact as validated.

```sh
"$PY" tools/trainer_ai/policy_pilot_sm83_check.py \
  --model PILOT_OUTPUT/trained.tai4 --evaluation PILOT_OUTPUT/evaluation.json \
  --build-dir PILOT_OUTPUT/sm83-final --mgba-prefix MGBA_PREFIX
```

This guard completed successfully with 16 cases over eight actual observations.
No further training, evaluation, or SM83 rerun followed the ratings-only cluster
correction. Any future guard run also needs the sole CPU slot.

## Completed bounded plumbing run

The single seed-7123 run completed 256 terminal training fights and 160 terminal
heldout fights, without unsupported or truncated outcomes. The 8-fight checkpoint
and serialized-next-update check passed before continuing the same run to 256.
No hyperparameter search followed. Both 312-byte exports passed reference checks;
the final learned export passed 16 real mGBA SM83 cases over eight actual heldout
observations, including empty legal masks. This establishes this export plumbing,
not ordinary-game fidelity or general strength. The exact learned TAI4 SHA256 is
`2b0e3027dbde05d1802d0ecd61d02e62fd9874f7b1739fea518e29d3f59708b3`.
Executed source hashes remain in checkpoint.json/evaluation.json, and the SM83
source/artifact hashes remain in sm83-final/results.json. Prose corrections do
not relabel those source identities or imply a fresh numeric run.

`policy_pilot_ratings.py --directory PILOT_OUTPUT` builds the evaluation dataset
for exported initial versus trained only. Rules provenance includes ROM, symbols,
native manifest, environment/profile sources, and synthetic event law. Suite and
protocol hashes are distinct. Each scenario cell groups one private event seed,
including both stat orientations and both actor roles (four paired outcomes).
Initial is anchored at 1000. Script
references remain plain winrates. Exclude incomplete scenario cells rather than
convert missing outcomes into draws.

The completed run observed trained wins in 22/32 initial-versus-trained fights,
yielding an experimental, regularized Elo-scale estimate 1088.086 versus initial
1000. The paired units are eight unique private event seeds, with both orientations
and actor roles retained within each cluster. The reported cluster bootstrap
score interval [0.5,0.875] is descriptive; 22/32 is not statistical proof of
improvement and does not establish ordinary-game strength or robust convergence. Fixed-script winrates are in the retained report.
