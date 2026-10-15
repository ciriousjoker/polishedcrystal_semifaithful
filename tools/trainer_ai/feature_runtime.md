# Provisional on-device feature extraction

This prototype adds exact SM83 feature arithmetic before the existing unchanged
16 x 16 x 12 integer network and the exact 312-byte learned pilot model. It does
not install a trainer hook or broaden the training fixture. Compilation and
execution must run serially with the project's sole CPU slot.

The feature order and integer scaling are exactly `policy_pilot.features`:

1. Own HP/MaxHP and opponent HP/MaxHP.
2. Own Attack, Defense, Speed, SpAtk and SpDef divided by 255.
3. The same five opponent stats divided by 255.
4. The first two own PP values and first two opponent PP values divided by 63.

Each output is `min(15, floor(15 * numerator / denominator))`. Stats and HP are
read as full unsigned big-endian 16-bit values. PP uses the lower six bits, as
the existing native observation and mask already do. The fractional arithmetic
uses fifteen bounded additions, tracking the seventeenth carry bit and reducing
the remainder each time. It requires no multiplication table or product wider
than sixteen stored bits. Values at or above the denominator saturate directly.

`FeatureExtract` accepts perspective 0 or 1 and rejects zero MaxHP. Its numeric
contract permits saturated HP ratios and arbitrary 16-bit stats. The separate
`FeatureEvaluate` wrapper requires positive HP and guards an explicit neutral,
one-member-per-party Smeargle fixture with the audited Tackle/Water Gun slots,
zero status/items/substatuses/weather, neutral stages and cached Own Tempo.
It computes the first two action bits from usable PP. It does not implement
Disable, Encore, choice locks, charging, trapping, four-move selection,
replacement, items or Struggle. Exhausted PP returns unsupported status 2.
Other unsupported inputs return status 1. Both failures return action 255 and
clear input, hidden activations, logits and legal mask, preventing stale policy
outputs from being consumed. Success returns status 0 and the numerical action.
The documented fixture guards are a bounded prototype precondition, not a
complete audit of every ordinary-game action restriction or field condition.

Every RAM address is generated from the frozen actual-game symbol file, whose
identity and ROM identity are checked before assembly. This avoids a second
manually maintained layout. Source/profile changes require a new audit rather
than silently reusing the old guard. The game objects remain frozen and are
linked with two additional objects into an external artifact. The runtime is
entered with interrupts disabled and bank 127 already selected. Bank switching,
restoration, a live trainer caller and interrupt-safe integration remain open.

The check script uses retained public heldout observations, both perspectives,
16-bit arithmetic edge cases including 65535, upper PP bits, individual guard
mutations, empty masks and zero denominators. Public observations are installed
into synthetic RAM states for this feature ABI test. It does not claim those
states contain a complete captured original fight RAM image. No opponent action,
RNG state or future event tape appears among the descriptors.

Run only after the coordinator grants the sole CPU slot:

```sh
python3 tools/trainer_ai/feature_runtime_check.py \
  --build-dir /Volumes/Storage/tmp/polished-ai/build/feature-runtime \
  --game /Volumes/Storage/tmp/polished-ai/build/stateful/game \
  --mgba-prefix /Volumes/Storage/tmp/polished-ai/build/stateful/mgba \
  --model /Volumes/Storage/tmp/polished-ai/build/policy-pilot/trained.tai4 \
  --evaluation /Volumes/Storage/tmp/polished-ai/build/policy-pilot/evaluation.json
```

Measured cycles and actual linked ROM/WRAM/HRAM placement are recorded in the
external `results.json`. The executed result is summarized below. Normal-speed seconds are
T-cycles divided by 4194304; double-speed values would be projections only.
The feature schema remains provisional, with no empirical feature ablation.

## Executed bounded result

The final serial actual-game link passed 194 case records, representing 211
actual mGBA routine calls including timing calibration and separately measured
numeric/extraction paths. This includes eight retained public heldout probes,
eight action/RNG noninterference mutations, 36 extraction edge fixtures,
45 direct ratio cases, one direct zero-denominator rejection, 79 individual
guard mutations, 12 PP-mask cases, four zero-HP/MaxHP guards and an invalid side.
All eight complete policy calls matched the existing Python feature encoder,
independent numerical reference, hidden activations, logits and chosen action.

The linked additions occupy one contiguous 1,948-byte interval in ROM bank 127:

| Component | Address interval | Bytes |
| --- | --- | ---: |
| Existing multiplication products | `$4000..$41ff` | 512 |
| Feature arithmetic, guards and descriptors | `$4200..$44ed` | 750 |
| Existing numerical runtime, including timing probe | `$44ee..$4663` | 374 |
| Exact learned model | `$4664..$479b` | 312 |

There are zero alignment/padding bytes within that complete interval, and zero
additional ROM0 allocation. These are newly linked sections, not a claim that
all existing game bytes retain their previous addresses. Existing bank-127
sections can relocate during linking. The external artifact has not been
installed into a playable game branch.

Numerical scratch is 72 WRAM0 bytes at `$ceb5..$cefc`; feature scratch is eight
bytes at `$cf0c..$cf13`. An existing unrelated section lies between them.
The total new WRAM is 80 bytes, leaving 220 bytes of WRAM0 free. The existing
network pair counter uses one HRAM byte at `$ffef`, leaving four free. The
maximum call frame is six bytes below entry SP, plus the caller's two-byte
return address. Every oracle invocation also checks that RAM writes remain
within declared scratch and this stack area.

On the eight heldout public fixtures, the complete guarded decision took
128,808 to 133,624 T-cycles, or **30.71 to 31.86 ms** at normal speed. Direct
extraction took 34,712 to 39,472 cycles (8.28 to 9.41 ms); numerical inference
86,808 to 86,872 cycles (20.70 to 20.71 ms); guard/call overhead was 7,288 to
7,296 cycles (1.74 ms). The total projected double-speed time would be 15.36
to 15.93 ms, but double speed was not enabled or measured. These timings do not
include a live trainer caller, bank switching or interrupt handling.

This is a bounded prototype improvement to M3. It is not full M3 completion,
ordinary-game battle coverage, a validated final feature schema or evidence
that this small trained policy plays well across the game.
