# Integer inference feasibility prototype

This is an untrained numerical fixture, not a battle policy. The provisional
64-input, 96-hidden, 12-output shape follows the inventory's four moves, six
switch targets and two item slots. The observation schema and training quality
remain unresolved. The PC demo accepts an explicit legal-slot mask, restricts replacement to party
slots 4..9, picks the lowest slot on ties and returns -1 for all-invalid masks.
Generating battle-correct masks and full-fight training are outside this prototype.

Run the exact arithmetic and assembled kernel checks:

```sh
python3 tools/trainer_ai/inference_check.py --build-dir /tmp/trainer-inference-check
cc -std=c11 -O3 -Wall -Wextra -Werror tools/trainer_ai/inference.c -o /tmp/trainer-inference
/tmp/trainer-inference bench
/tmp/trainer-inference export /tmp/untrained.tai
/tmp/trainer-inference load /tmp/untrained.tai
```

Requires RGBDS, Python 3 and a C compiler. No emulator or training dependencies.
The benchmark times 10,000 in-process network evaluations with varying inputs
using C `clock()` CPU time, excluding process startup and model initialization.
Multiply and LUT paths share the same exported model and integer semantics.

## Representation and bounds

The file contains a 16-byte header (`TAI4`, version 1, unsigned byte dimensions,
eight zero reserved bytes), low-nibble-first signed int4 weights in neuron-major
order, little-endian signed int16 biases for both layers, and one unsigned shift
byte per hidden neuron. No floats, implicit scales or native-struct padding.
Hidden values equal `min(15, max(0, floor(accumulator / 2^shift)))`. Output logits
are raw signed int16 values. The fixture uses shift 5, but validated files allow
0 through 15. Biases are restricted to -16384 through 16384.

For 64 inputs and 96 hidden values, maximum absolute dot terms are 7680 and
11520. Adding the allowed bias cannot overflow signed int16. C accumulates in
int32 for portable defined arithmetic, avoids signed right-shift assumptions,
and verifies the format before inference. SM83 uses exact low-byte addition and
signed high-byte addition with carry. Inputs and hidden values must be 0..15.
Feature clipping belongs to the future extractor. Quantization accuracy has not
been evaluated. A 192-hidden variant needs narrower output-bias bounds to avoid
16-bit overflow, for example +/-8192.

## ROM and architecture comparison

The clarified limit is 10240 bytes TOTAL ROM, including inference, parameters,
tables and supporting feature/action data. Dense packed int4 candidates:

| Shape | Packed weights | Biases | Shifts | Header | Model | Assembled kernel MAC cycles |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 64 x 48 x 12 | 1824 | 120 | 48 | 16 | 2008 | 555936 |
| 64 x 96 x 12 | 3648 | 216 | 96 | 16 | 3976 | 1111584 |
| 128 x 96 x 12 | 6720 | 216 | 96 | 16 | 7048 | 2045472 |
| 64 x 192 x 12 | 7296 | 408 | 192 | 16 | 7912 | 2222880 |

The actual assembled dot routine is 57 bytes. Product tables occupy 512 bytes,
using separate aligned low/high pages to preserve signed products and carry.
The standalone layout has 199 padding bytes between code and tables, so its
occupied interval is 768 bytes, excluding the artificial empty $0000-$00ff.
General placement needs up to 255 alignment bytes, total 824 bytes. Packing
other ROM data into this gap could recover it, but that is not assumed here.
The 64 x 96 x 12 model plus conservative kernel reservation is 4800 bytes,
leaving 5440 bytes for still-unimplemented layers, feature extraction, masks,
action selection and integration. This is a feasible budget path, not evidence
that the complete engine fits. The 128-input alternative leaves only 2368 bytes
with the same conservative reservation and needs careful integration accounting.
Keeping 16 outputs instead of 12 costs 200 additional parameter bytes at width
96, with four unused actions and 58464 additional kernel cycles.

At 64 x 96 x 12, int8 dense weights alone cost 7296 bytes and the complete
parameter format costs 7624. A full 4-bit activation by int8 signed-product LUT
would need 8192 bytes, so it cannot fit with that model. Software multiplication
avoids table ROM but requires wider accumulators: 64 x 15 x 128 exceeds int16.
Ternary weights in two bits would reduce parameters to 2152 bytes and replace
multiplication with select/add/subtract, at an unknown model-quality cost. No
ternary kernel or timing was implemented, so its performance is not established.
A shared action scorer can reduce the final head from 12 outputs to one, but
recomputing an entire trunk once per action is expensive. A shared trunk with
small conditioned action heads is worth evaluating after the schema is fixed.

## SM83 evidence and latency

`inference_check.py` assembles the actual source and interprets only its small
opcode subset, checking all 256 scalar products, 150 seeded vectors, and four
extreme accumulator cases. This is instruction-level execution, not a physical
GBC measurement or complete emulator. Instruction timing was cross-checked
against [gbdev's opcode tables](https://github.com/gbdev/gb-opcodes/blob/master/Opcodes.json).
Cycle accounting includes branch taken/untaken timing, entry and `ret`. The
counter must be 1..255 pairs. The kernel reserves HRAM $ff80 for its counter,
plus two stack bytes. That HRAM address is a standalone harness choice and must
be relocated for game integration. Kernel code is in ROM0; model bank handling
and interrupts are outside the checker.

Per pair: first product 124 T cycles, second product 140 T cycles, loop counter
and taken branch 40 T cycles. Entry costs 12, final branch saves 4, and `ret`
costs 16, hence `304 * pairs + 24`. Calls to `Dot`, pointers, bias reads, shifts,
clipping and action masks are excluded. The assembled binary and map are
retained in the supplied build directory. This loop deliberately uses a simple
reproducible layout, not an optimized final engine.

At 8,388,608 T cycles/s in GBC double-speed mode, raw MAC kernel latencies are
66.27, 132.51, 243.84 and 264.99 ms respectively. Normal speed doubles them.
Double-speed behavior is documented in [Pan Docs](https://github.com/gbdev/pandocs/blob/master/src/CGB_Registers.md).
For the default shape, allowing an unmeasured 800 T cycles per hidden neuron
and 200 per output for calls, addressing, biases, clipping, shifts and storing
adds 79200 cycles, projecting 141.95 ms at double speed or 283.90 ms normally.
This allowance is a planning estimate, not a measured upper bound. Feature
extraction, bank switches, interrupts and action legality/selection add further
latency. A complete network micro-ROM is the next hardware acceptance check.

The default activations/logits need 64 + 96 + 24 = 184 bytes of RAM, plus
pointers, counters and temporary storage. Network streaming can reuse buffers.
Desktop fixture: 10,000 evaluations on the current host measured approximately
5.34 us each with native multiplication and 4.22 us with LUT, matching checksums.
Those CPU figures concern inference only and are not GBC timing extrapolations
or whole-fight simulator throughput.
