# Complete numerical SM83 runtime fixture

This is an untrained network, not an integrated battle decision or selected
observation schema. `network_runtime.asm` evaluates both packed int4 layers,
writes all hidden values and signed int16 logits, then selects the highest legal
logit with lowest-slot ties. A zero legal mask returns `$ff` in A and
`NetworkAction`. Bits beyond the configured action count are ignored. Replacement
legality remains caller supplied; the fixture includes mask `$03f0`.

Compile-time defaults are 64 inputs, 48 hidden units, 12 actions. Both widths must
be even and in 2..136; actions are 1..12. Every input must be 0..15. All signed
int16 biases must be in -16384..16384 and shifts in 0..15. For either dot width
D <=136, every partial sum lies between -16384-D*120 and 16384+D*105,
hence within -32704..30664. This guarantees no signed wrap even at intermediate
MACs. Wider dimensions are rejected by assembler assertions, rather than
assuming cancellation in the final dot result.

The model format remains `TAI4` version 1: 16-byte header, neuron-major packed
weights for hidden then output layers (low nibble first), little-endian int16
biases for both layers, and hidden shift bytes. Model bytes are
`16 + I*H/2 + H*A/2 + 2*(H+A) + H`, or 2008 by default. This standalone runtime
trusts compile-time model validation, and does not parse an untrusted file. The
fixture generator enforces weight, bias and shift contracts. Arithmetic shifts
implement floor division even for negative accumulators, then clamp to 0..15.
Output logits are unsquashed signed int16.

All code, tables and model live in ROM0, so this prototype requires no bank
switch and does not silently assume a selected ROMX model bank. The default
complete numerical footprint must be measured from linked symbols. Alignment
can consume up to 255 bytes before the 512-byte table. The micro-ROM container
is padded to a physical 32 KiB cartridge image; occupied code/table/model bytes
are reported separately, since the project budget is 10240 bytes of added game
ROM. A 136x136x12 stress model is only a bound fixture and exceeds that budget.
The integrated game will need reserved linker sections or explicit bank switching.

The standalone ABI reserves WRAM0 starting at `$c000`: inputs, hidden, logits,
2-byte caller legal mask, action result and 13 bytes of bookkeeping. It also
reserves HRAM `$ff80` and at most six stack bytes including the caller return
address. These addresses are harness allocations, not available game RAM claims.
The routine clobbers all CPU registers and requires disabled interrupts. Game
integration must allocate scratch storage, preserve caller state, arrange model
bank access, and choose interrupt handling. No game RAM or inference sources are
modified by this fixture.

Run serially when the sole CPU-heavy execution slot is available:

```sh
python3 tools/trainer_ai/network_runtime_check.py \
  --build-dir /Volumes/Storage/tmp/polished-ai/build/network-runtime \
  --mgba-prefix /Volumes/Storage/tmp/polished-ai/build/stateful/mgba
```

The checker assembles three shapes, seeds WRAM and the actual SM83 registers,
and executes mGBA until a seeded return address. It compares hidden values,
logits, selected slot, A and stack return against independent Python integer
arithmetic. It covers all shifts, positive/negative bounds, signed comparisons,
ties, legal masks, replacement masks and all-invalid inputs. The private wrapper
is scoped from the existing stateful oracle, with a real mGBA timing counter;
it does not interpret opcodes. Execution is serial and no GPU is used.

Measured numerical-network cycles exclude feature extraction, game-correct
legal masks, bank/context hooks and battle actions. Conversion to GBC double
speed uses 8388608 T cycles/s only as a conversion, not a tested game timing.
Full rosters and observation ablations remain future schema/training work. This
can complete the numerical-runtime substep only, not the complete M3 milestone.

Validated on the private headless mGBA library: 198 network runs across 2x16x12,
64x48x12 and 136x136x12 matched the Python reference. The default fixture used
589148..603984 T cycles including both layers and action selection. This is
140.46..144.00 ms at normal speed; 70.23..72.00 ms is an arithmetic double-speed
projection only. The linked ROM is DMG mode and does not test double-speed mode.
A known 16-NOP plus RET probe takes 80 T cycles and 160 mGBA timing ticks,
calibrating the wrapper's explicit two-ticks-per-T-cycle conversion.

Default measured section accounting: 374 bytes runtime including the 17-byte
timing probe, 58 bytes table alignment, 512 bytes LUT, 2008 model bytes, total
2952 bytes. Reserving worst-case 255-byte alignment instead requires 3149 bytes,
leaving 7091 bytes of the 10240-byte project budget for future extraction and
integration. The standalone cartridge header/padding are additional harness
bytes, not added game data. Scratch is 152 WRAM bytes, 1 HRAM byte and at most
6 stack bytes. Removing the fixture timing probe is possible but not assumed.

Exact commands, source/library/artifact hashes and per-case cycles are retained
in `build/network-runtime/results.json` under the run directory. The measured
cycle range covers these golden vectors, not every possible model/input. No
claim is made for full-decision latency, learned-policy quality, emulator frame
latency, or integrated game-bank/interrupt behavior.

## Artifact-only placement in the real game

The same source supports `-D NET_GAME=1 -D NET_BANK=127`. This suppresses the
micro-ROM header and fixed addresses, allocates all executable/data sections in
one configurable ROMX bank, and lets the game linker allocate WRAM0 and HRAM.
Entry requires the caller to map that bank before calling and retain it through
the routine. The routine makes no bank changes. A future game hook must arrange
bank restoration and interrupt compatibility. No game source or Makefile changed.

```sh
python3 tools/trainer_ai/network_runtime_game_check.py \
  --build-dir /Volumes/Storage/tmp/polished-ai/build/network-runtime/game-placement \
  --game /Volumes/Storage/tmp/polished-ai/build/stateful/game \
  --mgba-prefix /Volumes/Storage/tmp/polished-ai/build/stateful/mgba
```

The script links frozen game objects with the new object using the existing
layout/linker/header flags, writing only its own artifact directory. It verifies
that input object, layout, baseline ROM and symbol-file hashes stay unchanged.
The final actual linker map places LUT at bank127:$4000..$41ff, model at
$4200..$49d7, runtime at $49d8..$4b4d, for 2894 added ROM bytes and no alignment
loss. Bank127 retains 11032 bytes free; ROM0 consumes no new space. Linker
placement may relocate existing unconstrained sections, so this is evidence of
a valid fresh link, not a byte patch to an existing game ROM.

Allocated scratch is WRAM0 $ceb5..$cf4c (152 bytes), leaving 148 bytes free;
the HRAM counter is $ffef, leaving four bytes free. Golden execution uses the
actual game stack region at $c0f0. Six mask cases match independent Python in
actual bank127 mGBA execution, retain bank127, return A and SP correctly, and
take 600980..602168 T cycles for the seeded model. The 80T probe also calibrates
the CGB-compatible game ROM in normal speed. These executions directly invoke
the numerical routine and do not claim trainer-hook or whole-game acceptance.

The integration report retains all source, artifact and frozen input hashes in
`game-placement/results.json`. The standalone numerical report records the
source revision used for its full vectors; the final conditional source is also
validated through the real-game artifact. Neither test enables double speed.

Cycle measurements start at NetworkEvaluate entry and include its final RET; caller CALL, bank mapping/restoration and interrupt setup are excluded. The relocated seeded-model range converts to 143.285..143.568 ms normally, or 71.642..71.784 ms as a double-speed arithmetic projection only.

The counted runtime includes a 17-byte timing-calibration entry (16 NOPs and RET).
It is test support included conservatively in the reported ROM footprint; a
production integration can omit it. Its instructions are not part of evaluation.
