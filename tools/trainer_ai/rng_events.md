# Explicit source RNG events

`rng_events.py` replays an ordered list of actual source routine calls using the
existing native ROM generator. Entries are `UpdateDividerCounters`,
`AdvanceRNGState`, `Random`, and `BattleRandomRange`. `DoNothing` is an auxiliary
build root required by the actual farjp return trampoline, not a fabricated hook.

An event has `routine`, `div_reads`, and optional `argument` fields. Only
`BattleRandomRange` accepts an argument, required in 1..255. Initial registers,
flags including carry, bank, SP, WRAM and HRAM belong to the contract. Each event
pushes the real sentinel return address into the current WRAM stack. Routine
clobbers flow into subsequent calls. This API does not preserve the interrupted
register context of a VBlank handler and must not be inserted into an active
battle caller as an interrupt without a separately specified save/restore ABI.
Unknown events, unsupported execution, missing or leftover DIV inputs, and
nonterminating paths reject rather than silently approximate.

`rng_events_check.py` runs 72 short sequences, each with zero, one, or two explicit
pairs of divider-update and RNG-advance calls before Random and BattleRandomRange.
It includes both incoming carry values and varied seven-byte RNG state, including
all-zero and all-255 state. Every call compares all registers, SP, PC, mapped bank,
all C000-DFFF WRAM and FF80-FFFE HRAM, and exact input consumption. Report records
include before/after RNG bytes, DIV inputs and output state hashes.

The first 36 sequences record actual DIV reads from unmodified mGBA executing the
actual baseline ROM. The remaining 36 use the optional wrapper input hook to
exercise DIV 0 and 255 in both orders. The wrapper still records exactly what the
ROM reads; native receives that recording. The mGBA library and ROM are unchanged.
The base oracle resets the machine for each routine, disables LCD/interrupts, and
restores supported RAM/registers. Thus these are conditional, synchronous call
comparisons. They do not prove continuous timer phase, ordinary hardware event
laws or probabilities, VBlank timing, full turn correctness, or omitted frontend
state projection. No fake frame event is assigned to a delay or presentation call.

The boundary tape control changes subsequent range results. For example report
case 12 uses the same declared per-routine boundary input pattern and returns
22, 88, and 19 with zero, one, and two refresh pairs respectively. This is concrete
state and draw sensitivity under those inputs, not distribution evidence.

## IID profile inference from source

The RAM declaration contains four state bytes, two cumulative-plus bytes and one
cumulative-minus byte. Let M be prior minus, C the incoming carry and U the first
DIV read. UpdateDividerCounters writes M' = U - M - C modulo 256. The subsequent
four-byte transform in Random resets carry with `add a` before any carry-sensitive
transform operation, and reads only the original four state bytes. Its new last
byte E' is therefore independent of the current call's DIV inputs. Random returns
A = E' - M' = E' - U + M + C modulo 256.

If U is independent and uniform over 0..255 conditional on all earlier state and
events, this bijection makes A exactly uniform conditional on that history. The
third independent uniform DIV input also makes hRandomAdd = E' + DIV3 uniform.
For non-link BattleRandomRange, its source rejection region has size a multiple
of the requested range, so its accepted modulo result is uniform under that IID
law, including across repeated rejection calls. RandomRange uses hRandomAdd and
has the analogous IID property. This is a mathematical consequence of a declared
IID input law, not a measured hardware property. It does not justify correlated
DIV sampling, asynchronous event laws depending on the current unseen sample,
arbitrary consumers of scratch flags/state, omitted frontend writes, or real-game
strength claims. Source or profile changes require rechecking the argument.

## Reproduce

From the exploration worktree, using existing source-backed game artifacts and
private mGBA installation, run:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 tools/trainer_ai/rng_events_check.py \
  --rom /Volumes/Storage/tmp/polished-ai/build/stateful/game/polishedcrystal-3.2.3.gbc \
  --sym /Volumes/Storage/tmp/polished-ai/build/stateful/game/polishedcrystal-3.2.3.sym \
  --source /Volumes/Storage/tmp/polished-ai/build/stateful/game \
  --prefix /Volumes/Storage/tmp/polished-ai/build/stateful/mgba \
  --build /Volumes/Storage/tmp/polished-ai/build/rng-events
```

The command builds only its source-derived native artifact and tiny oracle wrapper.
It does not rebuild the ROM, generator, emulator or training profile. The manifest
pins game source, ROM, symbols, current generator, compiler, native C and library.
`report.json` additionally pins the unchanged base wrapper, boundary wrapper and
mGBA library.
