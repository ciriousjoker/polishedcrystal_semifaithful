# Source-driven arithmetic lifting experiment

This is an exploratory native compiler slice, not a complete battle simulator.
`lifting.py` extracts DamagePass1..4 verbatim from `engine/battle/effect_commands.asm`,
Multiply/Divide including their loop implementations from `engine/math/math.asm`,
and PopBCDEHL from `home/header.asm`. It parses actual instructions and control flow,
then emits native C functions and labels. Arithmetic helpers remain translated
instructions, never replacements with multiplication or division formulas. The timed
native path uses ordinary compiled branches and calls, with no opcode dispatch.

`ram/hram.asm` supplies the actual UNION declarations. RGBDS assembles those
verbatim at an isolated test address and returns symbols to the native generator.
In particular, hDividend and hProduct overlap, hMultiplicand and hQuotient start
one byte later, hDivisor/hMultiplier/hRemainder overlap, and hMathBuffer follows
the five-byte union. No manually duplicated alias map is used.

## Reproduce

Requires Python 3, RGBDS and a C compiler. No third-party Python modules.
All execution is single-threaded. Choose a build directory outside Git:

```sh
python3 tools/trainer_ai/lifting_check.py --build-dir /tmp/lifting
cc -std=c11 -O3 '-DLIFTED_SOURCE="/tmp/lifting/lifted.c"' \
  tools/trainer_ai/lifting_bench.c -o /tmp/lifting/bench
/tmp/lifting/bench 300000
```

The directory retains fixture.asm, fixture.o, fixture.gb, fixture.sym, lifted.c,
lifted.dylib, manifest.json and checks.json. Native shared-library naming follows
this macOS experiment. It can be changed for another platform.

The manifest hashes the entire four extraction sources plus macros/rst.asm and
home/farcall.asm, which define the normalization assumptions. Rebuilding always
regenerates the source slice. This is not an incremental cache or full-ROM build
configuration identity. The fixture has an explicit isolated ROM0/HRAM/WRAM0
layout. Game-version branches, conditional compilation and other modules have
not been translated.

## Semantics and checks

8-bit wrap, 16-bit pair wrap, Z/C/N/H flags, carry chains, conditional branches,
HRAM memory operations and register push/pop are modeled. The checker executes
independently assembled SM83 bytes using a separate bounded Python interpreter.
It compares all seven data registers, final Z/C/N/H, all ten math-memory bytes,
both damage bytes and final stack balance over 2,456 seeded cases. These include
zero-defense normalization, level overflow, random/boundary math values and
explicit saturation/add-two boundaries. Independent integer formulas also check
all full-damage, multiply and divide cases. Zero division reports a fault.

A temporary copied source snapshot changes the actual `/ 5` instruction to
`/ 7`, rebuilds both native and assembled code, and changes damage from 51 to 38.
The original source is untouched. Nineteen unsupported instruction/operand
mutations reject, including DAA, ADD HL,BC/DE/SP, ADD SP,e8, stack-pointer loads,
indirect jumps, invalid HRAM load forms and nonbyte shift operands. A copied-source
ADD HL,BC assembles successfully in RGBDS, but the build entry point rejects it
before emitting native C. Every supported instruction passes an explicit operand
shape whitelist, with bounded immediate widths and RAM-only absolute operands.
Instruction parsing and symbol resolution fail closed for this bounded slice.
The byte interpreter is for differential verification only, never the training
runtime or the timed native path. It is not an independent hardware emulator and
common semantic errors remain possible despite separate implementations and
formula checks.

## Explicit abstractions

farcall/farjp become direct call/tail-call and jmp becomes jp. Source farcall
machinery documents preserved registers and saves/restores AF, but bank registers,
interrupts, hardware timing and the original return-address bytes are excluded.
Native function calls use the C call stack. Register push/pop use modeled memory
and a separate modeled SP. Consequently exact intermediate SP values and memory
written by return addresses differ from a real CPU. No lifted routine reads SP
or return addresses; adding such instructions rejects rather than silently
lifting them. Stack bytes outside the math/damage regions are not compared.
Crash becomes an explicit fault flag; exact crash-state stack contents are not
claimed. These limitations are material for extending to unrelated routines.

## Performance and decision

On the current Apple Silicon host, three 300,000-call runs achieved 1.485 to
1.867 million full arithmetic slices per second at -O3, median 1.585 million.
The matching handwritten integer formula was hundreds of times faster. Both
paths consumed the same deterministic 1,024 vectors and produced checksum
163816098. The benchmark reinitializes input registers and damage bytes, retains
one state object, and excludes extraction, compilation, ctypes/Python overhead,
full-battle logic and emulator execution. It does not establish training speed,
GPU suitability, model strength or added Game Boy ROM requirements.

The experiment makes automatic native translation credible for isolated pure
arithmetic and provides a source-update mechanism. It does not prove a complete
battle engine can be lifted economically. The next falsification target is a
real caller that obtains modified attacking/defending stats and traverses battle
state, macro dispatch and RNG, with actual farcall banked-byte parity. This will
expose state-layout/configuration coupling before investing in a general lifter
or a broad manual simulator.
