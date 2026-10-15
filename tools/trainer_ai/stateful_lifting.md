# Bounded stateful native ROM lifting

`stateful_lifting.py` decodes reachable instructions from the selected assembled
ROM at build time and emits native C instruction cases keyed by ROM bank and PC.
The runtime dispatches on PC after each instruction. It never fetches or decodes
an opcode at runtime. This is a correctness prototype, not an optimized basic
block compiler or a whole-battle simulator.

Unlike the older arithmetic fixture, CALL, RET and RST use the exact 16-bit SP and
RAM stack bytes. The real bank-switch and farcall wrapper instructions execute
unchanged. Farcall inline pointer recognition only discovers reachable code at
build time. It does not replace the runtime wrapper with a host function call.
Registers, flags, return addresses, WRAM, HRAM and MBC3 bank state are explicit.

The independent `stateful_fixture_check.py` runner uses the selected game ROM,
seeds routine calls in mGBA, records ordinary DIV reads, and replays those reads
through the native event tape. It compares complete CPU state and supported RAM,
including stack residue. Consult its report for actual corpus coverage. The
lifting module does not itself establish parity.

## API and provenance

`build(rom_path, sym_path, entries, build_dir, source_root)` creates generated C,
a content-addressed dylib, and `manifest.json`. Entry names must exist in the RGBDS
symbol file. `source_root` may be an external copied source tree: the canonical
tracked path list is read from the tools checkout, while file hashes are taken
from the actual supplied tree. The explicit source profile includes the tracked
tree except `tools/trainer_ai/`, which does not build game code. Missing snapshot
files reject generation. A changed expected input path set rejects loading, and
a source-backed manifest requires the source root at load time. Failed regeneration removes the acceptance
manifest before doing work. ROM and symbol hashes come from the consumed byte
snapshots. Source hashes are captured before generation. Inputs are checked again
before publishing the manifest, and a generator module imported before a file
change rejects. An older library cannot be accepted through that
manifest after failure. Immutable library names prevent dlopen cache reuse when
an assembled rule changes.

`load_verified(build_dir, rom_path, sym_path, source_root, entries)` rejects
mismatched ROM, symbols, generator, C output, ABI, compiler, profile, library,
source contents or configured entries before loading native code. Directly
loading a dylib bypasses this gate and must not be used as current-source proof.

`stateful_run(State *, budget, stop_pc)` executes until the stop PC or a fault.
The caller seeds SP and the actual return address bytes, current bank, supported
RAM, registers, and replay tape. `steps` accumulates executed instruction cases.
A fresh per-call budget bounds loops. Flags use the hardware F bit positions;
POP AF clears the unused low nibble.

## Explicit boundaries

The profile is MBC3 with banks below 128, ROM reads, flat WRAM at C000-DFFF with
WRAM bank one, HRAM, and DIV read events. Unknown reachable instructions reject
the build. Unknown dynamic destinations fault at runtime. Other MMIO reads and
writes, interrupts, external RAM, RTC, DMA, echo RAM and WRAM banking are
unsupported. An empty or exhausted DIV tape faults. The explicit `Crash` symbol
is a terminal fault rather than execution of the game crash UI.

Fault codes: 1 unknown PC, 2 memory/MMIO/event boundary, 3 instruction budget,
4 explicit game Crash. Unsupported paths must never become successful
transitions. Expanding this profile needs independent oracle cases and explicit
provenance changes. No training rollout or complete game parity is claimed.

## Running the differential checks

Requirements: Python 3, a C compiler, a built MBC3 game ROM with matching RGBDS
symbols, a complete snapshot of the tracked game sources, and mGBA development
headers/library. Use a compatible headless mGBA build if a package-installed
library has unavailable multimedia dependencies. The wrapper uses mGBA internal
SM83 headers, so record the exact mGBA version and library hash.

```sh
python3 -B tools/trainer_ai/stateful_fixture_check.py \
  --rom /path/to/game/polishedcrystal-3.2.3.gbc \
  --sym /path/to/game/polishedcrystal-3.2.3.sym \
  --source-root /path/to/game \
  --mgba-prefix /path/to/mgba-install \
  --build-dir /tmp/stateful-baseline

python3 -B tools/trainer_ai/stateful_fixture_updates.py \
  --game-source /path/to/disposable-game-copy \
  --mgba-prefix /path/to/mgba-install \
  --build-dir /tmp/stateful-updates
```

The update command temporarily changes and rebuilds source files. Use only a
disposable, independently buildable copy, never a working checkout with edits.
It restores each source and rebuilds the baseline in a `finally` block. All game
builds use `make -j1`; schedule this command serially with other heavy work.
The script forces relinking instead of trusting same-second object timestamps.

The corpus covers HitSelfInConfusion, attack/defense stage scaling, RandomRange,
BattleRandom, banked ConfusedDamageCalc, GetBattleVar table reads, and a chained
confusion calculation carrying real actor-derived registers into damage.
Inputs include both battle sides, extreme 16-bit stats, stage/level variation,
ordinary RNG rejection loops, and a bounded nonterminating RandomRange(0) case.
Reports distinguish successful parity from unsupported faults.

The source profile covers tracked files only. Newly introduced untracked or
ignored includes are not proven by source-only verification until incorporated
into that profile. A rebuilt ROM still changes its byte identity. The gate does
not build the game itself or prove that an arbitrary supplied ROM was assembled
from its supplied source tree. Pair fresh builds with source snapshots, and run
the differential update suite before accepting rule changes. Unchanged hashes
are provenance evidence, not a substitute for semantic coverage.

ROM/SYM bytes and source identities are captured before C compilation and
checked again before publishing the manifest. Concurrent input changes and a
generator file changed after Python import reject acceptance. Keep build output
directories exclusive to one process.

## Explicit optional boundary profiles

`build` and `load_verified` accept optional `boundaries` and `excluded` entry
lists. Their configuration is bound to the manifest. Default calls retain the
previous profile. A boundary case stops before executing the callee with fault 5;
the runtime never fabricates a return or resumes automatically. An external
profile must verify its exact bank/PC, real return stack and explicitly defined
semantics. Excluded entries and Crash remain fault 4 and must never resume as
successful presentation events. Ambiguity checks normalize ROM0 machine keys,
so a bank alias cannot turn Crash into an allowed event. See `battle_env.md` for
the experimental frontend profile and its substantial fidelity limitations.

## Optional explicit host boundaries

`build(..., boundaries=(), excluded=())` can mark named symbols or bank/PC
pairs. Both sets are bound into the acceptance manifest and must match at
`load_verified`. Boundary cases stop before the callee with fault 5; excluded
cases stop with fault 4 like terminal traps. ROM0 aliases are normalized before
rejecting a boundary that overlaps Crash or an exclusion. Default behavior
remains unchanged when both sets are empty.

The lifting runtime does not resume a boundary or fabricate a return. A specific
external profile must handle the event, validate the real stack and define its
semantics. Treating a callee as an identity return is a changed environment, not
proof that the original function has no side effects. The restricted battle
experiment documents its conditional profile in `battle_env.md`; unknown faults
must never be accepted as presentation events.
