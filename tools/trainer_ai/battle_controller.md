# Source controller under validation

`battle_controller.py` executes lifted assembled instructions and suspends before
declared input or presentation leaves. It preserves the source stack, bank
switching and controller callers. A boundary is an event, not an invented return;
the profile explicitly services an input or projection before popping that real
callee return address.

`battle_controller_profile.py` roots the real `DoBattle`, `BattleTurn`,
`PerformMove`, `ResolveFaints`, complete `HandleBetweenTurnEffects`, move command
handlers, ability pointer handlers and item-effect dispatch tables. Reachable
unsupported opcodes remain rejecting exclusions. Including a handler as a root
does not establish positive test coverage of that mechanic.

`battle_scenarios.py` accepts one through six members per side and one through
four moves per member. Initial stats use actual source routines. Ordinary
trainer mode uses the actual `NO_EXP_OPT` path: levels, base stats and EXP stay
fixed; normal battle stat stages and ability/item modifiers remain active.
Source EV bookkeeping before the no-EXP check is internal and cannot become an
actor input. The player bag and the enemy's exactly two trainer slots are
separate source interfaces.

Both public observations and policy choices must be frozen before either
current action is committed. XP/EV progress, pending opponent choices and future
random draws are excluded from actor observations. Source-query legality runs
on independent CPU/RAM/random copies and cannot modify the live fight.

## Bank and wrapper ABI

The additive native export `stateful_run_banked` stops at an exact ROMX bank and
PC, or globally for ROM0. The old `stateful_run` keeps its wildcard-bank behavior.
The State ABI is unchanged. `single_step()` executes an actual instruction with
a one-instruction budget when leaving a stopped controller-loop entry.

The generic lifter keeps O3 as its default. Controller validation builds default
to O0 to avoid repeatedly optimizing a large switch during source-integration
work. Optimization and the exact compiler argument vector are manifest bound;
optimization also contributes to the library filename identity. Final throughput
measurements require a separately labelled O3 artifact.

The controller opts into CGB WRAM banking. WRAM0 occupies `m[C000:CFFF]`, bank 1
remains canonical at `m[D000:DFFF]`, and banks 2 through 7 use the private backing
`m[0000:6000]`. These backing bytes never replace ROM reads, which still use the
assembled ROM. FF70 preserves its low three raw bits, returns hardware high bits,
and maps selector zero to bank 1. The State ABI is unchanged; the manifest records
the distinct banked memory profile. Default flat profiles still reject FF70.
The CGB fixture executes actual `ClearWindowData` before battle initialization,
so real window pushes, nested menu headers and pops use valid bank-7 storage.
All-bank proofs compare CPU, WRAM0 and all seven switchable banks, HRAM and SVBK.

Party menu attributes, standard menu headers and `CloseWindow` execute source
instructions. The input callback selects buttons and a cursor; actual
`Move2DMenuCursor` then performs the cursor bookkeeping consumed by the real
`GetMenuJoypad` and `PartyMenuSelect` callers. The source preserves AF and BG-map
mode around the lower `MenuJoypadLoop` input suspension.

Declarative discovery-only wrapper ABIs identify `AnonBankPush`'s inline bank
byte and the three source stack-jump-table wrappers. The real wrappers and stack
instructions execute unchanged. Unproved banked inline-byte continuations reject.
Far jumps omit the false inline continuation while normal far calls retain it.

The controller permits declared write-only audio sinks FF10 through FF14 and
the OBJ palette write FF48. Original source CPU and WRAM writes still execute.
Reads and other MMIO reject; strict profiles remain unchanged. All declarations
are manifest bound and must match on load. `LoadBallIconGFX` and `Request2bpp`
project pixel transfers only. Trainer-HUD ball classification, OAM and scratch
writes, status-index calculations and their callers execute source instructions.
These are conditional frontend projections, not unhooked rendering proofs.

## Evidence and remaining acceptance

Focused helpers cover source far-call discovery, 64 inline-wrapper CPU/RAM
comparisons, 16 actual audio-tail comparisons, ten bank-stop comparisons and
16 banked-WRAM/window comparisons.
Artifacts are retained under `build/battle-controller` in the external project.
Each report applies to its recorded generator and source identity. A later
generator needs its own acceptance evidence.

The boundary oracle runs independent mGBA until any actual suspension boundary,
then replays its DIV reads in native execution and compares CPU, all WRAM and
HRAM. It does not use the native result to choose the reference stop. Presentation
identity projections are explicit assumptions of this oracle. Separate real-UI
continuation proofs are required before claiming ordinary frontend fidelity.

M2 acceptance remains the source-bound coverage gate, meaningful positive
mechanic scenarios, full source-controller fights, reset/replay checks, and
executable source-update regeneration. A neutral handler return, matching two
identical hook configurations or an explicit rejecting exclusion does not close
an untested mechanic.

The controller explicitly enables `lcd_disabled=True`: reads of LCDC (`FF40`)
return zero, matching the independent headless reference with its LCD disabled.
The generic machine remains strict by default; LCDC writes, neighboring reads,
and mismatched loader configuration reject. This executes `ClearTileMap`'s
original WRAM fill and LCD-off return. The focused LCD check compares actual
source execution and validates the negative guards. LCD-on frontend behavior
still requires separate projection evidence.

The LCD-off pixel-copy path has an explicit `Copy2bpp` presentation boundary,
alongside `Request2bpp`. Its real caller still selects the branch and executes
the WRAM-bank wrapper. The wrapper's stack-built `.return` is an explicit source
root, so restoration executes normally. `EnableLCD` is projected only in the
headless profile. At `Pack_InitGFX` its A/F result is overwritten immediately by
`DrawPackGFX`'s pocket load and mask; BC/DE/HL are unchanged by that leaf. Other
callers remain conditional until their consumers receive equivalent review.

Typed menu input now executes the source `_2DMenuInterpretJoypad` before
`Move2DMenuCursor`. In particular, scrolling-menu pocket changes require the
source-derived edge flag in `w2DMenuFlags2`; supplying a direction alone would
leave the real pack controller waiting. Pack callback roots come from its actual
`dba` declarations, and text dispatch roots from `SpecialCharacters` and
`TextCommands`. The source scrolling-menu initialization, item context,
selection, effects and inventory removal all execute.

For healing-item confirmation, only `WaitPressAorB_BlinkCursor` is projected.
It blinks a cursor and waits for input while saving/restoring its two HRAM object
buffers. `ItemActionTextWaitButton` still executes its tilemap, action text and
mode writes. The reviewed `ItemRestoreHP` continuation calls `UseDisposableItem`
and ends with `xor a`, so the wait's A/F result is not consumed as an item result.
Other callers remain conditional until reviewed. This is presentation evidence,
not proof of an unhooked physical-input frontend.

Source-update experiments use an explicit optional audit object at both profile
build and `ControllerMachine` construction. The default verifier still requires
the previously frozen action profile. A bounded-update verifier must validate
its exact reviewed source delta, current ROM/symbol bytes and retained actual
serial Assembly-build receipt. Its configuration is recorded in both profile
and native manifest; mismatched runtime audit configurations reject. The generic
native loader still independently checks present generator, full game inputs,
ROM, symbols, compiler configuration and library bytes. This API does not grant
arbitrary new ROM hashes permission to execute.
