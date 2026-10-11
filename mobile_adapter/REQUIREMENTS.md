# Binding project requirements

Updated: 2026-10-11. This document records the user's requirements and subsequent
clarifications in English. It takes precedence over older implementation proposals
in PLAN.md. An unmet requirement is an acceptance blocker, not permission to
silently weaken it. Feasibility notes below are engineering assessments, not
user-approved exceptions.

## Minimal footprint

- **F1 MUST:** Minimal integration surface with Polished Crystal.
- **F2 MUST:** Minimal impact on ROM space.
- **F3 MUST:** The feature can be completely switched off with one compilation
  flag, producing a hash-identical GBC file to the corresponding pristine build.
- **F4 MUST:** All feature assembly lives in `mobile_adapter/rom`, apart from
  tooling and minimal integration code. Any deviation requires an explicit human
  decision after demonstrating why it is absolutely necessary and why alternatives
  do not suffice. Existing game helpers must not be relocated as a shortcut.

## Save files and recovery

- **S1 MUST:** Ordinary `.sav` files are entirely independent of multiplayer.
- **S2 MUST:** Saves are fully compatible in both directions between multiplayer
  and single-player counterparts built from the same source revision and matching
  build options, except for the mobile flag.
- **S3 MUST:** A button combination can exit multiplayer at any point. This action
  takes priority over all other game actions and must not depend on the server,
  adapter, a successful protocol exchange, or valid session state.
- **S4 MUST:** Whenever a save occurs, it must match the save produced in a
  single-player situation regardless of multiplayer state, including unexpected
  error states, malformed or malicious server packets, connection loss and an
  attacker flooding the emulator.

## Performance

- **P1 MUST:** Normal gameplay is indistinguishable between single-player and
  non-malicious multiplayer, with matching frame timings in every situation,
  including peers running at up to 400% speed. The user's DDoS exception applies
  to performance only; save integrity and graceful recovery remain requirements.
- **P2 MUST:** The regular game always takes priority. Multiplayer work must never
  block a regular game frame. Background work uses time the game would otherwise
  spend waiting for the next frame.

## Stability

- **R1 MUST:** Different emulator speeds coexist across the network, including
  100%, 200%, 300% and 400%.
- **R2 IMPORTANT:** Try to support arbitrary emulation speeds. If an upper bound
  is necessary, support at least 400%.
- **R3 MUST:** Clients handle broken or malicious server packets gracefully.
  Ignore invalid input when safe; abandon an untrustworthy stream locally when
  framing cannot be recovered safely.
- **R4 MUST:** Temporary server disconnects recover through automatic reconnect.
- **R5 MUST:** A compatible emulator state captured during multiplayer must remain
  escapable through the same button combination when restored without a usable
  Mobile Adapter, including an emulator build without libmobile.

The user explicitly clarified R5: state-format compatibility is already assumed.
The receiving emulator must successfully load the state; libmobile/adapter
availability may differ. Generic conversion between incompatible emulator state
formats is outside this requirement. Test the compatible ROM/core combinations,
including any supported builds with libmobile omitted, rather than assuming
that builds with different device support have compatible serialization.

## Definitions and unresolved feasibility boundaries

### Saves and exact equality

"Same hash" in S2 means the same source revision, not identical hashes for enabled
and disabled ROM binaries. The enabled binary necessarily contains different
code. Ordinary battery saves and full emulator states are distinct artifacts.

The proposed precise S4 comparison is: identical native save input state, initial
battery contents, RTC/time inputs and save operation produce identical cartridge
save bytes, for every multiplayer connection/error state. Connection data, peer
records, protocol buffers, tokens and recovery metadata must never enter the
battery save. A fresh offline load must not require the server. Emulator-specific
RTC wrappers/file metadata require separately verified import/export handling.

This definition still needs to be reconciled with the user's literal wording;
it is not an accepted relaxation. Different playthroughs can have different game
time, random outcomes and inventory. Legitimate trades or gifts intentionally
change normal game state. They cannot produce a byte-identical save to a
playthrough in which that inventory change never happened. They can preserve the
ordinary save format and serialize an equivalent native inventory identically.

The previous proposal to put a multiplayer transaction journal in `.sav` padding
is superseded by S1/S4 and must not be implemented. Exactly-once inventory transfer
across two clients, interruption, and immediate independent offline continuation
still needs a viable durability design. A server decision alone does not prove
whether a client durably applied it. A host sidecar or server-authoritative
inventory would change portability/offline assumptions and is not selected or
approved here. Milestone 5 is blocked on resolving this conflict before inventory
mutation code is written.

### Emergency exit and priority

The target is a bounded local escape path checked independently of ordinary menus,
scripts and protocol parsing. It must not wait for a graceful network shutdown.
Explicit user exit must disable automatic reconnect until the user joins again;
temporary involuntary disconnects retain automatic recovery.

The current implementation is insufficient: native `Joypad` returns early while
`wGameLogicPaused` is set for saving, and the current connected Select+B path
requests ordinary session teardown. Cancellation needs its own recovery design
and a measured worst-case response bound. No response-time bound is claimed yet.

The recovery path must not tear down a native save halfway through its critical
write sequence. Recognize/latch cancellation at a reachable input service point,
stop adapter activity safely, and complete native critical work before any game
state restoration that would interfere with it. Canceling multiplayer must not
mean aborting the normal save. Input detection has finite instruction/interrupt
latency even when it is given the highest software priority.

ROM code cannot respond while the emulator is paused/crashed, input is not
delivered, or execution is trapped forever with its input/interrupt service path
unreachable. The Game Boy's maskable interrupts are not an independent watchdog
or a hardware isolation boundary. These are actual platform limits, not a reason
to permit blocking network loops in the ROM.

For R5, treat the restored session as potentially stale: external sockets are not
restored by game RAM. Missing responses must not be required to release serial
ownership, discard volatile peers and continue locally. A successfully loaded
compatible state with absent libmobile is an explicit acceptance case, including
states captured mid-command and inside menus. A raw state cannot generally be
reused with a different ROM binary merely because both ROMs accept the same `.sav`.

### Frame priority and deferred remote presentation

The Game Boy has no second general-purpose CPU for multiplayer tasks. "Background"
means small resumable units of work placed in verified spare time before a native
frame deadline. Serial interrupts, bank switching, graphics publication and hooks
also consume cycles and must be included in the timing budget. A literal promise
of zero instructions outside existing waits is not currently established; any
necessary publication/input hook must remain surgical and be measured. Do not
treat a background label or byte-count cap as proof of matching frame timing.
Additional visible sprites also shorten the available HBlank on affected lines;
the spare-time budget must account for this hardware effect as well as CPU work.

Example: the last accepted remote position is tile A. The peer moves through B
to C while a busy local frame has no spare time. Native player/NPC updates and
display deadlines proceed normally. The remote world position remains A until a
bounded update fits; then the latest valid state C replaces it. B need not be
replayed. Old remote world coordinates must still be projected through the local
camera so the avatar does not become stuck to the screen. Rendering that cached
pose has a nonzero cost which must also fit the budget. Expensive animation/tile
updates may be deferred independently of position updates.

This is a proposed scheduling model, not the behavior already verified in the ROM.
It may reduce remote update frequency or increase remote latency. No fixed extra
delay is promised until measured. If a scene has no sufficient spare time for an
extended period, perfect native cadence and a fixed minimum remote freshness
cannot both be assumed. Report that case to the user; do not silently accept
seconds of delay or drop required local rendering. Coalescing applies to presence
snapshots, not to battle choices, confirmed trades or gifts.

Native timing parity means no additional missed world-update frames or longer
gaps than the matching pristine scene and input sequence. Existing baseline gaps
must remain visible in the comparison. Match visible input/audio timing as well;
identical instruction timing between different binaries is not implied. The
current implementation fails strict cadence and must not be described as meeting
P1/P2. Existing 300% evidence does not satisfy the new 400% acceptance requirement.

### Speed, hostile input and physical limits

Mixed 100%-400% speeds are realistic: use session generations, sequence numbers,
latest-state coalescing and bounded per-recipient work, independently of peers'
frame counters. The server owns real-time leases. Frame-based ROM deadlines must
be audited because 120 frames last four times less wall-clock time at 400%.
Arbitrary producer speed requires dropping superseded presence updates instead
of attempting to replay every intermediate state. It does not grant unbounded
throughput or immunity to a host too slow to run its selected emulator speed.

All wire-controlled lengths, types, indexes, counts and state transitions require
validation before use, with fixed memory bounds and bounded work even for valid
packet floods. Silence, partial frames and untrusted framing must permit local
recovery. The goal is to handle every input byte sequence safely; the present
finite fault suite is not proof of that property. Expand host fuzz/property tests,
assembly contract checks and real-ROM fault tests. A shared address space gives
the assembly module no hardware protection against arbitrary internal memory
corruption. An emulator/libmobile defect or host resource exhaustion outside the
ROM cannot be contained by ROM code alone. Document and test those components'
boundaries rather than promising absolute crash freedom across the full stack.

ROM space and sprite resources remain finite. At checkpoint 25fb35112, standard
enabled ROM0 has 7 free bytes, and ROMX has 5,945 free bytes across all banks;
fragmentation limits placement. Future interactions/battles/transfers need a budget
before implementation. Hardware OAM has 40 entries and displays at most 10 sprites
on a scanline. Three avatars still compete with NPCs, palettes and tile storage.
Perfect CPU scheduling does not remove these display limits. Minimal footprint
remains a design constraint, not evidence that all later features already fit.

## Required acceptance additions

- Bidirectional native-save loading between the corresponding enabled/disabled
  ROMs; exact save-byte comparison with equivalent native/RTC inputs, including
  main and backup saves. Existing save-failure tests are useful but insufficient.
- Priority exit during connection setup, established play, malformed/partial
  input, silence, menus, scripts, save activity and later battles/transfers. Check
  native state preservation, bounded response and explicit-exit reconnect policy.
- Restore connected compatible emulator states at multiple protocol phases with
  adapter disabled, unavailable, and a state-compatible libmobile-free build.
  Check local exit and continued movement without relying on server responses.
- Matched pristine timing tests at 100%/200%/300%/400%, with three clients, busy
  maps, scrolling, new avatar graphics, menus and controlled network delay. Include
  combined serial/parser/render costs and remote freshness, not just average FPS.
- Bounded behavior under malformed frames and sustained valid/invalid floods;
  reconnect after temporary failures without renewing an explicitly canceled
  session. Preserve normal save bytes under all injected transport conditions.
- Recheck disabled hashes, native memory layout and footprint after changes.

Current status: documentation only in this follow-up. No new 400%, emergency-exit,
save-byte-equivalence or connected-state-restore acceptance result is claimed.
Milestone 3 remains blocked by cadence; milestone 5 additionally requires resolving
save independence versus durable inventory recovery.

## Sources

- Local `home/joypad.asm`, `home/delay.asm`, `engine/menus/save.asm`,
  `mobile_adapter/rom/adapter.asm` and the committed layout/cadence evidence.
- [Pan Docs: interrupts](https://eldred.fr/pandocs/Interrupts.html),
  [HALT](https://eldred.fr/pandocs/Reducing_Power_Consumption.html),
  [rendering](https://eldred.fr/pandocs/Rendering.html) and
  [OAM](https://eldred.fr/pandocs/OAM.html).
- [Libretro core serialization API](https://docs.libretro.com/development/cores/developing-cores/#retro_serialize_size-retro_serialize-and-retro_unserialize)
  and the pinned DoubleCherryGB `libretro.cpp` serialization implementation.
