# Milestone 1 acceptance evidence

Date: 2026-10-10. Branch: `mobileadapter`. Status: complete for the implemented
transport foundation. The full multiplayer project remains at milestone 2.
Reproduction commands are in [../README.md](../README.md).

Scope reaffirmed 2026-10-11: this records completed transport-foundation acceptance
at the listed checkpoint, not a fresh run or proof of later four-player cadence,
priority escape, Bridge/cloud transport or compatibility metadata. Those expanded
checks belong to milestone 2. Future budget proposals below are historical;
[REQUIREMENTS.md](../REQUIREMENTS.md) supersedes them, including its rejection of a
durable save journal.

## Actual game, native adapter, Rust endpoint

The feature-enabled Polished Crystal game executes START, blue ISP dial, PPP,
TCP connect to `127.0.0.1:17423`, framed echo, checked response and complete
TCP/PPP/adapter shutdown. It then repeats with sequence 2. This is actual game
assembly through DoubleCherryGB's embedded libmobile, not a synthetic test ROM
or direct host socket masquerading as game communication. Three separate core
processes also reach the same server through three independent sockets.

The final standard suite passes 14 native game processes across 12 scenarios:
reconnect, three simultaneous clients, per-byte fragmented response, bad magic,
silence/timeout, remote close, unavailable server, unavailable adapter,
cancellation, recovery after a bad reply, oversized body and truncated reply.
Each checks normal one-tile movement after cleanup, correct map/object
coordinates, no crash marker, released serial ownership and all four guard
bytes. Server logs independently confirm requests and attempt sequences.
Intermediate PNGs and symbol traces are retained. A fresh ordinary battery
fixture is generated through the disabled game's normal New Game/Save menu;
Continue, valid main/backup checksums and movement pass in fresh processes.
No CPU/WRAM save state or test RAM injection is used.

The standard/faithful mobile builds pass. Faithful additionally passes its own
actual-game reconnect/movement scenario. Other mobile build combinations are
rejected explicitly; ordinary non-mobile build selection remains unchanged.
Generated mobile title graphics are isolated as well as object files, fixing
variant-switch contamination found during acceptance. These hashes use 2026
copyright metadata; pin that metadata when reproducing in another year.

## Actual placement and lifetime audit

| Resource | New use | Remaining in standard build |
| --- | --- | --- |
| ROM0 | 12-byte serial dispatch + 4-byte DelayFrame hook | 2 bytes |
| Banked driver | 1,390 bytes in bank 7f | 12,541 bytes ROMX overall |
| Other ROMX hooks | 12 bytes for cable/printer ownership | included above |
| WRAM0 resident state | 34 bytes, cec4..cee5 | 266 bytes overall |
| WRAMX bank 2 buffers | 676 bytes, d4eb..d78e | 2,161 bytes in its tail |
| HRAM and SRAM | no new allocation | original 5/1,832 bytes overall |

Total additional ROM use is 1,418 bytes. All 23,373 compared existing RAM symbols,
including aliases/exported symbols, retain their standard baseline addresses.
No existing helper file was moved from ROM0 to ROMX; no mapper change occurred.
The disabled ROM is exactly
`314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e`.
`tools/check_layout.py` repeats the placement comparison from current builds.

The resident state is in the original WRAM0 gap before Options. The buffers are
in a pinned bank-2 tail after Sound Stack, Pic Animations RAM, Sprites Backup
and Used Storage. Used Storage ends at d4ea, immediately before the new buffers.
Music save/restore copies `wChannelsEnd - wMusic`; picture and sprite backup
copies use their own declared ends. Menu/battle `Misc 404`/`Misc 1300` and other
UNION areas do not own or overlay these buffers. `ClearWRAM` deliberately wipes
all swappable banks on reset, so reconnect initialization must rebuild guards
and session state. MobileTick and the ISR restore the caller's bank. The ISR
uses only resident state and explicitly selected bank 2. It never borrows menu,
party, graphics or battle UNION storage for retained network data.

Future working ceilings, not reservations or completed implementations:

| Planned allocation | Ceiling |
| --- | --- |
| Transport/framing ROM | 2 KiB |
| Presence/renderer ROM | 2 KiB |
| Lobby/call/menu ROM | 2 KiB |
| Battle bridge ROM | 3 KiB |
| Durable gift/trade logic and data ROM | 3 KiB |
| Resident state | 96 bytes total |
| Adapter TX/RX plus guards/reassembly | current 676 bytes |
| Two peer records | 128 bytes |
| Session state | 96 bytes |
| Bounded actions/outgoing chunks | 256 bytes |
| Additional battle/transaction scratch | 512 bytes |
| Durable journal proposal | 256 bytes in SRAM bank-1 tail |

The proposed transient sum is 1,668 bytes within the original 2,837-byte bank-2
tail. Keep the base networking reservation below 1,536 bytes and allocate the
later 512-byte scratch separately in the remaining tail. Transfer parties and
Pokemon schemas in chunks; do not allocate a full network message for every
participant. Journal space at `baa4..bfff` is only a placement candidate. Record
redundancy, exact schema, checksum/save coupling and durable flush behavior must
be designed and tested in milestone 5. ROM ceilings total 12 KiB and leave a
small margin against the original 13,961-byte free budget; they do not prove all
future implementations fit. Recheck actual sections at every milestone. ROM0
has no room for another substantial hook. Put further integration in existing
banked foreground paths and the established frame pump.

## Runtime cycles, stack and preservation

The separate profiling copy observes actual instructions without modifying CPU
state or emulator timing. Reconnect measures 620 serial interrupts and 3,957
MobileTick calls:

| Measurement | Result |
| --- | --- |
| Longest full serial handler, including dispatch/far call | 1,716 CPU instruction cycles |
| Longest MobileTick body, including any nested interrupt work | 4,008 cycles |
| Largest serial work per core frame | 37,592 cycles |
| Measured speed during serial handlers | CGB double speed |
| Lowest observed SP | c0be |
| Headroom above c000 stack bottom | 190 bytes |
| Register/bank preservation failures | 0 |
| Video callbacks / audio batches in 4,000 calls | 3,997 / 3,997 |

The three missing startup video/audio callbacks also occur in ordinary LCD-off
transitions. At double speed a core frame has 140,448 CPU cycles. Measured serial
instructions use at most 26.8 percent (about 4.48 ms), and a tick body at most
about 0.48 ms. The current core does not charge the hardware's interrupt-entry
cycles; adding 20 cycles per serial entry slightly increases these estimates.
These are emulator observations for this fixture, not hardware certification or
future battle/rendering worst cases. Audio callback delivery is verified; this
is not an acoustic correctness claim or an overworld latency measurement.

Forty interrupts hit the two-instruction Bankswitch transition: the HRAM shadow
is already updated at address 0008 while the mapper write at 000a is pending.
The existing FarCall restoration completes that same intended mapper switch.
The observer accepts only PC=000a with preserved A/shadow and the expected target
physical bank; all other ROM-bank mismatches fail. The interrupted ROM0 mapper
write resumes with unchanged registers. This known transient is recorded as
`serial_bank_switch_completions`, not hidden by ignoring bank checks. WRAM
selector/pointer, ROM shadow, registers, stack and return PC are checked normally.

## Narrow hooks and remaining work

Hooks are limited to conditional includes/build rules, Serial dispatch,
DelayFrame's completed VBlank return, LinkCommunications,
Special_WaitForLinkedFriend, Printer_StartTransmission and generated title data.
Cable/printer paths release ownership before touching serial registers. The
accepted Select+B developer gesture consumes its buttons so the original
registered-item script does not interfere. The developer proof starts only in
an idle safe overworld; production connection and interaction UI are milestone 2/3.

Eight Rust unit tests and the production assembly/layout/vector contract pass
without launching an emulator. Clippy with warnings denied passes. ROM integration
is an explicit separate command. The detailed binary framing and adapter vectors
are in [../protocol/transport-v1.md](../protocol/transport-v1.md).

Current service deadlines depend on completed DelayFrame ticks. Later blocking
cable loops, dedicated UI/battle paths, ongoing-write deadlines/backpressure,
room protocol and durable operations are not covered by this echo proof. Their
bounded scheduling and dynamic lifetime scenarios belong to milestones 2 through
5. Crowded maps, live peer rendering, player calls, real battles and transactions
have not been implemented. The successful local echo does not establish the
later input-to-visible p95 target.

## Saved artifacts

Machine-readable summary: [milestone1-evidence.json](milestone1-evidence.json).
Full raw outputs on this host:

- `/Volumes/Storage/Cache/mobileadapter/m1-complete/`: final 14-process suite,
  per-instance input, state, trace, PNGs, server logs and profiling.
- `/Volumes/Storage/Cache/mobileadapter/m1-baseline-regression/`: regenerated
  ordinary Save/Continue/movement regression and checksums.
- `/Volumes/Storage/Cache/mobileadapter/m1-faithful-runtime/`: faithful reconnect.
- `/Volumes/Storage/Cache/mobileadapter/m1-layout-final.json`: build comparisons.
- `/Volumes/Storage/Cache/mobileadapter/build/doublecherry-profile-v2/`: measured
  read-only profiling core. The normal core remains in `build/doublecherry/`.

Record failed artifacts separately if investigating a regression. Never count
an unasserted screenshot, standalone TCP client or synthetic ROM as acceptance
of actual game transport.
