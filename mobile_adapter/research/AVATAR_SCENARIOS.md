# Selected figures and portable test scenarios

Completed on 2026-10-11 on branch `mobileadapter`, after milestone 2. This report
supersedes the fixed-Chris renderer, tile reservations, schema and build hashes
in the original [MILESTONE_2.md](MILESTONE_2.md) acceptance record.
Machine-readable evidence is in
[avatar-scenarios-evidence.json](avatar-scenarios-evidence.json).

## Result and manual workflow

The local player continues to use the normal saved character choice. Remote
players now publish that same `wPlayerGender` choice: Chris, Kris, Crys or Beta.
Each peer uses the native character/movement sprite table and appropriate native
palette, independently of the local figure. Standing, walking, running, cycling
and surfing use the corresponding vanilla assets. Skate shares the ordinary
sprite; Pikachu surfing uses the native shared Pikachu sprite.

The one-command launcher asks for one of three starting scenes before preparing
or launching anything: bedroom, Goldenrod Game Corner or Goldenrod City. The
defaults are Chris on Mac and Kris on Android. Explicit `--scenario`,
`--mac-avatar` and `--android-avatar` flags support repeatable unattended runs.
The fixed port, Mac volume, stable Android filenames and single ES-DE-style
launch intent remain unchanged.

[tests/scenarios](../tests/scenarios/README.md) contains twelve committed ordinary
32 KiB in-game saves, one per scene and character. Producers set the initial
spawn, clock and character-menu default in disposable ROM copies, then run
normal New Game, character selection and in-game Save. The manifest records
every changed initialization byte and the save hashes/checksums. No game RAM or
battery bytes were edited. The bedroom starts at 3,3 so the downward movement
probe does not hit the table. Every deployment generates fresh platform-specific
auto-states from those batteries with the current ROM and exact platform core.

## Rendering and resource changes

Lobby schema 3 adds an explicit figure byte: snapshots are 11 bytes and wire peer
records are 17 bytes. Schema-2 clients must be updated together with the server.
Internal snapshots remain 14 bytes and peer records remain 64 bytes.

Each peer has an independent 384-byte native graphics cache in bank 2,
`dac0..ddbf`. Only the four tiles for its current frame are uploaded to VRAM0,
at `64..67` and `68..6b`. The old VRAM1 side frames and font-area walking
reservation are released. Native facing geometry and mirroring are retained.
The native `MarkUsedPal` label is exported without moving or changing its
instruction bytes. When all eight palettes are occupied and no exact normal,
unmodified palette matches, that peer is deferred with reason 4. The status menu
currently reports the generic scene pause for this reason. Required NPC
palettes are never replaced to force an avatar onto the screen.

Existing milestone-2 NPC/OAM pressure policy and conservative large-sprite
deferrals remain. Remote avatars consume no NPC object slots. All 23,373
existing RAM symbols retain their baseline addresses; the disabled ROM retains
its exact baseline hash. No existing helper was relocated.

| Resource | Current use |
| --- | ---: |
| Resident mobile WRAM0 | 122 bytes, cap 128 |
| Mobile protocol/OAM WRAMX | 1,473 bytes, cap 1,536 |
| Independent peer graphics caches | 768 bytes |
| Remaining bank-2 tail after graphics | 576 bytes |
| Driver in ROMX bank 7f | 7,196 bytes, cap 8,192 |
| Standard free ROM0 / ROMX | 7 / 6,711 bytes |

## Start menu behavior

Opening Start deliberately pauses social drawing on that screen. Vanilla keeps
a still local scene and clears sprites under the menu panel. Other emulator
instances can keep moving independently. The menu owner publishes busy
availability, retains lobby membership, and restores the current peer figures
after closing. This is the accepted static-world behavior, not a disconnect.

The actual signal for this Start-menu path is `wScriptRunning == 255`;
`hInMenu` remains zero. Native tests in both the bedroom and Game Corner observe
paused frames with zero rendered peers and deferral reason 3, then two correctly
rendered peer figures after B closes the menu.

## Acceptance evidence

The final host target passes 20 Rust tests, eight deployment tests, six scenario
tests and production RGBDS vector/layout contracts. It starts no emulator.
Clippy with denied warnings, Rust formatting and the renderer audit pass.

Eleven cases run 33 independent native game processes with ordinary battery
saves: mixed Chris/Kris/Crys crowding, two Start-menu scenes, outdoor NPC motion,
300%/100%/100% speeds with 100 ms simulated RTT, profiled crowding, running,
cycling, surfing, faithful gameplay and 40 local latency samples. Normal mixed
figure cases also cover Beta. Movement-variant runtime cases use Kris; the
renderer audit covers the native sprite headers for all four choices.

The read-only oracle compares each visible peer's decompressed 24-tile cache,
four actual VRAM frame tiles and loaded palette identity against native sprite
assets. Figure IDs must match the other client's actual selected figure. NPC
motion is verified separately on every client while the local player stands
still and the occupied object mask is unchanged. Story flags, actor restoration
and hardware scanline limits remain checked in the crowded room.

Six profiled processes have zero bank/register or NPC-restoration failures.
The maximum foreground path is 25,392 cycles, serial ISR 1,432 cycles and
observed VBlank 67,840 cycles, below the respective 32,768 / 2,048 / 70,224
ceilings. Lowest observed SP is `c0ac`, above the `c040` gate.

Local motion-to-visible-peer latency is p50 83.097 ms, p95 100.643 ms and maximum
210.258 ms over 40 samples. Input-to-visible-peer p95 is 276.534 ms and includes
the unchanged vanilla turn delay. These are native local tests, not a measured
mobile-network latency distribution.

Fresh Mac and physical Pixel snapshots load Game Corner at 14,5 with Chris and
Kris respectively. Read-only screenshots of the running manual session confirm
the corresponding local and remote figures on both screens. The manual session
was left running. Raw ROMs, states, traces and screenshots remain in the external
host cache; only portable batteries and compact evidence are committed.

Every accepted test ROM was normalized back to localhost port 17423 and its
global checksum recomputed, matching the appropriate current compiler output:

| Build | SHA-256 |
| --- | --- |
| Standard mobile | `3bafac80ecea0d7da588f0b42ec7bc52ff1a703a60bb8708841ca03b791d1f87` |
| Faithful mobile | `6cd3a3997c33bb04bee19abbeb70c55d2c13e42077a9b8df7d1498e4966ee657` |
| Disabled standard | `314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e` |

The initial blocked-bedroom probe, two corrected Start-menu oracles and one
test-port collision are recorded as excluded development attempts. They are
not counted as accepted scenarios. The next feature work remains milestone 3,
adjacent interactions and calls, when requested.
