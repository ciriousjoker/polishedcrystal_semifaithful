# Multiplayer acceptance with cargo test

This suite runs real ROM instructions and compares them with freshly built
single-player controls. Existing native world-update gaps are allowed; additional
multiplayer delays, duplicates or longer gaps fail. Product failures are ordinary
failed tests. No acceptance requirement is marked ignored or expected to fail.

From the repository root:

```sh
cargo test --locked --manifest-path mobile_adapter/tests/Cargo.toml --no-fail-fast
```

Run the fast production-protocol/lobby tests without a core:

```sh
cargo test --locked --manifest-path mobile_adapter/tests/Cargo.toml --test host
```

Select one ROM contract:

```sh
cargo test --locked --manifest-path mobile_adapter/tests/Cargo.toml --test rom \
  equal_actor_load_frame_timing_matches_singleplayer -- --nocapture
```

All new test definitions and support code live here. `Cargo.toml` is an independent
test package, using the production Rust crates as dependencies. The existing C++
frontend has one opt-in `--acceptance 1` observation mode and includes this folder's
observer header. It does not change production ROM code. Python is build/process
tooling, not another test framework.

## Prerequisites and artifacts

The default host configuration uses the project's supplied, checksum-pinned
MBC30 DoubleCherryGB source archive, RGBDS 1.0.4, CMake, a C++17 compiler, Python 3
and Cargo. The ROM suite prepares its own profiling core from that source and
the current observer headers. Shared installed cores and human emulator sessions
are not changed. Linux can use the corresponding source and `.so` build.

Environment overrides:

| Variable | Purpose |
| --- | --- |
| `MOBILE_TEST_CACHE` | Existing toolchain/core cache; defaults to the configured host cache |
| `MOBILE_ACCEPTANCE_RGBDS` | RGBDS binary directory |
| `MOBILE_ACCEPTANCE_CORE_SOURCE` | Supplied MBC30 source tree |
| `MOBILE_ACCEPTANCE_CORE` | Already built equivalent core with the acceptance observation API |
| `MOBILE_ACCEPTANCE_RECOVERY_CORE` | Optional compatible instrumented core for restore without adapter |
| `MOBILE_ACCEPTANCE_PRISTINE_REF` | Explicit corresponding source revision for disabled byte identity; default is merge-base with master |
| `MOBILE_ACCEPTANCE_OUTPUT` | Artifact directory; choose a fresh path when any source or scenario input changes |

Default artifacts are under `MOBILE_TEST_CACHE` (the Storage cache on this host),
with a process-specific name printed during execution. Each test executable has
its own default session.
An explicitly shared output directory lets build and ROM tests reuse the same
prepared artifacts. Source fingerprints reject reuse across changed inputs.
ROM/core hashes, controller events, compressed per-frame traces, both complete
SRAM images, final screenshots and subprocess logs are retained.

By default Rust runs up to two functional scenarios concurrently. Set
`MOBILE_ACCEPTANCE_JOBS` to change that bound (each live scenario runs four cores).
The four performance cases declared in `assumptions.json` run exclusively,
including their controls, evidence decoding and assertions. Waiting performance
cases take priority over starting new functional work. Native controls and seed
producers run four at a time. All multiplayer cores retain real-time pacing.
Functional cases use a shorter five-second join warmup; performance cases retain
their original warmup. The measured window remains 800 frames for every case.
Shared build/fixture/seed construction is locked separately from live execution. Every scenario starts fresh guest instances and owns ephemeral ports.
Contracts for one immutable execution can share its evidence; no test depends on
another test having changed game state. Process deadlines also cover hung cores.

## Scenarios and independence from upstream content

`assumptions.json` distinguishes **hard dependencies** from **derived inputs**.
Hard dependencies are named assets or engine interfaces needed by the adapter.
Derived inputs include their current IDs, pixels, colors, dimensions, collision
geometry, structure sizes, field offsets and memory addresses. Exact graphics,
historic save versions, ordinary NPC counts/positions and story dialogue are not
frozen expectations. Hardware limits and chosen acceptance ceilings are separate
from source-stability assumptions.

Disposable source trees reuse existing maps and their current background and
collision assets. Only synthetic event headers are substituted there. Original
script definitions and constants remain available for references from other maps.
The tracked checkout is never temporarily edited for fixture construction.
Positions come from the current collision data; native spawn field order comes
from the current loader. Setup errors identify missing prerequisites and fail
acceptance instead of silently skipping a contract.

The fixture enters current native initialization without title, character,
clock or story-dialogue clicking. Native object macros, map loading, script
execution and main/backup serialization build the state. Fresh runs begin with
an identical erased battery image sized by the current core, avoiding undefined
unused SRAM contents. The initial image is retained alongside each run. Saves are generated by
the running ROM and shared by the paired controls of the current session. No committed `.sav` or old CPU snapshot starts a scenario.

The bootstrap emits object sizes/field offsets and sprite identities using the
current assembler. Zero-instruction labels permit read-only CPU observation of
the native figure template before the capacity check. Readiness, dialogue and
load markers are observed in the core, using no additional game RAM. Private
error-state fixtures deliberately initialize multiplayer's own fields in fixture
assembly; the host never patches working game RAM or expected outputs.

The test fixture assembly template is tooling in this folder, compiled only into
disposable producers. Production feature assembly continues to live in
`mobile_adapter/rom`.

## Acceptance coverage

| Requirement | Executable contracts |
| --- | --- |
| Disabled byte identity | `disabled_feature_is_byte_identical_to_pristine_counterpart` |
| Minimal integration and ROM space | Directory allowlist, integration review artifact, bank and byte-budget guard; human minimality review remains required |
| Native content and NPC preservation | All compiled map/event/movement inventories; twelve-object pool; deterministic NPC movement; normal conversation/event execution |
| Link-cable ownership | Actual `LinkCommunications` path and persistent local disconnect |
| Save independence and compatibility | Current native main/backup copy observers, complete SRAM equality, bidirectional counterpart loading |
| Priority exit | Overworld, actual observed native menu, scripted input lock, save, malformed/partial/silent/flooded transport, invalid private states during native save and restored online state without adapter |
| Relative frame timing | Equal visible actor load, full native pool and mixed 100/200/300/400 percent speeds with moving peers; native baseline gaps retained |
| Background priority | Measured serial/hook register restoration and multiplayer job deadline crossings |
| Malformed transport | Header/length/type/roster corruption, truncation, silence and sustained flooding against actual cores; exhaustive one-byte mutations and deterministic generated host event sequences |
| Reconnect and stale identity | Controlled two-second transport blackout, successful later responses without another join input; production resume tokens, sequence ordering and obsolete generations |
| ROM compatibility identity | The ROM must contain the current clean counterpart SHA-256 in `MobileCompatibilityHash`; current absence is a product failure, not a waived requirement |
| Duplicate trainer IDs | Four actual cores with equal native IDs and distinct server identities |
| Four-player scanline load | Four ordinary NPCs, player and three peers on the same row |
| Full object load | Player, twelve ordinary records and three private peers in the visible region |
| Palette correctness | Separate seven-NPC scene requiring eleven distinct requested colors, three remote appearances, native color references, night/forced lighting; no palette golden values |
| Temporal rendering | Actual hardware selection of complete figures, local visibility against native control, absence/fairness ceilings, priority shares, conflict-free continuous rendering and conflict removal |

The native object pool cannot represent twelve NPCs plus three additional native
avatars. That full-pool control retains all twelve original objects and measures
the ordinary workload. Separate unsaturated controls match all visible avatars,
positions and appearances. The limitation is explicit in each fixture manifest;
full-pool results are not presented as equal-actor-load evidence. The primary
receiver must contain all twelve ordinary objects. Other receivers are compared
against their native activation/culling, so normal off-screen culling is retained.

## Interpretation and limits

These are finite acceptance scenarios, not a proof covering all possible bytes,
story states, emulator defects or arbitrary source revisions. Cross-core state
conversion is outside the restore test: the receiving core must accept the actual
state format. The default restore test uses a compatible core with adapter disabled.

Exact save tests need equal logical inputs. The current native ROM first produces
one settled seed save per avatar. Both counterparts load the same freshly produced
file. This shares native daily NPC/phone data, lottery values and string padding
without naming those fields or assuming their future values. No historic battery
fixture is used. The constructor then discards only the seed's ordinary object
tables and uses the normal map loader to create the requested actors/events.

Save scenes use a fixed elapsed-game-time/software-RTC input. Ordinary world frames,
interrupts, audio and network remain live. Performance/rendering scenes retain the
normal native clocks. No saved byte is masked or normalized after multiplayer runs.
Ordinary cross-variant reload checks bypass scenario identity/object reconstruction
and exercise the real native continue path. A final capture barrier keeps every
peer attached until all four client results have been collected.

Synthetic engine scenarios are distinguished from pristine build/data guards.
The suite does not claim a full story playthrough on every unmodified release ROM.
It intentionally retains failures from an unfinished multiplayer head. A missing
observer or a scenario that never establishes its requested condition is a setup
failure, not successful acceptance. Arbitrary speeds beyond 400 percent are not
advertised as tested.

Presentation ceilings are explicit engineering acceptance choices in the
assumptions file, not values fitted to observed failures. Native omissions remain
the local-player visibility reference. Fairness is measured within priority
classes, and priority shares do not authorize permanent lower-class starvation.

Fault inputs trigger while the requested invalid state is actually observable,
including during native serialization. The escape input is not sent after an
already completed automatic recovery, where a connection toggle could initiate
a normal new session. Menu/link entry and native save call spans are read-only
core observations. Each remote figure uses its own current facing geometry.

Private state corruption is introduced by a fixture-only script helper during
an ordinary NPC-triggered save, after world initialization. The native control
uses the observed input frame without referring to multiplayer symbols.
Native eligibility comes from the singleplayer control on every frame, matched
by original map-object identity. Avatar doubles used to equalize workload are
excluded, and multiplayer cannot waive visible NPCs by hiding its own demand.
Reconnect scenarios cover both a two-second server blackout and one isolated
client connection reset; successful responses must cross the restored connection.

Palette checks require actual complete presentation of every remote figure in
the measured window. Correct cached colors with no visible peers do not pass.
`palette_oracle_rejects_unpresented_peers` checks this observation rule without
starting an emulator.

The suite copies only ROM build inputs. Disposable fixture compiler trees are
removed after compilation; ROMs, symbols, maps and the generated event header
remain. The native Rust Bridge build cache is shared between runs, and each run
freezes its own executable. Raw JSON evidence is retained. Contracts share a
scenario execution and a bounded two-scene in-memory evidence cache; different
contract names do not start duplicate emulator sessions.

Before builds/execution and after evidence writes, the suite checks for a 10 GiB
free-space reserve plus 512 MiB headroom. On low space it removes only identified
inactive acceptance directories in the test cache, oldest first. Active runs hold
OS leases for the entire test executable. Toolchains, worktrees and unrelated
files are excluded. If safe reclamation cannot restore the reserve, tests stop.
Supporting workspace/storage checks run without an emulator:

```sh
python3 -m unittest discover -s mobile_adapter/tests/support -p 'test_runtime.py'
```
