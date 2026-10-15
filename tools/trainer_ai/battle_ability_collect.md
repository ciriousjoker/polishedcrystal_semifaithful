# Retained positive Trace and Intimidate evidence

`battle_ability_collect.py` packages the two actual source-parity full fights in
`ability-projection-initial-wram/report.json`. It never imports a battle driver,
loads a native library, executes a fight, or manufactures historical metadata.
It uses the existing family byte-retention, execution binding and strict gate
helpers. Output must be empty.

The reviewed checkpoint explicitly pins all 16 executed host files, including
the ability runner. Original report, raw cases, initial 8192-byte WRAM and host
files are retained unchanged. The actual executed O3 manifest is
`dec3cb17a34aeeb9e0f36e44590d0890b7d0816ddff98fc7bb0346b9b32737b0`.
Source and assembled ROM/SYM identities, original build receipt/log, actual
loaded Oracle C/library/mGBA/ABI provenance and ordinary trainer options are
bound using existing gate logic. Every initial WRAM record is checked for
abilities enabled, NO_EXP and set style by `runtime_case_options`.

The report omits its top-level generator identity. The helper input view derives
that value from the actual first raw case, checks it against the manifest and
requires it in every raw case. The original report bytes stay unchanged. Raw
ability cases omit per-case host/Oracle duplication and comparison summary
rows. Source comparison and exact-replay success rely on the original successful
report and the pinned executed runner, which asserts all recorded segment replay
positions, states, turns, winner, displays and callbacks. No absent observations
are reconstructed.

Only `ability:TRACE` and `ability:INTIMIDATE` can be claimed. Explicit checks
require Porygon's Trace display and active cache replacement with Intimidate,
the resulting attack reductions, Gyarados's ordinary entry and switch/reentry,
and subsequent physical damage. Conversion is fixture context and adds no move
claim. Other species' ability identities and unactivated branches add no claim.
The projection is conditional on the same presentation hooks and IID DIV input;
this is not ordinary frontend continuation proof or M2 acceptance.

Use the common required arguments shown by `--help`: `--root`, `--inventory`,
`--report`, `--host-checkpoint`, `--manifest`, `--rom`, `--symbols`,
`--build-receipt`, `--build-log`, `--oracle-library`, `--mgba-library`, `--output`.
Metadata-only validation completed under coordinator CPU allocation: both
original ability cases retain two rules, and wrong copied ability, missing WRAM,
changed host and wrong actual Oracle bytes reject without a ledger. The external
task RESULT records exact commands, hashes and unchanged strict gate results.
