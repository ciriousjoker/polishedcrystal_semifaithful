# Retained positive Spikes and Toxic Spikes evidence

`battle_hazards_collect.py` packages only the actual source-parity executions in
`hazards-provenance/report.json`, using existing retention, binding and strict
gate helpers. It does not import a driver, load a native library, execute battles
or change historical evidence. Its explicit allowlist pins 15 executed host
files and the original checkpoint identity.

All 18 original full fights and their initial 8192-byte WRAM are byte-retained.
Four flying immunity cases are excluded controls and claim no rules. Fourteen
positive cases claim the source-derived `move:SPIKES`,
`effect:EFFECT_SPIKES`, `move:TOXIC_SPIKES` and
`effect:EFFECT_TOXIC_SPIKES`, `field:HAZARDS_SPIKES` and
`field:HAZARDS_TOXIC_SPIKES` keys. Grounded Toxic Spikes additionally claims
`status:PSN`, and two layers additionally claims `status:TOX`, from actual
zero-to-poison transitions and exact first residual damage. Source mask
definitions and assembled ROM placement/status bytes are checked. The effect names are read from the current
source inventory rather than assumed by the collector. There are no claims
for entrant abilities or unactivated branches.

Explicit validators check source fixture identity, both setter sides, each
positive layer placement, actual switch and opposing Splash, Spikes damage at
one eighth, one sixth and one quarter max HP, Toxic Spikes normal/toxic status
and first residual damage, flying type immunity, Poison type absorption, and
entry KO followed by the requested replacement's own Spikes damage. Flying
controls remain excluded even though their placement interval is positive.
The entry KO raw witness omits a separate fainted-HP snapshot; that observation
and setter/placement invariance rely on the reviewed executed driver assertions.
No missing snapshot is fabricated. Actual replacement choices and active member
HP are independently checked against retained records.

The original fresh report and raw cases bind actual hosts, Oracle C/library,
loaded mGBA and ABI sources. The executed O3 manifest is
`dec3cb17a34aeeb9e0f36e44590d0890b7d0816ddff98fc7bb0346b9b32737b0`.
Current source identities must match it. Existing helpers check ROM/SYM, original
build receipt/log, runtime abilities, NO_EXP and set style, source-terminal
whole fights and retained differential records. Exact replay success relies on
the original runner's successful per-case assertions and report. Output must be
empty and strict gate failures may contain only still-missing required rules.

This is conditional same-presentation-hook, IID DIV gameplay evidence, not
ordinary frontend continuation or complete M2 acceptance. Old `hazards` reports
without actual Oracle provenance are ineligible. Use the same explicit CLI
arguments as `battle_ability_collect.py`; the external task RESULT contains the
exact metadata-only validation command and results. Both original positive
reports passed; all ten consistent-hash semantic/provenance controls rejected
without a ledger. Canonical merging with prior evidence produced 188 tested
keys, 665 missing rules and zero invalid evidence in the unchanged strict gate.
Original inputs and prior canonical collection bytes remained unchanged.
