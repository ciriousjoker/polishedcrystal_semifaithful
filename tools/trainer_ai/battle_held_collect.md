# Retain held restorative switch coverage

`battle_held_collect.py` accepts the three provenance-complete held-restorative
reports through repeated `--report` arguments. It requires the explicitly pinned
15-file host checkpoint and binds the exact report, case, initial WRAM, native
manifest, ROM/SYM/generator, build receipt/log, Oracle C/dylib/loaded libmGBA and
State/Hook ABI identities. It never uses mutable current host files as executed
sources. The unchanged strict gate reads actual runtime options from each
retained post-reset WRAM snapshot.

The validator derives item IDs, held effects and parameters from matching source
and ROM tables. It checks reserve-to-active switching against Splash, holder
and target, positive HP/status/PP effects, toxic/burn residual ordering, item
consumption or retained Leftovers, party/active cache consistency, first typed
actions and the isolated fixture. Each case needs a successful ordinary source
terminal, complete differential segment records and report-confirmed exact
native replay. The original replay assertion is trusted only with its frozen
executed runner, just as in bag collection.

Only positively activated `held-effect` keys are claimed. Full-HP Leftovers
cases are retained as excluded controls and contribute no claims. The known
22 fights contain 20 positive activations and two controls; the four effects
are HELD_BERRY, HELD_LEFTOVERS, HELD_HEAL_STATUS and HELD_RESTORE_PP. The raw
witness omits unrelated-party snapshots, so their invariance remains explicitly
bound to the reviewed executed driver assertions. It is not independently
reconstructed by this collector. Lum confusion, Unnerve, theft, orbs and other
combinations remain unproved. Existing report limitations remain attached.

Original reports, cases, WRAM and execution/checkpoint bytes are copied without
modification into a new empty directory. Derived records use root-relative
references compatible with the unchanged merger. Missing required rules remain
visible; a successful collection does not mean M2 acceptance or frontend parity.
Any other gate error rejects before ledger emission. Ability and historical
hazard evidence are outside this collector.

Authorized metadata-only validation retained all 22 executed fights as 20
positive cases and two excluded controls. The positives contain 11,227 state
comparisons; including controls, the original execution contains 100 source
turns and 12,219 comparisons. Four held-effect keys were promoted. Wrong heal,
wrong holder, control promotion, missing initial WRAM, changed executed host
and wrong Oracle bytes each reject with exit 2 and no ledger. The two existing
bag negative controls also reject, and the old/new bag output bytes match.

The unchanged merger produced `build/battle-corpus/combined-with-held-restorative-o3`
in the external run. Its separate strict gate has 168 tested keys, 685 missing
required rules and no invalid evidence. Original collections stayed unchanged.
Evidence and exact commands are in `subagents/m2-held-collection-01/artifacts/validation-01`
and `artifacts/validate_packaging.py`. No native module, fight or build ran.
