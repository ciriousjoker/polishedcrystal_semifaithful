# Player bag adapter

Status: integrated with positive full-fight evidence; the inner-retry extension
is prepared pending focused validation.

`battle_bag.py` defines player inventory independently of the enemy trainer's
two item slots. `initialize_bag` writes initial inventory only. Item IDs, pockets
and battle contexts come from assembled `ItemAttributes`; pocket capacities come
from symbol spans. An explicit source macro guard rejects a changed attribute
layout. Duplicate stacks preserve their order and quantities.

`BagAction` selects an item stack, an optional party member and an optional move.
`BagInput` supplies cursors and joypad input at a caller-classified input leaf.
It does not perform item effects, consume items, choose turn consumption, or
assign `wItemEffectSucceeded`. The latter aliases `wBattlePlayerAction` in this
ROM and must remain owned by source control flow.

The intended control path is `BattleMenu_Pack` to `BattlePack`, its normal
scrolling selection and `ItemSubmenu.Use`, then actual `DoItemEffect`, party
selection, PP selection if needed, and `TossItem`. The low-level direct
`_DoMenuJoypadLoop` call in `ScrollingMenuJoyAction` needs an explicit input
boundary. Its caller must execute: it resolves item IDs, stack quantities and
indices from the real inventory list. The use submenu and party and move menus
likewise retain their source callers.

The adapter accepts ineffective target choices. A caller may cancel a retry or
select a different action; exhausting the callback bound rejects the run. A
failed item attempt must not become a successful turn through host assignment.

## Source cases required before support claims

| Case | Source behavior to observe |
| --- | --- |
| Potion on damaged active member | Party and active HP update, stack decrements, turn used |
| Potion on damaged reserve | Reserve HP changes, active HP unchanged |
| Potion at full HP | No consumption; source retry and cancellation |
| Potion on fainted member or egg | Source rejects, no consumption |
| Last item in a stack | Source compacts pocket and moves terminator |
| Duplicate stacks | Selected occurrence decrements, source preserves order |
| Full Restore with HP and status damage | Both heal; confused active handling follows source |
| Status-specific heal, wrong status, Full Heal confusion | Status selection and success versus retry |
| Revive, Max Revive, Revival Herb | Half/full HP, faint requirement, happiness effect |
| Ether, Max Ether | Selected mon and selected move, PP cap and active synchronization |
| Elixir, Max Elixir | All existing moves, PP Up bits preserved, no-effect case |
| Transformed active PP restoration | Party PP changes, transformed PP synchronization follows source |
| Energy Powder, Energy Root, Heal Powder | Actual heal and happiness change |
| Persim Berry | Active confusion cleared by source, no party selection |
| Guard Spec, Dire Hit, X items | Source stages, guards, critical state, happiness, Mirror Herb effects |
| Repeated Guard Spec or Dire Hit | Effect result 2 becomes 0 in `ItemSubmenu.BattleOnly` |
| Poke Ball in trainer battle | Trainer blocks catch, ball is consumed and turn is used |
| Poke Doll in trainer battle | Source rejects without consumption or battle end |
| Rare Candy, PP Up, EV berries | Battle context is unusable, source context handles rejection |
| Cancel at item, party or move selection | No effect, no consumption, no spent turn |

Each supported case needs actual-ROM segment comparisons and at least a whole
fight exercising its effect and later consequences. Routine parity alone is not
whole-fight evidence. UI projection and the RNG event contract remain separate
validation requirements. Item-attribute or handler Assembly changes must rebuild
and invalidate stale native artifacts before rerunning these cases.

The positive restorative source family now has separate full-fight evidence for
18 items; see `battle_controller_bag_family.md`. `CancelBagAction` is a nested
input choice for explicit rejected-attempt cancellation, not a top-level turn
action. `ineffective_item_prompt` recognizes the actual source no-effect text
caller and checks its ROM call/return scaffold. The controller may then request
same-stack retarget or cancellation from an explicit detached actor callback.
It never infers rejection from normal pocket navigation. Four focused complete
source fights passed with HP/PP retargets and explicit cancellation, including
unchanged hidden enemy intent and exact native replay. See
`battle_controller_bag_retry_check.md` for evidence and rejection limits.
