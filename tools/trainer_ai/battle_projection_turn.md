# Mixed HP/HUD source continuation probe

Run `battle_projection_turn_check.py --project /Volumes/Storage/Development/agentic_projects/polished_ai`
with the frozen controller snapshot in `build/battle-controller/snapshots`.
The probe loads no lifted native engine. It compares two independent mGBA
executions of actual BattleTurn.do_move followed by HandleBetweenTurnEffects.
One context uses the declared conditional presentation hooks. The other executes
all descendants of real UpdateHPBar, UpdatePlayerHUD and UpdateEnemyHUD, including
VBlank/LCD/DMA. Other presentation roots remain explicitly conditional outside
those real contexts. Peripheral clocks and VRAM persist between source phases.

Source Random executes in the reference context, recording A/F/hRandomAdd/Sub.
The mixed context replays combat outputs while real UI draws stay separate.
Replay validates immediate return and source logical caller identity by decoding
the actual FarCall/farjp/DoNothing frame and BattleRandomRange's preserved BC.
This is a logical randomness contract, not a hardware DIV timing distribution.

The fixed-level NO_EXP snapshot passes both source phases with equal full declared
gameplay regions, phase PC/bank/SP, CPU registers and three consumed draws. Logical
origins identify the source hit, critical and damage-spread checks separately.
The source block wBattle..wBattleEnd is compared without internal masking, alongside
both parties, enemy active struct, inventory, actor/action/move fields and damage.
Raw RAM differences and all register values remain in the JSON evidence.

A conservative trace records first reads of UI-written bytes before a combat
write. It includes equal-value writes and interrupt/helper reads, so it is a
source consumer diagnostic rather than proof of gameplay divergence. The current
fixture produces no gameplay mismatch. Failure assertions preserve the report
and snapshots first; unsupported paths are never repaired by broadening masks.

This is one attacking caller and complete endturn, not the full BattleTurn action
selection/order controller, a complete trainer-fight frontend proof, or coverage
of every move/status combination. The remaining identity roots still require
separate validation. Source-specific frame decoding rejects changed observed
call order and does not promise arbitrary stack unwinding.

Evidence is in build/battle-projection/turn-v3/report.json and phase snapshots.
Original immediate-wrapper-only evidence is retained under initial-revision.
The intermediate decoder that still found only the shared Range wrapper is
retained under farcall-only-revision; its distinct-origin assertion failed as
intended. The final decoder passed all phase, state and distinct-origin checks.
