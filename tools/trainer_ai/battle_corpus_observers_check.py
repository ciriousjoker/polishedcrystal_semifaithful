"""Synthetic observer false-positive checks, not simulator/ROM evidence."""
from copy import deepcopy
from dataclasses import replace
from types import SimpleNamespace
from battle_corpus_observers import MoveFrame, observe_move


def observation():
    member = dict(index=0, active=True, HP=80, MaxHP=100, status=0, item=0,
                  ability=9, species=116, form=0, egg=False,
                  moves=(1, 2, 0, 0), pp=(20, 20, 0, 0), types=(1, 1))
    field = dict(volatile={'confused': False}, stages=(7,) * 7,
                 reflect_turns=0, light_screen_turns=0, safeguard_turns=0, mist_turns=0,
                 spikes=0, toxic_spikes=0, FutureSightCount=0, TrappingMove=0,
                 WrapCount=0, PerishCount=0, ConfuseCount=0, TurnsTaken=0, EncoreCount=0)
    return dict(turn=0, teams=((deepcopy(member),), (deepcopy(member),)),
                battlefield=(deepcopy(field), deepcopy(field)), weather=0,
                weather_turns=0, trick_room=0)


def check():
    bindings = dict(inventory_sha256='source', moves={'PROBE': 1, 'SPLASH': 2},
                    status_masks=dict(PSN=8, BRN=16, FRZ=32, PAR=64, TOX=128),
                    sleep_mask=7, encore_duration_mask=15, weather=dict(WEATHER_NONE=0, WEATHER_RAIN=1, WEATHER_SUN=2,
                                               WEATHER_SANDSTORM=3, WEATHER_HAIL=4), neutral_ability=9,
                    neutral_species=116)
    case = SimpleNamespace(inventory_sha256='source', actor_side=0, move='PROBE',
                           effect='EFFECT_NORMAL_HIT', candidate_rules=('move:PROBE', 'effect:EFFECT_NORMAL_HIT'))
    before, after = observation(), observation()
    after['teams'][0][0]['pp'] = (19, 20, 0, 0)
    frame = MoveFrame(before, after, 0, 1,
                      ('BattleCommand_usedmovetext', 'BattleCommand_applydamage'), 0)
    assert observe_move(case, [frame], bindings).state == 'unsatisfied'  # PP alone
    after['teams'][1][0]['HP'] = 60
    assert observe_move(case, [frame], bindings).state == 'activated'
    assert observe_move(case, [replace(frame, attack_missed=1)], bindings).state == 'unsatisfied'
    assert observe_move(case, [replace(frame, executed_move=2)], bindings).state == 'blocked'
    assert observe_move(case, [replace(frame, phase='whole-turn')], bindings).state == 'blocked'
    assert observe_move(case, [replace(frame, source_commands=())], bindings).state == 'blocked'
    assert observe_move(case, [frame], {**bindings, 'inventory_sha256': 'stale'}).state == 'blocked'
    controlled = replace(frame, phase='controlled-source-turn', source_commands=(),
                         opponent_executed_move=2, move_execution_verified=True)
    assert observe_move(case, [controlled], bindings).state == 'activated'
    assert observe_move(case, [replace(controlled, move_execution_verified=False)], bindings).state == 'blocked'
    poisoned = deepcopy(before)
    poisoned['teams'][0][0]['status'] = 8
    assert observe_move(case, [replace(controlled, before=poisoned)], bindings).state == 'blocked'
    weather = deepcopy(before)
    weather['weather'] = 3
    assert observe_move(case, [replace(controlled, before=weather)], bindings).state == 'blocked'
    item = deepcopy(before)
    item['teams'][1][0]['item'] = 1
    assert observe_move(case, [replace(controlled, before=item)], bindings).state == 'blocked'
    case.effect = 'EFFECT_POISON'
    status_after = deepcopy(before)
    status_after['teams'][1][0]['status'] = 8
    status_frame = replace(controlled, after=status_after)
    assert observe_move(case, [status_frame], bindings).state == 'activated'
    assert observe_move(case, [replace(status_frame, after=before)], bindings).state == 'unsatisfied'
    case.effect = 'EFFECT_TOXIC'
    assert observe_move(case, [status_frame], bindings).state == 'unsatisfied'
    status_after['teams'][1][0]['status'] = 136
    assert observe_move(case, [status_frame], bindings).state == 'activated'
    case.effect = 'EFFECT_SLEEP'
    status_after['teams'][1][0]['status'] = 3
    assert observe_move(case, [status_frame], bindings).state == 'activated'
    case.effect = 'EFFECT_BURN_HIT'
    assert observe_move(case, [status_frame], bindings).state == 'blocked'  # secondary needs isolated span
    # Synthetic boundaries are predicate inputs only. They are never evidence
    # of an executed source callsite, whole-fight oracle comparison or replay.
    interval = dict(turn=0, side=0, move=1,
        used_call=dict(source='engine/battle/used_move_text.asm', start='DisplayUsedMoveText',
                       target='StdBattleTextbox', bank=1, return_pc=0x4103, scaffold_hex='cd0042c30043'),
        end_call=dict(source='engine/battle/core.asm', start='PerformMove',
                      target='LoadTileMapToTempTileMap', bank=2, return_pc=0x5103, scaffold_hex='cd0052'))
    secondary = deepcopy(before)
    secondary['teams'][1][0]['status'] = 16
    secondary['teams'][1][0]['HP'] -= 5
    local = replace(controlled, phase='source-action-span', source_interval=interval,
                    observation_read_only_verified=True, opponent_executed_move=None, after=secondary)
    assert observe_move(case, [local], bindings).state == 'activated'
    assert observe_move(case, [replace(local, attack_missed=1)], bindings).state == 'unsatisfied'
    assert observe_move(case, [replace(local, source_interval=None)], bindings).state == 'blocked'
    assert observe_move(case, [replace(local, observation_read_only_verified=False)], bindings).state == 'blocked'
    wrong_turn = {**interval, 'turn': 1}
    assert observe_move(case, [replace(local, source_interval=wrong_turn)], bindings).state == 'blocked'
    no_damage = deepcopy(secondary)
    no_damage['teams'][1][0]['HP'] = before['teams'][1][0]['HP']
    assert observe_move(case, [replace(local, after=no_damage)], bindings).state == 'unsatisfied'
    no_status = deepcopy(secondary)
    no_status['teams'][1][0]['status'] = 0
    assert observe_move(case, [replace(local, after=no_status)], bindings).state == 'unsatisfied'
    for key, value in (('species', 1), ('ability', 10), ('item', 1), ('status', 8)):
        confounded = deepcopy(before)
        confounded['teams'][1][0][key] = value
        assert observe_move(case, [replace(local, before=confounded)], bindings).state == 'blocked'
    case.effect = 'EFFECT_CONFUSE_HIT'
    confused = deepcopy(no_status)
    confused['battlefield'][1]['volatile']['confused'] = True
    confused['battlefield'][1]['ConfuseCount'] = 3
    assert observe_move(case, [replace(local, after=confused)], bindings).state == 'activated'
    confused['battlefield'][1]['ConfuseCount'] = 0
    assert observe_move(case, [replace(local, after=confused)], bindings).state == 'unsatisfied'
    case.effect = 'EFFECT_POISON_HIT'
    assert observe_move(case, [local], bindings).state == 'unsatisfied'  # Wrong secondary status.
    # Local secondary stages require damage and the intended side/stat/direction.
    for effect, target_side, stat, direction in (
            ('EFFECT_ATTACK_UP_HIT', 0, 0, 1),
            ('EFFECT_DEFENSE_UP_HIT', 0, 1, 1),
            ('EFFECT_SPEED_UP_HIT', 0, 2, 1),
            ('EFFECT_DEFENSE_DOWN_HIT', 1, 1, -1),
            ('EFFECT_SPEED_DOWN_HIT', 1, 2, -1),
            ('EFFECT_ACCURACY_DOWN_HIT', 1, 5, -1)):
        case.effect = effect
        stage_after = deepcopy(no_status)
        stages = list(stage_after['battlefield'][target_side]['stages'])
        stages[stat] += direction
        stage_after['battlefield'][target_side]['stages'] = tuple(stages)
        stage_frame = replace(local, after=stage_after)
        assert observe_move(case, [stage_frame], bindings).state == 'activated'
        assert observe_move(case, [replace(stage_frame, after=no_status)], bindings).state == 'unsatisfied'
        stage_only = deepcopy(stage_after)
        stage_only['teams'][1][0]['HP'] = before['teams'][1][0]['HP']
        assert observe_move(case, [replace(stage_frame, after=stage_only)], bindings).state == 'unsatisfied'
        assert observe_move(case, [replace(stage_frame, attack_missed=1)], bindings).state == 'unsatisfied'
        for wrong_side, wrong_stat, wrong_direction in (
                (target_side ^ 1, stat, direction),
                (target_side, (stat + 1) % 7, direction),
                (target_side, stat, -direction)):
            wrong = deepcopy(no_status)
            wrong_stages = list(wrong['battlefield'][wrong_side]['stages'])
            wrong_stages[wrong_stat] += wrong_direction
            wrong['battlefield'][wrong_side]['stages'] = tuple(wrong_stages)
            assert observe_move(case, [replace(stage_frame, after=wrong)], bindings).state == 'unsatisfied'
        assert observe_move(case, [replace(stage_frame, before=item)], bindings).state == 'blocked'
        assert observe_move(case, [replace(stage_frame, phase='controlled-source-turn')], bindings).state == 'blocked'
    case.effect = 'EFFECT_ALL_UP_HIT'
    all_up = deepcopy(no_status)
    all_up['battlefield'][0]['stages'] = (8, 8, 8, 8, 8, 7, 7)
    assert observe_move(case, [replace(local, after=all_up)], bindings).state == 'activated'
    all_up['battlefield'][0]['stages'] = (8, 8, 8, 8, 7, 7, 7)
    assert observe_move(case, [replace(local, after=all_up)], bindings).state == 'unsatisfied'
    # A target may reach zero HP while still active before ResolveFaints.
    case.effect = 'EFFECT_NORMAL_HIT'
    knockout = deepcopy(before)
    knockout['teams'][1][0]['HP'] = 0
    damage_frame = replace(local, after=knockout)
    assert observe_move(case, [damage_frame], bindings).state == 'activated'
    assert observe_move(case, [replace(damage_frame, after=before)], bindings).state == 'unsatisfied'
    user_only = deepcopy(before)
    user_only['teams'][0][0]['HP'] = 0
    assert observe_move(case, [replace(damage_frame, after=user_only)], bindings).state == 'unsatisfied'
    assert observe_move(case, [replace(damage_frame, attack_missed=1)], bindings).state == 'unsatisfied'
    replacement = deepcopy(knockout)
    replacement['teams'][1][0]['index'] = 1
    assert observe_move(case, [replace(damage_frame, after=replacement)], bindings).state == 'blocked'
    for confounded in (item, poisoned, weather):
        assert observe_move(case, [replace(damage_frame, before=confounded)], bindings).state == 'blocked'
    assert observe_move(case, [replace(damage_frame, source_interval=None)], bindings).state == 'blocked'
    assert observe_move(case, [replace(damage_frame, observation_read_only_verified=False)], bindings).state == 'blocked'
    case.effect = 'EFFECT_LEECH_HIT'
    drained = deepcopy(before)
    drained['teams'][1][0]['HP'] = 60
    drained['teams'][0][0]['HP'] = 90
    drain_frame = replace(local, after=drained)
    assert observe_move(case, [drain_frame], bindings).state == 'activated'
    no_heal = deepcopy(drained)
    no_heal['teams'][0][0]['HP'] = 80
    assert observe_move(case, [replace(drain_frame, after=no_heal)], bindings).state == 'unsatisfied'
    full_before, full_after = deepcopy(before), deepcopy(drained)
    full_before['teams'][0][0]['HP'] = full_after['teams'][0][0]['HP'] = 100
    assert observe_move(case, [replace(drain_frame, before=full_before, after=full_after)], bindings).state == 'unsatisfied'
    capped_before = deepcopy(before)
    capped_before['teams'][0][0]['HP'] = 95
    assert observe_move(case, [replace(drain_frame, before=capped_before, after=full_after)], bindings).state == 'activated'
    no_target_loss = deepcopy(drained)
    no_target_loss['teams'][1][0]['HP'] = 80
    assert observe_move(case, [replace(drain_frame, after=no_target_loss)], bindings).state == 'unsatisfied'
    assert observe_move(case, [replace(drain_frame, attack_missed=1)], bindings).state == 'unsatisfied'
    assert observe_move(case, [replace(drain_frame, before=item)], bindings).state == 'blocked'
    assert observe_move(case, [replace(drain_frame, source_interval=None)], bindings).state == 'blocked'
    for side, value in ((0, 101), (1, -1)):
        invalid_hp = deepcopy(drained)
        invalid_hp['teams'][side][0]['HP'] = value
        assert observe_move(case, [replace(drain_frame, after=invalid_hp)], bindings).state == 'unsatisfied'
    case.effect = 'EFFECT_RECOIL_HIT'
    assert observe_move(case, [drain_frame], bindings).state == 'unsatisfied'  # Healing is not recoil.
    recoiled = deepcopy(drained)
    recoiled['teams'][0][0]['HP'] = 70
    recoil_frame = replace(local, after=recoiled)
    assert observe_move(case, [recoil_frame], bindings).state == 'activated'
    assert observe_move(case, [replace(recoil_frame, after=no_heal)], bindings).state == 'unsatisfied'
    recoil_only = deepcopy(recoiled)
    recoil_only['teams'][1][0]['HP'] = 80
    assert observe_move(case, [replace(recoil_frame, after=recoil_only)], bindings).state == 'unsatisfied'
    assert observe_move(case, [replace(recoil_frame, attack_missed=1)], bindings).state == 'unsatisfied'
    nonneutral = deepcopy(before)
    nonneutral['teams'][0][0]['ability'] = 10  # Suppression/interactions need another fixture.
    assert observe_move(case, [replace(recoil_frame, before=nonneutral)], bindings).state == 'blocked'
    case.effect = 'EFFECT_RAIN_DANCE'
    wet = deepcopy(before)
    wet['weather'], wet['weather_turns'] = 1, 4
    assert observe_move(case, [replace(controlled, after=wet)], bindings).state == 'activated'
    wet['weather_turns'] = 0
    assert observe_move(case, [replace(controlled, after=wet)], bindings).state == 'unsatisfied'
    case.effect = 'EFFECT_HEAL'
    healed = deepcopy(before)
    healed['teams'][0][0]['HP'] = 100
    assert observe_move(case, [replace(controlled, after=healed)], bindings).state == 'activated'
    case.effect = 'EFFECT_ATTACK_UP_2'
    raised = deepcopy(before)
    raised['battlefield'][0]['stages'] = (9, 7, 7, 7, 7, 7, 7)
    assert observe_move(case, [replace(controlled, after=raised)], bindings).state == 'activated'
    case.effect = 'EFFECT_SPLASH'
    splash = deepcopy(before)
    splash['teams'][0][0]['pp'] = (19, 20, 0, 0)
    splash['battlefield'][0]['TurnsTaken'] = 1
    assert observe_move(case, [replace(controlled, after=splash)], bindings).state == 'activated'
    for field in splash['battlefield']:
        field['EncoreCount'] = 16  # Source move-index bookkeeping, zero duration.
    assert observe_move(case, [replace(controlled, after=splash)], bindings).state == 'activated'
    for side in (0, 1):
        duration = deepcopy(splash)
        duration['battlefield'][side]['EncoreCount'] = 17  # Actual duration 0 -> 1.
        assert observe_move(case, [replace(controlled, after=duration)], bindings).state == 'unsatisfied'
        stage = deepcopy(splash)
        stage['battlefield'][side]['stages'] = (8, 7, 7, 7, 7, 7, 7)
        assert observe_move(case, [replace(controlled, after=stage)], bindings).state == 'unsatisfied'
        flag = deepcopy(splash)
        flag['battlefield'][side]['volatile']['confused'] = True
        assert observe_move(case, [replace(controlled, after=flag)], bindings).state == 'unsatisfied'
        counter = deepcopy(splash)
        counter['battlefield'][side]['ConfuseCount'] = 1
        assert observe_move(case, [replace(controlled, after=counter)], bindings).state == 'unsatisfied'
    splash['teams'][1][0]['HP'] -= 1
    assert observe_move(case, [replace(controlled, after=splash)], bindings).state == 'unsatisfied'
    case.effect = 'EFFECT_METRONOME'
    assert observe_move(case, [controlled], bindings).state == 'blocked'
    print('Observer synthetic checks passed; no game mechanics were executed.')


if __name__ == '__main__':
    check()
