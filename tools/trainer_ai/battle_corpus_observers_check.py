"""Synthetic observer false-positive checks, not simulator/ROM evidence."""
from copy import deepcopy
from dataclasses import replace
from types import SimpleNamespace
from battle_corpus_observers import MoveFrame, observe_move


def observation():
    member = dict(index=0, active=True, HP=80, MaxHP=100, status=0, item=0,
                  ability=9, moves=(1, 2, 0, 0), pp=(20, 20, 0, 0), types=(1, 1))
    field = dict(volatile={'confused': False}, stages=(7,) * 7,
                 reflect_turns=0, light_screen_turns=0, safeguard_turns=0, mist_turns=0,
                 spikes=0, toxic_spikes=0, FutureSightCount=0, TrappingMove=0,
                 WrapCount=0, PerishCount=0, ConfuseCount=0, TurnsTaken=0, EncoreCount=0)
    return dict(teams=((deepcopy(member),), (deepcopy(member),)),
                battlefield=(deepcopy(field), deepcopy(field)), weather=0,
                weather_turns=0, trick_room=0)


def check():
    bindings = dict(inventory_sha256='source', moves={'PROBE': 1, 'SPLASH': 2},
                    status_masks=dict(PSN=8, BRN=16, FRZ=32, PAR=64, TOX=128),
                    sleep_mask=7, encore_duration_mask=15, weather=dict(WEATHER_NONE=0, WEATHER_RAIN=1, WEATHER_SUN=2,
                                               WEATHER_SANDSTORM=3, WEATHER_HAIL=4), neutral_ability=9)
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
