"""Conditional source switch-prefix and whole-send-in profiles.

Default mode stops before .done_ability without presentation returns. The
separate --full profile runs the whole send-in under named rendering returns.
Neither mode establishes ordinary frontend fidelity. Build only while holding
the coordinator's CPU slot.
"""
import argparse
import json
import pathlib

import battle_env_profile as base
import stateful_lifting

ENTRIES = ('SendInUserPkmn', 'GetUserSwitchTarget', 'GetAbility.Function',
           '_ReturnFarCall', '_CopyBytes')
BOUNDARIES = ('SendInUserPkmn.done_ability', 'GetPlayerSwitchTarget',
              'GetUserSwitchTarget.enemy_switch')
# Forced random displacement is a separate mechanic, not faint replacement.
REJECT = (*base.REJECT, 'GetUserSwitchTarget.random_select')

# Separate conditional rendering projection. Computational callers such as
# UserSentOutText, SendOutPlayerMon and Function_SetEnemyPkmnAndSendOutAnimation
# execute, including their battle RAM writes. None is an identity hook.
PRESENTATION = (*base.PRESENTATION, 'BattleTextbox', 'GetCGBLayout',
    'SetDefaultBGPAndOBP', 'LoadTileMapToTempTileMap', 'ApplyTilemapInVBlank',
    'GetBackpic', 'GetFrontpicOrGhostpic', 'BattleAnimateFrontpic', 'PlayStereoCry')
SENDIN_ENTRIES = (*ENTRIES, 'SpikesDamage', 'RunEntryAbilities', 'OwnTempoAbility', 'SubtractHP')


def build(rom, symbols, source_root, build_dir, full=False):
    base.verify_audit(rom, symbols, source_root)
    syms = stateful_lifting.symbols(symbols)
    data = pathlib.Path(rom).read_bytes()
    entries = SENDIN_ENTRIES if full else ENTRIES
    boundaries = PRESENTATION if full else BOUNDARIES
    excluded = list(REJECT)
    if full:
        # These are meaningful optional mechanics, not presentation returns.
        # Baton Pass/substitute and trainer final speech are outside the fixture.
        excluded += ['LoadAnim', 'BattleWinSlideInEnemyTrainerFrontpic',
                     'PlayMusic', 'SlideEnemyPicOut']
    for _ in range(8):
        traps = [syms['Crash'], *(syms[n] for n in boundaries)]
        traps += [syms[e] if isinstance(e, str) else tuple(e) for e in excluded]
        try:
            stateful_lifting.compile_rom(data, [syms[e] for e in entries], traps)
            break
        except ValueError as error:
            prefix = 'Unsupported reachable instructions: '
            if str(error).startswith('Non-ROM instruction '):
                bank, pc = str(error).split()[-1].split(':')
                added = [(int(bank, 16), int(pc, 16))]
            elif str(error).startswith(prefix):
                added = [(r['bank'], r['pc']) for r in json.loads(str(error)[len(prefix):])]
            else:
                raise
            if not set(added) - {tuple(e) for e in excluded if not isinstance(e, str)}:
                raise
            excluded.extend(added)
    else:
        raise ValueError('Switch closure exceeded bounded iterations: ' + repr(excluded))
    result = stateful_lifting.build(rom, symbols, entries, build_dir, source_root,
                                   boundaries=boundaries, excluded=excluded)
    (pathlib.Path(build_dir) / 'party-profile.json').write_text(json.dumps(dict(
        kind='conditional-headless-sendin' if full else 'computational-switch-prefix',
        entries=entries, stop_events=boundaries, excluded=excluded,
        profile_source=pathlib.Path(__file__).read_text()), indent=2) + '\n')
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rom', type=pathlib.Path, required=True)
    parser.add_argument('--sym', type=pathlib.Path, required=True)
    parser.add_argument('--source-root', type=pathlib.Path, required=True)
    parser.add_argument('--build-dir', type=pathlib.Path, required=True)
    parser.add_argument('--full', action='store_true')
    args = parser.parse_args()
    result = build(args.rom, args.sym, args.source_root, args.build_dir, args.full)
    print(json.dumps(dict(blocks=result['blocks'], library=result['library_path'])))
