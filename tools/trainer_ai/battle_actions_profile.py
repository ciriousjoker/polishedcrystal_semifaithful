"""Source voluntary-action closure with explicit conditional rendering returns."""
import argparse
import hashlib
import re
import json
import pathlib
import battle_env_profile as base
import battle_party_profile as party
import stateful_lifting

ENTRIES = (*party.SENDIN_ENTRIES, 'DeferredSwitch', 'ForceDeferredSwitch',
           'UserCanSwitch', 'UpdateMoveData', 'RunSwitchAbilities',
           'EnemyUsedPotion', 'EnemyUsedSuperPotion', 'EnemyUsedHyperPotion',
           'EnemyUsedMaxPotion', 'EnemyUsedFullHeal', 'EnemyUsedFullRestore',
           'AI_TryItem.used_item', *base.ENTRIES,
           'UpdateBattleMonInParty', 'UpdateEnemyMonInParty',
           'ResolveFaints.check_battle_over', 'ResetAbilityIgnorance',
           'TechnicianAbility', 'InsomniaAbility')
PRESENTATION = (*party.PRESENTATION, 'ClearSpeechBox', 'ClearPlayerHUD', 'ClearEnemyHUD',
                'AnimateHPBar', 'PlaySFX', 'WaitSFX', 'PrintText')
TERMINAL_EVENTS = ('WinTrainerBattle', 'LostBattle')
BOUNDARIES = (*PRESENTATION, *TERMINAL_EVENTS)
REJECT = (*party.REJECT, 'LoadAnim', 'BattleWinSlideInEnemyTrainerFrontpic',
          'PlayMusic', 'SlideEnemyPicOut')


# Narrow source and ROM masks admit only the experiments below. Every other
# byte, including every transitive rendering dependency, remains frozen.
SUPER_POTION_OFFSET = 230214
PIDGEY_HP_OFFSET = 133716
MASKED_ROM_SHA256 = '1ce6356274a09a5592a571890dee20e32a279e9ce61b1ebd4e4a2d6add58888c'
NORMALIZED_GAME_SOURCE_SHA256 = '79918a81c503ca516fc570def014472bb7a0827f2dc4d094df603636d01974d1'
ITEMS_SOURCE_SHA256 = '53629d1377a7b85b31a85e5625f2e55361b15e0c709f189f64b3970839a0e18b'
PIDGEY_SOURCE_SHA256 = '28f529f5e527789c214073443f6c2c1db85572ff5525f901c0d3424ecf37e0e9'


def verify_audit(rom, symbols, source_root):
    root = pathlib.Path(source_root)
    if hashlib.sha256(pathlib.Path(symbols).read_bytes()).hexdigest() != base.AUDITED_SYMBOLS_SHA256:
        raise ValueError('Action symbol layout changed; reaudit required')
    for path, expected in base.AUDITED_SOURCE.items():
        if hashlib.sha256((root / path).read_bytes()).hexdigest() != expected:
            raise ValueError('Action controller source changed; reaudit required: ' + path)
    items = (root / 'engine/battle/ai/items.asm').read_text()
    match = re.search(r'EnemyUsedSuperPotion:\n\tld a, SUPER_POTION\n\tld b, (\d+)\n', items)
    if match is None or not 50 <= int(match[1]) <= 100:
        raise ValueError('Unsupported healing source change')
    normalized = items[:match.start(1)] + '50' + items[match.end(1):]
    if hashlib.sha256(normalized.encode()).hexdigest() != ITEMS_SOURCE_SHA256:
        raise ValueError('Item source changed beyond audited healing immediate')
    pidgey = (root / 'data/pokemon/base_stats/pidgey.asm').read_text()
    match_hp = re.search(r'else\n\tbst (\d+),  (\d+),  35,  35,  50,  35,  56', pidgey)
    if match_hp is None or not 40 <= int(match_hp[2]) <= 60 or int(match_hp[1]) != 211 + int(match_hp[2]):
        raise ValueError('Unsupported Pidgey HP source change')
    normalized = (pidgey[:match_hp.start(1)] + '251,  40' + pidgey[match_hp.end(2):])
    if hashlib.sha256(normalized.encode()).hexdigest() != PIDGEY_SOURCE_SHA256:
        raise ValueError('Pidgey source changed beyond audited HP stat')
    moves = (root / 'data/moves/moves.asm').read_text()
    tackle = re.search(r'(move TACKLE,\s+EFFECT_NORMAL_HIT,\s+)(\d+)', moves)
    if tackle is None or not 2 <= int(tackle[2]) <= 255:
        raise ValueError('Unsupported Tackle source change')
    normalized_moves = moves[:tackle.start(2)] + '40' + moves[tackle.end(2):]
    normalized_sources = {
        'engine/battle/ai/items.asm': items[:match.start(1)] + '50' + items[match.end(1):],
        'data/pokemon/base_stats/pidgey.asm': normalized,
        'data/moves/moves.asm': normalized_moves,
    }
    source_hashes = {}
    for path in stateful_lifting.source_paths():
        content = normalized_sources[path].encode() if path in normalized_sources else (root / path).read_bytes()
        source_hashes[path] = hashlib.sha256(content).hexdigest()
    aggregate = hashlib.sha256(json.dumps(source_hashes, sort_keys=True, separators=(',', ':')).encode()).hexdigest()
    if aggregate != NORMALIZED_GAME_SOURCE_SHA256:
        raise ValueError('Game inputs changed beyond audited source fields')
    data = bytearray(pathlib.Path(rom).read_bytes())
    if data[SUPER_POTION_OFFSET] != int(match[1]) or data[PIDGEY_HP_OFFSET] != int(match_hp[2]):
        raise ValueError('Source and assembled computational values disagree')
    if data[344322] != int(tackle[2]):
        raise ValueError('Tackle source and assembled power disagree')
    if not 2 <= data[344322] <= 255:
        raise ValueError('Unsupported Tackle power')
    for offset in (*base.AUDITED_MASK_OFFSETS, SUPER_POTION_OFFSET, PIDGEY_HP_OFFSET):
        data[offset] = 0
    if hashlib.sha256(data).hexdigest() != MASKED_ROM_SHA256:
        raise ValueError('Action ROM changed beyond audited computation sites')


def build(rom, symbols, source_root, build_dir):
    verify_audit(rom, symbols, source_root)
    syms = stateful_lifting.symbols(symbols)
    data = pathlib.Path(rom).read_bytes()
    excluded = list(REJECT)
    for _ in range(8):
        traps = [syms['Crash'], *(syms[n] for n in BOUNDARIES)]
        traps += [syms[e] if isinstance(e, str) else tuple(e) for e in excluded]
        try:
            stateful_lifting.compile_rom(data, [syms[e] for e in ENTRIES], traps)
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
        raise ValueError('Action closure exceeded bounded iterations')
    return stateful_lifting.build(rom, symbols, ENTRIES, build_dir, source_root,
                                 boundaries=BOUNDARIES, excluded=excluded)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('rom', 'sym', 'source-root', 'build-dir'):
        parser.add_argument('--' + name, type=pathlib.Path, required=True)
    args = parser.parse_args()
    result = build(args.rom, args.sym, args.source_root, args.build_dir)
    print(json.dumps(dict(blocks=result['blocks'], library=result['library_path'])))
