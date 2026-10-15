"""Build real Assembly mutations and reject stale native/unaudited inputs."""
import argparse
import ctypes as C
import hashlib
import json
import os
import pathlib
import shutil
import subprocess
import sys
import battle_actions_profile as profile
from battle_actions import ActionLeaf
from battle_teams import TeamFight
import stateful_lifting


def env_for(root, game, build):
    return TeamFight(game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym',
                     game, action_build=build)


def healing(env):
    f = env.fight
    f._put('wEnemyMonHP', 10, 2)
    f._put('wOTPartyMon1HP', 10, 2)
    f._put('hBattleTurn', 1)
    env.action_leaf.call('EnemyUsedSuperPotion')
    assert f.word('wEnemyMonHP') == f.word('wOTPartyMon1HP')
    return f.word('wEnemyMonHP')


def main(root):
    directory = root / 'build/battle-actions/update'
    directory.mkdir(parents=True, exist_ok=True)
    baseline = root / 'build/stateful/game'
    game = directory / 'game'
    if not game.exists():
        # Copy the existing incremental build, never mutate the frozen source cache.
        shutil.copytree(baseline, game, ignore=shutil.ignore_patterns('.git'))
        items = game / 'engine/battle/ai/items.asm'
        text = items.read_text()
        before = 'EnemyUsedSuperPotion:\n\tld a, SUPER_POTION\n\tld b, 50\n'
        assert text.count(before) == 1
        items.write_text(text.replace(before, before.replace('50', '75')))
        pidgey = game / 'data/pokemon/base_stats/pidgey.asm'
        text = pidgey.read_text()
        before = 'bst 251,  40,  35,  35,  50,  35,  56'
        assert text.count(before) == 1
        pidgey.write_text(text.replace(before, 'bst 266,  55,  35,  35,  50,  35,  56'))
        log = directory / 'build.log'
        with log.open('w') as output:
            subprocess.run(['make', '-j1', 'polishedcrystal-3.2.3.gbc'], cwd=game,
                           stdout=output, stderr=subprocess.STDOUT, check=True)
    rom, sym = game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym'
    profile.verify_audit(rom, sym, game)
    old = root / 'build/battle-actions/full'
    manifest = json.loads((old / 'manifest.json').read_text())
    try:
        stateful_lifting.load_verified(old, rom, sym, game, profile.ENTRIES,
                                      profile.BOUNDARIES, tuple(manifest['excluded']))
    except ValueError as error:
        stale = str(error)
    else:
        raise AssertionError('Stale native artifact accepted changed Assembly')
    build = directory / 'native'
    current = json.loads((build / 'manifest.json').read_text()) if (build / 'manifest.json').exists() else {}
    if set(current.get('entries', {})) != set(profile.ENTRIES) or set(current.get('boundaries', {})) != set(profile.BOUNDARIES):
        if current:
            (directory / ('prior-manifest-' + current['rom_sha256'][:12] + '.json')).write_text(json.dumps(current, indent=2) + '\n')
        profile.build(rom, sym, game, build)
    env = env_for(root, game, build)
    baseline_env = env_for(root, baseline, old)
    baseline_hp, mutant_hp = healing(baseline_env), healing(env)
    assert (baseline_hp, mutant_hp) == (60, 85)
    assert baseline_env.observation(0)['teams'][0][1]['MaxHP'] == 100
    assert env.observation(0)['teams'][0][1]['MaxHP'] == 115
    # Independent mGBA uses the same explicit presentation projection; compare
    # the changed healing and actual changed send-in callers, not host formulas.
    from battle_actions_check import compare_call
    oracle_rows = []
    env.reset()
    f = env.fight
    f._put('wEnemyMonHP', 10, 2)
    f._put('hBattleTurn', 1)
    compare_call(env, 'EnemyUsedSuperPotion', root, oracle_rows)
    assert f.word('wEnemyMonHP') == 85
    f._put('hBattleTurn', 0)
    f._put('wPlayerSwitchTarget', 2)
    for entry in ('GetUserSwitchTarget', 'SendInUserPkmn', 'SpikesDamage', 'RunEntryAbilities'):
        compare_call(env, entry, root, oracle_rows)
    assert f.word('wBattleMonMaxHP') == 115
    env.reset()
    env.action_leaf.call = lambda entry: compare_call(env, entry, root, oracle_rows)
    from battle_actions import SwitchAction, ItemAction
    from battle_env import MoveAction
    while not env.terminal and not env.truncated:
        if f.turn == 0:
            f._put('wEnemyMonHP', 10, 2)
            chosen = (SwitchAction(1), ItemAction(0))
        else:
            chosen = tuple(MoveAction((f.turn + side) % 2) for side in (0, 1))
        legal = [env.legal_actions(side) for side in (0, 1)]
        env.step(tuple(action if action in legal[side] else legal[side][0]
                       for side, action in enumerate(chosen)))
    assert env.terminal and not env.truncated
    complete_fight = dict(turns=f.turn, winner=env.winner,
                          replacements=list(env.replacements))
    # Unsupported source changes cannot be silently blessed by a rebuild.
    core = game / 'engine/battle/core.asm'
    frozen = core.read_bytes()
    needle = b'\tcp HELD_SHED_SHELL\n'
    assert needle in frozen
    core.write_bytes(frozen.replace(needle, b'\tcp HELD_AIR_BALLOON\n', 1))
    try:
        profile.verify_audit(rom, sym, game)
    except ValueError as error:
        rejected = str(error)
    else:
        raise AssertionError('Unknown controller source change accepted')
    finally:
        core.write_bytes(frozen)
    # A changed skipped rendering instruction is unsafe even with unchanged
    # source/controller layout and a freshly computed ROM checksum.
    original_rom = rom.read_bytes()
    altered = bytearray(original_rom)
    bank, pc = stateful_lifting.symbols(sym)['StdBattleTextbox']
    offset = pc if pc < 0x4000 else bank * 0x4000 + pc - 0x4000
    altered[offset] ^= 1
    rom.write_bytes(altered)
    try:
        profile.verify_audit(rom, sym, game)
    except ValueError as error:
        skipped_rejected = str(error)
    else:
        raise AssertionError('Changed skipped rendering instruction accepted')
    finally:
        rom.write_bytes(original_rom)
    items = game / 'engine/battle/ai/items.asm'
    frozen_items = items.read_bytes()
    assert b'EnemyUsedSuperPotion:\n\tld a, SUPER_POTION\n\tld b, 75\n' in frozen_items
    items.write_bytes(frozen_items.replace(b'ld b, 75\n', b'ld b, 74\n', 1))
    try:
        profile.verify_audit(rom, sym, game)
    except ValueError as error:
        mismatch_rejected = str(error)
    else:
        raise AssertionError('Mismatched healing source and ROM accepted')
    finally:
        items.write_bytes(frozen_items)
    # Restored caches must remain independently loadable and fingerprint exact.
    profile.verify_audit(rom, sym, game)
    stateful_lifting.load_verified(build, rom, sym, game, profile.ENTRIES,
                                  profile.BOUNDARIES, tuple(json.loads((build / 'manifest.json').read_text())['excluded']))
    result = dict(baseline_healed_hp=baseline_hp, mutant_healed_hp=mutant_hp,
                  baseline_pidgey_max_hp=100, mutant_pidgey_max_hp=115,
                  old_manifest_rejected=stale, unknown_source_rejected=rejected,
                  changed_skipped_code_rejected=skipped_rejected,
                  mismatched_source_rom_rejected=mismatch_rejected,
                  oracle_calls=oracle_rows, mutated_complete_fight=complete_fight,
                  rom_sha256=hashlib.sha256(rom.read_bytes()).hexdigest(),
                  symbols_sha256=hashlib.sha256(sym.read_bytes()).hexdigest())
    (directory / 'report.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    main(parser.parse_args().artifact_root)
