#!/usr/bin/env python3
"""One meaningful copied-source move update and frontend audit rejection."""
import argparse
import hashlib
import json
import pathlib
import subprocess

import battle_env_check
import battle_env_profile
import stateful_lifting


def run(source, output, mgba):
    output.mkdir(parents=True, exist_ok=True)
    game = output / 'mutated-game'
    if game.exists():
        raise ValueError('Use a new update artifact directory; existing source is preserved')
    subprocess.run(['cp', '-cR', str(source), str(game)], check=True)
    rom = game / 'polishedcrystal-3.2.3.gbc'
    symbols = rom.with_suffix('.sym')
    original_digest = hashlib.sha256(rom.read_bytes()).hexdigest()
    moves = game / 'data/moves/moves.asm'
    text = moves.read_text()
    before = 'move TACKLE,          EFFECT_NORMAL_HIT,         40, NORMAL,'
    after = 'move TACKLE,          EFFECT_NORMAL_HIT,         80, NORMAL,'
    assert text.count(before) == 1, 'Actual move source changed; review mutation'
    moves.write_text(text.replace(before, after))
    # Force assembly and relinking, not same-second dependency timestamps.
    (game / 'main.o').unlink(missing_ok=True)
    rom.unlink()
    with (output / 'assemble.log').open('w') as log:
        subprocess.run(['make', '-j1'], cwd=game, stdout=log,
                       stderr=subprocess.STDOUT, check=True)
    assert hashlib.sha256(rom.read_bytes()).hexdigest() != original_digest
    baseline = output.parent / 'caller'
    try:
        stateful_lifting.load_verified(baseline, rom, symbols, game,
                                      battle_env_profile.ENTRIES)
    except ValueError as error:
        stale = str(error)
    else:
        raise AssertionError('Changed source/ROM accepted by old native library')
    build = output / 'native'
    battle_env_profile.build(rom, symbols, game, build)
    report = battle_env_check.run(rom, symbols, game, build, mgba)
    baseline_report = json.loads((baseline / 'fight-report.json').read_text())
    # First move is Tackle in both actual caller suites; its resulting HP must
    # change in addition to provenance, not merely rebuild a numerically same ROM.
    def first_attack(result):
        return next(row['hp'] for row in result['transitions'] if row['entry'] == 'DoTurn')
    assert first_attack(report) != first_attack(baseline_report)
    frontend = game / 'home/battle.asm'
    original = frontend.read_text()
    mutated = original.replace('StdBattleTextbox::\n',
        'StdBattleTextbox::\n\tld [wCurDamage], a ; rejected audit mutation\n', 1)
    assert mutated != original
    frontend.write_text(mutated)
    try:
        try:
            battle_env_profile.verify_audit(rom, symbols, game)
        except ValueError as error:
            assert 'Presentation source changed' in str(error)
            audit_rejection = str(error)
        else:
            raise AssertionError('Side-effectful presentation source accepted')
    finally:
        frontend.write_text(original)
    result = dict(status='conditional headless caller update matched',
                  baseline_rom_sha256=original_digest,
                  changed_rom_sha256=report['rom_sha256'], stale_rejection=stale,
                  first_attack_baseline=first_attack(baseline_report),
                  first_attack_changed=first_attack(report),
                  episodes=len(report['episodes']), transitions=len(report['transitions']),
                  audited_body_source_rejection=audit_rejection)
    (output / 'update-report.json').write_text(json.dumps(result, indent=2) + '\n')
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--game-source', type=pathlib.Path, required=True)
    parser.add_argument('--build-dir', type=pathlib.Path, required=True)
    parser.add_argument('--mgba-prefix', type=pathlib.Path, required=True)
    args = parser.parse_args()
    print(json.dumps(run(args.game_source, args.build_dir, args.mgba_prefix)))
