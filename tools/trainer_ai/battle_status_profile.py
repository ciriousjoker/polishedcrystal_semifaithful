"""Separate source-verified leaf profile for actual poison/burn residual execution."""
import json
import pathlib
import battle_env_profile as base
import stateful_lifting

ENTRIES = ['HandlePoison', 'HandleBurn', 'SubtractHP']


def build(rom, symbols, source_root, build_dir):
    base.verify_audit(rom, symbols, source_root)
    syms = stateful_lifting.symbols(symbols)
    data = pathlib.Path(rom).read_bytes()
    excluded = list(base.REJECT)
    for _ in range(8):
        traps = [syms['Crash']] + [syms[n] for n in base.PRESENTATION]
        traps += [syms[e] if isinstance(e, str) else e for e in excluded]
        try:
            stateful_lifting.compile_rom(data, [syms[e] for e in ENTRIES], traps)
            break
        except ValueError as error:
            prefix = 'Unsupported reachable instructions: '
            if not str(error).startswith(prefix):
                raise
            added = [(row['bank'], row['pc']) for row in json.loads(str(error)[len(prefix):])]
            if not set(added) - {e for e in excluded if not isinstance(e, str)}:
                raise
            excluded.extend(added)
    else:
        raise ValueError('Status closure exceeded bounded probe iterations')
    result = stateful_lifting.build(rom, symbols, ENTRIES, build_dir, source_root,
        boundaries=base.PRESENTATION, excluded=excluded)
    (pathlib.Path(build_dir) / 'status-profile.json').write_text(json.dumps(dict(
        kind='conditional-headless-status-leaf', entries=ENTRIES,
        presentation=base.PRESENTATION, excluded=excluded,
        profile_source=pathlib.Path(__file__).read_text()), indent=2) + '\n')
    return result

TERMINAL_ENTRIES = ['UpdateBattleMonInParty', 'UpdateEnemyMonInParty',
                    'ResolveFaints.check_battle_over']
TERMINAL_EVENTS = ('WinTrainerBattle', 'LostBattle')


def build_terminal(rom, symbols, source_root, build_dir):
    """Stop before battle progression, at explicit win/loss terminal events."""
    base.verify_audit(rom, symbols, source_root)
    return stateful_lifting.build(rom, symbols, TERMINAL_ENTRIES, build_dir,
        source_root, boundaries=TERMINAL_EVENTS)


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rom', type=pathlib.Path, required=True)
    parser.add_argument('--sym', type=pathlib.Path, required=True)
    parser.add_argument('--source-root', type=pathlib.Path, required=True)
    parser.add_argument('--build-dir', type=pathlib.Path, required=True)
    args = parser.parse_args()
    for name, builder in (('leaf', build), ('terminal', build_terminal)):
        manifest = builder(args.rom, args.sym, args.source_root, args.build_dir / name)
        print(json.dumps(dict(profile=name, blocks=manifest['blocks'],
                              library=manifest['library_path'])))
