"""Actual ordinary trainer controller, declared policy/UI event boundaries.

Static opcode exclusions remain rejecting diagnostics. Reaching any excluded
path is a coverage failure, never a successful battle transition.
"""
import argparse
import json
import pathlib
import re
import battle_actions_profile as previous
import stateful_lifting

# These policy choices replace input/AI selection only. Meaningful surrounding
# ParsePlayerAction, ParseEnemyAction and replacement callers execute unchanged.
CHOICES = ('GetSwitchScores', 'YesNoBox', 'ChooseMoveToForget',
           'MenuJoypadLoop', '_2DMenu', '_AIChooseMove', 'AI_MaybeSwitch', 'AI_TryItem')
TERMINALS = previous.TERMINAL_EVENTS
PRESENTATION = (*previous.PRESENTATION, 'PlaySlowCryBC',
    'PlayerMonFaintedAnimation', 'EnemyMonFaintedAnimation', 'PrintStatDifferences',
    'ClearBGPalettes', 'ClearPalettes', '_LoadBattleFontsHPBar',
    'ClearSprites', 'LoadPartyMenuGFX',
    'InitPartyMenuGFX', 'SafeLoadTempTileMapToTileMap',
    'LoadTempTileMapToTileMap', 'GetMemCGBLayout', 'ApplyTilemap',
    'FarCopyColorWRAM', 'SafeCopyTilemapAtOnce', 'MoveInfoBox',
    'LoadWeatherIconSprite', 'LoadBallIconGFX', 'Request2bpp', 'Copy2bpp', 'EnableLCD', 'WaitPressAorB_BlinkCursor')
CALL_ABIS = {'AnonBankPush': 'skip-byte', 'StackJumpTable': 'tail-table',
             'StandardStackJumpTable': 'tail-table', 'OffsetStackJumpTable': 'tail-table'}
WRITE_ONLY_MMIO = (*range(0xff10, 0xff15), 0xff48)
BANKED_WRAM = True
LCD_DISABLED = True
BOUNDARIES = (*PRESENTATION, *CHOICES, *TERMINALS)
ENTRIES = (*previous.ENTRIES, 'PerformMove', 'ResolveFaints',
           'HandleBetweenTurnEffects', 'BattleTurn', 'BattleTurn.do_move', 'DoBattle',
           'GetBaseData', 'CalcExpAtLevel', 'UpdatePkmnStats', 'CheckUsableMove',
           'CheckUsableMoves', 'ResetParticipants', 'GiveExperience',
           'GiveExperiencePoints', 'StackJumpTable', 'CheckLockedIn',
           'TerminateExpBarSound', 'CalcPkmnStats', 'EnemyUsedGuardSpec',
           'EnemyUsedDireHit', 'EnemyUsedXItem', 'DoItemEffect', '_DoItemEffect',
           'BattleMenu_Pack', 'BattlePack', 'ItemSubmenu.Use', 'UserCanSwitch', 'GetAbility',
           'ClearWindowData', 'Move2DMenuCursor', 'GetGender', 'StackCallInWRAMBankA.return',
           '_InitScrollingMenu', '_ScrollingMenu', '_2DMenuInterpretJoypad', 'DecompressString')


def source_dispatch_roots(source_root, symbols):
    """Actual command symbols and ability pointer tables, not guessed handlers."""
    roots = [name for name in symbols if name.startswith('BattleCommand_') and '.' not in name]
    text = (pathlib.Path(source_root) / 'engine/battle/abilities.asm').read_text()
    for match in re.finditer(r'^\s*dbw\s+\w+,\s*(\w+)\s*(?:;.*)?$', text, re.M):
        name = match[1]
        if name not in symbols:
            raise ValueError('Ability dispatch target has no ROM symbol: ' + name)
        roots.append(name)
    commands = (pathlib.Path(source_root) / 'engine/battle/effect_commands.asm').read_text()
    for match in re.finditer(r'^\s*dbw\s+\w+,\s*(Do\w+)\s*(?:;.*)?$', commands, re.M):
        if match[1] not in symbols:
            raise ValueError('Damage modifier target has no ROM symbol: ' + match[1])
        roots.append(match[1])
    items = (pathlib.Path(source_root) / 'engine/items/item_effects.asm').read_text()
    table = items.split('.ItemEffectRoutines:', 1)[1].split('assert_table_length', 1)[0]
    for match in re.finditer(r'^\s*dw\s+(\w+)\s*(?:;.*)?$', table, re.M):
        if match[1] not in symbols:
            raise ValueError('Item effect target has no ROM symbol: ' + match[1])
        roots.append(match[1])
    text_source = (pathlib.Path(source_root) / 'home/text.asm').read_text()
    for table, end in (('SpecialCharacters:', '\nNextLineChar::'),
                       ('TextCommands::', 'assert_table_length NUM_TEXT_COMMANDS')):
        section = text_source.split(table, 1)[1].split(end, 1)[0]
        for match in re.finditer(r'^\s*dw\s+(\w+)\s*(?:;.*)?$', section, re.M):
            if match[1] not in symbols:
                raise ValueError('Text dispatch target has no ROM symbol: ' + match[1])
            roots.append(match[1])
    item_pockets = (pathlib.Path(source_root) / 'engine/items/items.asm').read_text()
    for scope in ('_ReceiveItem', '_TossItem', '_CheckItem'):
        section = item_pockets.split(scope + '::', 1)[1].split('.Pockets:', 1)[1]
        section = section.split('\n.Item:', 1)[0] if scope == '_ReceiveItem' else section.split('\n.Medicine:', 1)[0]
        for name in re.findall(r'^\s*dw\s+([\w.]+)\s*(?:;.*)?$', section, re.M):
            name = scope + name if name.startswith('.') else name
            if name not in symbols:
                raise ValueError('Item pocket target has no ROM symbol: ' + name)
            roots.append(name)
    pack_callbacks = (pathlib.Path(source_root) / 'engine/items/pack.asm').read_text()
    for match in re.finditer(r'^\s*dba\s+(\w+)\s*(?:;.*)?$', pack_callbacks, re.M):
        if match[1] not in symbols:
            raise ValueError('Pack callback target has no ROM symbol: ' + match[1])
        roots.append(match[1])
    # Preserve the source pack controller; only its low-level input suspends.
    # All pointer targets are rooted because StackJumpTable has no normal
    # inline return continuation. Resolve local labels in their source scope.
    pack = (pathlib.Path(source_root) / 'engine/items/pack.asm').read_text()
    scope, selected = None, False
    tables = {'BattlePack.Jumptable', 'ItemSubmenu.UsableJumptable',
              'ItemSubmenu.UnusableJumptable', 'ItemSubmenu.ItemFunctionJumptable'}
    found = set()
    for raw in pack.splitlines():
        code = raw.split(';', 1)[0].strip()
        label = re.fullmatch(r'([\w.]+):?', code)
        if label and not code.startswith(('ret', 'nop')):
            name = label[1]
            if not name.startswith('.'):
                scope = name
            full = scope + name if name.startswith('.') else name
            selected = full in tables
            if selected:
                found.add(full)
        match = re.fullmatch(r'dw\s+([\w.]+)', code)
        if selected and match:
            name = match[1]
            name = scope + name if name.startswith('.') else name
            if name not in symbols:
                raise ValueError('Pack dispatch target has no ROM symbol: ' + name)
            roots.append(name)
    if found != tables:
        raise ValueError('Pack dispatch source tables changed: ' + str(found))
    party = (pathlib.Path(source_root) / 'engine/pokemon/party_menu.asm').read_text()
    table = party.split('PartyMenuActionQualities:', 1)[1].split('assert_table_length', 1)[0]
    targets = re.findall(r'^\s*dw\s+(\w+)\s*(?:;.*)?$', table, re.M)
    if not targets:
        raise ValueError('Party action presentation dispatch table is missing')
    for name in targets:
        if name not in symbols:
            raise ValueError('Party action presentation target has no ROM symbol: ' + name)
        roots.append(name)
    for path in sorted((pathlib.Path(source_root) / 'home').glob('*.asm')):
        for match in re.finditer(r'^\s*homecall\s+(\w+)\s*(?:;.*)?$', path.read_text(), re.M):
            if match[1] not in symbols:
                raise ValueError('Homecall target has no ROM symbol: ' + match[1])
            roots.append(match[1])
    return tuple(dict.fromkeys(roots))


def config(symbols, source_root):
    for name in (*ENTRIES, *BOUNDARIES):
        if name not in symbols:
            raise ValueError('Missing controller profile symbol: ' + name)
    return tuple(dict.fromkeys((*ENTRIES, *source_dispatch_roots(source_root, symbols))))


def audit_configuration(audit):
    return audit.configuration() if audit is not None else dict(kind='frozen-action-profile-v1')


def verify_audit(rom, symbols, source_root, audit=None):
    (audit.verify if audit is not None else previous.verify_audit)(rom, symbols, source_root)
    return audit_configuration(audit)


def build(rom, symbols, source_root, build_dir, discovery_only=False, optimization='O0', *, audit=None):
    audit_config = verify_audit(rom, symbols, source_root, audit)
    syms = stateful_lifting.symbols(symbols)
    entries = config(syms, source_root)
    data = pathlib.Path(rom).read_bytes()
    excluded = ['_PlaceNextVWFChar']
    for _ in range(12):
        traps = [syms['Crash'], *(syms[n] for n in BOUNDARIES)]
        traps += [syms[e] if isinstance(e, str) else tuple(e) for e in excluded]
        try:
            blocks, seen = stateful_lifting.compile_rom(data, [syms[e] for e in entries], traps,
                stateful_lifting.resolve_call_abis(syms, CALL_ABIS))
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
        raise ValueError('Controller closure discovery exceeded bounded iterations')
    destination = pathlib.Path(build_dir)
    destination.mkdir(parents=True, exist_ok=True)
    description = dict(entries=entries, boundaries=BOUNDARIES, excluded=excluded,
                       audit=audit_config,
                       banked_wram=BANKED_WRAM, lcd_disabled=LCD_DISABLED, optimization=optimization,
                       instructions=len(blocks), source=pathlib.Path(__file__).read_text())
    (destination / 'controller-profile.json').write_text(json.dumps(description, indent=2) + '\n')
    if discovery_only:
        return description
    manifest = stateful_lifting.build(rom, symbols, entries, build_dir, source_root,
                                 boundaries=BOUNDARIES, excluded=excluded,
                                 write_only_mmio=WRITE_ONLY_MMIO, call_abis=CALL_ABIS,
                                 optimization=optimization, banked_wram=BANKED_WRAM, lcd_disabled=LCD_DISABLED)
    manifest['controller_audit'] = audit_config
    (destination / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')
    return manifest


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('rom', 'sym', 'source-root', 'build-dir'):
        parser.add_argument('--' + name, type=pathlib.Path, required=True)
    parser.add_argument('--discovery-only', action='store_true')
    parser.add_argument('--optimization', choices=('O0', 'O1', 'O2', 'O3'), default='O0')
    args = parser.parse_args()
    result = build(args.rom, args.sym, args.source_root, args.build_dir, args.discovery_only, args.optimization)
    print(json.dumps(dict(instructions=result.get('blocks', result.get('instructions')),
                          entries=len(result['entries']))))
