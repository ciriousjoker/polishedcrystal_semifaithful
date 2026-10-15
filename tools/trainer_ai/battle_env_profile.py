"""Explicit experimental headless presentation profile, not an ordinary ROM proof."""
import hashlib
import json
import pathlib
import re
import stateful_lifting

# Identity-return ABI: preserve all registers/flags, pop exactly the actual return
# address. These functions are presentation boundaries, not battle commands.
PRESENTATION = (
    'StdBattleTextbox', 'PlayBattleAnim', 'UpdateHPBar',
    'UpdatePlayerHUD', 'UpdateEnemyHUD', 'DelayFrame',
)
# Reaching these is an unsupported path, never a successful presentation event.
REJECT = ('_PlaceNextVWFChar', 'StackJumpTable')
NORMAL_COMMANDS = (
    'checkobedience', 'usedmovetext', 'doturn', 'hastarget', 'checkhit',
    'checkpriority', 'critical', 'damagestats', 'damagecalc', 'stab',
    'damagevariation', 'moveanim', 'failuretext', 'applydamage', 'criticaltext',
    'supereffectivetext', 'postfainteffects', 'posthiteffects',
)
ENTRIES = ['DoNothing', 'DoTurn', 'DetermineMoveOrder', 'SubtractHP'] + [
    'BattleCommand_' + name for name in NORMAL_COMMANDS]


def build(rom, symbols, source_root, build_dir):
    """Decode conservative closure; unsupported opcodes remain rejecting traps.

    This exploratory profile changes only unsupported-path generation behavior.
    It never translates an unknown instruction to a successful operation.
    The rejection addresses and all profile identities are bound in the manifest.
    """
    verify_audit(rom, symbols, source_root)
    syms = stateful_lifting.symbols(symbols)
    data = pathlib.Path(rom).read_bytes()
    excluded = list(REJECT)
    for _ in range(8):
        roots = [syms[e] for e in ENTRIES]
        traps = [syms['Crash']] + [syms[n] for n in PRESENTATION]
        traps += [syms[e] if isinstance(e, str) else e for e in excluded]
        try:
            stateful_lifting.compile_rom(data, roots, traps)
            break
        except ValueError as error:
            message = str(error)
            prefix = 'Unsupported reachable instructions: '
            if not message.startswith(prefix):
                raise
            bad = json.loads(message[len(prefix):])
            added = [(row['bank'], row['pc']) for row in bad]
            if not set(added) - set(e for e in excluded if not isinstance(e, str)):
                raise
            excluded.extend(added)
    else:
        raise ValueError('Unsupported closure exceeded bounded probe iterations')
    manifest = stateful_lifting.build(
        rom, symbols, ENTRIES, build_dir, source_root,
        boundaries=PRESENTATION, excluded=excluded)
    profile = dict(schema=1, kind='experimental-headless-identity-return',
                   presentation=list(PRESENTATION), excluded=excluded,
                   profile_source=pathlib.Path(__file__).read_text())
    (pathlib.Path(build_dir) / 'headless-profile.json').write_text(
        json.dumps(profile, indent=2) + '\n')
    return manifest

# Frozen audit identities. A changed frontend needs explicit review, not rebuild approval.
AUDITED_SOURCE = {'home/battle.asm': '2c718f1d14a728b174701e10e4999720ae224679e39bc0d4f2a23619ee21a70c', 'engine/battle_anims/anim_commands.asm': '06fb17334955e780c8181ed6a2edccabf669e8fc6d4340a9b43d34f9550de765', 'engine/battle/core.asm': 'c70c34b834411bfe4ecb36fb98ac7881a3876eb3429eaa03cf2745eb79cbae65', 'home/delay.asm': '6e67beecc3db53585f94730c0de824c133428fac6169abb69c238c845333326b'}
AUDITED_SPANS = {'StdBattleTextbox': (4, '3f4aea335f1f83869ccd5df3e4c3e24ca0dbfa0851c07605d3da3afeb7a8bb53'), 'PlayBattleAnim': (18, '28bdaabfd5eeebadd40eb2b2979a5d4b00cb5348b47a0bc35421a3c9e621748c'), 'UpdateHPBar': (23, '9224e70dba6d2e3457ca9e785a62e46a888a2d21c94e6c559d82b6a146e987bf'), 'UpdatePlayerHUD': (15, '876ddfba2c7c7609e601c89e4e7f5484ea8cb56eedbf0d48ce4fd2cf79514494'), 'UpdateEnemyHUD': (12, '29ff34d35743ef7957d4673a3e89195373bba03e18dc2aa23ed53dff45291155'), 'DelayFrame': (7, '1f17c6a59ede306f96a146ddfbee407339c6ddef2a4353b874cc7fa567b85218')}

def verify_audit(rom, symbols, source_root):
    data = pathlib.Path(rom).read_bytes()
    syms = stateful_lifting.symbols(symbols)
    for path, expected in AUDITED_SOURCE.items():
        if hashlib.sha256((pathlib.Path(source_root) / path).read_bytes()).hexdigest() != expected:
            raise ValueError('Presentation source changed; reaudit required: ' + path)
    if hashlib.sha256(pathlib.Path(symbols).read_bytes()).hexdigest() != AUDITED_SYMBOLS_SHA256:
        raise ValueError('Headless symbol layout changed; reaudit required')
    masked = bytearray(data)
    if masked[0x54102] not in range(2, 256):
        raise ValueError('Unsupported Tackle power update')
    for offset in AUDITED_MASK_OFFSETS:
        masked[offset] = 0
    if hashlib.sha256(masked).hexdigest() != AUDITED_MASKED_ROM_SHA256:
        raise ValueError('Headless ROM profile changed; transitive reaudit required')
    for name, (count, expected) in AUDITED_SPANS.items():
        bank, pc = syms[name]
        start = pc if pc < 0x4000 else bank * 0x4000 + pc - 0x4000
        if hashlib.sha256(data[start:start + count]).hexdigest() != expected:
            raise ValueError('Presentation ROM changed; reaudit required: ' + name)

# Only this fixed-layout Tackle-power data update is automatically admissible.
# Pinning all remaining ROM bytes closes transitive skipped-frontend dependencies.
AUDITED_MASK_OFFSETS = (344322, 333, 334, 335)
AUDITED_MASKED_ROM_SHA256 = 'abd8c4ddbd0f2aef442863100d253c264381f6ae5c94c3313ecb606d32d6b268'
AUDITED_SYMBOLS_SHA256 = 'ee66579bea2f6212559b49cf4beda79316831bc068297f6e3206cc3037a4aca0'
