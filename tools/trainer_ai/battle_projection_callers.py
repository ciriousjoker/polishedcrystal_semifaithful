"""Real HP computational callers versus the declared identity UI projection.

The real path runs both HUDs, LCD/VBlank and DMA. The conditional path executes
identical source with only UpdateHPBar/UpdatePlayerHUD/UpdateEnemyHUD returning.
Only the explicit gameplay projection and source-defined live returns are
asserted equal. Register and UI scratch differences remain visible in evidence.
"""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
import subprocess
from battle_env import NativeFight
from stateful_fixture_check import State, symbols

ROOT = pathlib.Path(__file__).resolve().parents[2]


class Hook(C.Structure):
    _fields_ = [('bank', C.c_uint16), ('pc', C.c_uint16)]


def fixture(data, sy, source_root, side, old, amount, entry):
    game = NativeFight.__new__(NativeFight)
    game.rom, game.symbols, game.source_root = data, sy, source_root
    game.state, game._constant_cache, game.moves = State(), {}, (33, 55)
    game._initialize_fixture(1, 0)
    state = game.state
    def put(name, value, width=1, endian='big'):
        address = sy[name][1]
        state.m[address:address + width] = value.to_bytes(width, endian)
    state.bank, state.pc = sy[entry]
    put('hROMBank', state.bank)
    state.sp = sy['wStackTop'][1] - 2
    state.m[state.sp], state.m[state.sp + 1] = 0xfe, 0xff
    state.a, state.f = 17, 0xb0
    state.b, state.c = amount >> 8, amount & 255
    state.d, state.e = 31, 37
    hp = sy[('wBattleMonHP', 'wEnemyMonHP')[side]][1]
    state.h, state.l = hp >> 8, hp & 255
    put('hBattleTurn', side)
    put('hCGB', 1)
    put('hBGMapAddress', 0x9800, 2, 'little')
    for stem in ('wBattleMon', 'wEnemyMon', 'wPartyMon1', 'wOTPartyMon1'):
        put(stem + 'MaxHP', 150, 2)
    put(('wBattleMonHP', 'wEnemyMonHP')[side], old, 2)
    put(('wPartyMon1HP', 'wOTPartyMon1HP')[side], old, 2)
    # Ordinary HUD code expects terminated nicknames and initialized species/EXP.
    for stem in ('wBattleMonNickname', 'wEnemyMonNickname'):
        state.m[sy[stem][1]:sy[stem][1] + 11] = bytes([0x50]) * 11
    put('wPartyMon1Exp', 100000, 3)  # Smeargle GROWTH_FAST at level 50.
    put('wOTPartyMon1Exp', 100000, 3)
    put('wTempEnemyMonSpecies', state.m[sy['wEnemyMonSpecies'][1]])
    put('wTempBattleMonSpecies', state.m[sy['wBattleMonSpecies'][1]])
    put('wHPBuffer1', 150, 2, 'little')
    for name, target, count in (('OAMDMACode', 'hPushOAM', 6),
                                 ('RomHeaderChecksum', 'wRomChecksum', 2)):
        bank, pc = sy[name]
        offset = bank * 0x4000 + pc - 0x4000 if pc >= 0x4000 else pc
        address = sy[target][1]
        state.m[address:address + count] = data[offset:offset + count]
    return state


def run(project):
    source = project / 'build/stateful/game'
    rom = source / 'polishedcrystal-3.2.3.gbc'
    sym = source / 'polishedcrystal-3.2.3.sym'
    sy, data = symbols(sym), rom.read_bytes()
    prefix = project / 'build/stateful/mgba'
    build = project / 'build/battle-projection/callers-v2'
    build.mkdir(parents=True, exist_ok=True)
    libraries = []
    for tool, output in (('battle_projection_oracle.c', 'real.dylib'),
                         ('battle_env_oracle.c', 'identity.dylib')):
        path = build / output
        subprocess.run(['cc', '-O2', '-dynamiclib', '-I' + str(prefix / 'include'),
            str(ROOT / 'tools/trainer_ai' / tool), '-L' + str(prefix / 'lib'),
            '-lmgba', '-Wl,-rpath,' + str(prefix / 'lib'), '-o', str(path)], check=True)
        libraries.append(C.CDLL(str(path)))
    real, identity = libraries
    real.projection_open.argtypes = identity.oracle_open.argtypes = [C.c_char_p]
    real.projection_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16,
                                     C.c_uint16, C.POINTER(C.c_uint32)]
    identity.oracle_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16,
                                    C.c_uint16, C.POINTER(Hook), C.c_uint32]
    assert real.projection_open(str(rom).encode()) == 0
    assert identity.oracle_open(str(rom).encode()) == 0
    hooks = (Hook * 3)(*[Hook(*sy[name]) for name in
        ('UpdateHPBar', 'UpdatePlayerHUD', 'UpdateEnemyHUD')])
    rows = []
    for entry in ('_SubtractHP', 'RestoreHP'):
        cases = ((150, 10), (20, 20), (20, 30), (20, 0)) if entry == '_SubtractHP' else ((20, 10), (20, 150), (150, 20), (20, 0))
        for side in (0, 1):
            for old, amount in cases:
                initial = fixture(data, sy, source, side, old, amount, entry)
                actual, conditional = State.from_buffer_copy(initial), State.from_buffer_copy(initial)
                frames = C.c_uint32()
                real.projection_run(C.byref(actual), 3000000, sy['Crash'][1], sy['VBlank'][1], C.byref(frames))
                identity.oracle_run(C.byref(conditional), 300000, 65534,
                                    sy['hROMBank'][1], hooks, len(hooks))
                ranges = [(sy[stem][1], sy[stem + 'StructEnd'][1]) for stem in ('wBattleMon', 'wEnemyMon')]
                ranges += [(sy[name][1], sy[name][1] + width) for name, width in
                           (('wCurDamage', 2), ('wWhichMonFaintedFirst', 1))]
                differences = [a for lo, hi in ranges for a in range(lo, hi)
                               if actual.m[a] != conditional.m[a]]
                row = dict(entry=entry, side=side, old=old, amount=amount,
                    actual_fault=actual.fault, conditional_fault=conditional.fault,
                    actual_pc=actual.pc, actual_bank=actual.bank,
                    steps=actual.steps, frames=frames.value, div_reads=actual.event_count,
                    gameplay_differences=differences,
                    registers={name: [getattr(conditional, name), getattr(actual, name)] for name in 'afbcdehl'},
                    scratch={name: [conditional.m[sy[name][1]], actual.m[sy[name][1]]]
                             for name in ('wHPBuffer1', 'wHPBuffer2', 'wHPBuffer3', 'wMonType', 'wCurSpecies', 'wTempMonLevel')})
                rows.append(row)
                case_name = f'{entry}-{side}-{old}-{amount}'
                for label, state in (('initial', initial), ('actual', actual), ('conditional', conditional)):
                    (build / f'{case_name}-{label}.bin').write_bytes(bytes(state))
                if actual.fault or conditional.fault:
                    break
                assert not differences, row
                assert (actual.b, actual.c) == (conditional.b, conditional.c), row
                # _SubtractHP explicitly protects AF, then marks a first faint.
                if entry == '_SubtractHP':
                    assert (actual.a, actual.f) == (conditional.a, conditional.f), row
            if rows[-1]['actual_fault'] or rows[-1]['conditional_fault']:
                break
        if rows[-1]['actual_fault'] or rows[-1]['conditional_fault']:
            break
    real.projection_close()
    identity.oracle_close()
    result = dict(schema=2, rom_sha256=hashlib.sha256(data).hexdigest(),
        symbols_sha256=hashlib.sha256(sym.read_bytes()).hexdigest(),
        tool_sha256={name: hashlib.sha256((ROOT / 'tools/trainer_ai' / name).read_bytes()).hexdigest()
                     for name in ('battle_projection_callers.py', 'battle_projection_oracle.c', 'battle_env_oracle.c', 'battle_env.py')},
        rows=rows)
    (build / 'report.json').write_text(json.dumps(result, indent=2) + '\n')
    assert len(rows) == 16, 'Incomplete computational caller case matrix'
    assert all(not row['actual_fault'] and not row['conditional_fault'] for row in rows), rows
    print(json.dumps({'cases': len(rows), 'faults': [(r['actual_fault'], r['conditional_fault']) for r in rows]}))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--project', type=pathlib.Path, required=True)
    run(parser.parse_args().project)
