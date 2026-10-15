"""Discriminate a real HP frontend from the conditional identity-return profile.

No presentation hooks are installed. This is a bounded real routine experiment,
not an ordinary complete battle proof or a hardware randomness distribution test.
"""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
import subprocess
from stateful_fixture_check import State, symbols
from battle_env import NativeFight

ROOT = pathlib.Path(__file__).resolve().parents[2]


def run(project):
    game = project / 'build/stateful/game'
    rom = game / 'polishedcrystal-3.2.3.gbc'
    sym = game / 'polishedcrystal-3.2.3.sym'
    prefix = project / 'build/stateful/mgba'
    build = project / 'build/battle-projection'
    build.mkdir(parents=True, exist_ok=True)
    sy = symbols(sym)
    data = rom.read_bytes()
    library = build / 'real-frontend.dylib'
    subprocess.run(['cc', '-O2', '-dynamiclib', '-I' + str(prefix / 'include'),
                    str(ROOT / 'tools/trainer_ai/battle_projection_oracle.c'),
                    '-L' + str(prefix / 'lib'), '-lmgba', '-Wl,-rpath,' + str(prefix / 'lib'),
                    '-o', str(library)], check=True)
    mg = C.CDLL(str(library))
    mg.projection_open.argtypes = [C.c_char_p]
    mg.projection_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16,
                                  C.c_uint16, C.POINTER(C.c_uint32)]
    assert mg.projection_open(str(rom).encode()) == 0
    # Reuse only the data constructor. No lifted native artifact is loaded or run.
    fixture = NativeFight.__new__(NativeFight)
    fixture.rom, fixture.symbols, fixture.source_root = data, sy, game
    fixture.state, fixture._constant_cache, fixture.moves = State(), {}, (33, 55)
    fixture._initialize_fixture(1, 0)
    rows = []
    for side in (0, 1):
        for old, new in ((150, 140), (20, 0), (10, 150)):
            state = State.from_buffer_copy(fixture.state)
            def put(name, value, count=1):
                for i in range(count):
                    state.m[sy[name][1] + i] = (value >> (8 * i)) & 255
            entry = 'UpdateHPBar'
            state.bank, state.pc = sy[entry]
            put('hROMBank', state.bank)
            state.sp = sy['wStackTop'][1] - 2
            state.m[state.sp], state.m[state.sp + 1] = 0xfe, 0xff
            state.a, state.f, state.b, state.c = 17, 0xb0, 23, 29
            state.d, state.e, state.h, state.l = 31, 37, 41, 43
            put('hBattleTurn', side)
            put('hCGB', 1)
            put('hBGMapAddress', 0x9800, 2)
            # Actual ROM DMA code copied by the ordinary initialization routine.
            bank, pc = sy['OAMDMACode']
            offset = bank * 0x4000 + pc - 0x4000 if pc >= 0x4000 else pc
            target = sy['hPushOAM'][1]
            for i, value in enumerate(data[offset:offset + 6]):
                state.m[target + i] = value
            bank, pc = sy['RomHeaderChecksum']
            offset = bank * 0x4000 + pc - 0x4000 if pc >= 0x4000 else pc
            put('wRomChecksum', data[offset] | data[offset + 1] << 8, 2)
            put('wCurHPAnimMaxHP', 150, 2)
            put('wCurHPAnimOldHP', old, 2)
            put('wCurHPAnimNewHP', new, 2)
            # A post-damage caller has already committed the new active HP.
            active_hp = sy[('wBattleMonHP', 'wEnemyMonHP')[side]][1]
            state.m[active_hp], state.m[active_hp + 1] = new >> 8, new & 255
            state.m[active_hp + 2], state.m[active_hp + 3] = 0, 150
            initial = State.from_buffer_copy(state)
            count = C.c_uint32()
            mg.projection_run(C.byref(state), 2000000, sy['Crash'][1], sy['VBlank'][1], C.byref(count))
            changes = [address for address in (*range(0xc000, 0xe000), *range(0xff80, 0xffff))
                       if state.m[address] != initial.m[address]]
            aliases = {name: {'address': pc, 'before': initial.m[pc], 'after': state.m[pc]}
                       for name, (bank, pc) in sy.items()
                       if pc in changes and name.startswith(('w', 'h')) and '.' not in name}
            gameplay = [(sy[stem][1], sy[stem + 'StructEnd'][1])
                        for stem in ('wBattleMon', 'wEnemyMon')]
            differences = [address for lo, hi in gameplay for address in range(lo, hi)
                           if initial.m[address] != state.m[address]]
            assert not state.fault, (side, old, new, state.fault, hex(state.pc))
            assert count.value > 0 and state.event_count == count.value * 3
            assert not differences, differences
            assert state.sp == initial.sp + 2
            assert state.m[sy['wCurHPAnimOldHP'][1]] == new
            assert (state.b, state.c) == (initial.b, initial.c)
            rows.append(dict(side=side, old=old, new=new, fault=state.fault,
                pc=state.pc, bank=state.bank, steps=state.steps, vblanks=count.value,
                div_reads=state.event_count, gameplay_differences=differences,
                changed_aliases=aliases, changed_addresses=changes,
                registers={name: [getattr(initial, name), getattr(state, name)]
                           for name in 'afbcdehl'},
                final_sha256=hashlib.sha256(bytes(state)).hexdigest()))
            case_name = f'side-{side}-hp-{old}-{new}'
            (build / (case_name + '-initial.bin')).write_bytes(bytes(initial))
            (build / (case_name + '-final.bin')).write_bytes(bytes(state))
            if state.fault:
                break
        if rows[-1]['fault']:
            break
    mg.projection_close()
    result = dict(schema=1, profile='actual-unhooked-hp-frontend',
        rom_sha256=hashlib.sha256(data).hexdigest(),
        symbols_sha256=hashlib.sha256(sym.read_bytes()).hexdigest(),
        tool_sha256={path.name: hashlib.sha256(path.read_bytes()).hexdigest()
                     for path in (pathlib.Path(__file__), ROOT / 'tools/trainer_ai/battle_projection_oracle.c')},
        source_sha256={path: hashlib.sha256((game / path).read_bytes()).hexdigest()
                       for path in ('engine/battle/core.asm', 'engine/battle/anim_hp_bar.asm',
                                    'home/vblank.asm', 'home/delay.asm', 'home/video.asm',
                                    'home/random.asm', 'macros/ram.asm')},
        fixture_constructor_sha256=hashlib.sha256((ROOT / 'tools/trainer_ai/battle_env.py').read_bytes()).hexdigest(),
        rows=rows)
    assert len(rows) == 6
    (build / 'report.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({'cases': len(rows), 'faults': [row['fault'] for row in rows],
                      'vblanks': [row['vblanks'] for row in rows]}))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--project', type=pathlib.Path, required=True)
    run(parser.parse_args().project)
