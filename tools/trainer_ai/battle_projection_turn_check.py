"""Source caller continuation under mixed real HP/HUD and logical RNG replay.

No native engine is loaded. The input is a frozen actual controller snapshot.
Other UI hooks remain explicit. A mismatch produces retained diagnostic evidence
and fails; it is not excused by adding RAM masks.
"""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
import subprocess
from stateful_fixture_check import State, symbols
import battle_controller_profile as profile

ROOT = pathlib.Path(__file__).resolve().parents[2]
REAL_UI = ('UpdateHPBar', 'UpdatePlayerHUD', 'UpdateEnemyHUD')


class Location(C.Structure):
    _fields_ = [('bank', C.c_uint16), ('pc', C.c_uint16)]


class Draw(C.Structure):
    _fields_ = [('bank', C.c_uint16), ('caller', C.c_uint16),
                ('origin_bank', C.c_uint16), ('origin_pc', C.c_uint16), ('a', C.c_uint8), ('f', C.c_uint8), ('add', C.c_uint8), ('sub', C.c_uint8)]


def run(project):
    snapshots = project / 'build/battle-controller/snapshots'
    metadata = json.loads((snapshots / 'initial-do-move.json').read_text())
    state_bytes = (snapshots / 'initial-do-move.state').read_bytes()
    assert hashlib.sha256(state_bytes).hexdigest() == metadata['state_sha256']
    rom, sym = pathlib.Path(metadata['rom']), pathlib.Path(metadata['symbols'])
    for key, path in (('rom', rom), ('symbols', sym)):
        assert hashlib.sha256(path.read_bytes()).hexdigest() == metadata[key + '_sha256']
    sy, data = symbols(sym), rom.read_bytes()
    states = [State.from_buffer_copy(state_bytes) for _ in range(2)]
    assert len(state_bytes) == C.sizeof(State)
    assert states[0].pc == sy[metadata['entry']][1]
    assert states[0].m[sy['wInitialOptions2'][1]] & 4, 'Fixed-level NO_EXP required'
    # Supply valid frontend initialization in both copies. These writes are part
    # of the declared input, not a repair performed during combat continuation.
    for state in states:
        state.fault = state.steps = state.event_count = state.event_pos = 0
        state.m[sy['hCGB'][1]] = 1
        for name, target, count in (('OAMDMACode', 'hPushOAM', 6), ('RomHeaderChecksum', 'wRomChecksum', 2)):
            bank, pc = sy[name]
            offset = bank * 0x4000 + pc - 0x4000 if pc >= 0x4000 else pc
            address = sy[target][1]
            state.m[address:address + count] = data[offset:offset + count]
        address = sy['hBGMapAddress'][1]
        state.m[address:address + 2] = bytes((0, 0x98))
    build = project / 'build/battle-projection/turn-v3'
    build.mkdir(parents=True, exist_ok=True)
    source = ROOT / 'tools/trainer_ai/battle_projection_turn_oracle.c'
    prefix = project / 'build/stateful/mgba'
    library = build / 'mixed-ui.dylib'
    subprocess.run(['cc', '-O2', '-dynamiclib', '-I' + str(prefix / 'include'),
        str(source), '-L' + str(prefix / 'lib'), '-lmgba', '-Wl,-rpath,' + str(prefix / 'lib'),
        '-o', str(library)], check=True)
    mg = C.CDLL(str(library))
    mg.turn_open.argtypes = [C.c_char_p]
    mg.turn_run.argtypes = [C.POINTER(State), C.c_uint, C.c_uint32,
        C.POINTER(Location), C.c_uint, C.POINTER(Location), C.c_uint,
        C.c_uint16, C.c_uint16, C.c_uint16, C.c_uint16, C.c_uint16,
        C.c_uint16, C.c_uint16, C.c_uint16, C.POINTER(C.c_uint32)]
    mg.turn_draw_count.restype = C.c_uint
    mg.turn_draws.restype = C.POINTER(Draw)
    mg.turn_consumer_pc.argtypes = mg.turn_consumer_bank.argtypes = [C.c_uint]
    mg.turn_consumer_pc.restype = mg.turn_consumer_bank.restype = C.POINTER(C.c_uint16)
    mg.turn_consumer_step.argtypes = [C.c_uint]
    mg.turn_consumer_step.restype = C.POINTER(C.c_uint32)
    hooks = (Location * len(profile.PRESENTATION))(*[Location(*sy[name]) for name in profile.PRESENTATION])
    real_ui = (Location * len(REAL_UI))(*[Location(*sy[name]) for name in REAL_UI])
    # Conservative source-defined gameplay regions. The broad battle block is
    # compared without carving out animation scratch aliases from that block.
    region_names = [('wBattle', 'wBattleEnd'), ('wEnemyMon', 'wEnemyMonStructEnd'),
                    ('wPartyCount', 'wPartyMonNicknamesEnd'), ('wOTPartyData', 'wOTPartyDataEnd'),
                    ('wNumItems', 'wBerriesEnd')]
    regions = [(name, sy[name][1], sy[end][1]) for name, end in region_names]
    singles = ('wCurBattleMon', 'wCurMoveNum', 'wBattlePlayerAction', 'wBattleResult',
               'wBattleMode', 'wBattleType', 'wTotalBattleTurns')
    regions += [(name, sy[name][1], sy[name][1] + 1) for name in singles]
    regions += [('wCurDamage', sy['wCurDamage'][1], sy['wCurDamage'][1] + 2)]
    reverse = {}
    for name, (_, pc) in sy.items():
        if name.startswith(('w', 'h')) and '.' not in name:
            reverse.setdefault(pc, []).append(name)
    rows = []
    assert mg.turn_open(str(rom).encode()) == 0
    try:
        for phase in (metadata['entry'], 'HandleBetweenTurnEffects'):
            if rows:
                for state in states:
                    state.bank = sy[phase][0] or state.bank
                    state.pc = sy[phase][1]
                    state.m[sy['hROMBank'][1]] = state.bank
                    state.sp -= 2
                    state.m[state.sp:state.sp + 2] = bytes((254, 255))
                    state.fault = 0
            before = [State.from_buffer_copy(state) for state in states]
            stats = [(C.c_uint32 * 5)() for _ in range(2)]
            for mode, state in enumerate(states):
                mg.turn_run(C.byref(state), mode, 3000000, hooks, len(hooks), real_ui, len(real_ui),
                    sy['Random'][1], sy['hRandomAdd'][1], sy['hRandomSub'][1],
                    sy['Crash'][1], sy['VBlank'][1], sy['FarCall'][1] + 4, sy['DoNothing'][1], sy['BattleRandomRange.loop2'][1] + 3, stats[mode])
            ref, actual = states
            gameplay = [{'address': a, 'names': reverse.get(a, []), 'reference': ref.m[a], 'actual': actual.m[a]}
                        for _, lo, hi in regions for a in range(lo, hi) if ref.m[a] != actual.m[a]]
            all_differences = [{'address': a, 'names': reverse.get(a, []), 'reference': ref.m[a], 'actual': actual.m[a]}
                for a in (*range(0xc000, 0xe000), *range(0xff80, 0xffff)) if ref.m[a] != actual.m[a]]
            consumer_pc, consumer_bank = mg.turn_consumer_pc(1), mg.turn_consumer_bank(1)
            consumer_step = mg.turn_consumer_step(1)
            consumers = [{'address': a, 'names': reverse.get(a, []),
                          'consumer_pc': consumer_pc[a], 'consumer_bank': consumer_bank[a],
                          'consumer_step': consumer_step[a]}
                         for a in range(65536) if consumer_pc[a]]
            consumers.sort(key=lambda item: item['consumer_step'])
            draws = [{name: getattr(mg.turn_draws()[i], name) for name, _ in Draw._fields_}
                     for i in range(mg.turn_draw_count())]
            row = dict(phase=phase, faults=[state.fault for state in states],
                pc=[state.pc for state in states], bank=[state.bank for state in states],
                sp=[state.sp for state in states],
                stats=[list(value) for value in stats], gameplay_differences=gameplay,
                all_ram_differences=all_differences, ui_write_combat_consumers=consumers,
                registers={name: [getattr(ref, name), getattr(actual, name)] for name in 'afbcdehl'},
                logical_draws=draws)
            rows.append(row)
            for mode, state in enumerate(states):
                (build / f'{phase}-{mode}-initial.bin').write_bytes(bytes(before[mode]))
                (build / f'{phase}-{mode}-final.bin').write_bytes(bytes(state))
            if any(state.fault for state in states) or gameplay or stats[0][0] != stats[1][1]:
                break
    finally:
        mg.turn_close()
    report = dict(schema=3, scope='one-actual-do_move-then-full-endturn-mixed-hp-hud',
        remaining_identity_hooks=list(profile.PRESENTATION), actual_ui_roots=REAL_UI,
        choices_are_not_hooked=True, regions=regions, snapshot_metadata=metadata,
        snapshot_metadata_sha256=hashlib.sha256((snapshots / 'initial-do-move.json').read_bytes()).hexdigest(),
        tool_sha256={path.name: hashlib.sha256(path.read_bytes()).hexdigest()
                     for path in (pathlib.Path(__file__), source, ROOT / 'tools/trainer_ai/battle_controller_profile.py')}, rows=rows)
    (build / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({'phases': len(rows), 'faults': [r['faults'] for r in rows],
                      'gameplay_differences': [len(r['gameplay_differences']) for r in rows]}))
    assert len(rows) == 2 and all(r['faults'] == [0, 0] for r in rows), 'Source continuation fault; inspect report'
    assert all(r['pc'] == [65534, 65534] and r['sp'][0] == r['sp'][1] and r['bank'][0] == r['bank'][1] for r in rows), 'Source phase/bank/stack mismatch'
    assert not any(r['gameplay_differences'] for r in rows), 'Gameplay projection mismatch; inspect first consumer'
    assert all(r['stats'][0][0] == r['stats'][1][1] for r in rows), 'Logical draw consumption mismatch'
    assert len({(draw['origin_bank'], draw['origin_pc']) for draw in rows[0]['logical_draws']}) > 1, 'Logical caller identity was not distinguished'


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--project', type=pathlib.Path, required=True)
    run(parser.parse_args().project)
