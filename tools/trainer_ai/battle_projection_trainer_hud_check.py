"""Focused actual EnemySwitch_TrainerHud versus LoadBallIconGFX-only omission.

Requires a valid banked CGB controller snapshot. Full physical WRAM differences
are recorded; gameplay, staged ball icons and shadow OAM must agree. Differences
are not repaired by broad scratch masks. This is not a replacement-controller
continuation or hardware randomness distribution proof.
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


class Location(C.Structure):
    _fields_ = [('bank', C.c_uint16), ('pc', C.c_uint16)]


class RandomABI(C.Structure):
    _fields_ = [(name, C.c_uint16) for name in ('random', 'add', 'sub', 'farcall_return', 'donothing', 'range_return')]


class Draw(C.Structure):
    _fields_ = [(name, C.c_uint16) for name in ('bank', 'ret', 'origin_bank', 'origin_pc')] + [(name, C.c_uint8) for name in ('a', 'f', 'add', 'sub')]


def run(project, snapshot, metadata_path, continuation=False):
    metadata = json.loads(metadata_path.read_text())
    initial_bytes = snapshot.read_bytes()
    assert len(initial_bytes) == C.sizeof(State)
    assert hashlib.sha256(initial_bytes).hexdigest() == metadata['state_sha256']
    rom, sym = pathlib.Path(metadata['rom']), pathlib.Path(metadata['symbols'])
    for name, path in (('rom', rom), ('symbols', sym)):
        assert hashlib.sha256(path.read_bytes()).hexdigest() == metadata[name + '_sha256']
    assert metadata.get('banked_wram') is True, 'Banked snapshot provenance is required'
    sy, image = symbols(sym), rom.read_bytes()
    initial = State.from_buffer_copy(initial_bytes)
    # Preserve the actual KO/team/rules state. Prepare only an isolated source
    # caller and its explicit frontend initialization before either mode executes.
    initial.bank, initial.pc = sy['EnemySwitch_TrainerHud']
    initial.m[sy['hROMBank'][1]] = initial.bank
    if continuation:
        assert metadata['next_stop'] == 65534
        assert metadata['entry'] == 'EnemySwitch_TrainerHud'
    else:
        initial.sp = sy['wStackTop'][1] - 2
        initial.m[initial.sp:initial.sp + 2] = bytes((254, 255))
    initial.fault = initial.steps = initial.event_count = initial.event_pos = 0
    initial.m[sy['hCGB'][1]] = 1
    initial.m[sy['hBGMapAddress'][1]:sy['hBGMapAddress'][1] + 2] = bytes((0, 0x98))
    for name, target, count in (('OAMDMACode', 'hPushOAM', 6), ('RomHeaderChecksum', 'wRomChecksum', 2)):
        bank, pc = sy[name]
        offset = bank * 0x4000 + pc - 0x4000 if pc >= 0x4000 else pc
        initial.m[sy[target][1]:sy[target][1] + count] = image[offset:offset + count]
    prefix = project / 'build/stateful/mgba'
    build = project / 'build/battle-projection/trainer-hud'
    if continuation:
        build /= 'continuation'
    build.mkdir(parents=True, exist_ok=True)
    source = ROOT / 'tools/trainer_ai/battle_projection_trainer_hud_oracle.c'
    library = build / 'trainer-hud.dylib'
    subprocess.run(['cc', '-O2', '-dynamiclib', '-I' + str(prefix / 'include'),
        str(source), '-L' + str(prefix / 'lib'), '-lmgba', '-Wl,-rpath,' + str(prefix / 'lib'),
        '-o', str(library)], check=True)
    mg = C.CDLL(str(library))
    mg.hud_open.argtypes = [C.c_char_p]
    mg.hud_run.argtypes = [C.POINTER(State), C.c_uint, C.c_uint32,
        C.c_uint16, C.c_uint16, C.c_uint16, C.c_uint16, C.POINTER(C.c_uint32)]
    mg.hud_continue.argtypes = [*mg.hud_run.argtypes, C.POINTER(Location), C.c_uint, C.POINTER(RandomABI)]
    mg.hud_draw_count.restype = C.c_uint
    mg.hud_draws.restype = C.POINTER(Draw)
    hooks = (Location * len(profile.PRESENTATION))(*[Location(*sy[name]) for name in profile.PRESENTATION])
    random_abi = RandomABI(sy['Random'][1], sy['hRandomAdd'][1], sy['hRandomSub'][1],
        sy['FarCall'][1] + 4, sy['DoNothing'][1], sy['BattleRandomRange.loop2'][1] + 3)
    assert mg.hud_open(str(rom).encode()) == 0
    states = [State.from_buffer_copy(initial) for _ in range(2)]
    stats = [(C.c_uint32 * (5 if continuation else 2))() for _ in range(2)]
    draws = []
    try:
        for mode in (0, 1):
            arguments = (C.byref(states[mode]), mode, 1000000, *sy['LoadBallIconGFX'],
                         sy['Crash'][1], sy['VBlank'][1], stats[mode])
            if continuation:
                mg.hud_continue(*arguments, hooks, len(hooks), C.byref(random_abi))
            else:
                mg.hud_run(*arguments)
        if continuation:
            draws = [{name: getattr(mg.hud_draws()[i], name) for name, _ in Draw._fields_}
                     for i in range(mg.hud_draw_count())]
    finally:
        mg.hud_close()
    def physical(bank, pc):
        return (bank - 2) * 0x1000 + pc - 0xd000 if 0xd000 <= pc < 0xe000 and bank > 1 else pc
    def location(name):
        bank, pc = sy[name]
        return physical(bank, pc)
    aliases = {}
    for name, (bank, pc) in sy.items():
        if name.startswith(('w', 'h')) and '.' not in name and (0xc000 <= pc < 0xe000 or 0xff80 <= pc < 0xffff):
            aliases.setdefault(physical(bank, pc), []).append(name)
    physical_addresses = [*range(0x6000), *range(0xc000, 0xe000), 0xff70, *range(0xff80, 0xffff)]
    ref, actual = states
    differences = [{'address': a, 'names': aliases.get(a, []), 'reference': ref.m[a], 'actual': actual.m[a]}
                   for a in physical_addresses if ref.m[a] != actual.m[a]]
    regions = [(name, location(name), location(end)) for name, end in
               (('wBattle', 'wBattleEnd'), ('wEnemyMon', 'wEnemyMonStructEnd'),
                ('wPartyCount', 'wPartyMonNicknamesEnd'), ('wOTPartyData', 'wOTPartyDataEnd'),
                ('wNumItems', 'wBerriesEnd'))]
    regions += [('wBattleHUDTiles', location('wBattleHUDTiles'), location('wBattleHUDTiles') + 6),
                ('wTrainerHUDTiles', location('wTrainerHUDTiles'), location('wTrainerHUDTiles') + 4),
                ('wShadowOAM', location('wShadowOAM'), location('wShadowOAM') + 160)]
    semantic = [{'region': name, 'address': a, 'names': aliases.get(a, []),
                 'reference': ref.m[a], 'actual': actual.m[a]}
                for name, lo, hi in regions for a in range(lo, hi) if ref.m[a] != actual.m[a]]
    report = dict(schema=1, scope='actual-trainer-hud-then-endturn-continuation' if continuation else 'actual-trainer-hud-vs-one-graphics-leaf-omission',
        actual_has_no_hooks_inside_hud=True, conditional_hook='LoadBallIconGFX',
        continuation=continuation, source_outer_frame_preserved=continuation,
        other_conditional_presentation=list(profile.PRESENTATION) if continuation else [],
        logical_random_draws=draws,
        ff48='Actual mGBA write in both paths; no subsequent read replaced',
        metadata=metadata, snapshot_sha256=hashlib.sha256(initial_bytes).hexdigest(),
        tool_sha256={path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in
                     (pathlib.Path(__file__), source, ROOT / 'tools/trainer_ai/battle_controller_profile.py')}, regions=regions,
        faults=[state.fault for state in states], steps=[state.steps for state in states],
        stats=[list(value) for value in stats],
        registers={name: [getattr(ref, name), getattr(actual, name)] for name in
                   ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank')},
        all_physical_wram_svbk_hram_differences=differences, semantic_differences=semantic,
        staged_ball_tiles=[[state.m[location('wBattleHUDTiles') + i] for i in range(6)] for state in states])
    (build / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    (build / 'initial.bin').write_bytes(bytes(initial))
    for mode, state in enumerate(states):
        (build / f'final-{mode}.bin').write_bytes(bytes(state))
    print(json.dumps({'faults': report['faults'], 'semantic_differences': len(semantic),
                      'raw_differences': len(differences), 'stats': report['stats']}))
    assert report['faults'] == [0, 0], 'Source HUD caller failed; inspect report'
    assert all(value[1] == 1 for value in stats), 'Graphics source leaf not reached exactly once'
    assert not semantic, 'Gameplay/HUD source-state mismatch; inspect consumers before changing contract'
    assert ref.pc == actual.pc == 65534 and ref.sp == actual.sp, 'Caller return contract mismatch'
    if continuation:
        assert stats[0][2] == stats[1][3], 'Logical RNG replay consumption mismatch'
        assert all(getattr(ref, name) == getattr(actual, name) for name in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank')), 'Source continuation CPU mismatch'
        assert ref.m[0xff70] == actual.m[0xff70], 'Source continuation SVBK mismatch'


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--project', type=pathlib.Path, required=True)
    parser.add_argument('--snapshot', type=pathlib.Path, required=True)
    parser.add_argument('--metadata', type=pathlib.Path, required=True)
    parser.add_argument('--continuation', action='store_true')
    args = parser.parse_args()
    run(args.project, args.snapshot, args.metadata, args.continuation)
