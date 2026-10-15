"""Conditional source trainer-HUD execution with a bounded OBJ-palette sink."""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
from battle_controller_check import Oracle
from battle_controller_wram_check import machine_for
from battle_env import UnsupportedFight
import stateful_lifting


def main(root):
    game = root / 'build/stateful/game'
    rom, sym = game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym'
    directory = root / 'build/battle-controller/trainer-hud'
    directory.mkdir(parents=True, exist_ok=True)
    entries, boundaries = ('EnemySwitch_TrainerHud', 'DoNothing'), ('LoadBallIconGFX',)
    m = machine_for(rom, sym, game, directory / 'hud', entries,
                   boundaries=boundaries, write_only_mmio=(0xff48,))
    oracle = Oracle(root, m)
    m.segment_checker = oracle.compare
    for number in range(8):
        m.reset_empty(193 + number)
        for i, name in enumerate('afbcdehl'):
            setattr(m.state, name, (number * 29 + i * 17) & (240 if name == 'f' else 255))
        m._put('wBattleMode', 2)
        m._put('wOTPartyCount', 3)
        for index, (hp, status) in enumerate(((100, 0), (100, 8), (0, 0))):
            stem = 'wOTPartyMon' + str(index + 1)
            m._put(stem + 'HP', hp, 2)
            m._put(stem + 'Status', status)
        before = bytes(m.state.m[m.address('wOTPartyMon1'):m.address('wOTPartyMon4')])
        m.call('EnemySwitch_TrainerHud')
        assert bytes(m.state.m[m.address('wOTPartyMon1'):m.address('wOTPartyMon4')]) == before
        at = m.address('wBattleHUDTiles')
        assert tuple(m.state.m[at:at+6]) == (49, 50, 51, 52, 52, 52)
        assert m.byte('wPlaceBallsX') == 24 and m.byte('wPlaceBallsY') == 32
    hud_rows = list(oracle.rows)
    oracle.oracle.oracle_close()
    data = bytearray(rom.read_bytes())
    base = 127 * 0x4000
    labels = []
    for index, (opcode, address) in enumerate(((0xe0, 0x48), (0xf0, 0x48),
                                               (0xe0, 0x47), (0xe0, 0x49))):
        pc = 0x4000 + index * 0x10
        data[base + index*0x10:base + index*0x10 + 3] = bytes((opcode, address, 0xc9))
        labels.append('Probe' + str(index))
    probe_rom, probe_sym = directory / 'sinks.gbc', directory / 'sinks.sym'
    probe_rom.write_bytes(data)
    probe_sym.write_text(sym.read_text() + '\n' + ''.join(
        f'7f:{0x4000+index*0x10:04x} {label}\n' for index, label in enumerate(labels)))
    m = machine_for(probe_rom, probe_sym, None, directory / 'sinks', tuple(labels),
                    write_only_mmio=(0xff48,))
    oracle = Oracle(root, m)
    m.segment_checker = oracle.compare
    m.state.a = 228
    m.call('Probe0')
    sink_rows = list(oracle.rows)
    oracle.oracle.oracle_close()
    m.segment_checker = None
    for label in labels[1:]:
        m.reset_empty(193)
        try:
            m.call(label)
        except UnsupportedFight:
            assert m.state.fault == 2
        else:
            raise AssertionError('Palette read or adjacent write unexpectedly accepted')
    strict = directory / 'strict'
    stateful_lifting.build(probe_rom, probe_sym, tuple(labels), strict, optimization='O0')
    lib = stateful_lifting.load_verified(strict, probe_rom, probe_sym, entries=tuple(labels), optimization='O0')
    lib.stateful_run_banked.argtypes = m.lib.stateful_run_banked.argtypes
    m.reset_empty(193)
    m.prepare_call('Probe0')
    lib.stateful_run_banked(C.byref(m.state), 1000, 65534, m.state.bank)
    assert m.state.fault == 2
    try:
        stateful_lifting.load_verified(directory / 'sinks', probe_rom, probe_sym,
            entries=tuple(labels), banked_wram=True, optimization='O0')
    except ValueError:
        pass
    else:
        raise AssertionError('Palette sink configuration mismatch accepted')
    for address in (0xff47, 0xff49):
        try:
            stateful_lifting.validate_write_only_mmio((address,))
        except ValueError:
            pass
        else:
            raise AssertionError('Undeclared palette sink configuration accepted')
    report = dict(passed=True, conditional_hud_comparisons=hud_rows,
        machine_sink_comparisons=sink_rows, renderer_leaf='LoadBallIconGFX',
        sink='FF48 write only', actual_hud_scratch_and_oam_executed=True,
        party_records_unchanged=True, adjacent_and_read_guards=True,
        default_strict_and_config_guards=True,
        compared=['cpu', 'hram', 'wram-banks-0-7', 'svbk'],
        scope='Conditional pixel-copy projection, not an unhooked HUD fidelity proof',
        generator_sha256=stateful_lifting.MODULE_DIGEST,
        fixture_sha256=hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest())
    (directory / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(dict(passed=True, comparisons=len(hud_rows)+len(sink_rows))))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', required=True, type=pathlib.Path)
    main(parser.parse_args().artifact_root)
