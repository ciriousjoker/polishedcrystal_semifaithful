"""Headless LCD-disabled read contract and actual source tilemap-fill parity."""
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
    directory = root / 'build/battle-controller/lcd-disabled'
    directory.mkdir(parents=True, exist_ok=True)
    m = machine_for(rom, sym, game, directory / 'tilemap', ('ClearTileMap',),
                    boundaries=('ApplyTilemapInVBlank',), lcd_disabled=True)
    oracle = Oracle(root, m)
    m.segment_checker = oracle.compare
    for seed in (193, 227):
        m.reset_empty(seed)
        start, end = m.address('wTilemap'), m.address('wTilemapEnd')
        m.state.m[start:end] = bytes([seed]) * (end-start)
        m.call('ClearTileMap')
        bank, pc = m.symbols['ClearTileMap']
        offset = pc if pc < 0x4000 else bank * 0x4000 + pc - 0x4000
        assert m.rom[offset] == 0x3e
        assert bytes(m.state.m[start:end]) == bytes([m.rom[offset+1]]) * (end-start)
    rows = list(oracle.rows)
    oracle.oracle.oracle_close()
    data = bytearray(rom.read_bytes())
    entries = tuple('Probe' + str(i) for i in range(4))
    for i, (op, reg) in enumerate(((0xf0,0x40),(0xe0,0x40),(0xf0,0x3f),(0xf0,0x41))):
        data[127*0x4000+i*16:127*0x4000+i*16+3] = bytes((op,reg,0xc9))
    probe_rom, probe_sym = directory/'probe.gbc', directory/'probe.sym'
    probe_rom.write_bytes(data)
    probe_sym.write_text(sym.read_text() + '\n' + ''.join(
        f'7f:{0x4000+i*16:04x} {name}\n' for i,name in enumerate(entries)))
    m = machine_for(probe_rom, probe_sym, None, directory/'read', entries, lcd_disabled=True)
    oracle = Oracle(root,m)
    m.segment_checker = oracle.compare
    m.state.a = 193
    m.call('Probe0')
    assert m.state.a == 0
    rows += oracle.rows
    oracle.oracle.oracle_close()
    m.segment_checker = None
    for entry in entries[1:]:
        m.reset_empty(193)
        try:
            m.call(entry)
        except UnsupportedFight:
            assert m.state.fault == 2
        else:
            raise AssertionError('Undeclared LCD read/write accepted')
    strict = directory/'strict'
    stateful_lifting.build(probe_rom, probe_sym, entries, strict, optimization='O0')
    lib = stateful_lifting.load_verified(strict, probe_rom, probe_sym, entries=entries, optimization='O0')
    lib.stateful_run_banked.argtypes = m.lib.stateful_run_banked.argtypes
    m.reset_empty(193)
    m.prepare_call('Probe0')
    lib.stateful_run_banked(C.byref(m.state),1000,65534,m.state.bank)
    assert m.state.fault == 2
    try:
        stateful_lifting.load_verified(directory/'read',probe_rom,probe_sym,entries=entries,
                                      banked_wram=True,optimization='O0',lcd_disabled=False)
    except ValueError:
        pass
    else:
        raise AssertionError('LCD configuration mismatch accepted')
    for value in (0,1,None,'yes'):
        for operation in (stateful_lifting.build,stateful_lifting.load_verified):
            try:
                operation(probe_rom,probe_sym,entries,strict,lcd_disabled=value) if operation is stateful_lifting.build else operation(strict,probe_rom,probe_sym,lcd_disabled=value)
            except ValueError:
                pass
            else:
                raise AssertionError('Non-Boolean LCD configuration accepted')
    report = dict(passed=True,comparisons=rows,source_tilemap_fill_executed=True,
                  strict_default_and_config_guards=True,adjacent_reads_and_writes_rejected=True,
                  scope='Explicit headless LCD-off reference; LCD-on frontend needs separate projection proof',
                  generator_sha256=stateful_lifting.MODULE_DIGEST,
                  fixture_sha256=hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest())
    (directory/'report.json').write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps(dict(passed=True,comparisons=len(rows))))


if __name__ == '__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root',type=pathlib.Path,required=True)
    main(parser.parse_args().artifact_root)
