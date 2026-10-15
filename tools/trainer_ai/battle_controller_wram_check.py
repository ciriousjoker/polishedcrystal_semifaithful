"""All-bank CGB WRAM parity and real source window push/pop checks."""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
from battle_controller import ControllerMachine, Boundary
from battle_controller_check import Oracle
from battle_env import UnsupportedFight
import stateful_lifting


def machine_for(rom, symbols, source, directory, entries, *, boundaries=(), write_only_mmio=(), lcd_disabled=False):
    stateful_lifting.build(rom, symbols, entries, directory, source,
                          banked_wram=True, optimization='O0', boundaries=boundaries,
                          write_only_mmio=write_only_mmio, lcd_disabled=lcd_disabled)
    m = object.__new__(ControllerMachine)
    m.rom_path, m.symbols_path, m.source_root, m.build_dir = rom, symbols, source, directory
    m.rom, m.symbols, m.entries = rom.read_bytes(), stateful_lifting.symbols(symbols), entries
    m.lib = stateful_lifting.load_verified(directory, rom, symbols, source, entries,
                                         banked_wram=True, optimization='O0', boundaries=boundaries,
                                         write_only_mmio=write_only_mmio, lcd_disabled=lcd_disabled)
    m.lib.stateful_run_banked.argtypes = [C.POINTER(__import__('stateful_fixture_check').State),
                                        C.c_uint32, C.c_uint16, C.c_uint16]
    m._constant_cache, m.banked_wram = {}, True
    m.boundaries = {(pc if pc < 0x4000 else bank << 16 | pc): Boundary(name, bank, pc)
                    for name in boundaries for bank, pc in [m.symbols[name]]}
    m.segment_checker = m.single_step_checker = m.on_choice = None
    m.reset_empty(193)
    return m


def main(root):
    game = root / 'build/stateful/game'
    directory = root / 'build/battle-controller/banked-wram'
    directory.mkdir(parents=True, exist_ok=True)
    rom, symbols = game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym'
    entries = ('ClearWindowData', 'PushWindow', 'ExitMenu', 'DoNothing')
    m = machine_for(rom, symbols, game, directory / 'windows', entries)
    oracle = Oracle(root, m)
    m.segment_checker = oracle.compare
    for bank in range(2, 8):
        m.state.m[(bank-2)*0x1000:(bank-1)*0x1000] = bytes([bank * 17]) * 0x1000
    bank1 = bytes(m.state.m[0xd000:0xe000])
    for selected in (0, 2, 7):
        m.state.m[0xff70] = selected
        m.call('ClearWindowData')
        assert m.state.m[0xff70] == selected
        assert bytes(m.state.m[0xd000:0xe000]) == bank1
    m.state.m[0xff70] = 1
    m.call('ClearWindowData')
    assert m.state.m[0xff70] == 1
    assert bytes(m.state.m[0xd000:0xe000]) == bank1
    base = m.byte('wWindowStackSize')
    header = m.address('wMenuHeader')
    length = m.address('wMenuHeaderEnd') - header
    saved = []
    for index in range(2):
        data = bytearray(length)
        data[:5] = bytes((64, index, index, index + 2, index + 3))
        data[7] = index + 1
        m.state.m[header:header + length] = data
        saved.append(data)
        m.call('PushWindow')
        assert m.byte('wWindowStackSize') == base + index + 1
        assert m.state.m[0xff70] == 1
    m.call('ExitMenu')
    expected = bytearray(saved[0])
    expected[0] |= 1
    assert bytes(m.state.m[header:header + length]) == expected
    assert m.byte('wWindowStackSize') == base + 1
    m.call('ExitMenu')
    assert m.byte('wWindowStackSize') == base and m.state.m[0xff70] == 1
    assert bytes(m.state.m[0xd000:0xe000]) == bank1
    window_rows = list(oracle.rows)
    oracle.oracle.oracle_close()

    # New synthetic callsites test the machine memory model, not game rules.
    image = bytearray(rom.read_bytes())
    offset = 127 * 0x4000
    image[offset:offset+17] = bytes((0xe0, 0x70, 0x78, 0xea, 0, 0xd0,
        0xf0, 0x70, 0xe0, 0x80, 0xfa, 0, 0, 0xe0, 0x81, 0xc9, 0))
    image[offset+0x20:offset+0x23] = bytes((0xf0, 0x71, 0xc9))
    image[offset+0x30:offset+0x33] = bytes((0xe0, 0x71, 0xc9))
    probe_rom, probe_sym = directory / 'memory.gbc', directory / 'memory.sym'
    probe_rom.write_bytes(image)
    probe_sym.write_text(symbols.read_text() + '\n7f:4000 ProbeAccess\n7f:4020 ProbeBadRead\n7f:4030 ProbeBadWrite\n')
    entries = ('ProbeAccess', 'ProbeBadRead', 'ProbeBadWrite')
    m = machine_for(probe_rom, probe_sym, None, directory / 'memory', entries)
    oracle = Oracle(root, m)
    m.segment_checker = oracle.compare
    for raw in range(8):
        m.state.a, m.state.b = raw, 90 + raw
        m.call('ProbeAccess')
        assert m.state.m[0xff70] == raw
        assert m.state.m[0xff80] == 248 | raw
        assert m.state.m[0xff81] == image[0]
        address = 0xd000 if raw < 2 else (raw-2)*0x1000
        assert m.state.m[address] == 90 + raw
    assert m.state.m[0xd000] == 91  # Raw 0 and raw 1 alias the same bank.
    for raw in range(2, 8):
        assert m.state.m[(raw-2)*0x1000] == 90 + raw
    memory_rows = list(oracle.rows)
    m.segment_checker = None
    for name in ('ProbeBadRead', 'ProbeBadWrite'):
        m.state.fault = 0
        try:
            m.call(name)
        except UnsupportedFight:
            assert m.state.fault == 2
        else:
            raise AssertionError('Unrelated MMIO accepted')
    oracle.oracle.oracle_close()
    try:
        stateful_lifting.load_verified(directory / 'memory', probe_rom, probe_sym,
            entries=entries, optimization='O0')
    except ValueError:
        pass
    else:
        raise AssertionError('Banked artifact accepted a flat configuration')
    strict = directory / 'strict'
    stateful_lifting.build(probe_rom, probe_sym, entries, strict, optimization='O0')
    lib = stateful_lifting.load_verified(strict, probe_rom, probe_sym, entries=entries, optimization='O0')
    lib.stateful_run_banked.argtypes = m.lib.stateful_run_banked.argtypes
    m.reset_empty(193)
    m.prepare_call('ProbeAccess')
    lib.stateful_run_banked(C.byref(m.state), 1000, 65534, m.state.bank)
    assert m.state.fault == 2
    report = dict(passed=True, source_window_comparisons=window_rows,
        memory_model_comparisons=memory_rows, compared=['cpu', 'hram', 'wram-banks-0-7', 'svbk'],
        alias_zero_one=True, bank_isolation=True, source_nested_header_restore=True,
        strict_svbk_rejected=True, unrelated_mmio_rejected=True,
        configuration_mismatch_rejected=True, generator_sha256=stateful_lifting.MODULE_DIGEST,
        fixture_sha256=hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest())
    (directory / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(dict(passed=True, comparisons=len(window_rows)+len(memory_rows))))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', required=True, type=pathlib.Path)
    main(parser.parse_args().artifact_root)
