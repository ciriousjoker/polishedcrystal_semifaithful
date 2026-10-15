"""Actual audio-helper parity and strictly bounded optional MMIO-write guards."""
import argparse
import ctypes as C
import json
import pathlib
from stateful_fixture_check import State
import stateful_lifting
from battle_controller_check import Oracle


def main(root):
    game = root / 'build/stateful/game'
    rom, symbols = game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym'
    syms = stateful_lifting.symbols(symbols)
    entries = ('TerminateExpBarSound', 'DoNothing')
    build = root / 'build/battle-controller/mmio'
    projected = tuple(range(0xff10, 0xff15))
    libs = {}
    for name, addresses in (('strict', ()), ('projected', projected)):
        directory = build / name
        stateful_lifting.build(rom, symbols, entries, directory, game, write_only_mmio=addresses)
        libs[name] = stateful_lifting.load_verified(directory, rom, symbols, game, entries,
                                                   write_only_mmio=addresses)
        libs[name].stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]
    class Image:
        rom_path = rom
    oracle = Oracle(root, Image()).oracle
    rows = []
    for index in range(16):
        state = State()
        state.pc, state.bank = syms['TerminateExpBarSound'][1], 1 + index % 4
        state.sp = syms['wStackTop'][1] - 2
        state.m[state.sp], state.m[state.sp + 1] = 254, 255
        state.m[syms['hROMBank'][1]] = state.bank
        for position, name in enumerate('afbcdehl'):
            setattr(state, name, ((index * 17 + position * 29) & 255) & (240 if name == 'f' else 255))
        state.m[syms['wChannel5Flags'][1]] = 255 - index
        state.m[syms['wSoundInput'][1]] = index + 1
        reference = State.from_buffer_copy(state)
        oracle.oracle_run(C.byref(reference), 100000, 65534, state.bank,
                          syms['hROMBank'][1], None, 0)
        assert not reference.fault
        strict = State.from_buffer_copy(state)
        libs['strict'].stateful_run(C.byref(strict), 100000, 65534)
        assert strict.fault == 2
        native = State.from_buffer_copy(state)
        libs['projected'].stateful_run(C.byref(native), 100000, 65534)
        assert not native.fault
        for name in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'):
            assert getattr(reference, name) == getattr(native, name), (index, name)
        assert all(reference.m[address] == native.m[address] for address in
                   (*range(0xc000, 0xe000), *range(0xff80, 0xffff)))
        assert native.a == 0 and native.f == 128
        assert not native.m[syms['wChannel5Flags'][1]] and not native.m[syms['wSoundInput'][1]]
        rows.append(dict(case=index, strict_fault=strict.fault, projected_fault=native.fault))
    try:
        stateful_lifting.load_verified(build / 'projected', rom, symbols, game, entries)
    except ValueError as error:
        mismatch = str(error)
    else:
        raise AssertionError('Projected artifact accepted strict caller configuration')
    for invalid in ((0xff15,), (0xff10, 0xff10), (True,)):
        try:
            stateful_lifting.validate_write_only_mmio(invalid)
        except ValueError:
            pass
        else:
            raise AssertionError('Unapproved MMIO projection address accepted')
    # Separate ROM-only backend unit images exercise prohibited audio reads and
    # adjacent writes. No source/game-rule claim is made for these tiny snippets.
    image = bytearray(rom.read_bytes())
    offset = 127 * 0x4000
    image[offset:offset + 8] = bytes((0xf0, 0x10, 0xc9, 0x3e, 0, 0xe0, 0x15, 0xc9))
    unit_rom = build / 'guard.gbc'
    unit_sym = build / 'guard.sym'
    unit_rom.write_bytes(image)
    unit_sym.write_text(symbols.read_text() + '\n7f:4000 ReadGuard\n7f:4003 WriteGuard\n')
    stateful_lifting.build(unit_rom, unit_sym, ('ReadGuard', 'WriteGuard'),
                          build / 'guards', write_only_mmio=projected)
    guards = stateful_lifting.load_verified(build / 'guards', unit_rom, unit_sym,
        entries=('ReadGuard', 'WriteGuard'), write_only_mmio=projected)
    for pc in (0x4000, 0x4003):
        state = State()
        state.bank, state.pc, state.sp = 127, pc, syms['wStackTop'][1] - 2
        state.m[state.sp], state.m[state.sp + 1] = 254, 255
        guards.stateful_run(C.byref(state), 100000, 65534)
        assert state.fault == 2
    report = dict(actual_rom_cases=rows, allowed_write_addresses=projected,
                  strict_configuration_rejected=mismatch, unapproved_addresses_rejected=True,
                  reads_and_adjacent_writes_rejected=True, real_stack_top=syms['wStackTop'][1])
    (build / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(dict(actual_rom_cases=len(rows), guards='passed')))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    main(parser.parse_args().artifact_root)
