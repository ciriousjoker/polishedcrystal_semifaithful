"""Exhaustive SM83 DAA CPU/RAM parity at the actual source PrintNum site."""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
from stateful_fixture_check import State
from battle_controller_check import Oracle
import stateful_lifting


def main(root):
    game = root / 'build/stateful/game'
    rom, sym = game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym'
    syms, image = stateful_lifting.symbols(sym), rom.read_bytes()
    bank, begin = syms['_PrintNum.loop2']
    _, end = syms['_PrintNum.loop3']
    offset = bank * 0x4000 + begin - 0x4000
    site = image[offset:offset + end - begin].find(bytes((0x7e, 0x8f, 0x27, 0x32)))
    assert site >= 0, 'Actual PrintNum DAA source sequence changed'
    pc = begin + site + 2
    entries, boundaries = ((bank, pc),), ((bank, pc + 1),)
    directory = root / 'build/battle-controller/daa'
    stateful_lifting.build(rom, sym, entries, directory, game,
                          boundaries=boundaries, optimization='O0')
    library = stateful_lifting.load_verified(directory, rom, sym, game, entries,
                          boundaries=boundaries, optimization='O0')
    library.stateful_run_banked.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16, C.c_uint16]
    class Image:
        rom_path = rom
    oracle = Oracle(root, Image()).oracle
    digest = hashlib.sha256()
    for accumulator in range(256):
        for flags in range(0, 256, 16):
            state = State()
            state.a, state.f, state.pc, state.bank = accumulator, flags, pc, bank
            state.b, state.c, state.d, state.e, state.h, state.l = 17, 31, 47, 61, 79, 97
            state.sp = syms['wStackTop'][1]
            state.m[syms['hROMBank'][1]] = bank
            reference = State.from_buffer_copy(bytes(state))
            oracle.oracle_run(C.byref(reference), 4, pc + 1, bank,
                              syms['hROMBank'][1], None, 0)
            library.stateful_run_banked(C.byref(state), 4, pc + 1, bank)
            assert not state.fault and not reference.fault
            for name in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'):
                assert getattr(state, name) == getattr(reference, name), (accumulator, flags, name)
            assert bytes(state.m) == bytes(reference.m)
            assert not state.event_pos and not reference.event_count
            digest.update(bytes((accumulator, flags, state.a, state.f)))
    oracle.oracle_close()
    report = dict(passed=True, comparisons=4096, actual_rom_site=[bank, pc],
        cpu_and_all_memory_equal=True, no_random_draws=True,
        outputs_sha256=digest.hexdigest(), generator_sha256=stateful_lifting.MODULE_DIGEST,
        fixture_sha256=hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest())
    (directory / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(dict(passed=True, comparisons=4096)))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', required=True, type=pathlib.Path)
    main(parser.parse_args().artifact_root)
