"""Bank-aware stop ABI regression, including actual ROM GetAbility execution."""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
from stateful_fixture_check import State
from battle_controller_check import Oracle
import stateful_lifting
import battle_controller_profile as profile


def main(root):
    game = root / 'build/stateful/game'
    directory = root / 'build/battle-controller/banked-stop'
    directory.mkdir(parents=True, exist_ok=True)
    original = game / 'polishedcrystal-3.2.3.gbc'
    symbols = game / 'polishedcrystal-3.2.3.sym'
    syms = stateful_lifting.symbols(symbols)
    data = bytearray(original.read_bytes())
    data[127 * 0x4000:127 * 0x4000 + 5] = bytes((6, 7, 0xc3, 0, 1))
    data[0x100:0x108] = bytes((0x3e, 126, 0xea, 0, 0x20, 0xc3, 0, 0x40))
    data[126 * 0x4000] = 0xc9
    rom, sym = directory / 'probe.gbc', directory / 'probe.sym'
    rom.write_bytes(data)
    sym.write_text(symbols.read_text() + '\n7f:4000 StopProbe\n7e:4000 StopTarget\n00:0100 StopBridge\n')
    entries = ('GetAbility', '_ReturnFarCall', 'DoNothing', 'StopProbe', 'StopTarget')
    native = directory / 'native'
    stateful_lifting.build(rom, sym, entries, native, call_abis=profile.CALL_ABIS)
    library = stateful_lifting.load_verified(native, rom, sym, entries=entries,
                                             call_abis=profile.CALL_ABIS)
    for invalid in ('Ofast', 'O4', True):
        try:
            stateful_lifting.validate_optimization(invalid)
        except ValueError:
            pass
        else:
            raise AssertionError('Unsupported optimization accepted')
    try:
        stateful_lifting.load_verified(native, rom, sym, entries=entries,
            call_abis=profile.CALL_ABIS, optimization='O1')
    except ValueError as error:
        assert 'optimization configuration mismatch' in str(error)
    else:
        raise AssertionError('Compiler configuration mismatch accepted')
    optimized = stateful_lifting.build(rom, sym, entries, directory / 'native-o1',
        call_abis=profile.CALL_ABIS, optimization='O1')
    original_manifest = json.loads((native / 'manifest.json').read_text())
    assert pathlib.Path(optimized['library_path']).name != pathlib.Path(original_manifest['library_path']).name
    assert optimized['optimization'] == 'O1' and original_manifest['optimization'] == 'O3'
    library.stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]
    library.stateful_run_banked.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16, C.c_uint16]
    class Image:
        rom_path = rom
    oracle = Oracle(root, Image()).oracle
    rows = []
    def initial(pc, bank):
        state = State()
        state.pc, state.bank, state.sp = pc, bank, syms['wStackTop'][1] - 2
        state.m[state.sp], state.m[state.sp + 1] = 254, 255
        state.m[syms['hROMBank'][1]] = bank
        return state
    def compare(state, stop, bank, tag):
        reference = State.from_buffer_copy(state)
        oracle.oracle_run(C.byref(reference), 100000, stop, bank,
                          syms['hROMBank'][1], None, 0)
        assert not reference.fault, (tag, reference.fault)
        library.stateful_run_banked(C.byref(state), 100000, stop, bank)
        assert not state.fault, (tag, state.fault)
        for name in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'):
            assert getattr(state, name) == getattr(reference, name), (tag, name)
        assert all(state.m[a] == reference.m[a] for a in
                   (*range(0xc000, 0xe000), *range(0xff80, 0xffff))), tag
        rows.append(tag)
    state = initial(0x4000, 127)
    compare(state, 0x4000, 126, 'foreign-bank-same-PC-executes')
    assert state.b == 7 and state.pc == 0x4000 and state.bank == 126
    state = initial(0x4000, 127)
    compare(state, 0x4000, 127, 'exact-bank-stops-before-instruction')
    assert state.b == 0 and state.pc == 0x4000 and not state.steps
    state = initial(0x4000, 127)
    library.stateful_run(C.byref(state), 100000, 0x4000)
    assert state.pc == 0x4000 and not state.steps
    for bank in (1, 11, 63, 127):
        state = initial(syms['GetAbility'][1], bank)
        pointer = syms['wPartyMon1Personality'][1]
        state.h, state.l = pointer >> 8, pointer & 255
        state.c = 54
        state.m[pointer] = 0x20
        state.m[syms['wInitialOptions'][1]] = 2
        compare(state, syms['GetAbility.Function'][1], 0,
                'actual-ROM0-global-stop-' + str(bank))
        assert state.pc == syms['GetAbility.Function'][1]
        compare(state, 65534, 0, 'actual-GetAbility-continuation-' + str(bank))
    oracle.oracle_close()
    report = dict(comparisons=rows, legacy_wildcard_preserved=True,
        optimization_configuration_rejected=True, optimization_library_identity_distinct=True,
        state_abi_sha256=hashlib.sha256(stateful_lifting.ABI.encode()).hexdigest(),
        generator_sha256=stateful_lifting.MODULE_DIGEST)
    (directory / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(dict(comparisons=len(rows), legacy_wildcard='passed')))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    main(parser.parse_args().artifact_root)
