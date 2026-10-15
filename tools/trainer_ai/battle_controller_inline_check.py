"""Discovery ABI checks plus actual AnonBankPush/stack-dispatch ROM parity."""
import argparse
import ctypes as C
import json
import pathlib
from stateful_fixture_check import State
from battle_controller_check import Oracle
from battle_env import NativeFight
import stateful_lifting
import battle_controller_profile as profile


def main(root):
    game = root / 'build/stateful/game'
    rom, symbols = game / 'polishedcrystal-3.2.3.gbc', game / 'polishedcrystal-3.2.3.sym'
    syms = stateful_lifting.symbols(symbols)
    directory = root / 'build/battle-controller/inline-abi'
    directory.mkdir(parents=True, exist_ok=True)
    image = bytearray(3 * 0x4000)
    image[0x100] = 0xc9
    image[0x200:0x207] = bytes((0xcd, 0, 1, 2, 6, 7, 0xc9))
    blocks, seen = stateful_lifting.compile_rom(image, [(1, 0x200)],
                                               call_abis={(0, 0x100): 'skip-byte'})
    assert (2, 0x204) in seen and (1, 0x204) not in seen
    assert 'push(s,515)' in blocks[0x200]
    image[0x4000:0x4005] = bytes((0xcd, 0, 1, 0xd3, 0xc9))
    blocks, _ = stateful_lifting.compile_rom(image, [(1, 0x4000)],
                                             call_abis={(0, 0x100): 'tail-table'})
    assert (1 << 16 | 0x4003) not in blocks
    try:
        stateful_lifting.compile_rom(image, [(1, 0x4000)], call_abis={(0, 0x100): 'skip-byte'})
    except ValueError as error:
        assert 'Banked inline-byte' in str(error)
    else:
        raise AssertionError('Unproven banked continuation accepted')
    rows = []
    native_dir = directory / 'ability'
    entries = ('GetAbility', '_ReturnFarCall', 'DoNothing')
    stateful_lifting.build(rom, symbols, entries, native_dir, game, call_abis=profile.CALL_ABIS)
    library = stateful_lifting.load_verified(native_dir, rom, symbols, game, entries, call_abis=profile.CALL_ABIS)
    library.stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]
    class Image:
        rom_path = rom
    oracle = Oracle(root, Image()).oracle
    def compare(state, tag):
        ref = State.from_buffer_copy(state)
        oracle.oracle_run(C.byref(ref), 100000, 65534, state.bank, syms['hROMBank'][1], None, 0)
        assert not ref.fault, (tag, ref.fault, ref.pc)
        library.stateful_run(C.byref(state), 100000, 65534)
        assert not state.fault, (tag, state.fault, state.pc)
        for name in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'):
            assert getattr(ref, name) == getattr(state, name), (tag, name)
        assert all(ref.m[address] == state.m[address] for address in
                   (*range(0xc000, 0xe000), *range(0xff80, 0xffff))), tag
        rows.append(tag)
    def initial(pc, bank, number):
        state = State()
        state.pc, state.bank, state.sp = pc, bank, syms['wStackTop'][1] - 2
        state.m[state.sp], state.m[state.sp + 1] = 254, 255
        state.m[syms['hROMBank'][1]] = bank
        for index, name in enumerate('afbcdehl'):
            setattr(state, name, ((number * 17 + index * 11) & 255) & (240 if name == 'f' else 255))
        return state
    lookup = object.__new__(NativeFight)
    lookup.source_root, lookup._constant_cache = game, {}
    for number in range(16):
        state = initial(syms['GetAbility'][1], 1 + number % 4, number)
        state.c = lookup._constant('constants/pokemon_constants.asm',
                                  ('SMEARGLE', 'PSYDUCK', 'SENTRET', 'PIDGEY')[number % 4])
        pointer = syms['wPartyMon1Personality'][1]
        state.h, state.l = pointer >> 8, pointer & 255
        state.m[pointer] = ((number % 3) + 1) << 5
        state.m[pointer + 1] = 0
        state.m[syms['wInitialOptions'][1]] = 2
        compare(state, dict(kind='actual-GetAbility-AnonBankPush', case=number))
    try:
        stateful_lifting.load_verified(native_dir, rom, symbols, game, entries)
    except ValueError as error:
        mismatch = str(error)
    else:
        raise AssertionError('Inline ABI manifest accepted missing declaration')
    oracle.oracle_close()
    # The following ROM-only callsites exercise the unmodified actual source
    # StackJumpTable family. New synthetic callers are not game-rule evidence.
    data = bytearray(rom.read_bytes())
    offset = 127 * 0x4000
    names = ('StackJumpTable', 'StandardStackJumpTable', 'OffsetStackJumpTable')
    locations = (0x4000, 0x4010, 0x4040)
    for name, pc in zip(names, locations):
        target = syms[name][1]
        start = offset + pc - 0x4000
        data[start:start + 7] = bytes((0xcd, target & 255, target >> 8, 0x20, 0x40, 0x30, 0x40))
    data[offset + 0x20:offset + 0x23] = bytes((6, 7, 0xc9))
    data[offset + 0x30:offset + 0x33] = bytes((6, 9, 0xc9))
    probe_rom, probe_sym = directory / 'dispatch.gbc', directory / 'dispatch.sym'
    probe_rom.write_bytes(data)
    extra = ''.join(f'7f:{pc:04x} Probe{i}\n' for i, pc in enumerate(locations))
    probe_sym.write_text(symbols.read_text() + '\n' + extra + '7f:4020 Target0\n7f:4030 Target1\n')
    probe_entries = ('Probe0', 'Probe1', 'Probe2', 'Target0', 'Target1')
    native_dir = directory / 'dispatch'
    stateful_lifting.build(probe_rom, probe_sym, probe_entries, native_dir, call_abis=profile.CALL_ABIS)
    library = stateful_lifting.load_verified(native_dir, probe_rom, probe_sym,
        entries=probe_entries, call_abis=profile.CALL_ABIS)
    library.stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]
    assert oracle.oracle_open(str(probe_rom).encode()) == 0
    for which, pc in enumerate(locations):
        for number in range(16):
            state = initial(pc, 127, number)
            state.a = number % 2
            state.m[syms['wJumptableIndex'][1]] = number % 2
            if which == 2:
                state.h, state.l, state.b, state.c = 0xc6, 0, 0, 0
                state.m[0xc600] = number % 2
            compare(state, dict(kind=names[which], case=number))
            assert state.b == (7 if number % 2 == 0 else 9)
    oracle.oracle_close()
    report = dict(comparisons=rows, inline_return_push_preserved=True,
                  inline_bank_context_tracked=True, table_data_not_decoded=True,
                  banked_unproven_continuation_rejected=True, configuration_rejected=mismatch)
    (directory / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(dict(comparisons=len(rows), guards='passed')))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    main(parser.parse_args().artifact_root)
