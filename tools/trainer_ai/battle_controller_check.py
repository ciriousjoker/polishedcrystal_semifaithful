"""Actual controller differential runner at every explicit suspension boundary."""
import argparse
import ctypes as C
import hashlib
import json
import pathlib
import subprocess
import sys
from battle_controller import ControllerMachine
from battle_scenarios import Scenario, initialize
from battle_env import UnsupportedFight
from stateful_fixture_check import State
from battle_env_check import Hook
import battle_controller_profile as profile
import stateful_lifting


def memory_differences(reference, native, banked_wram=False):
    """Compare every modeled byte, expanding diagnostics only on mismatch.

    bytes equality uses a native memory comparison. The modeled regions and
    diagnostic address order are identical to the original per-address checks.
    Unmodeled hardware and the unused State memory gap remain outside the ABI.
    """
    ranges = ((0xc000, 0xe000), (0xff80, 0xffff))
    if banked_wram:
        ranges += ((0, 0x6000), (0xff70, 0xff71))
    left, right = bytes(reference.m), bytes(native.m)
    if all(left[start:end] == right[start:end] for start, end in ranges):
        return []
    return [address for start, end in ranges for address in range(start, end)
            if left[address] != right[address]]


class Oracle:
    def __init__(self, root, machine):
        self.root, self.rows = root, []
        build = root / 'build/battle-controller'
        source = pathlib.Path(__file__).with_name('battle_controller_oracle.c')
        library = build / ('controller-oracle-' + hashlib.sha256(source.read_bytes()).hexdigest()[:16] + '.dylib')
        prefix = root / 'build/stateful/mgba'
        if not library.exists():
            subprocess.run(['cc', '-O2', '-dynamiclib', '-I' + str(prefix / 'include'),
                str(source), '-L' + str(prefix / 'lib'), '-lmgba',
                '-Wl,-rpath,' + str(prefix / 'lib'), '-o', str(library)], check=True)
        self.oracle = C.CDLL(str(library))
        identity = lambda path: dict(path=str(path.resolve()),
            sha256=hashlib.sha256(path.read_bytes()).hexdigest())
        self.provenance = dict(schema=1, kind='instrumented-same-hooks-mgba',
            source=identity(source), library=identity(pathlib.Path(self.oracle._name)),
            abi_sources={name: identity(pathlib.Path(sys.modules[cls.__module__].__file__))
                         for name, cls in (('State', State), ('Hook', Hook))})
        # Resolve the actual loaded mGBA dependency through a symbol the oracle
        # calls, rather than assuming the configured rpath selected that file.
        class DlInfo(C.Structure):
            _fields_ = [('name', C.c_char_p), ('base', C.c_void_p),
                        ('symbol', C.c_char_p), ('address', C.c_void_p)]
        try:
            dladdr = C.CDLL(None).dladdr
            dladdr.argtypes, dladdr.restype = [C.c_void_p, C.POINTER(DlInfo)], C.c_int
            info = DlInfo()
            if not dladdr(C.cast(self.oracle.GBCoreCreate, C.c_void_p), C.byref(info)) or not info.name:
                raise ValueError('Cannot resolve loaded GBCoreCreate dependency')
            dependency = pathlib.Path(info.name.decode()).resolve()
            if dependency.parent != (prefix / 'lib').resolve() or not dependency.name.startswith('libmgba'):
                raise ValueError('Loaded mGBA dependency is outside configured prefix')
            self.provenance['libmgba'] = dict(**identity(dependency),
                identity_kind='loaded-GBCoreCreate-symbol-dladdr')
        except (AttributeError, OSError, ValueError) as error:
            self.provenance['libmgba_identity_error'] = str(error)
        self.banked_wram = getattr(machine, 'banked_wram', False)
        self.run = self.oracle.oracle_run_banked if self.banked_wram else self.oracle.oracle_run
        self.oracle.oracle_open.argtypes = [C.c_char_p]
        self.run.argtypes = [C.POINTER(State), C.c_uint32,
            C.c_uint16, C.c_uint16, C.c_uint16, C.c_void_p, C.c_uint32]
        assert self.oracle.oracle_open(str(machine.rom_path).encode()) == 0

    def compare(self, machine, stop, stop_bank=None):
        initial = bytes(machine.state)
        reference = State.from_buffer_copy(initial)
        if stop_bank is None:
            stop_bank = machine.state.bank
        hooks = (Hook * len(profile.BOUNDARIES))(*[Hook(*machine.symbols[name]) for name in profile.BOUNDARIES])
        self.run(C.byref(reference), 200000, stop,
                               stop_bank, machine.address('hROMBank'), hooks, len(hooks))
        assert not reference.fault, ('oracle', reference.fault, reference.bank, reference.pc)
        tape = tuple(reference.events[:reference.event_count])
        event, used = machine.raw_segment(tape, stop, stop_bank=stop_bank)
        assert used == tape
        for name in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'):
            assert getattr(reference, name) == getattr(machine.state, name), (event, name)
        differences = memory_differences(reference, machine.state, self.banked_wram)
        assert not differences, (event, differences[:16])
        machine.rng.consume(len(used))
        self.rows.append(dict(event=event.name if event else 'return', div_events=len(used)))
        return event, used

    def single_step(self, machine):
        reference = State.from_buffer_copy(bytes(machine.state))
        self.run(C.byref(reference), 1, 65534, machine.state.bank,
                               machine.address('hROMBank'), None, 0)
        assert reference.fault == 3
        machine._execute_instruction(tuple(reference.events[:reference.event_count]))
        assert machine.state.event_pos == reference.event_count
        for name in ('a', 'f', 'b', 'c', 'd', 'e', 'h', 'l', 'sp', 'pc', 'bank'):
            assert getattr(reference, name) == getattr(machine.state, name), ('one-step', name)
        assert not memory_differences(reference, machine.state, self.banked_wram)
        self.rows.append(dict(event='single-instruction-budget', div_events=reference.event_count))


def first_reserve(machine, side):
    active = machine.byte(('wCurBattleMon', 'wCurOTMon')[side])
    count = machine.byte(('wPartyCount', 'wOTPartyCount')[side])
    party = ('wPartyMon', 'wOTPartyMon')[side]
    for index in range(count):
        if index != active and machine.word(party + str(index + 1) + 'HP'):
            return index
    raise UnsupportedFight('Replacement callback has no living reserve')


def simple_choice(machine, name):
    if name == 'GetSwitchScores':
        machine._put('wEnemySwitchMonParam', first_reserve(machine, 1))
    elif name in ('_DoMenuJoypadLoop', 'MenuJoypadLoop'):
        state = machine.state
        owner = machine.menu_input_context()
        if owner == 'PartyMenuSelect':
            machine._put('wMenuCursorY', first_reserve(machine, 0) + 1)
        elif owner == 'MoveSelectionScreen':
            machine._put('wMenuCursorY', 1)
        else:
            raise UnsupportedFight('Unclassified menu input context: ' + owner)
        for field in ('hJoyPressed', 'hJoyLast', 'hJoyDown'):
            machine._put(field, 1)
    elif name == '_2DMenu':
        machine._put('wMenuCursorBuffer', 1)
        machine.state.a, machine.state.f = 0, 128
    elif name == 'YesNoBox':
        s = machine.state
        target = s.m[s.sp] | s.m[s.sp + 1] << 8
        if machine.address('ForgetMove') < target < machine.address('ForgetMove.cancel'):
            s.f |= 16  # decline replacing an existing move
        elif machine.address('LearnMove.cancel') < target < machine.address('LearnMove.learned'):
            s.f &= ~16  # confirm stopping this learning attempt
        else:
            raise UnsupportedFight('Unclassified YesNoBox context')
    else:
        raise UnsupportedFight('Unimplemented policy boundary ' + name)


def main(root):
    game = root / 'build/stateful/game'
    machine = ControllerMachine(game / 'polishedcrystal-3.2.3.gbc',
        game / 'polishedcrystal-3.2.3.sym', game, root / 'build/battle-controller/native')
    oracle = Oracle(root, machine)
    machine.segment_checker = oracle.compare
    machine.single_step_checker = oracle.single_step
    initialize(machine)
    machine.on_choice = simple_choice
    fixed = {}
    for side, party in enumerate(('wPartyMon', 'wOTPartyMon')):
        for index in range(machine.byte(('wPartyCount', 'wOTPartyCount')[side])):
            stem = party + str(index + 1)
            fixed[stem] = dict(level=machine.byte(stem + 'Level'),
                stats=tuple(machine.word(stem + name) for name in
                            ('MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')),
                exp=bytes(machine.state.m[machine.address(stem + 'Exp'):machine.address(stem + 'Exp') + 3]))
    # First execute actual send-in callers. The complete DoBattle/BattleTurn
    # route is the next acceptance step, not silently claimed by this probe.
    for side in (0, 1):
        machine._put('hBattleTurn', side)
        machine._put(('wPlayerSwitchTarget', 'wEnemySwitchTarget')[side], 1)
        for entry in ('GetUserSwitchTarget', 'SendInUserPkmn', 'SpikesDamage', 'RunEntryAbilities'):
            machine.call(entry)
    machine.call('ResetParticipants')
    machine.state.m[machine.address('wPartyParticipants')] = 1
    for side in (0, 1):
        machine._put('hBattleTurn', side)
        machine._put(('wCurPlayerMove', 'wCurEnemyMove')[side], machine._constant('constants/move_constants.asm', 'TACKLE'))
        machine._put(('wCurMoveNum', 'wCurEnemyMoveNum')[side], 0)
        machine.call('UpdateMoveData')
    from battle_decisions_check import check_machine
    decisions_contract = check_machine(machine)
    before_exp = int.from_bytes(bytes(machine.state.m[machine.address('wPartyMon1Exp'):machine.address('wPartyMon1Exp') + 3]), 'big')
    turns = 0
    hud_snapshot = False
    while not machine.terminal and turns < 32:
        for side in (0, 1):
            machine._put('hBattleTurn', side)
            machine._put(('wCurPlayerMove', 'wCurEnemyMove')[side], machine._constant('constants/move_constants.asm', 'TACKLE'))
            machine._put(('wCurMoveNum', 'wCurEnemyMoveNum')[side], 0)
            machine.call('UpdateMoveData')
        machine.call('DetermineMoveOrder')
        first = 0 if machine.state.f & 16 else 1
        machine._put('wEnemyGoesFirst', first)
        for side in (first, first ^ 1):
            machine._put('hBattleTurn', side)
            if turns == 0 and side == first:
                snapshot_dir = root / 'build/battle-controller/snapshots'
                snapshot_dir.mkdir(exist_ok=True)
                machine.prepare_call('BattleTurn.do_move')
                raw = bytes(machine.state)
                (snapshot_dir / 'initial-do-move.state').write_bytes(raw)
                (snapshot_dir / 'initial-do-move.json').write_text(json.dumps(dict(
                    entry='BattleTurn.do_move', prepared=True, bank=machine.state.bank,
                    state_sha256=hashlib.sha256(raw).hexdigest(),
                    rom=str(machine.rom_path), symbols=str(machine.symbols_path),
                    rom_sha256=hashlib.sha256(machine.rom_path.read_bytes()).hexdigest(),
                    symbols_sha256=hashlib.sha256(machine.symbols_path.read_bytes()).hexdigest(),
                    manifest=str(machine.build_dir / 'manifest.json'),
                    manifest_sha256=hashlib.sha256((machine.build_dir / 'manifest.json').read_bytes()).hexdigest(),
                    generator_sha256=stateful_lifting.MODULE_DIGEST,
                    fixture_sha256=hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest(),
                    options=dict(initial=machine.byte('wInitialOptions'), initial2=machine.byte('wInitialOptions2'))), indent=2) + '\n')
                begin = len(oracle.rows)
                machine.resume()
                (snapshot_dir / 'initial-do-move-boundaries.json').write_text(json.dumps(oracle.rows[begin:], indent=2) + '\n')
            else:
                machine.call('BattleTurn.do_move')
            if machine.terminal:
                break
        if not machine.terminal:
            if not hud_snapshot and not machine.word('wEnemyMonHP'):
                bank, pc = machine.symbols['EnemySwitch_TrainerHud']
                machine.prepare_call('HandleBetweenTurnEffects')
                machine.resume(stop=pc, stop_bank=bank)
                if not machine.terminal:
                    assert machine.state.pc == pc and machine.state.bank == bank
                    directory = root / 'build/battle-controller/snapshots'
                    raw = bytes(machine.state)
                    (directory / 'enemy-hud-entry.state').write_bytes(raw)
                    metadata = dict(entry='EnemySwitch_TrainerHud', source_caller='HandleBetweenTurnEffects',
                        next_stop=65534, banked_wram=True, hCGB=machine.byte('hCGB'),
                        state_sha256=hashlib.sha256(raw).hexdigest(),
                        rom=str(machine.rom_path), symbols=str(machine.symbols_path),
                        rom_sha256=hashlib.sha256(machine.rom_path.read_bytes()).hexdigest(),
                        symbols_sha256=hashlib.sha256(machine.symbols_path.read_bytes()).hexdigest(),
                        manifest=str(machine.build_dir / 'manifest.json'),
                        manifest_sha256=hashlib.sha256((machine.build_dir / 'manifest.json').read_bytes()).hexdigest(),
                        generator_sha256=stateful_lifting.MODULE_DIGEST)
                    (directory / 'enemy-hud-entry.json').write_text(json.dumps(metadata, indent=2) + '\n')
                    begin = len(oracle.rows)
                    machine.resume()
                    (directory / 'enemy-hud-continuation.state').write_bytes(bytes(machine.state))
                    (directory / 'enemy-hud-boundaries.json').write_text(json.dumps(oracle.rows[begin:], indent=2) + '\n')
                    hud_snapshot = True
            else:
                machine.call('HandleBetweenTurnEffects')
        turns += 1
    assert machine.terminal
    for stem, expected in fixed.items():
        assert machine.byte(stem + 'Level') == expected['level'], stem
        assert tuple(machine.word(stem + name) for name in
                     ('MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')) == expected['stats'], stem
        assert bytes(machine.state.m[machine.address(stem + 'Exp'):machine.address(stem + 'Exp') + 3]) == expected['exp'], stem
    after_exp = int.from_bytes(bytes(machine.state.m[machine.address('wPartyMon1Exp'):machine.address('wPartyMon1Exp') + 3]), 'big')
    report = dict(scope='source-do-move-and-full-endturn-host-scheduled-normal-fight', turns=turns, winner=machine.winner, fixed_levels_stats_and_exp=True, decisions_contract=decisions_contract, exp_before=before_exp, exp_after=after_exp, comparisons=oracle.rows,
        hp=[machine.word(name) for name in ('wBattleMonHP', 'wEnemyMonHP')],
        options=dict(initial=machine.byte('wInitialOptions'), initial2=machine.byte('wInitialOptions2')),
        generator_sha256=hashlib.sha256(pathlib.Path(__file__).with_name('stateful_lifting.py').read_bytes()).hexdigest())
    (root / 'build/battle-controller/first-fight.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(dict(comparisons=len(oracle.rows), hp=report['hp'])))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    main(parser.parse_args().artifact_root)
