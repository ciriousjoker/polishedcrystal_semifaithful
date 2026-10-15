"""Resumable actual-source trainer controller with explicit decision boundaries.

This implementation is under active validation. Unknown instructions, memory,
selection contexts and event exhaustion reject rather than substitute rules.
"""
import ctypes as C
import json
import pathlib
from dataclasses import dataclass
from battle_env import NativeFight, EventStream, UnsupportedFight
from stateful_fixture_check import State
import stateful_lifting
import battle_controller_profile as profile


@dataclass(frozen=True)
class Boundary:
    name: str
    bank: int
    pc: int


class ControllerMachine(NativeFight):
    def __init__(self, rom, symbols, source_root, build_dir, seed=193, *, optimization='O0', audit=None):
        self.rom_path, self.symbols_path = pathlib.Path(rom), pathlib.Path(symbols)
        self.source_root, self.build_dir = pathlib.Path(source_root), pathlib.Path(build_dir)
        self.audit_configuration = profile.verify_audit(rom, symbols, source_root, audit)
        self.rom = self.rom_path.read_bytes()
        self.symbols = stateful_lifting.symbols(symbols)
        self.entries = profile.config(self.symbols, source_root)
        self.manifest = json.loads((self.build_dir / 'manifest.json').read_text())
        if self.manifest.get('controller_audit') != self.audit_configuration:
            raise ValueError('Controller audit configuration mismatch; regenerate profile')
        description = json.loads((self.build_dir / 'controller-profile.json').read_text())
        if description.get('audit') != self.audit_configuration:
            raise ValueError('Controller profile audit configuration mismatch')
        self.lib = stateful_lifting.load_verified(build_dir, rom, symbols, source_root,
            self.entries, profile.BOUNDARIES, tuple(self.manifest.get('excluded', {})),
            write_only_mmio=profile.WRITE_ONLY_MMIO, call_abis=profile.CALL_ABIS,
            optimization=optimization, banked_wram=profile.BANKED_WRAM, lcd_disabled=profile.LCD_DISABLED)
        self.banked_wram = profile.BANKED_WRAM
        self.lib.stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]
        self.lib.stateful_run_banked.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16, C.c_uint16]
        self._constant_cache = {}
        self.boundaries = {(pc if pc < 0x4000 else bank << 16 | pc): Boundary(name, bank, pc)
                           for name in profile.BOUNDARIES for bank, pc in [self.symbols[name]]}
        self.reset_empty(seed)
        self.segment_checker = None
        self.single_step_checker = None
        self.on_choice = None

    def reset_empty(self, seed):
        self.state = State()
        self.state.sp = self.address('wStackTop')
        self.state.bank = self.symbols['PerformMove'][0]
        self._put('hROMBank', self.state.bank)
        self.state.m[0xff70] = 1
        self._put('hCGB', 1)
        self.rng = EventStream(seed)
        self.turn, self.max_turns = 0, 128
        self.events = []
        self.failed = self.terminal = self.truncated = False
        self.winner = self.truncation_reason = None
        for index in range(7):
            self.state.m[self.address('wRNGState') + index] = (seed + index * 37) & 255

    def _constant(self, path, name):
        """Default-build source const blocks, with explicit skips and indices."""
        key = (path, name)
        if key in self._constant_cache:
            return self._constant_cache[key]
        value = 0
        def integer(text):
            return int(text[1:], 16) if text.startswith('$') else int(text, 0)
        from battle_coverage import lines
        for _, code in lines(self.source_root, path):
            if code.startswith('const_def'):
                argument = code[len('const_def'):].strip()
                value = integer(argument or '0')
            elif code.startswith('const_next '):
                value = integer(code.split()[1])
            elif code.startswith('const_skip'):
                argument = code[len('const_skip'):].strip()
                value += integer(argument or '1')
            elif code.startswith('const '):
                if code.split()[1] == name:
                    self._constant_cache[key] = value
                    return value
                value += 1
        raise UnsupportedFight('Unknown source constant ' + name)

    def raw_segment(self, tape, stop=65534, budget=200000, stop_bank=None):
        s = self.state
        if type(stop) is not int or not 0 <= stop <= 65535:
            raise UnsupportedFight('Invalid stop address')
        if len(tape) > 4096:
            raise UnsupportedFight('Controller event tape exceeds ABI bound')
        s.event_count, s.event_pos = len(tape), 0
        s.events[:len(tape)] = tape
        if stop_bank is None:
            stop_bank = s.bank
        if type(stop_bank) is not int or not 0 <= stop_bank < len(self.rom) // 0x4000:
            raise UnsupportedFight('Invalid stop ROM bank')
        if 0x4000 <= stop < 0x8000 and stop_bank == 0:
            raise UnsupportedFight('Switchable stop requires a nonzero MBC3 bank')
        self.lib.stateful_run_banked(C.byref(s), budget, stop, stop_bank)
        used = tuple(tape[:s.event_pos])
        if s.fault == 0:
            return None, used
        key = s.pc if s.pc < 0x4000 else s.bank << 16 | s.pc
        if s.fault == 5 and key in self.boundaries:
            return self.boundaries[key], used
        raise UnsupportedFight(f'Controller fault {s.fault} at {s.bank:02x}:{s.pc:04x}')

    def segment(self, stop=65534, stop_bank=None):
        if self.segment_checker is not None:
            return self.segment_checker(self, stop, stop_bank)
        boundary, used = self.raw_segment(self.rng.preview(), stop, stop_bank=stop_bank)
        self.rng.consume(len(used))
        return boundary, used

    def single_step(self):
        """Execute one source instruction to leave a stopped loop entry."""
        if self.single_step_checker is not None:
            return self.single_step_checker(self)
        return self._execute_instruction(self.rng.preview())

    def _execute_instruction(self, tape):
        s = self.state
        if len(tape) > 4096:
            raise UnsupportedFight('Instruction event tape exceeds ABI bound')
        if s.fault:
            raise UnsupportedFight('Cannot step a suspended controller')
        before = s.steps
        s.event_count, s.event_pos = len(tape), 0
        s.events[:len(tape)] = tape
        self.lib.stateful_run_banked(C.byref(s), 1, 65534, s.bank)
        if s.fault != 3 or s.steps != before + 1:
            raise UnsupportedFight('Single instruction did not reach its budget boundary')
        s.fault = 0
        self.rng.consume(s.event_pos)

    def return_from_boundary(self):
        s = self.state
        if not 0xc000 <= s.sp < 0xdffe:
            raise UnsupportedFight('Invalid controller callback stack')
        target = s.m[s.sp] | s.m[s.sp + 1] << 8
        if target >= 0x8000 and target != 65534:
            raise UnsupportedFight('Invalid controller callback return')
        s.sp += 2
        s.pc, s.fault = target, 0

    def menu_input_context(self):
        """Decode the actual direct-call or RST farcall input-leaf frame."""
        s = self.state
        offset = 0
        if (s.pc == self.address('MenuJoypadLoop') and
                s.bank == self.symbols['MenuJoypadLoop'][0]):
            # Preserve the real wrapper's RES bit, PUSH/POP AF and BG-map mode
            # restoration. Its frame contains the loop return and saved AF.
            target = s.m[s.sp] | s.m[s.sp + 1] << 8
            if not self.address('_DoMenuJoypadLoop') < target < self.address('MenuJoypadLoop'):
                raise UnsupportedFight('Unknown menu-loop wrapper frame')
            offset = 4
        def word(offset):
            return s.m[s.sp + offset] | s.m[s.sp + offset + 1] << 8
        target, bank = word(offset), s.bank
        # FarCall pushes a return bank and its inline continuation. The home
        # input wrapper falls through to GetMenuJoypad after that continuation.
        if target == self.address('FarCall') + 4:
            bank, continuation = s.m[s.sp + offset + 2], word(offset + 3)
            if continuation != self.address('GetMenuJoypad'):
                raise UnsupportedFight('Unknown farcall menu continuation')
            target = word(offset + 5)
        owners = [(pc, label) for label, (mapped, pc) in self.symbols.items()
                  if '.' not in label and pc < target and
                  (pc < 0x4000 or mapped == bank)]
        if not owners:
            raise UnsupportedFight('Unknown menu input caller')
        return max(owners)[1]

    def service(self, event):
        self.events.append(event.name)
        if event.name in profile.TERMINALS:
            self.terminal, self.winner = True, (0 if event.name == 'WinTrainerBattle' else 1)
            return False
        if event.name in profile.CHOICES:
            if self.on_choice is None:
                raise UnsupportedFight('Controller requires choice at ' + event.name)
            self.on_choice(self, event.name)
            if event.name == 'MenuJoypadLoop':
                # The input callback chooses a cursor and button. Keep the
                # source cursor bookkeeping used by PartyMenuSelect and bags.
                self.call_from_boundary('_2DMenuInterpretJoypad')
                self.call_from_boundary('Move2DMenuCursor')
        elif event.name not in profile.PRESENTATION:
            raise UnsupportedFight('Unknown controller boundary')
        self.return_from_boundary()
        return True

    def call_from_boundary(self, entry):
        """Execute a declared source helper below a suspended callee frame."""
        s = self.state
        frame = s.pc, s.bank, s.sp, s.fault, self.byte('hROMBank')
        if s.fault != 5:
            raise UnsupportedFight('Nested boundary helper requires a suspension')
        s.fault = 0
        try:
            self.call(entry)
            if self.terminal or s.sp != frame[2]:
                raise UnsupportedFight('Unexpected nested boundary helper control flow')
        finally:
            s.pc, s.bank, s.sp, s.fault, rom_bank = frame
            self._put('hROMBank', rom_bank)

    def resume(self, stop=65534, stop_bank=None):
        for _ in range(4096):
            event, _ = self.segment(stop, stop_bank)
            if event is None:
                return None
            if not self.service(event):
                return event
        raise UnsupportedFight('Controller boundary count exceeded')

    def call(self, entry):
        if entry not in self.entries:
            raise UnsupportedFight('Entry is outside controller manifest: ' + entry)
        self.prepare_call(entry)
        return self.resume()
