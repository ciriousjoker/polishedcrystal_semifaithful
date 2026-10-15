"""Restricted 1v1 native actual-caller experiment with explicit frontend profile.

Headless presentation identity returns are NOT full ordinary-ROM equivalence.
Only explicitly synthetic learner-plumbing experiments may use this profile.
Ordinary-game fidelity training remains blocked on frontend/RNG validation.
"""
import ctypes as C
import json
import pathlib
import random
import re
from dataclasses import dataclass

import stateful_lifting
from stateful_fixture_check import State
import battle_env_profile as profile


class UnsupportedFight(ValueError):
    pass


@dataclass(frozen=True)
class MoveAction:
    slot: int


@dataclass(frozen=True)
class Transition:
    actor: int
    slot: int
    move: int
    damage: int
    target_hp: int
    pp: int
    missed: bool
    div_events: tuple
    presentation: tuple


class EventStream:
    """Private exogenous DIV tape; neither committed actions nor tape are observed.

    Uniform bytes are an explicit experimental event law, not a claim to model
    the hardware-time distribution of DIV reads in a running game frontend.
    """
    def __init__(self, seed):
        self.rng = random.Random(seed)

    def preview(self, count=4096):
        copy = random.Random()
        copy.setstate(self.rng.getstate())
        return bytes(copy.getrandbits(8) for _ in range(count))

    def consume(self, count):
        for _ in range(count):
            self.rng.getrandbits(8)


class NativeFight:
    """Construct a closed Smeargle/Own Tempo two-move fixture, then run ROM code.

    No arbitrary RAM state is accepted. Only orientation and private RNG seed vary.
    Actual DoTurn and DetermineMoveOrder execute natively, including every normal
    effect command. One-mon rosters terminate on faint; no replacement exists.
    No items/status/weather/hazards/delayed effects can enter the constructor.
    Exhausted PP explicitly truncates, never substitutes an approximate Struggle.
    """
    def __init__(self, rom, symbols, source_root, build_dir, seed=1,
                 orientation=0, max_turns=64):
        if orientation not in (0, 1) or not 1 <= max_turns <= 64:
            raise UnsupportedFight('Unsupported fixture orientation or turn limit')
        self.rom_path = pathlib.Path(rom)
        self.rom = self.rom_path.read_bytes()
        self.symbols_path = pathlib.Path(symbols)
        self.symbols = stateful_lifting.symbols(symbols)
        self.source_root = pathlib.Path(source_root)
        self.build_dir = pathlib.Path(build_dir)
        manifest = json.loads((self.build_dir / 'manifest.json').read_text())
        self.excluded = tuple(manifest.get('excluded', {}))
        self.lib = stateful_lifting.load_verified(
            build_dir, rom, symbols, source_root, profile.ENTRIES,
            profile.PRESENTATION, self.excluded)
        self.lib.stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]
        profile.verify_audit(rom, symbols, source_root)
        self.state = State()
        self.state.sp = 0xdff0
        self.state.bank = self.symbols['DoTurn'][0]
        self.state.m[self.address('hROMBank')] = self.state.bank
        self.rng = EventStream(seed)
        self.turn = 0
        self.max_turns = max_turns
        self.events = []
        self.terminal = False
        self.truncated = False
        self.truncation_reason = None
        self.winner = None
        self._constant_cache = {}
        self.moves = tuple(self._constant('constants/move_constants.asm', n)
                           for n in ('TACKLE', 'WATER_GUN'))
        self._initialize_fixture(seed, orientation)
        self.orientation = orientation
        self.failed = False
        for move in self.moves:
            data = self.move_data(move)
            if data[1] != 0 or data[4] != 100 or data[6] != 0:
                raise UnsupportedFight('Fixture move changed outside audited effect scope')
        if self.program(0) != profile.NORMAL_COMMANDS:
            raise UnsupportedFight('Normal-hit script changed; reaudit its command contract')

    def reset(self, seed=1, orientation=None):
        """Reuse this verified engine under its frozen source/ROM identity.

        Callers must create a new verified engine after changing any inputs.
        Reset does no compiler invocation, source hashing, or dynamic loading.
        """
        orientation = self.orientation if orientation is None else orientation
        if orientation not in (0, 1):
            raise UnsupportedFight('Invalid fixture orientation')
        self.orientation = orientation
        self.state = State()
        self.state.sp = 0xdff0
        self.state.bank = self.symbols['DoTurn'][0]
        self._put('hROMBank', self.state.bank)
        self.rng = EventStream(seed)
        self.turn = 0
        self.events = []
        self.terminal = self.truncated = self.failed = False
        self.truncation_reason = self.winner = None
        self._initialize_fixture(seed, orientation)
        return self.observation(0)

    def address(self, name):
        return self.symbols[name][1]

    def byte(self, name):
        return self.state.m[self.address(name)]

    def word(self, name):
        a = self.address(name)
        return self.state.m[a] * 256 + self.state.m[a + 1]

    def _put(self, name, value, width=1):
        a = self.address(name)
        self.state.m[a:a + width] = value.to_bytes(width, 'big')

    def _constant(self, path, name):
        # Only the initial simple const block is consumed, stopping at the named
        # fixture constant. New syntax fails instead of guessing a game profile.
        key = (path, name)
        if key in self._constant_cache:
            return self._constant_cache[key]
        counter = 0
        for raw in (self.source_root / path).read_text().splitlines():
            line = raw.split(';', 1)[0].strip()
            if not line:
                continue
            if line.startswith('const_def'):
                value = line[len('const_def'):].strip()
                counter = int(value or '0', 0)
            elif line.startswith('const '):
                if line.split()[1] == name:
                    self._constant_cache[key] = counter
                    return counter
                counter += 1
            elif line.startswith(('if ', 'else', 'endc', 'const_skip', 'const_next')):
                raise UnsupportedFight('Unsupported fixture constant expression')
        raise UnsupportedFight(f'Missing fixture constant {name}')

    def _initialize_fixture(self, seed, orientation):
        species = self._constant('constants/pokemon_constants.asm', 'SMEARGLE')
        ability = self._constant('constants/ability_constants.asm', 'OWN_TEMPO')
        # Plausible level-50 combat stats, deliberately asymmetric physical/special
        # preferences. Their EV/DV derivation is not part of this battle-start API.
        units = [(145, 75, 55, 140, 35, 80), (150, 25, 65, 125, 75, 55)]
        if orientation:
            units.reverse()
        self._put('wBattleMode', 2)  # ordinary trainer, not wild
        self._put('wPartyCount', 1)
        self._put('wOTPartyCount', 1)
        for side, values in enumerate(units):
            stem = ('wBattleMon', 'wEnemyMon')[side]
            party = ('wPartyMon1', 'wOTPartyMon1')[side]
            team = ('wPlayer', 'wEnemy')[side]
            for target in (stem, party):
                self._put(target + 'Species', species)
                self._put(target + 'Level', 50)
                for name, value in zip(('HP', 'MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef'),
                                       (values[0], *values)):
                    self._put(target + name, value, 2)
                for i, move in enumerate(self.moves):
                    self.state.m[self.address(target + 'Moves') + i] = move
                    self.state.m[self.address(target + 'PP') + i] = self.move_data(move)[5]
            self._put(stem + 'Type1', 0)
            self._put(stem + 'Type2', 0)
            self._put(team + 'Ability', ability)
            for stat in ('Atk', 'Def', 'SAtk', 'SDef', 'Spe', 'Acc', 'Eva'):
                self._put(team + stat + 'Level', 7)
        # Matching zero OT IDs guarantee ordinary obedience. Affection option,
        # all field effects, action/controller counters and held items start zero.
        for i in range(7):
            self.state.m[self.address('wRNGState') + i] = (seed + i * 37) & 255

    def _rom_bytes(self, name, offset, count):
        bank, pc = self.symbols[name]
        start = pc if pc < 0x4000 else bank * 0x4000 + pc - 0x4000
        return self.rom[start + offset:start + offset + count]

    def move_data(self, move):
        if not 1 <= move <= 254:
            raise UnsupportedFight('Unsupported move or Struggle')
        data = self._rom_bytes('Moves', (move - 1) * 8, 8)
        if len(data) != 8 or data[0] != move:
            raise UnsupportedFight('Move table layout mismatch')
        return data

    def program(self, effect):
        pc = int.from_bytes(self._rom_bytes('MoveEffectsPointers', effect * 2, 2), 'little')
        bank = self.symbols['MoveEffectsPointers'][0]
        reverse = {v: k.removeprefix('BattleCommand_')
                   for k, v in self.symbols.items()
                   if k.startswith('BattleCommand_') and '.' not in k}
        commands = []
        for i in range(128):
            code = self.rom[bank * 0x4000 + pc - 0x4000 + i]
            if code == 255:
                return tuple(commands)
            pointer = self._rom_bytes('BattleCommandPointers', (code - 1) * 2, 2)
            name = reverse.get((self.symbols['DoTurn'][0], int.from_bytes(pointer, 'little')))
            if name not in profile.NORMAL_COMMANDS:
                raise UnsupportedFight(f'Unvalidated effect command {name!r}')
            commands.append(name)
        raise UnsupportedFight('Effect program did not terminate')

    def legal_actions(self, side):
        if side not in (0, 1):
            raise UnsupportedFight('Invalid actor')
        if self.terminal or self.truncated:
            return ()
        stem = ('wBattleMon', 'wEnemyMon')[side]
        return tuple(MoveAction(i) for i in range(2)
                     if self.state.m[self.address(stem + 'PP') + i] & 63)

    def observation(self, side):
        # Schema remains full battle inputs rather than freezing an actor feature
        # selection. It intentionally excludes RNG, action RAM and UI scratch.
        teams = []
        for i, stem in enumerate(('wBattleMon', 'wEnemyMon')):
            mon = dict(species=self.byte(stem + 'Species'), level=self.byte(stem + 'Level'),
                       types=(self.byte(stem + 'Type1'), self.byte(stem + 'Type2')),
                       ability=self.byte(('wPlayerAbility', 'wEnemyAbility')[i]),
                       status=self.byte(stem + 'Status'), item=self.byte(stem + 'Item'),
                       moves=self.moves,
                       pp=tuple(self.state.m[self.address(stem + 'PP') + j] & 63 for j in range(2)))
            mon.update({name:self.word(stem + name) for name in
                        ('HP', 'MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')})
            teams.append((mon,))
        return dict(side=side, turn=self.turn, teams=tuple(teams),
                    terminal=self.terminal, truncated=self.truncated)

    def prepare_call(self, entry):
        s = self.state
        s.pc = self.address(entry)
        s.bank = self.symbols[entry][0] or s.bank
        self._put('hROMBank', s.bank)
        s.sp -= 2
        s.m[s.sp], s.m[s.sp + 1] = 254, 255

    def run_prepared(self, tape):
        """Execute a prepared call and return consumed tape + explicit UI events."""
        s = self.state
        if len(tape) > 4096:
            raise UnsupportedFight('RNG tape exceeds bound')
        s.event_count, s.event_pos = len(tape), 0
        s.events[:len(tape)] = tape
        boundaries = {(pc if pc < 0x4000 else bank << 16 | pc): name
                      for name in profile.PRESENTATION
                      for bank, pc in [self.symbols[name]]}
        events = []
        for _ in range(512):
            self.lib.stateful_run(C.byref(s), 200000, 65534)
            if s.fault == 0:
                return tuple(tape[:s.event_pos]), tuple(events)
            key = s.pc if s.pc < 0x4000 else s.bank << 16 | s.pc
            if s.fault != 5 or key not in boundaries:
                self.failed = True
                raise UnsupportedFight(f'Native fault {s.fault} at {s.bank:02x}:{s.pc:04x}')
            if not 0xc000 <= s.sp < 0xdffe:
                raise UnsupportedFight('Invalid presentation return stack')
            target = s.m[s.sp] | s.m[s.sp + 1] << 8
            if target >= 0x8000 and target != 65534:
                raise UnsupportedFight('Presentation return outside supported code')
            events.append(boundaries[key])
            s.sp += 2
            s.pc = target
            s.fault = 0
        raise UnsupportedFight('Too many presentation events')

    def _call(self, entry):
        self.prepare_call(entry)
        tape = self.rng.preview()
        used, frontend = self.run_prepared(tape)
        self.rng.consume(len(used))
        return used, frontend

    def commit_actions(self, actions):
        if len(actions) != 2 or self.terminal or self.truncated or self.failed:
            raise UnsupportedFight('Invalid turn boundary')
        if any(action not in self.legal_actions(side) for side, action in enumerate(actions)):
            raise UnsupportedFight('Illegal action')
        for side, action in enumerate(actions):
            move = self.moves[action.slot]
            self._put(('wCurPlayerMove', 'wCurEnemyMove')[side], move)
            self._put(('wCurMoveNum', 'wCurEnemyMoveNum')[side], action.slot)
            p = self.address(('wPlayerMoveStruct', 'wEnemyMoveStruct')[side])
            self.state.m[p:p + 8] = self.move_data(move)

    def step(self, actions):
        """Whole-turn candidate; actual ROM order and move callers, no emulator."""
        # Validation precedes mutation, so an illegal action leaves this instance
        # reusable. Any failure after committing poisons all subsequent steps.
        if len(actions) != 2 or self.terminal or self.truncated or self.failed:
            raise UnsupportedFight('Invalid turn boundary')
        if any(action not in self.legal_actions(side) for side, action in enumerate(actions)):
            raise UnsupportedFight('Illegal action')
        try:
            self.commit_actions(actions)
            self._call('DetermineMoveOrder')
            first = 0 if self.state.f & 16 else 1
            events = []
            for side in (first, first ^ 1):
                stem = ('wBattleMon', 'wEnemyMon')[side]
                opponent = ('wEnemyMon', 'wBattleMon')[side]
                before = self.word(opponent + 'HP')
                self._put('hBattleTurn', side)
                # PerformMove's neutral entry clears per-attack damage accumulation.
                self._put('wDamageTaken', 0, 2)
                used, frontend = self._call('DoTurn')
                hp = self.word(opponent + 'HP')
                action = actions[side]
                event = Transition(side, action.slot, self.moves[action.slot], before - hp,
                                   hp, self.state.m[self.address(stem + 'PP') + action.slot] & 63,
                                   bool(self.byte('wAttackMissed')), used, frontend)
                events.append(event)
                if hp == 0:
                    self.terminal, self.winner = True, side
                    break
            self.turn += 1
            if not self.terminal:
                if any(not self.legal_actions(side) for side in (0, 1)):
                    self.truncated, self.truncation_reason = True, 'unsupported-struggle'
                elif self.turn >= self.max_turns:
                    self.truncated, self.truncation_reason = True, 'turn-limit'
            self.events.extend(events)
            return tuple(events)
        except Exception:
            self.failed = True
            raise
