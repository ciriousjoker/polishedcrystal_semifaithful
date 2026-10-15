"""Actual-source voluntary switching in a bounded two-member battle profile."""
import ctypes as C
import json
import pathlib
from dataclasses import dataclass
from battle_env import NativeFight, EventStream, MoveAction, UnsupportedFight, Transition
from battle_party import PartyFight
from stateful_fixture_check import State
import stateful_lifting
import battle_actions_profile as profile


@dataclass(frozen=True)
class SwitchAction:
    index: int


@dataclass(frozen=True)
class ItemAction:
    slot: int


ITEM_ENTRIES = {'POTION': 'EnemyUsedPotion', 'SUPER_POTION': 'EnemyUsedSuperPotion',
                'HYPER_POTION': 'EnemyUsedHyperPotion', 'MAX_POTION': 'EnemyUsedMaxPotion',
                'FULL_HEAL': 'EnemyUsedFullHeal', 'FULL_RESTORE': 'EnemyUsedFullRestore'}


class ActionEngine(NativeFight):
    """Independent verified profile; no baseline handles are borrowed for updates."""
    def __init__(self, rom, symbols, source_root, build_dir, seed=193,
                 orientation=0, max_turns=64):
        if orientation not in (0, 1) or not 1 <= max_turns <= 64:
            raise UnsupportedFight('Unsupported action fixture orientation or turn limit')
        self.rom_path, self.symbols_path = pathlib.Path(rom), pathlib.Path(symbols)
        self.source_root, self.build_dir = pathlib.Path(source_root), pathlib.Path(build_dir)
        profile.verify_audit(rom, symbols, source_root)
        self.rom, self.symbols = self.rom_path.read_bytes(), stateful_lifting.symbols(symbols)
        self._constant_cache = {}
        self.moves = tuple(self._constant('constants/move_constants.asm', n)
                           for n in ('TACKLE', 'WATER_GUN'))
        self.orientation, self.max_turns = orientation, max_turns
        self.leaf = ActionLeaf(self, build_dir)
        self.lib = self.leaf.lib
        self.reset(seed, orientation)
        import battle_env_profile as base
        for move in self.moves:
            data = self.move_data(move)
            if data[1] != 0 or data[4] != 100 or data[6] != 0:
                raise UnsupportedFight('Move changed outside normal-hit action scope')
        if self.program(0) != base.NORMAL_COMMANDS:
            raise UnsupportedFight('Normal-hit command program changed')

    def _call(self, entry):
        return self.leaf.call(entry)


class ActionTerminal:
    def __init__(self, leaf):
        self.leaf = leaf

    def call(self, entry):
        used, events = self.leaf.call(entry)
        terminals = [event for event in events if event in profile.TERMINAL_EVENTS]
        return terminals[0] if terminals else None


class ActionLeaf:
    def __init__(self, fight, build_dir):
        self.fight = fight
        manifest = json.loads((pathlib.Path(build_dir) / 'manifest.json').read_text())
        self.lib = stateful_lifting.load_verified(build_dir, fight.rom_path,
            fight.symbols_path, fight.source_root, profile.ENTRIES,
            profile.BOUNDARIES, tuple(manifest.get('excluded', {})))
        self.lib.stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]

    def run_prepared(self, tape):
        f, s = self.fight, self.fight.state
        if len(tape) > 4096:
            raise UnsupportedFight('Action RNG tape exceeds bound')
        s.event_count, s.event_pos = len(tape), 0
        s.events[:len(tape)] = tape
        hooks = {(pc if pc < 0x4000 else bank << 16 | pc): name
                 for name in profile.BOUNDARIES for bank, pc in [f.symbols[name]]}
        events = []
        for _ in range(512):
            self.lib.stateful_run(C.byref(s), 200000, 65534)
            if not s.fault:
                return tuple(tape[:s.event_pos]), tuple(events)
            key = s.pc if s.pc < 0x4000 else s.bank << 16 | s.pc
            if s.fault != 5 or key not in hooks:
                raise UnsupportedFight(f'Action fault {s.fault} at {s.bank:02x}:{s.pc:04x}')
            if hooks[key] in profile.TERMINAL_EVENTS:
                return tuple(tape[:s.event_pos]), (*events, hooks[key])
            if not 0xc000 <= s.sp < 0xdffe:
                raise UnsupportedFight('Invalid action presentation stack')
            target = s.m[s.sp] | s.m[s.sp + 1] << 8
            if target >= 0x8000 and target != 65534:
                raise UnsupportedFight('Invalid action presentation return')
            s.sp += 2
            s.pc, s.fault = target, 0
            events.append(hooks[key])
        raise UnsupportedFight('Too many action presentation events')

    def call(self, entry):
        if entry not in profile.ENTRIES:
            raise UnsupportedFight('Unsupported action entry')
        self.fight.prepare_call(entry)
        used, events = self.run_prepared(self.fight.rng.preview())
        self.fight.rng.consume(len(used))
        return used, events


class ActionFight(PartyFight):
    """Host choices followed by real DoTurn/DeferredSwitch/party mechanics.

    The rendering and IID-DIV conditions of PartyFight remain in force. Only the
    audited normal-hit moves are accepted. Switch trapping is queried through
    actual UserCanSwitch on an isolated CPU/RAM/RNG copy, leaving observations
    and legality queries side-effect free.
    """
    def __init__(self, rom, symbols, source_root, base_build=None, party_build=None,
                 terminal_build=None, seed=193, orientation=0, max_turns=64,
                 *, action_build, inventory=('SUPER_POTION', 'FULL_RESTORE')):
        # Legacy build arguments remain positional compatibility only; this
        # independent engine loads exactly the combined verified profile.
        if len(inventory) != 2 or any(name is not None and name not in ITEM_ENTRIES for name in inventory):
            raise UnsupportedFight('Trainer inventory must have two supported or empty slots')
        self.inventory_names = tuple(inventory)
        self.fight = ActionEngine(rom, symbols, source_root, action_build,
                                  seed, orientation, max_turns)
        self.action_leaf = self.leaf = self.fight.leaf
        self.terminal_leaf = ActionTerminal(self.leaf)
        classes = [line.split()[1] for line in
                   (self.fight.source_root / 'constants/trainer_constants.asm').read_text().splitlines()
                   if line.strip().startswith('trainerclass ')]
        self.trainer_class = classes.index('YOUNGSTER')
        self._initialize_party()

    def _initialize_party(self):
        super()._initialize_party()
        self.fight._put('wEnemyItemState', 255)
        for slot, name in enumerate(self.inventory_names):
            self.fight._put('wEnemyTrainerItem' + str(slot + 1),
                            self.fight._constant('constants/item_constants.asm', name) if name else 0)

    def observation(self, side):
        result = super().observation(side)
        result['trainer_items'] = ((), tuple(self.fight.byte('wEnemyTrainerItem' + str(i))
                                            for i in (1, 2)))
        return result

    def legal_actions(self, side):
        moves = super().legal_actions(side)
        f = self.fight
        if f.terminal or f.truncated or f.failed:
            return ()
        if type(side) is not int or side not in (0, 1):
            raise UnsupportedFight('Invalid actor')
        items = ()
        if side == 1 and f.word('wEnemyMonHP'):
            items = tuple(ItemAction(slot) for slot in range(2)
                          if f.byte('wEnemyTrainerItem' + str(slot + 1)))
        moves = (*moves, *items)
        if not f.word(('wBattleMonHP', 'wEnemyMonHP')[side]):
            return ()
        reserves = self.reserves(side)
        if not reserves:
            return moves
        original = bytes(f.state)
        rng = f.rng.rng.getstate()
        try:
            f._put('hBattleTurn', side)
            self.action_leaf.call('UserCanSwitch')
            allowed = bool(f.state.f & 128)
        finally:
            C.memmove(C.byref(f.state), original, C.sizeof(State))
            f.rng.rng.setstate(rng)
        return (*moves, *(SwitchAction(i) for i in reserves)) if allowed else moves

    def commit_actions(self, actions):
        f = self.fight
        f._put('wDeferredSwitch', 0)
        f._put('wEnemyUsingItem', 0)
        for side, action in enumerate(actions):
            f._put(('wPlayerSwitchTarget', 'wEnemySwitchTarget')[side],
                   action.index + 1 if isinstance(action, SwitchAction) else 0)
            move = f.moves[action.slot] if isinstance(action, MoveAction) else 0
            f._put(('wCurPlayerMove', 'wCurEnemyMove')[side], move)
            if isinstance(action, MoveAction):
                f._put(('wCurMoveNum', 'wCurEnemyMoveNum')[side], action.slot)
            f._put('hBattleTurn', side)
            self.action_leaf.call('UpdateMoveData')
        # Actual ParseEnemyAction executes the effect before DetermineMoveOrder.
        # Both choices have already been validated against precommit legality.
        if isinstance(actions[1], ItemAction):
            slot = actions[1].slot
            item = f.byte('wEnemyTrainerItem' + str(slot + 1))
            name = next(name for name in ITEM_ENTRIES
                        if f._constant('constants/item_constants.asm', name) == item)
            self.action_leaf.call(ITEM_ENTRIES[name])
            address = f.address('wEnemyTrainerItem' + str(slot + 1))
            f.state.d, f.state.e = address >> 8, address & 255
            self.action_leaf.call('AI_TryItem.used_item')

    def step(self, actions, choose=None):
        f = self.fight
        if len(actions) != 2 or f.terminal or f.truncated or f.failed:
            raise UnsupportedFight('Invalid action turn boundary')
        if any(type(action) not in (MoveAction, SwitchAction, ItemAction)
               or type(action.index if isinstance(action, SwitchAction) else action.slot) is not int
               or action not in self.legal_actions(side) for side, action in enumerate(actions)):
            raise UnsupportedFight('Illegal action')
        choose = choose or (lambda side, observation, legal: legal[0])
        try:
            self.commit_actions(actions)
            f._call('DetermineMoveOrder')
            first = 0 if f.state.f & 16 else 1
            f._put('wEnemyGoesFirst', first)
            self.last_order = (first, first ^ 1)
            events = []
            for side in (first, first ^ 1):
                action = actions[side]
                opponent = ('wEnemyMon', 'wBattleMon')[side]
                stem = ('wBattleMon', 'wEnemyMon')[side]
                before = f.word(opponent + 'HP')
                f._put('hBattleTurn', side)
                f._put('wDamageTaken', 0, 2)
                used, presentation = f._call('DoTurn')
                switched, switch_events = self.action_leaf.call('DeferredSwitch')
                self.action_leaf.call('ResetAbilityIgnorance')
                if isinstance(action, MoveAction):
                    hp = f.word(opponent + 'HP')
                    events.append(Transition(side, action.slot, f.moves[action.slot], before - hp,
                        hp, f.state.m[f.address(stem + 'PP') + action.slot] & 63,
                        bool(f.byte('wAttackMissed')), used + switched, presentation + switch_events))
                elif isinstance(action, SwitchAction):
                    events.append(dict(actor=side, switch=action.index,
                                       div_events=used + switched, presentation=presentation + switch_events))
                else:
                    events.append(dict(actor=side, item_slot=action.slot,
                                       div_events=used + switched, presentation=presentation + switch_events))
                if not f.word(opponent + 'HP'):
                    break
            f.turn += 1
            self._resolve(choose)
            if not f.terminal:
                if any(not self.legal_actions(side) for side in (0, 1)):
                    f.truncated, f.truncation_reason = True, 'unsupported-struggle'
                elif f.turn >= f.max_turns:
                    f.truncated, f.truncation_reason = True, 'turn-limit'
            # Keep event memory bounded by the fixed turn limit.
            f.events.extend(events)
            return tuple(events)
        except Exception:
            f.failed = True
            raise
