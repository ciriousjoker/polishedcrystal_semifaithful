"""Typed, source-driven ordinary trainer battles with fixed levels.

This is a controller adapter, not a second battle rules implementation. The
assembled DoBattle and BattleTurn callers own combat, inventory consumption,
replacement and terminal decisions. Unsupported callbacks reject the episode.
Passing differential checks is required before claiming a mechanic supported.
"""
import copy
from dataclasses import asdict, dataclass, is_dataclass
import hashlib
import pathlib
import re

from battle_env import MoveAction, UnsupportedFight
from battle_actions import SwitchAction, ItemAction
from battle_bag import BagAction, BagInput, CancelBagAction, ineffective_item_prompt
from battle_decisions import ContinueAction, StruggleAction, observation, legal_actions, _query
from battle_scenarios import Scenario, initialize


ACTION_TYPES = (MoveAction, SwitchAction, ItemAction, ContinueAction, StruggleAction, BagAction)
TRAINER_EFFECTS = {
    'POTION': 'EnemyUsedPotion', 'SUPER_POTION': 'EnemyUsedSuperPotion',
    'HYPER_POTION': 'EnemyUsedHyperPotion', 'MAX_POTION': 'EnemyUsedMaxPotion',
    'FULL_HEAL': 'EnemyUsedFullHeal', 'FULL_RESTORE': 'EnemyUsedFullRestore',
    'GUARD_SPEC': 'EnemyUsedGuardSpec', 'DIRE_HIT': 'EnemyUsedDireHit',
    **{name: 'EnemyUsedXItem' for name in
       ('X_ATTACK', 'X_DEFEND', 'X_SPEED', 'X_SP_ATK', 'X_SP_DEF', 'X_ACCURACY')},
}


def decision_boundary(machine):
    """Locate the source call immediately after automatic turn preparation.

    Both source scaffold and assembled bank/address operands must agree. A
    source update that changes this contract requires review, not a guessed
    offset or a fallback to observations taken before held-item effects.
    """
    source = (machine.source_root / 'engine/battle/core.asm').read_text()
    match = re.search(r'^BattleTurn:\s*\n(.*?)^\.skip_ai_move:?\s*$', source, re.M | re.S)
    if match is None:
        raise UnsupportedFight('BattleTurn decision scaffold requires review')
    lines = [line.split(';', 1)[0].strip() for line in match[1].splitlines()]
    lines = [line for line in lines if line]
    scaffold = ['call HandleBerserkGene', 'farcall CheckMirrorHerb', 'call UpdateBattleMonInParty']
    if sum(lines[index:index + 3] == scaffold for index in range(len(lines) - 2)) != 1:
        raise UnsupportedFight('Automatic preparation decision scaffold changed')
    bank, start = machine.symbols['BattleTurn']
    end_bank, end = machine.symbols['BattleTurn.skip_ai_move']
    if bank != end_bank or not 0x4000 <= start < end < 0x8000:
        raise UnsupportedFight('BattleTurn decision search is outside one source ROM bank')

    def call_bytes(name):
        target_bank, target = machine.symbols[name]
        if not (target < 0x4000 or target_bank == bank) or not 0 <= target < 0x8000:
            raise UnsupportedFight('Near-call decision scaffold bank changed: ' + name)
        return bytes((0xcd, target & 255, target >> 8))

    rst_bank, rst = machine.symbols['FarCall']
    mirror_bank, mirror = machine.symbols['CheckMirrorHerb']
    if rst_bank or rst not in range(0, 0x40, 8) or not 0 <= mirror_bank <= 255 or not 0 <= mirror < 0x8000:
        raise UnsupportedFight('Far-call decision scaffold ABI changed')
    encoded = (call_bytes('HandleBerserkGene') +
               bytes((0xc7 | rst, mirror & 255, mirror >> 8, mirror_bank)) +
               call_bytes('UpdateBattleMonInParty'))
    offset = bank * 0x4000 + start - 0x4000
    region = machine.rom[offset:offset + end - start]
    matches = [index for index in range(len(region) - len(encoded) + 1)
               if region[index:index + len(encoded)] == encoded]
    if len(matches) != 1:
        raise UnsupportedFight('Assembled automatic preparation decision scaffold changed')
    pc = start + matches[0] + len(encoded) - 3
    return dict(bank=bank, pc=pc, source='BattleTurn',
                point='after-HandleBerserkGene-and-CheckMirrorHerb-before-UpdateBattleMonInParty',
                scaffold_hex=encoded.hex())


def encode_action(action):
    if type(action) not in ACTION_TYPES:
        raise UnsupportedFight('Unknown controller action type')
    return dict(type=type(action).__name__, **asdict(action))


def decode_action(record):
    if type(record) is not dict or 'type' not in record:
        raise UnsupportedFight('Malformed recorded action')
    classes = {cls.__name__: cls for cls in ACTION_TYPES}
    if record['type'] not in classes:
        raise UnsupportedFight('Unknown recorded action')
    try:
        return classes[record['type']](**{k: v for k, v in record.items() if k != 'type'})
    except TypeError as error:
        raise UnsupportedFight('Invalid recorded action fields') from error


@dataclass(frozen=True)
class FightTransition:
    turn: int
    actions: tuple
    observations: tuple
    terminal: bool
    truncated: bool
    winner: int | None
    div_events: tuple
    boundaries: tuple


class ControllerFight:
    """A reset/step API over the resumable actual-source controller.

    Construct with an already verified ControllerMachine. Optional chooser is
    called as chooser(side, detached_observation, legal_actions). A replacement
    chooser has the same signature, with only SwitchAction intents available.
    Both actors' inputs are copied before either chooser is called. Observers
    receive detached diagnostic records and never become actor input.

    No automatic action fallback exists. Ineffective bag choices that reopen
    the battle menu require an explicit retry_chooser. Only the player's intent
    is replaced; the submitted enemy intent remains hidden and unchanged.
    A source-derived stop after automatic Berserk Gene/Mirror Herb preparation
    defines the observation contract, before either actor commits an action.
    """
    def __init__(self, machine, *, chooser=None, replacement_chooser=None,
                 retry_chooser=None, bag_retry_chooser=None, observer=None, max_turns=128):
        if type(max_turns) is not int or not 1 <= max_turns <= 65535:
            raise UnsupportedFight('Invalid controller turn bound')
        self.machine = machine
        self.decision_boundary = decision_boundary(machine)
        self.chooser, self.replacement_chooser = chooser, replacement_chooser
        self.retry_chooser, self.bag_retry_chooser = retry_chooser, bag_retry_chooser
        self.observer, self.max_turns = observer, max_turns
        self._old_segment_checker = machine.segment_checker
        self._old_single_step_checker = machine.single_step_checker
        self._old_choice = machine.on_choice
        self._ready = False
        self._pending = None
        self.records = []
        self._div = []
        self._boundaries = []
        self._replacement_cache = None
        self._bag = None
        self._bag_attempted = False
        self._replacement_records = []
        self._retry_records = []
        self._bag_retry_records = []
        machine.segment_checker = self._segment
        machine.single_step_checker = self._single_step
        machine.on_choice = self._choice

    @property
    def terminal(self):
        return self.machine.terminal

    @property
    def truncated(self):
        return self.machine.truncated

    @property
    def winner(self):
        return self.machine.winner

    @property
    def turn(self):
        return self.machine.turn

    def _emit(self, stage, **record):
        if self.observer is not None:
            self.observer(stage, copy.deepcopy(record))

    def _segment(self, machine, stop, stop_bank=None):
        if self._old_segment_checker is not None:
            boundary, used = self._old_segment_checker(machine, stop, stop_bank)
        else:
            boundary, used = machine.raw_segment(machine.rng.preview(), stop, stop_bank=stop_bank)
            machine.rng.consume(len(used))
        self._div.extend(used)
        if boundary is not None:
            self._boundaries.append(boundary.name)
            self._emit('boundary', name=boundary.name, bank=boundary.bank,
                       pc=boundary.pc, turn=machine.turn)
            if (boundary.name == 'PrintText' and self._bag is not None and
                    self._pending is not None and ineffective_item_prompt(machine)):
                self._retry_bag_target()
        return boundary, used

    def _single_step(self, machine):
        if self._old_single_step_checker is not None:
            self._old_single_step_checker(machine)
        else:
            machine.single_step_checker = None
            try:
                machine.single_step()
            finally:
                machine.single_step_checker = self._single_step
        self._div.extend(machine.state.events[:machine.state.event_pos])

    def _at_decision(self):
        m = self.machine
        bank, pc = self.decision_boundary['bank'], self.decision_boundary['pc']
        return m.state.pc == pc and (pc < 0x4000 or m.state.bank == bank) and not m.state.fault

    def _resume_to_decision(self):
        m = self.machine
        bank, pc = self.decision_boundary['bank'], self.decision_boundary['pc']
        m.resume(stop=pc, stop_bank=bank)
        if not m.terminal and not self._at_decision():
            raise UnsupportedFight('Source controller returned before a decision or terminal boundary')

    def _snapshot(self):
        m = self.machine
        # This pair is constructed before any policy sees either observation.
        observations = tuple(observation(m, side) for side in (0, 1))
        masks = tuple(legal_actions(m, side) for side in (0, 1))
        return copy.deepcopy(observations), copy.deepcopy(masks)

    def reset(self, scenario=Scenario(), seed=193):
        m = self.machine
        self._ready = False
        self._pending = self._bag = self._replacement_cache = None
        self._bag_attempted = False
        self._replacement_records, self._retry_records = [], []
        self._bag_retry_records = []
        self.records, self._div, self._boundaries = [], [], []
        initialize(m, scenario, seed)
        # The user explicitly selected fixed-level training. Require the real
        # option, rather than emulating Tower or skipping a gameplay caller.
        from battle_coverage import runtime_option_bits
        no_exp = 1 << runtime_option_bits(m.source_root)['NO_EXP_OPT']
        if not m.byte('wInitialOptions2') & no_exp:
            raise UnsupportedFight('ControllerFight requires actual NO_EXP_OPT')
        if m.byte('wLinkMode') or m.byte('wInBattleTowerBattle') or m.byte('wBattleMode') != 2:
            raise UnsupportedFight('ControllerFight requires ordinary non-link trainer mode')
        self.seed, self.scenario = seed, scenario
        self._initial_party = self._fixed_party_values()
        self._initial_base_stats = None
        m.prepare_call('DoBattle')
        self._resume_to_decision()
        self._assert_fixed_party()
        # Actual DoBattle first calls FixPlayerEVsAndStats. That legal
        # initialization can normalize supplied EV records and recompute stats.
        # Freeze permanent stats after initialization and first automatic
        # preparation, before either actor commits its first action.
        self._initial_base_stats = self._fixed_base_stats()
        self._ready = not m.terminal
        self.frozen_observations, self.frozen_masks = self._snapshot()
        self.initial_state_sha256 = hashlib.sha256(bytes(m.state)).hexdigest()
        self.initial_div_events = tuple(self._div)
        self.initial_boundaries = tuple(self._boundaries)
        self._emit('reset', observations=self.frozen_observations,
                   legal_actions=tuple(tuple(encode_action(a) for a in mask) for mask in self.frozen_masks))
        return self.observations()

    def _fixed_party_values(self):
        m = self.machine
        # Temporary in-battle stats and species may change through Transform,
        # Power Trick or stat effects. Read permanent party fields separately.
        result = []
        for side, prefix in enumerate(('wPartyMon', 'wOTPartyMon')):
            count = m.byte(('wPartyCount', 'wOTPartyCount')[side])
            for index in range(count):
                stem = prefix + str(index + 1)
                address = m.address(stem + 'Exp')
                result.append((m.byte(stem + 'Level'), bytes(m.state.m[address:address + 3])))
        return tuple(result)

    def _fixed_base_stats(self):
        m = self.machine
        fields = ('MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')
        result = []
        for side, prefix in enumerate(('wPartyMon', 'wOTPartyMon')):
            count = m.byte(('wPartyCount', 'wOTPartyCount')[side])
            for index in range(count):
                stem = prefix + str(index + 1)
                result.append(dict(side=side, index=index,
                                   **{field: m.word(stem + field) for field in fields}))
        return tuple(result)

    def _assert_fixed_party(self):
        if self._fixed_party_values() != self._initial_party:
            raise UnsupportedFight('Fixed-level controller unexpectedly changed stored level or XP')
        if (self._initial_base_stats is not None and
                self._fixed_base_stats() != self._initial_base_stats):
            raise UnsupportedFight('Fixed-level controller unexpectedly changed stored base stats')

    def observations(self):
        if not hasattr(self, 'frozen_observations'):
            raise UnsupportedFight('Reset the controller before reading actors')
        return copy.deepcopy(self.frozen_observations)

    def observation(self, side):
        if type(side) is not int or side not in (0, 1):
            raise UnsupportedFight('Invalid actor side')
        return self.observations()[side]

    def legal_actions(self, side):
        if type(side) is not int or side not in (0, 1):
            raise UnsupportedFight('Invalid actor side')
        if not hasattr(self, 'frozen_masks'):
            raise UnsupportedFight('Reset the controller before querying actions')
        return copy.deepcopy(self.frozen_masks[side])

    def choose(self):
        if self.chooser is None:
            raise UnsupportedFight('No controller policy supplied')
        observations, masks = self.observations(), copy.deepcopy(self.frozen_masks)
        return tuple(self.chooser(side, observations[side], masks[side]) for side in (0, 1))

    def step(self, actions=None):
        m = self.machine
        if not self._ready or m.terminal or m.truncated or m.failed or not self._at_decision():
            raise UnsupportedFight('Controller is not at a live decision boundary')
        # Freeze again before callbacks, including caller-provided choices.
        self.frozen_observations, self.frozen_masks = self._snapshot()
        if actions is None:
            actions = self.choose()
        if not isinstance(actions, (tuple, list)) or len(actions) != 2:
            raise UnsupportedFight('Exactly two simultaneous typed actions are required')
        for side, action in enumerate(actions):
            if type(action) not in ACTION_TYPES or action not in self.frozen_masks[side]:
                raise UnsupportedFight('Action is outside the frozen legal mask for actor ' + str(side))
            # Python considers bool equal to int. Actor indices do not.
            if any(type(value) is not int for value in asdict(action).values()
                   if isinstance(value, (int, bool))):
                raise UnsupportedFight('Action indices must be actual integers')
        self._pending = tuple(copy.deepcopy(actions))
        self._bag = BagInput(m, actions[0]) if type(actions[0]) is BagAction else None
        self._bag_attempted = False
        self._replacement_records = []
        self._retry_records = []
        self._bag_retry_records = []
        self._replacement_cache = None
        self._div, self._boundaries = [], []
        before = self.observations()
        self._emit('turn-before', observations=before,
                   actions=tuple(encode_action(a) for a in actions))
        try:
            m.single_step()
            self._resume_to_decision()
            m.turn += 1
            self._assert_fixed_party()
            if not m.terminal and m.turn >= self.max_turns:
                m.truncated, m.truncation_reason = True, 'turn-limit'
                m.winner = None
            self.frozen_observations, self.frozen_masks = self._snapshot()
        except Exception:
            m.failed = True
            self._ready = False
            self._emit('failure', turn=m.turn, actions=tuple(encode_action(a) for a in actions),
                       boundaries=tuple(self._boundaries), div_events=tuple(self._div))
            raise
        finally:
            self._pending = None
        self._ready = not (m.terminal or m.truncated)
        transition = FightTransition(m.turn, tuple(actions), self.observations(),
            m.terminal, m.truncated, m.winner, tuple(self._div), tuple(self._boundaries))
        self.records.append(dict(turn=m.turn, actions=tuple(encode_action(a) for a in actions),
            terminal=m.terminal, truncated=m.truncated, winner=m.winner,
            div_events=tuple(self._div), boundaries=tuple(self._boundaries),
            replacement_choices=copy.deepcopy(self._replacement_records),
            retry_choices=copy.deepcopy(self._retry_records),
            bag_retry_choices=copy.deepcopy(self._bag_retry_records),
            state_sha256=hashlib.sha256(bytes(m.state)).hexdigest()))
        self._emit('turn-after', transition=asdict(transition))
        return transition

    def _press(self, button=1):
        m = self.machine
        for field in ('hJoyPressed', 'hJoyLast', 'hJoyDown'):
            m._put(field, button)
        m.state.a = button

    def _nested_call(self, entry):
        """Execute a real helper below the suspended callback's stack frame."""
        m, s = self.machine, self.machine.state
        frame = s.pc, s.bank, s.sp, s.fault, m.byte('hROMBank')
        s.fault = 0
        try:
            m.call(entry)
            if m.terminal or s.sp != frame[2]:
                raise UnsupportedFight('Unexpected nested policy helper control flow')
        finally:
            s.pc, s.bank, s.sp, s.fault, rom_bank = frame
            m._put('hROMBank', rom_bank)

    def _trainer_item(self, action):
        m = self.machine
        slot = 'wEnemyTrainerItem' + str(action.slot + 1)
        item = m.byte(slot)
        matches = [name for name in TRAINER_EFFECTS
                   if m._constant('constants/item_constants.asm', name) == item]
        if len(matches) != 1:
            raise UnsupportedFight('Trainer item has no reviewed policy effect ABI')
        name = matches[0]
        if name.startswith('X_'):
            m.state.a = item
        self._nested_call(TRAINER_EFFECTS[name])
        if name.startswith('X_') and m.state.f & 16:
            raise UnsupportedFight('Selected trainer stat item failed its actual source handler')
        pointer = m.address(slot)
        m.state.d, m.state.e = pointer >> 8, pointer & 255
        # Source owns rage/protect reset, slot consumption, item flag and carry.
        self._nested_call('AI_TryItem.used_item')

    def _replacement(self, side):
        m = self.machine
        # Refreeze at every actual request. A whole-turn cache would become
        # stale after another faint or repeated entry-hazard KO. Pending switch
        # targets are absent from observation, so simultaneous callbacks cannot
        # reveal the other actor's selected replacement.
        observations = tuple(observation(m, actor) for actor in (0, 1))
        masks = tuple(tuple(SwitchAction(p['index']) for p in observations[actor]['teams'][actor]
                            if not p['active'] and p['HP'] and not p['egg']) for actor in (0, 1))
        if not masks[side]:
            raise UnsupportedFight('Source requested replacement with no living reserve')
        if self.replacement_chooser is None:
            raise UnsupportedFight('Replacement requires an explicit policy')
        choice = self.replacement_chooser(side, copy.deepcopy(observations[side]), masks[side])
        if (type(choice) is not SwitchAction or type(choice.index) is not int or choice not in masks[side]):
            raise UnsupportedFight('Replacement is outside its frozen legal mask')
        self._replacement_records.append(dict(side=side, action=encode_action(choice)))
        self._emit('replacement-choice', side=side, observations=observations,
                   action=encode_action(choice))
        return choice.index

    def _retry_bag_target(self):
        """Ask only after an actual source item attempt reported no effect."""
        if self.bag_retry_chooser is None:
            raise UnsupportedFight('Rejected bag target requires an explicit inner retry policy')
        if len(self._bag_retry_records) >= 32:
            raise UnsupportedFight('Inner bag retry decision bound exceeded')
        m, previous = self.machine, self._bag.action
        current = observation(m, 0)
        current['bag_retry'] = dict(reason='ineffective-item-target',
                                   previous_action=encode_action(previous))
        choices = tuple(action for action in legal_actions(m, 0)
                        if type(action) is BagAction and action.item == previous.item and
                        action.stack == previous.stack) + (CancelBagAction(),)
        choice = self.bag_retry_chooser(0, copy.deepcopy(current), copy.deepcopy(choices))
        if type(choice) is CancelBagAction:
            self._bag.cancel()
            encoded = dict(type='CancelBagAction')
        elif type(choice) is BagAction and choice in choices:
            if any(type(value) is not int for value in asdict(choice).values()
                   if isinstance(value, (int, bool))):
                raise UnsupportedFight('Inner bag retry indices must be actual integers')
            self._pending = (copy.deepcopy(choice), self._pending[1])
            self._bag = BagInput(m, choice)
            encoded = encode_action(choice)
        else:
            raise UnsupportedFight('Inner bag retry choice is outside its detached mask')
        record = dict(reason='ineffective-item-target', previous_action=encode_action(previous),
                      choice=encoded)
        self._bag_retry_records.append(record)
        self._emit('bag-retry', observation=current, choices=tuple(
            dict(type='CancelBagAction') if type(action) is CancelBagAction else encode_action(action)
            for action in choices), **record)

    def _retry_player(self):
        if self.retry_chooser is None:
            raise UnsupportedFight('Reopened battle menu requires an explicit player retry policy')
        if len(self._retry_records) >= 32:
            raise UnsupportedFight('Player retry decision bound exceeded')
        m = self.machine
        current = observation(m, 0)
        mask = legal_actions(m, 0)
        action = self.retry_chooser(0, copy.deepcopy(current), copy.deepcopy(mask))
        if type(action) not in ACTION_TYPES or action not in mask:
            raise UnsupportedFight('Player retry is outside its source-queried legal mask')
        if any(type(value) is not int for value in asdict(action).values()
               if isinstance(value, (int, bool))):
            raise UnsupportedFight('Retry action indices must be actual integers')
        self._pending = (copy.deepcopy(action), self._pending[1])
        self._bag = BagInput(m, action) if type(action) is BagAction else None
        self._bag_attempted = False
        self._retry_records.append(encode_action(action))
        self._emit('player-retry', observation=current, action=encode_action(action))
        return action

    def _choice(self, machine, name):
        if machine is not self.machine:
            raise UnsupportedFight('Choice belongs to another controller')
        m = machine
        pending = self._pending
        if pending is None:
            raise UnsupportedFight('Source requested a policy choice before frozen actor decisions')
        if name == 'GetSwitchScores':
            m._put('wEnemySwitchMonParam', self._replacement(1))
        elif name in ('_DoMenuJoypadLoop', 'MenuJoypadLoop'):
            self._menu(m.menu_input_context())
        elif name == '_2DMenu':
            # _2DMenu is also used outside battle. Require the actual menu
            # owner and reject unknown call stacks rather than guessing.
            owner = self._direct_owner()
            if owner != '_BattleMenuCommon' or pending is None:
                raise UnsupportedFight('Unclassified 2D menu owner: ' + owner)
            action = pending[0]
            if self._bag_attempted:
                action = self._retry_player()
            if type(action) is BagAction:
                self._bag_attempted = True
            m._put('wMenuCursorBuffer', 2 if type(action) is SwitchAction else 3 if type(action) is BagAction else 1)
            m.state.a, m.state.f = 0, 128
        elif name == '_AIChooseMove':
            if pending is None:
                raise UnsupportedFight('Enemy move input outside a simultaneous turn')
            action = pending[1]
            if type(action) is MoveAction:
                slot = action.slot
            elif type(action) in (SwitchAction, ItemAction, StruggleAction):
                # AIChooseMove temporarily clears Encore before this callback.
                # Its internal provisional candidates belong to that actual
                # source state, not the actor's frozen decision mask. Source
                # restores Encore and checks the provisional move afterwards.
                usable = [slot for slot in range(4)
                          if m.state.m[m.address('wEnemyMonMoves') + slot]
                          and _query(m, 'CheckUsableMove', 1, a=slot).f & 128]
                if not usable:
                    raise UnsupportedFight('Source requested provisional AI selection without a usable move')
                slot = usable[0]
            else:
                raise UnsupportedFight('Source unexpectedly requested a move for a locked or struggling actor')
            m._put('wCurEnemyMoveNum', slot)
            m._put('wCurEnemyMove', m.state.m[m.address('wEnemyMonMoves') + slot])
            m.state.f = 128
        elif name == 'AI_MaybeSwitch':
            if pending is None:
                raise UnsupportedFight('Enemy switch input outside a simultaneous turn')
            action = pending[1]
            if type(action) is SwitchAction:
                m._put('wEnemySwitchTarget', action.index + 1)
                m.state.f |= 16
            else:
                m.state.f &= ~16
        elif name == 'AI_TryItem':
            if pending is None:
                raise UnsupportedFight('Enemy item input outside a simultaneous turn')
            if type(pending[1]) is ItemAction:
                self._trainer_item(pending[1])
            else:
                m.state.f &= ~16
        else:
            raise UnsupportedFight('Unimplemented source choice: ' + name)

    def _direct_owner(self):
        m, s = self.machine, self.machine.state
        target = s.m[s.sp] | s.m[s.sp + 1] << 8
        owners = [(pc, label) for label, (bank, pc) in m.symbols.items()
                  if '.' not in label and pc < target and (pc < 0x4000 or bank == s.bank)]
        if not owners:
            raise UnsupportedFight('Unknown direct menu callback caller')
        return max(owners)[1]

    def _menu(self, owner):
        m = self.machine
        action = self._pending[0] if self._pending else None
        if owner == 'MoveSelectionScreen':
            if self._bag is not None and m.byte('wMoveSelectionMenuType') == 1:
                self._bag.input('pp_move')
            elif type(action) is MoveAction:
                m._put('wMenuCursorY', action.slot + 1)
                self._press()
            else:
                raise UnsupportedFight('Move menu conflicts with typed action')
        elif owner == 'PartyMenuSelect':
            if self._bag is not None:
                self._bag.input('party')
            else:
                own = observation(m, 0)['teams'][0]
                candidate = next((p for p in own if type(action) is SwitchAction and
                                  p['index'] == action.index and not p['active'] and p['HP'] and not p['egg']), None)
                target = candidate['index'] if candidate is not None else self._replacement(0)
                m._put('wMenuCursorY', target + 1)
                self._press()
        elif owner == 'BattleMenuPKMN_Loop':
            if type(action) is not SwitchAction:
                raise UnsupportedFight('Party submenu conflicts with typed action')
            m._put('wMenuCursorY', 1)  # Select SWITCH, not STATS or CANCEL.
            self._press()
        elif owner == 'ScrollingMenuJoyAction':
            if self._bag is None:
                raise UnsupportedFight('Bag scrolling without a bag action')
            self._bag.input('scrolling')
        elif owner == 'VerticalMenu':
            if self._bag is None:
                raise UnsupportedFight('Vertical menu without a classified bag intent')
            self._bag.input('use_submenu')
        else:
            raise UnsupportedFight('Unclassified low-level menu owner: ' + owner)

    def replay_record(self):
        """Diagnostic provenance and private event tape, never actor features."""
        m = self.machine
        if not hasattr(self, 'seed'):
            raise UnsupportedFight('Cannot record a controller before reset')
        digest = lambda path: hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()
        return dict(scope='ordinary-trainer-fixed-level-source-controller', seed=self.seed,
            scenario=asdict(self.scenario) if is_dataclass(self.scenario) else None,
            options=dict(initial=m.byte('wInitialOptions'), initial2=m.byte('wInitialOptions2'),
                         link=m.byte('wLinkMode'), tower=m.byte('wInBattleTowerBattle'),
                         battle_mode=m.byte('wBattleMode')),
            rom_sha256=digest(m.rom_path), symbols_sha256=digest(m.symbols_path),
            generator_sha256=digest(pathlib.Path(__file__).with_name('stateful_lifting.py')),
            manifest_sha256=digest(m.build_dir / 'manifest.json'),
            decision_boundary=copy.deepcopy(self.decision_boundary),
            initial_div_events=self.initial_div_events,
            initial_boundaries=self.initial_boundaries,
            fixed_party=dict(
                level_xp_boundary='scenario-initialized-before-DoBattle',
                level_xp=tuple(dict(level=level, xp_hex=xp.hex()) for level, xp in self._initial_party),
                base_stats_boundary='after-source-DoBattle-and-first-automatic-preparation-before-first-actor-decision',
                base_stats=copy.deepcopy(self._initial_base_stats)),
            initial_state_sha256=self.initial_state_sha256, turns=copy.deepcopy(self.records),
            terminal=m.terminal, truncated=m.truncated, failed=m.failed,
            winner=m.winner, truncation_reason=m.truncation_reason)
