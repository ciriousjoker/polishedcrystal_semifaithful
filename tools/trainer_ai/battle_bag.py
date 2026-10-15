"""Typed player bag data and input callbacks, with effects left to the ROM.

This module does not heal, consume an item, set turn flags, or judge whether an
item works. BattlePack, ItemSubmenu.Use, DoItemEffect and TossItem own those
rules. The controller must classify menu callers before invoking input().
"""
from dataclasses import dataclass
from battle_env import UnsupportedFight


@dataclass(frozen=True)
class BagStack:
    item: str
    quantity: int = 1


@dataclass(frozen=True)
class BagAction:
    """Choose a stack and optional zero-based party and move targets.

    Selecting an ineffective target is permitted: the game determines retry and
    turn consumption. A bounded driver can cancel a retry through cancel().
    """
    item: str
    stack: int = 0  # occurrence among stacks with this item, not pocket index
    pokemon: int | None = None
    move: int | None = None


@dataclass(frozen=True)
class CancelBagAction:
    """Explicitly press B while unwinding a rejected pending bag attempt."""


def ineffective_item_prompt(machine):
    """Identify the actual no-effect source caller of projected PrintText.

    This is not inferred from repeated input callbacks or pocket navigation.
    The original caller still executes its zero success flag and all returns.
    """
    s = machine.state
    text_bank, text_pc = machine.symbols['WontHaveAnyEffectText']
    if s.bank != text_bank or ((s.h << 8) | s.l) != text_pc:
        return False
    if not 0xc000 <= s.sp < 0xdffe:
        raise UnsupportedFight('Invalid no-effect text call frame')
    target = s.m[s.sp] | (s.m[s.sp+1] << 8)
    bank, caller = machine.symbols['CantUseItemMessage']
    if machine.symbols['ItemNotUsed_ExitMenu'] != (bank, caller+3):
        raise UnsupportedFight('No-effect item return scaffold changed')
    print_pc = machine.symbols['PrintText'][1]
    expected = bytes((0xcd, print_pc & 255, print_pc >> 8))
    if machine._rom_bytes('CantUseItemMessage', 0, 3) != expected:
        raise UnsupportedFight('No-effect item text caller changed')
    return s.bank == bank and target == caller+3


POCKETS = {1: ('wNumItems', 'wItems', 'wItemsEnd'),
           2: ('wNumMedicine', 'wMedicine', 'wMedicineEnd'),
           3: ('wNumBalls', 'wBalls', 'wBallsEnd'),
           5: ('wNumBerries', 'wBerries', 'wBerriesEnd')}


def item_record(machine, name):
    """Read assembled item attributes after checking the decoder's layout."""
    path = machine.source_root / 'data/items/attributes.asm'
    macro = path.read_text().split('MACRO item_attribute', 1)[1].split('ENDM', 1)[0]
    code = [line.split(';', 1)[0].strip() for line in macro.splitlines()]
    code = [line for line in code if line]
    if code != ['dw \\1', 'db \\2, \\3, \\4', 'dn \\5, \\6']:
        raise UnsupportedFight('Item attribute layout changed; bag decoder needs review')
    identifier = machine._constant('constants/item_constants.asm', name)
    if not 1 <= identifier < 255:
        raise UnsupportedFight('Invalid bag item')
    data = machine._rom_bytes('ItemAttributes', (identifier - 1) * 6, 6)
    return identifier, data[4] & 15, data[5] & 15


def initialize_bag(machine, stacks=()):
    """Initialize game-start inventory; quantities change only through ROM code."""
    groups = {pocket: [] for pocket in POCKETS}
    for stack in stacks:
        if not isinstance(stack, BagStack) or type(stack.quantity) is not int or not 1 <= stack.quantity <= 99:
            raise UnsupportedFight('Bag quantities must be integers from one through 99')
        identifier, pocket, _ = item_record(machine, stack.item)
        if pocket not in groups:
            raise UnsupportedFight('Only ordinary stackable bag pockets are accepted')
        groups[pocket].append((identifier, stack.quantity))
    for pocket, entries in groups.items():
        count, start, end = POCKETS[pocket]
        capacity = (machine.address(end) - machine.address(start) - 1) // 2
        if len(entries) > capacity:
            raise UnsupportedFight('Bag pocket exceeds source-defined capacity')
        machine._put(count, len(entries))
        address = machine.address(start)
        size = machine.address(end) - address
        machine.state.m[address:address + size] = bytes(size)
        for index, pair in enumerate(entries):
            machine.state.m[address + index * 2:address + index * 2 + 2] = pair
        machine.state.m[address + len(entries) * 2] = 255


def inventory(machine):
    """Return raw pocket records, preserving duplicate stacks and their order."""
    result = {}
    for pocket, (count, start, end) in POCKETS.items():
        number = machine.byte(count)
        capacity = (machine.address(end) - machine.address(start) - 1) // 2
        if number > capacity:
            raise UnsupportedFight('Corrupt bag pocket count')
        address = machine.address(start)
        if machine.state.m[address + number * 2] != 255:
            raise UnsupportedFight('Missing bag pocket terminator')
        result[pocket] = tuple((machine.state.m[address + 2 * i],
                                machine.state.m[address + 2 * i + 1])
                               for i in range(number))
    return result


class BagInput:
    """One pending bag intent serviced at classified low-level input leaves.

    Supported contexts are scrolling, use_submenu, party and pp_move. The caller
    must establish context from the actual suspended call stack. Unknown callers
    reject. Input handling does not return from the boundary itself.
    """
    def __init__(self, machine, action):
        if not isinstance(action, BagAction) or type(action.stack) is not int or action.stack < 0:
            raise UnsupportedFight('Invalid typed bag action')
        self.machine, self.action = machine, action
        self.identifier, self.pocket, self.context = item_record(machine, action.item)
        if self.pocket not in POCKETS:
            raise UnsupportedFight('Unsupported bag pocket')
        for index, limit in ((action.pokemon, machine.byte('wPartyCount')), (action.move, 4)):
            if index is not None and (type(index) is not int or not 0 <= index < limit):
                raise UnsupportedFight('Invalid bag target index')
        self.cancelled = False
        self.callbacks = 0
        self._stack_index()

    def _stack_index(self):
        matches = [index for index, (item, quantity) in enumerate(inventory(self.machine)[self.pocket])
                   if item == self.identifier and quantity > 0]
        if self.action.stack >= len(matches):
            raise UnsupportedFight('Selected bag stack does not exist')
        return matches[self.action.stack]

    def cancel(self):
        self.cancelled = True

    def input(self, context):
        m, action = self.machine, self.action
        self.callbacks += 1
        if self.callbacks > 64:
            raise UnsupportedFight('Bag input retry bound exceeded')
        button = 2 if self.cancelled else 1  # PAD_B or PAD_A
        if context == 'scrolling':
            if not self.cancelled and m.byte('wCurPocket') != self.pocket - 1:
                button = 16  # PAD_RIGHT; real pack code changes the pocket
            elif not self.cancelled:
                index = self._stack_index()
                height = m.byte('wMenuData_ScrollingMenuHeight')
                if not height:
                    raise UnsupportedFight('Scrolling menu has no visible rows')
                scroll = max(0, index - height + 1)
                m._put('wMenuScrollPosition', scroll)
                m._put('wMenuCursorY', index - scroll + 1)
        elif context == 'use_submenu':
            m._put('wMenuCursorY', 1)
        elif context == 'party':
            if action.pokemon is None and not self.cancelled:
                raise UnsupportedFight('Bag effect requires a party target')
            m._put('wMenuCursorY', (action.pokemon or 0) + 1)
        elif context == 'pp_move':
            if action.move is None and not self.cancelled:
                raise UnsupportedFight('Bag effect requires a move target')
            m._put('wMenuCursorY', (action.move or 0) + 1)
        else:
            raise UnsupportedFight('Unclassified bag menu input: ' + context)
        m._put('hJoyPressed', button)
        m._put('hJoyLast', button)
        m._put('hJoyDown', button)
        # The direct DoMenuJoypadLoop boundary includes GetMenuJoypad's return.
        # Source callers derive their own success/carry and selection fields.
        m.state.a = button

