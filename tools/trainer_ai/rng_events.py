#!/usr/bin/env python3
"""Explicit source routine events under a bounded, synchronous RNG contract.

Events are ordinary routine calls, not interrupts or frames. Registers and flags
flow from one call to the next, except an explicitly supplied A argument. Each
call uses a real WRAM stack return sentinel. The caller owns scheduling and tape.
"""
import ctypes as C
from stateful_fixture_check import State

ENTRIES = ('UpdateDividerCounters', 'AdvanceRNGState', 'Random', 'BattleRandomRange')
STOP = 0xfffe
BUDGET = 100000


def prepare_call(state, symbols, name, argument=None):
    if name not in ENTRIES:
        raise ValueError('Unsupported RNG event: ' + name)
    if name == 'BattleRandomRange' and state.m[symbols['wLinkMode'][1]]:
        raise ValueError('Link RNG is outside the event profile')
    if argument is not None:
        if name != 'BattleRandomRange' or not 1 <= argument <= 255:
            raise ValueError('Only BattleRandomRange accepts A, in 1..255')
        state.a = argument
    elif name == 'BattleRandomRange':
        raise ValueError('BattleRandomRange requires an explicit nonzero range')
    bank, pc = symbols[name]
    if bank and bank != state.bank:
        raise ValueError('Event requires a different mapped bank')
    if not 0xc002 <= state.sp <= 0xdffe:
        raise ValueError('Unsupported stack location')
    state.sp -= 2
    state.m[state.sp] = STOP & 255
    state.m[state.sp + 1] = STOP >> 8
    state.pc = pc
    state.steps = state.fault = state.event_count = state.event_pos = 0


def replay_call(lib, state, symbols, name, div_reads, argument=None):
    if len(div_reads) > 4096 or any(not 0 <= x <= 255 for x in div_reads):
        raise ValueError('Invalid DIV tape')
    prepare_call(state, symbols, name, argument)
    state.event_count = len(div_reads)
    state.events[:len(div_reads)] = div_reads
    lib.stateful_run(C.byref(state), BUDGET, STOP)
    if state.fault or state.pc != STOP or state.event_pos != len(div_reads):
        raise ValueError(f'Unsupported RNG replay: fault={state.fault}, '
                         f'PC={state.pc:04x}, consumed={state.event_pos}/{len(div_reads)}')
    return state


def replay_events(lib, state, symbols, events):
    """Replay ordered {routine, div_reads, argument?} ordinary call events."""
    for event in events:
        if set(event) - {'routine', 'div_reads', 'argument'}:
            raise ValueError('Unknown event field')
        replay_call(lib, state, symbols, event['routine'], event['div_reads'],
                    event.get('argument'))
    return state
