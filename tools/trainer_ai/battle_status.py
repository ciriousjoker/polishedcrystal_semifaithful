"""Conditional headless poison, burn and Toxic fixtures, with explicit host endturn orchestration.

Actual HandlePoison or HandleBurn runs after a surviving actual DoTurn pair. This is not the
whole HandleBetweenTurnEffects controller or ordinary graphical battle fidelity.
"""
import ctypes as C
import json
import pathlib
from dataclasses import dataclass
from battle_env import NativeFight, UnsupportedFight
import battle_env_profile as base
import battle_status_profile as profile
import stateful_lifting
from stateful_fixture_check import State


@dataclass(frozen=True)
class Residual:
    hp_before: tuple
    hp_after: tuple
    div_events: tuple
    presentation: tuple


class PoisonLeaf:
    """Independent verified native handle, sharing only the matched State ABI."""
    def __init__(self, fight, build_dir):
        self.fight = fight
        self.symbols = fight.symbols
        manifest = json.loads((pathlib.Path(build_dir) / 'manifest.json').read_text())
        self.lib = stateful_lifting.load_verified(build_dir, fight.rom_path,
            fight.symbols_path, fight.source_root, profile.ENTRIES,
            base.PRESENTATION, tuple(manifest.get('excluded', {})))
        base.verify_audit(fight.rom_path, fight.symbols_path, fight.source_root)
        self.lib.stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]
        self.failed = False
        self.entry = 'HandlePoison'

    @property
    def state(self):
        return self.fight.state

    def address(self, name):
        return self.fight.address(name)

    def _put(self, *args):
        self.fight._put(*args)

    prepare_call = NativeFight.prepare_call
    run_prepared = NativeFight.run_prepared

    def call(self):
        self.prepare_call(self.entry)
        used, frontend = self.run_prepared(self.fight.rng.preview())
        self.fight.rng.consume(len(used))
        return used, frontend


class StatusFight:
    """One initially affected actor, Own Tempo, no items or other effects.

    Constructor inputs remain fixture parameters, not arbitrary RAM.
    Double residual faints reject. Single residual faints synchronize actual
    party state, then execute the source trainer battle-over decision leaf.
    """
    def __init__(self, rom, symbols, source_root, base_build, status_build,
                 seed=1, orientation=0, poisoned_side=0, max_turns=64, status='poison'):
        if type(poisoned_side) is not int or poisoned_side not in (0, 1):
            raise UnsupportedFight('Exactly one affected actor is required')
        if status not in ('poison', 'burn', 'toxic'):
            raise UnsupportedFight('Unsupported initial residual status')
        self.status = status
        self.fight = NativeFight(rom, symbols, source_root, base_build,
                                seed, orientation, max_turns)
        self.leaf = PoisonLeaf(self.fight, status_build)
        self.terminal_leaf = TerminalLeaf(self.fight, pathlib.Path(status_build).parent / 'terminal')
        self.leaf.entry = 'HandleBurn' if status == 'burn' else 'HandlePoison'
        self.poisoned_side = poisoned_side
        self.residuals = []
        self._set_status()

    def _set_status(self):
        bits = ('BRN',) if self.status == 'burn' else ('PSN', 'TOX') if self.status == 'toxic' else ('PSN',)
        value = sum(1 << self.fight._constant('constants/battle_constants.asm', bit) for bit in bits)
        self.fight._put(('wBattleMonStatus', 'wEnemyMonStatus')[self.poisoned_side], value)
        self.fight._put(('wPartyMon1Status', 'wOTPartyMon1Status')[self.poisoned_side], value)

    def reset(self, seed=1, orientation=None):
        self.fight.reset(seed, orientation)
        self.leaf.failed = False
        self.residuals = []
        self._set_status()
        return self.observation(0)

    def observation(self, side):
        observation = self.fight.observation(side)
        for actor, team in enumerate(observation['teams']):
            team[0]['toxic_count'] = self.fight.byte(('wPlayerToxicCount', 'wEnemyToxicCount')[actor])
        return observation

    def legal_actions(self, side):
        return self.fight.legal_actions(side)

    @property
    def terminal(self):
        return self.fight.terminal

    @property
    def truncated(self):
        return self.fight.truncated

    @property
    def winner(self):
        return self.fight.winner

    def step(self, actions):
        events = self.fight.step(actions)
        if self.terminal:
            return events
        try:
            before = tuple(self.fight.word(n + 'HP') for n in ('wBattleMon', 'wEnemyMon'))
            used, frontend = self.leaf.call()
            after = tuple(self.fight.word(n + 'HP') for n in ('wBattleMon', 'wEnemyMon'))
            self.residuals.append(Residual(before, after, used, frontend))
            self.terminal_leaf.resolve()
            return events
        except Exception:
            self.fight.failed = True
            raise


class TerminalLeaf:
    """Source party synchronization and trainer battle-over decision only.

    WinTrainerBattle/LostBattle are terminal events. They are never fabricated
    identity returns and no reward, money, experience or menu code executes.
    """
    def __init__(self, fight, build_dir):
        self.fight = fight
        self.symbols = fight.symbols
        base.verify_audit(fight.rom_path, fight.symbols_path, fight.source_root)
        self.lib = stateful_lifting.load_verified(build_dir, fight.rom_path,
            fight.symbols_path, fight.source_root, profile.TERMINAL_ENTRIES,
            profile.TERMINAL_EVENTS)
        self.lib.stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]

    state = PoisonLeaf.state
    address = PoisonLeaf.address
    _put = PoisonLeaf._put
    prepare_call = NativeFight.prepare_call

    def call(self, entry):
        if entry not in profile.TERMINAL_ENTRIES:
            raise UnsupportedFight('Unsupported terminal entry')
        self.prepare_call(entry)
        self.state.event_count = self.state.event_pos = 0
        self.lib.stateful_run(C.byref(self.state), 200000, 65534)
        if self.state.fault == 0:
            return None
        if self.state.fault == 5:
            for name in profile.TERMINAL_EVENTS:
                bank, pc = self.symbols[name]
                if self.state.pc == pc and (pc < 0x4000 or self.state.bank == bank):
                    return name
        raise UnsupportedFight('Unsupported source terminal decision path')

    def resolve(self):
        hp = tuple(self.fight.word(n + 'HP') for n in ('wBattleMon', 'wEnemyMon'))
        if hp.count(0) == 2:
            raise UnsupportedFight('Double faint is outside the terminal projection')
        for entry in profile.TERMINAL_ENTRIES:
            event = self.call(entry)
            if entry != 'ResolveFaints.check_battle_over' and event is not None:
                raise UnsupportedFight('Unexpected terminal event during party synchronization')
        if 0 not in hp:
            if event is not None:
                raise UnsupportedFight('Unexpected terminal event with two surviving actors')
            return None
        if event not in profile.TERMINAL_EVENTS:
            raise UnsupportedFight('No terminal event for one-mon residual faint')
        self.fight.terminal = True
        self.fight.truncated = False
        self.fight.truncation_reason = None
        self.fight.winner = 0 if event == 'WinTrainerBattle' else 1
        return event
