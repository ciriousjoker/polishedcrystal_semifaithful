"""Conditional source switch operations under a separate rendering profile."""
import ctypes as C
import json
import pathlib

from battle_env import UnsupportedFight
from battle_env import NativeFight, MoveAction, Transition
from battle_status import TerminalLeaf
from battle_party_probe import fixture
from stateful_fixture_check import State
import battle_env_profile as base
import battle_party_profile as profile
import stateful_lifting


class SendInLeaf:
    def __init__(self, fight, build_dir):
        self.fight = fight
        base.verify_audit(fight.rom_path, fight.symbols_path, fight.source_root)
        manifest = json.loads((pathlib.Path(build_dir) / 'manifest.json').read_text())
        self.lib = stateful_lifting.load_verified(build_dir, fight.rom_path,
            fight.symbols_path, fight.source_root, profile.SENDIN_ENTRIES,
            profile.PRESENTATION, tuple(manifest.get('excluded', {})))
        self.lib.stateful_run.argtypes = [C.POINTER(State), C.c_uint32, C.c_uint16]

    def run_prepared(self, tape):
        s = self.fight.state
        if len(tape) > 4096:
            raise UnsupportedFight('Party RNG tape exceeds bound')
        s.event_count, s.event_pos = len(tape), 0
        s.events[:len(tape)] = tape
        events = []
        for _ in range(512):
            self.lib.stateful_run(C.byref(s), 200000, 65534)
            if not s.fault:
                return tuple(tape[:s.event_pos]), tuple(events)
            hook = next((name for name in profile.PRESENTATION
                         if self.fight.symbols[name][1] == s.pc and
                         (s.pc < 0x4000 or self.fight.symbols[name][0] == s.bank)), None)
            if s.fault != 5 or hook is None:
                raise UnsupportedFight(f'Send-in fault {s.fault} at {s.bank:02x}:{s.pc:04x}')
            if not 0xc000 <= s.sp < 0xdffe:
                raise UnsupportedFight('Invalid party presentation return stack')
            target = s.m[s.sp] | s.m[s.sp + 1] << 8
            if target >= 0x8000 and target != 65534:
                raise UnsupportedFight('Invalid party presentation return address')
            s.sp += 2
            s.pc, s.fault = target, 0
            events.append(hook)
        raise UnsupportedFight('Too many party presentation events')

    def call(self, entry):
        if entry not in profile.SENDIN_ENTRIES:
            raise UnsupportedFight('Unsupported send-in entry')
        self.fight.prepare_call(entry)
        used, events = self.run_prepared(self.fight.rng.preview())
        self.fight.rng.consume(len(used))
        return used, events


class PartyFight:
    """Fixed neutral 2v2 fixture with explicit host phase/choice orchestration.

    Moves/order, party synchronization, whole SendInUserPkmn, hazards, entry
    abilities and party-based terminal decisions execute actual source. This is
    not the complete ordinary endturn, faint animation or experience controller.
    """
    def __init__(self, rom, symbols, source_root, base_build, party_build,
                 terminal_build, seed=193, orientation=0, max_turns=64):
        self.fight = NativeFight(rom, symbols, source_root, base_build,
                                 seed, orientation, max_turns)
        self.leaf = SendInLeaf(self.fight, party_build)
        self.terminal_leaf = TerminalLeaf(self.fight, terminal_build)
        classes = [line.split()[1] for line in
                   (self.fight.source_root / 'constants/trainer_constants.asm').read_text().splitlines()
                   if line.strip().startswith('trainerclass ')]
        self.trainer_class = classes.index('YOUNGSTER')
        self._initialize_party()

    def _initialize_party(self):
        self.replacements = []
        fixture(self.fight)
        self.fight._put('wOtherTrainerClass', self.trainer_class)
        self.fight._put('wOtherTrainerID', 1)
        self.species_profiles = []
        for side in (0, 1):
            self._sendin(side, 0)
            stem = ('wBattleMon', 'wEnemyMon')[side]
            self.species_profiles.append(dict(
                types=tuple(self.fight.byte(stem + n) for n in ('Type1', 'Type2')),
                ability=self.fight.byte(('wPlayerAbility', 'wEnemyAbility')[side])))

    def reset(self, seed=193, orientation=None):
        """Reuse verified handles; source sends rebuild the closed party fixture."""
        self.fight.reset(seed, orientation)
        self._initialize_party()
        return self.observation(0)

    @property
    def terminal(self):
        return self.fight.terminal

    @property
    def truncated(self):
        return self.fight.truncated

    @property
    def winner(self):
        return self.fight.winner

    def _sendin(self, side, index):
        self.fight._put('hBattleTurn', side)
        self.fight._put(('wPlayerSwitchTarget', 'wEnemySwitchTarget')[side], index + 1)
        for entry in ('GetUserSwitchTarget', 'SendInUserPkmn', 'SpikesDamage', 'RunEntryAbilities'):
            self.leaf.call(entry)

    def reserves(self, side):
        if type(side) is not int or side not in (0, 1):
            raise UnsupportedFight('Invalid party side')
        f = self.fight
        active = f.byte(('wCurBattleMon', 'wCurOTMon')[side])
        party = ('wPartyMon', 'wOTPartyMon')[side]
        return tuple(index for index in range(2) if index != active
                     and f.word(party + str(index + 1) + 'HP')
                     and not f.state.m[f.address(party + str(index + 1) + 'Personality') + 1] & 0x40)

    def observation(self, side):
        if side not in (0, 1):
            raise UnsupportedFight('Invalid party observer')
        f = self.fight
        teams = []
        for actor, party in enumerate(('wPartyMon', 'wOTPartyMon')):
            members = []
            active = f.byte(('wCurBattleMon', 'wCurOTMon')[actor])
            for index in range(2):
                stem = ('wBattleMon', 'wEnemyMon')[actor] if index == active else party + str(index + 1)
                mon = dict(species=f.byte(stem + 'Species'), level=f.byte(stem + 'Level'),
                    active=index == active, item=f.byte(stem + 'Item'), status=f.byte(stem + 'Status'),
                    moves=tuple(f.state.m[f.address(stem + 'Moves') + j] for j in range(4)),
                    pp=tuple(f.state.m[f.address(stem + 'PP') + j] & 63 for j in range(4)),
                    **self.species_profiles[actor])
                mon.update({n: f.word(stem + n) for n in
                    ('HP', 'MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')})
                members.append(mon)
            teams.append(tuple(members))
        return dict(side=side, turn=f.turn, teams=tuple(teams),
                    terminal=f.terminal, truncated=f.truncated)

    def legal_actions(self, side):
        return self.fight.legal_actions(side)

    def _resolve(self, choose):
        event = None
        for entry in ('UpdateBattleMonInParty', 'UpdateEnemyMonInParty',
                      'ResolveFaints.check_battle_over'):
            event = self.terminal_leaf.call(entry)
        if event is not None:
            self.fight.terminal = True
            self.fight.winner = 0 if event == 'WinTrainerBattle' else 1
            return
        fainted = [side for side, stem in enumerate(('wBattleMon', 'wEnemyMon'))
                   if not self.fight.word(stem + 'HP')]
        if len(fainted) > 1:
            raise UnsupportedFight('Simultaneous replacement is outside this fixture')
        for side in fainted:
            legal = self.reserves(side)
            if not legal:
                raise UnsupportedFight('Source returned without terminal event or reserve')
            # Choice callback sees only a fresh public observation and legal indices.
            index = choose(side, self.observation(side), legal)
            if type(index) is not int or index not in legal:
                raise UnsupportedFight('Illegal replacement choice')
            previous = self.fight.byte(('wCurBattleMon', 'wCurOTMon')[side])
            self._sendin(side, index)
            self.replacements.append(dict(side=side, previous=previous, selected=index,
                                          turn=self.fight.turn))

    def step(self, actions, choose=None):
        f = self.fight
        if len(actions) != 2 or f.terminal or f.truncated or f.failed:
            raise UnsupportedFight('Invalid party turn boundary')
        if any(action not in self.legal_actions(side) for side, action in enumerate(actions)):
            raise UnsupportedFight('Illegal party move action')
        choose = choose or (lambda side, observation, legal: legal[0])
        try:
            f.commit_actions(actions)
            f._call('DetermineMoveOrder')
            first = 0 if f.state.f & 16 else 1
            events = []
            for side in (first, first ^ 1):
                opponent = ('wEnemyMon', 'wBattleMon')[side]
                stem = ('wBattleMon', 'wEnemyMon')[side]
                before = f.word(opponent + 'HP')
                f._put('hBattleTurn', side)
                f._put('wDamageTaken', 0, 2)
                used, presentation = f._call('DoTurn')
                hp = f.word(opponent + 'HP')
                slot = actions[side].slot
                events.append(Transition(side, slot, f.moves[slot], before - hp, hp,
                    f.state.m[f.address(stem + 'PP') + slot] & 63,
                    bool(f.byte('wAttackMissed')), used, presentation))
                if not hp:
                    break
            f.turn += 1
            self._resolve(choose)
            if not f.terminal:
                if any(not self.legal_actions(side) for side in (0, 1)):
                    f.truncated, f.truncation_reason = True, 'unsupported-struggle'
                elif f.turn >= f.max_turns:
                    f.truncated, f.truncation_reason = True, 'turn-limit'
            f.events.extend(events)
            return tuple(events)
        except Exception:
            f.failed = True
            raise
