"""Two bounded whole-fight proofs for scoped ability display projection.

Requires an already rebuilt O3 controller. Same-hook mGBA comparisons remain
conditional on the presentation model; ordinary frontend proof is separate.
"""
import argparse
import copy
import hashlib
import json
import pathlib
import tempfile
import battle_controller_profile as profile

from battle_actions import SwitchAction
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
from battle_env import MoveAction, UnsupportedFight
from battle_scenarios import Pokemon, Scenario


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def require(value, message):
    if not value:
        raise UnsupportedFight(message)


def snapshot(m):
    return dict(player_ability=m.byte('wPlayerAbility'), enemy_ability=m.byte('wEnemyAbility'),
        player_attack=m.byte('wPlayerAtkLevel'), enemy_attack=m.byte('wEnemyAtkLevel'),
        ability_mode=m.byte('wInAbility'), player=m.byte('wCurBattleMon'),
        enemy_hp=m.word('wEnemyMonHP'))


def scenario(m, case):
    if case == 'trace-conversion-intimidate':
        tackle = m._constant('constants/move_constants.asm', 'TACKLE')
        conversion = m._constant('constants/move_constants.asm', 'CONVERSION')
        return Scenario(player=(Pokemon('PORYGON', ability_slot=0,
            moves=('CONVERSION', 'TACKLE', 'WATER_GUN'),
            pp=(m.move_data(conversion)[5], m.move_data(tackle)[5], 0)),),
            enemy=(Pokemon('GYARADOS', ability_slot=0, moves=('SPLASH', 'TACKLE')),),
            inventory=(None, None))
    return Scenario(player=(Pokemon('GYARADOS', ability_slot=0, moves=('TACKLE',)),
        Pokemon('PSYDUCK', moves=('SPLASH',))),
        enemy=(Pokemon('SNORLAX', moves=('SPLASH', 'TACKLE')),), inventory=(None, None))


def choose(fight, case):
    selected = [first_attack(side, fight.observation(side), fight.legal_actions(side))
                for side in (0, 1)]
    if case == 'trace-conversion-intimidate':
        desired = MoveAction(0 if fight.turn == 0 else 1)
        if desired in fight.legal_actions(0):
            selected[0] = desired
    elif fight.turn < 2:
        desired = SwitchAction(1 if fight.turn == 0 else 0)
        require(desired in fight.legal_actions(0), 'Required switch/reentry is not legal')
        selected[0] = desired
    return tuple(selected)


def run_case(m, oracle, case, output):
    callbacks = []
    display = []
    captured = []
    initial_wram = []
    initial_wram_path = output / (case + '.initial-wram.bin')
    fight = None
    def conversion_policy(side, observation, actions):
        require(side == 0 and MoveAction(2) in actions, 'Missing zero-PP Water type choice')
        callbacks.append(dict(side=side, choice=2))
        return MoveAction(2)
    def observer(stage, row):
        if stage == 'boundary' and row['name'] in (
                'PerformAbilityGFX', 'PerformAbilityReplacementGFX'):
            display.append(dict(name=row['name'], turn=row['turn'], state=snapshot(m)))
            if case == 'trace-conversion-intimidate' and not captured:
                raw = output / 'first-ability-wrapper-entry.bin'
                raw.write_bytes(bytes(m.state))
                metadata = dict(state_sha256=sha(raw), rom=str(m.rom_path),
                    symbols=str(m.symbols_path), rom_sha256=sha(m.rom_path),
                    symbols_sha256=sha(m.symbols_path), banked_wram=True,
                    entry=row['name'], battle_turn=m.byte('hBattleTurn'),
                    snapshot_boundary='first-ability-wrapper-before-first-policy-decision',
                    decision_boundary=fight.decision_boundary)
                (output / 'first-ability-wrapper-entry.json').write_text(json.dumps(metadata, indent=2) + '\n')
                captured.append(True)
    m.segment_checker, m.single_step_checker = oracle.compare, oracle.single_step
    fight = ControllerFight(m, chooser=first_attack, replacement_chooser=first_replacement,
        conversion_chooser=conversion_policy, observer=observer, max_turns=128)
    recording = RecordedRun(oracle)
    fight._old_segment_checker = recording.capture_segment
    fight._old_single_step_checker = recording.capture_one
    base = scenario(m, case)
    def execute():
        fight.reset(base, 193)
        wram = bytes(m.state.m[0xc000:0xe000])
        if initial_wram:
            require(wram == initial_wram[0], 'Post-reset initial WRAM replay diverged')
        else:
            initial_wram.append(wram)
            initial_wram_path.write_bytes(wram)
        states = [snapshot(m)]
        intimidate = m._constant('constants/ability_constants.asm', 'INTIMIDATE')
        require(states[0]['enemy_attack'] == 6, 'Initial Intimidate did not lower Attack')
        if case == 'trace-conversion-intimidate':
            require(states[0]['player_ability'] == intimidate and states[0]['player_attack'] == 6,
                'Trace cache replacement or replayed Intimidate effect missing')
            require(any(row['name'] == 'PerformAbilityReplacementGFX' for row in display),
                'Trace replacement display was not reached')
        while not fight.terminal and not fight.truncated:
            fight.step(choose(fight, case))
            states.append(snapshot(m))
            require(m.byte('wInAbility') == 0, 'Ability mode leaked past a completed turn')
            if case == 'trace-conversion-intimidate' and fight.turn == 1:
                water = m._constant('constants/type_constants.asm', 'WATER')
                active = next(p for p in fight.observation(0)['teams'][0] if p['active'])
                require(active['types'] == (water, water) and active['pp'][2] == 0,
                    'Trace fixture lost actual zero-PP-target Conversion semantics')
        require(fight.terminal and not fight.truncated and not m.failed,
            'Ability case did not complete its ordinary trainer fight')
        require(states[0]['ability_mode'] == 0 and states[-1]['ability_mode'] == 0,
            'Source EndAbility did not restore ability mode')
        if case == 'intimidate-switch-reentry':
            require(len(states) > 3 and states[1]['player'] == 1 and states[2]['player'] == 0,
                'Requested switch and reentry did not occur')
            require(states[2]['enemy_attack'] == 5, 'Intimidate reentry did not lower Attack again')
            require(states[3]['enemy_hp'] < states[2]['enemy_hp'],
                'Post-reentry physical attack did not damage the opponent')
        else:
            require(len(callbacks) == 1, 'Expected exactly one actual Conversion callback')
        return states, fight.replay_record()
    states, record = execute()
    initial_display, initial_callbacks = copy.deepcopy(display), copy.deepcopy(callbacks)
    record.update(case=case, snapshots=states, displays=initial_display,
        callbacks=initial_callbacks, recorded_segments=recording.rows,
        initial_wram=str(initial_wram_path), initial_wram_sha256=sha(initial_wram_path),
        initial_wram_boundary='after-actual-ControllerFight.reset-before-first-actor-action')
    destination = output / (case + '.json')
    destination.write_text(json.dumps(record, indent=2) + '\n')
    display.clear(); callbacks.clear()
    fight._old_segment_checker, fight._old_single_step_checker = recording.replay_segment, recording.replay_one
    replay_states, replay = execute()
    require(recording.position == len(recording.rows) and replay_states == states and
        display == initial_display and callbacks == initial_callbacks and
        replay['turns'] == record['turns'] and replay['winner'] == record['winner'],
        'Ability exact native replay diverged')
    result = dict(case=case, passed=True, turns=fight.turn, comparisons=len(recording.rows),
        replay_equal=True, evidence=str(destination), evidence_sha256=sha(destination),
        initial_wram=str(initial_wram_path), initial_wram_sha256=sha(initial_wram_path))
    print(json.dumps(result), flush=True)
    return result


def run(root, output):
    game = root / 'build/stateful/game'
    source = pathlib.Path(__import__('battle_controller').__file__).parent
    dependencies = ('battle_controller.py', 'battle_controller_profile.py',
        'battle_controller_fight.py', 'battle_controller_fight_check.py',
        'battle_controller_check.py', 'battle_controller_oracle.c', 'battle_scenarios.py',
        'battle_decisions.py', 'battle_env.py', 'stateful_lifting.py',
        'battle_bag.py', 'battle_actions.py', 'stateful_fixture_check.py',
        'battle_env_check.py', 'battle_coverage.py')
    sources = {name: sha(source / name) for name in dependencies}
    m = ControllerMachine(game / 'polishedcrystal-3.2.3.gbc',
        game / 'polishedcrystal-3.2.3.sym', game, root / 'build/battle-controller/native-o3',
        optimization='O3')
    guarded = ('engine/battle/ability_gfx.asm', 'engine/battle/abilities.asm',
        'engine/pokemon/abilities.asm', 'engine/gfx/vwf.asm', 'home/vwf.asm',
        'home/farcall.asm', 'constants/charmap.asm')
    with tempfile.TemporaryDirectory(prefix='ability-guard-') as directory:
        private = pathlib.Path(directory)
        for name in guarded:
            target = private / name
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes((game / name).read_bytes())
        changed = private / 'engine/gfx/vwf.asm'
        changed.write_bytes(changed.read_bytes() + b'\n; unsupported source change\n')
        try:
            profile.config({}, private)
        except ValueError as error:
            require(str(error) == 'Ability presentation source requires review: engine/gfx/vwf.asm',
                'Changed wrapper source did not fail at its exact source guard')
        else:
            raise UnsupportedFight('Unsupported wrapper source silently admitted')
    oracle = Oracle(root, m)
    output.mkdir(parents=True, exist_ok=True)
    try:
        cases = [run_case(m, oracle, case, output) for case in
                 ('trace-conversion-intimidate', 'intimidate-switch-reentry')]
    finally:
        oracle.oracle.oracle_close()
    require(sources == {name: sha(source / name) for name in dependencies},
            'Host source changed during ability proof')
    report = dict(passed=True, cases=cases, source_change_rejected=True, host_sources=sources, runner_sha256=sha(__file__),
        oracle_provenance=oracle.provenance, rom_sha256=sha(m.rom_path),
        symbols_sha256=sha(m.symbols_path), manifest_sha256=sha(m.build_dir / 'manifest.json'),
        projection_limits=['Only two named ability graphics wrappers are added to presentation.',
            'Independent mGBA segment checks use the same presentation boundaries.',
            'Ordinary frontend continuation remains required to validate the graphics projection.',
            'No memory regions are masked; scratch union contents are conditional on the hooks.'])
    (output / 'report.json').write_text(json.dumps(report, indent=2) + '\n')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    parser.add_argument('--output', type=pathlib.Path, required=True)
    args = parser.parse_args()
    run(args.artifact_root, args.output)
