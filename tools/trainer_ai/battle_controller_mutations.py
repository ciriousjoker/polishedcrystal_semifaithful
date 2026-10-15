"""Serial real-Assembly controller regeneration and full-fight update checks.

Run only with exclusive ownership of the project's single CPU execution slot.
Every updated game has one exact reviewed delta and its own native artifact.
"""
import argparse
from dataclasses import replace
import hashlib
import json
import pathlib
import shutil

from battle_actions import SwitchAction, ItemAction
from battle_controller import ControllerMachine
from battle_controller_check import Oracle
from battle_controller_fight import ControllerFight
from battle_controller_fight_check import RecordedRun, first_attack, first_replacement
import battle_controller_profile as profile
from battle_controller_updates import BoundedUpdateAudit, build_update, MUTATIONS, TARGET
from battle_scenarios import Scenario
import stateful_lifting


def sha(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def rejected(operation, message):
    try:
        operation()
    except (ValueError, FileNotFoundError):
        return
    raise AssertionError(message)


def probe(root, machine, mutation, updated):
    """Actual changed data/rule behavior, independently checked against ROM."""
    oracle = Oracle(root, machine)
    machine.segment_checker, machine.single_step_checker = oracle.compare, oracle.single_step
    scenario = Scenario(inventory=(None, None))
    if mutation == 'burn-toxic-rule':
        status = 1 << machine._constant('constants/battle_constants.asm', 'BRN')
        scenario = replace(scenario, player=(replace(scenario.player[0], status=status),),
                           enemy=(scenario.enemy[0],))
    fight = ControllerFight(machine, chooser=first_attack, replacement_chooser=first_replacement)
    fight.reset(scenario, 193)
    if mutation == 'burn-toxic-rule':
        before = machine.word('wBattleMonHP')
        maximum = machine.word('wBattleMonMaxHP')
        machine.call('HandleBetweenTurnEffects')
        after = machine.word('wBattleMonHP')
        expected = max(1, maximum // (8 if updated else 16))
        assert before - after == expected, (before, after, maximum, expected)
        result = dict(rule='actual-full-endturn-burn', hp_before=before, hp_after=after,
                      maximum=maximum, expected_damage=expected)
    else:
        machine._put('hBattleTurn', 0)
        machine._put('wCurPlayerMove', machine._constant('constants/move_constants.asm', 'TACKLE'))
        machine.call('UpdateMoveData')
        power = machine.byte('wPlayerMoveStructPower')
        expected = 55 if updated and mutation == 'move-power' else 40
        assert power == expected, (power, expected)
        result = dict(rule='actual-source-UpdateMoveData', power=power,
                      hBattleTurn=machine.address('hBattleTurn'))
    result['comparisons'] = list(oracle.rows)
    oracle.oracle.oracle_close()
    machine.segment_checker = machine.single_step_checker = None
    return result


def full_fight(root, machine, destination, mutation):
    oracle = Oracle(root, machine)
    machine.segment_checker, machine.single_step_checker = oracle.compare, oracle.single_step
    recording = RecordedRun(oracle)
    fight = ControllerFight(machine, chooser=first_attack, replacement_chooser=first_replacement)
    fight._old_segment_checker, fight._old_single_step_checker = recording.capture_segment, recording.capture_one
    scenario = Scenario(inventory=('SUPER_POTION', None))
    scenario = replace(scenario, enemy=(replace(scenario.enemy[0], hp=10), *scenario.enemy[1:]))
    if mutation == 'burn-toxic-rule':
        scenario = replace(scenario, player=(replace(scenario.player[0], status=1 << machine._constant('constants/battle_constants.asm', 'BRN')),
                                             *scenario.player[1:]))
    def selected():
        actions = list(fight.choose())
        if machine.turn == 0:
            actions[1] = ItemAction(0)
            if mutation != 'burn-toxic-rule':
                actions[0] = SwitchAction(1)
        return tuple(actions)
    fight.reset(scenario, 193)
    while not fight.terminal and not fight.truncated:
        fight.step(selected())
    assert fight.terminal and not fight.truncated and not machine.failed
    result = fight.replay_record()
    result['recorded_segments'] = recording.rows
    result['comparisons'] = list(oracle.rows)
    result['controller_audit'] = machine.audit_configuration
    destination.write_text(json.dumps(result, indent=2) + '\n')
    fight._old_segment_checker, fight._old_single_step_checker = recording.replay_segment, recording.replay_one
    fight.reset(scenario, 193)
    while not fight.terminal and not fight.truncated:
        fight.step(selected())
    assert fight.terminal and recording.position == len(recording.rows)
    replay = fight.replay_record()
    assert replay['turns'] == result['turns'] and replay['winner'] == result['winner']
    oracle.oracle.oracle_close()
    machine.segment_checker = machine.single_step_checker = None
    return dict(turns=machine.turn, winner=fight.winner, comparisons=len(result['comparisons']),
                recorded_native_replay_equal=True, evidence=str(destination))


def negative_guards(audit, rom, symbols, candidate, receipt, mutation, baseline):
    """Six actual-byte mutation guards, restoring and rechecking every input."""
    def changed(path, content, label):
        path = pathlib.Path(path)
        original = path.read_bytes()
        try:
            path.write_bytes(content)
            rejected(lambda: audit.verify(rom, symbols, candidate), label + ' accepted')
        finally:
            path.write_bytes(original)
        audit.verify(rom, symbols, candidate)
    raw = bytearray(rom.read_bytes())
    raw[0x100] ^= 1
    changed(rom, raw, 'unsupported ROM instruction')
    core = candidate / 'engine/battle/core.asm'
    changed(core, core.read_bytes() + b'\n; unknown source delta guard\n', 'unknown source delta')
    rendering = candidate / 'home/gfx.asm'
    before = rendering.read_bytes()
    assert before.count(b'Copy2bpp::') == 1
    changed(rendering, before.replace(b'Copy2bpp::', b'Copy2bpp::\n\tnop'), 'changed presentation instructions')
    source_path = MUTATIONS[mutation][0]
    changed(candidate / source_path, (baseline / source_path).read_bytes(), 'source/ROM mismatch')
    record = json.loads(receipt.read_text())
    record['effective_defines'] = ['FAITHFUL']
    changed(receipt, json.dumps(record).encode(), 'invalid build receipt')
    log = pathlib.Path(json.loads(receipt.read_text())['build_log_path'])
    changed(log, log.read_bytes() + b'\nrgbasm -D FAITHFUL -o main.o main.asm\n', 'changed retained build log')
    return ['unsupported-rom-instruction', 'unknown-source-delta', 'presentation-instruction-change',
            'source-rom-mismatch', 'invalid-receipt', 'changed-build-log']


def run(root, mutations):
    baseline = root / 'build/stateful/game'
    output = root / 'build/battle-controller/source-updates'
    output.mkdir(parents=True, exist_ok=True)
    base_rom, base_sym = baseline / TARGET, (baseline / TARGET).with_suffix('.sym')
    baseline_machine = ControllerMachine(base_rom, base_sym, baseline, root / 'build/battle-controller/native')
    rows = []
    for mutation in mutations:
        folder = output / mutation
        candidate, native = folder / 'game', folder / 'native'
        folder.mkdir(parents=True, exist_ok=True)
        receipt = folder / 'build-receipt.json'
        if not candidate.exists():
            shutil.copytree(baseline, candidate)
        if not receipt.exists():
            receipt = build_update(baseline, candidate, mutation, folder)
        audit = BoundedUpdateAudit(baseline, receipt)
        rom, symbols = candidate / TARGET, (candidate / TARGET).with_suffix('.sym')
        audit.verify(rom, symbols, candidate)
        # Tackle data is an existing narrow allowed parameter in the older
        # frozen profile. Executable and layout changes must still reject.
        default_data_parameter_allowed = mutation == 'move-power'
        if default_data_parameter_allowed:
            profile.verify_audit(rom, symbols, candidate)
        else:
            rejected(lambda: profile.verify_audit(rom, symbols, candidate), 'Default frozen audit accepted new game')
        rejected(lambda: stateful_lifting.load_verified(root / 'build/battle-controller/native', rom, symbols,
            candidate, baseline_machine.entries, profile.BOUNDARIES,
            tuple(baseline_machine.manifest.get('excluded', {})),
            write_only_mmio=profile.WRITE_ONLY_MMIO, call_abis=profile.CALL_ABIS,
            optimization='O0', banked_wram=True, lcd_disabled=True), 'Stale original native artifact accepted')
        try:
            machine = ControllerMachine(rom, symbols, candidate, native, audit=audit)
        except (ValueError, FileNotFoundError):
            profile.build(rom, symbols, candidate, native, audit=audit)
            machine = ControllerMachine(rom, symbols, candidate, native, audit=audit)
        rejected(lambda: ControllerMachine(rom, symbols, candidate, native),
                 'Runtime accepted an artifact with the wrong audit configuration')
        old = probe(root, baseline_machine, mutation, False)
        new = probe(root, machine, mutation, True)
        if mutation == 'hram-layout':
            assert old['hBattleTurn'] != new['hBattleTurn']
        elif mutation == 'move-power':
            assert old['power'] != new['power']
        else:
            assert old['expected_damage'] != new['expected_damage']
        guards = negative_guards(audit, rom, symbols, candidate, receipt, mutation, baseline)
        whole = full_fight(root, machine, folder / 'whole-fight.json', mutation)
        row = dict(mutation=mutation, passed=True, baseline_probe=old, updated_probe=new,
                   stale_native_and_audit_configuration_rejected=True,
                   default_data_parameter_allowed=default_data_parameter_allowed,
                   negative_guards=guards, restored_inputs_reverified=True,
                   whole_fight=whole, receipt_sha256=sha(receipt),
                   native_manifest_sha256=sha(native/'manifest.json'), controller_audit=audit.configuration())
        (folder/'report.json').write_text(json.dumps(row, indent=2)+'\n')
        rows.append(row)
        print(json.dumps(dict(mutation=mutation,passed=True,whole_fight=whole)), flush=True)
    (output/'report.json').write_text(json.dumps(dict(passed=True,cases=rows,
        generator_sha256=stateful_lifting.MODULE_DIGEST, fixture_sha256=sha(__file__)),indent=2)+'\n')


if __name__ == '__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root',required=True,type=pathlib.Path)
    parser.add_argument('--mutations',nargs='+',choices=tuple(MUTATIONS),default=list(MUTATIONS))
    args=parser.parse_args()
    run(args.artifact_root,args.mutations)
