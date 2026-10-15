"""Bounded source-backed controller updates, never a general audit bypass.

This module does not rebuild on import. Assembly and native compilation must
only run while its caller holds the project's single CPU execution slot.
"""
import hashlib
import json
import os
from pathlib import Path
import shlex
import subprocess

import battle_actions_profile
import stateful_lifting


TARGET = 'polishedcrystal-3.2.3.gbc'
MUTATIONS = {
    'move-power': (
        'data/moves/moves.asm',
        'move TACKLE,          EFFECT_NORMAL_HIT,         40,',
        'move TACKLE,          EFFECT_NORMAL_HIT,         55,', 'main.o'),
    'burn-toxic-rule': (
        'engine/battle/endturn.asm',
        '\tcall nz, GetSixteenthMaxHP\n',
        '\tcall nz, GetEighthMaxHP\n', 'main.o'),
    'hram-layout': (
        'ram/hram.asm',
        'hBattleTurn:: db\nhBattlePalFadeMode:: db',
        'hBattlePalFadeMode:: db\nhBattleTurn:: db', 'ram.o'),
}


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def source_hashes(root):
    root = Path(root)
    return {name: digest(root / name) for name in stateful_lifting.source_paths()}


def expected_sources(baseline, mutation):
    """Describe exactly one reviewed source delta against an audited baseline."""
    if mutation not in MUTATIONS:
        raise ValueError('Unknown controller update mutation')
    path, before, after, _ = MUTATIONS[mutation]
    content = (Path(baseline) / path).read_text()
    if content.count(before) != 1:
        raise ValueError('Mutation source anchor is missing or ambiguous: ' + path)
    updated = content.replace(before, after)
    expected = source_hashes(baseline)
    expected[path] = hashlib.sha256(updated.encode()).hexdigest()
    return expected, updated


def build_update(baseline, candidate, mutation, output):
    """Incrementally assemble an already privately copied audited source cache.

    The caller creates the private cache once. All game inputs and inherited
    object files must initially equal the frozen cache; reused mutated caches
    require verification with their existing receipt instead of a second edit.
    """
    baseline, candidate, output = map(Path, (baseline, candidate, output))
    if baseline.resolve() == candidate.resolve():
        raise ValueError('The frozen game cache cannot be modified')
    battle_actions_profile.verify_audit(baseline / TARGET,
        (baseline / TARGET).with_suffix('.sym'), baseline)
    original = source_hashes(baseline)
    if source_hashes(candidate) != original:
        raise ValueError('Private cache is not an unchanged baseline snapshot')
    # Do not inherit modified or stale game objects from an unrelated build.
    objects = sorted(path.relative_to(baseline) for path in baseline.glob('**/*.o')
                     if 'tools' not in path.relative_to(baseline).parts)
    if not objects or any(not (candidate / path).is_file() or
                          digest(candidate / path) != digest(baseline / path)
                          for path in objects):
        raise ValueError('Inherited game objects do not match the audited cache')
    expected, updated = expected_sources(baseline, mutation)
    path, _, _, changed_object = MUTATIONS[mutation]
    (candidate / path).write_text(updated)
    (candidate / changed_object).unlink(missing_ok=True)
    (candidate / TARGET).unlink(missing_ok=True)
    output.mkdir(parents=True, exist_ok=True)
    log_path = output / 'assemble.log'
    argv = ['make', '-j1', TARGET]
    env = dict(os.environ)
    for name in ('MAKEFLAGS', 'GNUMAKEFLAGS', 'MFLAGS', 'MAKEOVERRIDES'):
        env.pop(name, None)
    with log_path.open('w') as log:
        subprocess.run(argv, cwd=candidate, env=env, stdout=log,
                       stderr=subprocess.STDOUT, check=True)
    if source_hashes(candidate) != expected:
        raise ValueError('Game inputs changed during Assembly build')
    invocations = []
    links = []
    for raw in log_path.read_text().splitlines():
        try:
            command = shlex.split(raw)
        except ValueError:
            continue
        if command and Path(command[0]).name == 'rgbasm':
            invocations.append(command)
        elif command and Path(command[0]).name == 'rgblink':
            links.append(command)
    if not invocations or not links or not any(
            '-o' in command and command[command.index('-o') + 1] == changed_object
            for command in invocations):
        raise ValueError('Build log does not show the changed object assembled and ROM linked')
    if any(arg.startswith('-D') for command in invocations for arg in command[1:]):
        raise ValueError('Unexpected build define in default controller update')
    rom, symbols = candidate / TARGET, (candidate / TARGET).with_suffix('.sym')
    receipt = dict(schema=1, target=TARGET, mutation=mutation,
        make_argv=argv, assembler_invocations=invocations,
        linker_invocations=links, effective_defines=[],
        baseline_rom_sha256=digest(baseline / TARGET),
        baseline_symbols_sha256=digest((baseline / TARGET).with_suffix('.sym')),
        rom_sha256=digest(rom), symbols_sha256=digest(symbols),
        sources=expected, build_log_path=str(log_path.resolve()),
        build_log_sha256=digest(log_path))
    receipt_path = output / 'build-receipt.json'
    receipt_path.write_text(json.dumps(receipt, indent=2) + '\n')
    return receipt_path


class BoundedUpdateAudit:
    """Explicit audit object passed into controller build and runtime loaders.

    A trusted runner receipt binds actual make output to exact source and raw
    artifacts. Arbitrary updates, altered presentation, selection ABIs and build
    configuration remain rejected. New evidence must be collected after build.
    """
    def __init__(self, baseline, receipt):
        self.baseline, self.receipt = Path(baseline), Path(receipt)

    def configuration(self):
        receipt = json.loads(self.receipt.read_text())
        if receipt.get('mutation') not in MUTATIONS:
            raise ValueError('Unknown controller update mutation')
        return dict(kind='bounded-source-mutation-v1', mutation=receipt['mutation'],
                    receipt_sha256=digest(self.receipt))

    def verify(self, rom, symbols, source_root):
        baseline = self.baseline
        battle_actions_profile.verify_audit(baseline / TARGET,
            (baseline / TARGET).with_suffix('.sym'), baseline)
        receipt = json.loads(self.receipt.read_text())
        if receipt.get('schema') != 1 or receipt.get('target') != TARGET:
            raise ValueError('Unsupported controller update receipt')
        if receipt.get('make_argv') != ['make', '-j1', TARGET] or receipt.get('effective_defines') != []:
            raise ValueError('Controller update build configuration mismatch')
        for key, path in (('baseline_rom_sha256', baseline / TARGET),
                          ('baseline_symbols_sha256', (baseline / TARGET).with_suffix('.sym')),
                          ('rom_sha256', rom), ('symbols_sha256', symbols),
                          ('build_log_sha256', receipt['build_log_path'])):
            if receipt.get(key) != digest(path):
                raise ValueError('Controller update receipt bytes mismatch: ' + key)
        expected, _ = expected_sources(baseline, receipt.get('mutation'))
        if receipt.get('sources') != expected or source_hashes(source_root) != expected:
            raise ValueError('Controller sources changed beyond the reviewed mutation')
        invocations, links = receipt.get('assembler_invocations'), receipt.get('linker_invocations')
        if not isinstance(invocations, list) or not invocations or not isinstance(links, list) or not links:
            raise ValueError('Missing actual assembler or linker invocation')
        for command in invocations:
            if not isinstance(command, list) or not command or not all(isinstance(arg, str) for arg in command):
                raise ValueError('Malformed assembler invocation')
            if Path(command[0]).name != 'rgbasm' or any(arg.startswith('-D') for arg in command[1:]):
                raise ValueError('Unsupported assembler invocation')
        for command in links:
            if not isinstance(command, list) or not command or not all(isinstance(arg, str) for arg in command):
                raise ValueError('Malformed linker invocation')
            if Path(command[0]).name != 'rgblink' or '-o' not in command or command[command.index('-o') + 1] != TARGET:
                raise ValueError('Unsupported linker invocation')
        logged_commands = []
        for raw in Path(receipt['build_log_path']).read_text().splitlines():
            try:
                command = shlex.split(raw)
            except ValueError:
                continue
            if command and Path(command[0]).name in ('rgbasm', 'rgblink'):
                logged_commands.append(command)
        if [command for command in logged_commands if Path(command[0]).name == 'rgbasm'] != invocations or [command for command in logged_commands if Path(command[0]).name == 'rgblink'] != links:
            raise ValueError('Receipt invocations disagree with retained actual build log')
        changed_object = MUTATIONS[receipt['mutation']][3]
        if not any('-o' in command and command[command.index('-o') + 1] == changed_object for command in invocations):
            raise ValueError('Changed source object was not assembled')
        syms = stateful_lifting.symbols(symbols)
        before_syms = stateful_lifting.symbols((baseline / TARGET).with_suffix('.sym'))
        if receipt['mutation'] == 'hram-layout':
            if syms['hBattleTurn'] != before_syms['hBattlePalFadeMode'] or syms['hBattlePalFadeMode'] != before_syms['hBattleTurn']:
                raise ValueError('Expected HRAM symbol relocation is absent')
        return receipt
