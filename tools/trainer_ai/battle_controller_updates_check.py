"""Negative audit checks for an already assembled bounded controller update.

Run after actual build_update. This file neither fabricates a build receipt nor
claims full-fight parity; that separate differential run remains mandatory.
"""
import argparse
import json
from pathlib import Path

from battle_controller_updates import BoundedUpdateAudit, TARGET


def rejections(baseline, candidate, receipt_path, output):
    baseline, candidate, receipt_path = map(Path, (baseline, candidate, receipt_path))
    audit = BoundedUpdateAudit(baseline, receipt_path)
    rom, symbols = candidate / TARGET, (candidate / TARGET).with_suffix('.sym')
    audit.verify(rom, symbols, candidate)
    rows = []

    def reject(name, path, changed):
        original = path.read_bytes()
        try:
            path.write_bytes(changed)
            try:
                audit.verify(rom, symbols, candidate)
            except (ValueError, KeyError) as error:
                rows.append(dict(name=name, rejected=True, reason=str(error)))
            else:
                raise AssertionError('Unsupported update accepted: ' + name)
        finally:
            path.write_bytes(original)

    source = candidate / 'engine/battle/core.asm'
    original = source.read_bytes()
    needle = b'\tcp HELD_SHED_SHELL\n'
    if needle not in original:
        raise AssertionError('Unknown-source rejection anchor is missing')
    reject('unknown-combat-source', source,
           original.replace(needle, b'\tcp HELD_AIR_BALLOON\n', 1))
    presentation = candidate / 'home/delay.asm'
    reject('changed-presentation-source', presentation,
           presentation.read_bytes() + b'\n; Unreviewed presentation change.\n')
    # Mutate real executable bytes after the receipt; do not rewrite its hashes.
    import stateful_lifting
    bank, pc = stateful_lifting.symbols(symbols)['StdBattleTextbox']
    offset = pc if pc < 0x4000 else bank * 0x4000 + pc - 0x4000
    altered = bytearray(rom.read_bytes())
    altered[offset] ^= 1
    reject('changed-renderer-rom-after-build', rom, altered)
    source = candidate / 'data/moves/moves.asm'
    reject('source-rom-mismatch', source, source.read_bytes() + b'\n; Stale ROM.\n')
    altered_receipt = json.loads(receipt_path.read_text())
    altered_receipt['mutation'] = 'unknown-unreviewed-rule'
    reject('unknown-mutation-receipt', receipt_path,
           (json.dumps(altered_receipt) + '\n').encode())
    altered_receipt = json.loads(receipt_path.read_text())
    altered_receipt['effective_defines'] = ['FAITHFUL']
    reject('unsupported-build-define', receipt_path,
           (json.dumps(altered_receipt) + '\n').encode())
    audit.verify(rom, symbols, candidate)
    result = dict(scope='audit-negative-checks-only', configuration=audit.configuration(),
                  rejected=len(rows), cases=rows, restored_candidate_verified=True)
    Path(output).write_text(json.dumps(result, indent=2) + '\n')
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('baseline', 'candidate', 'receipt', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    args = parser.parse_args()
    print(json.dumps(rejections(args.baseline, args.candidate, args.receipt, args.output)))
