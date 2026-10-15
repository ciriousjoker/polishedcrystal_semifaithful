"""Focused static-discovery regression for real farcall/farjp wrapper semantics.

Run the existing stateful fixture suite into a fresh controller cache separately
for actual-ROM CPU/RAM parity, including farcall and farjp execution.
"""
import argparse
import hashlib
import json
import pathlib
import stateful_lifting


def main(root):
    rows = []
    for far_jump in (False, True):
        image = bytearray(3 * 0x4000)
        image[0x10] = 0xc9
        image[0x0f] = 0xc9
        word = 0x4010 | (0x8000 if far_jump else 0)
        image[0x4000:0x4006] = bytes((0xd7, word & 255, word >> 8, 2, 0, 0xc9))
        image[0x8010] = 0xc9
        blocks, _ = stateful_lifting.compile_rom(image, [(1, 0x4000), (0, 0x0f)])
        continuation = (1 << 16 | 0x4004) in blocks
        assert continuation == (not far_jump)
        assert (2 << 16 | 0x4010) in blocks and 0x10 in blocks and 0x0f in blocks
        image[0x4004] = 0xd3  # Not an SM83 instruction, deliberately unreachable for farjp.
        if far_jump:
            stateful_lifting.compile_rom(image, [(1, 0x4000), (0, 0x0f)])
        else:
            try:
                stateful_lifting.compile_rom(image, [(1, 0x4000), (0, 0x0f)])
            except ValueError as error:
                assert str(error).startswith('Unsupported reachable instructions: ')
            else:
                raise AssertionError('Reachable farcall continuation was not decoded')
        rows.append(dict(farjp=far_jump, inline_continuation_discovered=continuation,
                         explicit_do_nothing_root=True, target_discovered=True))
    report = dict(cases=rows, generator_sha256=hashlib.sha256(
        pathlib.Path(stateful_lifting.__file__).read_bytes()).hexdigest())
    directory = root / 'build/battle-controller'
    directory.mkdir(parents=True, exist_ok=True)
    (directory / 'farjp-discovery.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--artifact-root', type=pathlib.Path, required=True)
    main(parser.parse_args().artifact_root)
