#!/usr/bin/env python3
"""Build and guard an isolated production MBC30 layout without editing sources."""
import argparse
import json
from pathlib import Path
import subprocess
from audit_rom_budget import layout, validate_mobile
from probe_emulator import digest, require

REPO = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cache', type=Path, default=Path('/Volumes/Storage/Cache/mobileadapter'))
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    args.output = args.output.resolve()
    require(not args.output.exists(), 'Choose a fresh MBC30 layout directory')
    args.output.mkdir(parents=True)
    originals = {p: digest(p) for p in REPO.glob('polishedcrystal*.gbc')}
    with (args.output / 'build.log').open('w') as log:
        subprocess.run(['make', '-j4', 'mobile', f'RGBDS={args.cache.resolve() / "rgbds-1.0.4"}/',
            f'MOBILE_OBJ_DIR={args.output / "objects"}', f'ROM_NAME={args.output / "expanded"}'],
            cwd=REPO, stdout=log, stderr=subprocess.STDOUT, check=True)
    rom = args.output / 'expanded.gbc'
    result = validate_mobile(rom.read_bytes(), layout(rom.with_suffix('.map').read_text()))
    require(all(digest(p) == expected for p, expected in originals.items()), 'Production ROM changed during isolated link')
    result.update({'layout_passed': True, 'production_postlink_passed': True,
                   'rom_sha256': digest(rom), 'scope': 'Production placement only; execute runtime and save checks separately.'})
    (args.output / 'evidence.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
