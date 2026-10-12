#!/usr/bin/env python3
"""Prepare uncompressed canonical poses from the game's own avatar inventory."""
import argparse
from pathlib import Path
import re

REPO = Path(__file__).resolve().parents[2]


def inventory():
    constants, next_id = {}, 0
    for line in (REPO / 'constants/sprite_constants.asm').read_text().splitlines():
        code = line.partition(';')[0].strip()
        if code.startswith(('const_def', 'const_next ')):
            value = code.split()[1:] or ['0']
            next_id = int(value[0].replace('$', '0x'), 0)
        match = re.fullmatch(r'const (SPRITE_\w+)', code)
        if match:
            constants[match[1]] = next_id
            next_id += 1
    headers = re.findall(r'^\s*overworld_sprite (\w+),',
                         (REPO / 'data/sprites/sprites.asm').read_text(), re.M)
    graphics = dict(re.findall(r'(\w+):: INCBIN "([^"]+\.2bpp)\.lzp"',
                               (REPO / 'gfx/sprites.asm').read_text()))
    sprites = re.findall(r'^\s*db (SPRITE_\w+)',
                         (REPO / 'data/player/state_sprites.asm').read_text(), re.M)
    rows = []
    for sprite in sprites:
        index = constants[sprite]
        if index == 0 or index - 1 >= len(headers):
            raise ValueError(f'Unsupported canonical sprite: {sprite}')
        rows.append((sprite, index, graphics[headers[index - 1]]))
    if not rows:
        raise ValueError('Empty canonical avatar inventory')
    return rows


def source(rows):
    paths = list(dict.fromkeys(row[2] for row in rows))
    lines = ['; Generated from native avatar definitions. Do not edit.',
             'SECTION "Mobile Virtual Avatar Identities", ROMX, BANK[MOBILE_ROM_BANK]',
             'MobileVirtualAssetSprites::']
    for sprite, index, _ in rows:
        lines += [f'\tASSERT {sprite} == {index}', f'\tdb {sprite}']
    lines += ['MobileVirtualAssetSpritesEnd::',
              '\tASSERT @ - MobileVirtualAssetSprites == NUM_PLAYER_GENDERS * NUM_PLAYER_STATES',
              'SECTION "Mobile Virtual Avatar Assets", ROMX, BANK[MOBILE_ROM_BANK + 1]',
              'MobileVirtualAssetStart::', 'MobileVirtualAssetPointers::']
    for _, _, path in rows:
        lines += [f'\tdw MobileVirtualAsset{paths.index(path)}']
    for index, path in enumerate(paths):
        if (REPO / path).stat().st_size != 384:
            raise ValueError(f'Canonical walking sprite is not 24 tiles: {path}')
        lines += [f'MobileVirtualAsset{index}:', f'\tINCBIN "{path}"',
                  f'\tASSERT @ - MobileVirtualAsset{index} == 24 tiles']
    lines += ['MobileVirtualAssetEnd::', '']
    return '\n'.join(lines)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--dependencies', action='store_true')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    rows = inventory()
    if args.dependencies:
        print(' '.join(dict.fromkeys(row[2] for row in rows)))
    else:
        if args.output is None:
            parser.error('--output is required')
        value = source(rows)
        args.output.parent.mkdir(parents=True, exist_ok=True)
        if not args.output.exists() or args.output.read_text() != value:
            args.output.write_text(value)


if __name__ == '__main__':
    main()
