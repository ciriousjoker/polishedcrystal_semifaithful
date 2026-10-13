"""Generate bounded four-quad programs from native facing definitions."""
import re


def rectangle_programs(native):
    groups, labels, rows, count = [], [], [], None
    for line in native.splitlines():
        code = line.partition(';')[0].strip()
        label = re.fullmatch(r'(Facing\w+):', code)
        if label and label[1] == 'Facings':
            continue
        if label:
            if count is not None:
                if len(rows) != count:
                    raise ValueError('Native facing count differs from its body')
                groups.append((labels, count, rows))
                labels, rows, count = [], [], None
            labels.append(label[1])
        elif labels and code.startswith('db '):
            fields = [value.strip() for value in code[3:].split(',')]
            if count is None:
                if len(fields) != 1 or not fields[0].isdigit():
                    raise ValueError('Native facing count is no longer a literal')
                count = int(fields[0])
            else:
                if len(fields) != 4:
                    raise ValueError('Native facing quad format changed')
                rows.append(fields)
    if count is not None:
        if len(rows) != count:
            raise ValueError('Incomplete final native facing')
        groups.append((labels, count, rows))
    if not groups or groups[0][0][0] != 'FacingStepDown0':
        raise ValueError('Native facing body origin changed')

    lines = ['SECTION "Mobile Native Rectangle Programs", ROMX, BANK[MOBILE_ROM_BANK + 2]',
             'MobileNativeRectangleProgramsStart::',
             'MobileVirtualWriteRectangleDispatch::', '\tjr nc, MobileVirtualWriteOrdinaryRectangle',
             '\tjr z, MobileVirtualWriteDescriptorRectangle', '\tjr MobileVirtualWritePrivateRectangle',
             'MobileVirtualWriteOrdinaryRectangle::', '\tpush hl', '\tpush bc',
             '\tld bc, -FacingStepDown0', '\tadd hl, bc',
             '\tadd hl, hl', '\tadd hl, hl', '\tld bc, MobileRectanglePrograms',
             '\tadd hl, bc', '\tld a, [hli]', '\tld h, [hl]', '\tld l, a',
             '\tpop bc', '\tjp hl',
             'MobileVirtualWritePrivateRectangle::', '\tpush hl',
             '\tld bc, -MobileCopiedFacingStepDown0', '\tadd hl, bc',
             '\tadd hl, hl', '\tadd hl, hl', '\tld bc, MobileRectanglePrograms + 2',
             '\tadd hl, bc', '\tld a, [hli]', '\tld h, [hl]', '\tld l, a',
             '\tjp hl',
             'MobileVirtualWriteDescriptorRectangle::', '\tpush hl', '\tpush bc',
             '\tld bc, -MobileCopiedFacingStepDown0', '\tadd hl, bc',
             '\tadd hl, hl', '\tadd hl, hl', '\tld bc, MobileRectanglePrograms + 2',
             '\tadd hl, bc', '\tld a, [hli]', '\tld h, [hl]', '\tld l, a',
             '\tinc hl', '\tinc hl', '\tpop bc', '\tjp hl', 'MobileRectanglePrograms:']
    for names, count, _ in groups:
        name = names[0]
        # One dispatch entry for every source byte, with programs only at
        # the first quad. Native validated facing pointers select these entries.
        lines += [f'\tds 4 ; {name} count byte',
                  f'\tASSERT (@ - MobileRectanglePrograms) / 4 == {name} - FacingStepDown0 + 1']
        if count == 4:
            lines += [f'\tdw MobileOrdinary{name}, MobilePrivate{name}', '\tds 15 * 4']
        else:
            lines += [f'\tds {count * 4} * 4']
    lines += ['MobileRectangleProgramsEnd::',
              '\tASSERT (@ - MobileRectanglePrograms) / 4 == MobileCopiedFacingsEnd - MobileCopiedFacingStepDown0']

    for names, count, rows in groups:
        if count != 4:
            continue
        name = names[0]
        for ordinary in (True, False):
            prefix = 'MobileOrdinary' if ordinary else 'MobilePrivate'
            lines += [prefix + name + ':']
            tiles = [int(row[3][1:], 16) for row in rows if re.fullmatch(r'\$[0-9a-fA-F]+', row[3])]
            simple = (ordinary and len(tiles) == 4 and
                      len({tile & 128 for tile in tiles}) == 1 and
                      max(tile & 127 for tile in tiles) <= 15 and
                      all('ABSOLUTE_TILE_ID' not in row[2] and 'NEXT_PALETTE' not in row[2]
                          for row in rows))
            if simple:
                # The fast branch proves the whole four-tile sum cannot wrap or
                # straddle the standing/walking boundary. Other native inputs
                # retain the byte-exact general program below.
                lines += ['\tldh a, [hCurSpriteTile]', '\tcp 113', '\tjp nc, .unusual_base']
                if tiles[0] & 128:
                    lines += ['\tbit 3, d', '\tjr z, .vram0_base', '\tadd $40',
                              '\tjr .tile_base', '.vram0_base', '\tadd $80', '.tile_base']
                lines += ['\tld e, a', '\tldh a, [hCurSpriteXPixel]', '\tld h, a',
                          '\tldh a, [hCurSpriteYPixel]', '\tld l, a']
                for quad, (y, x, flags, tile) in enumerate(rows):
                    for axis, value in (('l', y), ('h', x)):
                        lines += ['\tld a, ' + axis]
                        if value != '0':
                            lines += ['\tadd ' + value]
                        lines += ['\tld [bc], a', '\tinc c']
                    lines += ['\tld a, e']
                    low_tile = tiles[quad] & 127
                    if low_tile:
                        lines += [f'\tadd ${low_tile:02x}']
                    lines += ['\tld [bc], a', '\tinc c', f'if ({flags}) & RELATIVE_ATTRIBUTES',
                              '\tldh a, [hCurSpriteOAMFlags]', f'if ({flags}) & (OAM_XFLIP | OAM_YFLIP | OAM_PRIO)',
                              f'\tor ({flags}) & (OAM_XFLIP | OAM_YFLIP | OAM_PRIO)', 'endc',
                              '\tand OAM_XFLIP | OAM_YFLIP | OAM_PRIO', '\tor d',
                              'else', '\tld a, d',
                              f'if ({flags}) & (OAM_XFLIP | OAM_YFLIP | OAM_PRIO)',
                              f'\tor ({flags}) & (OAM_XFLIP | OAM_YFLIP | OAM_PRIO)',
                              'endc', 'endc', '\tld [bc], a', '\tld a, c', '\tsub 7', '\tld c, a']
                lines += ['\tjp .finish', '.unusual_base']
            if not ordinary:
                # The alternate entry reads the immutable unbanked descriptor
                # directly. It avoids copying an eight-byte template solely to
                # read it again. Both entries keep the original facing cursor
                # on the dispatcher's stack and share the exact pixel program.
                lines += ['\tjr .template', '.descriptor',
                          '\tASSERT @ - ' + prefix + name + ' == 2',
                          '\tpush de', '\tld h, b', '\tld l, c',
                          '\tld a, [hli]', '\tld e, a',
                          '\tld a, [hli]', '\tld d, a',
                          '\tld a, [hli]', '\tld b, a',
                          '\tld a, [hli]', '\tld c, a',
                          '\tinc l', '\tinc l', '\tinc l',
                          '\tld a, [hl]', '\tld [wMobileVirtualTemplate + 7], a',
                          '\tld h, e', '\tld l, d', '\tpop de', '\tjr .coordinates',
                          '.template', '\tld a, [wMobileVirtualTemplate]', '\tld h, a',
                          '\tld a, [wMobileVirtualTemplate + 1]', '\tld l, a',
                          '\tld a, [wMobileVirtualTemplate + 2]', '\tld b, a',
                          '\tld a, [wMobileVirtualTemplate + 3]', '\tld c, a',
                          '.coordinates']
                private_simple = (len(tiles) == 4 and len({tile & 128 for tile in tiles}) == 1 and
                                  max(tile & 127 for tile in tiles) <= 15 and
                                  all('ABSOLUTE_TILE_ID' not in row[2] and 'NEXT_PALETTE' not in row[2]
                                      for row in rows))
                if private_simple:
                    lines += ['\tld a, b', '\tcp 113', '\tjp nc, .unusual_base']
                    if tiles[0] & 128:
                        lines += ['\tbit 3, c', '\tjr z, .vram0_base', '\tadd $40',
                                  '\tjr .tile_base', '.vram0_base', '\tadd $80', '.tile_base', '\tld b, a']
                    for quad, (y, x, flags, tile) in enumerate(rows):
                        for axis, value in (('l', y), ('h', x)):
                            lines += ['\tld a, ' + axis]
                            if value != '0':
                                lines += ['\tadd ' + value]
                            lines += ['\tld [de], a', '\tinc e']
                        lines += ['\tld a, b']
                        if tiles[quad] & 127:
                            lines += [f'\tadd ${tiles[quad] & 127:02x}']
                        lines += ['\tld [de], a', '\tinc e', f'if ({flags}) & RELATIVE_ATTRIBUTES',
                                  '\tld a, [wMobileVirtualTemplate + 7]', f'\tor {flags}',
                                  '\tand OAM_XFLIP | OAM_YFLIP | OAM_PRIO', '\tor c',
                                  'else', '\tld a, c',
                                  f'\tor ({flags}) & (OAM_XFLIP | OAM_YFLIP | OAM_PRIO)',
                                  'endc', '\tld [de], a', '\tinc e']
                    lines += ['\tjp .finish', '.unusual_base']
            for quad, (y, x, flags, tile) in enumerate(rows):
                for axis, value in ((1, y), (0, x)):
                    if ordinary:
                        source = 'hCurSpriteYPixel' if axis else 'hCurSpriteXPixel'
                        lines += [f'\tldh a, [{source}]']
                    else:
                        lines += ['\tld a, ' + ('l' if axis else 'h')]
                    if value != '0':
                        lines += [f'\tadd {value}']
                    lines += ['\tld [bc], a', '\tinc c'] if ordinary else ['\tld [de], a', '\tinc e']
                lines += [f'if ({flags}) & ABSOLUTE_TILE_ID', f'\tld a, {tile}', 'else']
                lines += ['\tldh a, [hCurSpriteTile]'] if ordinary else ['\tld a, b']
                lines += [f'\tadd {tile}', 'endc', '\tbit 3, ' + ('d' if ordinary else 'c'),
                          f'\tjr z, .tile{quad}', '\tcp $80', f'\tjr c, .tile{quad}',
                          '\tsub $40', f'.tile{quad}']
                lines += ['\tld [bc], a', '\tinc c'] if ordinary else ['\tld [de], a', '\tinc e']
                lines += [f'if ({flags}) & RELATIVE_ATTRIBUTES']
                lines += ['\tldh a, [hCurSpriteOAMFlags]'] if ordinary else ['\tld a, [wMobileVirtualTemplate + 7]']
                lines += [f'\tor {flags}', '\tand OAM_XFLIP | OAM_YFLIP | OAM_PRIO',
                          '\tor ' + ('d' if ordinary else 'c'), 'else',
                          '\tld a, ' + ('d' if ordinary else 'c'),
                          f'\tor ({flags}) & (OAM_XFLIP | OAM_YFLIP | OAM_PRIO)',
                          'endc', f'if ({flags}) & NEXT_PALETTE', '\tinc a', 'endc']
                if ordinary:
                    lines += ['\tld [bc], a', '\tld a, c', '\tsub 7', '\tld c, a']
                else:
                    lines += ['\tld [de], a', '\tinc e']
            lines += ['.finish', '\tpop hl', '\tld a, l', '\tadd 16', '\tld l, a',
                      '\tjr nc, .source_ready', '\tinc h', '.source_ready']
            if ordinary:
                lines += [f'\tld e, {rows[-1][2]}', '\txor a', '\tldh [hUsedSpriteTile], a']
            lines += ['\tld a, BANK(Facings)' if ordinary else '\tld a, [wMobileVirtualRectangleReturnBank]', '\tjp Bankswitch']
    lines += ['MobileNativeRectangleProgramsEnd::',
              '\tASSERT @ - MobileNativeRectangleProgramsStart <= $4000']
    return '\n'.join(lines)
