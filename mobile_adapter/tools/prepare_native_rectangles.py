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
             'MobileVirtualWriteRectangleDispatch::', '\tjp z, MobileVirtualPriorityDispatch', '\tjr c, MobileVirtualWritePrivateRectangle',
             'MobileVirtualWriteOrdinaryRectangle::', '\tpush hl', '\tpush bc',
             '\tld bc, -FacingStepDown0', '\tadd hl, bc',
             '\tadd hl, hl', '\tadd hl, hl', '\tld bc, MobileRectanglePrograms',
             '\tadd hl, bc', '\tld a, [hli]', '\tld h, [hl]', '\tld l, a',
             '\tpop bc', '\tjp hl',
             'MobileVirtualWritePrivateRectangle::', '\tpush hl',
             '\tld bc, -MobileCopiedFacingStepDown0', '\tadd hl, bc',
             '\tadd hl, hl', '\tadd hl, hl', '\tld bc, MobileRectanglePrograms + 2',
             '\tadd hl, bc', '\tld a, [hli]', '\tld h, [hl]', '\tld l, a',
             '\tjp hl', 'MobileRectanglePrograms:']
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
            if not ordinary:
                lines += ['\tld a, [wMobileVirtualTemplate + 2]', '\tld b, a',
                          '\tld a, [wMobileVirtualTemplate + 3]', '\tld c, a']
            for quad, (y, x, flags, tile) in enumerate(rows):
                for axis, value in ((1, y), (0, x)):
                    if ordinary:
                        source = 'hCurSpriteYPixel' if axis else 'hCurSpriteXPixel'
                        lines += [f'\tldh a, [{source}]']
                    else:
                        lines += [f'\tld a, [wMobileVirtualTemplate + {axis}]']
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
            lines += ['\tpop hl', '\tld a, l', '\tadd 16', '\tld l, a',
                      '\tjr nc, .source_ready', '\tinc h', '.source_ready']
            if ordinary:
                lines += [f'\tld e, {rows[-1][2]}', '\txor a', '\tldh [hUsedSpriteTile], a']
            lines += ['\tld a, BANK(Facings)' if ordinary else '\tld a, MOBILE_ROM_BANK', '\tjp Bankswitch']
    lines += ['MobileNativeRectangleProgramsEnd::',
              '\tASSERT @ - MobileNativeRectangleProgramsStart <= $4000']
    return '\n'.join(lines)
