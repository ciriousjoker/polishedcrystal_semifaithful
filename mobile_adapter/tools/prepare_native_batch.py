"""Derive the mobile native emission batch from the unchanged engine body.

The native priority pass remains in the engine. Its emission body is copied
into an upper bank, with the same general-facing fallback and generated
four-quad programs. No native simulation or map data is changed.
"""
import re


def replace_once(source, before, after):
    if source.count(before) != 1:
        raise ValueError('Native batch source contract changed: ' + before[:80])
    return source.replace(before, after, 1)


def batch_source(engine, native, rectangle_source):
    body = engine.split('.InitSpritesByPriority:\n', 1)[1].split('StepFunction_Half1:\n', 1)[0]
    body = '.InitSpritesByPriority:\n' + body
    body = replace_once(body, '\tand $f\n\tadd a',
                        '\tand $f\n\tcp NUM_OBJECT_STRUCTS\n\tjr nc, .invalid_priority\n'
                        '\tld [wMobileVirtualIndex], a\n\tadd a')
    body = replace_once(body, '\tpop hl\n\tpop bc\n\tjr .next_sprite',
                        '.invalid_priority\n\tpop hl\n\tpop bc\n\tjr .next_sprite')
    body = replace_once(body, '\tcall .InitSprite\n',
                        '\tmobile_virtual_capture_kind\n\tcall .InitSprite\n')
    # Near jumps need the same extended range as the previous inline capture.
    body = body.replace('\tjr z, .done', '\tjp z, .done').replace('\tjr nc, .done', '\tjp nc, .done')
    body = replace_once(body, '\tsub c\n\tcp LOW(wShadowOAM)\n\tjr c, .full',
                        '\tmobile_virtual_capture MobileBatchFacingStepDown0, .draw_rectangle, .full\n'
                        '\tmobile_native_oam_bounds\n'
                        '\tldh a, [hUsedSpriteTile]\n\tcp 4\n\tjr nz, .addsprite\n'
                        '.draw_rectangle\n\tcall MobileBatchRectangleDispatch\n\tjr .sprite_done')
    body = replace_once(body, '\tld a, OBJ_SIZE * (OAM_COUNT - 1)\n',
                        '.sprite_done\n\tmobile_virtual_complete_capture\n'
                        '\tld a, OBJ_SIZE * (OAM_COUNT - 1)\n')
    body = replace_once(body, '\tld bc, Facings\n', '\tld bc, MobileBatchFacings\n')
    # Contiguous native fields can be addressed incrementally without changing
    # byte arithmetic, including structures that cross a WRAM page boundary.
    body = replace_once(body, '\tld hl, OBJECT_FLAGS2\n\tadd hl, bc', '\tinc hl\n\tinc hl\n\tinc hl')
    body = replace_once(body, '\tld hl, OBJECT_PALETTE\n\tadd hl, bc\n\tld d, a', '\tinc hl\n\tld d, a')
    body = replace_once(body, '\tld hl, OBJECT_SPRITE_X_OFFSET\n\tadd hl, bc', '\tinc hl\n\tinc hl')
    body = replace_once(body, '\tld hl, OBJECT_SPRITE_Y_OFFSET\n\tadd hl, bc', '\tinc hl\n\tinc hl')
    assertions = ['OBJECT_FLAGS2 == OBJECT_SPRITE_TILE + 3',
                  'OBJECT_PALETTE == OBJECT_FLAGS2 + 1',
                  'OBJECT_SPRITE_X_OFFSET == OBJECT_SPRITE_X + 2',
                  'OBJECT_SPRITE_Y_OFFSET == OBJECT_SPRITE_Y + 2']
    lines = ['SECTION "Mobile Native Emission Batch", ROMX, BANK[MOBILE_ROM_BANK + 3]',
             'MobileNativeBatchStart::']
    lines += ['\tASSERT ' + assertion for assertion in assertions]
    lines += ['MobileNativeBatch::', '\tld c, PRIORITY_LOW', '\tcall .InitSpritesByPriority',
              '\tld c, PRIORITY_NORM', '\tcall .InitSpritesByPriority',
              '\tld c, PRIORITY_HIGH', '\tjr .InitSpritesByPriority', body]
    labels = re.findall(r'^(\w+)::?', native, re.M)
    aliases = {name: 'MobileBatch' + name for name in labels}
    lines += [re.sub(r'\b(?:' + '|'.join(map(re.escape, labels)) + r')\b',
                     lambda match: aliases[match[0]], native)]
    # Keep one program implementation: derive ordinary programs directly from
    # the shared generator, replacing only their bank-return convention.
    programs = re.findall(r'(MobileOrdinary(Facing\w+):\n.*?)(?=MobilePrivateFacing)',
                          rectangle_source, re.S)
    if not programs:
        raise ValueError('Native rectangle generator emitted no ordinary programs')
    lines += ['MobileBatchRectangleDispatch::', '\tpush hl', '\tpush bc',
              '\tld bc, -MobileBatchFacingStepDown0', '\tadd hl, bc', '\tadd hl, hl',
              '\tld bc, MobileBatchRectangleTable', '\tadd hl, bc', '\tld a, [hli]',
              '\tld h, [hl]', '\tld l, a', '\tpop bc', '\tjp hl',
              'MobileBatchRectangleTable:']
    groups = re.findall(r'\tds 4 ; (Facing\w+) count byte\n.*?\n(.*?)(?=\tds 4 ;|MobileRectangleProgramsEnd)',
                        rectangle_source, re.S)
    for name, entry in groups:
        lines += ['\tds 2', f'\tASSERT (@ - MobileBatchRectangleTable) / 2 == MobileBatch{name} - MobileBatchFacingStepDown0 + 1']
        if '\tdw MobileOrdinary' in entry:
            lines += [f'\tdw MobileBatch{name}Program', '\tds 15 * 2']
        else:
            count = re.search(r'\tds (\d+) \* 4', entry)
            if not count:
                raise ValueError('Native rectangle table shape changed')
            lines += [f'\tds {count[1]} * 2']
    for program, name in programs:
        program = replace_once(program, 'MobileOrdinary' + name + ':', 'MobileBatch' + name + 'Program:')
        program = replace_once(program, '\tld a, BANK(Facings)\n\tjp Bankswitch', '\tret')
        lines += [program]
    lines += ['MobileNativeBatchEnd::', '\tASSERT @ - MobileNativeBatchStart <= $4000']
    return '\n'.join(lines)
