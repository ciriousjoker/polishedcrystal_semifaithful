#!/usr/bin/env python3
"""Execute the linked retained normal source and immutable consumer.

These are emitted instruction checks. Native cadence, physical LCD output,
graphics leases and arbitrary native palette writers need separate validation.
"""
import argparse
import json
from pathlib import Path
import random
import re
from retained_source_cpu import Machine, require

ROOT = Path(__file__).resolve().parents[2]


def symbols(path):
    result = {}
    for line in path.read_text().splitlines():
        match = re.match(r'([0-9a-f]+):([0-9a-f]+) (\S+)', line)
        if match:
            result[match[3]] = (int(match[1], 16), int(match[2], 16))
        else:
            match = re.match(r'([0-9a-f]+) (\S+)', line)
            if match:
                result[match[2]] = (0, int(match[1], 16))
    return result


def machine(image, seed=1, bank=128, wbk=5):
    current = Machine(*image, seed)
    current.bank = bank
    current.memory[0xff70] = wbk
    current.memory[current.address('hROMBank')] = bank
    return current


def put(current, name, value):
    current.write(current.address(name), value)


def get(current, name):
    return current.read(current.address(name))


def raw(image, label, offset, count):
    bank, address = image[1][label]
    start = bank * 0x4000 + address - 0x4000 + offset
    return image[0][start:start + count]


def expected_header_output(image, header):
    pointer = int.from_bytes(raw(image, 'MobileCopiedFacings', header[4] * 2, 2), 'little')
    bank = image[1]['MobileCopiedFacings'][0]
    start = bank * 0x4000 + pointer - 0x4000
    count = image[0][start]
    result = bytearray()
    for index in reversed(range(count)):
        y, x, flags, tile = image[0][start + 1 + index * 4:start + 5 + index * 4]
        tile = (tile + (0 if flags & 4 else header[2])) & 255
        if header[3] & 8 and tile >= 128:
            tile -= 64
        attributes = ((header[5] if flags & 2 else 0) | flags) & 0xe0
        attributes |= header[3]
        if flags & 8:
            attributes = attributes + 1 & 255
        result.extend(((header[1] + y) & 255, (header[0] + x) & 255, tile, attributes))
    return bytes(result)


def verify(image):
    costs = {}
    cases = 0
    rng = random.Random(0x405104)
    constants = {}
    text = (ROOT / 'constants/map_object_constants.asm').read_text()
    for key in ('OBJECT_SPRITE', 'OBJECT_SPRITE_TILE', 'OBJECT_FLAGS2', 'OBJECT_PALETTE',
                'OBJECT_SPRITE_X', 'OBJECT_SPRITE_X_OFFSET', 'OBJECT_SPRITE_Y',
                'OBJECT_SPRITE_Y_OFFSET', 'OBJECT_FACING'):
        constants[key] = int(re.search(r'^DEF ' + key + r'\s+rb ; ([0-9a-f]+)', text, re.M)[1], 16)

    # Paired actual producer, with capture authorization as the sole difference.
    producer_headers = []
    for facing in range(62):
        count = raw(image, 'MobileCopiedFacings', facing * 2, 2)
        pointer = int.from_bytes(count, 'little')
        count = raw(image, 'MobileCopiedFacings', pointer - image[1]['MobileCopiedFacings'][1], 1)[0]
        for used in (0, 8, 112, 148, 156, 160, 255):
            for slot in (0, 1, 12):
                seed = rng.randrange(2**32)
                pair = [machine(image, seed, image[1]['MobileNativeBatch'][0], 1) for _ in range(2)]
                for active, current in enumerate(pair):
                    for name, value in (('wMobileNativeCaptureActive', active), ('wMobileVirtualStage', 0),
                                        ('wPlayerBGMapOffsetX', 3), ('wPlayerBGMapOffsetY', 4), ('hUsedOAMIndex', used)):
                        put(current, name, value)
                    priorities = current.address('wObjectPriorities')
                    current.memory[priorities:priorities + 16] = bytes([16 | slot]) + bytes(15)
                    header_start = current.address('wMobileVirtualDescriptors')
                    current.memory[header_start:header_start + 104] = bytes(104)
                    current.banks[5][0x868:0x890] = bytes([255]) * 40
                    object_start = current.address('wPlayerStruct' if slot == 0 else f'wObject{slot}Struct')
                    for key, value in (('OBJECT_SPRITE', 1), ('OBJECT_SPRITE_TILE', (34, 127, 162, 255)[(facing + slot) % 4]),
                                       ('OBJECT_FLAGS2', (0, 255)[(facing + slot) % 2]), ('OBJECT_PALETTE', (3, 255)[facing % 2]),
                                       ('OBJECT_SPRITE_X', 48), ('OBJECT_SPRITE_X_OFFSET', 0), ('OBJECT_SPRITE_Y', 56),
                                       ('OBJECT_SPRITE_Y_OFFSET', 0), ('OBJECT_FACING', facing)):
                        current.write(object_start + constants[key], value)
                    current.execute('MobileNativeBatch')
                baseline, captured = pair
                shadow = captured.address('wShadowOAM')
                require(baseline.memory[shadow:shadow + 160] == captured.memory[shadow:shadow + 160], 'Capture changed native 160-byte output')
                require(baseline.r == captured.r and baseline.f == captured.f and baseline.sp == captured.sp and
                        baseline.bank == captured.bank and baseline.memory[0xff70] == captured.memory[0xff70], 'Capture changed producer ABI')
                require(get(baseline, 'hUsedOAMIndex') == get(captured, 'hUsedOAMIndex'), 'Capture changed native cursor')
                start = captured.address('wMobileVirtualDescriptors') + slot * 8
                header = bytes(captured.memory[start:start + 8])
                require(header[4] == facing and header[6] & 31 == count and header[6] & 128, 'Incomplete immutable producer intent')
                expected = expected_header_output(image, header)
                if header[7] != 255:
                    require(captured.memory[shadow + header[7]:shadow + header[7] + count * 4] == expected, 'Producer tuple order differs from immutable intent')
                    require(captured.banks[5][0x868 + header[7] // 4:0x868 + header[7] // 4 + count] == bytes([slot]) * count, 'Actual completed writer did not establish ownership')
                elif used <= 160 - count * 4:
                    raise AssertionError('Fitting actor unexpectedly overflowed')
                producer_headers.append(header)
                cases += 1

    # Every valid facing is emitted from its immutable header after live mutation.
    emitter_cases = 0
    for header in producer_headers:
        current = machine(image, emitter_cases + 1)
        start = current.address('wMobileVirtualTemplate') - 0xd000
        current.banks[5][start:start + 8] = header
        before = bytes(current.banks[5])
        current.memory[current.address('wShadowOAM'):current.address('wShadowOAM') + 160] = bytes([0xa5]) * 160
        forbidden = []
        original_read = current.read
        def read(address):
            if current.address('wShadowOAM') <= address < current.address('wShadowOAM') + 160:
                forbidden.append(address)
            if 0xd4cc <= address < 0xd686:
                forbidden.append(address)
            return original_read(address)
        current.read = read
        cost = current.execute('MobileVirtualEmitImmutableGroup')
        require(not forbidden, 'Immutable emitter read live native state')
        require(get(current, 'wMobileVirtualRejected') == 1, 'Valid immutable figure rejected')
        output_start = current.address('wMobileVirtualGroup') - 0xd000
        expected = expected_header_output(image, header)
        require(current.banks[5][output_start:output_start + len(expected)] == expected, 'General immutable emitter output differs from arithmetic')
        require(current.banks[5][0xb4b:0xb8b] == before[0xb4b:0xb8b], 'Emitter scratched retained native RGB')
        costs.setdefault(str(len(expected) // 4), []).append(cost)
        emitter_cases += 1
    base = bytearray(producer_headers[0])
    for field, value in ((6, 0), (6, 128), (4, 62), (4, 255), (7, 1), (7, 160), (7, 252)):
        header = base.copy()
        header[field] = value
        current = machine(image)
        start = current.address('wMobileVirtualTemplate') - 0xd000
        current.banks[5][start:start + 8] = header
        before = bytes(current.banks[5][0xab0:0xaf0])
        current.execute('MobileVirtualEmitImmutableGroup')
        require(get(current, 'wMobileVirtualRejected') == 2 and bytes(current.banks[5][0xab0:0xaf0]) == before, 'Malformed header wrote a partial figure')

    lifecycle = 0
    for wbk in range(8):
        for enabled in (False, True):
            current = machine(image, lifecycle + 1, wbk=wbk)
            native_bank = wbk & 7 or 1
            current.banks[native_bank][current.address('wStateFlags') - 0xd000] = int(enabled)
            put(current, 'wMobileLocalValid', 1)
            put(current, 'hInMenu', 0)
            current.banks[5][0x8ea] = 0 # no in-progress hardware transaction in this lifecycle case
            current.banks[5][0x890:0x8c0] = bytes(range(48))
            history = bytes(current.banks[5][0x890:0x8c0])
            registers, flags = current.r.copy(), current.f
            current.execute('MobileBeginNormalAttempt')
            require(current.r == registers and current.f == flags and current.memory[0xff70] == wbk, 'Begin changed native caller ABI')
            require(current.banks[5][0x890:0x8c0] == history, 'Ordinary refresh changed identity/history')
            require(current.banks[5][0x8ee] == int(enabled) and get(current, 'wMobileNativeCaptureActive') == int(enabled), 'Wrong native flag bank or skipped authorization')
            require(current.banks[5][0x8e6] == 0 and current.banks[5][0x8e9] == 0 and current.banks[5][0x8ed] == 0, 'Begin retained a stale source/candidate')
            if enabled:
                raw_oam = bytes((index * 17 + lifecycle) & 255 for index in range(160))
                native_rgb = bytes((index * 11 + lifecycle) & 255 for index in range(64))
                shadow = current.address('wShadowOAM')
                current.memory[shadow:shadow + 160] = raw_oam
                current.banks[5][0xdc0:0xe00] = native_rgb
                current.banks[1][0xcac:0xcae] = bytes((7, 8))
                put(current, 'wMobileWanted', 0)
                put(current, 'wMobileOnline', 0)
                writes = []
                original_write = current.write
                def write(address, value):
                    writes.append((current.memory[0xff70] & 7 or 1, address, value))
                    original_write(address, value)
                current.write = write
                current.execute('MobileCaptureNormalSource')
                require(current.banks[5][0x840:0x868] == raw_oam[::4], 'Retained Ys differ from actual source')
                triples = bytes(value for index, value in enumerate(raw_oam) if index % 4)
                require(current.banks[5][0x92a:0x9a2] == triples and current.banks[5][0xb4b:0xb8b] == native_rgb, 'Retained tuples/RGB incomplete')
                require(get(current, 'wMobileVirtualMapGroup') == 7 and get(current, 'wMobileVirtualMapNumber') == 8, 'Map identity read under wrong bank')
                valid_writes = [index for index, entry in enumerate(writes) if entry == (5, 0xd8e6, 1)]
                require(len(valid_writes) == 1 and all(index < valid_writes[0] for index, entry in enumerate(writes) if entry[0] == 5 and (0xd92a <= entry[1] < 0xd9a2 or 0xdb4b <= entry[1] < 0xdb8b)), 'Source validity preceded complete capture')
                require(current.banks[5][0x8ed] == 0 and current.banks[5][0x8ee] == 0 and get(current, 'wMobileNativeCaptureActive') == 0, 'Source incorrectly certified fallback or retained authorization')
                # A second closure cannot reuse equal fields or completed headers.
                current.execute('MobileCaptureNormalSource')
                require(current.banks[5][0x8e6] == 0, 'Consumed authorization resurrected equal source')
            lifecycle += 1

    # Equal-byte native replacement revokes producer ownership before the store.
    ownership_cases = 0
    for index in range(40):
        current = machine(image, index + 7, wbk=1)
        current.banks[5][0x868:0x890] = bytes([1]) * 40
        current.banks[5][0x8ee] = 1
        put(current, 'wMobileNativeCaptureActive', 1)
        current.banks[5][0x8e6] = current.banks[5][0x8e9] = current.banks[5][0x8ed] = 1
        current.put('hl', current.address('wShadowOAM') + index * 4)
        registers, flags = current.r.copy(), current.f
        current.execute('MobileUnknownNativeWrite')
        require(current.r == registers and current.f == flags and current.memory[0xff70] == 1, 'Weather prewrite changed caller ABI')
        require(current.banks[5][0x868 + index] == 255 and current.banks[5][0x8e6] == current.banks[5][0x8e9] == current.banks[5][0x8ed] == 0, 'Equal replacement retained actor ownership/source')
        require(current.banks[5][0x8ee] == 1 and get(current, 'wMobileNativeCaptureActive') == 1, 'Weather consumed normal producer authorization')
        current.write(current.pair('hl'), current.read(current.pair('hl')))
        require(current.banks[5][0x868 + index] == 255, 'Equal-byte restoration resurrected ownership')
        ownership_cases += 1
    return {'producer_pairs': cases, 'immutable_emitter_cases': emitter_cases, 'malformed_rejections': 7,
            'lifecycle_bank_gate_cases': lifecycle, 'equal_byte_owner_replacements': ownership_cases,
            'emitter_t_clocks_by_count': {count: {'min': min(values), 'max': max(values)} for count, values in costs.items()},
            'scope': 'Functional emitted source/ABI checks only; physical native acceptance remains unresolved.'}


def verify_guards_and_output(image):
    guard_cases = 0
    hardware_cases = 0
    camera_cases = 0
    costs = {}
    def armed(wbk=5):
        current = machine(image, guard_cases + hardware_cases + 83, wbk=wbk)
        for offset in (0x8e6, 0x8e7, 0x8e9, 0x8ed, 0x8ee):
            current.banks[5][offset] = 1
        current.banks[5][0x8ea] = 0
        put(current, 'wMobileNativeCaptureActive', 1)
        put(current, 'wMobileVirtualStage', 7)
        current.memory[current.address('wMobileVirtualDescriptors'):current.address('wMobileVirtualDescriptors') + 104] = bytes([0xc4]) * 104
        current.banks[5][0x868:0x890] = bytes([3]) * 40
        current.banks[5][current.address('wMobilePrivatePalDirty') - 0xd000] = 0
        return current

    specs = [('MobilePalettePrewriteCopyPals', 'hl', 0xddc0, 1),
             ('MobilePalettePrewriteCopyPals', 'hl', 0xdd80, 16),
             ('MobilePalettePrewriteCopyPals', 'hl', 0xdd80, 8),
             ('MobilePalettePrewriteCopyPals', 'hl', 0xddbf, 1),
             ('MobilePalettePrewriteCopyPals', 'hl', 0xd900, 0),
             ('MobilePalettePrewriteFade', 'hl', 0xdd80, 64),
             ('MobilePalettePrewriteFade', 'hl', 0xdd80, 32),
             ('MobilePalettePrewriteFade', 'hl', 0xddbf, 1),
             ('MobilePalettePrewriteFade', 'hl', 0xdc00, 0),
             ('MobilePalettePrewriteLoad', 'de', 0xddc0, 8),
             ('MobilePalettePrewriteLoad', 'de', 0xdd80, 64),
             ('MobilePalettePrewriteLoad', 'de', 0xdd80, 0),
             ('MobilePalettePrewriteWhite', 'de', 0xddc0, 8),
             ('MobilePalettePrewriteWhite', 'de', 0xdd80, 8),
             ('MobilePalettePrewriteFixedCopy', 'de', 0xdd80, 128),
             ('MobilePalettePrewriteFixedCopy', 'de', 0xdd80, 64),
             ('MobilePalettePrewriteBlack', 'hl', 0xddc0, 64),
             ('MobilePalettePrewriteClear', 'hl', 0xdd80, 128)]
    for entry, pointer, start, count in specs:
        for wbk in (0, 1, 3, 5, 0xfd):
            current = armed(wbk)
            current.put(pointer, start)
            if entry.endswith('CopyPals'):
                current.r['c'] = count
                length = (count or 256) * 8
            elif entry.endswith('Fade'):
                current.r['d'] = count
                length = (count or 256) * 2
            elif entry.endswith('Load'):
                current.r['c'] = count
                length = count or 256
            else:
                current.put('bc', count)
                length = count
            fixed = entry.endswith(('White', 'FixedCopy', 'Black', 'Clear'))
            intersects = start < 0xde00 and start + length > 0xddc0 and (fixed or wbk & 7 == 5)
            registers, flags = current.r.copy(), current.f
            headers = bytes(current.memory[current.address('wMobileVirtualDescriptors'):current.address('wMobileVirtualDescriptors') + 104])
            owners = bytes(current.banks[5][0x868:0x890])
            rgb = bytes(current.banks[5][0xdc0:0xe00])
            cost = current.execute(entry)
            require(current.r == registers and current.f == flags and current.memory[0xff70] == wbk, 'Palette guard changed native ABI')
            require(current.banks[5][0x8e6] == int(not intersects) and current.banks[5][0x8e9] == int(not intersects) and current.banks[5][0x8ed] == int(not intersects), f'Wrong range/bank guard {entry}/{start:04x}/{count}/{wbk}')
            require(current.banks[5][0x8ee] == 1 and get(current, 'wMobileNativeCaptureActive') == 1, 'Palette guard consumed normal auth')
            require(bytes(current.memory[current.address('wMobileVirtualDescriptors'):current.address('wMobileVirtualDescriptors') + 104]) == headers and current.banks[5][0x868:0x890] == owners and current.banks[5][0xdc0:0xe00] == rgb, 'Palette guard changed headers/owners/native RGB')
            costs.setdefault(entry, []).append(cost)
            guard_cases += 1

    # Execute the actual native CopyPals loop, not only its range guard.
    for wbk, start, palettes in ((5, 0xddc0, 8), (5, 0xdd80, 16), (5, 0xdd80, 8), (3, 0xddc0, 8)):
        current = armed(wbk)
        bank = wbk & 7 or 1
        current.bank = image[1]['CopyPals'][0]
        current.banks[bank][0xd00:0xe00] = bytes([0x34, 0x12]) * 128
        current.put('hl', start)
        current.put('de', 0xdd00)
        current.r['b'], current.r['c'] = 0xe4, palettes
        writes = []
        original_write = current.write
        def write(address, value):
            if (current.memory[0xff70] & 7 or 1) == 5 and 0xddc0 <= address < 0xde00:
                require(current.banks[5][0x8e6] == current.banks[5][0x8e9] == current.banks[5][0x8ed] == 0, 'Actual native RGB store preceded revocation')
                writes.append(address)
            original_write(address, value)
        current.write = write
        current.execute('CopyPals')
        require(bool(writes) == (wbk == 5 and start + palettes * 8 > 0xddc0), 'Actual writer range coverage mismatch')
        guard_cases += 1

    # The actual object handler guard must distinguish target-only updates.
    for fading in (0, 1):
        for no_apply in (0, 1):
            current = armed(1)
            current.put('de', image[1]['wOBPals1'][1])
            current.banks[5][current.address('wPalFadeDelayFrames') - 0xd000] = fading
            flag_bank, flag_address = image[1]['wPalFlags']
            # NO_DYN_PAL_APPLY includes the persistent and one-shot native flags.
            mask = 0x11
            current.memory[0xff70] = flag_bank
            current.write(flag_address, mask if no_apply else 0)
            current.memory[0xff70] = 1
            current.execute('MobilePalettePrewriteObjectHandler')
            require(current.banks[5][0x8e6] == int(not (fading or not no_apply)), 'Object target-only/fade guard is wrong')
            guard_cases += 1

    # Share a hardware decision, then invalidate its RAM source after private RGB.
    for decision in (1, 2, 3):
        for wbk in (0, 1, 3, 5):
            current = armed(wbk)
            current.banks[5][0x8ea] = decision
            native = bytes(range(64))
            peers = bytes(range(100, 124))
            live = bytes(range(192, 256))
            current.banks[5][0xb4b:0xb8b] = native
            peer_offset = current.address('wMobileVirtualFrameColors') - 0xd000
            current.banks[5][peer_offset:peer_offset + 24] = peers
            current.banks[5][0xdc0:0xe00] = live
            mapping = bytes((0, 8, 2, 9, 4, 10, 6, 7))
            map_offset = current.address('wMobilePrivatePalBindings') - 0xd000
            current.banks[5][map_offset:map_offset + 8] = mapping
            values = []
            original_write = current.write
            def write(address, value):
                if address == 0xff6b:
                    values.append(value)
                original_write(address, value)
            current.write = write
            current.memory[0xff70] = 5
            current.execute('MobileNativeUpdateOBPalettes')
            expected = bytes(value for source in mapping for value in (native[source * 8:source * 8 + 8] if source < 8 else peers[(source - 8) * 8:(source - 8) * 8 + 8]))
            require(bytes(values) == (expected if decision == 2 else live if decision == 1 else b''), 'OBJ hardware RGB disagrees with shared decision')
            if decision == 2:
                require(current.banks[5][0x8ea] == 0x82, 'Private RGB choice was not locked before stores')
                current.put('de', 0xddc0)
                current.put('bc', 8)
                current.execute('MobilePalettePrewriteFixedCopy')
                require(current.banks[5][0x8e6] == 0 and current.banks[5][0x8ea] == 0x82, 'Post-upload revocation changed locked OAM choice')
            if decision != 3:
                current.memory[0xff70] = wbk
                registers, flags = current.r.copy(), current.f
                before_bank = current.memory[0xff70]
                current.execute('MobileVirtualPushOAM')
                require(current.transfers[-1][0] == (0xd7 if decision == 2 else 0xc1), 'OAM consumer chose a different page after RGB upload')
                require(current.memory[0xff70] == before_bank and current.r['d'] == registers['d'] and current.r['e'] == registers['e'] and current.r['h'] == registers['h'] and current.r['l'] == registers['l'], 'DMA changed preserved native ABI')
                require(current.r['a'] == (0xd7 if decision == 2 else 0xc1) and current.f == (0xc0 | (flags & 16)), f'DMA terminal A/flags: decision={decision}, WBK={wbk}, A={current.r[chr(97)]}, F={current.f}, inputF={flags}')
            hardware_cases += 1

    for conflict in ('hRequested2bpp', 'hRequested1bpp', 'hDMATransfer', 'rVDMA_LEN', 'hOAMUpdate', None):
        current = armed(1)
        for name, value in [('wMobileNativeGraphicsFence', 0), ('wMobileWanted', 1), ('wMobileOnline', 1), ('wMobileLocalValid', 1), ('hInMenu', 0), ('hVBlank', 0), ('hVBlankCounter', 27), ('wMobileVirtualHeaderOAM', 27), ('wMobileVirtualPreviousOAM', 1), ('hRequested2bpp', 0), ('hRequested1bpp', 0), ('hDMATransfer', 0), ('rVDMA_LEN', 255), ('hOAMUpdate', 0)]:
            if name == 'rVDMA_LEN':
                current.memory[0xff55] = value
            else:
                put(current, name, value)
        if conflict == 'rVDMA_LEN':
            current.memory[0xff55] = 0
        elif conflict:
            put(current, conflict, 1)
        current.execute('MobileVirtualChooseOutput')
        require(current.banks[5][0x8ea] == (3 if conflict == 'hOAMUpdate' else 1 if conflict else 2), 'Native transfer conflict was not decided before RGB')
        require(current.memory[0xff70] == 1, 'Shared output decision leaked WRAM bank')
        hardware_cases += 1

    # A live native world-coordinate update cannot move the captured camera.
    for world in (0, 42, 127, 255):
        current = armed(5)
        current.banks[5][0x8f0:0x8f4] = bytes((0x34, 0x12, 0x78, 0x56))
        current.memory[0xff70] = 1
        for name, value in [('wScriptRunning', 0), ('wPanningAroundTinyMap', 0), ('wXCoord', world), ('wYCoord', 255 - world), ('wMobileOnline', 1), ('wMobilePeerCount', 0)]:
            put(current, name, value)
        current.execute('MobileNativePrepareSpritesWork')
        require(current.read(current.address('wMobileCameraX')) == 0x34 and current.read(current.address('wMobileCameraX') + 1) == 0x12 and current.read(current.address('wMobileCameraY')) == 0x78 and current.read(current.address('wMobileCameraY') + 1) == 0x56, 'Live simulation moved captured view anchor')
        camera_cases += 1
    current = armed(5)
    put(current, 'wMobileNativeGraphicsFence', 0)
    put(current, 'hOAMUpdate', 1)
    put(current, 'wMobileVirtualQueuedPeer', 1)
    before = bytes(current.banks[5])
    current.execute('MobileVirtualCommitPose')
    require(bytes(current.banks[5]) == before and get(current, 'wMobileVirtualQueuedPeer') == 1, 'Suppressed DMA changed retained pose/source')

    for mismatch in (False, True):
        current = armed(5)
        put(current, 'wMobileBudgetIndex', 0)
        colors = bytes((3, 7, 11, 15, 19, 23, 27, 31))
        native_offset = current.address('wMobileStableNativeRGB') - 0xd000
        cache_offset = current.address('wMobilePrivatePalColors') - 0xd000
        current.banks[5][native_offset:native_offset + 64] = bytes([255]) * 64
        current.banks[5][native_offset + 24:native_offset + 32] = colors
        current.banks[5][cache_offset:cache_offset + 8] = colors
        if mismatch:
            current.banks[5][cache_offset + 7] ^= 1
        current.execute('MobileVirtualFindRetainedPeerColor')
        require(bool(current.f & 16) == mismatch and (mismatch or current.r['a'] == 3), 'Native color sharing ignored exact eight-byte RGB')

    # Run the real retained composer instructions with no peer cache dependency.
    current = armed(5)
    for name, value in [('wMobileNativeGraphicsFence', 0), ('hInMenu', 0), ('wMobileWanted', 1), ('wMobileOnline', 1), ('wMobileLocalValid', 1), ('wMobilePeerCount', 0), ('wScriptRunning', 0), ('wPanningAroundTinyMap', 0)]:
        put(current, name, value)
    current.banks[5][0x840:0x868] = bytes([160]) * 40
    current.banks[5][0x868:0x890] = bytes([255]) * 40
    address = current.address('wMobileVirtualDescriptors')
    current.memory[address:address + 104] = bytes(104)
    current.memory[address:address + 8] = bytes((48, 56, 32, 0, 0, 0, 0x84, 0))
    current.banks[5][0x8e9] = 0
    current.memory[0xff44] = 0
    current.execute('MobileVirtualBeginRetainedComposition')
    stages = [get(current, 'wMobileVirtualStage')]
    for _ in range(100):
        current.execute('MobileVirtualComposeStep')
        stages.append(get(current, 'wMobileVirtualStage'))
        if not stages[-1]:
            break
    require(stages[-1] == 0 and 6 in stages and 7 in stages and current.banks[5][0x8e9] == 1, f'Retained composition failed to publish: {stages}')
    require(current.banks[5][0x8e6] == 1, 'Useful composition revoked its immutable source')
    require(get(current, 'wMobileVirtualLength') == 16, 'Useful composition did not retain the complete local figure')
    output_offset = current.address('wMobileVirtualOutput') - 0xd000
    require(bytes(current.banks[5][output_offset:output_offset + 16]) == expected_header_output(image, bytes((48, 56, 32, 0, 0, 0, 0x84, 0))), 'Useful publication changed the local complete tuple program')
    return {'palette_guard_range_bank_cases': guard_cases, 'shared_output_cases': hardware_cases, 'frozen_camera_cases': camera_cases, 'suppressed_pose_cases': 1, 'exact_rgb_sharing_cases': 2, 'useful_composer_stage_trace': stages,
            'guard_entry_t_clocks': {name: {'min': min(values), 'max': max(values)} for name, values in costs.items()}}

def verify_native_guard_callers(image, disabled):
    """Pair complete native callers, executing FarCall and original ROM0 helpers.

    SetBlackObjectPals is paired through its actual tail jump into DelayFrame.
    The frame wait itself belongs to the separate native integration oracle.
    """
    cases = stores = same_byte_stores = delay_paths = 0
    class DelayBoundary(Exception):
        pass
    source_flags = ('wMobileStableSourceValid', 'wMobileStablePendingValid',
                    'wMobileStableNativeCandidateValid')
    feature_bytes = set()
    for name, length in ([(name, 1) for name in source_flags] +
                         [('wMobileStablePendingNative', 2), ('wMobileStablePendingPrivate', 2)] +
                         [(name, 1) for name in ('wMobileStableActorViewValid', 'wMobileStableCapacityValid',
                           'wMobileStableFinalDecision', 'wMobileStableProducerActive', 'wMobilePrivatePalDirty')]):
        address = image[1][name][1]
        if 0xd000 <= address < 0xe000:
            feature_bytes.update(range(address - 0xd000, address - 0xd000 + length))
    specifications = [('ClearSprites', 0), ('RestoreSprites', 0),
                      ('SetBlackObjectPals', 0)]
    specifications += [('ClearNormalSprites', used) for used in (0, 1, 4, 159, 160, 161, 255)]
    for seed in (11, 42, 99):
        for wbk in (0, 1, 5, 0xf9):
            for entry, used in specifications:
                for equal in (False, True):
                    pair = []
                    for enabled, linked in ((False, disabled), (True, image)):
                        current = machine(linked, seed, bank=7, wbk=wbk)
                        # Stable native addresses, deterministic native source, and complete bank image.
                        current.memory[current.address('hUsedOAMIndex')] = used
                        raw_start = current.address('wShadowOAM')
                        current.memory[raw_start:raw_start + 512] = bytes(512) if equal else bytes(range(256)) * 2
                        backup = current.address('wShadowOAMBackup') - 0xd000
                        backup_bank = linked[1]['wShadowOAMBackup'][0]
                        current.banks[backup_bank][backup:backup + 160] = current.memory[raw_start:raw_start + 160] if equal else bytes(reversed(range(160)))
                        current.banks[5][0xdc0:0xe00] = bytes(64) if equal else bytes(range(64))
                        headers_address = current.address('wMobileVirtualDescriptors') if enabled else 0
                        if enabled:
                            current.memory[0xff70] = 5
                            for name in source_flags + ('wMobileStableProducerActive', 'wMobileNativeCaptureActive'):
                                put(current, name, 1)
                            put(current, 'wMobileStableFinalDecision', 0x82)
                            put(current, 'wMobilePrivatePalDirty', 0)
                            current.memory[0xff70] = wbk
                            current.memory[headers_address:headers_address + 104] = bytes([0xc4]) * 104
                        headers = bytes(current.memory[headers_address:headers_address + 104])
                        owners = bytes(current.banks[5][0x868:0x890])
                        native_writes = []
                        original_write, original_read = current.write, current.read
                        visited = set()
                        def read(address):
                            if address in (current.address('ByteFill'), current.address('_ByteFill'), current.address('CopyBytes'), current.address('_CopyBytes')):
                                visited.add(address)
                            if entry == 'SetBlackObjectPals' and address == current.address('DelayFrame'):
                                raise DelayBoundary
                            return original_read(address)
                        def write(address, value):
                            nonlocal stores, same_byte_stores
                            span_start = (raw_start & 0xff00) | ((160 - used) & 255) if entry == 'ClearNormalSprites' else raw_start
                            span_count = used if entry == 'ClearNormalSprites' else 160
                            native = span_start <= address < span_start + span_count or ((current.memory[0xff70] & 7 or 1) == 5 and 0xddc0 <= address < 0xde00)
                            if native:
                                if enabled:
                                    require(all(current.banks[5][current.address(n) - 0xd000] == 0 for n in source_flags), 'Native store preceded guard')
                                    require(current.banks[5][current.address('wMobileStableFinalDecision') - 0xd000] == 0x82, 'Guard broke private RGB lock')
                                    if entry == 'SetBlackObjectPals':
                                        require(bytes(current.memory[headers_address:headers_address + 104]) == headers and bytes(current.banks[5][0x868:0x890]) == owners, 'Black guard changed headers/owners')
                                        require(get(current, 'wMobileNativeCaptureActive') == 1 and current.banks[5][current.address('wMobileStableProducerActive') - 0xd000] == 1, 'Black guard consumed producer authorization')
                                        require(current.banks[5][current.address('wMobilePrivatePalDirty') - 0xd000] & 7 == 7, 'Black guard did not dirty all peer colors')
                                    else:
                                        require(get(current, 'wMobileNativeCaptureActive') == 0 and current.banks[5][current.address('wMobileStableProducerActive') - 0xd000] == 0, 'OAM guard retained producer authorization')
                                    stores += 1
                                    same_byte_stores += int(original_read(address) == value)
                                native_writes.append((address, value, dict(current.r), current.f, current.memory[0xff70]))
                            original_write(address, value)
                        current.read, current.write = read, write
                        try:
                            current.execute(entry)
                            require(entry != 'SetBlackObjectPals', 'Black caller bypassed DelayFrame')
                        except DelayBoundary:
                            require(entry == 'SetBlackObjectPals' and current.sp == 0xc7fe, 'Black caller tail stack changed')
                        require(current.bank == 7 and current.memory[current.address('hROMBank')] == 7 and current.memory[0xff70] == wbk, 'Native caller leaked bank state')
                        if enabled:
                            require(all(current.banks[5][current.address(n) - 0xd000] == 0 for n in source_flags), 'Caller did not revoke even with zero native stores')
                            require(current.banks[5][current.address('wMobileStableFinalDecision') - 0xd000] == 0x82, 'Caller lost private RGB lock')
                        target = 'CopyBytes' if entry == 'RestoreSprites' else 'ByteFill'
                        require(current.address(target) in visited and current.address('_' + target) in visited, 'Original native RST entry/body was not executed')
                        # All actual native store registers and flags must match, as must terminal ABI.
                        pair.append((dict(current.r), current.f, current.sp, current.bank,
                                     current.memory[current.address('hROMBank')], current.memory[0xff70],
                                     bytes(current.memory[raw_start:raw_start + 512]), bytes(current.banks[1]),
                                     bytes(value for i, value in enumerate(current.banks[5]) if i not in feature_bytes),
                                     tuple(bytes(current.banks[i]) for i in (2, 3, 4, 6, 7)), native_writes))
                    require(pair[0] == pair[1], f'Native caller ABI/output mismatch: {entry}/{used}/{wbk}/{equal}')
                    count = 160 if entry in ('ClearSprites', 'RestoreSprites') else 64 if entry == 'SetBlackObjectPals' else used
                    require(len(pair[1][-1]) == count, f'Native boundary/zero-count semantics changed: {entry}/{used}')
                    cases += 1
                    delay_paths += int(entry == 'SetBlackObjectPals')
    return {'native_guard_caller_pairs': cases, 'native_guard_actual_stores': stores,
            'native_guard_same_byte_stores': same_byte_stores, 'black_delay_tail_paths': delay_paths}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rom', type=Path, required=True)
    parser.add_argument('--sym', type=Path, required=True)
    parser.add_argument('--output', type=Path)
    parser.add_argument('--disabled-rom', type=Path)
    parser.add_argument('--disabled-sym', type=Path)
    args = parser.parse_args()
    image = (args.rom.read_bytes(), symbols(args.sym))
    result = verify(image)
    result.update(verify_guards_and_output(image))
    if args.disabled_rom:
        require(args.disabled_sym is not None, 'Disabled symbol file is required')
        result.update(verify_native_guard_callers(image, (args.disabled_rom.read_bytes(), symbols(args.disabled_sym))))
    text = json.dumps(result, indent=2) + '\n'
    if args.output:
        args.output.write_text(text)
    print(text)


if __name__ == '__main__':
    main()
