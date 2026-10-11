#!/usr/bin/env python3
"""Link isolated ROM-size candidates and restore all production sources.

These are byte measurements, not tested implementations. Later candidates remove
the prototype menu and one-shot echo diagnostic. Never use their ROMs as a release.
Run serially with other builds. Only feature-owned assembly is temporarily edited;
ordinary game code, production ROMs, emulator processes and saves are untouched.
"""
import argparse
import json
from pathlib import Path
import re
import shutil
import subprocess
from audit_rom_budget import measure
from probe_emulator import digest, read_symbols, require

REPO = Path(__file__).resolve().parents[2]


def replace_once(text, old, new):
    require(text.count(old) == 1, "Candidate source contract changed: " + old[:60])
    return text.replace(old, new)


def remove_range(text, start, end):
    begin, finish = text.index(start), text.index(end)
    require(begin < finish, "Candidate range is reversed")
    return text[:begin] + text[finish:]


def cleanup(sources):
    sources["adapter.asm"] = remove_range(sources["adapter.asm"], "MobileValidateResponse:\n", "MobileConsumeResponse:\n")
    sources["session.asm"] = remove_range(sources["session.asm"], "MobileComputeInstance:\n", "MobileWorldPixel:\n")
    sources["session.asm"] = remove_range(sources["session.asm"], "MobileHelloVector::\n", "MobilePollVector:\n")
    sources["native_actors.asm"] = remove_range(sources["native_actors.asm"], "MobileRenderSprites::\n", "MobileProjectPeer:\n")


def instrument(sources):
    sites = {}
    for name, text in sources.items():
        if name == "native_palette_macros.asm":
            continue
        scope, output = None, []
        for line in text.splitlines():
            label = re.match(r"^(\w+)::?", line)
            if label:
                scope = label[1]
            jump = re.match(r"\s*(?:jp|jmp)\s+(?:(?:nz|z|nc|c),\s*)?([\w.]+)\s*(?:;.*)?$", line)
            if jump and scope and jump[1] != "hl":
                marker = f".MobileBudgetJump{len(sites):04d}"
                target = scope + jump[1] if jump[1].startswith(".") else jump[1]
                sites[scope + marker] = {"source": name, "marker": marker, "target": target}
                output.append(marker + ":")
            output.append(line)
        sources[name] = "\n".join(output) + "\n"
    return sites


def shorten_jumps(sources, sites, symbols):
    eligible = {}
    for name, site in sites.items():
        if name not in symbols or site["target"] not in symbols:
            continue
        origin, target = symbols[name], symbols[site["target"]]
        if origin[0] == target[0] and -128 <= target[1] - origin[1] - 2 <= 127:
            eligible[site["marker"]] = site
    changed = 0
    for source, text in sources.items():
        lines = text.splitlines()
        for index, line in enumerate(lines[:-1]):
            if line.removesuffix(":") in eligible:
                lines[index + 1], count = re.subn(r"^(\s*)(?:jp|jmp)(\s+)", r"\1jr\2", lines[index + 1])
                changed += count
        sources[source] = "\n".join(lines) + "\n"
    return changed


def compact_policy(sources):
    text = sources["native_lease_policy.asm"]
    indices = [int(value) for value in re.findall(r"^\s*db (\d+) ; \w+$", text, re.M)]
    offsets = [int(value) for value in re.findall(r"dw MobileLeaseMapIndices \+ (\d+)", text)] + [len(indices)]
    require(len(indices) == 611 and len(offsets) == 40, "Policy map/group inventory changed")
    # Prefix population count per eight-map chunk keeps lookup O(1): no scanning
    # an 86-map group or an arbitrary-length compressed stream at runtime.
    groups, recovered, lookup_bound = [], [], 0
    for start, end in zip(offsets, offsets[1:]):
        values = indices[start:end]
        entries = [value for value in values if value]
        chunks, prefix = [], 0
        for at in range(0, len(values), 8):
            byte = sum(1 << bit for bit, value in enumerate(values[at:at + 8]) if value)
            chunks.extend((byte, prefix))
            prefix += byte.bit_count()
        groups.append((chunks, entries))
        for number, expected in enumerate(values):
            bitmap, prefix = chunks[2 * (number // 8):2 * (number // 8) + 2]
            bit = number % 8
            actual = entries[prefix + (bitmap & ((1 << bit) - 1)).bit_count()] if bitmap & (1 << bit) else 0
            require(actual == expected, "Compressed policy changes a map's approved mask")
            recovered.append(actual)
            lookup_bound = max(lookup_bound, bit)
    # Two words per group: chunk base and packed nonzero-index base. The small
    # population-count loop examines at most seven bits, independent of map count.
    loader = '''MobileNativeLoadLeasePolicy:
\tld a, [wMapGroup]
\tdec a
\tadd a
\tadd a
\tld e, a
\tld d, 0
\tld hl, MobileLeaseGroupOffsets
\tadd hl, de
\tld a, [hli]
\tld e, a
\tld a, [hli]
\tld d, a
\tld a, [hli]
\tld h, [hl]
\tld l, a
\tpush hl
\tld a, [wMapNumber]
\tdec a
\tld c, a
\tand 7
\tld b, a
\tld a, c
\tand $f8
\trrca
\trrca
\tld l, a
\tld h, 0
\tadd hl, de
\tld a, [hli]
\tld c, [hl]
\tinc b
.rank
\trrca
\tdec b
\tjr z, .selected
\tjr nc, .rank
\tinc c
\tjr .rank
.selected
\tpop hl
\tld a, 0
\tjr nc, .index
\tld e, c
\tld d, 0
\tadd hl, de
\tld a, [hl]
.index
\tld l, a
\tld h, 0
\tld d, h
\tld e, l
\tadd hl, hl
\tadd hl, de
\tld de, MobileLeaseMasks
\tadd hl, de
\tld a, MOBILE_WRAM_BANK
\tldh [rWBK], a
\tld de, wMobileLeaseMask
\tld bc, 3
\trst CopyBytes
\tld a, 1
\tldh [rWBK], a
\tret
MobileLeaseGroupOffsets:
'''
    for group in range(len(groups)):
        loader += f"\tdw .chunks{group}, .indices{group}\n"
    for group, (chunks, entries) in enumerate(groups):
        loader += f".chunks{group}:\n\tdb " + ", ".join(map(str, chunks)) + "\n"
        loader += f".indices{group}:\n"
        if entries:
            loader += "\tdb " + ", ".join(map(str, entries)) + "\n"
    begin, end = text.index("MobileNativeLoadLeasePolicy:"), text.index("MobileLeaseMasks:")
    sources["native_lease_policy.asm"] = text[:begin] + loader + text[end:]
    return {"maps_compared": len(recovered), "chunks_and_indices_bytes": sum(len(a) + len(b) for a, b in groups),
            "group_pointer_bytes": 4 * len(groups), "maximum_rank_bits": lookup_bound,
            "scope": "Host table equivalence and link only; executed decoder/cadence not validated"}


def no_echo(sources):
    text = sources["adapter.asm"]
    text = replace_once(text, '''\tld a, [wMobileMode]
\tand a
\tjr nz, .hello
\tld [wMobileWanted], a ; consume one diagnostic request, never auto-repeat it
\tjr .claim
.hello
\tcall MobileBuildHello
''', '\tcall MobileBuildHello\n')
    text = replace_once(text, '\tld a, [wMobileMode]\n\tand a\n\tjr z, .echo\n', '')
    text = remove_range(text, '.echo\n', '.size\n\tld a, c')
    begin, end = text.index('.copy\n\tld a, [wMobileMode]'), text.index('.copied\n\tld a, [wMobileStep]')
    text = text[:begin] + '''\tld a, [wMobileStep]
\tcp 4
\tjr nz, .copy_byte
\tinc de
\tdec c
\tjr z, .copied
.copy_byte
\tld a, [hli]
\tld [de], a
\tinc de
\tdec c
\tjr nz, .copy_byte
''' + text[end:]
    begin, end = text.index('\tld a, [wMobileStep]\n\tcp 4\n\tjr nz, .header\n', text.index('.connection\n')), text.index('.header\n\tpop bc')
    text = text[:begin] + text[end:]
    begin = text.index('\n.data\n', text.index('MobileConsumeResponse:\n')) + 1
    end = text.index('.advance\n\tld hl, wMobileStep', begin)
    text = text[:begin] + '.data\n\tjp MobileAppendLobbyData\n' + text[end:]
    text = remove_range(text, 'MobileRequestVector::\n', 'MobileReplyHeader:\n')
    text = replace_once(text, '\tdb $50, $43, 1, 2\n', '\tdb $50, $43, 1\n')
    sources['adapter.asm'] = text
    sources['session.asm'] = replace_once(sources['session.asm'], '\tld a, [wMobileMode]\n\tand a\n\tret z\n', '')
    sources['native_palettes.asm'] = replace_once(sources['native_palettes.asm'], '''\txor a
\tbit 0, c ; A adds the offline one-shot echo diagnostic
\tjr nz, .mode
\tinc a
.mode
''', '\tld a, 1\n')


def audit_type_tags(baseline):
    """Check a possible zero-growth metadata encoding, without changing a ROM.

    Tagged bytes must never reach ordinary game RAM. A normalizing load seam and
    a bounded ROM reader would still need implementation and runtime/save tests.
    """
    rom, symbols = baseline.read_bytes(), read_symbols(baseline.with_suffix('.sym'))
    rows = json.loads((REPO / 'mobile_adapter/research/native-lease-policy.json').read_text())['maps']
    objects, approved = 0, 0
    for row in rows:
        bank, address = symbols[row['map'] + '_MapScriptHeader']
        cursor = bank * 0x4000 + address - 0x4000
        for size in (2, 3, 5, 5, 5):
            cursor += 1 + rom[cursor] * size
        count, expected = rom[cursor], set(row['approved_indices'])
        require(count == row['objects'] and count <= 20, 'Map object inventory changed')
        require(all(1 <= index <= count for index in expected), 'Approved object index is invalid')
        recovered = set()
        for index in range(1, count + 1):
            value = rom[cursor + 1 + 13 * (index - 1) + 7]
            require(not value & 0x80, 'Object type bit 7 is already occupied')
            tagged = value | (0x80 if index in expected else 0)
            require(tagged & 0x7f == value, 'Normalizing the tag changes an ordinary object type')
            if tagged & 0x80:
                recovered.add(index)
            objects += 1
        require(recovered == expected, 'Type tags change the audited allowlist')
        approved += len(recovered)
    return {'maps_compared': len(rows), 'object_type_bytes_checked': objects,
            'approved_flags_recovered': approved, 'existing_bit7_uses': 0,
            'added_map_record_bytes': 0,
            'scope': 'Host byte transform only. Not an implemented ROM reader, save or timing guarantee.'}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cache', type=Path, default=Path('/Volumes/Storage/Cache/mobileadapter'))
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    output = args.output.resolve()
    require(not output.exists(), 'Choose a fresh probe directory')
    output.mkdir(parents=True)
    paths = list((REPO / 'mobile_adapter/rom').glob('*.asm'))
    originals = {path: path.read_bytes() for path in paths}
    sources = {path.name: data.decode() for path, data in originals.items()}
    artifacts = {path: digest(path) for pattern in ('polishedcrystal*.gbc', 'polishedcrystal*.map', 'polishedcrystal*.sym')
                 for path in REPO.glob(pattern)}
    baseline = REPO / 'polishedcrystal-3.2.3.gbc'
    production = measure(baseline, REPO / 'polishedcrystal-mobile-3.2.3.gbc')
    tags = audit_type_tags(baseline)
    reports = []

    def build(name, note):
        for path in paths:
            path.write_text(sources[path.name])
        with (output / (name + '.log')).open('w') as log:
            subprocess.run(['make', '-j4', 'mobile', f'RGBDS={args.cache / "rgbds-1.0.4"}/',
                            f'MOBILE_OBJ_DIR={output / "objects"}', f'ROM_NAME={output / name}'],
                           cwd=REPO, stdout=log, stderr=subprocess.STDOUT, check=True)
        report = measure(baseline, output / (name + '.gbc'))
        report.update({'candidate': name, 'note': note,
                       'savings_from_production_bytes': production['net_addon_bytes'] - report['net_addon_bytes']})
        reports.append(report)
        print(f"{name}: {report['net_addon_bytes']} net bytes, {report['savings_from_production_bytes']} saved", flush=True)
        return read_symbols(output / (name + '.sym'))

    try:
        cleanup(sources)
        sites = instrument(sources)
        symbols = build('cleanup', 'Remove 93 unused routine bytes and relocate the 23-byte host-only HELLO fixture.')
        changed = shorten_jumps(sources, sites, symbols)
        build('short_jumps', f'{changed} statically in-range JP/JMP sites shortened to JR; runtime not validated.')
        # Drop instrumentation before rewriting the generated policy/echo ranges.
        for name in sources:
            sources[name] = re.sub(r'^\.MobileBudgetJump\d+:\n', '', sources[name], flags=re.M)
        policy = compact_policy(sources)
        build('compact_policy', 'Bounded prefix/bitmap policy lookup; same approved indices on all 611 maps.')
        for name in ('native_lease_policy.asm', 'native_instance.asm', 'native_tile_backup.asm', 'native_save.asm', 'native_palettes.asm'):
            sources[name], count = re.subn(r'(SECTION "Mobile [^"\n]+", ROMX)(?:, BANK\[MOBILE_ROM_BANK\])?\s*\n', r'\1, BANK[MOBILE_ROM_BANK]\n', sources[name])
            require(count == 1, 'Feature section placement changed')
        near = 0
        for name in sources:
            sources[name], count = re.subn(r'\bfar(call|jp)(\s+Mobile\w+)', r'\1\2', sources[name])
            near += count
        build('single_bank', f'All six feature sections in bank 128; {near} internal banked calls become near calls. Callee bank/register contracts need runtime validation.')
        no_echo(sources)
        build('without_echo', 'Move one-shot transport diagnostic to separate test ROM; persistent protocol and emergency recovery retained in candidate source.')
        sources['menu.asm'] = '''; Size-only candidate: retain chord consumption, move prototype controls to Bridge.
MobileMenuInput::
\tldh a, [hJoypadDown]
\tand PAD_SELECT | PAD_B
\tcp PAD_SELECT | PAD_B
\tcall z, MobileConsumeKey
\tret
'''
        build('bridge_controls', 'Remove prototype Select+A menu; Select+B join/disconnect remains. Replacement in-game social UI has not been budgeted.')
        result = {'production_unchanged': True, 'runtime_accepted': False, 'production': production,
                  'compact_policy': policy, 'type_tag_research': tags, 'candidates': reports}
    finally:
        for path, data in originals.items():
            path.write_bytes(data)
        require(all(digest(path) == expected for path, expected in artifacts.items()), 'Production build artifact changed')
        shutil.rmtree(output / 'objects', ignore_errors=True)
    (output / 'evidence.json').write_text(json.dumps(result, indent=2) + '\n')
    print('Sources and production ROM/map/symbol files restored; candidate runtime acceptance remains open.')


if __name__ == '__main__':
    main()
