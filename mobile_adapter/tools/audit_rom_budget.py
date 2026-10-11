#!/usr/bin/env python3
"""Guard the MBC30 split and measure feature and lower-half integration storage.

The disabled counterpart stays 2 MiB. Banked feature sections belong exclusively
in banks 128..255; original game sections stay below bank 128. No ordinary game
code reduction can disguise the separately reported feature-owned byte count.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re

REPO = Path(__file__).resolve().parents[2]
ROM_BUDGET_BYTES = 2 * 1024 * 1024
LOWER_INTEGRATION_LIMIT_BYTES = 256


def layout(text):
    totals = {}
    for kind in ('ROM0', 'ROMX'):
        match = re.search(rf'^\s*{kind}: (\d+) bytes used / (\d+) free', text, re.M)
        if not match:
            raise ValueError(f'Missing {kind} map summary')
        totals[kind] = {'used': int(match[1]), 'free': int(match[2])}
    banks, sections, current = {}, [], None
    for line in text.splitlines():
        match = re.match(r'(\w+) bank #(\d+):', line)
        if match:
            current = (match[1], int(match[2])) if match[1] in totals else None
            if current is not None:
                if current in banks:
                    raise ValueError('Duplicate ROM bank')
                banks[current] = 0
        match = re.search(r'SECTION:.*\(\$([0-9a-fA-F]+) bytes?\) \["([^"\n]+)"\]', line)
        if match and current is not None:
            size = int(match[1], 16)
            banks[current] += size
            sections.append({'name': match[2], 'kind': current[0], 'bank': current[1], 'bytes': size})
    for kind, total in totals.items():
        selected = [size for (region, _), size in banks.items() if region == kind]
        if (not selected or any(size > 0x4000 for size in selected) or
                sum(selected) != total['used'] or
                sum(selected) + total['free'] != len(selected) * 0x4000):
            raise ValueError(f'Inconsistent {kind} section inventory/summary')
    return {'used': sum(total['used'] for total in totals.values()),
            'free': sum(total['free'] for total in totals.values()),
            'rom0_free': totals['ROM0']['free'], 'sections': sections,
            'lower_used': sum(s['bytes'] for s in sections if s['bank'] < 128),
            'feature_sections': [s for s in sections if s['name'].startswith('Mobile ')],
            'banks': [{'kind': kind, 'bank': bank, 'free': 0x4000 - size}
                      for (kind, bank), size in banks.items()]}


def validate_mobile(rom, mapping):
    if len(rom) != 4 * 1024 * 1024 or rom[0x147:0x14a] != bytes((0x10, 7, 3)):
        raise ValueError('Mobile requires 4 MiB MBC30 with unchanged 32 KiB RTC battery allocation')
    features = mapping['feature_sections']
    if not features:
        raise ValueError('Mobile ROM sections are missing')
    if any(s['kind'] != 'ROMX' or not 128 <= s['bank'] <= 255 for s in features):
        raise ValueError('Banked Mobile section escaped the upper 2 MiB')
    if any(s['bank'] >= 128 and not s['name'].startswith('Mobile ') for s in mapping['sections']):
        raise ValueError('Original game section escaped the lower 2 MiB')
    capacity = mapping['used'] + mapping['free']
    # rgbfix pads to a power of two after rgblink writes its map. Unrepresented
    # tail banks are padding, never a source of unaccounted executable bytes.
    last = max(bank['bank'] for bank in mapping['banks'])
    if capacity != (last + 1) * 0x4000 or not 128 <= last <= 255:
        raise ValueError('Map ROM capacity does not match contiguous MBC30 banks')
    if rom[capacity:] != bytes([0xff]) * (len(rom) - capacity):
        raise ValueError('ROM padding beyond mapped banks contains unaccounted data')
    return {'rom_bytes': len(rom), 'save_ram_bytes': 32768,
            'upper_feature_bytes': sum(s['bytes'] for s in features),
            'lower_free_bytes': 2 * 1024 * 1024 - mapping['lower_used'],
            'feature_sections': features, 'upper_placement_passed': True}


def compare(baseline, mobile, limit=ROM_BUDGET_BYTES):
    if limit <= 0:
        raise ValueError('ROM budget must be positive')
    if baseline['feature_sections']:
        raise ValueError('Disabled counterpart contains Mobile ROM sections')
    if not mobile['feature_sections']:
        raise ValueError('Mobile ROM sections are missing')
    net = mobile['used'] - baseline['used']
    gross = sum(s['bytes'] for s in mobile['feature_sections'])
    integration = mobile['lower_used'] - baseline['lower_used']
    accounted = max(net, gross)
    return {'budget_bytes': limit, 'lower_integration_limit_bytes': LOWER_INTEGRATION_LIMIT_BYTES,
            'net_addon_bytes': net, 'feature_section_bytes': gross,
            'integration_net_bytes': integration, 'accounted_bytes': accounted,
            'within_budget': accounted <= limit and integration <= LOWER_INTEGRATION_LIMIT_BYTES,
            'required_savings_bytes': max(0, accounted - limit),
            'remaining_budget_bytes': limit - accounted,
            'baseline_free_bytes': baseline['free'],
            'mobile_free_bytes': 4 * 1024 * 1024 - mobile['used'],
            'mobile_rom0_free_bytes': mobile['rom0_free'],
            'lower_free_bytes': 2 * 1024 * 1024 - mobile['lower_used'],
            'upper_free_bytes': 2 * 1024 * 1024 - gross,
            'feature_sections': mobile['feature_sections'], 'mobile_rom_banks': mobile['banks']}


def measure(baseline_path, mobile_path, limit=ROM_BUDGET_BYTES):
    before, after = baseline_path.read_bytes(), mobile_path.read_bytes()
    if len(before) != 2 * 1024 * 1024 or before[0x147:0x14a] != bytes((0x10, 6, 3)):
        raise ValueError('Disabled counterpart must remain 2 MiB with its original mapper/save allocation')
    baseline, mobile = [layout(p.with_suffix('.map').read_text()) for p in (baseline_path, mobile_path)]
    validate_mobile(after, mobile)
    if baseline['used'] + baseline['free'] != len(before):
        raise ValueError('Disabled map ROM capacity differs from its ROM file')
    report = compare(baseline, mobile, limit)
    report.update({'baseline_sha256': hashlib.sha256(before).hexdigest(),
                   'mobile_sha256': hashlib.sha256(after).hexdigest(),
                   'scope': 'Matching source/options, freshly linked maps. Placement/bytes only; no runtime acceptance.'})
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--baseline', type=Path, default=REPO / 'polishedcrystal-3.2.3.gbc')
    parser.add_argument('--mobile', type=Path, default=REPO / 'polishedcrystal-mobile-3.2.3.gbc')
    parser.add_argument('--output', type=Path)
    parser.add_argument('--layout-only', action='store_true', help='Guard one mobile link without a counterpart build')
    parser.add_argument('--strict', action='store_true', help='Enforce upper capacity and the 256-byte lower integration ceiling')
    args = parser.parse_args()
    if args.layout_only:
        report = validate_mobile(args.mobile.read_bytes(), layout(args.mobile.with_suffix('.map').read_text()))
        print(f"MBC30 placement passed: {report['upper_feature_bytes']:,} feature bytes upstairs; {report['lower_free_bytes']:,} bytes free downstairs")
    else:
        report = measure(args.baseline, args.mobile)
        print(f"MBC30 addon: {report['feature_section_bytes']:,} upper bytes; {report['integration_net_bytes']:,} net lower integration bytes; {report['lower_free_bytes']:,} lower bytes free")
        if args.strict and not report['within_budget']:
            raise SystemExit('MBC30 ROM budget failed: upper capacity or minimal lower integration ceiling exceeded')
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + '\n')


if __name__ == '__main__':
    main()
