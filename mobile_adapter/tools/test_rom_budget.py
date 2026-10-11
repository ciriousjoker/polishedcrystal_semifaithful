"""MBC30 regressions: placement, counterpart header, padding and integration."""
import tempfile
import unittest
from pathlib import Path
from audit_rom_budget import compare, layout, measure, validate_mobile


def mapping(home, banked, feature=0, feature_bank=128):
    count = max(128, feature_bank + 1) if feature else 128
    rows = [f'SUMMARY:\n\tROM0: {home} bytes used / {16384 - home} free\n'
            f'\tROMX: {banked + feature + 1} bytes used / {(count - 1) * 16384 - banked - feature - 1} free\n\n'
            f'ROM0 bank #0:\n\tSECTION: $0000 (${home:04x} bytes) ["Home"]\n']
    for bank in range(1, count):
        rows.append(f'ROMX bank #{bank}:\n')
        if bank == 1:
            rows.append(f'\tSECTION: $4000 (${banked:04x} bytes) ["Game"]\n'
                        '\tSECTION: $7fff ($0001 byte) ["Terminator"]\n')
        if feature and bank == feature_bank:
            rows.append(f'\tSECTION: $5000 (${feature:04x} bytes) ["Mobile Adapter"]\n')
    rows.append('SRAM bank #0:\n\tSECTION: $a000 ($0400 bytes) ["Mobile Not ROM"]\n')
    return ''.join(rows)


def rom(size=4, header=(0x10, 7, 3)):
    data = bytearray([0xff]) * (size * 1024 * 1024)
    data[0x147:0x14a] = bytes(header)
    return data


class RomBudgetTest(unittest.TestCase):
    def test_whole_rom_includes_integration_and_fragmented_free_space(self):
        report = compare(layout(mapping(100, 100)), layout(mapping(117, 100, 700)), 716)
        self.assertEqual(report['net_addon_bytes'], 717)
        self.assertEqual(report['integration_net_bytes'], 17)
        self.assertEqual(report['feature_section_bytes'], 700)
        self.assertEqual(report['required_savings_bytes'], 1)
        self.assertFalse(report['within_budget'])
        self.assertEqual(report['lower_free_bytes'], 2 * 1024 * 1024 - 218)

    def test_native_code_reduction_cannot_hide_feature_cost(self):
        report = compare(layout(mapping(500, 100)), layout(mapping(100, 100, 700)), 699)
        self.assertEqual(report['net_addon_bytes'], 300)
        self.assertEqual(report['accounted_bytes'], 700)
        self.assertFalse(report['within_budget'])

    def test_lower_integration_ceiling_cannot_be_funded_with_upper_space(self):
        report = compare(layout(mapping(100, 100)), layout(mapping(357, 100, 700)))
        self.assertFalse(report['within_budget'])
        self.assertEqual(report['integration_net_bytes'], 257)

    def test_exact_limit_passes(self):
        self.assertTrue(compare(layout(mapping(100, 100)), layout(mapping(100, 100, 700)), 700)['within_budget'])

    def test_missing_or_wrong_counterparts_fail(self):
        disabled, enabled = layout(mapping(100, 100)), layout(mapping(100, 100, 700))
        for pair in ((disabled, disabled), (enabled, enabled)):
            with self.assertRaises(ValueError):
                compare(*pair)

    def test_inconsistent_map_fails_instead_of_underreporting(self):
        source = mapping(100, 100, 700)
        for invalid in (source.replace('($02bc bytes)', '($02bb bytes)'),
                        source.replace('ROMX bank #1:', 'ROMX bank #1:\nROMX bank #1:')):
            with self.assertRaises(ValueError):
                layout(invalid)

    def test_missing_summary_fails(self):
        with self.assertRaises(ValueError):
            layout('ROM0 bank #0:\n')

    def test_upper_feature_and_lower_game_rules(self):
        validate_mobile(rom(), layout(mapping(100, 100, 700)))
        with self.assertRaisesRegex(ValueError, 'escaped the upper'):
            validate_mobile(rom(), layout(mapping(100, 100, 700, 127)))
        source = mapping(100, 100, 700).replace('["Mobile Adapter"]', '["Game Upper"]')
        source = source.replace('ROMX bank #127:', 'ROMX bank #127:\n\tSECTION: $4000 ($0000 bytes) ["Mobile Empty"]')
        with self.assertRaisesRegex(ValueError, 'escaped the upper'):
            validate_mobile(rom(), layout(source))
        source = mapping(100, 100, 700).replace('["Mobile Adapter"]', '["Game Upper"]\n\tSECTION: $7000 ($0000 bytes) ["Mobile Empty"]')
        with self.assertRaisesRegex(ValueError, 'Original game'):
            validate_mobile(rom(), layout(source))

    def test_unmapped_tail_cannot_hide_code(self):
        data = rom()
        data[-1] = 0
        with self.assertRaisesRegex(ValueError, 'unaccounted'):
            validate_mobile(data, layout(mapping(100, 100, 700)))

    def test_mapper_save_allocation_and_truncation_are_rejected(self):
        for data in (rom(2), rom(header=(0x10, 6, 3)), rom(header=(0x10, 7, 5)), rom(header=(0x1b, 7, 3))):
            with self.assertRaises(ValueError):
                validate_mobile(data, layout(mapping(100, 100, 700)))

    def test_measure_accepts_expansion_only_for_mobile(self):
        with tempfile.TemporaryDirectory() as folder:
            before, after = (Path(folder) / name for name in ('base.gbc', 'mobile.gbc'))
            before.write_bytes(rom(2, (0x10, 6, 3)))
            after.write_bytes(rom())
            before.with_suffix('.map').write_text(mapping(100, 100))
            after.with_suffix('.map').write_text(mapping(117, 100, 700))
            self.assertEqual(measure(before, after)['integration_net_bytes'], 17)
            before.write_bytes(rom())
            with self.assertRaisesRegex(ValueError, 'Disabled counterpart'):
                measure(before, after)


if __name__ == '__main__':
    unittest.main()
