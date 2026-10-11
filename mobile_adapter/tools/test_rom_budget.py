"""Budget regressions: fragmented banks, integration cost and non-ROM storage."""
import tempfile
import unittest
from pathlib import Path
from audit_rom_budget import compare, layout, measure


def mapping(home, banked, feature=0):
    section = f'\tSECTION: $4000 (${banked:04x} bytes) ["Game"]\n'
    if feature:
        section += f'\tSECTION: $5000 (${feature:04x} bytes) ["Mobile Adapter"]\n'
    # Real rgblink maps spell one-byte sections with singular "byte".
    return (f'SUMMARY:\n\tROM0: {home} bytes used / {16384 - home} free\n'
            f'\tROMX: {banked + feature + 1} bytes used / {16383 - banked - feature} free in 1 banks\n\n'
            f'ROM0 bank #0:\n\tSECTION: $0000 (${home:04x} bytes) ["Home"]\n'
            'ROMX bank #1:\n' + section + '\tSECTION: $7fff ($0001 byte) ["Terminator"]\n'
            'SRAM bank #0:\n\tSECTION: $a000 ($0400 bytes) ["Mobile Not ROM"]\n')


class RomBudgetTest(unittest.TestCase):
    def test_whole_rom_includes_integration_and_fragmented_free_space(self):
        before = layout(mapping(100, 100))
        after = layout(mapping(117, 100, 700))
        report = compare(before, after, 716)
        self.assertEqual(report['net_addon_bytes'], 717)
        self.assertEqual(report['integration_net_bytes'], 17)
        self.assertEqual(report['feature_section_bytes'], 700)
        self.assertEqual(report['required_savings_bytes'], 1)
        self.assertFalse(report['within_budget'])
        self.assertEqual(report['mobile_free_bytes'], 32768 - 918)
        self.assertEqual(len(report['feature_sections']), 1)

    def test_native_code_reduction_cannot_fund_feature_sections(self):
        report = compare(layout(mapping(500, 100)), layout(mapping(100, 100, 700)), 699)
        self.assertEqual(report['net_addon_bytes'], 300)
        self.assertEqual(report['accounted_bytes'], 700)
        self.assertFalse(report['within_budget'])

    def test_exact_limit_passes(self):
        report = compare(layout(mapping(100, 100)), layout(mapping(100, 100, 700)), 700)
        self.assertTrue(report['within_budget'])
        self.assertEqual(report['remaining_budget_bytes'], 0)

    def test_missing_or_wrong_counterparts_fail(self):
        disabled = layout(mapping(100, 100))
        enabled = layout(mapping(100, 100, 700))
        for arguments in ((disabled, disabled), (enabled, enabled)):
            with self.assertRaises(ValueError):
                compare(*arguments)

    def test_inconsistent_map_fails_instead_of_underreporting(self):
        source = mapping(100, 100, 700)
        with self.assertRaises(ValueError):
            layout(source.replace('($02bc bytes)', '($02bb bytes)'))
        with self.assertRaises(ValueError):
            layout(source.replace('ROMX bank #1:', 'ROMX bank #1:\nROMX bank #1:'))

    def test_missing_summary_fails(self):
        with self.assertRaises(ValueError):
            layout('ROM0 bank #0:\n')

    def test_mapper_expansion_cannot_silently_pass_current_budget(self):
        with tempfile.TemporaryDirectory() as temporary:
            before, after = (Path(temporary) / name for name in ('base.gbc', 'mobile.gbc'))
            rom = bytearray(2 * 1024 * 1024)
            rom[0x147:0x14a] = bytes((0x10, 6, 3))
            before.write_bytes(rom)
            rom[0x148] = 7
            after.write_bytes(rom)
            with self.assertRaisesRegex(ValueError, 'Mapper'):
                measure(before, after)


if __name__ == '__main__':
    unittest.main()
