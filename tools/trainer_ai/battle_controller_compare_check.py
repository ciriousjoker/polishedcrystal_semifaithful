"""Model-boundary regressions for the full-state oracle's fast byte comparison."""
import unittest
from battle_controller_check import memory_differences
from stateful_fixture_check import State


class MemoryComparisonTests(unittest.TestCase):
    def setUp(self):
        self.reference, self.native = State(), State()

    def test_equal_states(self):
        for banked in (False, True):
            self.assertEqual(memory_differences(self.reference, self.native, banked), [])

    def test_every_declared_bank_and_fixed_region(self):
        # Six shadow banks, canonical bank 1, fixed bank 0, raw SVBK and HRAM.
        # Include both ends to detect length and endpoint omissions.
        regions = [(bank * 0x1000, bank * 0x1000 + 0xfff) for bank in range(6)]
        regions += [(0xc000, 0xcfff), (0xd000, 0xdfff), (0xff70,), (0xff80, 0xfffe)]
        for region in regions:
            for address in region:
                with self.subTest(address=hex(address)):
                    self.native.m[address] = 129
                    self.assertEqual(memory_differences(self.reference, self.native, True), [address])
                    self.native.m[address] = 0

    def test_flat_mode_has_only_fixed_bank1_and_hram(self):
        for address in (0xc000, 0xcfff, 0xd000, 0xdfff, 0xff80, 0xfffe):
            self.native.m[address] = 255
        for address in (0, 0x5fff, 0xff70):
            self.native.m[address] = 1
        self.assertEqual(memory_differences(self.reference, self.native, False),
                         [0xc000, 0xcfff, 0xd000, 0xdfff, 0xff80, 0xfffe])

    def test_unmodeled_gaps_and_mmio_are_not_added(self):
        for address in (0x6000, 0x7fff, 0x8000, 0xbfff, 0xe000, 0xff04,
                        0xff10, 0xff40, 0xff48, 0xff6f, 0xff71, 0xff7f, 0xffff):
            self.native.m[address] = 1
        self.assertEqual(memory_differences(self.reference, self.native, True), [])
        self.assertEqual(memory_differences(self.reference, self.native, False), [])

    def test_diagnostic_address_order_and_multiple_changes(self):
        for address in (0xff70, 0x5fff, 0, 0xfffe, 0xff80, 0xdfff, 0xc000):
            self.native.m[address] = 1
        self.assertEqual(memory_differences(self.reference, self.native, True),
                         [0xc000, 0xdfff, 0xff80, 0xfffe, 0, 0x5fff, 0xff70])


if __name__ == '__main__':
    unittest.main()
