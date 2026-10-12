"""Reject map mutations while accepting relocated native script references."""
from pathlib import Path
import tempfile
import unittest
from audit_independent_maps import audit


class IndependentMapTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        for directory in ('data/maps', 'data/sprites'):
            (self.root / directory).mkdir(parents=True)
        (self.root / 'data/maps/attributes.asm').write_text('map_attributes First, 0\nmap_attributes Added, 0\n')
        (self.root / 'data/sprites/map_objects.asm').write_text('db 0\n' * 6)
        self.baseline = self.root / 'baseline.gbc'
        self.mobile = self.root / 'mobile.gbc'
        self.write(self.baseline, 0x500)
        self.write(self.mobile, 0x550, feature=True)

    def write(self, path, script, feature=False):
        data = bytearray(0x1000)
        names = {'SpriteMovementData': 0x600, 'NativeScript': script}
        for index, name in enumerate(('First', 'Added')):
            header, attribute = 0x100 + index * 0x100, 0x800 + index * 3
            names[name + '_MapScriptHeader'] = header
            names[name + '_MapAttributes'] = attribute
            data[attribute:attribute + 3] = bytes((0, 4, 5))
            data[header + 5] = 1
            record = bytearray((1, 8, 8, 2, 0, 0, 15, 0, 0, 0, 0, 255, 255))
            record[9:11] = script.to_bytes(2, 'little')
            data[header + 6:header + 19] = record
        if feature:
            names['MobileVirtualActorVersion'] = 0x900
        path.write_bytes(data)
        path.with_suffix('.sym').write_text(''.join(f'00:{value:04x} {name}\n' for name, value in names.items()))

    def test_added_maps_and_relocated_scripts_need_no_permissions(self):
        result = audit(self.baseline, self.mobile, self.root)
        self.assertEqual(result['map_count'], 2)
        self.assertEqual([row['required_remote_object_slots'] for row in result['maps']], [0, 0])

    def test_changed_native_actor_or_unknown_target_fails(self):
        for offset in (0x107, 0x10f):
            self.write(self.mobile, 0x550, feature=True)
            data = bytearray(self.mobile.read_bytes())
            data[offset] ^= 1
            self.mobile.write_bytes(data)
            with self.assertRaises(ValueError):
                audit(self.baseline, self.mobile, self.root)

    def test_lease_code_and_changed_format_fail(self):
        with self.mobile.with_suffix('.sym').open('a') as stream:
            stream.write('00:0901 MobileNativeLeaseSlot\n')
        with self.assertRaises(ValueError):
            audit(self.baseline, self.mobile, self.root)
        self.write(self.mobile, 0x550, feature=True)
        data = bytearray(self.mobile.read_bytes())
        data[0x105] = 21
        self.mobile.write_bytes(data)
        with self.assertRaises(ValueError):
            audit(self.baseline, self.mobile, self.root)


if __name__ == '__main__':
    unittest.main()
