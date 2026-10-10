"""Host-only contracts for manual-session deployment; no emulator or ADB."""
import ipaddress
from pathlib import Path
import tempfile
import unittest

import dev_session as session


class DeploymentContracts(unittest.TestCase):
    def test_lan_patch_changes_only_owned_endpoint_and_global_checksum(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            data = bytearray([0xff]) * (2 * 1024 * 1024)
            location = 0x7f * 0x4000 + 0x557
            data[location:location + 6] = ipaddress.IPv4Address("127.0.0.1").packed + session.PORT.to_bytes(2, "big")
            source, symbols, destination = root / "source", root / "symbols", root / "output"
            source.write_bytes(data)
            symbols.write_text("7f:4557 MobileEndpoint\n")
            session.lan_rom(source, symbols, destination, "192.168.2.25")
            result = destination.read_bytes()
            self.assertEqual(source.read_bytes(), data)
            self.assertEqual(result[location:location + 6], bytes([192, 168, 2, 25]) + session.PORT.to_bytes(2, "big"))
            changed = {i for i, (before, after) in enumerate(zip(data, result)) if before != after}
            self.assertTrue(changed <= set(range(location, location + 6)) | {0x14e, 0x14f})
            self.assertEqual(int.from_bytes(result[0x14e:0x150], "big"),
                             (sum(result[:0x14e]) + sum(result[0x150:])) & 0xffff)

    def test_unknown_endpoint_or_bad_symbol_cannot_be_patched(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            source, symbols, destination = root / "source", root / "symbols", root / "output"
            source.write_bytes(bytes(2 * 1024 * 1024))
            for value in ("00:4557", "7f:4557", "7f:7fff"):
                symbols.write_text(value + " MobileEndpoint\n")
                with self.assertRaises(ValueError):
                    session.lan_rom(source, symbols, destination, "192.168.2.25")
                self.assertFalse(destination.exists())

    def test_android_profile_preserves_touch_controls_and_uses_flat_stable_paths(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            target = "/storage/emulated/0/Download/polishedcrystal_mobile_adapter"
            base = 'input_overlay = "/existing/touch.cfg"\nvideo_driver = "vulkan"\nsavefile_directory = "/old"\n'
            session.profile(root, target, base, android=True, package="com.retroarch")
            result = (root / "retroarch.cfg").read_text()
            self.assertIn('input_overlay = "/existing/touch.cfg"', result)
            self.assertIn('video_driver = "vulkan"', result)
            self.assertIn(f'savefile_directory = "{target}"', result)
            self.assertIn(f'savestate_directory = "{target}"', result)
            self.assertEqual(result.count("\nsavefile_directory ="), 1)
            self.assertIn('savestate_auto_load = "true"', result)
            self.assertIn('savestate_auto_save = "false"', result)
            self.assertIn('dcgb_mobile_adatper_enabled = "1"', (root / "core-options.cfg").read_text())

    def test_mac_volume_is_five_percent_without_changing_system_volume(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            session.profile(root, root)
            result = (root / "retroarch.cfg").read_text()
            self.assertIn('audio_volume = "-26.0206"', result)
            self.assertAlmostEqual(10 ** (-26.0206 / 20), .05, places=6)


if __name__ == "__main__":
    unittest.main()
