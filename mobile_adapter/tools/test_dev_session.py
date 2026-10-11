"""Host-only contracts for manual-session deployment; no emulator or ADB."""
import ipaddress
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

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
            base = ('input_overlay = "/existing/touch.cfg"\nvideo_driver = "vulkan"\n'
                    'savefile_directory = "/old"\nglobal_core_options = "false"\n')
            session.profile(root, target, base, android=True, package="com.retroarch")
            result = (root / "retroarch.cfg").read_text()
            self.assertIn('input_overlay = "/existing/touch.cfg"', result)
            self.assertIn('video_driver = "vulkan"', result)
            self.assertIn(f'savefile_directory = "{target}"', result)
            self.assertIn(f'savestate_directory = "{target}"', result)
            self.assertEqual(result.count("\nsavefile_directory ="), 1)
            self.assertIn('savestate_auto_load = "true"', result)
            self.assertIn('savestate_auto_save = "false"', result)
            self.assertIn('global_core_options = "true"', result)
            self.assertNotIn('global_core_options = "false"', result)
            self.assertIn('game_specific_options = "false"', result)
            self.assertIn(f'core_options_path = "{target}/core-options.cfg"', result)
            self.assertIn('libretro_log_level = "0"', result)
            self.assertIn('dcgb_mobile_adatper_enabled = "1"', (root / "core-options.cfg").read_text())

    def test_mac_volume_is_five_percent_without_changing_system_volume(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            session.profile(root, root)
            result = (root / "retroarch.cfg").read_text()
            self.assertIn('audio_volume = "-26.0206"', result)
            self.assertIn('pause_nonactive = "false"', result)
            self.assertIn('input_player1_select = "shift"', result)
            self.assertAlmostEqual(10 ** (-26.0206 / 20), .05, places=6)

    def test_core_marker_invalidates_after_core_or_app_replacement(self):
        with tempfile.TemporaryDirectory() as folder:
            core = Path(folder) / "core.so"
            core.write_bytes(b"first core")
            details = "versionCode=1 minSdk=16\nfirstInstallTime=2026-10-10 12:00\nlastUpdateTime=2026-10-10 12:00\n"
            record = session.core_install_record(core, "com.retroarch", details)
            self.assertEqual(record, session.core_install_record(core, "com.retroarch", details))
            core.write_bytes(b"second core")
            self.assertNotEqual(record, session.core_install_record(core, "com.retroarch", details))
            core.write_bytes(b"first core")
            self.assertNotEqual(record, session.core_install_record(core, "com.retroarch", details.replace("versionCode=1", "versionCode=2")))
            self.assertNotEqual(record, session.core_install_record(core, "com.retroarch", details.replace("lastUpdateTime=2026-10-10 12:00", "lastUpdateTime=2026-10-10 13:00")))
            with self.assertRaises(ValueError):
                session.core_install_record(core, "com.retroarch", "")

    def test_frontend_status_reports_initialization_without_requiring_osd_in_logs(self):
        self.assertEqual(session.frontend_status("[Mobile] Adapter initialized\n"),
                         {"initialized": True, "state": False})
        self.assertEqual(session.frontend_status("Mobile Adapter GB plugged in\n"),
                         {"initialized": False, "state": False})
        self.assertEqual(session.frontend_status("Auto-loading save state from x succeeded.\n"),
                         {"initialized": False, "state": True})
        self.assertEqual(session.frontend_status("Auto-loading save state from x failed.\n"),
                         {"initialized": False, "state": False})

    def test_server_labels_are_derived_from_peer_ip(self):
        for peer, expected in (("192.168.2.25", "mac"), ("192.168.2.23", "android"), ("192.168.2.99", "unknown")):
            line = f"CLIENT_CONNECTED session=1 peer={peer}:12345\n"
            self.assertEqual(session.labelled_server_line(line, "192.168.2.25", "192.168.2.23"),
                             line.rstrip() + f" client={expected}\n")
        echo = "ECHO session=1 sequence=1 bytes=4\n"
        self.assertEqual(session.labelled_server_line(echo, "192.168.2.25"), echo)

    def test_android_launch_stops_app_and_uses_one_explicit_game_intent(self):
        class Device:
            package = "com.retroarch"
            root = "/storage/emulated/0/Download/polishedcrystal_mobile_adapter"
            calls = []

            def shell(self, *command):
                self.calls.append(command)
                return ("[Mobile] Adapter initialized\n"
                        "Auto-loading save state from x succeeded.\n")

            def exists(self, path):
                return True

        device = Device()
        intent = session.android_intent(device.package, device.root)
        with patch("builtins.print"):
            session.launch_android(device, intent)
        self.assertEqual(device.calls[0], ("am", "force-stop", "com.retroarch"))
        starts = [command for command in device.calls if command[:2] == ("am", "start")]
        self.assertEqual(len(starts), 1)
        self.assertEqual(starts[0][starts[0].index("-f") + 1], "0x10008000")
        self.assertIn("com.retroarch/com.retroarch.browser.retroactivity.RetroActivityFuture", starts[0])
        self.assertNotIn("CoreSideloadActivity", " ".join(starts[0]))
        extras = {starts[0][i + 1]: starts[0][i + 2] for i, item in enumerate(starts[0]) if item == "--es"}
        self.assertEqual(extras, {
            "ROM": device.root + "/polishedcrystal-mobile.gbc",
            "LIBRETRO": "/data/data/com.retroarch/cores/DoubleCherryGB_libretro.so",
            "CONFIGFILE": device.root + "/retroarch.cfg"})


if __name__ == "__main__":
    unittest.main()
