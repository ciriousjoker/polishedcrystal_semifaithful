"""Host-only contracts for manual-session deployment; no emulator or ADB."""
import ipaddress
import os
from pathlib import Path
import signal
import socket
import subprocess
import sys
import tempfile
import time
from types import SimpleNamespace
import unittest
from unittest.mock import patch

import dev_session as session


class DeploymentContracts(unittest.TestCase):
    def test_bridge_modes_share_one_household_and_hosted_delay(self):
        args = SimpleNamespace(mode="local", lobby=73, upstream=None,
                               hosted_port=18787, test_one_way_delay_ms=80)
        commands = session.bridge_commands(args, "192.168.1.10")
        self.assertEqual(len(commands), 1)
        self.assertIn("192.168.1.10:17423", commands[0][0])
        self.assertNotIn("--upstream", commands[0][0])
        args.mode = "hosted"
        commands = session.bridge_commands(args, "192.168.1.10")
        self.assertEqual(len(commands), 2)
        self.assertEqual(commands[0][0][-2:], ["--test-one-way-delay-ms", "80"])
        self.assertEqual(commands[1][0][-2:], ["--upstream", "ws://127.0.0.1:18787"])
        args.mode = "remote"
        with self.assertRaisesRegex(ValueError, "requires --upstream"):
            session.bridge_commands(args, "127.0.0.1")
        args.upstream = "wss://example.test"
        commands = session.bridge_commands(args, "127.0.0.1")
        self.assertEqual(len(commands), 1)
        self.assertEqual(commands[0][0][-2:], ["--upstream", args.upstream])

    def test_free_port_never_prompts_or_stops_a_process(self):
        with patch.object(session, "port_available", return_value=True), patch.object(session, "port_listeners") as owners, patch.object(session.os, "kill") as kill:
            session.ensure_port_available("127.0.0.1", ask=lambda _: self.fail("Unexpected prompt"))
        owners.assert_not_called()
        kill.assert_not_called()

    def test_declined_or_closed_restart_prompt_preserves_the_listener(self):
        for answer in ("", "n", "no", None):
            def ask(_):
                if answer is None:
                    raise EOFError
                return answer
            with patch.object(session, "port_available", return_value=False), patch.object(session, "port_listeners", return_value={123: "test-server"}), patch.object(session.os, "kill") as kill, patch("builtins.print"):
                with self.assertRaises(session.SessionCancelled):
                    session.ensure_port_available("127.0.0.1", interactive=True, ask=ask)
                kill.assert_not_called()

    def test_unattended_restart_never_kills_a_port_owner(self):
        with patch.object(session, "port_available", return_value=False), patch.object(session, "port_listeners", return_value={123: "test-server"}), patch.object(session.os, "kill") as kill, patch("builtins.print"):
            with self.assertRaisesRegex(ValueError, "interactive terminal"):
                session.ensure_port_available("127.0.0.1", interactive=False)
            kill.assert_not_called()

    def test_changed_port_owner_requires_a_new_approval(self):
        with patch.object(session, "port_available", return_value=False), patch.object(session, "port_listeners", side_effect=[{123: "old"}, {456: "new"}]), patch.object(session.os, "kill") as kill, patch("builtins.print"):
            with self.assertRaisesRegex(ValueError, "owner changed"):
                session.ensure_port_available("127.0.0.1", interactive=True, ask=lambda _: "yes")
            kill.assert_not_called()

    def test_approved_restart_terminates_only_reviewed_listeners(self):
        with patch.object(session, "port_available", side_effect=[False, False, True]), patch.object(session, "port_listeners", side_effect=[{123: "server", 456: "other"}, {123: "server"}]), patch.object(session.os, "kill") as kill, patch.object(session.time, "sleep"), patch("builtins.print"):
            session.ensure_port_available("127.0.0.1", interactive=True, ask=lambda _: "y")
            kill.assert_called_once_with(123, signal.SIGTERM)

    def test_bind_probe_distinguishes_a_listener_from_a_free_port(self):
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            port = listener.getsockname()[1]
            listener.listen()
            self.assertFalse(session.port_available("127.0.0.1", port))
        self.assertTrue(session.port_available("127.0.0.1", port))

    def test_listener_that_ignores_shutdown_is_not_force_killed(self):
        with patch.object(session, "port_available", return_value=False), patch.object(session, "port_listeners", return_value={123: "server"}), patch.object(session.os, "kill") as kill, patch.object(session.time, "monotonic", side_effect=[0, 6]), patch("builtins.print"):
            with self.assertRaisesRegex(ValueError, "no forced kill"):
                session.ensure_port_available("127.0.0.1", interactive=True, ask=lambda _: "y")
            kill.assert_called_once_with(123, signal.SIGTERM)

    @unittest.skipUnless(Path("/usr/sbin/lsof").exists(), "macOS listener discovery")
    def test_approved_restart_frees_a_real_test_listener(self):
        code = ('import socket, time; s=socket.socket(); s.bind(("127.0.0.1", 0)); '
                's.listen(); print(s.getsockname()[1], flush=True); time.sleep(30)')
        listener = subprocess.Popen([sys.executable, "-c", code], stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE, text=True, start_new_session=True)
        try:
            port = int(listener.stdout.readline())
            self.assertIn(listener.pid, session.port_listeners(port))
            with patch("builtins.print"):
                session.ensure_port_available("127.0.0.1", port, interactive=True, ask=lambda _: "yes")
            listener.communicate(timeout=5)
            self.assertEqual(listener.returncode, -signal.SIGTERM)
            self.assertTrue(session.port_available("127.0.0.1", port))
        finally:
            if listener.poll() is None:
                listener.kill()
                listener.communicate()

    def test_declined_restart_has_a_success_exit_and_no_failure_message(self):
        with patch.object(session, "main", side_effect=session.SessionCancelled), patch("builtins.print") as output:
            self.assertEqual(session.entrypoint(), 0)
        self.assertIn("cancelled", output.call_args.args[0])
        self.assertNotIn("failed", output.call_args.args[0])

    def test_terminal_ctrl_c_releases_server_but_keeps_gui_processes_alive(self):
        with tempfile.TemporaryDirectory() as folder, socket.socket() as reservation:
            root = Path(folder)
            reservation.bind(("127.0.0.1", 0))
            port = reservation.getsockname()[1]
            reservation.close()
            server = root / "mobile_adapter/server/target/debug/mobile-hosted"
            server.parent.mkdir(parents=True)
            server.write_text(f'#!{sys.executable}\nimport os, socket, sys, time\n'
                              f'open({str(root / "server.pid")!r}, "w").write(str(os.getpid()))\n'
                              'address, port = sys.argv[2].split(":")\n'
                              'listener = socket.socket()\nlistener.bind((address, int(port)))\nlistener.listen()\n'
                              'print("MOBILE_HOSTED_READY " + sys.argv[2] + " ready", flush=True)\ntime.sleep(30)\n')
            bridge = root / "bridge"
            bridge.write_text(f'#!{sys.executable}\nimport os, time\n'
                              f'open({str(root / "bridge.pid")!r}, "w").write(str(os.getpid()))\n'
                              'print("BRIDGE_READY 127.0.0.1:17423 mode=internet", flush=True)\ntime.sleep(30)\n')
            frontend = root / "frontend"
            frontend.write_text(f'#!{sys.executable}\nimport os, time\n'
                                'from pathlib import Path\n'
                                'root = Path(__import__("sys").argv[3]).parent\n'
                                '(root / "frontend.pid").write_text(str(os.getpid()))\ntime.sleep(30)\n')
            for file in (server, frontend, bridge):
                file.chmod(0o755)
            macs = [root / "mac-1", root / "mac-2"]
            for mac in macs:
                mac.mkdir()
            code = (f'import sys; sys.path.insert(0, {str(Path(session.__file__).parent)!r}); '
                    'import dev_session as s; from pathlib import Path; from types import SimpleNamespace; '
                    f's.REPO=Path({str(root)!r}); '
                    f's.main=lambda: s.start(SimpleNamespace(mode="hosted", lobby=1, upstream=None, hosted_port={port}, test_one_way_delay_ms=0, '
                    f'bridge_executable=Path({str(bridge)!r}), core=s.RELEASE_CORE, frontend=Path({str(frontend)!r}), cache=Path({str(root)!r})), '
                    f'[Path(mac) for mac in {list(map(str, macs))!r}], "127.0.0.1"); '
                    'sys.exit(s.entrypoint())')
            parent = subprocess.Popen([sys.executable, "-c", code], stdout=subprocess.PIPE,
                                      stderr=subprocess.STDOUT, text=True, start_new_session=True)
            children = []
            try:
                deadline = time.monotonic() + 5
                while len(children) < 4 and time.monotonic() < deadline:
                    children = [int(path.read_text()) for path in root.rglob("*.pid") if path.read_text()]
                    time.sleep(.02)
                self.assertEqual(len(children), 4)
                os.killpg(parent.pid, signal.SIGINT)
                output, _ = parent.communicate(timeout=8)
                self.assertEqual(parent.returncode, 0, output)
                self.assertIn("windows remain open", output)
                self.assertNotIn("failed", output.lower())
                self.assertTrue(session.port_available("127.0.0.1", port))
                with self.assertRaises(ProcessLookupError):
                    os.kill(int((root / "server.pid").read_text()), 0)
                for path in [root / "bridge.pid", *[mac / "frontend.pid" for mac in macs]]:
                    os.kill(int(path.read_text()), 0)
            finally:
                if parent.poll() is None:
                    parent.kill()
                    parent.communicate()
                for pid in children:
                    try:
                        os.kill(pid, signal.SIGTERM)
                    except ProcessLookupError:
                        pass

    def test_instance_prompt_retries_and_unattended_requires_an_explicit_count(self):
        replies = iter(["0", "five", "5", "3"])
        with patch("builtins.print"):
            self.assertEqual(session.choose_instances(interactive=True, ask=lambda _: next(replies)), 3)
        self.assertEqual(session.choose_instances(interactive=True, ask=lambda _: ""), 2)
        self.assertEqual(session.choose_instances(4, interactive=False), 4)
        with self.assertRaisesRegex(ValueError, "--instances"):
            session.choose_instances(interactive=False)
        for value in (0, 5):
            with self.assertRaises(ValueError):
                session.choose_instances(value)
        self.assertEqual(session.player_avatars(4), list(session.AVATARS))
        self.assertEqual(session.player_avatars(2, "beta"), ["beta", "chris"])

    def test_android_prompt_only_appears_for_authorized_devices(self):
        args = SimpleNamespace(mac_only=False, android=False, serial=None)
        with patch.object(session, "adb_devices", return_value=[]), patch("builtins.print"):
            self.assertFalse(session.choose_android(args, interactive=True, ask=lambda _: self.fail("Unexpected phone prompt")))
        with patch.object(session, "adb_devices", return_value=["pixel"]):
            self.assertTrue(session.choose_android(args, interactive=True, ask=lambda _: "yes"))
            self.assertFalse(session.choose_android(args, interactive=True, ask=lambda _: ""))
            with self.assertRaisesRegex(ValueError, "--android"):
                session.choose_android(args, interactive=False)
            args.serial = "missing"
            with self.assertRaisesRegex(ValueError, "not connected"):
                session.choose_android(args, interactive=True)
            args.serial = None
            args.android = True
            self.assertTrue(session.choose_android(args, interactive=False))
        args.mac_only = True
        with patch.object(session, "adb_devices") as detect:
            self.assertFalse(session.choose_android(args, interactive=False))
            detect.assert_not_called()

    def test_adb_discovery_ignores_offline_and_unauthorized_devices(self):
        args = SimpleNamespace(sdk=Path("/unavailable"))
        with patch.object(session.shutil, "which", return_value=sys.executable), patch.object(session, "output", return_value="List of devices attached\nready\tdevice\noffline\toffline\nlocked\tunauthorized\n"):
            self.assertEqual(session.adb_devices(args), ["ready"])

    def test_lan_patch_changes_only_owned_endpoint_and_global_checksum(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            data = bytearray([0xff]) * (4 * 1024 * 1024)
            data[0x147:0x14a] = bytes((0x10, 7, 3))
            location = 0x80 * 0x4000 + 0x557
            data[location:location + 6] = ipaddress.IPv4Address("127.0.0.1").packed + session.PORT.to_bytes(2, "big")
            source, symbols, destination = root / "source", root / "symbols", root / "output"
            source.write_bytes(data)
            symbols.write_text("80:4557 MobileEndpoint\n")
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
            for value in ("00:4557", "7f:4557", "80:7fff", "100:4557"):
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
