from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import socket
import threading
import time

import runtime
from storage import active_run, ensure_free_space


class ObserverHooks(unittest.TestCase):
    def test_compiled_effect_observer_boundaries(self):
        import subprocess
        with tempfile.TemporaryDirectory() as tmp:
            binary = Path(tmp) / "observer"
            source = Path(__file__).with_name("test_effect_observer.cpp")
            subprocess.run(["c++", "-std=c++17", "-O2", str(source), "-o", str(binary)], check=True)
            subprocess.run([str(binary)], check=True)

    def source(self, root):
        (root / "gb_core").mkdir()
        (root / "libretro").mkdir()
        (root / "gb_core/cpu.cpp").write_text(
            "void cpu::io_write(word adr,byte dat)\n{\n}\n"
            "mobile_profile_before_instruction(ref_gb, regs, total_clock);\n"
            "mobile_profile_after_instruction(ref_gb, regs, total_clock, observed_opcode, tmp_clocks);\n")
        (root / "libretro/libretro.cpp").write_text(
            "bool retro_load_game(const struct retro_game_info *info)\n{\n}\n"
            "void retro_reset(void)\n{\n}\n"
            "bool retro_unserialize(const void *data, size_t size)\n{\n        return true;\n}\n")

    def test_supported_hooks_are_idempotent_and_missing_anchors_fail_before_write(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            self.source(root)
            runtime.extend_profile(root)
            original = {p: p.read_bytes() for p in root.rglob("*") if p.is_file()}
            runtime.extend_profile(root)
            self.assertEqual(original, {p: p.read_bytes() for p in original})
            self.assertIn("acceptance_profile_after(game, regs, opcode, cycles);",
                          (root / "gb_core/profile_core.h").read_text())
            self.assertEqual((root / "libretro/libretro.cpp").read_text().count(
                "acceptance_profile_restore_boundary();"), 2)  # declaration and successful restore
            cpu = root / "gb_core/cpu.cpp"
            cpu.write_text(cpu.read_text().replace("void cpu::io_write", "void unsupported::io_write"))
            before = {p: p.read_bytes() for p in original}
            with self.assertRaisesRegex(RuntimeError, "unique observer source anchor"):
                runtime.extend_profile(root)
            self.assertEqual(before, {p: p.read_bytes() for p in before})

    def test_duplicate_and_misplaced_anchors_fail(self):
        with self.assertRaises(RuntimeError):
            runtime.inject_unique("anchor anchor", "anchor", "anchor hook")
        with self.assertRaises(RuntimeError):
            runtime.inject_unique("anchor elsewhere hook", "anchor", "anchor hook")

        with self.assertRaises(RuntimeError):
            runtime.inject_unique("anchor hook elsewhere hook", "anchor", "anchor hook")

    def test_projection_retains_effect_state_and_raw_events(self):
        import gzip
        import json
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            fields = {"vAcceptanceEffectCount": 3, "vAcceptanceEffectOverflow": 0,
                      "vAcceptanceNativeSceneReady": 1,
                      **{f"vAcceptanceEffectActive{i}": i for i in range(8)}}
            profile = {"native_effect_events": [{"fields": [1, 2, 3]}],
                       "native_effect_event_count": 1, "native_effect_event_overflow": 0}
            with gzip.open(root / "trace.jsonl.gz", "wt") as stream:
                stream.write(json.dumps({"frame": 1, "symbols": fields}) + "\n")
            (root / "profile.json").write_text(json.dumps(profile))
            (root / "state.json").write_text(json.dumps({"frame_calls": 1, "symbols": fields}))
            (root / "result.sav").write_bytes(b"save")
            (root / "input-events.jsonl").write_text("")
            result = runtime.projected(root)
            self.assertEqual(result["frames"][0]["state"], fields)
            self.assertEqual(result["profile"], profile)


class Workspaces(unittest.TestCase):
    def test_rom_copy_excludes_apps_patches_and_generated_artifacts(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "source"
            files = ["Makefile", "layout.link", "main.asm", "main.o",
                     "data/maps/maps.asm", "mobile_adapter/rom/ram.asm",
                     "mobile_adapter/tools/prepare_virtual_avatars.py",
                     "mobile_adapter/build-mobile/main.o",
                     "mobile_adapter/bridge/.dart_tool/huge.dill",
                     "mobile_adapter/server/target/debug/huge.a",
                     "mobile_adapter/tests/support/observer.hpp",
                     "mobile_adapter/tests/scenarios/save.sav", "patches/old.ips"]
            for name in files:
                path = source / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(b"input")
            clean, fixture = Path(tmp) / "clean", Path(tmp) / "fixture"
            runtime.copy_sources(source, clean)
            runtime.copy_sources(source, fixture, reuse_objects=True)
            for copy in (clean, fixture):
                self.assertTrue((copy / "data/maps/maps.asm").exists())
                self.assertTrue((copy / "mobile_adapter/rom/ram.asm").exists())
                for absent in ("bridge", "server"):
                    self.assertFalse((copy / "mobile_adapter" / absent).exists())
                self.assertTrue((copy / "mobile_adapter/tests/support/observer.hpp").exists())
                self.assertFalse((copy / "mobile_adapter/tests/scenarios").exists())
                self.assertFalse((copy / "patches").exists())
            self.assertFalse((clean / "main.o").exists())
            self.assertFalse((clean / "mobile_adapter/build-mobile").exists())
            self.assertTrue((fixture / "main.o").exists())
            self.assertTrue((fixture / "mobile_adapter/build-mobile/main.o").exists())

    def test_disposable_compiler_inputs_removed_but_probe_outputs_retained(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp)
            (source / "maps").mkdir()
            (source / "maps/test.asm").write_text("input")
            for name in ("fixture.gbc", "fixture.sym", "fixture.map", "main.o", "Makefile"):
                (source / name).write_bytes(b"input")
            runtime.retain_rom_outputs(source)
            self.assertEqual(sorted(p.name for p in source.iterdir()),
                             ["fixture.gbc", "fixture.map", "fixture.sym"])


class PristineCopyright(unittest.TestCase):
    def generate(self, size, compressed_size=288):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "gfx/splash").mkdir(parents=True)
            (root / "tools").mkdir()
            (root / "Makefile").write_text("archived reference recipe")
            text = "@ 1 9 9 5 - 2 0 2 6    G A M E   F R E A K"
            (root / "gfx/splash/copyright.txt").write_text(text + "\n")
            calls = []

            def fake_run(command, cwd, log):
                calls.append(command)
                self.assertEqual(cwd, root)
                if len(calls) == 1:
                    (root / "tools/fine_print").write_bytes(b"compiled generator")
                if len(calls) == 2:
                    self.assertEqual(calls[0], ["make", "-C", "tools", "-j1", "fine_print", "lzpcompress"])
                    self.assertEqual(command, ["tools/fine_print", "-c", "013", "-s", "0",
                                               text, "gfx/splash/copyright.2bpp"])
                    (root / "gfx/splash/copyright.2bpp").write_bytes(bytes(size))
                if len(calls) == 3:
                    self.assertEqual(command, ["tools/lzpcompress", "--", "gfx/splash/copyright.2bpp",
                                               "gfx/splash/copyright.2bpp.lzp"])
                    (root / "gfx/splash/copyright.2bpp.lzp").write_bytes(bytes(compressed_size))

            with patch.object(runtime, "run", side_effect=fake_run):
                evidence = runtime.prepare_pristine_copyright(root, root)
            self.assertEqual(evidence["text"], text)
            self.assertEqual(evidence["asset_bytes"], 480)
            self.assertEqual(evidence["compressed_bytes"], 288)
            self.assertEqual(evidence["asset_sha256"], runtime.sha(root / "gfx/splash/copyright.2bpp"))
            self.assertTrue((root / "pristine-copyright.json").is_file())

    def test_dependencies_precede_generation_with_exact_text(self):
        self.generate(480)

    def test_empty_artwork_is_rejected(self):
        with self.assertRaisesRegex(RuntimeError, "288 compressed bytes"):
            self.generate(1)

    def test_empty_linked_artwork_is_rejected(self):
        with self.assertRaisesRegex(RuntimeError, "288 compressed bytes"):
            self.generate(480, compressed_size=1)


class DeclarativeConfiguration(unittest.TestCase):
    def test_opcode_or_out_of_span_patch_is_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            rom, destination = Path(tmp) / "test.gbc", Path(tmp) / "patched.gbc"
            rom.write_bytes(bytes(0x4000))
            names = {"AcceptanceConfig": (0, 0x200), "AcceptanceConfigEnd": (0, 0x219),
                     "AcceptanceAvatar": (0, 0x201), "AcceptanceBoot": (0, 0x220)}
            runtime.patch_data(rom, names, {"AcceptanceAvatar": bytes([3])}, destination)
            self.assertEqual(destination.read_bytes()[0x201], 3)
            with self.assertRaisesRegex(ValueError, "declared test configuration"):
                runtime.patch_data(rom, names, {"AcceptanceBoot": bytes([0])}, destination)
            with self.assertRaises(ValueError):
                runtime.patch_data(rom, names, {"AcceptanceAvatar": bytes([1, 2])}, destination)
            names["AcceptanceAvatar"] = (0, 0x220)
            with self.assertRaisesRegex(ValueError, "ROM span"):
                runtime.patch_data(rom, names, {"AcceptanceAvatar": bytes([1])}, destination)


class FaultDelivery(unittest.TestCase):
    def test_unmodified_welcome_is_not_counted_as_a_roster_fault(self):
        welcome = b"PC\x01" + bytes([runtime.WIRE_KINDS["WELCOME"]]) + b"\x00\x01\x00\x01\x01"
        roster = b"PC\x01" + bytes([runtime.WIRE_KINDS["ROSTER"]]) + b"\x00\x02\x00\x02\x01\x02"
        with socket.create_server(("127.0.0.1", 0)) as upstream:
            def serve():
                connection, _ = upstream.accept()
                with connection:
                    connection.sendall(welcome)
                    self.assertEqual(connection.recv(1), b"P")
                    connection.sendall(roster)
            worker = threading.Thread(target=serve)
            worker.start()
            proxy = runtime.Proxy(upstream.getsockname()[1], "roster", wait_for_arm=True)
            host = threading.Thread(target=proxy.serve_forever)
            host.start()
            try:
                with socket.create_connection(proxy.server_address, timeout=2) as client:
                    def receive(size):
                        data = bytearray()
                        while len(data) < size:
                            chunk = client.recv(size - len(data))
                            if not chunk: self.fail("Proxy closed before the requested fault")
                            data.extend(chunk)
                        return bytes(data)
                    self.assertEqual(receive(len(welcome)), welcome)
                    self.assertEqual(proxy.injected, 0)
                    proxy.armed = True
                    client.sendall(b"P")
                    self.assertEqual(receive(len(roster)), roster[:8] + bytes([255, 255]))
                    deadline = time.monotonic() + 2
                    while not proxy.injected and time.monotonic() < deadline:
                        time.sleep(.001)
                    self.assertEqual(proxy.injected, 1)
            finally:
                proxy.shutdown()
                proxy.server_close()
                host.join(timeout=2)
                worker.join(timeout=2)


class StorageReserve(unittest.TestCase):
    def make_run(self, root, name):
        run = root / name
        with active_run(run):
            (run / "evidence.json").write_text("{}")
        return run

    def test_cleanup_preserves_active_current_toolchain_and_unidentified_content(self):
        with tempfile.TemporaryDirectory() as tmp:
            cache = Path(tmp)
            old = self.make_run(cache, "acceptance-suite-old")
            current = self.make_run(cache, "acceptance-suite-current")
            toolchain = cache / "rgbds-1.0.4"
            toolchain.mkdir()
            unrelated = cache / "acceptance-suite-user-data"
            unrelated.mkdir()
            (unrelated / "save.sav").write_bytes(b"user data")
            with active_run(cache / "acceptance-suite-active"):
                deleted = ensure_free_space(cache, current, reserve=100, headroom=10,
                    free=lambda: 0 if old.exists() else 110)
            self.assertEqual(deleted, [old.resolve()])
            for path in (current, toolchain, unrelated, cache / "acceptance-suite-active"):
                self.assertTrue(path.exists())

    def test_space_failure_does_not_delete_symlink_or_active_run(self):
        with tempfile.TemporaryDirectory() as tmp:
            cache = Path(tmp) / "cache"
            cache.mkdir()
            outside = self.make_run(Path(tmp), "outside")
            (cache / "acceptance-suite-symlink").symlink_to(outside)
            with active_run(cache / "current"):
                with self.assertRaisesRegex(RuntimeError, "cannot be restored safely"):
                    ensure_free_space(cache, cache / "current", reserve=100, headroom=0, free=lambda: 0)
            self.assertTrue(outside.exists())
            self.assertTrue((cache / "current").exists())

    def test_sufficient_space_performs_no_deletion(self):
        with tempfile.TemporaryDirectory() as tmp:
            cache = Path(tmp)
            old = self.make_run(cache, "acceptance-suite-old")
            self.assertEqual(ensure_free_space(cache, cache / "current", reserve=100,
                             headroom=0, free=lambda: 100), [])
            self.assertTrue(old.exists())

class IdentityEvidence(unittest.TestCase):
    def test_negative_identity_patches_only_named_data_and_checksum(self):
        with tempfile.TemporaryDirectory() as tmp:
            rom, result = Path(tmp) / "fixture.gbc", Path(tmp) / "negative.gbc"
            original = bytes(0x4000)
            rom.write_bytes(original)
            names = {"AcceptanceConfig": (0, 0x200), "AcceptanceConfigEnd": (0, 0x220),
                     "MobileCompatibilityHash": (0, 0x300), "MobileCompatibilityHashEnd": (0, 0x320),
                     "MobileHelloPrefix": (0, 0x400), "MobileHelloVector": (0, 0x405)}
            for mismatch, changed in (("digest", 0x300), ("schema", 0x400)):
                runtime.patch_identity_fixture(rom, names, mismatch, result)
                data = result.read_bytes()
                self.assertEqual([i for i, (a, b) in enumerate(zip(original, data))
                                  if a != b and i not in (0x14e, 0x14f)], [changed])
                self.assertEqual(int.from_bytes(data[0x14e:0x150], "big"),
                                 (sum(data[:0x14e]) + sum(data[0x150:])) & 65535)
            names["MobileHelloVector"] = (0, 0x406)
            with self.assertRaisesRegex(ValueError, "data spans"):
                runtime.patch_identity_fixture(rom, names, "schema", result)
            names.pop("AcceptanceConfig")
            with self.assertRaisesRegex(ValueError, "optional test cartridge"):
                runtime.patch_identity_fixture(rom, names, "digest", result)

    def test_proxy_captures_fragmented_native_hello_and_delivered_error(self):
        hello = b"PC\x01\x10\x00\x01\x00\x2f" + bytes([5, 0, 4, 0, 1]) + bytes(42)
        error = b"PC\x01\x16\x00\x01\x00\x01\x02"
        with socket.create_server(("127.0.0.1", 0)) as upstream:
            def serve():
                connection, _ = upstream.accept()
                with connection:
                    observed = bytearray()
                    while len(observed) < len(hello):
                        observed.extend(connection.recv(128))
                    self.assertEqual(bytes(observed), hello)
                    connection.sendall(error)
            worker = threading.Thread(target=serve)
            worker.start()
            proxy = runtime.Proxy(upstream.getsockname()[1], "none", identity="digest")
            host = threading.Thread(target=proxy.serve_forever)
            host.start()
            try:
                with socket.create_connection(proxy.server_address, timeout=2) as client:
                    client.sendall(hello[:7])
                    client.sendall(hello[7:19])
                    client.sendall(hello[19:])
                    received = bytearray()
                    while len(received) < len(error):
                        received.extend(client.recv(128))
                    self.assertEqual(bytes(received), error)
                worker.join(timeout=2)
                self.assertFalse(worker.is_alive())
                self.assertEqual([(row["direction"], row["frame"]) for row in proxy.wire],
                                 [("native-to-bridge", hello.hex()), ("bridge-to-native", error.hex())])
                for _ in range(1000):
                    proxy.observe(0, "bridge-to-native", error)
                self.assertEqual(len(proxy.wire), 5)
            finally:
                proxy.shutdown()
                proxy.server_close()
                host.join(timeout=2)

    def test_held_rejection_preserves_pending_native_socket_after_bridge_eof(self):
        error = b"PC\x01\x16\x00\x01\x00\x01\x02"
        with socket.create_server(("127.0.0.1", 0)) as upstream:
            def serve():
                connection, _ = upstream.accept()
                with connection:
                    connection.sendall(error)
            worker = threading.Thread(target=serve)
            worker.start()
            proxy = runtime.Proxy(upstream.getsockname()[1], "none", identity="digest-held")
            host = threading.Thread(target=proxy.serve_forever)
            host.start()
            try:
                with socket.create_connection(proxy.server_address, timeout=2) as client:
                    deadline = time.monotonic() + 2
                    while not proxy.wire and time.monotonic() < deadline:
                        time.sleep(.001)
                    self.assertEqual(proxy.wire[0]["frame"], error.hex())
                    worker.join(timeout=2)
                    self.assertFalse(worker.is_alive())
                    client.settimeout(.1)
                    with self.assertRaises(socket.timeout):
                        client.recv(1)
            finally:
                proxy.shutdown()
                proxy.server_close()
                host.join(timeout=2)


if __name__ == "__main__":
    unittest.main()
