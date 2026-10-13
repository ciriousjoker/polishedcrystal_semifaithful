from pathlib import Path
import tempfile
import unittest

import runtime
from storage import active_run, ensure_free_space


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


if __name__ == "__main__":
    unittest.main()
