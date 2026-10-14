"""Focused host counterexamples, not native P1/P2 acceptance."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class NativeVBlankTimingTests(unittest.TestCase):
    def test_standalone_counterexamples(self):
        support = Path(__file__).resolve().parent
        scratch = os.environ.get("TMPDIR", "/Volumes/Storage/tmp")
        with tempfile.TemporaryDirectory(prefix="vblank-timing-95-", dir=scratch) as directory:
            binary = Path(directory) / "test"
            env = dict(os.environ, TMPDIR=scratch)
            subprocess.run(["clang++", "-std=c++17", "-Wall", "-Wextra", "-Werror",
                            str(support / "test_native_vblank_timing.cpp"), "-o", str(binary)],
                           check=True, env=env, capture_output=True, text=True)
            subprocess.run([str(binary)], check=True, env=env, capture_output=True, text=True)


if __name__ == "__main__":
    unittest.main()
