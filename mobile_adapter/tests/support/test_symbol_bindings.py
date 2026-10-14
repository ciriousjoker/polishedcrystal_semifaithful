"""Compile the production symbol loader and verify its private alias boundary.

Set both MOBILE_BINDING_ENABLED_SYMBOLS and MOBILE_BINDING_DISABLED_SYMBOLS
to run the additional real-fixture integration case. Synthetic host cases run
without those paths. MOBILE_BINDING_TEST_OUTPUT retains artifacts.
No emulator, ROM build, core build, or observer is run.
"""
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[3]


class SymbolBindingsTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.output = Path(os.environ.get("MOBILE_BINDING_TEST_OUTPUT") or
                          tempfile.mkdtemp(prefix="symbol-bindings-"))
        cls.output.mkdir(parents=True, exist_ok=True)
        source = (ROOT / "mobile_adapter/tools/smoke_libretro.cpp").read_text()
        start = source.index("static void load_bindings(")
        end = source.index("\nstatic std::map<std::string, unsigned> capture(", start)
        cls.loader = source[start:end]
        binding = re.search(r"struct Binding \{[^}]+\};", source).group()
        harness = '''#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
''' + binding + '''
static std::map<std::string, Binding> bindings;
static void fail(const std::string& message) { throw std::runtime_error(message); }
''' + cls.loader + '''
int main(int argc, char** argv) {
    if (argc != 2) return 3;
    try {
        load_bindings(argv[1]);
        for (const auto& item : bindings)
            std::cout << item.first << " " << item.second.bank << " " << item.second.address << "\\n";
    } catch (const std::exception& error) {
        std::cerr << error.what() << "\\n";
        return 2;
    }
}
'''
        cpp = cls.output / "binding_harness.cpp"
        cpp.write_text(harness)
        cls.exe = cls.output / "binding_harness"
        subprocess.run(["c++", "-std=c++17", str(cpp), "-o", str(cls.exe)], check=True)
        symbol_paths = [os.environ.get(key) for key in
                        ("MOBILE_BINDING_ENABLED_SYMBOLS", "MOBILE_BINDING_DISABLED_SYMBOLS")]
        if any(symbol_paths) and not all(symbol_paths):
            raise ValueError("Configure both enabled and disabled fixture SYM paths together")
        cls.symbols = [Path(path) for path in symbol_paths] if all(symbol_paths) else []
        cls.aliases = {}
        asm_path = ROOT / "mobile_adapter/rom/ram.asm"
        for name, expr in re.findall(r"^DEF (wMobileStable\w+) EQU (.+)$", asm_path.read_text(), re.M):
            if expr.startswith("$"):
                value = int(expr[1:], 16)
            elif expr == "wMobileVirtualMergedPalette":
                value = 0xdb4b
            else:
                match = re.fullmatch(r"(wMobileStable\w+)(?: \+ ([0-9]+))?", expr)
                if not match:
                    raise AssertionError(f"Unrecognized audited alias expression: {expr}")
                value = cls.aliases[match[1]] + int(match[2] or 0)
            cls.aliases[name] = value
        if len(cls.aliases) != 33:
            raise AssertionError("Retained alias schema changed; review all names and layouts")
        hashes = {str(p): hashlib.sha256(p.read_bytes()).hexdigest()
                  for p in [ROOT / "mobile_adapter/tools/smoke_libretro.cpp", asm_path, *cls.symbols]}
        (cls.output / "source-input-hashes.json").write_text(json.dumps(hashes, indent=2) + "\n")
        cls.case_number = 0

    def parse(self, rows=None, path=None, valid=True):
        if path is None:
            type(self).case_number += 1
            path = self.output / f"case-{self.case_number:03}.sym"
            path.write_text(rows)
        result = subprocess.run([str(self.exe), str(path)], text=True, capture_output=True)
        self.assertEqual(result.returncode, 0 if valid else 2, result.stderr)
        if not valid:
            self.assertTrue(result.stderr.strip())
            return {}
        return {name: (int(bank), int(address)) for name, bank, address in
                (row.split() for row in result.stdout.splitlines())}

    def test_full_actual_enabled_and_disabled_symbols(self):
        if not self.symbols:
            self.skipTest("Real fixture integration requires both MOBILE_BINDING_*_SYMBOLS paths")
        for index, path in enumerate(self.symbols):
            with self.subTest(path=str(path)):
                values = self.parse(path=path)
                stable = {name: pair for name, pair in values.items() if name.startswith("wMobileStable")}
                expected = {name: (5, address) for name, address in self.aliases.items()} if index == 0 else {}
                self.assertEqual(stable, expected)
                self.assertIn("wObjectStructs", values)
                self.assertIn("hROMBank", values)
                self.assertIn("sChecksum", values)

    def test_all_33_aliases_typed_and_colonless_without_order_dependency(self):
        for typed in (False, True):
            rows = "".join(f"{'05:' if typed else ''}{address:04x} {name}\n"
                           for name, address in reversed(list(self.aliases.items())))
            self.assertEqual(self.parse(rows), {name: (5, addr) for name, addr in self.aliases.items()})
        self.assertTrue(all(0xd840 <= address < 0xda00 for name, address in self.aliases.items()
                            if name != "wMobileStableNativeRGB"))
        self.assertEqual(self.aliases["wMobileStableNativeRGB"], 0xdb4b)
        self.assertEqual(self.aliases["wMobileStableNativeRGB"] + 64, 0xdb8b)

    def test_known_names_require_exact_private_layout(self):
        for name, address in self.aliases.items():
            for location in ("cf44", "d83f", "da00", "e000", "ffff", f"00:{address:04x}",
                             f"04:{address:04x}", f"06:{address:04x}", f"{address + 1:04x}"):
                with self.subTest(name=name, location=location):
                    self.parse(f"{location} {name}\n", valid=False)

    def test_unknown_stable_and_other_colonless_names_fail(self):
        for name in ("wMobileStableUnknown", "wMobileStable", "wMobileStableNativeRGBEnd",
                     "wMobileOtherAlias", "wMobileNativeUnknown"):
            for location in ("d840", "db4b"):
                self.parse(f"{location} {name}\n", valid=False)
        self.parse("05:d840 wMobileStableUnknown\n", valid=False)

    def test_malformed_stable_locations_fail(self):
        for location in ("d840junk", "-d840", "+d840", "0xd840", "000d840", "05:", ":d840",
                         "05:d840:00", "005:d840", "xx:d840", "05:d84g"):
            self.parse(f"{location} wMobileStableOriginalYs\n", valid=False)

    def test_legacy_alias_and_typed_native_behavior(self):
        rows = {"wMobileVirtualExample": (5, 0xcf44), "wMobileNativeShapeKeys": (5, 0xd840),
                "wMobileNativeShapes": (5, 0xdcff), "wMobileVirtualQueuedPose": (0, 0xcf0b),
                "wMobileVirtualBackgroundFrame": (0, 0xcf14)}
        # Fixed WRAM aliases below D000 intentionally retain bank 0.
        rows["wMobileVirtualExample"] = (0, 0xcf44)
        actual = self.parse("".join(f"{address:04x} {name}\n" for name, (_, address) in rows.items()))
        self.assertEqual(actual, rows)
        for location, name in (("cf0a", "wMobileVirtualQueuedPose"),
                               ("cf15", "wMobileVirtualBackgroundFrame"),
                               ("cf43", "wMobileVirtualExample"), ("dd00", "wMobileNativeShapes")):
            self.parse(f"{location} {name}\n", valid=False)
        self.assertEqual(self.parse("00:c100 wObjectStructs\n05:d123 wMobileTypedNative\n"
                                    "00:ff80 hROMBank\n02:a123 sChecksum\n00:c123 wUnknownOrdinary\n"),
                         {"wObjectStructs": (0, 0xc100), "wMobileTypedNative": (5, 0xd123),
                          "hROMBank": (0, 0xff80), "sChecksum": (2, 0xa123)})


if __name__ == "__main__":
    unittest.main()
