#!/usr/bin/env python3
"""Execute a real 4 MiB mapper diagnostic with the chosen headless core."""
import argparse
import json
from pathlib import Path
import subprocess
from probe_emulator import digest, require

REPO = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cache", type=Path, default=Path("/Volumes/Storage/Cache/mobileadapter"))
    parser.add_argument("--core", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--require-supported", action="store_true")
    args = parser.parse_args()
    args.output = args.output.resolve()
    args.cache = args.cache.resolve()
    if args.core:
        args.core = args.core.resolve()
    require(not args.output.exists(), "Choose a fresh mapper diagnostic directory")
    args.output.mkdir(parents=True)
    rgbds = args.cache / "rgbds-1.0.4"
    rom, symbols = args.output / "probe.gbc", args.output / "probe.sym"
    subprocess.run([str(rgbds / "rgbasm"), "-o", str(args.output / "probe.o"),
                    str(REPO / "mobile_adapter/rom/fixtures/mbc30_probe.asm")], check=True)
    subprocess.run([str(rgbds / "rgblink"), "-p", "0xff", "-n", str(symbols), "-o", str(rom),
                    str(args.output / "probe.o")], check=True)
    subprocess.run([str(rgbds / "rgbfix"), "-v", "-C", "-p", "0xff", "-m", "MBC3+TIMER+RAM+BATTERY", "-r", "3", str(rom)], check=True)
    require(len(rom.read_bytes()) == 4 * 1024 * 1024, "Mapper probe is not 4 MiB")
    runner = args.output / "smoke-libretro"
    subprocess.run(["clang++", "-std=c++17", "-O2", "-I",
        str(args.cache / "sources/doublecherry/libretro/DoubleCherryEngine"),
        str(REPO / "mobile_adapter/tools/smoke_libretro.cpp"), "-lz", "-o", str(runner)], check=True)
    core = args.core or args.cache / "build/doublecherry/DoubleCherryGB_libretro.dylib"
    with (args.output / "process.log").open("w") as log:
        subprocess.run([str(runner), str(core), str(rom), str(args.output / "screen.ppm"), str(args.output),
            "--frames", "1200", "--adapter", "0", "--symbols", str(symbols), "--state-out", str(args.output / "state.json")],
            stdout=log, stderr=subprocess.STDOUT, check=True)
    state = json.loads((args.output / "state.json").read_text())["symbols"]
    observed = {bank: state["wMobileMBC30Bank" + bank] for bank in ("01", "80", "FF")}
    expected = {"01": 0x31, "80": 0xa5, "FF": 0x5a}
    require(observed["01"] == expected["01"], "Ordinary mapper control failed")
    result = {"supports_upper_rom_banks": observed == expected, "observed": observed, "expected": expected,
              "core_sha256": digest(core), "probe_rom_sha256": digest(rom),
              "scope": "Live ROM bank reads only; RTC, save serialization and mapper state restore need separate tests."}
    (args.output / "evidence.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))
    require(not args.require_supported or result["supports_upper_rom_banks"], "Core does not support the required MBC30 ROM banks")


if __name__ == "__main__":
    main()
