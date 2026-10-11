#!/usr/bin/env python3
"""Prove native baseline automation with a real in-game battery save.

This explicitly launches an emulator. It is separate from normal host unit tests.
The input timings are specific to the pinned baseline, not a general network
acceptance harness. Artifacts are retained on failure; existing output is refused.
"""

import argparse
import gzip
import hashlib
import json
from pathlib import Path
import re
import struct
import subprocess
import sys
import zlib


def trace_text(path):
    """Read current compressed or historical plain instruction-observer evidence."""
    if path.exists():
        return path.read_text()
    with gzip.open(str(path) + ".gz", "rt") as trace:
        return trace.read()


BASELINE_SHA256 = "314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e"
CORE_REVISION = "03f58ca3dfb4b716f7e66a0e0467f9e85ef82abb"
REPO = Path(__file__).resolve().parents[2]
FIXTURES = Path(__file__).resolve().parent / "fixtures"


def require(condition, message):
    if not condition:
        raise ValueError(message)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read_symbols(path):
    symbols = {}
    for line in path.read_text().splitlines():
        match = re.match(r"^([0-9a-fA-F]+):([0-9a-fA-F]+)\s+(\S+)", line)
        if match:
            bank, address, name = match.groups()
            symbols[name] = (int(bank, 16), int(address, 16))
    return symbols


def validate_battery(path, symbols):
    data = path.read_bytes()
    require(len(data) == 32768, "Expected raw 32 KiB battery SRAM")

    def offset(name):
        bank, address = symbols[name]
        require(bank < 4 and 0xa000 <= address < 0xc000, f"Invalid SRAM symbol {name}")
        return bank * 0x2000 + address - 0xa000

    version = int.from_bytes(data[offset("sSaveVersion"):offset("sSaveVersion") + 2], "big")
    require(version == 10, "Unexpected baseline save version")
    checksums = {}
    for prefix in ("s", "sBackup"):
        require(data[offset(prefix + "CheckValue1")] == 97, "Missing first save sentinel")
        require(data[offset(prefix + "CheckValue2")] == 127, "Missing second save sentinel")
        start, end = offset(prefix + "GameData"), offset(prefix + "GameDataEnd")
        require(start < end, "Invalid save checksum span")
        computed = sum(data[start:end]) & 0xffff
        checksum_offset = offset(prefix + "Checksum")
        stored = int.from_bytes(data[checksum_offset:checksum_offset + 2], "little")
        require(stored == computed, f"Invalid {prefix} game-data checksum")
        checksums[prefix] = stored
    return {"bytes": len(data), "save_version": version, "checksums": checksums,
            "sha256": digest(path)}


def convert_png(ppm, png):
    # The native frontend emits this fixed, uncompressed PPM header.
    header = b"P6\n160 144\n255\n"
    image = ppm.read_bytes()
    require(image.startswith(header), "Unexpected native image header")
    pixels = image[len(header):]
    require(len(pixels) == 160 * 144 * 3, "Invalid native image size")

    def chunk(kind, payload):
        return (struct.pack(">I", len(payload)) + kind + payload
                + struct.pack(">I", zlib.crc32(kind + payload) & 0xffffffff))

    rows = b"".join(b"\0" + pixels[y * 480:(y + 1) * 480] for y in range(144))
    png.write_bytes(b"\x89PNG\r\n\x1a\n"
                    + chunk(b"IHDR", struct.pack(">IIBBBBB", 160, 144, 8, 2, 0, 0, 0))
                    + chunk(b"IDAT", zlib.compress(rows)) + chunk(b"IEND", b""))


def run_stage(args, name, frames, script, save_in=None, save_out=False):
    directory = args.output / name
    directory.mkdir()
    image = directory / "screen.ppm"
    state = directory / "state.json"
    command = [str(args.runner), str(args.core), str(args.rom), str(image), str(directory),
               "--frames", str(frames), "--input", str(script),
               "--symbols", str(args.symbols), "--state-out", str(state)]
    if save_in:
        command += ["--save-in", str(save_in)]
    if save_out:
        command += ["--save-out", str(directory / "ingame.sav")]
    with (directory / "process.log").open("w") as log:
        subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=60)
    values = json.loads(state.read_text())["symbols"]
    for symbol, expected in {"hCGB": 1, "hCrashCode": 0, "hInMenu": 0,
                             "wSaveFileExists": 1, "wMapGroup": 24, "wMapNumber": 5,
                             "sSaveVersion": 10}.items():
        require(values[symbol] == expected, f"{name}: unexpected {symbol}={values[symbol]}")
    # Player object map coordinates include a four-tile map border.
    require(values["wPlayerMapX"] == values["wXCoord"] + 4, f"{name}: inconsistent player X")
    require(values["wPlayerMapY"] == values["wYCoord"] + 4, f"{name}: inconsistent player Y")
    convert_png(image, directory / "screen.png")
    return {"frame_calls": frames, "exit_code": 0, "symbols": values,
            "input_sha256": digest(script), "ppm_sha256": digest(image)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--core", type=Path, required=True)
    parser.add_argument("--runner", type=Path, required=True)
    parser.add_argument("--rom", type=Path, default=REPO / "polishedcrystal-3.2.3.gbc")
    parser.add_argument("--symbols", type=Path, default=REPO / "polishedcrystal-3.2.3.sym")
    parser.add_argument("--output", type=Path, required=True, help="New artifact directory")
    args = parser.parse_args()
    for field in ("core", "runner", "rom", "symbols", "output"):
        setattr(args, field, getattr(args, field).resolve())
    require(digest(args.rom) == BASELINE_SHA256, "Input scripts require the exact baseline ROM")
    require(not args.output.exists(), "Output directory already exists; choose a new path")
    args.output.mkdir(parents=True)
    symbols = read_symbols(args.symbols)

    created = run_stage(args, "create", 12200, FIXTURES / "new-game-save.input", save_out=True)
    battery_path = args.output / "create" / "ingame.sav"
    battery = validate_battery(battery_path, symbols)
    require((created["symbols"]["wXCoord"], created["symbols"]["wYCoord"]) == (3, 3),
            "Unexpected initial player position")
    continued = run_stage(args, "continue", 3500, FIXTURES / "continue.input", battery_path)
    moved = run_stage(args, "move", 4200, FIXTURES / "continue-move.input", battery_path)
    for field in ("wMapGroup", "wMapNumber", "wXCoord", "wYCoord", "wPlayerMapX", "wPlayerMapY"):
        require(continued["symbols"][field] == created["symbols"][field], f"Continue changed {field}")
    require(moved["symbols"]["wXCoord"] == continued["symbols"]["wXCoord"] + 1,
            "Movement did not complete one tile right")
    require(moved["symbols"]["wYCoord"] == continued["symbols"]["wYCoord"],
            "Movement changed the Y coordinate")
    require(moved["ppm_sha256"] != continued["ppm_sha256"], "Movement did not change rendered output")
    require(digest(battery_path) == battery["sha256"], "Input battery fixture was modified")

    report = {
        "source_base_commit": "8f3c7e9651e988d876e32a4c7ad9ef1bb76702a8",
        "expected_core_revision": CORE_REVISION,
        "rom_sha256": digest(args.rom), "symbols_sha256": digest(args.symbols),
        "core_sha256": digest(args.core), "runner_sha256": digest(args.runner),
        "battery": battery, "stages": {"create": created, "continue": continued, "move": moved},
        "passed": True,
        "network_exchange_tested": False,
        "limitations": ["Pinned baseline input timings; production scenarios need state-driven phases",
                        "No adapter command exchange, network timing, or multiplayer acceptance coverage",
                        "No RTC trailer import; raw SRAM only", "No CPU/WRAM snapshot loaded"],
    }
    (args.output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    print("Native automation passed: in-game Save, fresh Continue, one-tile movement, valid checksums.")
    print(f"Artifacts: {args.output}")


if __name__ == "__main__":
    try:
        main()
    except (ValueError, KeyError, OSError, subprocess.SubprocessError) as error:
        print(f"Native automation failed: {error}", file=sys.stderr)
        sys.exit(1)
