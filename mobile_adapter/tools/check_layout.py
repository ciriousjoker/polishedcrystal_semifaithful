#!/usr/bin/env python3
"""Compare completed build layouts and enforce dedicated mobile placement."""
import argparse
import json
from pathlib import Path
from probe_emulator import BASELINE_SHA256, digest, read_symbols, require

REPO = Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--baseline", type=Path, default=REPO / "polishedcrystal-3.2.3.gbc")
parser.add_argument("--mobile", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.gbc")
parser.add_argument("--faithful", type=Path, default=REPO / "polishedcrystal-faithful-mobile-3.2.3.gbc")
parser.add_argument("--output", type=Path, required=True)
args = parser.parse_args()
require(digest(args.baseline) == BASELINE_SHA256, "Disabled ROM differs from exact baseline")
old = read_symbols(args.baseline.with_suffix(".sym"))
new = read_symbols(args.mobile.with_suffix(".sym"))
ram = {name: value for name, value in old.items() if name[0] in "whs" and (
    (value[0] < 8 and 0xc000 <= value[1] < 0xe000) or
    (value[0] < 4 and 0xa000 <= value[1] < 0xc000) or 0xff80 <= value[1] < 0xffff)}
changed = {name: [value, new.get(name)] for name, value in ram.items() if value != new.get(name)}
require(not changed, f"Existing RAM symbols moved: {changed}")
require(new["wMobileResidentStart"] == (0, 0xcec4), "Resident start changed")
require(new["wMobileBufferStart"] == (2, 0xd4eb), "Buffer bank or start changed")
resident = new["wMobileResidentEnd"][1] - new["wMobileResidentStart"][1]
buffers = new["wMobileBufferEnd"][1] - new["wMobileBufferStart"][1]
code = new["MobileCodeEnd"][1] - new["MobileCodeStart"][1]
require(resident <= 128 and buffers <= 1536 and code <= 8192, "Mobile allocation exceeds caps")
report = {"existing_ram_symbols_checked": len(ram), "existing_ram_symbols_changed": changed,
          "resident_bytes": resident, "buffer_bytes": buffers, "banked_driver_bytes": code}
for name in ("baseline", "mobile", "faithful"):
    path = getattr(args, name)
    report[name + "_rom_sha256"] = digest(path)
    report[name + "_summary"] = path.with_suffix(".map").read_text().split("\n\n")[0]
args.output.write_text(json.dumps(report, indent=2) + "\n")
print(f"Layout passed: {len(ram)} existing RAM symbols preserved, {code} driver ROM bytes")
