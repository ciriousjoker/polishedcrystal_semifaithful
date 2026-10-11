#!/usr/bin/env python3
"""Link an isolated mobile-only 4 MiB placement experiment, restore all sources.

Does not change the production mapper, root ROMs or user emulator. This is a
layout feasibility probe, not runtime, RTC or save compatibility acceptance.
Run serially with other builds: temporary feature-source changes are restored
in a finally block and isolated objects cannot be reused by the ordinary build.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess
from probe_emulator import digest, require

REPO = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cache", type=Path, default=Path("/Volumes/Storage/Cache/mobileadapter"))
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    args.output = args.output.resolve()
    args.cache = args.cache.resolve()
    require(not args.output.exists(), "Choose a fresh expanded layout directory")
    args.output.mkdir(parents=True)
    names = ("constants.asm", "native_lease_policy.asm", "native_instance.asm", "native_tile_backup.asm",
             "native_save.asm", "native_palettes.asm")
    paths = [REPO / "mobile_adapter/rom" / name for name in names]
    originals = {path: path.read_bytes() for path in paths}
    original_roms = {path: digest(path) for path in REPO.glob("polishedcrystal*.gbc")}
    try:
        for path in paths:
            text = originals[path].decode()
            if path.name == "constants.asm":
                require(text.count("DEF MOBILE_ROM_BANK EQU $7f") == 1, "Mobile bank definition changed")
                text = text.replace("DEF MOBILE_ROM_BANK EQU $7f", "DEF MOBILE_ROM_BANK EQU $80")
            else:
                text, count = re.subn(r'(SECTION "Mobile [^"\n]+", ROMX)\s*\n', r'\1, BANK[MOBILE_ROM_BANK]\n', text)
                require(count == 1, "Feature section placement changed: " + path.name)
            path.write_text(text)
        with (args.output / "build.log").open("w") as log:
            process = subprocess.run(["make", "-j4", "mobile", f"RGBDS={args.cache / 'rgbds-1.0.4'}/",
                f"MOBILE_OBJ_DIR={args.output / 'objects'}", f"ROM_NAME={args.output / 'expanded'}"],
                cwd=REPO, stdout=log, stderr=subprocess.STDOUT)
        if process.returncode:
            errors = [line for line in (args.output / "build.log").read_text().splitlines() if line.startswith("error:")]
            require(errors == ["error: unknown ROM bank $80"], "Expanded build failed before the known native bankends limitation")
    finally:
        for path, original in originals.items():
            path.write_bytes(original)
        require(all(digest(path) == expected for path, expected in original_roms.items()), "Production ROM changed during layout experiment")
    rom, mapping = args.output / "expanded.gbc", args.output / "expanded.map"
    data = rom.read_bytes()
    require(len(data) == 4 * 1024 * 1024 and data[0x148] == 7 and data[0x149] == 3,
            "Expanded cartridge size/save allocation is wrong")
    bank, sections, native_upper = None, [], []
    for line in mapping.read_text().splitlines():
        match = re.match(r"(\w+) bank #(\d+):", line)
        if match:
            bank = int(match[2]) if match[1] == "ROMX" else None
        match = re.search(r'SECTION:.*\["([^"]+)"\]', line)
        if match and bank is not None:
            if match[1].startswith("Mobile "):
                sections.append({"section": match[1], "bank": bank})
            elif bank >= 128:
                native_upper.append(match[1])
    require(len(sections) == 6 and all(section["bank"] == 128 for section in sections),
            "Not every banked feature section moved to the upper ROM half")
    require(not native_upper, "Original game sections unexpectedly moved above 2 MiB")
    result = {"layout_passed": True, "production_postlink_passed": process.returncode == 0,
        "postlink_limitation": "Native tools/bankends rejects bank 128; the diagnostic checks linked sections independently.", "rom_bytes": len(data), "save_ram_bytes": 32768,
        "rom_sha256": hashlib.sha256(data).hexdigest(), "feature_sections": sections,
        "scope": "All banked feature assembly in bank 128; guarded ROM0 integration remains below 2 MiB. Runtime not accepted."}
    (args.output / "evidence.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
