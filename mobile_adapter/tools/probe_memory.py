#!/usr/bin/env python3
"""Run a disposable assembly/link capacity experiment, never a playable ROM.

Requires a built, matching baseline checkout. Original source and build files
are read only. The scratch objects preserve public ROM0 entry points with far
call trampolines while relocating three foreground helper files into ROMX.
"""

import argparse
import hashlib
import json
import re
import subprocess
import tempfile
from pathlib import Path


def run(command, cwd):
    result = subprocess.run(command, cwd=cwd, text=True, capture_output=True)
    if result.returncode:
        raise RuntimeError(" ".join(map(str, command)) + "\n" + result.stdout + result.stderr)
    return result.stdout + result.stderr


def read_summary(path):
    lines = path.read_text().splitlines()
    return [line.strip() for line in lines[1:7]]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    root = args.build_dir.resolve()
    assert (root / "polishedcrystal-3.2.3.gbc").exists(), "Build the baseline first"
    makefile = (root / "Makefile").read_text()
    objects = re.search(r"rom_obj := (.*?)\n\n", makefile, re.S)[1].replace("\\", "").split()
    assert all((root / obj).exists() for obj in objects)
    moved_files = ["home/item.asm", "home/print_bcd.asm", "home/restore_music.asm"]
    with tempfile.TemporaryDirectory(prefix="mobileadapter-memory-") as directory:
        scratch = Path(directory)
        home = (root / "home.asm").read_text()
        banked = ['SECTION "Mobile Memory Probe Helpers", ROMX, BANK[$7f]']
        for relative in moved_files:
            source = (root / relative).read_text()
            names = re.findall(r"^(\w+)::?", source, re.M)
            trampolines = "\n".join(name + "::\n\tfarcall Probe_" + name + "\n\tret" for name in names)
            home = home.replace('INCLUDE "' + relative + '"', trampolines)
            for name in sorted(names, key=len, reverse=True):
                source = re.sub(r"\b" + re.escape(name) + r"\b", "Probe_" + name, source)
            banked.append(source)
        (scratch / "home.asm").write_text(home)
        (scratch / "helpers.asm").write_text("\n".join(banked))
        flags = ["-Weverything", "-Wtruncation=1", "-E", "-Q8", "-P", "includes.asm"]
        run(["rgbasm", *flags, "-o", str(scratch / "home.o"), str(scratch / "home.asm")], root)
        run(["rgbasm", *flags, "-o", str(scratch / "helpers.o"), str(scratch / "helpers.asm")], root)
        link_objects = [str(scratch / "home.o") if obj == "home.o" else str(root / obj) for obj in objects]
        base = ["rgblink", "-Weverything", "-Wtruncation=1", "-M", "-p", "0xff", "-l", "layout.link"]

        def link(name, objects_to_link):
            run([*base, "-n", str(scratch / (name + ".sym")), "-m", str(scratch / (name + ".map")),
                 "-o", str(scratch / (name + ".gbc")), *objects_to_link], root)
            return read_summary(scratch / (name + ".map"))

        relocated = link("relocated", [*link_objects, str(scratch / "helpers.o")])
        (scratch / "budget.asm").write_text('''
SECTION "Mobile IRQ Capacity Probe", ROM0
ProbeIRQCapacity:: ds 256
SECTION "Mobile ROM Capacity Probe", ROMX, BANK[$7f]
ProbeProtocolCapacity:: ds 8192
SECTION "Mobile Resident Capacity Probe", WRAM0
ProbeResidentCapacity:: ds 96
SECTION "Mobile Buffer Capacity Probe", WRAMX, BANK[2]
ProbeBufferCapacity:: ds 1536
SECTION "Mobile Owner Capacity Probe", HRAM
ProbeOwnerCapacity:: ds 1
SECTION "Mobile Journal Capacity Probe", SRAM, BANK[1]
ProbeJournalCapacity:: ds 128
''')
        run(["rgbasm", "-o", str(scratch / "budget.o"), str(scratch / "budget.asm")], root)
        reserved = link("reserved", [*link_objects, str(scratch / "helpers.o"), str(scratch / "budget.o")])
        report = {
            "baseline_commit": run(["git", "rev-parse", "HEAD"], root).strip(),
            "baseline_rom_sha256": hashlib.sha256((root / "polishedcrystal-3.2.3.gbc").read_bytes()).hexdigest(),
            "rgbasm_version": run(["rgbasm", "--version"], root).strip(),
            "baseline": read_summary(root / "polishedcrystal-3.2.3.map"),
            "after_helper_relocation": relocated,
            "after_relocation_and_reservation": reserved,
            "moved_files": moved_files,
            "reservations_bytes": {"ROM0": 256, "ROMX": 8192, "WRAM0": 96, "WRAMX_bank_2": 1536, "HRAM": 1, "SRAM_bank_1": 128},
            "validation": "Both experimental object sets assembled and linked successfully. No emulator execution or timing test.",
            "limitations": [
                "The ROM contains inert capacity placeholders and is not a playable implementation.",
                "Relocated foreground helpers need register/bank/timing regression tests before adoption.",
                "Reservations are an illustrative budget, not measured sizes of an implemented driver.",
                "This experiment uses the matching standard baseline build, not every build variant.",
            ],
        }
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
