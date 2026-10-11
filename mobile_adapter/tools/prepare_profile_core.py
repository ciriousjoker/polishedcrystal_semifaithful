#!/usr/bin/env python3
"""Create a separate, read-only profiled copy of the pinned native core."""
import argparse
import hashlib
from core_runtime import CORE_REVISION, CPU_SHA256
from pathlib import Path
import shutil
import subprocess

REVISION = CORE_REVISION
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--source", type=Path, required=True)
parser.add_argument("--output", type=Path, required=True)
args = parser.parse_args()
source, output = args.source.resolve(), args.output.resolve()
if (source / ".git").exists():
    assert subprocess.check_output(["git", "-C", str(source), "rev-parse", "HEAD"], text=True).strip() == REVISION
assert not output.exists(), "Output already exists"
original = (source / "gb_core/cpu.cpp").read_bytes()
assert hashlib.sha256(original).hexdigest() == CPU_SHA256, "Unreviewed CPU modifications"
code = original.decode()
newline = "\r\n" if "\r\n" in code else "\n"
code = code.replace("\r\n", "\n")
def insert(anchor, replacement):
    global code
    assert code.count(anchor) == 1, f"Pinned anchor changed: {anchor}"
    code = code.replace(anchor, replacement)
insert('#include "gb.h"', '#include "gb.h"\n#include "profile_core.h"')
insert('\t\twrite(regs.SP-2,regs.PC&0xFF);', '''\t\tunsigned pending = ref_gb->get_regs()->IF & ref_gb->get_regs()->IE;
        bool serial_selected = (pending & INT_SERIAL) && !(pending & (INT_VBLANK | INT_LCDC | INT_TIMER));
        if (serial_selected) mobile_profile_enter(mobile_irq_profile, ref_gb, regs, total_clock);
\t\twrite(regs.SP-2,regs.PC&0xFF);''')
insert('\n\t\top_code=op_read();', '\n\t\tmobile_profile_before_instruction(ref_gb, regs, total_clock);\n\t\top_code=op_read();\n        unsigned observed_opcode = op_code;')
insert('\t\ttotal_clock+=tmp_clocks;', '\t\ttotal_clock+=tmp_clocks;\n        mobile_profile_after_instruction(ref_gb, regs, total_clock, observed_opcode, tmp_clocks);')
shutil.copytree(source, output, ignore=shutil.ignore_patterns(".git", "build", "cmake-build-*"))
shutil.copyfile(Path(__file__).with_name("profile_core.h"), output / "gb_core/profile_core.h")
(output / "gb_core/cpu.cpp").write_bytes(code.replace("\n", newline).encode())
print(f"Prepared read-only profiling core at {output}")
