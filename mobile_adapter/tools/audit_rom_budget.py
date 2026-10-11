#!/usr/bin/env python3
"""Measure the entire ROM addon against a matching disabled build.

Use maps generated alongside the supplied ROMs. The net ROM cost includes
integration changes; the separate section sum prevents crediting reductions in
ordinary game code against feature-owned storage. This gate measures bytes, not
runtime correctness, timing or compatibility with arbitrary future game code.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re

REPO = Path(__file__).resolve().parents[2]
ROM_BUDGET_BYTES = 10_000


def layout(text):
    totals = {}
    for kind in ("ROM0", "ROMX"):
        match = re.search(rf"^\s*{kind}: (\d+) bytes used / (\d+) free", text, re.M)
        if not match:
            raise ValueError(f"Missing {kind} map summary")
        totals[kind] = {"used": int(match[1]), "free": int(match[2])}
    banks, sections, current = {}, [], None
    for line in text.splitlines():
        match = re.match(r"(\w+) bank #(\d+):", line)
        if match:
            current = (match[1], int(match[2])) if match[1] in totals else None
            if current is not None:
                if current in banks:
                    raise ValueError("Duplicate ROM bank")
                banks[current] = 0
        match = re.search(r'SECTION:.*\(\$([0-9a-fA-F]+) bytes?\) \["([^"\n]+)"\]', line)
        if match and current is not None:
            size = int(match[1], 16)
            banks[current] += size
            if match[2].startswith("Mobile "):
                sections.append({"name": match[2], "kind": current[0],
                                 "bank": current[1], "bytes": size})
    for kind, total in totals.items():
        selected = [size for (region, _), size in banks.items() if region == kind]
        if (not selected or any(size > 0x4000 for size in selected) or
                sum(selected) != total["used"] or
                sum(selected) + total["free"] != len(selected) * 0x4000):
            raise ValueError(f"Inconsistent {kind} section inventory/summary")
    return {"used": sum(total["used"] for total in totals.values()),
            "free": sum(total["free"] for total in totals.values()),
            "rom0_free": totals["ROM0"]["free"], "feature_sections": sections,
            "banks": [{"kind": kind, "bank": bank, "free": 0x4000 - size}
                      for (kind, bank), size in banks.items()]}


def compare(baseline, mobile, limit=ROM_BUDGET_BYTES):
    if limit <= 0:
        raise ValueError("ROM budget must be positive")
    if baseline["feature_sections"]:
        raise ValueError("Disabled counterpart contains Mobile ROM sections")
    if not mobile["feature_sections"]:
        raise ValueError("Mobile ROM sections are missing")
    net = mobile["used"] - baseline["used"]
    gross = sum(section["bytes"] for section in mobile["feature_sections"])
    accounted = max(net, gross)
    return {"budget_bytes": limit, "net_addon_bytes": net,
            "feature_section_bytes": gross, "integration_net_bytes": net - gross,
            "accounted_bytes": accounted, "within_budget": accounted <= limit,
            "required_savings_bytes": max(0, accounted - limit),
            "remaining_budget_bytes": limit - accounted,
            "baseline_free_bytes": baseline["free"], "mobile_free_bytes": mobile["free"],
            "mobile_rom0_free_bytes": mobile["rom0_free"],
            "feature_sections": mobile["feature_sections"],
            "mobile_rom_banks": mobile["banks"]}


def measure(baseline_path, mobile_path, limit=ROM_BUDGET_BYTES):
    paths = (baseline_path, mobile_path)
    data = [path.read_bytes() for path in paths]
    if any(len(rom) != 2 * 1024 * 1024 for rom in data):
        raise ValueError("This budget checkpoint requires two 2 MiB ROMs")
    if any(rom[0x147:0x14a] != bytes((0x10, 6, 3)) for rom in data):
        raise ValueError("Mapper/ROM/save allocation differs from the 2 MiB RTC counterpart")
    layouts = [layout(path.with_suffix(".map").read_text()) for path in paths]
    if any(item["used"] + item["free"] != len(rom) for item, rom in zip(layouts, data)):
        raise ValueError("Map ROM capacity differs from its ROM file")
    report = compare(*layouts, limit)
    report.update({"baseline_sha256": hashlib.sha256(data[0]).hexdigest(),
                   "mobile_sha256": hashlib.sha256(data[1]).hexdigest(),
                   "scope": "Matching source/options, freshly linked maps. Bytes only; no runtime acceptance."})
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", type=Path, default=REPO / "polishedcrystal-3.2.3.gbc")
    parser.add_argument("--mobile", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.gbc")
    parser.add_argument("--output", type=Path)
    parser.add_argument("--strict", action="store_true", help="Fail when the binding 10,000-byte budget is exceeded")
    args = parser.parse_args()
    report = measure(args.baseline, args.mobile)
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n")
    print(f"ROM addon: {report['net_addon_bytes']:,} net bytes; "
          f"{report['feature_section_bytes']:,} in feature sections; "
          f"budget {report['budget_bytes']:,}; required savings {report['required_savings_bytes']:,}")
    if args.strict and not report["within_budget"]:
        raise SystemExit("ROM budget failed: optimization is required before acceptance")


if __name__ == "__main__":
    main()
