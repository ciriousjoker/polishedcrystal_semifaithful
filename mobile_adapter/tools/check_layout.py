#!/usr/bin/env python3
"""Compare completed build layouts and enforce dedicated mobile placement."""
import argparse
import json
import re
from pathlib import Path
from probe_emulator import BASELINE_SHA256, digest, read_symbols, require
from audit_rom_budget import layout, validate_mobile

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
avatar_bytes = new["wMobileAvatarCacheEnd"][1] - new["wMobileAvatarCache1"][1]
require(new["wMobileAvatarCache1"] == (2, 0xda40) and avatar_bytes == 1152, "Avatar cache placement changed")
require(new["wMobileBufferEnd"][1] <= 0xda40 and new["wMobileAvatarCacheEnd"][1] <= 0xe000, "Avatar cache overlaps retained data")
legacy_allocations = ('wMobileRenderMapMask', 'wMobileScanlineDelta',
                      'wMobileOAMOutput', 'wMobileNPCOptional', 'wMobileOAMSnapshot')
require(not any(name in new for name in legacy_allocations), 'Unused legacy renderer memory returned')
backups = new["wMobileActorBackup1"]
tiles = new["wMobileActorTiles1"]
require(backups == (5, 0xd562), "Native object backup placement changed")
require(new["wMobileActorBackupEnd"][1] - backups[1] == 3 * 38,
        "Native object/camera backup pool changed")
require(tiles[0] == 5 and tiles[1] % 16 == 0 and
        new["wMobileActorTilesEnd"][1] - tiles[1] == 3 * 384 and
        new["wMobileActorTilesEnd"][1] <= 0xdd00, "Raw tile backups overlap native memory or violate GDMA alignment")
require(not any(name.startswith("sMobile") for name in new), "Mobile state must never allocate SRAM")
helpers = sum(new[end][1] - new[start][1] for start, end in
              (("MobileNativeInstanceStart", "MobileNativeInstanceEnd"),
               ("MobileNativeTileCodeStart", "MobileNativeTileCodeEnd"),
               ("MobileNativeSaveCodeStart", "MobileNativeSaveCodeEnd")))
policy_bytes = new["MobileNativePolicyEnd"][1] - new["MobileNativePolicyStart"][1]
require(policy_bytes <= 1280, "Lease policy ROM budget exceeded")
require(helpers <= 768, "Feature helper ROM budget exceeded")
views = new['wMobileNativeOriginalsEnd'][1] - new['wMobileNativeOriginals'][1]
private_colors = new['wMobilePrivatePalEnd'][1] - new['wMobilePrivatePalTemporary'][1]
palette_code = new['MobileNativePaletteCodeEnd'][1] - new['MobileNativePaletteCodeStart'][1]
require(new['wMobileNativeOriginals'] == (0, 0xcf44) and views == 121 and
        new['wMobileResidentEnd'][1] <= 0xcf44 and new['wMobileNativeOriginalsEnd'][1] <= 0xd000,
        'Native original views overlap resident or native memory')
require(new['wMobilePrivatePalTemporary'] == new['wMobileActorTilesEnd'] and
        private_colors == 53 and new['wMobilePrivatePalEnd'][1] <= 0xdd00,
        'Private palette storage overlaps native memory')
require(palette_code <= 1536, 'Private palette ROM budget exceeded')
require(new['wMobileAvatarBank'] == new['wMobileNativeOriginalsEnd'] and
        new['wMobileAvatarBank'][1] < 0xd000, 'VRAM bank latch escaped private unbanked memory')
near_calls = 0
for source in sorted((REPO / 'mobile_adapter/rom').rglob('*.asm')):
    owner = None
    for number, line in enumerate(source.read_text().splitlines(), 1):
        label = re.match(r'^(Mobile\w+)::?', line)
        if label:
            owner = label[1]
        call = re.match(r'^\s*(?:call|jp|jmp|jr)\s+(?:(?:nz|z|nc|c),\s*)?([A-Za-z_]\w*(?:\.\w+)?)\s*(?:;.*)?$', line)
        if not call or owner not in new or call[1] not in new:
            continue
        origin, target = new[owner], new[call[1]]
        if not (0x4000 <= origin[1] < 0x8000 and 0x4000 <= target[1] < 0x8000):
            continue
        near_calls += 1
        require(origin[0] == target[0],
                f'{source.relative_to(REPO)}:{number}: near call/jump {owner} -> {call[1]} crosses ROM banks')
report = {"banked_near_calls_checked": near_calls, "native_original_view_bytes": views, "private_palette_bytes": private_colors, "private_palette_code_bytes": palette_code, "existing_ram_symbols_checked": len(ram), "existing_ram_symbols_changed": changed,
          "avatar_cache_bytes": avatar_bytes, "removed_legacy_renderer_bytes": 456, "resident_bytes": resident, "buffer_bytes": buffers, "banked_driver_bytes": code, "feature_helper_bytes": helpers, "lease_policy_bytes": policy_bytes, "raw_native_tile_backup_bytes": 1152}
for name in ("baseline", "mobile", "faithful"):
    path = getattr(args, name)
    if name != "baseline":
        report[name + "_mbc30"] = validate_mobile(path.read_bytes(), layout(path.with_suffix(".map").read_text()))
    report[name + "_rom_sha256"] = digest(path)
    report[name + "_summary"] = path.with_suffix(".map").read_text().split("\n\n")[0]
args.output.write_text(json.dumps(report, indent=2) + "\n")
print(f"Layout passed: {len(ram)} existing RAM symbols preserved, {code} driver ROM bytes")
