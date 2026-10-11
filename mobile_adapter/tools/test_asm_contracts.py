#!/usr/bin/env python3
"""Compile the actual driver, RAM layout and wire vectors without emulation."""
import argparse
from pathlib import Path
import subprocess
import tempfile
from probe_emulator import read_symbols, require

REPO = Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--rgbds", type=Path, required=True, help="Pinned RGBDS directory")
args = parser.parse_args()
with tempfile.TemporaryDirectory(prefix="mobile-asm-") as temporary:
    out = Path(temporary)
    fixture = (REPO / "mobile_adapter/tests/driver.asm").read_text()
    fixture += '\nDEF CopyBytes EQU $20\nDEF ByteFill EQU $28\nDEF FarCall EQU $10\nDEF Bankswitch EQU $08\nDEF DoNothing EQU $0150\n'
    fixture += 'SECTION "Serial return stub", ROM0[$200]\nSerial:\n.end: ret\n'
    fixture += 'SECTION "Render link stubs", ROM0[$3000]\n'
    fixture += 'GetFarByte:: ret\nSwapHLDE:: ret\nFarDecompressWRA6InB:: ret\nGet2bpp:: ret\nCopy2bpp:: ret\nFarDecompressToDE:: ret\nClearJoypad:: ret\nOpenText:: ret\nCloseText:: ret\nPrintText:: ret\nLoadMenuHeader:: ret\nVerticalMenu:: ret\nExitMenu:: ret\nWaitButton:: ret\n'
    fixture += 'SECTION "Native facing data", ROMX, BANK[2]\nINCLUDE "data/sprites/facings.asm"\n'
    fixture += 'SECTION "Sprite link stub", ROMX, BANK[1]\nGetSprite:: ret\nMarkUsedPal:: ret\nINCLUDE "data/player/state_sprites.asm"\n'
    fixture += 'SECTION "LCD layout contracts", ROMX, BANK[$12]\n'
    oam = (REPO / "engine/gfx/load_push_oam.asm").read_text()
    fixture += oam[oam.index('LOAD "OAM DMA"'):oam.index("ENDL") + 4] + "\n"
    for path, label in (("engine/pokedex/lcd.asm", "PHB_LCDCode:"),
                        ("engine/pc/bills_pc_ui.asm", "BillsPC_LCDCode:")):
        source = (REPO / path).read_text()
        start = source.index("\n" + label) + 1
        end = source.index("\n.End:", start) + len("\n.End:")
        fixture += source[start:end] + "\n"
    (out / "driver.asm").write_text(fixture)
    layout = (REPO / "layout.link").read_text()
    (out / "ram.link").write_text(layout[layout.index("VRAM $0"):])
    subprocess.run([str(args.rgbds / "rgbasm"), "-Weverything", "-DMOBILE_ADAPTER", "-P", "includes.asm",
                    "-o", str(out / "driver.o"), str(out / "driver.asm")], cwd=REPO, check=True)
    subprocess.run([str(args.rgbds / "rgblink"), "-Weverything", "-n", str(out / "driver.sym"),
                    "-l", str(out / "ram.link"), "-o", str(out / "driver.gb"), str(out / "driver.o")], cwd=REPO, check=True)
    symbols = read_symbols(out / "driver.sym")
    bank, address = symbols["MobileRequestVector"]
    offset = bank * 0x4000 + address - 0x4000
    rom = (out / "driver.gb").read_bytes()
    require(rom[offset:offset + 12] == bytes.fromhex("50 43 01 01 00 01 00 04 54 45 53 54"),
            "Actual ROM vector differs from Rust protocol")
    bank, address = symbols["MobileHelloVector"]
    offset = bank * 0x4000 + address - 0x4000
    require(rom[offset:offset + 23] == bytes.fromhex(
        "50 43 01 10 00 01 00 0f 03 00 03 00 01 00 00 00 00 00 00 00 00 00 00"),
        "Actual ROM HELLO differs from the standard-build lobby protocol")
    require(symbols["wMobileResidentStart"] == (0, 0xcec4), "Resident placement changed")
    require(symbols["wMobileBufferStart"] == (2, 0xd4eb), "Buffer placement changed")
    require(symbols["wMobileBufferEnd"][1] <= 0xe000, "Buffer overflow")
    def facing(table, index):
        bank, address = symbols[table]
        offset = bank * 0x4000 + address - 0x4000 + index * 2
        pointer = int.from_bytes(rom[offset:offset + 2], "little")
        offset = bank * 0x4000 + pointer - 0x4000
        return bytearray(rom[offset:offset + 17])

    for index in range(16):
        native, remote = facing("Facings", index), facing("MobileSocialFacings", index)
        require(native[0] == remote[0] == 4, "Approved avatar must contain four 8x8 quads")
        for attribute in range(3, 17, 4):
            native[attribute] &= 0xe0
        require(native == remote, f"Remote facing {index} differs from native sprite geometry")
    # Existing save/object/stack symbols must still retain their baseline addresses.
    for name, expected in {"wStackBottom": (0, 0xc000), "wStackTop": (0, 0xc0ff),
                           "hROMBank": (0, 0xff87)}.items():
        require(symbols[name] == expected, f"Existing layout changed: {name}")
print("Production assembly, bounds and cross-language vector checks passed")
