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
parser.add_argument("--renderer", choices=("native", "virtual"), default="native")
args = parser.parse_args()
if args.renderer == "virtual":
    from prepare_virtual_avatars import inventory, source
    avatar_inventory = inventory()
    # Host-only graphics preparation also supports a clean checkout. No ROM or
    # emulator build is needed for this production-assembly fixture.
    paths = list(dict.fromkeys(row[2] for row in avatar_inventory))
    subprocess.run(["make", *paths, "RGBDS=" + str(args.rgbds) + "/"], cwd=REPO, check=True,
                   stdout=subprocess.DEVNULL)
    assets = REPO / "mobile_adapter/build-mobile-virtual/virtual-avatars.asm"
    assets.parent.mkdir(parents=True, exist_ok=True)
    assets.write_text(source(avatar_inventory))
with tempfile.TemporaryDirectory(prefix="mobile-asm-") as temporary:
    out = Path(temporary)
    fixture = (REPO / "mobile_adapter/tests/driver.asm").read_text()
    fixture += '\nDEF CopyBytes EQU $20\nDEF ByteFill EQU $28\nDEF AddNTimes EQU $18\nDEF FarCall EQU $10\nDEF Bankswitch EQU $08\nDEF DoNothing EQU $0150\n'
    fixture += 'SECTION "OAM bounds execution fixture", ROM0[$220]\nBoundsFixture::\n mobile_native_oam_bounds\n ld a, 1\n ret\n.full: xor a\n ret\n'
    fixture += 'SECTION "Serial return stub", ROM0[$200]\nSerial:\n.end: ret\n'
    fixture += 'SECTION "Render link stubs", ROM0[$3000]\n'
    fixture += 'GetFarByte:: ret\nGetFarWord:: ret\nFarCopyBytes:: ret\nSwapHLDE:: ret\nFarDecompressWRA6InB:: ret\nGet2bpp:: ret\nCopy2bpp:: ret\nFarDecompressToDE:: ret\nGetMapConnections:: ret\nGetMapAttributes:: ret\nClearJoypad:: ret\nOpenText:: ret\nCloseText:: ret\nPrintText:: ret\nLoadMenuHeader:: ret\nVerticalMenu:: ret\nExitMenu:: ret\nWaitButton:: ret\n'
    fixture += 'SECTION "Native facing data", ROMX, BANK[2]\nINCLUDE "data/sprites/facings.asm"\n'
    fixture += 'SECTION "Sprite link stub", ROMX, BANK[1]\nSpriteHeaders:: ds SPRITE_VARS * SPRITEDATA_LENGTH\nGetSprite:: ret\nMarkUsedPal:: ret\nCopyObjectSpritePalHandler:: ret\n\nINCLUDE "data/player/state_sprites.asm"\n'
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
    subprocess.run([str(args.rgbds / "rgbasm"), "-Weverything", "-DMOBILE_ADAPTER",
                    *(["-DMOBILE_VIRTUAL_ACTORS"] if args.renderer == "virtual" else []), "-P", "includes.asm",
                    "-o", str(out / "driver.o"), str(out / "driver.asm")], cwd=REPO, check=True)
    subprocess.run([str(args.rgbds / "rgblink"), "-Weverything", "-n", str(out / "driver.sym"),
                    "-l", str(out / "ram.link"), "-o", str(out / "driver.gb"), str(out / "driver.o")], cwd=REPO, check=True)
    symbols = read_symbols(out / "driver.sym")
    # Execute the actual compiled bounds macro in a tiny instruction runner.
    # No Game Boy emulator, game RAM, or reimplementation of its predicate.
    machine = (out / "driver.gb").read_bytes()
    def run_bounds(a, c):
        pc, zero, carry, stack = symbols["BoundsFixture"][1], False, False, []
        for _ in range(64):
            op = machine[pc]
            pc += 1
            if op == 0xa7:  # and a
                zero, carry = a == 0, False
            elif op == 0xfe:  # cp immediate
                value = machine[pc]
                pc += 1
                zero, carry = a == value, a < value
            elif op in (0xca, 0xd2, 0xc3):  # jp z/nc/unconditional
                target = int.from_bytes(machine[pc:pc + 2], "little")
                pc += 2
                if op == 0xc3 or (zero if op == 0xca else not carry):
                    pc = target
            elif op == 0xf5:
                stack.append((a, zero, carry))
            elif op == 0xf1:
                a, zero, carry = stack.pop()
            elif op == 0x79:
                a = c
            elif op == 0x87:
                carry = a >= 128
                a = (a * 2) & 255
                zero = a == 0
            elif op == 0xd6:
                value = machine[pc]
                pc += 1
                carry = a < value
                a = (a - value) & 255
                zero = a == 0
            elif op == 0xb9:
                zero, carry = a == c, a < c
            elif op in (0x18, 0x28, 0x30):
                delta = machine[pc]
                pc += 1
                if delta >= 128:
                    delta -= 256
                if op == 0x18 or (zero if op == 0x28 else not carry):
                    pc += delta
            elif op == 0x3e:
                a = machine[pc]
                pc += 1
            elif op == 0xaf:
                a, zero, carry = 0, True, False
            elif op == 0xc9:
                require(not stack, "OAM bounds left an unbalanced stack")
                return a
            else:
                raise ValueError(f"Unsupported bounds fixture opcode {op:02x}")
        raise ValueError("OAM bounds fixture did not terminate")
    for count in range(256):
        for end in range(256):
            expected = 1 <= count <= 40 and end <= 156 and count * 4 - 4 <= end
            require(bool(run_bounds(count, end)) == expected,
                    f"Compiled OAM bounds failed for count={count}, reverse offset={end}")
    bank, address = symbols["MobileRequestVector"]
    offset = bank * 0x4000 + address - 0x4000
    rom = (out / "driver.gb").read_bytes()
    require(rom[offset:offset + 12] == bytes.fromhex("50 43 01 01 00 01 00 04 54 45 53 54"),
            "Actual ROM vector differs from Rust protocol")
    bank, address = symbols["MobileHelloVector"]
    offset = bank * 0x4000 + address - 0x4000
    require(rom[offset:offset + 23] == bytes.fromhex(
        "50 43 01 10 00 01 00 0f 04 00 04 00 01 00 00 00 00 00 00 00 00 00 00"),
        "Actual ROM HELLO differs from the standard-build lobby protocol")
    require(symbols["wMobileResidentStart"] == (0, 0xcec4), "Resident placement changed")
    require(symbols["wMobileBufferStart"] == (2, 0xd4eb), "Buffer placement changed")
    require(symbols["wMobileBufferEnd"][1] <= 0xe000, "Buffer overflow")
    if args.renderer == "native":
        bank, address = symbols["MobileNativeCopyFourTiles"]
        offset = bank * 0x4000 + address - 0x4000
        require(rom[offset:offset + 5] == bytes.fromhex("f3 e0 4f 06 04"),
                "Native tile copy must select VRAM bank only after disabling interrupts")
    else:
        require("MobileNativeCopyFourTiles" not in symbols,
                "Virtual renderer must exclude the retired active-scanout tile copier")
    require(symbols["wMobileAvatarBank"] == symbols["wMobileNativeOriginalsEnd"] and
            symbols["wMobileAvatarBank"][1] < 0xd000, "Private VRAM bank latch escaped unbanked feature RAM")
    def facing(table, index):
        bank, address = symbols[table]
        offset = bank * 0x4000 + address - 0x4000 + index * 2
        pointer = int.from_bytes(rom[offset:offset + 2], "little")
        offset = bank * 0x4000 + pointer - 0x4000
        return bytearray(rom[offset:offset + 17])

    require(symbols["wMobileActorTiles1"][1] % 16 == 0, "Native GDMA source alignment changed")
    for index in range(16):
        require(facing("Facings", index)[0] == 4, "Native avatar must contain four quads")
    # The cheap five-figure path relies on each four-quad native template
    # contributing at most two hardware objects to any scanline. New geometry
    # must fail this emitted-data contract instead of silently overflowing OAM.
    table_bank, table_address = symbols["Facings"]
    end_bank, end_address = symbols["FacingStepDown0"]
    native_count = (end_address - table_address) // 2 - 1
    require(table_bank == end_bank and native_count > 16, "Native facing inventory changed")
    for index in range(native_count):
        bank, address = symbols["Facings"]
        offset = bank * 0x4000 + address - 0x4000 + index * 2
        pointer = int.from_bytes(rom[offset:offset + 2], "little")
        start = bank * 0x4000 + pointer - 0x4000
        count = rom[start]
        require(0 < count <= 16, "Native figure exceeds independent renderer scratch bound")
        if args.renderer == "virtual":
            for quad in range(count):
                attributes, tile = rom[start + 3 + quad * 4:start + 5 + quad * 4]
                if attributes & 4:  # ABSOLUTE_TILE_ID
                    require(tile not in set(range(0x64, 0x6f)) | {0x3f},
                            "Native absolute tile overlaps a private pose window")
                else:
                    require((tile & 0x7f) <= 14,
                            f"Native facing {index} tile {tile:02x} exceeds the audited fifteen-tile atlas footprint")
        if count == 4:
            ys = [rom[start + 1 + quad * 4] for quad in range(4)]
            ys = [y - 256 if y >= 128 else y for y in ys]
            demand = [sum(y <= line < y + 8 for y in ys)
                      for line in range(min(ys), max(ys) + 8)]
            require(ys[0] == ys[1] and ys[2] == ys[3] and abs(ys[2] - ys[0]) == 8,
                    "Four-quad native figure requires exact-row admission instead of interval admission")
            require(max(demand) <= 2, "Four-quad native figure invalidates five-figure scanline proof")
            flags = [rom[start + 3 + quad * 4] & 8 for quad in range(4)]
            require(len(set(flags)) == 1,
                    "Four-quad facing requires per-quad palette inventory")
    if "MobileVirtualActorVersion" in symbols:
        from asm_rectangle_tests import verify_rectangles
        verify_rectangles(rom, symbols, require)
        from asm_row_tests import verify_rows
        verify_rows(rom, symbols, require)
        def compiled_bytes(name, length):
            bank, address = symbols[name]
            offset = bank * 0x4000 + address - 0x4000
            return rom[offset:offset + length]
        native_sprites = compiled_bytes("PlayerStateSprites", len(avatar_inventory))
        require(compiled_bytes("MobileVirtualAssetSprites", len(avatar_inventory)) == native_sprites,
                "Build-prepared avatar inventory differs from the actual native state mapping")
        pointers = compiled_bytes("MobileVirtualAssetPointers", len(avatar_inventory) * 2)
        bank = symbols["MobileVirtualAssetPointers"][0]
        for index, (_, _, path) in enumerate(avatar_inventory):
            address = int.from_bytes(pointers[index * 2:index * 2 + 2], "little")
            offset = bank * 0x4000 + address - 0x4000
            require(rom[offset:offset + 384] == (REPO / path).read_bytes(),
                    "Build-prepared avatar differs from native graphics: " + path)
        bank, address = symbols["MobileNativeSaveObjects"]
        offset = bank * 0x4000 + address - 0x4000
        require(rom[offset] == 0xc9, "Virtual presentation must never project private data into saves")
    # Existing save/object/stack symbols must still retain their baseline addresses.
    for name, expected in {"wStackBottom": (0, 0xc000), "wStackTop": (0, 0xc0ff),
                           "hROMBank": (0, 0xff87)}.items():
        require(symbols[name] == expected, f"Existing layout changed: {name}")
print("Production assembly, bounds and cross-language vector checks passed")
