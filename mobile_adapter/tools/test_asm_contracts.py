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
parser.add_argument("--renderer", choices=("virtual",), default="virtual", help="Independent renderer; retained for existing test commands")
parser.add_argument("--only", choices=("all", "admission"), default="all", help="Run a focused compiled family during renderer iteration")
args = parser.parse_args()
if args.renderer == "virtual":
    from prepare_virtual_avatars import inventory, source
    avatar_inventory = inventory()
    # Host-only graphics preparation also supports a clean checkout. No ROM or
    # emulator build is needed for this production-assembly fixture.
    paths = list(dict.fromkeys(row[2] for row in avatar_inventory))
    subprocess.run(["make", *paths, "RGBDS=" + str(args.rgbds) + "/"], cwd=REPO, check=True,
                   stdout=subprocess.DEVNULL)
    assets = REPO / "mobile_adapter/build-mobile/virtual-avatars.asm"
    assets.parent.mkdir(parents=True, exist_ok=True)
    assets.write_text(source(avatar_inventory))
with tempfile.TemporaryDirectory(prefix="mobile-asm-") as temporary:
    out = Path(temporary)
    fixture = (REPO / "mobile_adapter/tests/driver.asm").read_text()
    fixture += '\nDEF CopyBytes EQU $20\nDEF ByteFill EQU $28\nDEF AddNTimes EQU $18\nDEF FarCall EQU $10\nDEF DoNothing EQU $0150\n'
    native = (REPO / "engine/overworld/map_objects.asm").read_text()
    import re
    for name in ("PRIORITY_LOW", "PRIORITY_NORM", "PRIORITY_HIGH"):
        definition = re.search(r"^DEF " + name + r"\s+EQU .*", native, re.M)
        require(definition is not None, "Native emission priority definition missing")
        fixture += definition[0] + "\n"
    fixture += 'SECTION "OAM bounds execution fixture", ROM0[$220]\nBoundsFixture::\n mobile_native_oam_bounds\n ld a, 1\n ret\n.full: xor a\n ret\n'
    fixture += 'SECTION "Serial return stub", ROM0[$200]\nSerial:\n.end: ret\n'
    fixture += 'SECTION "Native bank return fixture", ROM0[$8]\nBankswitch::\n ldh [hROMBank], a\n ld [rROMB0], a\n ret\n'
    fixture += 'SECTION "Resident rectangle fixture", ROM0[$250]\n mobile_native_rectangle_trampoline\n'
    fixture += 'SECTION "Render link stubs", ROM0[$3000]\n'
    fixture += 'FarCall_hl:: ret\nGetFarByte:: ret\nGetFarWord:: ret\nFarCopyBytes:: ret\nSwapHLDE:: ret\nFarDecompressWRA6InB:: ret\nGet2bpp:: ret\nCopy2bpp:: ret\nFarDecompressToDE:: ret\nGetMapConnections:: ret\nGetMapAttributes:: ret\nClearJoypad:: ret\nOpenText:: ret\nCloseText:: ret\nPrintText:: ret\nLoadMenuHeader:: ret\nVerticalMenu:: ret\nExitMenu:: ret\nWaitButton:: ret\n'
    fixture += 'SECTION "Native facing data", ROMX, BANK[2]\nINCLUDE "data/sprites/facings.asm"\n'
    fixture += 'SECTION "Sprite link stub", ROMX, BANK[1]\nSpriteHeaders:: ds SPRITE_VARS * SPRITEDATA_LENGTH\nGetSprite:: ret\nMarkUsedPal:: ret\nCopyObjectSpritePalHandler:: ret\nLookupOBPalette:: ret\n\nINCLUDE "data/player/state_sprites.asm"\n'
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
                    "-P", "includes.asm",
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
    if args.only == "admission":
        for line in (out / "driver.sym").read_text().splitlines():
            alias = re.match(r"^([0-9a-fA-F]+)\s+(wMobile(?:Virtual|NativeShape)\S+)$", line)
            if alias:
                address, name = alias.groups()
                symbols[name] = (0 if int(address, 16) < 0xd000 else 5, int(address, 16))
        from asm_admission_plan_tests import verify_admission_plans
        verify_admission_plans(rom, symbols, require)
        print("Focused compiled admission contracts passed")
        raise SystemExit(0)
    from asm_peer_invalidation_tests import verify_peer_invalidation
    verify_peer_invalidation(rom, symbols, require)
    from asm_raw_rectangle_tests import verify_raw_rectangles
    verify_raw_rectangles(rom, symbols, require)
    for line in (out / "driver.sym").read_text().splitlines():
        alias = re.match(r"^([0-9a-fA-F]+)\s+(wMobile(?:Virtual|NativeShape)\S+)$", line)
        if alias:
            address, name = alias.groups()
            if name.startswith("wMobileVirtualPlan"):
                require(0xd000 <= int(address, 16) < 0xdd00, "Plan alias escaped audited private bank 5")
            symbols[name] = (0 if int(address, 16) < 0xd000 else 5, int(address, 16))
    from asm_admission_plan_tests import verify_admission_plans
    verify_admission_plans(rom, symbols, require)
    from asm_graphics_ownership_tests import verify_graphics_ownership
    verify_graphics_ownership(rom, symbols, require)
    # Execute each emitted suffix store sequence from the compiled jump table.
    # Verify every prefix byte survives, including tile/attribute bytes of hidden
    # quads, and all forty possible suffix lengths stay within the DMA page.
    bank, table = symbols["MobileVirtualComposeStep.suffix_table"]
    stop = symbols["MobileVirtualComposeStep.suffix40"][1]
    output = (symbols["wMobileActorTiles1"][1] + 13 * 4 + 255) & 0xff00
    def suffix_byte(address):
        return rom[bank * 0x4000 + address - 0x4000]
    for first in range(41):
        entry = table + first * 2
        pc = suffix_byte(entry) | suffix_byte(entry + 1) << 8
        require(pc == symbols[f"MobileVirtualComposeStep.suffix{first}"][1],
                "Compiled suffix dispatch changed")
        memory = bytearray([0xa5] * 65536)
        expected = bytearray(memory)
        expected[output + first * 4:output + 160:4] = bytes([160]) * (40 - first)
        clocks = 0
        while pc != stop:
            require(suffix_byte(pc) == 0xea, "Unexpected suffix store instruction")
            address = suffix_byte(pc + 1) | suffix_byte(pc + 2) << 8
            memory[address] = 160
            pc += 3
            clocks += 16
            require(clocks <= 640, "Suffix exceeds its complete forty-quad bound")
        require(memory == expected, "Suffix changed an admitted prefix or non-Y byte")
    print("Emitted OAM suffix stores: 41 cases, maximum 640 cycles")
    from asm_aligned_inventory_tests import verify_aligned_inventory
    verify_aligned_inventory(rom, symbols, require)
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
    # Walk the compiled WRAM IRQ, not just source labels/macros. No reachable
    # branch can depend on the interrupted ROM bank or switch away its code.
    image_bank, image_start = symbols["MobileIRQImageSource"]
    _, image_end = symbols["MobileIRQImageEnd"]
    runtime_bank, runtime_start = symbols["MobileIRQRuntimeStart"]
    _, runtime_end = symbols["MobileIRQRuntimeEnd"]
    require(runtime_bank == 2 and runtime_start == 0xd980 and runtime_end <= 0xdb00,
            "Executable IRQ escaped its protected WRAM island")
    require(image_end - image_start == runtime_end - runtime_start,
            "IRQ source/runtime image sizes differ")
    image_offset = image_bank * 0x4000 + image_start - 0x4000
    irq = rom[image_offset:image_offset + runtime_end - runtime_start]
    pending, visited = [symbols["MobileSerialIRQ"][1]], set()
    two_byte = {0x06, 0x0e, 0x16, 0x1e, 0x26, 0x2e, 0x36, 0x3e, 0x18,
                0x20, 0x28, 0x30, 0x38, 0xc6, 0xce, 0xd6, 0xde, 0xe0, 0xe6,
                0xe8, 0xee, 0xf0, 0xf6, 0xf8, 0xfe, 0xcb}
    three_byte = {0x01, 0x08, 0x11, 0x21, 0x31, 0xc2, 0xc3, 0xc4, 0xca,
                  0xcc, 0xcd, 0xd2, 0xd4, 0xda, 0xdc, 0xea, 0xfa}
    while pending:
        pc = pending.pop()
        if pc in visited:
            continue
        require(runtime_start <= pc < runtime_end, "IRQ reached outside its image")
        visited.add(pc)
        at = pc - runtime_start
        op = irq[at]
        width = 3 if op in three_byte else 2 if op in two_byte else 1
        require(at + width <= len(irq), "IRQ instruction exceeds image")
        require(op not in (0xfb, 0xd9, 0xe9) and op & 0xc7 != 0xc7,
                "IRQ enables interrupts or makes an unaudited indirect transfer")
        require(not (op == 0xe0 and irq[at + 1] == 0x70) and
                not (op == 0xea and int.from_bytes(irq[at + 1:at + 3], "little") == 0xff70),
                "RAM IRQ switches away its executing bank")
        if op in (0xc2, 0xc3, 0xca, 0xcd, 0xd2, 0xda):
            pending.append(int.from_bytes(irq[at + 1:at + 3], "little"))
        elif op in (0x18, 0x20, 0x28, 0x30, 0x38):
            delta = irq[at + 1]
            pending.append(pc + 2 + (delta if delta < 128 else delta - 256))
        if op not in (0xc9, 0xc3, 0x18):
            pending.append(pc + width)
    print(f"Protected RAM serial IRQ: {len(irq)} bytes, {len(visited)} instructions checked")
    # OBJ bank-1 $ff shares physical VRAM with signed BG tile $ff. Guard
    # every authored tileset, including future maps, rather than a map whitelist.
    pairs = 0
    for attributes in sorted((REPO / "data/tilesets").glob("*_attributes.bin")):
        metatiles = attributes.with_name(attributes.name.replace("_attributes.bin", "_metatiles.bin"))
        require(metatiles.exists(), f"Tileset attribute data has no metatile counterpart: {attributes.name}")
        attrs, tiles = attributes.read_bytes(), metatiles.read_bytes()
        require(len(attrs) == len(tiles), f"Tileset ownership data length mismatch: {attributes.name}")
        require(not any(tile == 255 and attr & 8 for tile, attr in zip(tiles, attrs)),
                f"Native bank-1 BG tile $ff overlaps the private avatar window: {attributes.name}")
        pairs += 1
    require(pairs > 0, "No native tileset ownership inventory was checked")
    native_loaders = (REPO / "home/map.asm").read_text()
    bank_one_loader = native_loaders.split("_LoadTilesetGFX2:\n", 1)[1].split("_LoadTilesetGFX0:", 1)[0]
    require("ld de, vTiles4" in bank_one_loader and "jr _DoLoadTilesetGFX" in bank_one_loader,
            "Native bank-1 tileset destination changed; re-audit private tile ownership")
    count_loader = native_loaders.split("_DoLoadTilesetGFX:\n", 1)[1].split("_DoLoadTilesetGFX0:", 1)[0]
    require(re.search(r"ld c, \$7f\b", count_loader) is not None,
            "Native bank-1 tileset loader no longer leaves its final tile unused")
    print(f"Native tileset ownership: {pairs} bank-aware asset pairs and loader bounds passed")
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
        copy_bank, copy_body = symbols['MobileCopiedFacingStepDown0']
        original_body = symbols['FacingStepDown0'][1]
        mirrored = copy_bank * 0x4000 + copy_body + pointer - original_body - 0x4000
        require(rom[mirrored:mirrored + 1 + count * 4] == rom[start:start + 1 + count * 4],
                f'Upper native facing mirror differs at facing {index}')
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
            require(min(ys) == 0, "Canonical native facing changed the aligned-scene template origin")
            require(max(demand) <= 2, "Four-quad native figure invalidates five-figure scanline proof")
            flags = [rom[start + 3 + quad * 4] & 8 for quad in range(4)]
            require(len(set(flags)) == 1,
                    "Four-quad facing requires per-quad palette inventory")
    if "MobileVirtualActorVersion" in symbols:
        # Test the emitted palette-choice table against hardware ownership for
        # every eight-slot mask, including weather and ordinary slots6/7.
        color_bank, color_start = symbols['MobileVirtualFreeColor']
        color_start += (color_bank - 1) * 0x4000
        for occupied in range(256):
            chosen = rom[color_start + occupied]
            require((chosen == 255) == (occupied == 255),
                    'Palette choice omitted available hardware capacity')
            if chosen != 255:
                require(chosen < 8 and not occupied & (1 << chosen),
                        'Palette choice overwrites an admitted hardware color')
        print('All 256 hardware palette ownership masks passed')
        from asm_rectangle_tests import verify_rectangles
        verify_rectangles(rom, symbols, require)
        from asm_row_tests import verify_rows
        verify_rows(rom, symbols, require)
        from asm_native_template_tests import verify_native_templates
        verify_native_templates(rom, symbols, require)
        # Fast native DMA admission depends on the original emitted quad order,
        # not just a set of rectangle coordinates. Reject upstream drift here.
        facing_bank, facing_table = symbols['Facings']
        table_offset = facing_bank * 0x4000 + facing_table - 0x4000
        for index in range(62):
            pointer = int.from_bytes(rom[table_offset + index * 2:table_offset + index * 2 + 2], 'little')
            offset = facing_bank * 0x4000 + pointer - 0x4000
            if rom[offset] == 4:
                quads = [rom[offset + 1 + quad * 4:offset + 5 + quad * 4] for quad in range(4)]
                require([quad[0] for quad in quads] == [0, 0, 8, 8],
                        'Canonical native facing no longer has audited reverse-OAM Y order')
                require(all(not quad[2] & 8 for quad in quads),
                        'Canonical native facing now requires NEXT_PALETTE handling')
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
        require("MobileNativeSaveObjects" not in symbols,
                "Independent presentation must not include save projection code")
    # Existing save/object/stack symbols must still retain their baseline addresses.
    for name, expected in {"wStackBottom": (0, 0xc000), "wStackTop": (0, 0xc0ff),
                           "hROMBank": (0, 0xff87)}.items():
        require(symbols[name] == expected, f"Existing layout changed: {name}")
print("Production assembly, bounds and cross-language vector checks passed")
