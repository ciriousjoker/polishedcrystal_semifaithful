#!/usr/bin/env python3
"""Build current disposable fixtures and collect real core evidence for Rust.

This is tooling, not a second test runner. Assertions live in the Rust tests.
No tracked map source, game RAM or historic battery fixture is modified.
"""
import argparse
import concurrent.futures
import contextlib
import fcntl
import gzip
import hashlib
import json
import math
import os
from pathlib import Path
import re
import select
import shutil
import socket
import socketserver
import subprocess
import threading
import time
from storage import active_run, ensure_free_space

ROOT = Path(__file__).resolve().parents[3]
TESTS = ROOT / "mobile_adapter/tests"
CACHE = Path(os.environ.get("MOBILE_TEST_CACHE", "/Volumes/Storage/Cache/mobileadapter"))
ASSUMPTIONS = json.loads((TESTS / "assumptions.json").read_text())
PROTOCOL_SOURCE = (ROOT / "mobile_adapter/server/crates/protocol/src/lobby.rs").read_text()
WIRE_KINDS = {name: int(value, 0) for name, value in re.findall(
    r"pub const (\w+): u8 = (0x[\da-fA-F]+|\d+);", PROTOCOL_SOURCE)}
for role in ("WELCOME", "ROSTER", "DELTA"):
    if role not in WIRE_KINDS:
        raise RuntimeError("Declared lobby wire constant interface changed: " + role)


def run(command, cwd, log, timeout=240):
    with Path(log).open("w") as stream:
        subprocess.run(list(map(str, command)), cwd=cwd, stdout=stream,
                       stderr=subprocess.STDOUT, check=True, timeout=timeout)


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def observer_fingerprint():
    paths = [ROOT / "mobile_adapter/tools/smoke_libretro.cpp", ROOT / "mobile_adapter/tools/profile_core.h",
             TESTS / "support/profile_native.hpp", TESTS / "support/observer.hpp"]
    return hashlib.sha256("".join(sha(path) for path in paths).encode()).hexdigest()


def extend_profile(profile_source):
    header = profile_source / "gb_core/profile_core.h"
    text = (ROOT / "mobile_adapter/tools/profile_core.h").read_text()
    text = text.replace("static void mobile_profile_before_instruction(gb* game, const cpu_regs& regs, int clock) {",
        '#include "acceptance_profile.hpp"\nstatic void mobile_profile_before_instruction(gb* game, const cpu_regs& regs, int clock) {\n    acceptance_profile_before(game, regs);')
    text = text.replace("&span == &mobile_render_spans[10] && mobile_native_descriptor_address",
                        "&span == &mobile_render_spans[10] && mobile_native_shadow_address")
    text = text.replace("for (unsigned i = 0; i < 104; ++i)",
                        "for (unsigned i = 0; mobile_native_descriptor_address && i < 104; ++i)")
    text = text.replace("i * 34", "i * acceptance_native_stride")
    text = text.replace("ram + 0x1000 + mobile_native_object_address", "ram + acceptance_native_object_bank * 0x1000 + mobile_native_object_address")
    text = text.replace("actor[0]", "actor[acceptance_native_sprite_field]").replace("actor[1]", "actor[acceptance_native_map_field]")
    header.write_text(text)
    shutil.copyfile(TESTS / "support/profile_native.hpp", profile_source / "gb_core/acceptance_profile.hpp")


def refresh_observers(meta, prepared):
    if meta.get("observer_fingerprint") == observer_fingerprint():
        return meta
    source = Path(meta["source"])
    core_source = Path(os.environ.get("MOBILE_ACCEPTANCE_CORE_SOURCE",
        str(CACHE / "mbc30-ef247c6/doublecherryGB-libretro-ef247c6")))
    if "MOBILE_ACCEPTANCE_CORE" not in os.environ:
        extend_profile(prepared / "core-source")
        run(["cmake", "--build", prepared / "core-build", "--parallel", "4"], source, prepared / "core-refresh.log")
    else:
        meta["core"] = os.environ["MOBILE_ACCEPTANCE_CORE"]
    run(["c++", "-std=c++17", "-O2", "-I", core_source / "libretro/DoubleCherryEngine",
         ROOT / "mobile_adapter/tools/smoke_libretro.cpp", "-lz", "-o", meta["runner"]], source, prepared / "runner-refresh.log")
    meta["observer_fingerprint"] = observer_fingerprint()
    meta["core_sha256"] = sha(meta["core"])
    (prepared / "manifest.json").write_text(json.dumps(meta, indent=2))
    return meta


def copy_sources(source, destination, reuse_objects=False):
    # Copy ROM build inputs, not the entire checkout (Flutter, scenario batteries,
    # historical patches and generated application output are unrelated).
    roots = {"audio", "constants", "data", "engine", "gfx", "home", "macros",
             "maps", "ram", "tools", "mobile_adapter"}
    ignored = [".git", "target", "build", ".dart_tool", "node_modules", "__pycache__",
               "*.gbc", "*.sym", "*.map", "*.patch", "*.ips", "*.pyc"]
    if not reuse_objects:
        ignored += ["build-*", "build-mobile", "build-faithful-mobile", "*.o"]
    basic_ignore = shutil.ignore_patterns(*ignored)
    def exclude(directory, names):
        relative = Path(directory).relative_to(source)
        blocked = set(basic_ignore(directory, names))
        if relative == Path("."):
            blocked.update(name for name in names if
                (Path(directory, name).is_dir() and name not in roots) or
                (Path(directory, name).is_file() and name not in {"Makefile", "layout.link", ".rgbds-version"}
                 and not name.endswith(".asm") and not (reuse_objects and name.endswith(".o"))))
        elif relative == Path("mobile_adapter"):
            allowed = {"rom", "tools", "tests"}
            if reuse_objects:
                allowed.add("build-mobile")
            blocked.update(set(names) - allowed)
        elif relative == Path("mobile_adapter/tests"):
            blocked.update(set(names) - {"support"})
        elif relative == Path("mobile_adapter/tests/support"):
            blocked.update(name for name in names if not name.endswith(".hpp"))
        return blocked
    shutil.copytree(source, destination, ignore=exclude)


def retain_rom_outputs(source):
    """Keep binaries/symbols required by probes, not disposable compiler trees."""
    for path in source.iterdir():
        if path.is_dir():
            shutil.rmtree(path)
        elif path.suffix not in (".gbc", ".sym", ".map"):
            path.unlink()


@contextlib.contextmanager
def preparation_lock(out):
    # Different cases own independent executions but share one immutable build,
    # fixture per scene and seed per hour/avatar. Only their construction locks.
    with (out / ".prepare.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        yield


def validate_session(out):
    names = subprocess.check_output(["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"], cwd=ROOT).split(b"\0")
    digest = hashlib.sha256()
    for name in sorted(set(names)):
        if not name:
            continue
        path = ROOT / os.fsdecode(name)
        if path.is_file():
            digest.update(name)
            digest.update(path.read_bytes())
    current = digest.hexdigest()
    stamp = out / "source-fingerprint.txt"
    if stamp.exists() and stamp.read_text().strip() != current:
        raise RuntimeError("Source/scenario/observer inputs changed. Choose a fresh MOBILE_ACCEPTANCE_OUTPUT directory; old observations are retained but cannot pass current acceptance.")
    stamp.write_text(current + "\n")


def symbols(path):
    result = {}
    for line in path.read_text().splitlines():
        match = re.match(r"([\da-fA-F]+):([\da-fA-F]+)\s+(\S+)", line)
        if match:
            result[match[3]] = (int(match[1], 16), int(match[2], 16))
    return result


def rom_offset(binding):
    bank, address = binding
    return bank * 0x4000 + address - (0x4000 if address >= 0x4000 else 0)


def patch_data(rom, names, values, destination):
    data = bytearray(rom.read_bytes())
    for name, value in values.items():
        start = rom_offset(names[name])
        data[start:start + len(value)] = value
    data[0x14e:0x150] = ((sum(data[:0x14e]) + sum(data[0x150:])) & 65535).to_bytes(2, "big")
    destination.write_bytes(data)


def prepare(out):
    prepared = out / "prepared"
    if (prepared / "manifest.json").exists():
        meta = json.loads((prepared / "manifest.json").read_text())
        if meta.get("source_fingerprint") != (out / "source-fingerprint.txt").read_text().strip():
            raise RuntimeError("Prepared binaries have no matching source provenance. Choose a fresh output directory.")
        return refresh_observers(meta, prepared)
    prepared.mkdir(exist_ok=True)
    source = prepared / "source"
    if source.exists():
        shutil.rmtree(source)
    copy_sources(ROOT, source)
    rgbds = Path(os.environ.get("MOBILE_ACCEPTANCE_RGBDS", str(CACHE / "rgbds-1.0.4")))
    toolchain = ["make", "-j4", f"RGBDS={rgbds}/"]
    version = re.search(r"^VERSION := (\S+)", (source / "Makefile").read_text(), re.M)[1]
    for variant in ("crystal", "mobile"):
        run(toolchain + [variant], source, prepared / f"{variant}.log")
    pristine_ref = os.environ.get("MOBILE_ACCEPTANCE_PRISTINE_REF") or subprocess.check_output(
        ["git", "merge-base", "HEAD", "master"], cwd=ROOT, text=True).strip()
    pristine = prepared / "pristine"
    pristine.mkdir(exist_ok=True)
    archive = prepared / "pristine.tar"
    run(["git", "archive", "-o", archive, pristine_ref], ROOT, prepared / "archive.log")
    run(["tar", "-xf", archive, "-C", pristine], ROOT, prepared / "extract.log")
    run(toolchain + ["crystal"], pristine, prepared / "pristine.log")
    retain_rom_outputs(pristine)
    archive.unlink()
    core_source = Path(os.environ.get("MOBILE_ACCEPTANCE_CORE_SOURCE",
        str(CACHE / "mbc30-ef247c6/doublecherryGB-libretro-ef247c6")))
    core = Path(os.environ.get("MOBILE_ACCEPTANCE_CORE",
        str(CACHE / "mbc30-ef247c6/build-profile/DoubleCherryGB_libretro.dylib")))
    # Build observers from this checkout, never a stale shared profiling header.
    if "MOBILE_ACCEPTANCE_CORE" not in os.environ:
        profile_source = prepared / "core-source"
        run(["python3", source / "mobile_adapter/tools/prepare_profile_core.py",
             "--source", core_source, "--output", profile_source], source, prepared / "profile-prepare.log")
        run(["python3", source / "mobile_adapter/tools/observe_core_palettes.py",
             profile_source], source, prepared / "palettes.log")
        extend_profile(profile_source)
        run(["cmake", "-S", profile_source, "-B", prepared / "core-build",
             "-DCMAKE_BUILD_TYPE=Release", "-DLIBMOBILE_BUILD_SHARED=OFF"], source, prepared / "cmake.log")
        run(["cmake", "--build", prepared / "core-build", "--parallel", "4"], source, prepared / "core-build.log")
        core = prepared / "core-build/DoubleCherryGB_libretro.dylib"
        if not core.exists():
            core = prepared / "core-build/DoubleCherryGB_libretro.so"
    runner = prepared / "core-runner"
    run(["c++", "-std=c++17", "-O2", "-I", core_source / "libretro/DoubleCherryEngine",
         source / "mobile_adapter/tools/smoke_libretro.cpp", "-lz", "-o", runner], source, prepared / "runner.log")
    # Cargo already fingerprints inputs and locks its target directory. Reuse
    # that cache across runs, but freeze the executable used by this run.
    shared_target = CACHE / "acceptance-build-cache/server"
    run(["cargo", "build", "--locked", "--manifest-path", ROOT / "mobile_adapter/server/Cargo.toml",
         "--target-dir", shared_target, "--bin", "mobile-bridge"], ROOT, prepared / "server.log")
    shutil.copy2(shared_target / "debug/mobile-bridge", prepared / "mobile-bridge")
    manifest = {"source": str(source), "pristine": str(pristine), "pristine_ref": pristine_ref,
        "version": version, "rgbds": str(rgbds), "core": str(core), "runner": str(runner),
        "bridge": str(prepared / "mobile-bridge"),
        "baseline_sha256": sha(source / f"polishedcrystal-{version}.gbc"),
        "pristine_sha256": sha(pristine / f"polishedcrystal-{version}.gbc"),
        "mobile_sha256": sha(source / f"polishedcrystal-mobile-{version}.gbc"),
        "core_sha256": sha(core), "assumptions": ASSUMPTIONS, "observer_fingerprint": observer_fingerprint(),
        "source_fingerprint": (out / "source-fingerprint.txt").read_text().strip()}
    adapter_bindings = {}
    for relative in ("mobile_adapter/tools/smoke_libretro.cpp", "mobile_adapter/tests/support/observer.hpp",
                     "mobile_adapter/tests/support/bootstrap.asm.in"):
        text = (ROOT / relative).read_text()
        names = set(re.findall(r'"([whs][A-Z]\w*)"', text))
        names.update(re.findall(r'(?:call|farcall|jmp|farjp)\s+([A-Z]\w*)', text))
        names.update(re.findall(r'native_addresses\.at\("([^"]+)"\)', text))
        adapter_bindings[relative] = sorted(names)
    (prepared / "adapter-bindings.json").write_text(json.dumps(adapter_bindings, indent=2))
    (prepared / "manifest.json").write_text(json.dumps(manifest, indent=2))
    return manifest


def floor_area(source, name):
    # Current map order and collision source are queried, not frozen numeric IDs.
    import sys
    sys.path.insert(0, str(source / "mobile_adapter/tools"))
    from audit_independent_maps import map_inventory
    record = next(row for row in map_inventory(source) if row["map"] == name)
    tileset = re.search(rf"^\s*map {name}, TILESET_(\w+),", (source / "data/maps/maps.asm").read_text(), re.M)[1].lower()
    collision_rows = re.findall(r"^\s*tilecoll\s+([^;\n]+)",
        (source / f"data/tilesets/{tileset}_collision.asm").read_text(), re.M)
    collisions = [[token.strip() for token in row.split(",")] for row in collision_rows]
    if not collisions or len({len(row) for row in collisions}) != 1:
        raise RuntimeError("Native collision entry format cannot be derived")
    cells = math.isqrt(len(collisions[0]))
    if cells * cells != len(collisions[0]):
        raise RuntimeError("Native collision entries no longer describe a square block")
    blocks = (source / f"maps/{name}.ablk").read_bytes()
    width, height = record["width"] * cells, record["height"] * cells
    record["collision_cells_per_block_axis"] = cells
    def floor(x, y):
        block = blocks[(y // cells) * record["width"] + x // cells]
        return collisions[block][(y % cells) * cells + x % cells] == "FLOOR"
    candidates = []
    for row in range(2, height - 2):
        for x in range(1, width - 7):
            if not all(floor(x + dx, row) for dx in range(8)):
                continue
            if not floor(x + 3, row + 1) or not all(floor(x + dx, row - 1) for dx in (4, 5, 6)):
                continue
            points = [(px, py) for py in range(row - 1, row + 3) for px in range(x, x + 8) if floor(px, py)]
            if len(points) >= 16:
                candidates.append((x, row - 1, points))
    if not candidates:
        raise RuntimeError(f"Declared assumption failed: {name} lacks an eight-cell floor row and sixteen visible floor positions")
    record["origin"] = list(candidates[0][:2])
    record["floor_points"] = candidates[0][2]
    spawn_source = (source / "engine/overworld/spawn_points.asm").read_text().split("EnterMapSpawnPoint:", 1)[1]
    record["spawn_fields"] = re.findall(r"ld \[(wMapGroup|wMapNumber|wXCoord|wYCoord)\], a", spawn_source)[:4]
    if set(record["spawn_fields"]) != {"wMapGroup", "wMapNumber", "wXCoord", "wYCoord"}:
        raise RuntimeError("Native spawn data order cannot be derived from its current loader")
    return record


def palette_choices(meta):
    source = Path(meta["source"])
    declarations = (source / "constants/sprite_data_constants.asm").read_text().split("; MapObjectPals indexes", 1)[1].split("DEF NUM_OW_TIME_OF_DAY_PALS", 1)[0]
    names = re.findall(r"^\s*ow_npc_pal_const\s+(\w+)", declarations, re.M)
    times = (source / "constants/ram_constants.asm").read_text().split("\tconst MORN", 1)[1].split("DEF NUM_DAYTIMES", 1)[0]
    daytime = ["MORN", *re.findall(r"^\s*const\s+(\w+)", times, re.M)].index("DAY")
    rom = source / f"polishedcrystal-{meta['version']}.gbc"
    start = rom_offset(symbols(rom.with_suffix(".sym"))["MapObjectPals"]) + daytime * len(names) * 8
    data = rom.read_bytes()
    player_source = (source / "engine/overworld/player_object.asm").read_text()
    player_palettes = re.findall(r"assert PLAYER_\w+\s*\+\s*1\s*==\s*PAL_NPC_(\w+)", player_source)
    if len(player_palettes) != 4 or not all(name in names for name in player_palettes):
        raise RuntimeError("Declared native avatar palette mapping interface changed")
    avatar_colors = {data[start + names.index(name) * 8:start + (names.index(name) + 1) * 8]
                     for name in player_palettes}
    unique = {}
    for index, name in enumerate(names):
        encoded = bytes(data[start + index * 8:start + (index + 1) * 8])
        # Avoid the four avatar defaults so the crowded scene adds seven colors.
        if encoded in avatar_colors:
            continue
        unique.setdefault(encoded, name)
    if len(unique) < 7:
        raise RuntimeError("Declared palette-table prerequisite failed: fewer than seven distinct non-avatar day palettes")
    return [{"name": name, "encoded_day_colors": list(encoded)} for encoded, name in list(unique.items())[:7]]


def fixture(meta, out, scene):
    directory = out / ("fixture-" + scene)
    if (directory / "manifest.json").exists():
        return json.loads((directory / "manifest.json").read_text())
    directory.mkdir(exist_ok=True)
    source = directory / "source"
    if source.exists():
        shutil.rmtree(source)
    copy_sources(Path(meta["source"]), source, reuse_objects=True)
    record = floor_area(source, "GoldenrodGameCorner")
    x, y = record["origin"]
    primary = (x + 3, y + 1)
    peers = [(x + 4, y + 1), (x + 5, y + 1), (x + 6, y + 1)]
    count = 12 if scene == "pool" else 7 if scene == "palettes" else 4 if scene == "scanline" else 1 if scene in ("interaction", "script", "link", "save", "private-save", "warp", "npc-movement") else 0
    points = [tuple(point) for point in record["floor_points"] if tuple(point) not in [primary, *peers]]
    if scene == "scanline":
        points = [(x, y + 1), (x + 1, y + 1), (x + 2, y + 1), (x + 7, y + 1)]
    if count == 1:
        points = [(primary[0], primary[1] + 1)]
    palette_data = palette_choices(meta)
    colors = [palette["name"] for palette in palette_data]
    header = "GoldenrodGameCorner_MapScriptHeader:\n\tdef_scene_scripts\n\tdef_callbacks\n\tcallback MAPCALLBACK_OBJECTS, AcceptanceSceneReady\n\tdef_warp_events\n\tdef_coord_events\n\tdef_bg_events\n\tdef_object_events\n"
    for i, (px, py) in enumerate(points[:count]):
        if count == 1:
            header += "AcceptanceSeedNPC::\n"
        if scene == "pool" and i >= 6:
            header += f"\titemball_event {px}, {py}, POTION, 1, -1\n"
        else:
            header += f"\tobject_event {px}, {py}, SPRITE_POKEFAN_M, SPRITEMOVEDATA_STANDING_DOWN, 0, 0, -1, PAL_NPC_{colors[i % 7]}, OBJECTTYPE_SCRIPT, 0, AcceptanceNPC, -1\n"
    # Equal-load native controls where the normal object pool can represent them.
    equal_load = scene in ("empty", "scanline", "night", "dark")
    if equal_load:
        header += "if !DEF(MOBILE_ADAPTER)\n"
        for i, ((px, py), figure) in enumerate(zip(peers, ("KRIS", "CRYS", "BETA"))):
            header += f"AcceptanceControlPeer{i}::\n\tobject_event {px}, {py}, SPRITE_{figure}, SPRITEMOVEDATA_STANDING_DOWN, 0, 0, -1, 0, OBJECTTYPE_SCRIPT, 0, AcceptanceNPC, -1\n"
        header += "endc\n"
    header += "\nAcceptanceSceneReady:\n\tcallasm AcceptanceReadyMark\n"
    if scene in ("save", "private-save"):
        header += "\tcallasm AcceptancePauseGameTimer\n"
    header += "\tcallasm AcceptanceSave\n\tendcallback\n\nAcceptanceNPC:\n\topentext\n\twritetext AcceptanceText\n\twaitbutton\n\tclosetext\n"
    if scene == "private-save":
        header += "\tcallasm AcceptanceApplyInteractionFault\n\tcallasm AcceptanceSave\n"
    elif scene == "save":
        header += "\tcallasm AcceptanceSave\n"
    elif scene == "link":
        header += "\tcallasm AcceptanceLink\n"
    elif scene == "script":
        header += "\tloadmem wInputFlags, 255\n\tpause 120\n\tloadmem wInputFlags, 0\n"
    elif scene == "warp":
        header += "\twarp PLAYERS_HOUSE_2F, 3, 3\n"
    elif scene == "npc-movement":
        header += "\tapplymovement 1, AcceptanceMovement\n"
    header += "\tsetevent EVENT_TEMPORARY_UNTIL_MAP_RELOAD_1\n\tcallasm AcceptanceDialogMark\n\tend\n\nAcceptanceText:\n\ttext \"TEST\"\n\tdone\n"
    if scene == "npc-movement":
        header += "\nAcceptanceMovement:\nrept 64\n\tstep_right\n\tstep_left\nendr\n\tstep_end\n"
    def replace_events(path, replacement):
        original = path.read_text()
        labels = list(re.finditer(r"(?m)^[A-Za-z_]\w*:(?::)?", original))
        header_name = path.stem + "_MapScriptHeader"
        header = next((label for label in labels if label[0].rstrip(":") == header_name), None)
        if header is None:
            raise RuntimeError("Declared map header/script interface changed: " + str(path))
        # Other maps may reference scripts defined in this map's source file.
        # Preserve their definitions while replacing only this map's event table.
        following = next((label.start() for label in labels if label.start() > header.start()), len(original))
        path.write_text(original[:header.start()] + replacement + "\n" + original[following:])
    replace_events(source / "maps/GoldenrodGameCorner.asm", header)
    (directory / "events.asm").write_text(header)
    replace_events(source / "maps/PlayersHouse2F.asm", "PlayersHouse2F_MapScriptHeader:\n\tdef_scene_scripts\n\tdef_callbacks\n\tdef_warp_events\n\tdef_coord_events\n\tdef_bg_events\n\tdef_object_events\n")
    if scene in ("night", "dark"):
        maps = source / "data/maps/maps.asm"
        text = maps.read_text()
        pattern = r"(?m)^(\s*map GoldenrodGameCorner,.*,)\s*PALETTE_\w+\s*$"
        replacement = "PALETTE_AUTO" if scene == "night" else "PALETTE_NITE | IN_DARKNESS"
        text, changed = re.subn(pattern, lambda m: m[1] + " " + replacement, text)
        if changed != 1:
            raise RuntimeError("Declared native lighting header interface changed")
        maps.write_text(text)
    intro = source / "engine/menus/intro_menu.asm"
    intro.write_text(intro.read_text().replace("CrystalIntroSequence:\n", "CrystalIntroSequence:\n\tjmp AcceptanceBoot\n", 1)
                    + "\n" + (TESTS / "support/bootstrap.asm.in").read_text())
    menu = source / "engine/menus/main_menu.asm"
    menu.write_text(menu.read_text().replace("MainMenu:\n", "MainMenu:\n\tfarjp AcceptanceBoot\n", 1))
    objects = source / "engine/overworld/map_objects.asm"
    text = objects.read_text()
    needle = "\tldh [hUsedSpriteTile], a\nif DEF(MOBILE_ADAPTER)"
    if text.count(needle) != 1:
        raise RuntimeError("Declared native-facing observation point changed")
    objects.write_text(text.replace(needle, "\tldh [hUsedSpriteTile], a\n.AcceptanceFacing:\nif DEF(MOBILE_ADAPTER)"))
    spawn = source / "data/maps/spawn_points.asm"
    text = spawn.read_text()
    match = re.search(r"(?m)^\s*spawn\s+PLAYERS_HOUSE_2F,\s*\d+,\s*\d+", text)
    if not match:
        raise RuntimeError("Declared native spawn-table interface changed")
    coordinate_values = {"wXCoord": primary[0], "wYCoord": primary[1]}
    ordered_coordinates = [coordinate_values[name] for name in record["spawn_fields"] if name in coordinate_values]
    spawn.write_text(text[:match.start()] + f"\tspawn GOLDENROD_GAME_CORNER, {ordered_coordinates[0]}, {ordered_coordinates[1]}" + text[match.end():])
    for variant in ("crystal", "mobile"):
        run(["make", "-j4", variant, f"RGBDS={meta['rgbds']}/"], source, directory / f"{variant}.log")
    retain_rom_outputs(source)
    manifest = {"source": str(source), "scene": scene, "map": record, "primary": primary,
        "peers": peers, "ordinary_objects": count, "equal_actor_load": equal_load,
        "limitation": None if count <= 9 else "The native pool cannot represent fifteen ordinary objects. Full-pool control preserves all twelve ordinary objects; equal-actor controls run separately.",
        "native_object_positions": points[:count], "palettes": palette_data,
        "clock_input": "fixed game-time/software-RTC input; ordinary world/network remain live" if scene in ("save", "private-save") else "normal native game-time counter",
        "recipe_inputs": "Current native save produced once, then shared by both counterparts; no named daily/NPC field is pinned."}
    (directory / "manifest.json").write_text(json.dumps(manifest, indent=2))
    return manifest


class Proxy(socketserver.ThreadingTCPServer):
    daemon_threads = True
    allow_reuse_address = True
    def __init__(self, upstream, fault):
        self.upstream, self.fault = upstream, fault
        self.started = time.monotonic()
        self.injected = 0
        self.recovered = 0
        self.connection_lock = threading.Lock()
        self.single_reset = False
        super().__init__(("127.0.0.1", 0), ProxyHandler)


class ProxyHandler(socketserver.BaseRequestHandler):
    def handle(self):
        pending = bytearray()
        with self.server.connection_lock:
            opened_after_reset = self.server.single_reset
        try:
            with socket.create_connection(("127.0.0.1", self.server.upstream)) as upstream:
                for stream in (self.request, upstream):
                    stream.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
                while True:
                    elapsed = time.monotonic() - self.server.started
                    if self.server.fault == "reconnect" and 4 <= elapsed < 6:
                        self.server.injected += 1
                        return
                    if self.server.fault == "reconnect-one" and 4 <= elapsed < 6:
                        with self.server.connection_lock:
                            if not self.server.single_reset:
                                self.server.single_reset = True
                                self.server.injected += 1
                                return
                    readable, _, _ = select.select((self.request, upstream), (), (), .02)
                    for stream in readable:
                        data = stream.recv(4096)
                        if not data:
                            return
                        if stream is self.request:
                            upstream.sendall(data)
                            continue
                        pending.extend(data)
                        while len(pending) >= 8:
                            size = 8 + int.from_bytes(pending[6:8], "big")
                            if size > 128:
                                raise RuntimeError("Unexpected upstream framing")
                            if len(pending) < size:
                                break
                            frame = bytes(pending[:size])
                            del pending[:size]
                            elapsed = time.monotonic() - self.server.started
                            fault = self.server.fault
                            if elapsed >= 4 and fault != "none":
                                if fault not in ("reconnect", "reconnect-one"):
                                    self.server.injected += 1
                                if fault == "silence":
                                    continue
                                if fault == "partial":
                                    self.request.sendall(frame[:4])
                                    continue
                                if fault == "magic":
                                    frame = b"!" + frame[1:]
                                elif fault == "version":
                                    frame = frame[:2] + b"\xff" + frame[3:]
                                elif fault == "length":
                                    frame = frame[:6] + b"\xff\xff" + frame[8:]
                                elif fault == "kind":
                                    frame = frame[:3] + b"\xff" + frame[4:]
                                elif fault == "flood":
                                    frame = frame * 64
                                elif fault == "roster" and frame[3] in (WIRE_KINDS["ROSTER"], WIRE_KINDS["DELTA"]):
                                    frame = frame[:8] + b"\xff" * (len(frame) - 8)
                            self.request.sendall(frame)
                            valid_response = frame[3] in (WIRE_KINDS["WELCOME"], WIRE_KINDS["ROSTER"], WIRE_KINDS["DELTA"])
                            if valid_response and ((fault == "reconnect" and elapsed >= 6) or
                                                   (fault == "reconnect-one" and opened_after_reset)):
                                self.server.recovered += 1
        except OSError:
            return


def projected(directory):
    names = ("hCrashCode", "hInMenu", "wMapStatus", "wScriptRunning", "wInputFlags", "wXCoord", "wYCoord",
        "wMapGroup", "wMapNumber", "wTimeOfDay", "vAcceptanceReady", "vAcceptanceDialogDone", "vAcceptanceLoaded", "vAcceptanceLoadFailed", "vStoryFlagsHash",
        "wMobileOnline", "wMobileWanted", "wMobileOwner", "wMobilePeerCount", "wMobilePlayerId", "wMobileGeneration",
        "vNPCOccupiedMask", "vNPCPositionsHash", "vLocalPaletteHash", "vOAMMaxScanline",
        "vProfileWorldUpdates", "vAcceptanceNativeHash", "vAcceptancePartyHash", "vAcceptanceInventoryHash",
        "vAcceptanceOAMHash", "vAcceptanceLocalComplete", "vAcceptanceFrameAudio", "vAcceptanceSaveCount",
        "vAcceptanceLocalPaletteHash", "vAcceptanceLocalTileHash", "vAcceptancePaletteDemand", "vAcceptanceSaveActive", "vAcceptanceMenuActive", "vAcceptanceLinkEntered",
        "vAcceptanceSaveBytes", "vAcceptanceSaveMain", "vAcceptanceSaveBackup", "vAcceptanceTrainerID")
    frames = []
    with gzip.open(directory / "trace.jsonl.gz", "rt") as stream:
        for line in stream:
            row = json.loads(line)
            state = row["symbols"]
            values = {key: state[key] for key in names if key in state}
            for slot in range(13):
                for suffix in ("Count", "Complete", "Sprite", "Map", "PaletteHash"):
                    name = f"vNativeSource{slot}{suffix}"
                    if name in state:
                        values[name] = state[name]
            for peer in range(1, 4):
                for suffix in ("Id", "Generation", "Avatar", "Visible", "NativeStage", "OAMStart", "Status", "MapGroup", "MapNumber"):
                    key = f"wMobilePeer{peer}{suffix}"
                    if key in state:
                        values[key] = state[key]
                for suffix in ("PaletteHash", "TileHash", "CacheHash", "OAM0"):
                    key = f"vMobilePeer{peer}{suffix}"
                    if key in state:
                        values[key] = state[key]
                for suffix in ("Complete", "Partial", "TileHash"):
                    key = f"vAcceptancePeer{peer}{suffix}"
                    if key in state:
                        values[key] = state[key]
            frames.append({"frame": row["frame"], "state": values})
    return {"frames": frames,
        "profile": json.loads((directory / "profile.json").read_text()),
        "state": json.loads((directory / "state.json").read_text())["symbols"],
        "save": list((directory / "result.sav").read_bytes()),
        "save_sha256": sha(directory / "result.sav"),
        "events": [json.loads(line) for line in (directory / "input-events.jsonl").read_text().splitlines()]}


def seed_saves(meta, session, fx, hour):
    """One current native producer per avatar; never reuse historic battery data."""
    seed_fx = fixture(meta, session, "save")
    source = Path(seed_fx["source"])
    native = source / f"polishedcrystal-{meta['version']}.gbc"
    names = symbols(native.with_suffix(".sym"))
    def produce(index, position):
        directory = session / f"seed-{hour}-{index}"
        if not (directory / "result.sav").exists():
            directory.mkdir(exist_ok=True)
            values = {"wMapGroup": fx["map"]["group"], "wMapNumber": fx["map"]["number"],
                      "wXCoord": position[0], "wYCoord": position[1]}
            patch_data(native, names, {"AcceptanceAvatar": bytes([index]),
                "AcceptanceTrainerID": (10001 + index).to_bytes(2,"big"), "AcceptanceHour": bytes([hour]),
                "SpawnPoints": bytes(values[name] for name in fx["map"]["spawn_fields"]),
                "AcceptanceSeedNPC": bytes([native.read_bytes()[rom_offset(names["AcceptanceSeedNPC"])],position[1]+5,position[0]+4])}, directory / "fixture.gbc")
            (directory / "input.txt").write_text("1200 A 8\n1290 A 8\n")
            run([meta["runner"], meta["core"], directory / "fixture.gbc", directory / "screen.ppm", directory,
                "--frames", "1800", "--symbols", native.with_suffix(".sym"), "--input", directory / "input.txt",
                "--state-out", directory / "state.json", "--save-out", directory / "result.sav",
                "--trace-out", directory / "trace.jsonl.gz", "--input-events", directory / "input-events.jsonl",
                "--profile-out", directory / "profile.json", "--profile-window-start", "600",
                "--adapter", "0", "--acceptance", "1"], source, directory / "process.log")
        state = json.loads((directory / "state.json").read_text())["symbols"]
        profile = json.loads((directory / "profile.json").read_text())
        if state["vAcceptanceDialogDone"] != 1 or profile["save_copy_count"] < 4:
            raise RuntimeError("Native seed producer did not reach its post-initialization main/backup save")
        return directory / "result.sav"
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        return list(pool.map(produce, range(4), [fx["primary"], *fx["peers"]]))


def execute(meta, fx, out, seeds, action="idle", fault="none", speeds=(1, 1, 1, 1), duplicate=False, settle_frames=300):
    if out.exists():
        original = out
        attempt = 2
        while out.exists():
            out = original.with_name(original.name + f"-attempt-{attempt}")
            attempt += 1
    source = Path(fx["source"])
    version = meta["version"]
    mobile = source / f"polishedcrystal-mobile-{version}.gbc"
    native = source / f"polishedcrystal-{version}.gbc"
    mobile_symbols, native_symbols = symbols(mobile.with_suffix(".sym")), symbols(native.with_suffix(".sym"))
    out.mkdir(exist_ok=True)
    log = (out / "bridge.log").open("w")
    bridge = subprocess.Popen([meta["bridge"], "--bind", "127.0.0.1:0"], stdout=log, stderr=subprocess.STDOUT)
    deadline = time.monotonic() + 8
    try:
        while True:
            ready = re.search(r"BRIDGE_READY 127\.0\.0\.1:(\d+)", (out / "bridge.log").read_text())
            if ready:
                upstream_port = int(ready[1])
                break
            if bridge.poll() is not None or time.monotonic() >= deadline:
                raise RuntimeError("Bridge did not become ready")
            time.sleep(.02)
    except Exception:
        bridge.terminate()
        bridge.wait(timeout=5)
        log.close()
        raise
    proxy = Proxy(upstream_port, fault)
    port = proxy.server_address[1]
    threading.Thread(target=proxy.serve_forever, daemon=True).start()
    proxy.started = time.monotonic()

    def client(index, enabled):
        speed = speeds[index]
        # Native initialization is complete before frame 500. The explicit join
        # starts at 600; retain five real seconds for join/assets before measuring
        # the unchanged 800-frame window and 200-frame recovery tail.
        first, profile = 500, 500 + settle_frames * speed
        total = profile + 1000 * speed
        directory = out / ("mobile" if enabled else "single") / str(index)
        directory.mkdir(parents=True)
        rom = directory / "fixture.gbc"
        current, bindings = (mobile, mobile_symbols) if enabled else (native, native_symbols)
        position = fx["primary"] if index == 0 else fx["peers"][index - 1]
        changes = {"AcceptanceAvatar": bytes([index]), "AcceptanceScenarioLoad": bytes([1]), "AcceptanceTrainerID": (12345 if duplicate else 10001 + index).to_bytes(2, "big"),
                   "AcceptanceHour": bytes([22 if action == "night" else 12]),
                   "AcceptanceInitialFault": bytes([0]),
                   "AcceptanceInteractionFault": bytes([int(action.split(":")[1]) if action.startswith("private:") and enabled and index == 0 else 0])}
        if enabled:
            bank, addr = bindings["MobileEndpoint"]
            # Endpoint is declarative ROM data, not an opcode or RAM mutation.
            changes["MobileEndpoint"] = current.read_bytes()[rom_offset((bank, addr)):rom_offset((bank, addr)) + 4] + port.to_bytes(2, "big")
        # SPAWN_HOME's entry is current-build data; only map position differs per player.
        start = rom_offset(bindings["SpawnPoints"])
        values = {"wMapGroup": fx["map"]["group"], "wMapNumber": fx["map"]["number"], "wXCoord": position[0], "wYCoord": position[1]}
        changes["SpawnPoints"] = bytes(values[name] for name in fx["map"]["spawn_fields"])
        if not enabled and fx["equal_actor_load"]:
            figure_at = rom_offset(bindings["AcceptanceFigures"])
            figures = current.read_bytes()[figure_at:figure_at + 4]
            positions = [fx["primary"], *fx["peers"]]
            for slot, avatar in enumerate(i for i in range(4) if i != index):
                px, py = positions[avatar]
                changes[f"AcceptanceControlPeer{slot}"] = bytes([figures[avatar], py + 4, px + 4])
        patch_data(current, bindings, changes, rom)
        text = "# Native initialization enters the fixture without title/dialog input.\n"
        if enabled and not (index == 0 and action.startswith("private:")):
            for button in ("SELECT", "B"):
                text += f"ON 600 vAcceptanceReady=1,wMapStatus=2,wScriptRunning=0 {button} 8\n"
        at = profile + 150 * speed
        if index == 0 and (action in ("interaction", "save", "script", "link", "warp", "fault-save-exit", "npc-movement") or action.startswith("private:")):
            text += f"{at} A 8\n{at + 90} A 8\n"
        elif index == 0 and action == "menu":
            text += f"{at} START 8\n"
        if index > 0 and action == "clear-conflict":
            text += f"{at} UP 12\n"
        if index > 0 and speeds != (1, 1, 1, 1):
            for cycle in range(8):
                step = profile + 30 + cycle * 72
                text += f"{step} UP 12\n{step + 36} DOWN 12\n"
        if index == 0 and (action.startswith("exit") or action == "fault-save-exit" or action.startswith("private:")):
            if action == "fault-save-exit" or action.startswith("private:"):
                at += 350
            if action == "exit-menu":
                text += f"{at - 80} START 8\n"
            elif action == "exit-script":
                text += f"{at - 100} A 8\n{at - 20} A 8\n"
            elif action == "exit-save":
                text += f"{at - 100} A 8\n{at - 1} A 8\n"
            if action.startswith("private:"):
                value = action.split(":")[1]
                if enabled:
                    text += f"ON 0 vAcceptanceReady=1,vAcceptanceSaveActive=1,wMobileWanted={value} SELECT 8\nON 0 vAcceptanceReady=1,vAcceptanceSaveActive=1,wMobileWanted={value} B 8\n"
                else:
                    # Use the actual mobile event time without referencing any
                    # feature symbol in the native counterpart.
                    events = [json.loads(line) for line in (out / "mobile/0/input-events.jsonl").read_text().splitlines()]
                    selected = [event for event in events if event["button"] == 2]
                    if not selected:
                        raise RuntimeError("Private fault was not observable during native save")
                    when = selected[-1]["frame"]
                    text += f"{when} SELECT 8\n{when} B 8\n"
            elif action == "exit-save":
                text += f"ON {at} vAcceptanceSaveActive=1 SELECT 8\nON {at} vAcceptanceSaveActive=1 B 8\n"
            else:
                text += f"{at} SELECT 8\n{at} B 8\n"
            text += f"{at + 180} B 8\n{at + 250} LEFT 12\n"
        script = directory / "input.txt"
        script.write_text(text)
        command = [meta["runner"], meta["core"], rom, directory / "screen.ppm", directory,
            "--frames", total, "--symbols", current.with_suffix(".sym"), "--input", script,
            "--state-out", directory / "state.json", "--save-out", directory / "result.sav",
            "--save-in", seeds[index],
            "--trace-out", directory / "trace.jsonl.gz", "--input-events", directory / "input-events.jsonl",
            "--profile-out", directory / "profile.json", "--profile-window-start", profile,
            "--pace-from", first if enabled else total, "--pace-multiplier", speed, "--adapter", int(enabled), "--acceptance", "1"]
        if enabled and index == 0 and action == "resume":
            command += ["--test-snapshot-out", directory / "online.state"]
        if enabled:
            command += ["--finish-barrier", out / "capture"]
        run(command, source, directory / "process.log", timeout=100)
        return projected(directory)

    try:
        capture_barrier = out / "capture.exit"
        capture_barrier.touch()
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            futures = [pool.submit(client,index,True) for index in range(4)]
            deadline = time.monotonic() + 90
            try:
                while True:
                    if any(future.done() for future in futures):
                        for future in futures:
                            if future.done(): future.result()
                    captured = 0
                    for index in range(4):
                        try:
                            json.loads((out / f"mobile/{index}/state.json").read_text())
                            captured += 1
                        except (FileNotFoundError,json.JSONDecodeError):
                            pass
                    if captured == 4: break
                    if time.monotonic() >= deadline: raise TimeoutError("Four-core final capture did not complete")
                    time.sleep(.02)
            finally:
                capture_barrier.unlink(missing_ok=True)
            online = [future.result() for future in futures]
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            offline = list(pool.map(lambda index: client(index, False), range(4)))
        save_reload = []
        if action == "save":
            for producer, consumer in (("mobile", "single"), ("single", "mobile")):
                directory = out / f"reload-{producer}-into-{consumer}"
                directory.mkdir()
                target = native if consumer == "single" else mobile
                (directory / "input.txt").write_text("# The normal save loader enters the generated scene.\n")
                run([meta["runner"], meta["core"], target, directory / "screen.ppm", directory,
                    "--frames", "1400", "--symbols", target.with_suffix(".sym"),
                    "--save-in", out / producer / "0/result.sav", "--save-out", directory / "result.sav",
                    "--input", directory / "input.txt", "--state-out", directory / "state.json",
                    "--trace-out", directory / "trace.jsonl.gz", "--input-events", directory / "input-events.jsonl",
                    "--profile-out", directory / "profile.json", "--profile-window-start", "400",
                    "--adapter", "0", "--acceptance", "1"], source, directory / "process.log")
                save_reload.append({"producer": producer, "consumer": consumer, "result": projected(directory)})
        recovery = None
        if action == "resume":
            recovery_dir = out / "restore-without-adapter"
            recovery_dir.mkdir()
            (recovery_dir / "input.txt").write_text("40 SELECT 8\n40 B 8\n120 LEFT 12\n")
            run([meta["runner"], os.environ.get("MOBILE_ACCEPTANCE_RECOVERY_CORE", meta["core"]),
                out / "mobile/0/fixture.gbc", recovery_dir / "screen.ppm", recovery_dir,
                "--frames", "1200", "--symbols", mobile.with_suffix(".sym"),
                "--snapshot-in", out / "mobile/0/online.state", "--input", recovery_dir / "input.txt",
                "--state-out", recovery_dir / "state.json", "--save-out", recovery_dir / "result.sav",
                "--input-events", recovery_dir / "input-events.jsonl", "--trace-out", recovery_dir / "trace.jsonl.gz",
                "--profile-out", recovery_dir / "profile.json", "--profile-window-start", "200",
                "--adapter", "0", "--acceptance", "1"], source, recovery_dir / "process.log")
            recovery = projected(recovery_dir)
        return {"fixture": fx, "clients": [{"mobile": m, "single": s} for m, s in zip(online, offline)],
            "fault": fault, "injected": proxy.injected, "recovered": proxy.recovered, "speeds": speeds, "action": action,
            "recovery": recovery,
            "save_reload": save_reload,
            "initial_save_sha256": [sha(seed) for seed in seeds],
            "artifact_directory": str(out),
            "assumptions": ASSUMPTIONS,
            "source_rom_sha256": meta["mobile_sha256"], "core_sha256": meta["core_sha256"]}
    finally:
        bridge.terminate()
        try:
            bridge.wait(timeout=5)
        except subprocess.TimeoutExpired:
            bridge.kill()
            bridge.wait()
        log.close()
        proxy.shutdown()
        proxy.server_close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--case", required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    started = time.monotonic()
    ensure_free_space(CACHE, args.output)
    with preparation_lock(args.output):
        validate_session(args.output)
        if (args.output / (args.case + ".json")).exists():
            return
        meta = prepare(args.output)
    if args.case == "build":
        source = Path(meta["source"])
        import sys
        sys.path.insert(0, str(source / "mobile_adapter/tools"))
        from audit_independent_maps import audit
        from audit_rom_budget import measure
        base = source / f"polishedcrystal-{meta['version']}.gbc"
        mobile = source / f"polishedcrystal-mobile-{meta['version']}.gbc"
        data = {"manifest": meta, "maps": audit(base, mobile, source), "budget": measure(base, mobile),
                "symbols": sorted(symbols(mobile.with_suffix(".sym")))}
        names = symbols(mobile.with_suffix(".sym"))
        if "MobileCompatibilityHash" in names:
            start = rom_offset(names["MobileCompatibilityHash"])
            data["compatibility_hash"] = mobile.read_bytes()[start:start + 32].hex()
    else:
        fault = args.case.removeprefix("fault-") if args.case.startswith("fault-") else "none"
        scene = args.case if args.case in ("scanline", "pool", "palettes", "interaction", "script", "link", "save", "warp", "npc-movement", "night", "dark") else "empty"
        action = args.case if args.case in ("interaction", "script", "link", "save", "warp", "menu", "exit", "exit-menu", "exit-script", "exit-save", "night", "dark", "resume", "clear-conflict", "npc-movement") else "idle"
        if args.case.startswith("fault-") and fault not in ("reconnect", "reconnect-one"):
            scene, action = "save", "fault-save-exit"
        if action == "clear-conflict":
            scene = "scanline"
        if action == "exit-script":
            scene = "script"
        if action == "exit-save":
            scene = "save"
        if args.case.startswith("private-"):
            scene, action = "private-save", "private:" + args.case.split("-")[1]
        speeds = (1, 2, 3, 4) if args.case == "speeds" else (1, 1, 1, 1)
        with preparation_lock(args.output):
            fx = fixture(meta, args.output, scene)
            seeds = seed_saves(meta, args.output, fx, 22 if action == "night" else 12)
        ensure_free_space(CACHE, args.output)
        prepared_at = time.monotonic()
        execution_wall_start = time.time()
        data = execute(meta, fx, args.output / args.case, seeds,
                       action, fault, speeds, args.case == "duplicate-ids",
                       settle_frames=1300 if args.case in ASSUMPTIONS["performance_cases"] else 300)
        data["execution_wall_start"] = execution_wall_start
        data["execution_wall_end"] = time.time()
        data["execution_seconds"] = time.monotonic() - prepared_at
        data["preparation_seconds"] = prepared_at - started
    destination = args.output / (args.case + ".json")
    temporary = destination.with_suffix(".json.partial")
    temporary.write_text(json.dumps(data))
    temporary.replace(destination)
    ensure_free_space(CACHE, args.output)


if __name__ == "__main__":
    try:
        import sys
        output = Path(sys.argv[sys.argv.index("--output") + 1])
        with active_run(output):
            main()
    except Exception as error:
        import sys
        if "--output" in sys.argv and "--case" in sys.argv:
            out = Path(sys.argv[sys.argv.index("--output") + 1])
            out.mkdir(parents=True, exist_ok=True)
            (out / (sys.argv[sys.argv.index("--case") + 1] + ".error")).write_text(str(error))
        raise
