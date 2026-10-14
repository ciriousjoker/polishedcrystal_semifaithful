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


class OrdinaryProcesses:
    """Own only this capture's runners/readers and bounded FIFO status."""
    def __init__(self, reader, count):
        self.reader, self.count = reader, count
        self.stop = threading.Event()
        self.lock = threading.Lock()
        self.children = []
        self.runners = []

    def spawn(self, command, runner=False, **kwargs):
        with self.lock:
            if self.stop.is_set():
                raise RuntimeError("Ordinary capture cancelled")
            child = subprocess.Popen(list(map(str, command)), **kwargs)
            self.children.append(child)
            if runner:
                self.runners.append(child)
            return child

    def cancel(self):
        self.stop.set()
        with self.lock:
            runners = list(self.runners)
        for child in runners:
            if child.poll() is None:
                child.terminate()

    def close(self):
        self.cancel()
        with self.lock:
            children = list(self.children)
        # run() closes its keeper and drains the reader after the writer exits.
        for child in children:
            try:
                child.wait(timeout=3)
            except subprocess.TimeoutExpired:
                child.kill()
                child.wait()

    def run(self, command, cwd, log, directory, timeout=100):
        fifo = directory / "ordinary.fifo"
        read_fd = keeper = -1
        reader = runner = drainer = None
        progress = {"records": 0, "last": "", "drained_eof": False}
        failure = None
        success = False
        cancelled = False
        def drain(pipe):
            with pipe:
                for line in pipe:
                    line = line.strip()
                    # Count only complete, actually drained validation records.
                    if re.fullmatch(r"\d+ \d+ \d+ [0-9a-f]{8} pixels=23040 physical=PASS intent=UNPROVEN", line):
                        progress["records"] += 1
                        progress["last"] = line
            progress["drained_eof"] = True
        try:
            os.mkfifo(fifo)
            read_fd = os.open(fifo, os.O_RDONLY | os.O_NONBLOCK)
            keeper = os.open(fifo, os.O_WRONLY | os.O_NONBLOCK)
            os.set_blocking(read_fd, True)
            with (directory / "ordinary-reader-errors.log").open("w") as errors, Path(log).open("w") as output:
                reader = self.spawn([self.reader, self.count], stdin=read_fd, stdout=subprocess.PIPE,
                                    stderr=errors, text=True)
                drainer = threading.Thread(target=drain, args=(reader.stdout,))
                drainer.start()
                os.close(read_fd)
                read_fd = -1
                runner = self.spawn(command, runner=True, cwd=cwd, stdout=output, stderr=subprocess.STDOUT)
                deadline = time.monotonic() + timeout
                while runner.poll() is None:
                    if keeper >= 0 and (directory / "state.json.ordinary-complete").exists():
                        os.close(keeper)
                        keeper = -1
                    if reader.poll() not in (None, 0):
                        raise RuntimeError("Ordinary physical reader rejected stream: " + str(directory))
                    if self.stop.is_set() or time.monotonic() >= deadline:
                        raise TimeoutError("Ordinary runner cancelled or timed out")
                    time.sleep(.01)
                if keeper >= 0:
                    os.close(keeper)
                    keeper = -1
                if runner.returncode:
                    raise subprocess.CalledProcessError(runner.returncode, command)
                reader.wait(timeout=5)
                drainer.join(timeout=2)
                if reader.returncode or not progress["drained_eof"] or progress["records"] != self.count:
                    raise RuntimeError("Ordinary reader lacks exact records/footer/EOF")
                success = True
        except BaseException as error:
            failure = error
            cancelled = self.stop.is_set()
            self.cancel()
            raise
        finally:
            # Stop writers first, then release the keeper, allowing the reader to
            # consume all buffered PASS records and report truncated footer/EOF.
            if runner is not None and runner.poll() is None:
                runner.terminate()
                try:
                    runner.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    runner.kill()
                    runner.wait()
            if read_fd >= 0:
                os.close(read_fd)
            if keeper >= 0:
                os.close(keeper)
            if reader is not None:
                try:
                    reader.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    reader.kill()
                    reader.wait()
            if drainer is not None:
                drainer.join()
            summary = {**progress, "footer_eof": bool(success),
                "physical": "PASS" if success else "INCOMPLETE", "intent": "UNPROVEN",
                "status": "complete" if success else "cancelled" if cancelled else "failed",
                "failure": None if failure is None else type(failure).__name__ + ": " + str(failure),
                "cancelled": cancelled, "cancellation_requested": self.stop.is_set(),
                "reader_returncode": None if reader is None else reader.returncode,
                "runner_returncode": None if runner is None else runner.returncode}
            (directory / "ordinary-reader.json").write_text(json.dumps(summary))
            fifo.unlink(missing_ok=True)


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def observer_fingerprint():
    paths = [ROOT / "mobile_adapter/tools/smoke_libretro.cpp", ROOT / "mobile_adapter/tools/profile_core.h",
             ROOT / "mobile_adapter/tools/conditional_input.hpp",
             TESTS / "support/profile_native.hpp", TESTS / "support/static_ordinary_intent.hpp",
             TESTS / "support/observer.hpp", Path(__file__)]
    return hashlib.sha256("".join(sha(path) for path in paths).encode()).hexdigest()


def inject_unique(text, anchor, replacement, shared_hook=False):
    """Accept exactly one original or already hooked anchor, never partial hooks."""
    if text.count(anchor) != 1:
        raise RuntimeError("Expected unique observer source anchor: " + anchor)
    hook = replacement[len(anchor):] if replacement.startswith(anchor) else None
    if replacement in text:
        if hook and not shared_hook and text.count(hook.strip()) != 1:
            raise RuntimeError("Duplicate observer hook: " + hook.strip())
        return text
    if hook and not shared_hook and hook.strip() in text:
        raise RuntimeError("Misplaced or duplicate observer hook: " + hook.strip())
    return text.replace(anchor, replacement)


def extend_profile(profile_source):
    header = profile_source / "gb_core/profile_core.h"
    text = (ROOT / "mobile_adapter/tools/profile_core.h").read_text()
    text = inject_unique(text, "static void mobile_profile_before_instruction(gb* game, const cpu_regs& regs, int clock) {",
        '#include "acceptance_profile.hpp"\nstatic void mobile_profile_before_instruction(gb* game, const cpu_regs& regs, int clock) {\n    acceptance_profile_before(game, regs);')
    text = inject_unique(text,
        "static void mobile_profile_after_instruction(gb* game, const cpu_regs& regs, int clock, unsigned opcode, unsigned cycles) {",
        "static void mobile_profile_after_instruction(gb* game, const cpu_regs& regs, int clock, unsigned opcode, unsigned cycles) {\n    acceptance_profile_after(game, regs, opcode, cycles);")
    text = text.replace("&span == &mobile_render_spans[10] && mobile_native_descriptor_address",
                        "&span == &mobile_render_spans[10] && mobile_native_shadow_address")
    text = text.replace("for (unsigned i = 0; i < 104; ++i)",
                        "for (unsigned i = 0; mobile_native_descriptor_address && i < 104; ++i)")
    text = text.replace("i * 34", "i * acceptance_native_stride")
    text = text.replace("ram + 0x1000 + mobile_native_object_address", "ram + acceptance_native_object_bank * 0x1000 + mobile_native_object_address")
    text = text.replace("actor[0]", "actor[acceptance_native_sprite_field]").replace("actor[1]", "actor[acceptance_native_map_field]")
    cpu = profile_source / "gb_core/cpu.cpp"
    cpu_text = cpu.read_text()
    for hook in ("mobile_profile_before_instruction(ref_gb, regs, total_clock);",
                 "mobile_profile_after_instruction(ref_gb, regs, total_clock, observed_opcode, tmp_clocks);"):
        if cpu_text.count(hook) != 1:
            raise RuntimeError("Expected unique CPU instruction hook: " + hook)
    cpu_text = inject_unique(cpu_text, "void cpu::io_write(word adr,byte dat)\n{",
        "void cpu::io_write(word adr,byte dat)\n{\n    acceptance_profile_effect_io_write(ref_gb, adr, dat);")
    libretro = profile_source / "libretro/libretro.cpp"
    libretro_text = libretro.read_text()
    declarations = ('extern "C" void acceptance_profile_effect_reset();',
                    'extern "C" void acceptance_profile_restore_boundary();')
    for declaration in declarations:
        if libretro_text.count(declaration) > 1:
            raise RuntimeError("Duplicate observer declaration: " + declaration)
        if declaration not in libretro_text:
            libretro_text = declaration + "\n" + libretro_text
    for function in ("bool retro_load_game(const struct retro_game_info *info)", "void retro_reset(void)"):
        libretro_text = inject_unique(libretro_text, function + "\n{",
            function + "\n{\n    acceptance_profile_effect_reset();", shared_hook=True)
    if libretro_text.count("acceptance_profile_effect_reset();") != 3:
        raise RuntimeError("Expected exactly two reset hooks and one declaration")
    start = libretro_text.find("bool retro_unserialize(const void *data, size_t size)\n{")
    if start < 0 or libretro_text.count("bool retro_unserialize(const void *data, size_t size)\n{") != 1:
        raise RuntimeError("Expected unique retro_unserialize anchor")
    # This supported source ends the restore body with its sole successful return.
    end = libretro_text.find("\n}", start)
    if end < 0:
        raise RuntimeError("Missing retro_unserialize closing anchor")
    body = libretro_text[start:end]
    body = inject_unique(body, "        return true;",
        "        acceptance_profile_restore_boundary();\n        return true;")
    libretro_text = libretro_text[:start] + body + libretro_text[end:]
    lcd_source = profile_source / "gb_core/lcd.cpp"
    lcd_text = lcd_source.read_text(encoding="utf-8-sig")
    if ('acceptance_profile_render_line(gb*, unsigned);' in lcd_text or
            'acceptance_profile_render_line(ref_gb, scanline);' in lcd_text):
        raise RuntimeError("Incompatible prior LCD observer hook; rebuild from original pinned core source")
    if ('acceptance_profile_render_line(ref_gb, scanline, now_win_line);' in lcd_text and
            'acceptance_profile_render_buffer(buf, scanline);' not in lcd_text):
        raise RuntimeError('Incompatible prior buffer observer hook; rebuild from original pinned core source')
    buffer_declaration = 'extern "C" void acceptance_profile_render_buffer(const void*, unsigned);'
    if buffer_declaration not in lcd_text:
        lcd_text = buffer_declaration + '\n' + lcd_text
    declaration = 'extern "C" void acceptance_profile_render_line(gb*, unsigned, int);'
    if lcd_text.count(declaration) > 1:
        raise RuntimeError("Duplicate LCD observer declaration")
    if declaration not in lcd_text:
        include = '#include "gb.h"'
        if lcd_text.count(include) != 1:
            raise RuntimeError("Expected unique LCD include anchor")
        lcd_text = lcd_text.replace(include, include + "\n" + declaration)
    lcd_text = inject_unique(lcd_text, "void lcd::render(void *buf,int scanline)\n{",
        "void lcd::render(void *buf,int scanline)\n{\n    acceptance_profile_render_buffer(buf, scanline);\n    acceptance_profile_render_line(ref_gb, scanline, now_win_line);")
    # Both native LCD-off paths clear the shared output without lcd::render.
    # Reject before the write, including a completed but unconsumed raster.
    gb_source = profile_source / "gb_core/gb.cpp"
    gb_bytes = gb_source.read_bytes()
    gb_newline = "\r\n" if b"\r\n" in gb_bytes else "\n"
    gb_text = gb_bytes.decode("utf-8").replace("\r\n", "\n")
    unsupported_declaration = 'extern "C" void acceptance_profile_ordinary_unsupported_output();'
    if gb_text.count(unsupported_declaration) > 1:
        raise RuntimeError("Duplicate unsupported output observer declaration")
    if unsupported_declaration not in gb_text:
        declaration_offset = int(gb_text.startswith("\ufeff"))
        gb_text = gb_text[:declaration_offset] + unsupported_declaration + "\n" + gb_text[declaration_offset:]
    for anchor in ("            memset(vframe, 0xff, 160 * 144 * 2);",
                   "\t\t\t\tmemset(vframe,0xff,160*144*2);"):
        indent = anchor[:len(anchor) - len(anchor.lstrip())]
        gb_text = inject_unique(gb_text, anchor,
            indent + "acceptance_profile_ordinary_unsupported_output();\n" + anchor)
    if gb_text.count("acceptance_profile_ordinary_unsupported_output();") != 3:
        raise RuntimeError("Expected both native LCD-off clear hooks and one declaration")
    # Observe actual renderer globals at the conversion and output calls. All
    # anchors are checked before any source is written.
    renderer = profile_source / "libretro/dmy_renderer.cpp"
    renderer_text = renderer.read_text()
    # Keep an initial UTF-8 BOM at byte zero when adding declarations.
    declaration_offset = int(renderer_text.startswith("\ufeff"))
    declarations = ('extern "C" void acceptance_profile_palette_config(unsigned, unsigned, float);',
                    'extern "C" void acceptance_profile_display_begin(const void*, unsigned, unsigned, unsigned, unsigned, unsigned, unsigned, float, unsigned, unsigned, unsigned, unsigned, unsigned);')
    for declaration in declarations:
        if renderer_text.count(declaration) > 1:
            raise RuntimeError("Duplicate display observer declaration")
        if declaration not in renderer_text:
            renderer_text = (renderer_text[:declaration_offset] + declaration + "\n" +
                             renderer_text[declaration_offset:])
    renderer_text = inject_unique(renderer_text, "word dmy_renderer::map_color(word gb_col)\n{",
        "word dmy_renderer::map_color(word gb_col)\n{\n    acceptance_observe_palette_config(); // Observe actual palette mapping call.")
    renderer_text = inject_unique(renderer_text,
        "void dmy_renderer::apply_all_gb_effects_and_render(byte* buf, int width, int height, int depth, int pitch)\n{",
        "void dmy_renderer::apply_all_gb_effects_and_render(byte* buf, int width, int height, int depth, int pitch)\n{\n    acceptance_profile_display_begin(buf, width, height, depth, pitch, (is_gbc_rom || useGbcLCDforDmG) && gbc_lcd_interlacing_enabled, gbc_lcd_interfacing_fast, gbc_lcd_interlacing_brightness, is_odd_frame, is_gbc_rom, useGbcLCDforDmG, gbc_rgbSubpixel_upscale_factor, gbc_lcd_blur_effect_enabled);")
    renderer_header = profile_source / "libretro/dmy_renderer.h"
    renderer_header_text = renderer_header.read_text()
    renderer_header_text = inject_unique(renderer_header_text,
        "virtual word map_color(word gb_col);",
        "virtual word map_color(word gb_col);\n    static void acceptance_observe_palette_config();")
    getter = ('void dmy_renderer::acceptance_observe_palette_config() {\n'
              '    acceptance_profile_palette_config(unsigned(is_gbc_rom) | unsigned(gbc_color_correction_enabled) << 1 | unsigned(detect_as_gba) << 2 | unsigned(rgb565) << 3, unsigned(gbc_cc_mode), light_temperature);\n'
              '}\nextern "C" void acceptance_profile_palette_current() {\n'
              '    dmy_renderer::acceptance_observe_palette_config();\n}\n')
    # Put the getter immediately before map_color. Its complete placement must
    # survive reruns; an orphaned, moved or duplicate getter fails closed.
    anchor = "word dmy_renderer::map_color(word gb_col)\n{"
    if getter in renderer_text:
        if renderer_text.count(getter) != 1 or getter + anchor not in renderer_text:
            raise RuntimeError("Duplicate or misplaced actual palette getter")
    else:
        if "acceptance_profile_palette_current" in renderer_text or "void dmy_renderer::acceptance_observe_palette_config" in renderer_text:
            raise RuntimeError("Partial actual palette getter")
        if renderer_text.count(anchor) != 1:
            raise RuntimeError("Missing actual palette getter anchor")
        renderer_text = renderer_text.replace(anchor, getter + anchor)
    renderer_header.write_text(renderer_header_text)
    renderer.write_text(renderer_text)
    gb_source.write_bytes(gb_text.replace("\n", gb_newline).encode("utf-8"))
    lcd_source.write_text(lcd_text)
    header.write_text(text)
    cpu.write_text(cpu_text)
    libretro.write_text(libretro_text)
    shutil.copyfile(TESTS / "support/profile_native.hpp", profile_source / "gb_core/acceptance_profile.hpp")
    shutil.copyfile(TESTS / "support/static_ordinary_intent.hpp", profile_source / "gb_core/static_ordinary_intent.hpp")


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


def validate_native_effect_sources(rom, names):
    """Bind the unchanged linked native teardown writer and its return site."""
    required = ["ShakeHeadbuttTree", "ShakeHeadbuttTree.got_gfx", "ShakeHeadbuttTree.copy_done_loop", "ShakeHeadbuttTree.hide_loop",
                "ClearSpriteAnims", "DelayFrame", "wSpriteAnimDict", "wSpriteAnimsEnd"]
    if any(name not in names for name in required):
        raise RuntimeError("Missing native effect source anchor")
    data = Path(rom).read_bytes()
    def exact(name, expected):
        at = rom_offset(names[name])
        if data[at:at + len(expected)] != expected:
            raise RuntimeError("Changed linked native effect source: " + name)
    exact("ShakeHeadbuttTree.copy_done_loop", bytes.fromhex("3a121b0d20fa"))
    clear = names["ClearSpriteAnims"][1]
    delay = names["DelayFrame"][1]
    exact("ShakeHeadbuttTree.hide_loop", bytes.fromhex("77190d20fb") +
          bytes([0xcd]) + clear.to_bytes(2, "little") + bytes([0xcd]) + delay.to_bytes(2, "little"))
    start = names["wSpriteAnimDict"][1]
    length = names["wSpriteAnimsEnd"][1] - start
    exact("ClearSpriteAnims", bytes([0xaf, 0x21]) + start.to_bytes(2, "little") +
          bytes([0x01]) + length.to_bytes(2, "little") + bytes([0xef, 0xc9]))
    start_at = rom_offset(names["ShakeHeadbuttTree"])
    end_at = rom_offset(names["ShakeHeadbuttTree.got_gfx"])
    init_call = bytes([0xcd]) + clear.to_bytes(2, "little")
    if data[start_at:end_at].count(init_call) != 1:
        raise RuntimeError("Missing or ambiguous native initialization source caller")
    bank, pc = names["ShakeHeadbuttTree.hide_loop"]
    return bank, pc + 8


def patch_data(rom, names, values, destination):
    data = bytearray(rom.read_bytes())
    sizes = {"AcceptanceScenarioID": 1, "AcceptanceAvatar": 1, "AcceptanceTrainerID": 2,
             "AcceptanceHour": 1, "AcceptanceInitialFault": 1, "AcceptanceScenarioLoad": 1,
             "AcceptanceInteractionFault": 1, "AcceptanceLighting": 1, "AcceptanceNPCOverride": 1,
             "AcceptanceNPCPosition": 2, "AcceptanceSpawn": 4, "AcceptanceControlPeers": 9,
             "MobileEndpoint": 6, "AcceptanceSeedProfile": 1}
    first, last = rom_offset(names["AcceptanceConfig"]), rom_offset(names["AcceptanceConfigEnd"])
    for name, value in values.items():
        if name not in sizes or len(value) != sizes[name]:
            raise ValueError("Only declared test configuration fields may be patched: " + name)
        start = rom_offset(names[name])
        if start + len(value) > len(data) or (name != "MobileEndpoint" and not first <= start < start + len(value) <= last):
            raise ValueError("Test configuration field exceeds its declared ROM span: " + name)
        data[start:start + len(value)] = value
    data[0x14e:0x150] = ((sum(data[:0x14e]) + sum(data[0x150:])) & 65535).to_bytes(2, "big")
    destination.write_bytes(data)


def patch_identity_fixture(rom, names, mismatch, destination):
    """Negative declarations only, in a disposable optional-test cartridge."""
    if "AcceptanceConfig" not in names or "AcceptanceConfigEnd" not in names:
        raise ValueError("Identity negatives require an optional test cartridge")
    data = bytearray(rom.read_bytes())
    first = rom_offset(names["MobileCompatibilityHash"])
    last = rom_offset(names["MobileCompatibilityHashEnd"])
    prefix = rom_offset(names["MobileHelloPrefix"])
    vector = rom_offset(names["MobileHelloVector"])
    if last - first != 32 or vector - prefix != 5 or not (0 <= first < last <= len(data)) or not (0 <= prefix < vector <= len(data)):
        raise ValueError("Identity declaration data spans changed")
    if mismatch == "digest":
        data[first] ^= 1
    elif mismatch == "schema":
        data[prefix] ^= 0xff
    else:
        raise ValueError("Unknown negative identity declaration")
    data[0x14e:0x150] = ((sum(data[:0x14e]) + sum(data[0x150:])) & 65535).to_bytes(2, "big")
    destination.write_bytes(data)


def prepare_pristine_copyright(pristine, logs):
    """Generate the archived recipe's asset before parallel Make expansion."""
    # GNU Make 3.81 can expand the old $(shell cat $<) recipe without its
    # automatic prerequisite value. Keep the reference source unchanged and
    # invoke the same fine_print tool directly after building its dependencies.
    run(["make", "-C", "tools", "-j1", "fine_print", "lzpcompress"], pristine,
        logs / "pristine-copyright-tool.log")
    source = pristine / "gfx/splash/copyright.txt"
    text = " ".join(source.read_text().rstrip("\n").split("\n"))
    if not text.strip():
        raise RuntimeError("Pristine copyright text is empty")
    asset = pristine / "gfx/splash/copyright.2bpp"
    run(["tools/fine_print", "-c", "013", "-s", "0", text,
         "gfx/splash/copyright.2bpp"], pristine, logs / "pristine-copyright-asset.log")
    compressed = pristine / "gfx/splash/copyright.2bpp.lzp"
    run(["tools/lzpcompress", "--", "gfx/splash/copyright.2bpp",
         "gfx/splash/copyright.2bpp.lzp"], pristine, logs / "pristine-copyright-compress.log")
    if asset.stat().st_size != 480 or compressed.stat().st_size != 288:
        raise RuntimeError("Pristine copyright artwork must contain 480 raw and 288 compressed bytes")
    evidence = {"text": text, "text_sha256": sha(source),
                "asset_bytes": asset.stat().st_size, "asset_sha256": sha(asset),
                "compressed_bytes": compressed.stat().st_size, "compressed_sha256": sha(compressed),
                "makefile_sha256": sha(pristine / "Makefile"),
                "tool_sha256": sha(pristine / "tools/fine_print"),
                "generator": "tools/fine_print -c 013 -s 0 <copyright.txt> gfx/splash/copyright.2bpp"}
    (logs / "pristine-copyright.json").write_text(json.dumps(evidence, indent=2) + "\n")
    return evidence


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
    pristine_copyright = prepare_pristine_copyright(pristine, prepared)
    run(toolchain + ["crystal"], pristine, prepared / "pristine.log")
    if (sha(pristine / "gfx/splash/copyright.2bpp") != pristine_copyright["asset_sha256"] or
            sha(pristine / "gfx/splash/copyright.2bpp.lzp") != pristine_copyright["compressed_sha256"]):
        raise RuntimeError("Parallel pristine build changed the prepared copyright artwork")
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
        "pristine_copyright": pristine_copyright,
        "version": version, "rgbds": str(rgbds), "core": str(core), "runner": str(runner),
        "bridge": str(prepared / "mobile-bridge"),
        "baseline_sha256": sha(source / f"polishedcrystal-{version}.gbc"),
        "pristine_sha256": sha(pristine / f"polishedcrystal-{version}.gbc"),
        "mobile_sha256": sha(source / f"polishedcrystal-mobile-{version}.gbc"),
        "core_sha256": sha(core), "assumptions": ASSUMPTIONS, "observer_fingerprint": observer_fingerprint(),
        "source_fingerprint": (out / "source-fingerprint.txt").read_text().strip()}
    adapter_bindings = {}
    for relative in ("mobile_adapter/tools/smoke_libretro.cpp", "mobile_adapter/tests/support/observer.hpp",
                     "mobile_adapter/rom/test_bootstrap.asm"):
        text = (ROOT / relative).read_text()
        names = set(re.findall(r'"([whs][A-Z]\w*)"', text))
        names.update(re.findall(r'(?:call|farcall|jmp|farjp)\s+([A-Z]\w*)', text))
        names.update(re.findall(r'native_addresses\.at\("([^"]+)"\)', text))
        adapter_bindings[relative] = sorted(names)
    (prepared / "adapter-bindings.json").write_text(json.dumps(adapter_bindings, indent=2))
    (prepared / "manifest.json").write_text(json.dumps(manifest, indent=2))
    return manifest


def fixture(meta, out, scene):
    """One all-scenario ROM pair; a case is a ROM-data configuration, not a build."""
    directory = out / "fixture-pair"
    manifest_path = directory / "manifest.json"
    if manifest_path.exists():
        shared = json.loads(manifest_path.read_text())
    else:
        directory.mkdir(exist_ok=True)
        source = directory / "source"
        if source.exists():
            shutil.rmtree(source)
        copy_sources(Path(meta["source"]), source, reuse_objects=True)
        version = meta["version"]
        # Generation derives current palette bytes from the clean 2 MiB ROM.
        baseline = Path(meta["source"]) / f"polishedcrystal-{version}.gbc"
        for path in (baseline, baseline.with_suffix(".sym")):
            shutil.copy2(path, source / path.name)
        for variant in ("crystal", "mobile"):
            run(["make", "-j4", variant, "MOBILE_TEST_SCENARIOS=1", f"RGBDS={meta['rgbds']}/"],
                source, directory / f"{variant}.log")
        scenarios = json.loads((source / "mobile_adapter/build-test/acceptance-scenarios.json").read_text())
        (directory / "scenarios.json").write_text(json.dumps(scenarios, indent=2))
        shutil.copy2(source / "mobile_adapter/build-test/acceptance-scenarios.asm", directory / "events.asm")
        shared = {"source": str(source), "scenarios": scenarios,
            "single_rom": str(source / f"polishedcrystal-test-{version}.gbc"),
            "mobile_rom": str(source / f"polishedcrystal-mobile-test-{version}.gbc"),
            "build_flag": "MOBILE_TEST_SCENARIOS", "test_rom_bytes": 4 * 1024 * 1024}
        for key in ("single_rom", "mobile_rom"):
            rom = Path(shared[key]).read_bytes()
            if len(rom) != shared["test_rom_bytes"] or rom[0x147:0x14a] != bytes((0x10, 7, 3)):
                raise RuntimeError("Both optional test ROMs must use 4 MiB MBC30 and normal 32 KiB SRAM")
        single_names = symbols(Path(shared["single_rom"]).with_suffix(".sym"))
        mobile_names = symbols(Path(shared["mobile_rom"]).with_suffix(".sym"))
        shared["scenario_headers_match"] = all(
            single_names["Acceptance_" + name.replace("-", "_") + "_Header"] ==
            mobile_names["Acceptance_" + name.replace("-", "_") + "_Header"]
            for name, row in scenarios.items() if row.get("scene_type") != "original-map")
        original = scenarios["native-headbutt"]["original_header"]
        shared["original_map_header_matches"] = single_names[original] == mobile_names[original]
        if (source / "maps/Route29.asm").read_bytes() != (Path(meta["source"]) / "maps/Route29.asm").read_bytes():
            raise RuntimeError("Original Route29 events changed during fixture preparation")
        shared["save_event_addresses_match"] = all(
            single_names[f"Acceptance_{name}_{suffix}"] == mobile_names[f"Acceptance_{name}_{suffix}"]
            for name in ("save", "private_save") for suffix in ("NPC", "Ready", "Text"))
        if not shared["scenario_headers_match"] or not shared["save_event_addresses_match"] or not shared["original_map_header_matches"]:
            raise RuntimeError("Paired native save/event addresses must match before testing")
        retain_rom_outputs(source)
        manifest_path.write_text(json.dumps(shared, indent=2))
    fx = dict(shared["scenarios"][scene])
    fx.update({key: shared[key] for key in ("source", "single_rom", "mobile_rom", "build_flag")})
    return fx


class Proxy(socketserver.ThreadingTCPServer):
    daemon_threads = True
    allow_reuse_address = True
    def __init__(self, upstream, fault, wait_for_arm=False, identity=None):
        self.upstream, self.fault = upstream, fault
        self.identity = identity
        self.started = time.monotonic()
        self.injected = 0
        self.recovered = 0
        self.connection_lock = threading.Lock()
        self.single_reset = False
        self.next_connection = 0
        self.wire = []
        self.wire_counts = {}
        self.armed = False
        self.wait_for_arm = wait_for_arm
        super().__init__(("127.0.0.1", 0), ProxyHandler)


    def observe(self, connection, direction, frame):
        # Identity evidence is bounded independently of ongoing presence/polls.
        if not self.identity or frame[3] not in (WIRE_KINDS["HELLO"], WIRE_KINDS["WELCOME"], WIRE_KINDS["ERROR"]):
            return
        with self.connection_lock:
            key = (connection, direction, frame[3])
            if key not in self.wire_counts and len(self.wire_counts) >= 256:
                return
            count = self.wire_counts.get(key, 0)
            self.wire_counts[key] = count + 1
            if count < 4 and len(self.wire) < 256:
                self.wire.append({"connection": connection, "direction": direction, "frame": frame.hex()})


class ProxyHandler(socketserver.BaseRequestHandler):
    def handle(self):
        pending = bytearray()
        outgoing = bytearray()
        with self.server.connection_lock:
            connection = self.server.next_connection
            self.server.next_connection += 1
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
                            if not self.server.identity:
                                upstream.sendall(data)
                                continue
                            outgoing.extend(data)
                            while len(outgoing) >= 8:
                                size = 8 + int.from_bytes(outgoing[6:8], "big")
                                if size > 128:
                                    raise RuntimeError("Unexpected native framing")
                                if len(outgoing) < size:
                                    break
                                self.server.observe(connection, "native-to-bridge", bytes(outgoing[:size]))
                                del outgoing[:size]
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
                            original_frame = frame
                            if self.server.identity:
                                self.server.observe(connection, "bridge-to-native", frame)
                            del pending[:size]
                            if self.server.identity == "digest-held" and frame[3] == WIRE_KINDS["ERROR"]:
                                # Record the authoritative rejection, but hold its delivery.
                                # The rejected native candidate must cancel its live wait
                                # physically, without racing automatic ERROR teardown.
                                # Keep EOF from becoming an alternate automatic escape.
                                hold_deadline = time.monotonic() + 15
                                while time.monotonic() < hold_deadline:
                                    readable, _, _ = select.select((self.request,), (), (), .02)
                                    if readable and not self.request.recv(4096):
                                        return
                                return
                            elapsed = time.monotonic() - self.server.started
                            fault = self.server.fault
                            if (self.server.armed or (not self.server.wait_for_arm and elapsed >= 4)) and fault != "none":
                                if fault == "silence":
                                    self.server.injected += 1
                                    continue
                                if fault == "partial":
                                    self.request.sendall(frame[:4])
                                    self.server.injected += 1
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
                            if frame != original_frame:
                                self.server.injected += 1
                            valid_response = frame[3] in (WIRE_KINDS["WELCOME"], WIRE_KINDS["ROSTER"], WIRE_KINDS["DELTA"])
                            if valid_response and ((fault == "reconnect" and elapsed >= 6) or
                                                   (fault == "reconnect-one" and opened_after_reset)):
                                self.server.recovered += 1
        except OSError:
            return


def projected(directory):
    names = ("hROMBank", "hJoypadDown", "hJoyPressed", "wMobileMenuState", "vMobileMenuSourceBank", "vMobileMenuSourcePC", "vMobileMenuInstructionActive", "vAcceptanceMenuSourceBank", "vAcceptanceMenuSourcePC", "hCrashCode", "hInMenu", "wMapStatus", "wScriptRunning", "wInputFlags", "wXCoord", "wYCoord",
        "wMapGroup", "wMapNumber", "wTimeOfDay", "vAcceptanceReady", "vAcceptanceDialogDone", "vAcceptanceLoaded", "vAcceptanceLoadFailed", "vStoryFlagsHash",
        "wMobileOnline", "wMobileWanted", "wMobileOwner", "wMobileServerError", "wMobilePeerCount", "wMobilePlayerId", "wMobileGeneration",
        "vNPCOccupiedMask", "vNPCPositionsHash", "vLocalPaletteHash", "vOAMMaxScanline",
        "vProfileWorldUpdates", "vAcceptanceNativeHash", "vAcceptancePartyHash", "vAcceptanceInventoryHash",
        "vAcceptanceOAMHash", "vAcceptanceLocalComplete", "vAcceptanceFrameAudio", "vAcceptanceSaveCount",
        "vAcceptanceLocalPaletteHash", "vAcceptanceLocalTileHash", "vAcceptancePaletteDemand", "vAcceptanceSaveActive", "vAcceptanceMenuActive", "vAcceptanceLinkEntered",
        "vAcceptanceSaveBytes", "vAcceptanceSaveMain", "vAcceptanceSaveBackup", "vAcceptanceTrainerID",
        "vAcceptanceEffectCount", "vAcceptanceEffectOverflow", "vAcceptanceNativeSceneReady", "vAcceptanceHeadbuttControlUpReleased", "vAcceptanceHeadbuttFinished", "vAcceptanceHeadbuttAnimationStarted", "vAcceptanceHeadbuttAsked", "vAcceptanceHeadbuttAskScript", "vAcceptanceHeadbuttUseScript", "hScriptBank", "hScriptPos", "wMenuCursorX", "wMenuCursorY", "wBattleMode", "wBattleMenuFlags", "wBattleMenuCursorBuffer", "wMobileKeyLatch", "wMobileTransferActive", "wMobileSavedSerialIE", "vAcceptanceSerialSC", "vAcceptanceSerialIE",
        *(f"vAcceptanceEffectActive{i}" for i in range(8)))
    frames = []
    with gzip.open(directory / "trace.jsonl.gz", "rt") as stream:
        for line in stream:
            row = json.loads(line)
            state = row["symbols"]
            values = {key: state[key] for key in names if key in state}
            values.update((key, value) for key, value in state.items()
                          if key.startswith(("vMobileMenuTile", "vMobileMenuRoster")))
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
                for suffix in ("PaletteHash", "TileHash", "CacheHash", "OAM0", "OAM1"):
                    key = f"vMobilePeer{peer}{suffix}"
                    if key in state:
                        values[key] = state[key]
                for suffix in ("Complete", "Partial", "TileHash"):
                    key = f"vAcceptancePeer{peer}{suffix}"
                    if key in state:
                        values[key] = state[key]
            frames.append({"frame": row["frame"], "state": values})
    return {"frame_calls": json.loads((directory / "state.json").read_text())["frame_calls"], "frames": frames,
        "profile": json.loads((directory / "profile.json").read_text()),
        "state": json.loads((directory / "state.json").read_text())["symbols"],
        "save": list((directory / "result.sav").read_bytes()),
        "save_sha256": sha(directory / "result.sav"),
        "events": [json.loads(line) for line in (directory / "input-events.jsonl").read_text().splitlines()]}


def seed_saves(meta, session, fx, hour):
    """One current native producer per avatar; never reuse historic battery data."""
    seed_fx = fixture(meta, session, "save")
    source = Path(seed_fx["source"])
    native = Path(seed_fx["single_rom"])
    names = symbols(native.with_suffix(".sym"))
    producer = fx.get("producer", fx)
    native_headbutt = "producer" in fx
    def produce(index, position):
        directory = session / f"seed-{hour}-{index}"
        if not (directory / "result.sav").exists():
            directory.mkdir(exist_ok=True)
            values = {"wMapGroup": producer["map"]["group"], "wMapNumber": producer["map"]["number"],
                      "wXCoord": position[0], "wYCoord": position[1]}
            patch_data(native, names, {"AcceptanceAvatar": bytes([index]),
                "AcceptanceTrainerID": (10001 + index).to_bytes(2,"big"), "AcceptanceHour": bytes([hour]),
                "AcceptanceScenarioID": bytes([seed_fx["scene_id"]]),
                "AcceptanceSpawn": bytes(values[name] for name in producer["map"]["spawn_fields"]),
                "AcceptanceNPCOverride": bytes([1]),
                "AcceptanceSeedProfile": bytes([int(native_headbutt)]),
                "AcceptanceNPCPosition": bytes([position[1]+5,position[0]+4])}, directory / "fixture.gbc")
            (directory / "input.txt").write_text("AFTER 0 vAcceptanceReady=1,wMapStatus=2,wScriptRunning=0 A 8\nAFTER 90 vAcceptanceReady=1,wMapStatus=2,wScriptRunning=0 A 8\n")
            seed_observer = ["--effect-span", "0:GiveANickname_YesNo", "--effect-span", "1:DoMenuJoypadLoop"] if native_headbutt else []
            if native_headbutt:
                (directory / "input.txt").write_text("AFTER 0 vAcceptanceReady=1,wMapStatus=2,wScriptRunning=0 A 8\nAFTER 90 vAcceptanceReady=1,wMapStatus=2,wScriptRunning=0 A 8\nON 0 vAcceptanceEffectActive0=1 B 8\nAFTER 60 vAcceptanceEffectActive0=1 B 8\nON 0 vAcceptanceEffectActive0=1,vAcceptanceEffectActive1=1,wMenuCursorY=1 B 8\n")
            run([meta["runner"], meta["core"], directory / "fixture.gbc", directory / "screen.ppm", directory,
                "--frames", "1800", "--symbols", native.with_suffix(".sym"), "--input", directory / "input.txt",
                "--state-out", directory / "state.json", "--ram-out", directory / "wram.bin", "--save-out", directory / "result.sav",
                "--trace-out", directory / "trace.jsonl.gz", "--input-events", directory / "input-events.jsonl",
                "--profile-out", directory / "profile.json", "--profile-window-start", "600",
                "--adapter", "0", "--acceptance", "1", *seed_observer], source, directory / "process.log")
        state = json.loads((directory / "state.json").read_text())["symbols"]
        profile = json.loads((directory / "profile.json").read_text())
        if state["vAcceptanceDialogDone"] != 1 or profile["save_copy_count"] < 4:
            raise RuntimeError("Native seed producer did not reach its post-initialization main/backup save")
        if native_headbutt:
            expected = producer["party"]
            data = native.read_bytes()
            def constant(name):
                at = rom_offset(names[name])
                return int.from_bytes(data[at:at+2], "little")
            assertions = {"vAcceptancePartyCount": 1, "vAcceptancePartySpecies": constant("AcceptanceHeadbuttSpecies"),
                          "vAcceptancePartyForm": constant("AcceptanceHeadbuttForm"), "vAcceptancePartyLevel": expected["level"],
                          "vAcceptancePartyMove": constant("AcceptanceHeadbuttMove"), "vAcceptancePartyItem": 0,
                          "vAcceptancePartyStatus": 0, "vAcceptancePartyEgg": 0, "vAcceptanceHeadbuttFlag": 1}
            if any(state.get(key) != value for key, value in assertions.items()) or not state.get("vAcceptancePartyHP", 0):
                raise RuntimeError("Native Headbutt producer party/flag assertions failed: " + repr({key: state.get(key) for key in assertions}))
        return directory / "result.sav"
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        return list(pool.map(produce, range(4), [producer["primary"], *producer["peers"]]))


def mobile_menu_inputs(action):
    """Physical actions anchored to actual menu ownership and measurement state."""
    text = ""
    def chord(anchor, buttons, duration=8):
        nonlocal text
        for button in buttons:
            text += f"{anchor} {button} {duration}\n"
    if action == "mobile-menu-connect":
        chord("ON 0 vAcceptanceReady=1,wMapStatus=2,wScriptRunning=0", ("SELECT", "A"))
        chord("AFTER 100 wMobileMenuState=1,wMenuCursorY=1", ("A",))
        chord("AFTER 650 vAcceptanceMeasuring=1", ("LEFT",), 12)
        return text
    chord("AFTER 50 vAcceptanceMeasuring=1", ("SELECT", "A"))
    if action in ("mobile-menu-players", "mobile-menu-exit-roster"):
        chord("AFTER 40 wMobileMenuState=1", ("DOWN",))
        chord("AFTER 80 wMobileMenuState=1", ("A",))
        if action == "mobile-menu-players":
            # Advance the native paragraph prompts, then WaitButton and menu B.
            for at in (150, 210, 270):
                chord(f"AFTER {at} wMobileMenuState=1", ("A",))
            chord("AFTER 330 wMobileMenuState=1", ("B",))
        else:
            chord("AFTER 140 wMobileMenuState=1", ("SELECT", "B"), 160)
            chord("AFTER 330 wMobileMenuState=1", ("B",))
    elif action == "mobile-menu-cancel":
        for at in (40, 65, 90):
            chord(f"AFTER {at} wMobileMenuState=1", ("DOWN",))
        chord("AFTER 120 wMobileMenuState=1", ("A",))
    elif action == "mobile-menu-disconnect":
        for at in (40, 65):
            chord(f"AFTER {at} wMobileMenuState=1", ("DOWN",))
        chord("AFTER 100 wMobileMenuState=1", ("A",))
    elif action == "mobile-menu-exit":
        chord("AFTER 100 wMobileMenuState=1", ("SELECT", "B"), 160)
        chord("AFTER 300 wMobileMenuState=1", ("B",))
    else:
        chord("AFTER 100 wMobileMenuState=1", ("B",))
    if action in ("mobile-menu-disconnect", "mobile-menu-exit", "mobile-menu-exit-roster"):
        chord("AFTER 500 vAcceptanceMeasuring=1", ("SELECT", "B"))
    chord("AFTER 650 vAcceptanceMeasuring=1", ("LEFT",), 12)
    return text


HEADBUTT_FUNCTIONAL_HORIZON = 3200
HEADBUTT_RECOVERY_WINDOW = 2800


def functional_horizon_options(functional, enabled, action):
    """Extend only gated Headbutt clients, including their healthy siblings."""
    if functional and enabled and action in ("native-headbutt", "native-headbutt-held"):
        return ["--functional-horizon", str(HEADBUTT_FUNCTIONAL_HORIZON)]
    return []


def native_headbutt_recovery_inputs(rom, names):
    """Advance native text, select Run with B, then confirm it physically with A."""
    # Derive the suspended script position from this build's linked data.
    # startbattle consumes its opcode before StartBattle runs synchronously.
    macros = (ROOT / "macros/scripts/events.asm").read_text()
    commands = re.findall(r"^\s*const (\w+_command)\s*$", macros, re.M)
    if len(re.findall(r"^\s*const_def\s*$", macros, re.M)) != 1 or re.search(
            r"^\s*const_(?:skip|next)\b", macros, re.M):
        raise RuntimeError("Script command numbering requires a new explicit binding")
    tail = bytes(commands.index(name + "_command") for name in
                 ("randomwildmon", "startbattle", "reloadmapafterbattle", "end"))
    bank, end = names["AutoHeadbuttScript.no_battle"]
    begin_bank, begin = names["AutoHeadbuttScript"]
    if bank != begin_bank or end - begin < len(tail):
        raise RuntimeError("Native Headbutt encounter script binding changed")
    data = Path(rom).read_bytes()
    at = rom_offset((bank, end - len(tail)))
    if data[at:at + len(tail)] != tail:
        raise RuntimeError("Native Headbutt encounter script tail changed")
    battle_position = end - 2
    phase = (f"vAcceptanceHeadbuttFinished=1,wScriptRunning=255,wBattleMode=1,"
             f"hScriptBank={bank},hScriptPos={battle_position}")
    menu_bank, menu_pc = names["DoMenuJoypadLoop"]
    menu = (f"{phase},vAcceptanceMenuActive=1,"
            f"vAcceptanceMenuSourceBank={menu_bank},vAcceptanceMenuSourcePC={menu_pc},"
            "wBattleMenuFlags=7")
    text = ""
    # hInMenu is zero at the observed tree encounter intro. Actual menu-loop
    # ownership distinguishes text from the later battle menu, not hInMenu.
    # Keep native escape failure/retry behavior within the existing horizon.
    for offset in range(0, HEADBUTT_RECOVERY_WINDOW, 40):
        text += f"OPTIONAL CHECKED_AFTER {offset} {phase},vAcceptanceMenuActive=0 B 8\n"
        # QUICK_B moves the cursor to Run but the $87 battle-menu filter only
        # accepts A. Stop selecting once B has moved the cursor off the top row.
        text += f"OPTIONAL CHECKED_AFTER {offset} {menu},wMenuCursorY=1 B 8\n"
        # A may confirm only the actual bottom-right Run cursor. Require a
        # continuous physical release before each pulse, including after failed
        # native escape attempts; stale readiness cannot carry across text.
        text += (f"OPTIONAL CHECKED_AFTER {offset + 16} {menu},"
                 "wMenuCursorX=2,wMenuCursorY=2,hJoypadDown=0 A 8\n")
    return text


def native_headbutt_inputs(rom, names, profile, index, start_ready=""):
    """Drive common native actions from each primary role's own game state."""
    # Networking and Select+B ownership challenges are separate from normal
    # gameplay. Never replay one role's actions into the independent control.
    text = "# Common native Headbutt actions use independent game readiness.\n"
    if index != 0:
        return text
    world = ("vAcceptanceNativeSceneReady=1,wMapStatus=2,wScriptRunning=0,hInMenu=0,"
             "wBattleMode=0,wXCoord=46,wYCoord=8,vAcceptanceHeadbuttAsked=0,"
             "vAcceptanceHeadbuttFinished=0,vAcceptancePartyCount=1,"
             "vAcceptancePartyMove=29")
    # Networking may delay UP, but cannot replace any native prerequisite.
    start = f",{start_ready}" if start_ready else ""
    text += f"ON {profile + 50} {world},hJoypadDown=0{start} UP 8\n"
    text += f"CHECKED_AFTER 16 {world},wPlayerDirection=4,hJoypadDown=0,vAcceptanceHeadbuttControlUpReleased=1 A 8\n"
    for offset in range(0, 500, 40):
        text += f"OPTIONAL CHECKED_AFTER {offset} vAcceptanceHeadbuttAsked=1,vAcceptanceHeadbuttAskScript=1,vAcceptanceMenuActive=0,vAcceptanceHeadbuttFinished=0,hJoypadDown=0 B 1\n"
        text += f"OPTIONAL CHECKED_AFTER {offset} vAcceptanceHeadbuttAsked=1,vAcceptanceHeadbuttUseScript=1,vAcceptanceMenuActive=0,wBattleMode=0,vAcceptanceHeadbuttFinished=0,hJoypadDown=0 B 1\n"
    menu_bank, menu_pc = names["DoMenuJoypadLoop"]
    text += ("CHECKED_AFTER 16 vAcceptanceHeadbuttAsked=1,vAcceptanceHeadbuttAskScript=1,"
             "vAcceptanceMenuActive=1,"
             f"vAcceptanceMenuSourceBank={menu_bank},vAcceptanceMenuSourcePC={menu_pc},"
             "wMenuCursorX=1,wMenuCursorY=1,wBattleMode=0,hJoypadDown=0,vAcceptanceHeadbuttFinished=0 A 8\n")
    for offset in range(0, 400, 40):
        text += f"OPTIONAL CHECKED_AFTER {offset} vAcceptanceHeadbuttFinished=1,wScriptRunning=255,wBattleMode=0,vAcceptanceMenuActive=0,hJoypadDown=0 B 8\n"
    # Ownership can change during a delivered A8. Native text/selection B must
    # wait for its physical release as well as the independent source state.
    text += native_headbutt_recovery_inputs(rom, names).replace(
        " B 8\n", ",hJoypadDown=0 B 8\n")
    text += ("ON 0 vAcceptanceHeadbuttFinished=1,wScriptRunning=0,wBattleMode=0,"
             "vAcceptanceNativeSceneReady=1,wMapStatus=2,hJoypadDown=0 LEFT 12\n")
    return text


def native_headbutt_control_inputs(rom, names, profile, index):
    """Build the independent disabled role with the common native actions."""
    return native_headbutt_inputs(rom, names, profile, index)


def execute(meta, fx, out, seeds, action="idle", fault="none", speeds=(1, 1, 1, 1), duplicate=False, settle_frames=300, functional=False, moving_peers=False, identity=None, ordinary_count=0, static_sources=False):
    if ordinary_count and (ordinary_count != 800 or not functional or action != "idle" or fault != "none" or identity or moving_peers):
        raise ValueError("Ordinary capture requires exactly 800 idle functional outputs without faults")
    if out.exists():
        original = out
        attempt = 2
        while out.exists():
            out = original.with_name(original.name + f"-attempt-{attempt}")
            attempt += 1
    if static_sources and (fx.get("scene") != "empty" or action != "idle" or fault != "none" or identity or moving_peers):
        raise ValueError("Static source extraction supports only unchanged EMPTY stationary idle inputs")
    source = Path(fx["source"])
    version = meta["version"]
    mobile = Path(fx["mobile_rom"])
    native = Path(fx["single_rom"])
    mobile_symbols, native_symbols = symbols(mobile.with_suffix(".sym")), symbols(native.with_suffix(".sym"))
    out.mkdir(exist_ok=True)
    ordinary = None
    if ordinary_count:
        reader = out / "ordinary-picture-reader"
        run(["rustc", "--edition=2021", "-O", TESTS / "support/ordinary_picture_reader.rs", "-o", reader],
            source, out / "ordinary-reader-build.log")
        ordinary = OrdinaryProcesses(reader, ordinary_count)
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
        try:
            bridge.wait(timeout=5)
        except subprocess.TimeoutExpired:
            bridge.kill()
            bridge.wait()
        log.close()
        raise
    try:
        proxy = Proxy(upstream_port, fault, wait_for_arm=functional, identity=identity)
    except BaseException:
        bridge.terminate()
        try:
            bridge.wait(timeout=5)
        except subprocess.TimeoutExpired:
            bridge.kill()
            bridge.wait()
        log.close()
        raise
    port = proxy.server_address[1]
    threading.Thread(target=proxy.serve_forever, daemon=True).start()
    proxy.started = time.monotonic()

    def client(index, enabled, original_actors=False):
        speed = speeds[index]
        ready = "vAcceptanceNativeSceneReady" if fx.get("native_map_events") else "vAcceptanceReady"
        replay = None
        # Native initialization is complete before frame 500. The explicit join
        # starts at 600; retain five real seconds for join/assets before measuring
        # the unchanged 800-frame window and 200-frame recovery tail.
        first, profile = 500, 500 + settle_frames * speed
        total = profile + 1000 * speed
        if functional:
            # One normal measurement window in emulated frames, regardless of
            # wall-clock acceleration. The actual lobby releases the start gate.
            profile, total = 800, 12000
            if not enabled:
                replay = json.loads((out / f"mobile/{index}/profile.json").read_text())
                profile = replay["world_window_start"]
                total = json.loads((out / f"mobile/{index}/state.json").read_text())["frame_calls"]
        directory = out / ("mobile" if enabled else "single-original" if original_actors else "single") / str(index)
        directory.mkdir(parents=True)
        rom = directory / "fixture.gbc"
        current, bindings = (mobile, mobile_symbols) if enabled else (native, native_symbols)
        original_rom = current.read_bytes()
        position = fx["primary"] if index == 0 else fx["peers"][index - 1]
        changes = {"AcceptanceScenarioID": bytes([fx["scene_id"]]), "AcceptanceAvatar": bytes([index]), "AcceptanceScenarioLoad": bytes([1]), "AcceptanceTrainerID": (12345 if duplicate else 10001 + index).to_bytes(2, "big"),
                   "AcceptanceHour": bytes([22 if action == "night" else 12]),
                   "AcceptanceInitialFault": bytes([0]), "AcceptanceNPCOverride": bytes([0]),
                   "AcceptanceLighting": bytes([original_rom[rom_offset(bindings["AcceptanceDarkLighting" if action == "dark" else "AcceptanceAutoLighting"])]] ) if action in ("dark", "night") else bytes([255]),
                   "AcceptanceInteractionFault": bytes([int(action.split(":")[1]) if action.startswith("private:") and enabled and index == 0 else 0])}
        if enabled:
            bank, addr = bindings["MobileEndpoint"]
            # Endpoint is declarative ROM data, not an opcode or RAM mutation.
            changes["MobileEndpoint"] = original_rom[rom_offset((bank, addr)):rom_offset((bank, addr)) + 4] + port.to_bytes(2, "big")
        # SPAWN_HOME's entry is current-build data; only map position differs per player.
        values = {"wMapGroup": fx["map"]["group"], "wMapNumber": fx["map"]["number"], "wXCoord": position[0], "wYCoord": position[1]}
        changes["AcceptanceSpawn"] = bytes(values[name] for name in fx["map"]["spawn_fields"])
        if not enabled and fx["equal_actor_load"] and not original_actors:
            figure_at = rom_offset(bindings["AcceptanceFigures"])
            figures = original_rom[figure_at:figure_at + 4]
            positions = [fx["primary"], *fx["peers"]]
            controls = bytearray()
            for avatar in (i for i in range(4) if i != index):
                px, py = positions[avatar]
                controls.extend((figures[avatar], py + 4, px + 4))
            changes["AcceptanceControlPeers"] = bytes(controls)
        patch_data(current, bindings, changes, rom)
        if enabled and index == 0 and identity in ("digest", "schema", "digest-held"):
            patch_identity_fixture(rom, bindings, identity.removesuffix("-held"), rom)
        text = "# Native initialization enters the fixture without title/dialog input.\n"
        if enabled and not (index == 0 and (action.startswith("private:") or action == "mobile-menu-connect")):
            for button in ("SELECT", "B"):
                text += f"ON 0 vAcceptanceReady=1,wMapStatus=2,wScriptRunning=0 {button} 8\n"
        if enabled and index == 0 and identity in ("digest", "schema", "digest-held"):
            # Observe rejection first; the held case exercises physical local cancellation.
            if identity == "digest-held":
                text += "AFTER 100 vAcceptanceMeasuring=1 SELECT 8\nAFTER 100 vAcceptanceMeasuring=1 B 8\n"
            text += "AFTER 200 vAcceptanceMeasuring=1 LEFT 12\n"
        if functional and enabled and index == 1 and fault not in ("none", "reconnect", "reconnect-one"):
            # Demand a genuine response after arming the proxy. Standing clients
            # can be served by the Bridge cache without any upstream bytes.
            text += "ON 0 vAcceptanceFaultArmed=1 RIGHT 8\n"
        at = profile + 150 * (1 if functional else speed)
        if index == 0 and (action in ("interaction", "save", "script", "link", "warp", "fault-save-exit", "npc-movement") or action.startswith("private:")):
            text += f"{at} A 8\n{at + 90} A 8\n"
        elif index == 0 and action == "menu":
            text += f"{at} START 8\n"
        if index > 0 and action == "clear-conflict":
            text += f"{at} UP 12\n"
        if index > 0 and moving_peers:
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
        if enabled and index == 0 and action.startswith("mobile-menu-"):
            text += mobile_menu_inputs(action)
        if functional and enabled:
            transformed = []
            for line in text.splitlines():
                fields = line.split()
                if fields and fields[0].isdigit():
                    offset = max(0, int(fields[0]) - profile)
                    transformed.append(f"AFTER {offset} vAcceptanceMeasuring=1 {fields[1]} {fields[2]}")
                elif fields and fields[0] == "ON" and int(fields[1]) > 0:
                    transformed.append(f"ON 0 vAcceptanceMeasuring=1,{fields[2]} {fields[3]} {fields[4]}")
                else:
                    transformed.append(line)
            text = "\n".join(transformed) + "\n"
        elif functional and not enabled and action in ("native-headbutt", "native-headbutt-held"):
            text = native_headbutt_control_inputs(native, native_symbols, profile, index)
        elif functional and not enabled:
            # Reproduce actual observed inputs, not guessed timings. The initial
            # connect chord is feature-specific and absent from the native run.
            observed = [json.loads(line) for line in (out / f"mobile/{index}/input-events.jsonl").read_text().splitlines()]
            text = "# Observed mobile actions replayed in the native control.\n"
            for event in observed:
                if event["frame"] < profile and event["button"] in (0, 2):
                    continue
                button_name = {0: "B", 2: "SELECT", 3: "START", 4: "UP", 5: "DOWN", 6: "LEFT", 7: "RIGHT", 8: "A"}[event["button"]]
                text += f"{event['frame']} {button_name} {event['duration']}\n"
        if fx.get("native_map_events"):
            text = text.replace("vAcceptanceReady", ready)
            if index == 0 and enabled and not ordinary_count:
                peer_gate = ",".join(f"vAcceptancePeer{peer}Complete=1,wMobilePeer{peer}NativeStage=7" for peer in range(1,4))
                text += native_headbutt_inputs(mobile, mobile_symbols, profile, index,
                                              f"vAcceptanceMeasuring=1,{peer_gate}")
                if action == "native-headbutt-held":
                    for button in ("SELECT", "B"):
                        text += f"ON 0 vAcceptanceEffectActive0=1,vAcceptanceHeadbuttAnimationStarted=1 {button} 160\n"
                        text += f"CHECKED_AFTER 240 vAcceptanceHeadbuttAnimationStarted=1,wScriptRunning=0,wMobileWanted=0 {button} 8\n"
        script = directory / "input.txt"
        script.write_text(text)
        command = [meta["runner"], meta["core"], rom, directory / "screen.ppm", directory,
            "--frames", total, "--symbols", current.with_suffix(".sym"), "--input", script,
            "--state-out", directory / "state.json", "--ram-out", directory / "wram.bin", "--save-out", directory / "result.sav",
            "--save-in", seeds[index],
            "--trace-out", directory / "trace.jsonl.gz", "--input-events", directory / "input-events.jsonl",
            "--profile-out", directory / "profile.json", *([] if ordinary_count else ["--profile-window-start", total - 1000 if functional and enabled else profile]),
            "--pace-from", first if enabled else total, "--pace-multiplier", speed, "--adapter", int(enabled), "--acceptance", "1"]
        if static_sources:
            command += ["--static-intent-sources", directory / "static-sources.jsonl"]
        command += functional_horizon_options(functional, enabled, action)
        if functional and index == 0 and action in ("native-headbutt", "native-headbutt-held"):
            command += ["--native-headbutt-control", "1"]
        if fx.get("native_map_events"):
            command += ["--native-object-count", fx["ordinary_objects"], "--acceptance-menu-routine", "DoMenuJoypadLoop"]
            predicates = [("wMapGroup", fx["map"]["group"]), ("wMapNumber", fx["map"]["number"]),
                          ("wMapStatus", 2), ("wScriptRunning", 0), ("wPlayerObject", 0),
                          ("wPlayerSprite", "nonzero"), ("wPlayerObject + 1", 0), ("wPartyCount", "nonzero")]
            # The native map object's sprite is the first byte; the struct map index is a separate emitted binding.
            predicates[6] = ("wPlayerMapObjectIndex", 0)
            for field, (name, expected) in enumerate(predicates):
                command += ["--native-scene-field", f"{field}:{name}:{expected}:255"]
            validate_native_effect_sources(current, bindings)
            spans = ["ShakeHeadbuttTree", "WriteVCopyRegistersToHRAM", "MobileNativeGraphicsRequest", "MobileNativeGraphicsRelease", "MobileVirtualCommitPose.dma", "DoNextFrameForAllSprites_OW", "TryHeadbuttOW", "RunScriptCommand", "ClearSpriteAnims"]
            for number, name in enumerate(spans):
                if enabled or number not in (2,3,4):
                    command += ["--effect-span", f"{number}:{name}"]
            watches = [("wShadowOAM",160),("hInMenu",1),("wScriptRunning",1),("hRequested2bpp",7),("wSpriteAnimationStructs",160)]
            if enabled: watches.append(("wMobileNativeGraphicsFence",1))
            else: watches.append(("hInMenu",1))
            watches.append(("hScriptBank",3))
            for number,(name,length) in enumerate(watches):
                command += ["--effect-watch", f"{number}:{name}:{length}"]
        if fx.get("native_map_events") and index == 0 and not ordinary_count:
            command += ["--effect-video", "1"]
        if enabled and index == 0 and action.startswith("mobile-menu-"):
            command += ["--acceptance-menu-routine", "MobileMenuInput"]
        if functional and enabled and not ordinary_count:
            command += ["--measurement-gate", out / "measure", "--measurement-warmup", "300"]
            if action.startswith("private:") or identity in ("digest", "schema", "digest-held"):
                command += ["--expected-online", 0 if index == 0 else 1, "--expected-peers", 0 if index == 0 else 2]
        if enabled and index == 0 and action == "resume":
            command += ["--test-snapshot-out", directory / "online.state"]
        if enabled:
            command += ["--finish-barrier", out / "capture"]
        if ordinary_count:
            # Read-only state/profile summaries suffice; do not retain frame traces.
            trace_at = command.index("--trace-out")
            del command[trace_at:trace_at + 2]
            command += ["--ordinary-stream", directory / "ordinary.fifo", "--ordinary-count", ordinary_count,
                        "--ordinary-gate", out / "ordinary.gate"]
            ordinary.run(command, source, directory / "process.log", directory)
            state = json.loads((directory / "state.json").read_text())
            return {"frame_calls": state["frame_calls"], "state": state["symbols"],
                    "profile": json.loads((directory / "profile.json").read_text())}
        else:
            run(command, source, directory / "process.log", timeout=100)
        return projected(directory)

    try:
        capture_barrier = out / "capture.exit"
        capture_barrier.touch()
        if ordinary_count:
            (out / "capture").touch()
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            if identity in ("digest", "schema", "digest-held"):
                healthy = [pool.submit(client, index, True) for index in range(1, 4)]
                deadline = time.monotonic() + 60
                while not all((out / f"mobile/{index}/state.json.measurement-ready").exists() for index in range(1, 4)):
                    for future in healthy:
                        if future.done(): future.result()
                    if time.monotonic() >= deadline:
                        raise TimeoutError("Three healthy native clients did not establish admission")
                    time.sleep(.01)
                futures = [pool.submit(client, 0, True), *healthy]
            else:
                futures = [pool.submit(client,index,True) for index in range(4)]
            deadline = time.monotonic() + 90
            try:
                if ordinary_count:
                    while not all((out / f"mobile/{index}/state.json.ordinary-ready").exists() for index in range(4)):
                        for future in futures:
                            if future.done(): future.result()
                        if time.monotonic() >= deadline:
                            raise TimeoutError("Four ordinary clients did not reach actual readiness")
                        time.sleep(.01)
                    (out / "ordinary.gate").touch()
                    while not all((out / f"mobile/{index}/state.json.ordinary-complete").exists() for index in range(4)):
                        for future in futures:
                            if future.done(): future.result()
                        if time.monotonic() >= deadline:
                            raise TimeoutError("Four exact ordinary captures did not complete")
                        time.sleep(.01)
                    (out / "capture").unlink()
                elif functional:
                    while not all((out / f"mobile/{index}/state.json.measurement-ready").exists() for index in range(4)):
                        for future in futures:
                            if future.done(): future.result()
                        if time.monotonic() >= deadline:
                            raise TimeoutError("Four actual clients did not reach lobby readiness")
                        time.sleep(.01)
                    if identity in ("digest", "schema", "digest-held"):
                        while not any(item["direction"] == "bridge-to-native" and bytes.fromhex(item["frame"])[3] == WIRE_KINDS["ERROR"] for item in proxy.wire):
                            if time.monotonic() >= deadline:
                                raise TimeoutError("Native candidate received no rejection")
                            time.sleep(.01)
                    proxy.armed = True
                    (out / "measure.fault").touch()
                    if fault not in ("none", "reconnect", "reconnect-one"):
                        while not proxy.injected:
                            if time.monotonic() >= deadline: raise TimeoutError("No malformed server bytes were delivered")
                            time.sleep(.01)
                    (out / "measure").touch()
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
            except BaseException:
                if ordinary:
                    ordinary.close()
                raise
            finally:
                capture_barrier.unlink(missing_ok=True)
            online = [future.result() for future in futures]
        if ordinary_count:
            return {"fixture": fx, "clients": online, "ordinary_count": ordinary_count,
                    "ordinary_readers": [json.loads((out / f"mobile/{i}/ordinary-reader.json").read_text()) for i in range(4)],
                    "artifact_directory": str(out), "intent": "UNPROVEN"}
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            offline = list(pool.map(lambda index: client(index, False), range(4)))
        # Keep equal actor load for performance. Separately replay the same
        # original NPCs for exact simulation: additional native avatars cause
        # legitimate extra sprite reloads when a dialogue closes.
        original_npc_control = client(0, False, original_actors=True) if action == "npc-movement" else None
        save_reload = []
        if action == "save":
            for producer, consumer in (("mobile", "single"), ("single", "mobile")):
                directory = out / f"reload-{producer}-into-{consumer}"
                directory.mkdir()
                base_target = native if consumer == "single" else mobile
                target = directory / "fixture.gbc"
                patch_data(base_target, symbols(base_target.with_suffix(".sym")),
                    {"AcceptanceScenarioID": bytes([fx["scene_id"]]), "AcceptanceScenarioLoad": bytes([0])}, target)
                (directory / "input.txt").write_text("# The normal save loader enters the generated scene.\n")
                run([meta["runner"], meta["core"], target, directory / "screen.ppm", directory,
                    "--frames", "1400", "--symbols", base_target.with_suffix(".sym"),
                    "--save-in", out / producer / "0/result.sav", "--save-out", directory / "result.sav",
                    "--input", directory / "input.txt", "--state-out", directory / "state.json", "--ram-out", directory / "wram.bin",
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
                "--state-out", recovery_dir / "state.json", "--ram-out", recovery_dir / "wram.bin", "--save-out", recovery_dir / "result.sav",
                "--input-events", recovery_dir / "input-events.jsonl", "--trace-out", recovery_dir / "trace.jsonl.gz",
                "--profile-out", recovery_dir / "profile.json", "--profile-window-start", "200",
                "--adapter", "0", "--acceptance", "1"], source, recovery_dir / "process.log")
            recovery = projected(recovery_dir)
        return {"fixture": fx, "clients": [{"mobile": m, "single": s} for m, s in zip(online, offline)],
            "menu_charmap": {match[1]: int(match[2], 16) for match in re.finditer(r'charmap "(.)",\s*\$([0-9a-fA-F]+)', (Path(meta["source"]) / "constants/charmap.asm").read_text())} if action.startswith("mobile-menu-") else {},
            "menu_text": dict(re.findall(r'\.(OfflineText|OnlineText|RosterText)\n\s*text "([^"]+)"', (Path(meta["source"]) / "mobile_adapter/rom/menu.asm").read_text())) if action.startswith("mobile-menu-") else {},
            "identity": identity, "identity_wire": list(proxy.wire) if identity else [],
            "clean_counterpart_sha256": meta["baseline_sha256"],
            "fault": fault, "injected": proxy.injected, "recovered": proxy.recovered, "speeds": speeds, "action": action,
            "execution_mode": "functional-400-percent" if functional else "controlled-real-time",
            "recovery": recovery,
            "original_npc_control": original_npc_control,
            "save_reload": save_reload,
            "initial_save_sha256": [sha(seed) for seed in seeds],
            "artifact_directory": str(out),
            "assumptions": ASSUMPTIONS,
            "source_rom_sha256": meta["mobile_sha256"], "core_sha256": meta["core_sha256"]}
    finally:
        if ordinary:
            ordinary.close()
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
        fixture(meta, args.output, "empty")
        test_pair = json.loads((args.output / "fixture-pair/manifest.json").read_text())
        test_pair["production_has_test_entry"] = "AcceptanceBoot" in symbols(base.with_suffix(".sym")) or "AcceptanceBoot" in symbols(mobile.with_suffix(".sym"))
        data = {"test_pair": test_pair, "manifest": meta, "maps": audit(base, mobile, source), "budget": measure(base, mobile),
                "symbols": sorted(symbols(mobile.with_suffix(".sym")))}
        names = symbols(mobile.with_suffix(".sym"))
        if "MobileCompatibilityHash" in names:
            start = rom_offset(names["MobileCompatibilityHash"])
            data["compatibility_hash"] = mobile.read_bytes()[start:start + 32].hex()
    else:
        fault = args.case.removeprefix("fault-") if args.case.startswith("fault-") else "none"
        scene = args.case if args.case in ("scanline", "pool", "palettes", "interaction", "script", "link", "save", "warp", "npc-movement", "night", "dark") else "empty"
        if args.case in ("native-headbutt", "native-headbutt-held"):
            scene = "native-headbutt"
        action = args.case if args.case in ("interaction", "script", "link", "save", "warp", "menu", "exit", "exit-menu", "exit-script", "exit-save", "night", "dark", "resume", "clear-conflict", "npc-movement") else "idle"
        if args.case.startswith("native-headbutt"):
            action = args.case
        if args.case.startswith("mobile-menu-"):
            action = args.case
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
        functional = args.case not in ASSUMPTIONS["performance_cases"] and fault not in ("reconnect", "reconnect-one")
        speeds = (1, 2, 3, 4) if args.case == "speeds" else (4, 4, 4, 4) if functional else (1, 1, 1, 1)
        with preparation_lock(args.output):
            fx = fixture(meta, args.output, scene)
            seeds = seed_saves(meta, args.output, fx, 22 if action == "night" else 12)
        ensure_free_space(CACHE, args.output)
        prepared_at = time.monotonic()
        execution_wall_start = time.time()
        data = execute(meta, fx, args.output / args.case, seeds,
                       action, fault, speeds, args.case == "duplicate-ids",
                       settle_frames=1300 if args.case in ASSUMPTIONS["performance_cases"] else 300, functional=functional,
                       moving_peers=args.case in ("speeds", "speeds-100"),
                       identity=args.case.removeprefix("identity-") if args.case.startswith("identity-") else None)
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
