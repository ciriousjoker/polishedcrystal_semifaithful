from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import socket
import threading
import time

import runtime
from storage import active_run, ensure_free_space


class ConditionalInputs(unittest.TestCase):
    def test_compiled_parser_and_delivery_regressions(self):
        import subprocess
        with tempfile.TemporaryDirectory() as tmp:
            binary = Path(tmp) / "conditional-input"
            source = Path(__file__).with_name("test_conditional_input.cpp")
            subprocess.run(["c++", "-std=c++17", "-O2", str(source), "-o", str(binary)], check=True)
            subprocess.run([str(binary)], check=True)


class FinalStateExport(unittest.TestCase):
    def test_production_final_export_reads_world_updates_and_retains_observers(self):
        import json
        import subprocess

        frontend = Path(__file__).resolve().parents[2] / "tools/smoke_libretro.cpp"
        source = frontend.read_text()
        start = source.index('    if (arguments.count("--state-out")) {')
        end = source.index('\n    if (barrier_enabled', start)
        export = source[start:end]
        harness = r'''
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <map>
#include <string>
#include <type_traits>
#include <vector>
using Values = std::map<std::string, unsigned>;
static unsigned metric_calls = 0, metric_index = ~0u;
static uint64_t metric_value = 0;
static uint64_t read_metric(unsigned index) {
    ++metric_calls;
    metric_index = index;
    return metric_value;
}
static void fail(const std::string&) { std::exit(2); }
static Values capture(const unsigned char*, size_t) {
    return {{"hCrashCode", 0}, {"hCGB", 1}, {"wMobileOnline", 1}};
}
static void acceptance_capture(Values& values, const unsigned char*, size_t) {
    values["vAcceptanceCaptured"] = 43;
}
static unsigned marker_count(unsigned index) { return 10 + index; }
static unsigned serial_status(unsigned index) { return 20 + index; }
static unsigned effect_record(unsigned, unsigned field) {
    return field == 32 ? 3 : 0;
}
static unsigned effect_returns(unsigned index) {
    return index == 0 ? 2 : 1;
}
static unsigned effect_active(unsigned index) { return index == 4; }
static unsigned scene_ready() { return 1; }
template<class Function> static Function symbol(void*, const char* name) {
    if constexpr (std::is_same_v<Function, unsigned(*)(unsigned, unsigned)>) {
        if (std::string(name) == "acceptance_profile_effect_record") return effect_record;
    } else if constexpr (std::is_same_v<Function, unsigned(*)(unsigned)>) {
        if (std::string(name) == "acceptance_profile_effect_returns") return effect_returns;
        if (std::string(name) == "acceptance_profile_effect_active") return effect_active;
    } else if constexpr (std::is_same_v<Function, unsigned(*)()>) {
        if (std::string(name) == "acceptance_profile_native_scene_ready") return scene_ready;
    }
    fail("Unexpected core getter");
    return nullptr;
}
int main(int argc, char** argv) {
    if (argc != 6) return 3;
    const std::string system_directory = argv[1];
    std::map<std::string, std::string> arguments = {
        {"--state-out", system_directory + "/state.json"}, {"--symbols", "fixture.sym"}};
    const bool acceptance_enabled = std::stoul(argv[2]);
    const bool effect_enabled = true;
    using Metric = uint64_t(*)(unsigned);
    Metric metric = std::stoul(argv[3]) ? read_metric : nullptr;
    metric_value = std::stoull(argv[4]);
    auto acceptance_marker_count = std::stoul(argv[5]) ? marker_count : nullptr;
    auto effect_serial_status = serial_status;
    void* core = nullptr;
    const unsigned char* sram = nullptr;
    const size_t sram_size = 0;
    const unsigned input_frame = 3200;
    const unsigned char video_oam[160] = {};
    const Values video_figure_values = {{"vNativeFigureComplete0", 1}};
    const std::vector<unsigned> effect_span_ids = {0, 4};
''' + export + r'''
    std::ofstream calls(system_directory + "/calls.json");
    calls << "{\"calls\":" << metric_calls << ",\"index\":" << metric_index << "}";
}
'''
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            cpp, binary = root / "export.cpp", root / "export"
            cpp.write_text(harness)
            subprocess.run(["c++", "-std=c++17", "-O0", str(cpp), "-o", str(binary)], check=True)

            def run(acceptance, getter, value, markers=1):
                subprocess.run([str(binary), str(root), str(acceptance), str(getter),
                                str(value), str(markers)], check=True)
                state = json.loads((root / "state.json").read_text())
                calls = json.loads((root / "calls.json").read_text())
                self.assertEqual(state["frame_calls"], 3200)
                self.assertEqual((root / "hardware-oam.bin").stat().st_size, 160)
                fields = state["symbols"]
                expected = {"hCrashCode": 0, "hCGB": 1, "wMobileOnline": 1,
                            "vNativeFigureComplete0": 1, "vAcceptanceSerialSC": 20,
                            "vAcceptanceSerialIE": 21, "vAcceptanceHeadbuttAsked": 1,
                            "vAcceptanceHeadbuttFinished": 2,
                            "vAcceptanceHeadbuttAnimationStarted": 1,
                            "vAcceptanceEffectCount": 3, "vAcceptanceEffectOverflow": 0,
                            "vAcceptanceNativeSceneReady": 1,
                            "vAcceptanceEffectActive0": 0, "vAcceptanceEffectActive4": 1}
                if acceptance:
                    expected.update(vAcceptanceCaptured=43,
                                    vProfileWorldUpdates=value if getter else 0)
                    names = ("Ready", "DialogDone", "Loaded", "LoadFailed", "LinkEntered")
                    expected.update({"vAcceptance" + name: 10 + i if markers else 0
                                     for i, name in enumerate(names)})
                self.assertEqual(fields, expected)
                self.assertEqual(calls["calls"], int(bool(acceptance and getter)))
                if acceptance and getter:
                    self.assertEqual(calls["index"], 21)

            # A populated getter must export its actual current value, not an
            # earlier trace sample. Zero and missing optional getters stay zero.
            run(1, 1, 138)
            run(1, 1, 500)
            run(1, 1, 0)
            run(1, 0, 138, markers=0)
            run(0, 1, 138)

            # The former final block compiles but fails the same output check.
            old_export = export.replace(
                '            values["vProfileWorldUpdates"] = metric ? metric(21) : 0;\n', '')
            self.assertNotEqual(export, old_export)
            cpp.write_text(harness.replace(export, old_export))
            subprocess.run(["c++", "-std=c++17", "-O0", str(cpp), "-o", str(binary)], check=True)
            with self.assertRaises(AssertionError):
                run(1, 1, 138)


class ActualDisplayExport(unittest.TestCase):
    def test_production_callback_and_serializer_bind_actual_output(self):
        import subprocess
        import json
        frontend = Path(__file__).resolve().parents[2] / "tools/smoke_libretro.cpp"
        source = frontend.read_text()
        begin = source.index('    if (effect_video_field && effect_video_field(0)')
        end = source.index('    if (!data) return;', begin)
        callback = source[begin:end]
        begin = source.index('                for (unsigned i = 0; i < effect_video_frames.size(); ++i) {')
        end = source.index("                profile << ']';", begin)
        serializer = source[begin:end]
        struct_begin = source.index('struct EffectVideoFrame {')
        struct_end = source.index('\n};', struct_begin) + 3
        harness = r"""
#include <algorithm>
#include <cmath>
#include <cstring>
#include <cstdlib>
#include <sstream>
#include <stdexcept>
#include <iostream>
#include <vector>
static void fail(const char* message) { throw std::runtime_error(message); }
static unsigned meta[14]={1,7,12,1,0,144,0,143,0,0,0,0,0,7};
static unsigned display[24]={1,9,7,12,144,160,144,16,320,1,1,0x3f828f5c,0,1,0,1,0,1,11,2,0,0,0,24};
static unsigned short pixels[160*144]={};
static auto effect_video_field = +[](unsigned i){return meta[i];};
static auto effect_display_field = +[](unsigned i){return display[i];};
static auto effect_display_matches = +[](const void* p)->unsigned{return p==pixels;};
static auto effect_video_byte = +[](unsigned){return 0u;};
static auto effect_video_row = +[](unsigned,unsigned){return 0u;};
static auto effect_video_change = +[](unsigned,unsigned){return 0u;};
STRUCT
static std::vector<EffectVideoFrame> effect_video_frames;
static unsigned effect_video_last=0,effect_video_overflow=0,effect_display_last=0;
static unsigned input_frame=13,video_frames=13,effect_private_oam_page=0xd8,effect_private_oam_bank=7;
static unsigned pixel_format=2;
static constexpr unsigned RETRO_PIXEL_FORMAT_RGB565=2;
void capture(const void* data,unsigned w=160,unsigned h=144,unsigned pitch=320) {
CALLBACK
}
int main(int argc,char** argv) {
 unsigned mode=argc>1?std::atoi(argv[1]):0;
 if(mode==1)display[2]=8;
 if(mode==2)display[23]=23;
 if(mode==3)display[4]=1;
 if(mode==4)display[12]=2;
 if(mode==5)display[15]=3;
 if(mode==6)display[16]=1;
 if(mode==7)display[21]=1;
 if(mode==8)display[17]=0;
 if(mode==9)display[11]=0x7fc00000;
 try { capture(mode==10?pixels+1:pixels); } catch(const std::runtime_error&) {return 2;}
 if(effect_video_frames.size()!=1)return 3;
 if(mode==11){effect_video_last=0;try{capture(pixels);}catch(const std::runtime_error&){return 2;}return 3;}
 std::ostringstream profile;
 auto bytes=[&](const auto& data){profile<<'[';for(unsigned i=0;i<data.size();++i){if(i)profile<<',';profile<<unsigned(data[i]);}profile<<']';};
SERIALIZER
 std::cout<<profile.str();
}
"""
        harness = harness.replace('STRUCT', source[struct_begin:struct_end]).replace('CALLBACK', callback).replace('SERIALIZER', serializer)
        with tempfile.TemporaryDirectory() as tmp:
            cpp=Path(tmp)/"display-export.cpp"; binary=Path(tmp)/"display-export"
            cpp.write_text(harness)
            subprocess.run(["c++","-std=c++17","-O2",str(cpp),"-o",str(binary)],check=True)
            frame=json.loads(subprocess.check_output([str(binary)],text=True))
            self.assertEqual(frame["display_metadata"],[1,9,7,12,144,160,144,16,320,1,1,0x3f828f5c,0,1,0,1,0,1,11,2,0,0,0,24])
            self.assertEqual(frame["render_metadata"][1:3],[7,12])
            self.assertEqual(len(frame["pixels"]),23040*2)
            for mode in range(1,12):
                self.assertEqual(subprocess.run([str(binary),str(mode)],capture_output=True).returncode,2,mode)


class ObserverHooks(unittest.TestCase):
    def test_compiled_effect_observer_boundaries(self):
        import subprocess
        with tempfile.TemporaryDirectory() as tmp:
            binary = Path(tmp) / "observer"
            source = Path(__file__).with_name("test_effect_observer.cpp")
            subprocess.run(["c++", "-std=c++17", "-O2", str(source), "-o", str(binary)], check=True)
            subprocess.run([str(binary)], check=True)

    def source(self, root):
        (root / "gb_core").mkdir()
        (root / "libretro").mkdir()
        (root / "gb_core/cpu.cpp").write_text(
            "void cpu::io_write(word adr,byte dat)\n{\n}\n"
            "mobile_profile_before_instruction(ref_gb, regs, total_clock);\n"
            "mobile_profile_after_instruction(ref_gb, regs, total_clock, observed_opcode, tmp_clocks);\n")
        (root / "gb_core/lcd.cpp").write_text('#include "gb.h"\nvoid lcd::render(void *buf,int scanline)\n{\n}\n')
        (root / "libretro/libretro.cpp").write_text(
            "bool retro_load_game(const struct retro_game_info *info)\n{\n}\n"
            "void retro_reset(void)\n{\n}\n"
            "bool retro_unserialize(const void *data, size_t size)\n{\n        return true;\n}\n")

        (root / "libretro/dmy_renderer.h").write_text("virtual word map_color(word gb_col);\n")
        (root / "libretro/dmy_renderer.cpp").write_text(
            "word dmy_renderer::map_color(word gb_col)\n{\n}\n"
            "void dmy_renderer::apply_all_gb_effects_and_render(byte* buf, int width, int height, int depth, int pitch)\n{\n}\n")

    def test_supported_hooks_are_idempotent_and_missing_anchors_fail_before_write(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            self.source(root)
            runtime.extend_profile(root)
            original = {p: p.read_bytes() for p in root.rglob("*") if p.is_file()}
            runtime.extend_profile(root)
            self.assertEqual(original, {p: p.read_bytes() for p in original})
            self.assertIn("acceptance_profile_after(game, regs, opcode, cycles);",
                          (root / "gb_core/profile_core.h").read_text())
            self.assertEqual((root / "libretro/libretro.cpp").read_text().count(
                "acceptance_profile_restore_boundary();"), 2)  # declaration and successful restore
            cpu = root / "gb_core/cpu.cpp"
            cpu.write_text(cpu.read_text().replace("void cpu::io_write", "void unsupported::io_write"))
            before = {p: p.read_bytes() for p in original}
            with self.assertRaisesRegex(RuntimeError, "unique observer source anchor"):
                runtime.extend_profile(root)
            self.assertEqual(before, {p: p.read_bytes() for p in before})

    def test_missing_display_anchor_rejects_before_any_write(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp); self.source(root)
            renderer=root / "libretro/dmy_renderer.cpp"
            renderer.write_text(renderer.read_text().replace("word dmy_renderer::map_color", "word changed::map_color"))
            before={p:p.read_bytes() for p in root.rglob("*") if p.is_file()}
            with self.assertRaisesRegex(RuntimeError,"unique observer source anchor"):
                runtime.extend_profile(root)
            self.assertEqual(before,{p:p.read_bytes() for p in before})

    def test_display_declarations_preserve_initial_bom_and_other_content(self):
        for prefix in ("", "\ufeff"):
            with self.subTest(initial_bom=bool(prefix)), tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp)
                self.source(root)
                renderer = root / "libretro/dmy_renderer.cpp"
                renderer.write_text(prefix + renderer.read_text() + "// Keep embedded \ufeff content.\n")
                runtime.extend_profile(root)
                rendered = renderer.read_bytes()
                self.assertEqual(rendered.startswith(b"\xef\xbb\xbf"), bool(prefix))
                self.assertEqual(rendered.count(b"\xef\xbb\xbf"), 1 + bool(prefix))
                self.assertTrue(rendered.endswith("// Keep embedded \ufeff content.\n".encode()))
                self.assertTrue(rendered[len(prefix.encode()):].startswith(b'extern "C"'))
                before = {p: p.read_bytes() for p in root.rglob("*") if p.is_file()}
                runtime.extend_profile(root)
                self.assertEqual(before, {p: p.read_bytes() for p in before})
                renderer.write_bytes(rendered.replace(b"word dmy_renderer::map_color", b"word changed::map_color"))
                before = {p: p.read_bytes() for p in before}
                with self.assertRaisesRegex(RuntimeError, "unique observer source anchor"):
                    runtime.extend_profile(root)
                self.assertEqual(before, {p: p.read_bytes() for p in before})

    def test_missing_actual_getter_header_anchor_rejects_before_any_write(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp); self.source(root)
            header=root / "libretro/dmy_renderer.h"
            header.write_text("virtual word changed_color(word gb_col);\n")
            before={p:p.read_bytes() for p in root.rglob("*") if p.is_file()}
            with self.assertRaisesRegex(RuntimeError,"unique observer source anchor"):
                runtime.extend_profile(root)
            self.assertEqual(before,{p:p.read_bytes() for p in before})

    def test_duplicate_and_misplaced_anchors_fail(self):
        with self.assertRaises(RuntimeError):
            runtime.inject_unique("anchor anchor", "anchor", "anchor hook")
        with self.assertRaises(RuntimeError):
            runtime.inject_unique("anchor elsewhere hook", "anchor", "anchor hook")

        with self.assertRaises(RuntimeError):
            runtime.inject_unique("anchor hook elsewhere hook", "anchor", "anchor hook")

    def test_projection_retains_effect_state_and_raw_events(self):
        import gzip
        import json
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            fields = {"vAcceptanceEffectCount": 3, "vAcceptanceEffectOverflow": 0,
                      "vAcceptanceNativeSceneReady": 1,
                      **{f"vAcceptanceEffectActive{i}": i for i in range(8)}}
            profile = {"native_effect_events": [{"fields": [1, 2, 3]}],
                       "native_effect_event_count": 1, "native_effect_event_overflow": 0}
            with gzip.open(root / "trace.jsonl.gz", "wt") as stream:
                stream.write(json.dumps({"frame": 1, "symbols": fields}) + "\n")
            (root / "profile.json").write_text(json.dumps(profile))
            (root / "state.json").write_text(json.dumps({"frame_calls": 1, "symbols": fields}))
            (root / "result.sav").write_bytes(b"save")
            (root / "input-events.jsonl").write_text("")
            result = runtime.projected(root)
            self.assertEqual(result["frames"][0]["state"], fields)
            self.assertEqual(result["profile"], profile)


class Workspaces(unittest.TestCase):
    def test_rom_copy_excludes_apps_patches_and_generated_artifacts(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "source"
            files = ["Makefile", "layout.link", "main.asm", "main.o",
                     "data/maps/maps.asm", "mobile_adapter/rom/ram.asm",
                     "mobile_adapter/tools/prepare_virtual_avatars.py",
                     "mobile_adapter/build-mobile/main.o",
                     "mobile_adapter/bridge/.dart_tool/huge.dill",
                     "mobile_adapter/server/target/debug/huge.a",
                     "mobile_adapter/tests/support/observer.hpp",
                     "mobile_adapter/tests/scenarios/save.sav", "patches/old.ips"]
            for name in files:
                path = source / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(b"input")
            clean, fixture = Path(tmp) / "clean", Path(tmp) / "fixture"
            runtime.copy_sources(source, clean)
            runtime.copy_sources(source, fixture, reuse_objects=True)
            for copy in (clean, fixture):
                self.assertTrue((copy / "data/maps/maps.asm").exists())
                self.assertTrue((copy / "mobile_adapter/rom/ram.asm").exists())
                for absent in ("bridge", "server"):
                    self.assertFalse((copy / "mobile_adapter" / absent).exists())
                self.assertTrue((copy / "mobile_adapter/tests/support/observer.hpp").exists())
                self.assertFalse((copy / "mobile_adapter/tests/scenarios").exists())
                self.assertFalse((copy / "patches").exists())
            self.assertFalse((clean / "main.o").exists())
            self.assertFalse((clean / "mobile_adapter/build-mobile").exists())
            self.assertTrue((fixture / "main.o").exists())
            self.assertTrue((fixture / "mobile_adapter/build-mobile/main.o").exists())

    def test_disposable_compiler_inputs_removed_but_probe_outputs_retained(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp)
            (source / "maps").mkdir()
            (source / "maps/test.asm").write_text("input")
            for name in ("fixture.gbc", "fixture.sym", "fixture.map", "main.o", "Makefile"):
                (source / name).write_bytes(b"input")
            runtime.retain_rom_outputs(source)
            self.assertEqual(sorted(p.name for p in source.iterdir()),
                             ["fixture.gbc", "fixture.map", "fixture.sym"])


class PristineCopyright(unittest.TestCase):
    def generate(self, size, compressed_size=288):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "gfx/splash").mkdir(parents=True)
            (root / "tools").mkdir()
            (root / "Makefile").write_text("archived reference recipe")
            text = "@ 1 9 9 5 - 2 0 2 6    G A M E   F R E A K"
            (root / "gfx/splash/copyright.txt").write_text(text + "\n")
            calls = []

            def fake_run(command, cwd, log):
                calls.append(command)
                self.assertEqual(cwd, root)
                if len(calls) == 1:
                    (root / "tools/fine_print").write_bytes(b"compiled generator")
                if len(calls) == 2:
                    self.assertEqual(calls[0], ["make", "-C", "tools", "-j1", "fine_print", "lzpcompress"])
                    self.assertEqual(command, ["tools/fine_print", "-c", "013", "-s", "0",
                                               text, "gfx/splash/copyright.2bpp"])
                    (root / "gfx/splash/copyright.2bpp").write_bytes(bytes(size))
                if len(calls) == 3:
                    self.assertEqual(command, ["tools/lzpcompress", "--", "gfx/splash/copyright.2bpp",
                                               "gfx/splash/copyright.2bpp.lzp"])
                    (root / "gfx/splash/copyright.2bpp.lzp").write_bytes(bytes(compressed_size))

            with patch.object(runtime, "run", side_effect=fake_run):
                evidence = runtime.prepare_pristine_copyright(root, root)
            self.assertEqual(evidence["text"], text)
            self.assertEqual(evidence["asset_bytes"], 480)
            self.assertEqual(evidence["compressed_bytes"], 288)
            self.assertEqual(evidence["asset_sha256"], runtime.sha(root / "gfx/splash/copyright.2bpp"))
            self.assertTrue((root / "pristine-copyright.json").is_file())

    def test_dependencies_precede_generation_with_exact_text(self):
        self.generate(480)

    def test_empty_artwork_is_rejected(self):
        with self.assertRaisesRegex(RuntimeError, "288 compressed bytes"):
            self.generate(1)

    def test_empty_linked_artwork_is_rejected(self):
        with self.assertRaisesRegex(RuntimeError, "288 compressed bytes"):
            self.generate(480, compressed_size=1)


class DeclarativeConfiguration(unittest.TestCase):
    def test_opcode_or_out_of_span_patch_is_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            rom, destination = Path(tmp) / "test.gbc", Path(tmp) / "patched.gbc"
            rom.write_bytes(bytes(0x4000))
            names = {"AcceptanceConfig": (0, 0x200), "AcceptanceConfigEnd": (0, 0x219),
                     "AcceptanceAvatar": (0, 0x201), "AcceptanceBoot": (0, 0x220)}
            runtime.patch_data(rom, names, {"AcceptanceAvatar": bytes([3])}, destination)
            self.assertEqual(destination.read_bytes()[0x201], 3)
            with self.assertRaisesRegex(ValueError, "declared test configuration"):
                runtime.patch_data(rom, names, {"AcceptanceBoot": bytes([0])}, destination)
            with self.assertRaises(ValueError):
                runtime.patch_data(rom, names, {"AcceptanceAvatar": bytes([1, 2])}, destination)
            names["AcceptanceAvatar"] = (0, 0x220)
            with self.assertRaisesRegex(ValueError, "ROM span"):
                runtime.patch_data(rom, names, {"AcceptanceAvatar": bytes([1])}, destination)


class FaultDelivery(unittest.TestCase):
    def test_unmodified_welcome_is_not_counted_as_a_roster_fault(self):
        welcome = b"PC\x01" + bytes([runtime.WIRE_KINDS["WELCOME"]]) + b"\x00\x01\x00\x01\x01"
        roster = b"PC\x01" + bytes([runtime.WIRE_KINDS["ROSTER"]]) + b"\x00\x02\x00\x02\x01\x02"
        with socket.create_server(("127.0.0.1", 0)) as upstream:
            def serve():
                connection, _ = upstream.accept()
                with connection:
                    connection.sendall(welcome)
                    self.assertEqual(connection.recv(1), b"P")
                    connection.sendall(roster)
            worker = threading.Thread(target=serve)
            worker.start()
            proxy = runtime.Proxy(upstream.getsockname()[1], "roster", wait_for_arm=True)
            host = threading.Thread(target=proxy.serve_forever)
            host.start()
            try:
                with socket.create_connection(proxy.server_address, timeout=2) as client:
                    def receive(size):
                        data = bytearray()
                        while len(data) < size:
                            chunk = client.recv(size - len(data))
                            if not chunk: self.fail("Proxy closed before the requested fault")
                            data.extend(chunk)
                        return bytes(data)
                    self.assertEqual(receive(len(welcome)), welcome)
                    self.assertEqual(proxy.injected, 0)
                    proxy.armed = True
                    client.sendall(b"P")
                    self.assertEqual(receive(len(roster)), roster[:8] + bytes([255, 255]))
                    deadline = time.monotonic() + 2
                    while not proxy.injected and time.monotonic() < deadline:
                        time.sleep(.001)
                    self.assertEqual(proxy.injected, 1)
            finally:
                proxy.shutdown()
                proxy.server_close()
                host.join(timeout=2)
                worker.join(timeout=2)


class StorageReserve(unittest.TestCase):
    def make_run(self, root, name):
        run = root / name
        with active_run(run):
            (run / "evidence.json").write_text("{}")
        return run

    def test_cleanup_preserves_active_current_toolchain_and_unidentified_content(self):
        with tempfile.TemporaryDirectory() as tmp:
            cache = Path(tmp)
            old = self.make_run(cache, "acceptance-suite-old")
            current = self.make_run(cache, "acceptance-suite-current")
            toolchain = cache / "rgbds-1.0.4"
            toolchain.mkdir()
            unrelated = cache / "acceptance-suite-user-data"
            unrelated.mkdir()
            (unrelated / "save.sav").write_bytes(b"user data")
            with active_run(cache / "acceptance-suite-active"):
                deleted = ensure_free_space(cache, current, reserve=100, headroom=10,
                    free=lambda: 0 if old.exists() else 110)
            self.assertEqual(deleted, [old.resolve()])
            for path in (current, toolchain, unrelated, cache / "acceptance-suite-active"):
                self.assertTrue(path.exists())

    def test_space_failure_does_not_delete_symlink_or_active_run(self):
        with tempfile.TemporaryDirectory() as tmp:
            cache = Path(tmp) / "cache"
            cache.mkdir()
            outside = self.make_run(Path(tmp), "outside")
            (cache / "acceptance-suite-symlink").symlink_to(outside)
            with active_run(cache / "current"):
                with self.assertRaisesRegex(RuntimeError, "cannot be restored safely"):
                    ensure_free_space(cache, cache / "current", reserve=100, headroom=0, free=lambda: 0)
            self.assertTrue(outside.exists())
            self.assertTrue((cache / "current").exists())

    def test_sufficient_space_performs_no_deletion(self):
        with tempfile.TemporaryDirectory() as tmp:
            cache = Path(tmp)
            old = self.make_run(cache, "acceptance-suite-old")
            self.assertEqual(ensure_free_space(cache, cache / "current", reserve=100,
                             headroom=0, free=lambda: 100), [])
            self.assertTrue(old.exists())

class IdentityEvidence(unittest.TestCase):
    def test_negative_identity_patches_only_named_data_and_checksum(self):
        with tempfile.TemporaryDirectory() as tmp:
            rom, result = Path(tmp) / "fixture.gbc", Path(tmp) / "negative.gbc"
            original = bytes(0x4000)
            rom.write_bytes(original)
            names = {"AcceptanceConfig": (0, 0x200), "AcceptanceConfigEnd": (0, 0x220),
                     "MobileCompatibilityHash": (0, 0x300), "MobileCompatibilityHashEnd": (0, 0x320),
                     "MobileHelloPrefix": (0, 0x400), "MobileHelloVector": (0, 0x405)}
            for mismatch, changed in (("digest", 0x300), ("schema", 0x400)):
                runtime.patch_identity_fixture(rom, names, mismatch, result)
                data = result.read_bytes()
                self.assertEqual([i for i, (a, b) in enumerate(zip(original, data))
                                  if a != b and i not in (0x14e, 0x14f)], [changed])
                self.assertEqual(int.from_bytes(data[0x14e:0x150], "big"),
                                 (sum(data[:0x14e]) + sum(data[0x150:])) & 65535)
            names["MobileHelloVector"] = (0, 0x406)
            with self.assertRaisesRegex(ValueError, "data spans"):
                runtime.patch_identity_fixture(rom, names, "schema", result)
            names.pop("AcceptanceConfig")
            with self.assertRaisesRegex(ValueError, "optional test cartridge"):
                runtime.patch_identity_fixture(rom, names, "digest", result)

    def test_proxy_captures_fragmented_native_hello_and_delivered_error(self):
        hello = b"PC\x01\x10\x00\x01\x00\x2f" + bytes([5, 0, 4, 0, 1]) + bytes(42)
        error = b"PC\x01\x16\x00\x01\x00\x01\x02"
        with socket.create_server(("127.0.0.1", 0)) as upstream:
            def serve():
                connection, _ = upstream.accept()
                with connection:
                    observed = bytearray()
                    while len(observed) < len(hello):
                        observed.extend(connection.recv(128))
                    self.assertEqual(bytes(observed), hello)
                    connection.sendall(error)
            worker = threading.Thread(target=serve)
            worker.start()
            proxy = runtime.Proxy(upstream.getsockname()[1], "none", identity="digest")
            host = threading.Thread(target=proxy.serve_forever)
            host.start()
            try:
                with socket.create_connection(proxy.server_address, timeout=2) as client:
                    client.sendall(hello[:7])
                    client.sendall(hello[7:19])
                    client.sendall(hello[19:])
                    received = bytearray()
                    while len(received) < len(error):
                        received.extend(client.recv(128))
                    self.assertEqual(bytes(received), error)
                worker.join(timeout=2)
                self.assertFalse(worker.is_alive())
                self.assertEqual([(row["direction"], row["frame"]) for row in proxy.wire],
                                 [("native-to-bridge", hello.hex()), ("bridge-to-native", error.hex())])
                for _ in range(1000):
                    proxy.observe(0, "bridge-to-native", error)
                self.assertEqual(len(proxy.wire), 5)
            finally:
                proxy.shutdown()
                proxy.server_close()
                host.join(timeout=2)

    def test_held_rejection_preserves_pending_native_socket_after_bridge_eof(self):
        error = b"PC\x01\x16\x00\x01\x00\x01\x02"
        with socket.create_server(("127.0.0.1", 0)) as upstream:
            def serve():
                connection, _ = upstream.accept()
                with connection:
                    connection.sendall(error)
            worker = threading.Thread(target=serve)
            worker.start()
            proxy = runtime.Proxy(upstream.getsockname()[1], "none", identity="digest-held")
            host = threading.Thread(target=proxy.serve_forever)
            host.start()
            try:
                with socket.create_connection(proxy.server_address, timeout=2) as client:
                    deadline = time.monotonic() + 2
                    while not proxy.wire and time.monotonic() < deadline:
                        time.sleep(.001)
                    self.assertEqual(proxy.wire[0]["frame"], error.hex())
                    worker.join(timeout=2)
                    self.assertFalse(worker.is_alive())
                    client.settimeout(.1)
                    with self.assertRaises(socket.timeout):
                        client.recv(1)
            finally:
                proxy.shutdown()
                proxy.server_close()
                host.join(timeout=2)


if __name__ == "__main__":
    unittest.main()
