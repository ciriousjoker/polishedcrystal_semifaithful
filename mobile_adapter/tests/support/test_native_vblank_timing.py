"""Focused host counterexamples, not native P1/P2 acceptance."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class NativeVBlankTimingTests(unittest.TestCase):
    def test_standalone_counterexamples(self):
        support = Path(__file__).resolve().parent
        scratch = os.environ.get("TMPDIR", "/Volumes/Storage/tmp")
        with tempfile.TemporaryDirectory(prefix="vblank-timing-95-", dir=scratch) as directory:
            binary = Path(directory) / "test"
            env = dict(os.environ, TMPDIR=scratch)
            subprocess.run(["clang++", "-std=c++17", "-Wall", "-Wextra", "-Werror",
                            str(support / "test_native_vblank_timing.cpp"), "-o", str(binary)],
                           check=True, env=env, capture_output=True, text=True)
            subprocess.run([str(binary)], check=True, env=env, capture_output=True, text=True)

class NativeTimingRunnerConfiguration(unittest.TestCase):
    def test_production_cli_guard_enabled_disabled_and_restore_boundaries(self):
        support = Path(__file__).resolve().parent
        frontend = support.parents[1] / "tools/smoke_libretro.cpp"
        source = frontend.read_text()
        start = source.index('    const bool timing_requested = arguments.count("--native-vblank-timing");')
        end = source.index('    const bool disabled_reference =', start)
        guard = source[start:end]
        harness = r'''
#include <map>
#include <vector>
#include <string>
#include <stdexcept>
#include <cassert>
static void fail(const std::string& s) { throw std::runtime_error(s); }
static void validate(std::map<std::string,std::string> arguments, unsigned fields=8) {
 std::map<std::string,std::vector<std::string>> effect_options;
 effect_options["--native-scene-field"].resize(fields);
''' + guard + r'''
}
int main() {
 std::map<std::string,std::string> args={{"--native-vblank-timing","out"},{"--frames","1400"},
 {"--symbols","fixture.sym"},{"--state-out","state"},{"--profile-out","profile"},
 {"--acceptance","1"},{"--adapter","0"},{"--profile-window-start","0"}};
 validate(args);
 auto reject=[&](auto a,unsigned fields=8){bool failed=false;try{validate(a,fields);}catch(const std::runtime_error&){failed=true;}assert(failed);};
 reject(args,7);auto a=args;a.erase("--frames");reject(a);
 a=args;a["--snapshot-in"]="state";reject(a);
 a=args;a["--profile-window-start"]="3600";reject(a);
 a=args;a["--measurement-gate"]="gate";reject(a);
 args["--adapter"]="1";reject(args);
 args["--measurement-gate"]="gate";validate(args);
 a=args;a["--expected-peers"]="2";reject(a);
 a=args;a["--effect-video"]="1";reject(a);
 validate({});
}
'''
        scratch = os.environ.get("TMPDIR", "/Volumes/Storage/tmp")
        with tempfile.TemporaryDirectory(prefix="vblank-wiring-111-", dir=scratch) as directory:
            cpp = Path(directory) / "test.cpp"
            cpp.write_text(harness)
            binary = Path(directory) / "test"
            subprocess.run(["clang++", "-std=c++17", "-Wall", "-Wextra", "-Werror", str(cpp), "-o", str(binary)], check=True, capture_output=True, text=True)
            subprocess.run([str(binary)], check=True, capture_output=True, text=True)


class NativeTimingLoadLifecycle(unittest.TestCase):
    def test_configure_before_generated_load_and_repeated_reset(self):
        import runtime
        from test_runtime import ObserverHooks
        support = Path(__file__).resolve().parent
        runner = (support.parents[1] / "tools/smoke_libretro.cpp").read_text()
        config = runner.index('if (!config(mode.second,anims.second,footprint.second')
        load = runner.index('if (!symbol<bool(*)(const retro_game_info*)>(core, "retro_load_game")(&game))')
        arm = runner.index('if (!symbol<unsigned(*)()>(core, "acceptance_profile_timing_arm")())')
        self.assertLess(config, load)
        self.assertLess(load, arm)
        self.assertEqual(runner.count('"acceptance_profile_timing_config"'), 1)
        scratch = os.environ.get("TMPDIR", "/Volumes/Storage/tmp")
        with tempfile.TemporaryDirectory(prefix="vblank-lifecycle-115-", dir=scratch) as directory:
            root = Path(directory)
            fixture = root / "fixture"
            fixture.mkdir()
            ObserverHooks().source(fixture)
            runtime.extend_profile(fixture)
            injected = (fixture / "libretro/libretro.cpp").read_text()
            def entry(signature):
                start = injected.index(signature)
                end = injected.index('\n}', start) + 2
                return injected[start:end]
            load_body = entry('bool retro_load_game(')
            reset_body = entry('void retro_reset(')
            self.assertEqual(load_body, 'bool retro_load_game(const struct retro_game_info *info)\n{\n    acceptance_profile_effect_reset();\n}')
            self.assertEqual(reset_body, 'void retro_reset(void)\n{\n    acceptance_profile_effect_reset();\n}')
            load_body = load_body[:-1] + '    (void)info; return true;\n}'
            mock = (support / "test_effect_observer.cpp").read_text()
            preamble = mock[:mock.index('int main(){')]
            harness = preamble + '\nstruct retro_game_info {};\n' + load_body + '\n' + reset_body + r"""
int main() {
 gb g; cpu_regs r; r.PC=0x200; r.SP=0xc100;
 // Mode off stays off across the actual generated lifecycle callbacks.
 assert(retro_load_game(nullptr)); retro_reset();
 acceptance_profile_before(&g,r); acceptance_profile_after(&g,r,0,4);
 assert(!acceptance_profile_timing_status(0)&&!acceptance_profile_timing_status(6));
 assert(acceptance_profile_timing_point(0,0x1000,1)==1);
 for(unsigned i=0;i<8;++i) assert(acceptance_profile_timing_field(i,0xff90+i));
 assert(acceptance_profile_timing_config(0xff98,0xff99,0xc800,0xffa0,0,0x200,0x200,0,0));
 acceptance_profile_simulation(2,0x4400,1,0xc000,34,1,10);
 for(unsigned i=0;i<8;++i) {
   acceptance_profile_native_scene_field(i,0,0xc200+i,i==4||i==6||i==7?~0u:i,255);
   g.cpu.memory[0xc200+i]=i;
 }
 g.cpu.memory[0xc204]=1;
 acceptance_profile_native_scene_field(5,0,0xc205,0,255);g.cpu.memory[0xc205]=0;
 acceptance_profile_native_scene_enable(8);
 auto ready=[&](){
   g.mbc.mapping=g.rom.data+0x4000;r.PC=0x4400;r.SP=0xc100;
   stack(g,r.SP,0x4700);acceptance_profile_before(&g,r);
   r.PC=0x4410;acceptance_profile_before(&g,r);
   r.PC=0x4700;r.SP=0xc102;acceptance_profile_after(&g,r,0xc9,16);
   assert(acceptance_profile_native_scene_ready());
   g.mbc.mapping=g.rom.data;
 };
 auto dispatch=[&](){
   r.PC=0x200;r.SP=0xc100;auto pre=acceptance_timing_sample(&g,r);
   r.PC=0x40;r.SP=0xc0fe;stack(g,r.SP,0x200);
   acceptance_timing_irq(&g,pre,r,0x200);
 };
 auto ret=[&](unsigned pc,unsigned sp,unsigned opcode){
   acceptance_profile_before(&g,r);r.PC=pc;r.SP=sp;g.cpu.clock+=16;
   acceptance_profile_after(&g,r,opcode,16);
 };
 auto fresh=[&](){
   assert(acceptance_profile_timing_status(0));
   assert(acceptance_timing_point_count==1&&acceptance_timing_points[0].pc==0x1000);
   for(unsigned i=0;i<8;++i)assert(acceptance_timing_pending[i]==0xff90+i);
   assert(acceptance_timing_mode_address==0xff98&&acceptance_timing_map_anims==0xff99);
   assert(acceptance_timing_footprint==0xc800&&acceptance_timing_hram==0xffa0);
   assert(acceptance_timing_dma_bank==0&&acceptance_timing_dma_return==0x200);
   assert(acceptance_timing_vblank_return==0x200);
   assert(!acceptance_profile_timing_point(0,0x1200,1));
   assert(!acceptance_profile_timing_field(0,0xff80));
   assert(!acceptance_profile_timing_status(1)&&!acceptance_profile_timing_status(2));
   assert(!acceptance_profile_timing_status(3)&&!acceptance_profile_timing_status(4));
   assert(!acceptance_profile_timing_status(5)&&!acceptance_profile_timing_status(6));
   assert(!acceptance_profile_timing_status(7)&&!acceptance_profile_timing_status(8));
   assert(!acceptance_profile_timing_event(~0u,6)&&!acceptance_profile_native_scene_ready());
   assert(acceptance_profile_scene_diagnostic(0)&&acceptance_profile_scene_diagnostic(1)==8);
   for(unsigned i=7;i<12;++i)assert(!acceptance_profile_scene_diagnostic(i));
   for(unsigned i=0;i<6;++i)assert(!acceptance_profile_scene_snapshot(i,0));
   assert(!acceptance_profile_timing_arm()); // Context was reset too.
   // Boot callbacks and a pre-arm IRQ earn no timing events or readiness.
   dispatch();ret(0x200,0xc100,0xd9);
   assert(!acceptance_profile_timing_status(6)&&!acceptance_profile_native_scene_ready());
   ready();assert(acceptance_profile_timing_arm());
   assert(!acceptance_profile_timing_arm());
 };
 assert(retro_load_game(nullptr));fresh();
 // Start a genuine IRQ/job and DMA wait, then reset before it can complete.
 dispatch();r.PC=0x1000;r.SP=0xc0fc;stack(g,r.SP,0x200);
 ret(0xffa0,0xc0fc,0xc3);
 const unsigned char bytes[]={0xe2,0x05,0x20,0xfd,0xc9};
 for(unsigned i=0;i<5;++i)g.cpu.memory[0xffa0+i]=bytes[i];
 acceptance_profile_before(&g,r);g.cpu.regs=r;
 acceptance_profile_effect_io_write(&g,0xff46,0xc0);
 r.PC=0xffa1;g.cpu.clock+=8;acceptance_profile_after(&g,r,0xe2,8);
 assert(acceptance_profile_timing_status(6)==3);
 retro_reset();fresh();
 // Leftover HRAM and RETI returns cannot close a pre-reset scope.
 r.PC=0xffa4;r.SP=0xc0fc;ret(0x200,0xc0fe,0xc9);
 ret(0x200,0xc100,0xd9);assert(!acceptance_profile_timing_status(5));
 dispatch();r.PC=0x1000;r.SP=0xc0fc;stack(g,r.SP,0x200);
 ret(0xffa0,0xc0fc,0xc3);
 acceptance_profile_before(&g,r);g.cpu.regs=r;
 acceptance_profile_effect_io_write(&g,0xff46,0xc0);
 r.PC=0xffa1;g.cpu.clock+=8;acceptance_profile_after(&g,r,0xe2,8);
 r.PC=0xffa4;ret(0x200,0xc0fe,0xc9);
 assert(acceptance_timing.event(3)->kind==native_vblank_timing::Kind::dma_wait_complete);
 assert(acceptance_timing.event(4)->kind==native_vblank_timing::Kind::job_return);
 ret(0x200,0xc100,0xd9);assert(acceptance_profile_timing_status(5)==1);
 // Within-experiment failure stays sticky through arm, reads and restore.
 dispatch();g.cpu.memory[0xc800]=1;acceptance_profile_before(&g,r);
 assert(acceptance_profile_timing_status(3));auto witness=acceptance_profile_timing_event(~0u,6);
 assert(!acceptance_profile_timing_arm());acceptance_profile_restore_boundary();
 assert(!acceptance_profile_timing_arm()&&acceptance_profile_timing_status(3));
 assert(acceptance_profile_timing_event(~0u,6)==witness);
 g.cpu.memory[0xc800]=0;retro_reset();fresh();
 dispatch();ret(0x200,0xc100,0xd9);assert(acceptance_profile_timing_status(5)==1);
 acceptance_profile_restore_boundary();assert(!acceptance_profile_timing_arm());
 assert(acceptance_profile_timing_status(4)==unsigned(native_vblank_timing::Failure::restored));
 std::puts("PASS configure before generated load/reset, immutable bindings, fresh readiness/IRQ/DMA and sticky failures");
}
"""
            cpp = root / "lifecycle.cpp"
            cpp.write_text(harness)
            binary = root / "lifecycle"
            subprocess.run(["clang++", "-std=c++17", "-Wall", "-Wextra", "-Werror", "-Wno-unused-function",
                            "-I", str(support), str(cpp), "-o", str(binary)], check=True, capture_output=True, text=True)
            result = subprocess.run([str(binary)], capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


class NativeSceneReadinessWitness(unittest.TestCase):
    def test_runner_never_armed_durably_exports_json_and_exact_gates(self):
        import json
        support = Path(__file__).resolve().parent
        runner = (support.parents[1] / "tools/smoke_libretro.cpp").read_text()
        start = runner.index('    auto timing_prearm_snapshot = [&]()')
        end = runner.index('    auto align_raster =', start)
        snapshot = runner[start:end]
        schema = runner.index('        timing << "{\\"schema\\":\\"native-vblank-timing-v1')
        start = runner.rindex('    if (timing_requested) {', 0, schema)
        end = runner.index('    if (metric) {', schema)
        export = runner[start:end]
        start = runner.index('            const bool scene =', runner.index('if (timing_requested && !timing_armed)'))
        end = runner.index('            if (ready)', start)
        gate = runner[start:end]
        harness = r"""
#include <cassert>
#include <cstdint>
#include <fstream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
static unsigned scene_ready=1,metric21=1;
static unsigned scene(){return scene_ready;}
template<typename T> T symbol(void*,const char*) {return reinterpret_cast<T>(&scene);}
static void fail(const char* message){throw std::runtime_error(message);}
int main(int argc,char** argv){
 assert(argc==2); void* core=nullptr;
 auto metric=+[](unsigned){return uint64_t(metric21);};
 bool adapter_enabled=false,measurement_gate=false,measurement_released=false,measurement_ready=false;
 std::map<std::string,unsigned> values={{"wMapStatus",2},{"wScriptRunning",0},{"wMobileOnline",1},{"wMobilePeerCount",3}};
 auto gate=[&](){
""" + gate + r"""
 return ready;
 };
 assert(gate());
 scene_ready=0;assert(!gate());scene_ready=1;
 metric21=0;assert(!gate());metric21=1;
 values["wMapStatus"]=1;assert(!gate());values["wMapStatus"]=2;
 values["wScriptRunning"]=1;assert(!gate());values["wScriptRunning"]=0;
 adapter_enabled=true;assert(!gate());measurement_released=true;assert(!gate());measurement_ready=true;
 assert(gate());values["wMobileOnline"]=0;assert(!gate());values["wMobileOnline"]=1;
 values["wMobilePeerCount"]=2;assert(!gate());values["wMobilePeerCount"]=3;
 // Never armed output still contains the independent false metric operand.
 metric21=0;scene_ready=0;adapter_enabled=false;
 bool timing_requested=true,timing_armed=false;
 unsigned timing_arm_frame=0,timing_done_frame=0,input_frame=1400,measurement_start=0;
 std::string timing_prearm_load,timing_prearm_arm;
 auto timing_status=+[](unsigned f)->uint64_t{return f==0 ? 1 : 0;};
 auto timing_event=+[](unsigned,unsigned)->uint64_t{return 0;};
 auto scene_diagnostic=+[](unsigned f)->uint64_t{return f==1 ? 8 : 0;};
 auto scene_field=+[](unsigned i,unsigned f)->uint64_t{return i*10+f;};
 auto scene_snapshot=+[](unsigned,unsigned)->uint64_t{return 0;};
 std::vector<std::pair<std::string,std::pair<unsigned,unsigned>>> timing_bindings;
 std::map<std::string,std::string> arguments={{"--native-vblank-timing",argv[1]}};
 void* sram=nullptr;size_t sram_size=0;
 auto capture=[&](void*,size_t){return values;};
""" + snapshot + r"""
 try {
""" + export + r"""
 assert(false);
 } catch(const std::runtime_error& error) {
 assert(std::string(error.what())=="Incomplete or invalid bounded native timing evidence");
 }
}
"""
        scratch = os.environ.get("TMPDIR", "/Volumes/Storage/tmp")
        with tempfile.TemporaryDirectory(prefix="prearm-json-135-", dir=scratch) as directory:
            cpp = Path(directory) / "runner.cpp"
            cpp.write_text(harness)
            binary = Path(directory) / "runner"
            result = subprocess.run(["clang++", "-std=c++17", "-Wall", "-Wextra", "-Werror", str(cpp), "-o", str(binary)], capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            output = Path(directory) / "timing.json"
            result = subprocess.run([str(binary),str(output)], capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            data = json.loads(output.read_text())
            self.assertEqual(data["status"], [1]+[0]*12)
            self.assertEqual(data["events"], [])
            self.assertEqual(data["first_failure_sample"], [0]*17)
            self.assertIsNone(data["prearm"]["before_arm"])
            self.assertEqual(data["prearm"]["final_live"]["host_frame"],1400)
            self.assertEqual(data["prearm"]["final_live"]["metric21"],0)
            self.assertEqual(len(data["prearm"]["boundaries"]),6)
            self.assertEqual(data["prearm"]["final_live"]["live_fields"][7],[70+i for i in range(7)])

    def test_real_boundaries_pure_getters_fixed_banks_and_reset(self):
        support = Path(__file__).resolve().parent
        mock = (support / "test_effect_observer.cpp").read_text()
        harness = mock[:mock.index('int main(){')] + r"""
int main() {
 gb g; cpu_regs r; r.PC=0x200; r.SP=0xc100;
 acceptance_profile_simulation(2,0x4400,1,0xc000,34,1,10);
 for(unsigned i=0;i<8;++i) {
   acceptance_profile_native_scene_field(i,1,0xd200+i,i,255);
   g.cpu.ram[0x1200+i]=i;g.cpu.memory[0xd200+i]=99;
 }
 acceptance_profile_native_scene_enable(8);
 auto entry=[&](){
   g.mbc.mapping=g.rom.data+0x4000;r.PC=0x4400;r.SP=0xc100;
   stack(g,r.SP,0x4700);acceptance_profile_before(&g,r);
 };
 auto ret=[&](unsigned pc,unsigned sp){
   r.PC=0x4410;acceptance_profile_before(&g,r);
   r.PC=pc;r.SP=sp;acceptance_profile_after(&g,r,0xc9,16);
 };
 // Real callbacks cannot create witness records with the option off.
 entry();ret(0x4700,0xc102);
 assert(!acceptance_profile_scene_diagnostic(0));
 assert(!acceptance_profile_scene_diagnostic(7));
 assert(!acceptance_profile_scene_snapshot(0,0));
 assert(acceptance_profile_timing_point(0,0x1000,1)==1);
 for(unsigned i=0;i<8;++i) assert(acceptance_profile_timing_field(i,0xff90+i));
 assert(acceptance_profile_timing_config(0xff98,0xff99,0xc800,0xffa0,0,0x200,0x200,0,0));
 acceptance_profile_effect_reset();
 assert(acceptance_profile_scene_diagnostic(0)&&acceptance_profile_scene_diagnostic(1)==8);
 assert(!acceptance_profile_scene_diagnostic(13));
 assert(acceptance_profile_scene_field(0,5)==~uint64_t(0));
 assert(!acceptance_profile_scene_diagnostic(7));
 // Reading matching memory cannot invent completion or readiness.
 r.PC=0x200;acceptance_profile_before(&g,r);
 for(unsigned n=0;n<3;++n) {
   assert(!acceptance_profile_scene_diagnostic(12));
   for(unsigned i=0;i<8;++i) assert(acceptance_profile_scene_field(i,5)==i);
   assert(!acceptance_profile_scene_diagnostic(2)&&!acceptance_profile_scene_diagnostic(3));
 }
 mobile_observed_frames=20;entry();
 assert(acceptance_profile_scene_diagnostic(7)==1);
 assert(acceptance_profile_scene_snapshot(0,3)==0x4400);
 assert(acceptance_profile_scene_snapshot(0,8)==0xc100);
 assert(acceptance_profile_scene_snapshot(0,11)==0x4700);
 assert(acceptance_profile_scene_snapshot(0,19+7)==7);
 // Wrong bank and stack/PC produce an honest candidate, no match.
 r.PC=0x4410;acceptance_profile_before(&g,r);
 g.mbc.mapping=g.rom.data+0x8000;r.PC=0x4800;r.SP=0xc103;
 acceptance_profile_after(&g,r,0xc9,16);
 assert(acceptance_profile_scene_diagnostic(8)==1&&!acceptance_profile_scene_diagnostic(9));
 assert(acceptance_profile_scene_snapshot(2,13)==14);
 assert(!acceptance_profile_scene_diagnostic(2));
 // New genuine entry, then a failed eighth initial predicate at exact return.
 g.mbc.mapping=g.rom.data+0x4000;mobile_observed_frames=21;entry();
 assert(acceptance_profile_scene_diagnostic(11)==1);
 g.cpu.ram[0x1207]=8;ret(0x4700,0xc102);
 assert(acceptance_profile_scene_diagnostic(9)==1);
 assert(!acceptance_profile_scene_diagnostic(2)&&!acceptance_profile_scene_diagnostic(3));
 assert(acceptance_profile_scene_diagnostic(10)==1);
 assert(acceptance_profile_scene_snapshot(3,13)==0);
 assert(acceptance_profile_scene_snapshot(3,17)==128);
 assert(acceptance_profile_scene_snapshot(3,26)==8);
 assert(acceptance_profile_scene_snapshot(4,26)==8);
 // Frozen boundary values survive later live mutations and getter calls.
 g.cpu.ram[0x1207]=7;
 assert(!acceptance_profile_scene_diagnostic(12));
 assert(acceptance_profile_scene_field(7,6)==7);
 assert(acceptance_profile_scene_snapshot(3,26)==8);
 assert(acceptance_profile_scene_snapshot(0,1)==20);
 assert(acceptance_profile_scene_snapshot(1,1)==21);
 assert(acceptance_profile_scene_snapshot(2,13)==14);
 assert(!acceptance_profile_scene_diagnostic(2));
 // All eight initial gates still apply independently.
 for(unsigned i=0;i<8;++i) {
   acceptance_profile_effect_reset();entry();g.cpu.ram[0x1200+i]=99;
   ret(0x4700,0xc102);assert(!acceptance_profile_native_scene_ready());
   assert(acceptance_profile_scene_diagnostic(12)==(1u<<i));g.cpu.ram[0x1200+i]=i;
 }
 acceptance_profile_effect_reset();entry();ret(0x4700,0xc102);
 assert(acceptance_profile_native_scene_ready());
 assert(acceptance_profile_scene_snapshot(2,14)&&acceptance_profile_scene_snapshot(2,15));
 // Existing ready-state transient latch remains unchanged; live mask reports it.
 g.cpu.ram[0x1207]=99;r.PC=0x4700;acceptance_profile_before(&g,r);
 assert(acceptance_profile_native_scene_ready()&&acceptance_profile_scene_diagnostic(12)==128);
 g.cpu.ram[0x1200]=99;acceptance_profile_before(&g,r);
 assert(!acceptance_profile_native_scene_ready()&&!acceptance_profile_scene_diagnostic(3));
 assert(acceptance_profile_scene_diagnostic(10)==1);
 const auto boundary=acceptance_profile_scene_snapshot(2,26);
 acceptance_profile_restore_boundary();assert(acceptance_profile_scene_diagnostic(5));
 assert(acceptance_profile_scene_snapshot(2,26)==boundary);
 assert(acceptance_profile_timing_status(3));
 acceptance_profile_effect_reset();
 assert(acceptance_profile_scene_diagnostic(0)&&acceptance_profile_scene_diagnostic(1)==8);
 for(unsigned i=7;i<12;++i)assert(!acceptance_profile_scene_diagnostic(i));
 for(unsigned i=0;i<6;++i)assert(!acceptance_profile_scene_snapshot(i,0));
 assert(!acceptance_profile_scene_diagnostic(2)&&!acceptance_profile_scene_diagnostic(3));
 assert(!acceptance_profile_timing_status(3));
 std::puts("PASS bounded real entry/return, mismatch, fixed-bank live vs frozen, pure getters, all gates and reset");
}
"""
        scratch = os.environ.get("TMPDIR", "/Volumes/Storage/tmp")
        with tempfile.TemporaryDirectory(prefix="scene-witness-135-", dir=scratch) as directory:
            cpp = Path(directory) / "witness.cpp"
            cpp.write_text(harness)
            binary = Path(directory) / "witness"
            result = subprocess.run(["clang++", "-std=c++17", "-Wall", "-Wextra", "-Werror", "-Wno-unused-function",
                                     "-I", str(support), str(cpp), "-o", str(binary)], capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            result = subprocess.run([str(binary)], capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
