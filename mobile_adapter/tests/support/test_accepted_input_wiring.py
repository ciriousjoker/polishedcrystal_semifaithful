"""Actual profile wrapper and generated load/reset accepted-input regression."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

SUPPORT = Path(__file__).resolve().parent


class AcceptedInputWiring(unittest.TestCase):
    def test_runner_guard_and_preload_configuration(self):
        runner = (SUPPORT.parents[1] / 'tools/smoke_libretro.cpp').read_text()
        start = runner.index('    const bool accepted_inputs_requested =')
        end = runner.index('    const bool timing_requested =', start)
        self.assertLess(runner.index('"acceptance_profile_input_config"'),
                        runner.index('if (!symbol<bool(*)(const retro_game_info*)>(core, "retro_load_game")(&game))'))
        source = r'''#include <map>
#include <vector>
#include <string>
#include <stdexcept>
#include <cassert>
static void fail(const std::string& s){throw std::runtime_error(s);}
static void check(std::map<std::string,std::string> arguments,unsigned fields=8) {
std::map<std::string,std::vector<std::string>> effect_options;
effect_options["--native-scene-field"].resize(fields);
''' + runner[start:end] + r'''}
int main(){
std::map<std::string,std::string> a={{"--accepted-ordinary-inputs","out"},{"--static-intent-sources","sources"},
{"--symbols","symbols"},{"--profile-out","profile"},{"--frames","6000"},{"--acceptance","1"},{"--adapter","1"},{"--input","input"}};
check(a);check({});
for(const char* key:{"--native-vblank-timing","--disabled-native-reference","--ordinary-stream","--snapshot-in","--snapshot-out","--test-snapshot-out","--test-snapshot-roundtrip","--native-headbutt-control","--effect-video","--measurement-gate","--expected-online","--expected-peers"}) {
a[key]="1";bool rejected=false;try{check(a);}catch(const std::runtime_error&){rejected=true;}assert(rejected);a.erase(key);
}
for(const char* key:{"--static-intent-sources","--symbols","--profile-out","--frames","--acceptance","--adapter","--input"}) {
auto b=a;b.erase(key);bool rejected=false;try{check(b);}catch(const std::runtime_error&){rejected=true;}assert(rejected);
}
for(unsigned n:{0u,7u,9u}){bool rejected=false;try{check(a,n);}catch(const std::runtime_error&){rejected=true;}assert(rejected);}
}
'''
        with tempfile.TemporaryDirectory(dir=os.environ.get('TMPDIR')) as directory:
            root=Path(directory)
            (root/'guard.cpp').write_text(source)
            subprocess.run(['c++','-std=c++17','-Wall','-Wextra','-Werror',str(root/'guard.cpp'),'-o',str(root/'guard')],check=True)
            subprocess.run([str(root/'guard')],check=True)

    def test_generated_lifecycle_and_original_instruction_wrappers(self):
        import runtime
        from test_runtime import ObserverHooks
        with tempfile.TemporaryDirectory(dir=os.environ.get('TMPDIR')) as directory:
            root = Path(directory)
            fixture = root / 'fixture'
            fixture.mkdir()
            ObserverHooks().source(fixture)
            runtime.extend_profile(fixture)
            self.assertEqual((fixture / 'gb_core/accepted_ordinary_input.hpp').read_bytes(),
                             (SUPPORT / 'accepted_ordinary_input.hpp').read_bytes())
            injected = (fixture / 'libretro/libretro.cpp').read_text()
            def entry(signature):
                start = injected.index(signature)
                return injected[start:injected.index('\n}', start) + 2]
            load = entry('bool retro_load_game(')
            reset = entry('void retro_reset(')
            self.assertEqual(load.count('acceptance_profile_effect_reset();'), 1)
            self.assertEqual(reset.count('acceptance_profile_effect_reset();'), 1)
            load = load[:-1] + '(void)info; return true;\n}'
            preamble = (SUPPORT / 'test_effect_observer.cpp').read_text().split('int main(){')[0]
            runner = (SUPPORT.parents[1] / 'tools/smoke_libretro.cpp').read_text()
            export_start = runner.index('        if (input_status) {\n', runner.index('        run();'))
            export_end = runner.index('        if (static_started) {', export_start)
            export_fixture = r'''
#include <fstream>
#include <sstream>
#include <stdexcept>
static void fail(const std::string& message) {throw std::runtime_error(message);}
static void export_inputs(std::ostream& input_log, unsigned input_events_written=0) {
 const auto input_status=&acceptance_profile_input_status;
 const auto input_event=&acceptance_profile_input_event;
 const auto input_drain=&acceptance_profile_input_drain;
 bool input_ready_written=false;const unsigned input_frame=0;
''' + runner[export_start:export_end] + '}\n'
            source = preamble + export_fixture + '\nstruct retro_game_info {};\n' + load + '\n' + reset + r'''
static gb g; static cpu_regs r;
static void tick(unsigned pc) {
 r.PC=pc;r.SP=0xc100;acceptance_profile_before(&g,r);
 acceptance_profile_after(&g,r,0,4);
}
static void input(unsigned i) {tick(0x4100+i*0x10);}
static void setframe(const std::vector<unsigned char>& f) {
 for(unsigned i=0;i<f.size();++i)g.cpu.ram[0x2000+i]=f[i];
 g.cpu.memory[0xc300]=f.size();
}
static void welcome() {
 std::vector<unsigned char> f={0x50,0x43,1,17,0,1,0,15,0,1,0,2,0,1,1,2,3,4,5,6,7,8,4};
 setframe(f);input(0);g.cpu.memory[0xc302]=1;input(1);
 assert(acceptance_input.online&&!acceptance_profile_input_status(3));
}
static std::vector<unsigned char> frame(unsigned n,unsigned kind=19,unsigned revision=1) {
 std::vector<unsigned char> f={0x50,0x43,1,(unsigned char)kind,0,1,0,(unsigned char)(5+n*17),0,1,0,2,(unsigned char)n};
 for(unsigned i=0;i<n;++i) {
  const unsigned char p[]={0,(unsigned char)(10+i),0,1,0,(unsigned char)revision,1,2,0,3,0,4,0x12,0x34,0x56,0x21,(unsigned char)i};
  f.insert(f.end(),p,p+17);
 }
 return f;
}
static void slots(unsigned n,unsigned revision=1) {
 // Literal independent expansion of the wire above, no observer parser.
 for(unsigned i=0;i<n;++i) {
  const unsigned char p[]={0,(unsigned char)(10+i),0,1,0,(unsigned char)revision,1,2,0,3,0,4,1,0x23,4,0x56,2,1,(unsigned char)i,0};
  std::copy_n(p,20,g.cpu.ram+0x2400+i*64);
 }
}
static void arm() {
 retro_reset();assert(acceptance_profile_input_status(1));
 assert(!acceptance_profile_input_status(2)&&!acceptance_profile_input_status(5));
 assert(!acceptance_profile_input_point(0,1,0x4100,0));
 g.mbc.mapping=g.rom.data;g.cpu.memory[0xc302]=0;
 g.cpu.memory[0xc310]=1;g.cpu.memory[0xc311]=2;g.cpu.memory[0xc312]=2;g.cpu.memory[0xc313]=0;
 tick(0x200);assert(!acceptance_static_intent.instruction&&!acceptance_profile_input_status(5));
 g.mbc.mapping=g.rom.data; r.PC=0x4600;r.SP=0xc100;stack(g,r.SP,0x4700);
 acceptance_profile_before(&g,r);r.PC=0x4700;r.SP=0xc102;acceptance_profile_after(&g,r,0xc9,16);
 assert(acceptance_profile_static_status(6));assert(acceptance_profile_static_enable());
 r.PC=0x4500;r.SP=0xc100;stack(g,r.SP,0x4520);
 acceptance_profile_before(&g,r);r.PC=0x4520;r.SP=0xc102;acceptance_profile_after(&g,r,0xc9,16);
 tick(0x4540);assert(acceptance_profile_input_status(2));
 assert(!acceptance_profile_input_status(3));
 assert(acceptance_profile_input_event(0,0,13)==0); // prejoin empty membership retained
 assert(acceptance_profile_input_event(0,0,9)==1&&acceptance_profile_input_event(0,0,10)>0);
 assert(acceptance_profile_input_drain(acceptance_profile_input_status(5)));
}
static void commit(unsigned n,unsigned kind=19,unsigned revision=1) {
 setframe(frame(n,kind,revision));g.cpu.memory[0xc301]=0;input(2);
 assert(acceptance_input.pending);
 tick(0x4200);tick(0x4200); // staged incomplete pumps earn no acceptance
 slots(n,revision);input(3);
 assert(!acceptance_profile_input_status(3));
 assert(acceptance_profile_input_drain(acceptance_profile_input_status(5)));
}
int main() {
 (void)&acceptance_timing_irq; (void)&acceptance_profile_effect_io_write;
 assert(retro_load_game(nullptr));tick(0x200);assert(!acceptance_profile_input_status(1));
 for(unsigned i=0;i<12;++i)assert(acceptance_profile_input_point(i,1,0x4100+i*0x10,0));
 const unsigned banks[]={2,0,0,2,0},addresses[]={0xd000,0xc300,0xc301,0xd400,0xc302};
 for(unsigned i=0;i<5;++i)assert(acceptance_profile_input_field(i,banks[i],addresses[i]));
 for(unsigned i=5;i<17;++i)assert(acceptance_profile_input_field(i,0,0xc320+i));
 assert(acceptance_profile_input_config());
 acceptance_profile_static_point(0,1,0x4500);acceptance_profile_static_point(1,1,0x4510);
 acceptance_profile_static_point(2,1,0x4540);acceptance_profile_static_point(3,1,0x4580);
 acceptance_profile_static_inputs(1,0xd000,0xd300,0xd301);
 for(unsigned i=0;i<4;++i)acceptance_profile_static_world_field(i,0,0xc310+i);
 acceptance_profile_simulation(1,0x4600,1,0xd000,34,1,0);
 assert(retro_load_game(nullptr));arm();welcome();
 for(unsigned n=0;n<=3;++n)commit(n);
 assert(acceptance_input.next_definition==4&&acceptance_input.current.count==3);
 tick(0x4580);const auto old=acceptance_input.latched;
 setframe(frame(1,24,2));g.cpu.memory[0xc301]=0;input(2);
 r.PC=0x4200;r.SP=0xc100;r.AF.w=0;acceptance_profile_before(&g,r);
 r.PC=0x4201;r.SP=0xc102;acceptance_profile_after(&g,r,0xc9,16);
 assert(acceptance_input.latest==4); // genuine carry-clear incomplete RET cannot commit
 slots(1,2);input(3);assert(!acceptance_profile_input_status(3));
 assert(acceptance_input.latched.input==old.input&&acceptance_input.latest==5);
 tick(0x4580);assert(acceptance_input.latched.input==5);
 assert(acceptance_profile_input_event(acceptance_profile_input_status(5)-2,1,3)==24);
 assert(acceptance_profile_input_drain(acceptance_profile_input_status(5)));
 commit(1,19,2);assert(acceptance_input.current.count==1);commit(0);
 // Repeat revisions generate distinct definitions, with bounded durable release.
 for(unsigned i=0;i<20;++i){tick(0x4580);commit(0);}
 assert(!acceptance_profile_input_status(3));
 arm();welcome();setframe(frame(1));g.mbc.mapping=g.rom.data+0x4000;input(2);
 assert(!acceptance_input.pending);g.mbc.mapping=g.rom.data;input(2);assert(acceptance_input.pending);
 arm();welcome();setframe(frame(2));input(2);input(6);assert(!acceptance_input.pending);
 slots(2);input(3);assert(acceptance_input.failure==accepted_ordinary_input::Failure::transaction);
 auto first=acceptance_profile_input_status(3);acceptance_profile_restore_boundary();assert(acceptance_profile_input_status(3)==first);
 // Same-header replacement is cancelled before the original append copy.
 arm();welcome();setframe(frame(1));input(2);input(6);setframe(frame(1));input(2);
 assert(acceptance_input.candidate.transaction==2);slots(1);input(3);assert(!acceptance_profile_input_status(3));
 arm();welcome();setframe(frame(1));g.cpu.ram[0x2009]=9;input(2);assert(acceptance_input.failure==accepted_ordinary_input::Failure::receiver);
 for(unsigned corrupt : {12u,18u}) {arm();welcome();setframe(frame(1));input(2);slots(1);g.cpu.ram[0x2400+corrupt]^=1;input(3);assert(acceptance_input.failure==accepted_ordinary_input::Failure::committed_input);}
 arm();welcome();input(0);input(1);assert(acceptance_input.failure==accepted_ordinary_input::Failure::lifetime);
 for(unsigned boundary : {8u,9u,10u,11u}) {arm();welcome();input(boundary);assert(acceptance_input.failure==accepted_ordinary_input::Failure::retired);assert(!acceptance_input.latest);}
 arm();welcome();acceptance_profile_restore_boundary();assert(acceptance_input.failure==accepted_ordinary_input::Failure::retired);
 arm();welcome();g.cpu.memory[0xc327]=1;tick(0x4200);assert(acceptance_input.failure==accepted_ordinary_input::Failure::retired);g.cpu.memory[0xc327]=0;
 arm();welcome();g.cpu.memory[0xc302]=0;tick(0x4200);assert(acceptance_input.failure==accepted_ordinary_input::Failure::retired);
 arm();welcome();g.cpu.memory[0x4120]=1;setframe(frame(1));input(2);assert(acceptance_input.failure==accepted_ordinary_input::Failure::transaction);g.cpu.memory[0x4120]=0;
 arm();welcome();r.PC=0x4200;acceptance_profile_before(&g,r);acceptance_profile_before(&g,r);assert(acceptance_input.failure==accepted_ordinary_input::Failure::ordinal);
 arm();welcome();acceptance_profile_after(&g,r,0,4);assert(acceptance_input.failure==accepted_ordinary_input::Failure::ordinal);
 arm();welcome();acceptance_static_intent.sources[0].closed=acceptance_static_intent.instruction+100;tick(0x4580);assert(acceptance_input.failure==accepted_ordinary_input::Failure::missing_source);
 arm();welcome();for(unsigned i=0;i<40;++i)tick(0x4580);assert(acceptance_input.failure==accepted_ordinary_input::Failure::definition_pressure);

 // Independent119: four retained definitions and a live outer latch make release fallible.
 arm();welcome();
 for(unsigned i=0;i<4;++i) {
  setframe(frame(0));g.cpu.memory[0xc301]=0;input(2);slots(0);input(3);tick(0x4580);
 }
 assert(acceptance_input.latest==4&&acceptance_input.latched.input==4&&acceptance_input.released==0);
 setframe(frame(1));g.cpu.ram[0x2009]=9;input(2);
 assert(acceptance_profile_input_status(3)==4&&acceptance_profile_input_status(4)==17);
 const auto queued=acceptance_profile_input_status(5);assert(queued==9);
 assert(!acceptance_profile_input_drain(queued));
 assert(acceptance_profile_input_status(5)==queued&&acceptance_input.released==0);
 for(unsigned exported : {0u,32768u}) {
  {std::ofstream log("failure.jsonl");
   bool rejected=false;
   try {export_inputs(log,exported);} catch(const std::runtime_error& error) {
    rejected=true;
    assert(std::string(error.what())==(exported ? "Accepted input event export limit exceeded (32768)" :
       "Accepted input observer failed: reason=4 instruction=17"));
    // Read before the writer closes: the original witness was explicitly flushed.
    std::ifstream durable("failure.jsonl");std::string witness;
    assert(std::getline(durable,witness)&&witness=="{\"first_failure\":4,\"instruction\":17}");
   }
   assert(rejected);}
  std::ifstream log("failure.jsonl");std::string line;
  assert(std::getline(log,line)&&line=="{\"first_failure\":4,\"instruction\":17}");
  unsigned records=0;while(std::getline(log,line))++records;
  assert(records==(exported ? 0 : queued));
  assert(acceptance_profile_input_status(3)==4&&acceptance_profile_input_status(4)==17);
  assert(acceptance_profile_input_status(5)==queued&&acceptance_input.released==0);
 }
 // A normal successful export still releases old definitions and keeps latest/latch.
 arm();welcome();
 for(unsigned i=0;i<4;++i) {
  setframe(frame(0));g.cpu.memory[0xc301]=0;input(2);slots(0);input(3);tick(0x4580);
 }
 {std::ostringstream log;export_inputs(log);assert(log.str().find("first_failure")==std::string::npos);}
 assert(!acceptance_profile_input_status(5)&&!acceptance_profile_input_status(3));
 assert(acceptance_input.released==3&&acceptance_input.latest==4&&acceptance_input.latched.input==4);
 for(unsigned i=0;i<20;++i){tick(0x4580);commit(0);}
 assert(!acceptance_profile_input_status(3));
 std::puts("PASS actual generated load/reset and real wrapper accepted WELCOME/capture/commit/cancel/source/lifetime negatives");
}
'''
            (root / 'test.cpp').write_text(source)
            subprocess.run(['c++', '-std=c++17', '-Wall', '-Wextra', '-Werror', '-I', str(SUPPORT),
                            str(root / 'test.cpp'), '-o', str(root / 'test')], check=True)
            subprocess.run([str(root / 'test')], check=True, cwd=root)


if __name__ == '__main__':
    unittest.main()
