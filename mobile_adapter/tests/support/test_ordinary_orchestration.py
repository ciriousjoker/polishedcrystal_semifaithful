"""Host-only ordinary gate and subprocess ownership checks."""
import ast
import concurrent.futures
import json
from pathlib import Path
import subprocess
import tempfile
import time
import unittest
import runtime

class OrdinaryOrchestration(unittest.TestCase):
    def test_four_readers_eof_and_failure_cleanup(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp)
            reader=root/'reader.py'
            reader.write_text("#!/usr/bin/env python3\nimport sys\nassert sys.stdin.buffer.read()==b'complete'\nfor i in range(int(sys.argv[1])): print(i)\n")
            reader.chmod(0o755)
            runner=root/'runner.py'
            runner.write_text("import pathlib,sys,time\np=pathlib.Path(sys.argv[1])\nwith (p/'ordinary.fifo').open('wb',buffering=0) as pipe:\n (p/'ready').touch()\n while not (p.parent/'gate').exists(): time.sleep(.002)\n pipe.write(b'complete')\n(p/'state.json.ordinary-complete').touch()\nwhile not (p.parent/'release').exists(): time.sleep(.002)\n")
            owned=runtime.OrdinaryProcesses(reader,800)
            dirs=[root/str(i) for i in range(4)]
            for d in dirs:d.mkdir()
            def wait(paths,jobs):
                deadline=time.monotonic()+3
                while not all(p.exists() for p in paths):
                    for f in jobs:
                        if f.done():f.result()
                    if time.monotonic()>deadline:raise TimeoutError('Host readiness timeout')
                    time.sleep(.002)
            try:
                with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
                    jobs=[pool.submit(owned.run,['python3',runner,d],root,d/'process.log',d,5) for d in dirs]
                    wait([d/'ready' for d in dirs],jobs)
                    self.assertFalse(any((d/'state.json.ordinary-complete').exists() for d in dirs))
                    (root/'gate').touch()
                    wait([d/'state.json.ordinary-complete' for d in dirs],jobs)
                    self.assertTrue(all(not f.done() for f in jobs))
                    (root/'release').touch()
                    for f in jobs:f.result()
                for d in dirs:
                    self.assertFalse((d/'ordinary.fifo').exists())
                    self.assertEqual(json.loads((d/'ordinary-reader.json').read_text())['records'],800)
                bad=root/'bad';bad.mkdir()
                with self.assertRaises(subprocess.CalledProcessError):
                    owned.run(['python3','-c','raise SystemExit(2)'],root,bad/'process.log',bad,1)
                self.assertTrue(all(p.poll() is not None for p in owned.children))
                self.assertFalse((bad/'ordinary.fifo').exists())
            finally:owned.close()

    def test_reader_rejection_and_timeout_stop_owned_children(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp)
            reader=root/'reader.py'
            reader.write_text("#!/usr/bin/env python3\nimport sys\nassert sys.stdin.buffer.read()==b'complete'\n")
            reader.chmod(0o755)
            for name,script,error,timeout in [
                ('rejected', "import pathlib,sys; (pathlib.Path(sys.argv[1])/'ordinary.fifo').write_bytes(b'bad')", RuntimeError, 2),
                ('timeout', "import time; time.sleep(30)", TimeoutError, .05),
            ]:
                directory=root/name;directory.mkdir()
                owned=runtime.OrdinaryProcesses(reader,800)
                try:
                    with self.assertRaises(error):
                        owned.run(['python3','-c',script,directory],root,directory/'process.log',directory,timeout)
                    self.assertTrue(all(p.poll() is not None for p in owned.children))
                    self.assertFalse((directory/'ordinary.fifo').exists())
                finally:owned.close()

    def test_extracted_gate_revalidates_and_never_rearms(self):
        source=(runtime.ROOT/'mobile_adapter/tools/smoke_libretro.cpp').read_text()
        setup=source[source.index('    const bool ordinary_gate ='):source.index('    unsigned measurement_start =')]
        gate=source[source.index('        if (ordinary_gate && !ordinary_armed) {'):source.index('        if (input_frame == pace_from)')]
        self.assertLess(source.index(gate),source.index('        run();',source.index(gate)))
        harness=r'''
#include <map>
#include <string>
#include <vector>
#include <fstream>
#include <stdexcept>
#include <cassert>
#include <type_traits>
using Values=std::map<std::string,unsigned>;
static bool live=true,scene=true,changes=false;
static unsigned arms=0,aligns=0,raster=7;
static Values capture(const unsigned char*,unsigned){return {{"wMapStatus",2},{"wScriptRunning",0},{"wMobileOnline",live?1u:0u},{"wMobilePeerCount",3}};}
static unsigned marker(unsigned){return scene;}
static unsigned long updates(unsigned){return 1;}
static unsigned arm(unsigned n){assert(n==800);++arms;return 1;}
static unsigned scene_read(){return scene;}
static void fail(const std::string&s){throw std::runtime_error(s);}
template<class F> F symbol(void*,const char*){if constexpr(std::is_same_v<F,unsigned(*)(unsigned)>)return arm;else return scene_read;}
int main(int argc,char**argv){
 std::map<std::string,std::string> arguments={{"--ordinary-gate",std::string(argv[1])+"/gate"},{"--state-out",std::string(argv[1])+"/state"}};
 std::map<std::string,std::vector<std::string>> effect_options;
 bool acceptance_enabled=true,profile_frame=true,ordinary_status=true;
 const unsigned char*sram=nullptr;unsigned sram_size=0;
 auto acceptance_marker_count=marker;auto metric=updates;
 void*core=nullptr;unsigned input_frame=0,ordinary_start=0,raster_alignment_lines=0;
 auto raster_state=[](){return raster;};
 auto align_raster=[](){++aligns;raster=0x100;if(changes)live=false;return 1;};
SETUP
 auto tick=[&](){GATE};
 tick();assert(ordinary_announced&&arms==0&&aligns==0);
 std::ofstream(arguments.at("--ordinary-gate"))<<"release";
 live=false;bool rejected=false;try{tick();}catch(const std::runtime_error&){rejected=true;}
 assert(rejected&&arms==0&&aligns==0);
 live=true;changes=true;rejected=false;try{tick();}catch(const std::runtime_error&){rejected=true;}
 assert(rejected&&arms==0&&aligns==1);
 live=true;changes=false;tick();assert(arms==1&&ordinary_armed);
 std::remove(arguments.at("--ordinary-gate").c_str());tick();
 std::ofstream(arguments.at("--ordinary-gate"))<<"release";tick();assert(arms==1);
}
'''.replace('SETUP',setup).replace('GATE',gate)
        with tempfile.TemporaryDirectory() as tmp:
            cpp,exe=Path(tmp)/'gate.cpp',Path(tmp)/'gate-test';cpp.write_text(harness)
            subprocess.run(['c++','-std=c++17',str(cpp),'-o',str(exe)],check=True)
            subprocess.run([str(exe),tmp],check=True,timeout=5)


    def test_existing_attempts_preserve_stale_ordinary_artifacts(self):
        source=runtime.ROOT/'mobile_adapter/tests/support/runtime.py'
        module=ast.parse(source.read_text())
        execute=next(n for n in module.body if isinstance(n,ast.FunctionDef) and n.name=='execute')
        execute.body=execute.body[:2]+[ast.Return(value=ast.Name(id='out',ctx=ast.Load()))]
        isolated=ast.fix_missing_locations(ast.Module(body=[execute],type_ignores=[]))
        namespace={}
        exec(compile(isolated,str(source),'exec'),namespace)
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);original=root/'capture'
            artifacts=['ordinary.gate','capture','control','output','state.json.ordinary-ready','state.json.ordinary-complete']
            for directory in [original,root/'capture-attempt-2']:
                directory.mkdir()
                for name in artifacts:(directory/name).write_text('stale evidence')
            selected=namespace['execute']({}, {}, original, [], functional=True, ordinary_count=800)
            self.assertEqual(selected,root/'capture-attempt-3')
            self.assertFalse(selected.exists())
            for directory in [original,root/'capture-attempt-2']:
                for name in artifacts:self.assertEqual((directory/name).read_text(),'stale evidence')

    def test_extracted_capture_rejects_scope_loss_and_preserves_completed_pumping(self):
        source=(runtime.ROOT/'mobile_adapter/tools/smoke_libretro.cpp').read_text()
        setup=source[source.index('    const bool ordinary_gate ='):source.index('    unsigned measurement_start =')]
        gate=source[source.index('        if (ordinary_gate && !ordinary_armed) {'):source.index('        if (input_frame == pace_from)')]
        execution=source[source.index('        // Check the idle scene on both sides'):source.index('        if (effect_enabled) {',source.index('        // Check the idle scene on both sides'))]
        harness=r"""
#include <cassert>
#include <fstream>
#include <map>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>
using Values=std::map<std::string,unsigned>;
static unsigned loss=0,loss_during_run=0,loss_during_profile=0,status=0,records=0,arms=0,runs=0;
static int ordinary_fd=8;static unsigned ordinary_count=0;
static Values capture(const unsigned char*,unsigned){return {{"wMapStatus",loss==1?1u:2u},{"wScriptRunning",loss==2?1u:0u},{"wMobileOnline",loss==3?0u:1u},{"wMobilePeerCount",loss==4?2u:3u}};}
static unsigned marker(unsigned){return loss!=5;}
static unsigned long updates(unsigned){return loss!=6;}
static unsigned arm(unsigned n){assert(n==800);++arms;status=1;return 1;}
static unsigned state(unsigned f){return f==1?status:records;}
static unsigned scene(){return loss!=5;}
static void profile(){if(loss_during_profile)loss=loss_during_profile;}
static void fail(const std::string&s){throw std::runtime_error(s);}
template<class F> F symbol(void*,const char*){if constexpr(std::is_same_v<F,unsigned(*)(unsigned)>)return arm;else return scene;}
int main(int argc,char**argv){
std::string phase=argv[2];unsigned field=std::stoul(argv[3]);
std::map<std::string,std::string> arguments={{"--ordinary-gate",std::string(argv[1])+"/gate"},{"--state-out",std::string(argv[1])+"/state"}};
std::map<std::string,std::vector<std::string>> effect_options;
bool acceptance_enabled=true;auto profile_frame=profile;auto ordinary_status=state;
const unsigned char*sram=nullptr;unsigned sram_size=0;auto acceptance_marker_count=marker;auto metric=updates;
void*core=nullptr;unsigned input_frame=0,ordinary_start=0,raster_alignment_lines=0,frame_limit=12000;
unsigned raster_boundary_failures=0,measurement_start=0;bool raster_aligned=false;
auto raster_state=[](){return 0x100u;};auto align_raster=[](){return 1u;};
auto run=[&](){++runs;if(status==1&&++records==800){status=2;ordinary_count=800;ordinary_fd=-1;}if(loss_during_run)loss=loss_during_run;};
SETUP
std::ofstream(arguments.at("--ordinary-gate"))<<"go";
auto tick=[&](){GATE EXECUTION};
auto rejected=[&](){bool caught=false;try{tick();}catch(const std::runtime_error&){caught=true;}assert(caught);assert(!ordinary_completed);assert(!std::ifstream(arguments.at("--state-out")+".ordinary-complete").good());assert(arms==1);};
if(phase=="before"){GATE loss=field;rejected();assert(runs==0);}
else if(phase=="during"){for(unsigned i=0;i<399;++i){tick();++input_frame;}loss_during_run=field;rejected();assert(runs==400);}
else if(phase=="final"){for(unsigned i=0;i<799;++i){tick();++input_frame;}loss_during_run=field;rejected();assert(runs==800&&records==800&&status==2);}
else if(phase=="completion"){for(unsigned i=0;i<799;++i){tick();++input_frame;}loss_during_profile=field;rejected();assert(runs==800&&status==2);}
else {for(unsigned i=0;i<800;++i){tick();++input_frame;}assert(ordinary_completed&&arms==1&&runs==800&&frame_limit==800);assert(std::ifstream(arguments.at("--state-out")+".ordinary-complete").good());loss=field;std::remove(arguments.at("--ordinary-gate").c_str());tick();std::ofstream(arguments.at("--ordinary-gate"))<<"go";tick();assert(runs==802&&arms==1&&records==800);}
}
""".replace('SETUP',setup).replace('GATE',gate).replace('EXECUTION',execution)
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);cpp=root/'scope.cpp';exe=root/'scope-test';cpp.write_text(harness)
            subprocess.run(['c++','-std=c++17',str(cpp),'-o',str(exe)],check=True)
            for phase in ['before','during','final','completion','completed-pumping']:
                for field in range(1,7):
                    with self.subTest(phase=phase,field=field):
                        directory=root/f'{phase}-{field}';directory.mkdir()
                        subprocess.run([str(exe),str(directory),phase,str(field)],check=True,timeout=5)

if __name__=='__main__':unittest.main()
