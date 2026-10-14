"""Bounded cold ordering diagnostic and the actual runner export, without a core build."""
from pathlib import Path
import json
import os
import subprocess
import tempfile
import unittest

SUPPORT = Path(__file__).resolve().parent


class ColdIntentBoundaries(unittest.TestCase):
    def run_cpp(self, source):
        with tempfile.TemporaryDirectory(dir=os.environ.get('TMPDIR')) as directory:
            root = Path(directory)
            (root / 'test.cpp').write_text(source)
            subprocess.run(['c++', '-std=c++17', '-Wall', '-Wextra', '-Werror', '-I', str(SUPPORT),
                            str(root / 'test.cpp'), '-o', str(root / 'test')], check=True)
            return subprocess.check_output([str(root / 'test')], cwd=root, text=True)

    def test_actual_hooks_and_immutable_missing_witness(self):
        preamble = (SUPPORT / 'test_effect_observer.cpp').read_text().split('int main(){')[0]
        self.run_cpp(preamble + r'''
int main() {
 using namespace cold_intent_boundaries;
 (void)&acceptance_timing_irq; (void)&acceptance_profile_effect_io_write;
 (void)&acceptance_profile_before; (void)&acceptance_profile_after; (void)&stack;
 gb game; acceptance_game_context=&game;
 acceptance_input_configured=true;
 assert(acceptance_profile_cold_config());
 assert(!acceptance_profile_cold_config());
 // Actual load/reset hook preserves declaration and discards runtime.
 acceptance_profile_effect_reset();
 assert(acceptance_cold.configured && !acceptance_cold.active);
 for(auto& p:acceptance_static_intent.points) p={0,0x100};
 acceptance_static_intent.objects=0xc000;
 acceptance_static_world_configured=acceptance_static_world_ready=true;
 assert(acceptance_profile_static_enable());
 assert(acceptance_cold.records[0][kind]==activation);
 // Display of the preceding raster occurs before row0, even with capture off.
 int buffer=0;
 acceptance_profile_display_begin(&buffer,160,144,16,320,0,0,1.0f,0,1,0,0,0);
 assert(acceptance_cold.records[1][kind]==display);
 assert(acceptance_cold.records[1][raster_id]==absent);
 acceptance_profile_cold_callback(&buffer);
 assert(acceptance_cold.records[2][kind]==callback);
 acceptance_profile_cold_callback(&buffer);
 assert(acceptance_cold.count==3); // duplicate does not acknowledge physical output
 acceptance_cold.cpu=after_cpu;
 acceptance_profile_render_buffer(&buffer,0);
 assert(acceptance_cold.missing && acceptance_cold.first_row);
 auto witness=acceptance_cold.witness;
 assert(witness[kind]==row0 && witness[latch_op]==0 && witness[source]==0);
 // A later good latch cannot repair or replace the cold witness.
 acceptance_input.latched={1,9,1,8,0}; acceptance_static_intent.instruction=9;
 for(unsigned i=0;i<30;++i) acceptance_profile_render_buffer(&buffer,0);
 assert(acceptance_cold.count==16 && acceptance_cold.witness==witness);
 auto count=acceptance_cold.count; auto failure=acceptance_static_intent.failure;
 assert(acceptance_profile_cold_value(1,16,0)==absent);
 assert(acceptance_profile_cold_value(2,1,0)==absent);
 assert(acceptance_profile_cold_value(1,0,field_count)==absent);
 assert(acceptance_profile_cold_value(99,0,0)==absent);
 assert(acceptance_profile_cold_value(0,1,0)==absent);
 assert(acceptance_cold.count==count && acceptance_static_intent.failure==failure);
 acceptance_profile_effect_reset();
 assert(acceptance_cold.configured && !acceptance_cold.active && !acceptance_cold.missing);
 assert(acceptance_profile_cold_value(2,0,0)==absent);
 // Physical IDs are added only when the existing physical observer assigns them.
 acceptance_static_world_ready=true; assert(acceptance_profile_static_enable());
 acceptance_static_intent.instruction=9; acceptance_input.latched={1,9,1,8,0};
 acceptance_cold.cpu=after_cpu; acceptance_video_enabled=true; acceptance_video_until=1;
 acceptance_profile_render_buffer(&buffer,0);
 assert(acceptance_cold.records[1][raster_id]==absent && !acceptance_cold.missing);
 acceptance_profile_render_line(&game,0);
 assert(acceptance_cold.records[2][kind]==raster && acceptance_cold.records[2][raster_id]==1);
 acceptance_profile_display_begin(&buffer,160,144,16,320,0,0,1.0f,0,1,0,0,0);
 assert(acceptance_cold.records[3][kind]==display && acceptance_cold.records[3][raster_id]==1);
 assert(acceptance_cold.records[3][display_id]==1);
 acceptance_profile_cold_callback(&buffer);
 assert(acceptance_cold.records[4][display_id]==1 && acceptance_cold.records[4][raster_id]==1);
 // Same before-opcode ordinal is valid only after that opcode completed.
 Ledger l; l.configured=true; Record r{}; r[kind]=activation; l.push(r);
 r[kind]=row0; r[latch_op]=1; r[latch_source]=1; r[latch_closed]=8;
 r[latch_instruction]=r[instruction]=9; r[cpu_phase]=before_cpu;
 assert(!Ledger::causal(r)); r[cpu_phase]=after_cpu; assert(Ledger::causal(r));
 l.push(r); assert(l.first_row && !l.missing);
 r[latch_closed]=9; assert(!Ledger::causal(r));
 r[latch_closed]=8; r[latch_instruction]=10; assert(!Ledger::causal(r));
 r[instruction]=11; r[cpu_phase]=before_cpu; assert(Ledger::causal(r));
}
''')

    def test_actual_runner_optin_and_export(self):
        runner = (SUPPORT.parents[1] / 'tools/smoke_libretro.cpp').read_text()
        start = runner.index('    const bool cold_requested =')
        end = runner.index('    const bool timing_requested =', start)
        config = runner.index('"acceptance_profile_cold_config"')
        self.assertLess(config, runner.index('if (!symbol<bool(*)(const retro_game_info*)>(core, "retro_load_game")(&game))'))
        self.assertLess(runner.index('        cold_flush();', runner.index('        run();')),
                        runner.index('        if (input_status) {', runner.index('        run();')))
        self.run_cpp(r'''
#include <map>
#include <vector>
#include <string>
#include <stdexcept>
#include <cassert>
static void fail(const std::string& s) {throw std::runtime_error(s);}
static void check(std::map<std::string,std::string> arguments) {
 std::map<std::string,std::vector<std::string>> effect_options;
 effect_options["--native-scene-field"].resize(8);
''' + runner[start:end] + r'''
}
int main() {
 check({});
 std::map<std::string,std::string> a={{"--cold-intent-boundaries","cold"},{"--accepted-ordinary-inputs","out"},
 {"--static-intent-sources","sources"},{"--symbols","symbols"},{"--profile-out","profile"},
 {"--frames","1200"},{"--acceptance","1"},{"--adapter","1"},{"--input","input"}};
 check(a);
 for(const char* key:{"--accepted-ordinary-inputs","--static-intent-sources","--symbols"}) {
  auto b=a;b.erase(key);bool rejected=false;try{check(b);}catch(const std::runtime_error&){rejected=true;}assert(rejected);
 }
 for(const char* key:{"--snapshot-in","--snapshot-out","--effect-video","--ordinary-stream","--native-vblank-timing"}) {
  auto b=a;b[key]="1";bool rejected=false;try{check(b);}catch(const std::runtime_error&){rejected=true;}assert(rejected);
 }
}
''')
        start = runner.index('static uint64_t (*cold_value)')
        end = runner.index('static unsigned ordinary_start', start)
        actual = runner[start:end].replace('static void (*cold_callback)(const void*) = nullptr;', '')
        rows = self.run_cpp(r'''
#include <fstream>
#include <iostream>
#include "cold_intent_boundaries.hpp"
static cold_intent_boundaries::Ledger ledger;
static uint64_t value(unsigned s,unsigned i,unsigned f){return ledger.value(s,i,f);}
''' + actual + r'''
int main() {
 using namespace cold_intent_boundaries;
 cold_log.open("ledger.jsonl");cold_value=&value;
 ledger.configured=true;Record r{};r[kind]=activation;ledger.push(r);
 r[kind]=row0;r[phase]=render_begin;r[raster_id]=r[display_id]=absent;
 r[pending_entry]=42;ledger.push(r);
 cold_flush();cold_flush();cold_log.close();
 std::ifstream file("ledger.jsonl");std::cout<<file.rdbuf();
}
''')
        data = [json.loads(row) for row in rows.splitlines()]
        self.assertEqual([r['event'] for r in data], ['boundary', 'boundary', 'MissingColdIntent'])
        self.assertEqual(data[2]['pending_entry'], 42)
        self.assertIsNone(data[2]['raster_id'])
        self.assertIsNone(data[2]['display_id'])
        self.assertEqual(data[1]['latched_opportunity'], 0)
        self.assertEqual(data[1]['latched_input'], 0)


if __name__ == '__main__':
    unittest.main()
