"""Focused finite disabled reference tests. Host results are not native acceptance."""
from pathlib import Path
import subprocess
import tempfile
import unittest

SUPPORT = Path(__file__).resolve().parent


class NativeReferenceTests(unittest.TestCase):
    def test_native_source_closure_palette_and_dma_lifetime(self):
        source = r'''
#include "static_ordinary_intent.hpp"
#include "native_reference.hpp"
#include <cassert>
int main() {
    static_ordinary_intent::Observer s;
    native_reference::Observer r;
    s.objects=0xd000; r.enabled=true;
    unsigned memory[65536]={};
    for (unsigned f=0; f<24; ++f) r.fields[f]={0,0xc000+f*32};
    r.fields[0]={0,0xc400};
    auto read=[&](unsigned bank,unsigned at)->unsigned {
        if (bank==9) return (at+7)&255;
        if (bank==10) return (at+9)&255;
        return memory[at];
    };
    memory[0xc100]=0; memory[0xc101]=0x45;
    r.before(s,true,false,false,0,0,0,read);
    for (unsigned a=0;a<4;++a) {
        r.sprites[a]=a+1; r.palettes[a]=a; r.map_x[a]=10+a; r.map_y[a]=12;
        auto& src=s.sources[0];
        for (auto* v : {&src.original,&src.at_return,&src.at_close}) {
            (*v)[a*34]=a+1; (*v)[a*34+6]=a;
            (*v)[a*34+16]=10+a; (*v)[a*34+17]=12; (*v)[a*34+33]=a;
        }
        memory[r.fields[2].address+a]=a;
        memory[r.fields[1].address]=a*16;
        r.before(s,false,true,false,0xd000+a*34,0xc100,1,read);
        for (unsigned n=0;n<4;++n) for(unsigned f=0;f<4;++f)
            memory[0xc400+(39-a*4-n)*4+f]=f==0?32+a*24:f==1?32+n*8:f==2?n:a;
        memory[r.fields[1].address]=(a+1)*16;
        // A mismatching return cannot complete the emitter.
        r.after(1,0x4500,0xc102,0xc0fe,true,read); assert(r.emitter_active);
        r.after(1,0x4500,0xc102,0xc100,true,read); assert(!r.emitter_active);
    }
    auto& src=s.sources[0]; src.id=1; src.entry=1; src.returned=20; src.closed=21; src.emitted=15;
    s.latest=1;
    // Native GameCorner INDOOR/PALETTE_DAY key, not learned from hardware output.
    for (auto item : {std::pair<unsigned,unsigned>{9,1},{10,1},{11,1},{12,12},
            {17,1},{18,0x55},{19,11},{20,23},{21,3},{22,255}})
        memory[r.fields[item.first].address]=item.second;
    memory[r.fields[15].address+3]=1;
    const auto base=r;
    // Independently corrupt every required operand before the first close.
    for (unsigned f=9;f<24;++f) {
        if (f==13) continue; // Existing per-actor palette-fade guard below.
        const unsigned count=f==15?4:1;
        for (unsigned n=0;n<count;++n) {
            const auto addr=r.fields[f].address+n; const auto old=memory[addr];
            memory[addr]^=f==14?4:1;
            auto wrong=base; wrong.before(s,false,false,true,0,0,0,read);
            assert(wrong.failure==14 && !wrong.latest.id && !wrong.dma.id && !wrong.raster.id);
            assert(wrong.first_failure[1]==14 && wrong.first_failure[2]==3 && !wrong.ready());
            assert(wrong.first_failure[32] == memory[r.fields[9].address]);
            memory[addr]=old;
        }
    }
    // Review91 exact stable wrong-first receiver reproduction now fails closed.
    memory[r.fields[9].address]=2; memory[r.fields[10].address]=2; memory[r.fields[11].address]=255;
    auto repro=base; repro.before(s,false,false,true,0,0,0,read);
    assert(repro.failure==14 && !repro.latest.id);
    memory[r.fields[9].address]=1; memory[r.fields[10].address]=1; memory[r.fields[11].address]=1;
    auto scheduling=base; memory[r.fields[14].address]=0xfb;
    scheduling.before(s,false,false,true,0,0,0,read);
    assert(!scheduling.failure && scheduling.latest.id==1);
    memory[r.fields[14].address]=0;
    r.before(s,false,false,true,0,0,0,read);
    assert(!r.failure && r.latest.id==1 && r.latest.data[444]==32);
    const auto published=r;
    for (unsigned f : {9u,10u,11u,14u,15u,16u,17u,18u,19u,20u,21u,22u,23u}) {
        for (unsigned n=0;n<(f==15?4u:1u);++n) {
            const auto addr=r.fields[f].address+n; const auto old=memory[addr];
            memory[addr]^=f==14?4:1;
            auto later=published; src.id=2;
            later.before(s,false,false,true,0,0,0,read);
            assert(later.failure==14 && later.latest.id==1 && !later.dma.id);
            memory[addr]=old; src.id=1;
        }
    }
    r.on_dma(0xc4,0); assert(r.dma.id==1);
    r.row(0); assert(r.raster.id==1);
    r.latest.id=2; r.latest.data[444]=77;
    r.row(143); assert(!r.failure && r.raster.id==1 && r.raster.data[444]==32);
    r.on_dma(0xc4,0); r.row(143); assert(r.failure==12); // mid-raster rebinding forbidden
    auto wrong=base; memory[wrong.fields[2].address]=7;
    wrong.before(s,false,false,true,0,0,0,read); assert(wrong.failure==8 && !wrong.latest.id);
    memory[wrong.fields[2].address]=0;
    auto missing=base; missing.completed=7;
    missing.before(s,false,false,true,0,0,0,read); assert(missing.failure==5);
    // Mid-call enablement cannot authenticate retained old DMA or its rows.
    auto cold=base; cold.context(55,1094,103); cold.on_dma(0xc4,0);
    cold.row(103); cold.row(0); cold.row(143);
    assert(!cold.failure && !cold.ready() && !cold.raster.id);
    cold.before(s,false,false,true,0,0,0,read);
    assert(cold.latest.id==1 && !cold.ready());
    cold.row(0); assert(!cold.raster.id); // closed source alone is insufficient
    cold.on_dma(0xc4,0); assert(cold.ready() && !cold.raster.id);
    cold.row(70); assert(!cold.raster.id); // no manufactured partial-raster credit
    cold.row(0); assert(cold.raster.id==1 && !cold.failure);
    auto no_emitter=base; no_emitter.completed=7;
    no_emitter.context(55,2583,0); no_emitter.before(s,false,false,true,0,0,0,read);
    assert(no_emitter.failure==5 && !no_emitter.ready());
    const auto immutable=no_emitter.first_failure;
    assert(immutable[1]==5 && immutable[2]==3 && immutable[7]==1 && immutable[11]==7);
    no_emitter.context(99,9999,143); no_emitter.on_dma(0xc4,0); no_emitter.row(0); no_emitter.fail(14);
    assert(no_emitter.first_failure==immutable && no_emitter.failure==5);
    const auto status=no_emitter.status(); assert(no_emitter.status()==status);
    assert(no_emitter.first_failure==immutable);
    auto wrong_dma=base; wrong_dma.latest.id=1; wrong_dma.on_dma(0xd0,1); assert(wrong_dma.failure==11);
}
'''
        with tempfile.TemporaryDirectory() as directory:
            cpp = Path(directory) / "reference.cpp"
            exe = Path(directory) / "reference"
            cpp.write_text(source)
            subprocess.run(["c++", "-std=c++17", "-Wall", "-Wextra", "-Werror", "-I", str(SUPPORT), str(cpp), "-o", str(exe)], check=True)
            subprocess.run([str(exe)], check=True)

    def test_profile_control_prologue_commits_only_known_dma_at_actual_row_zero(self):
        source = r'''
#define main prior_observer_main
#include "test_effect_observer.cpp"
#undef main
int main() {
    gb g; acceptance_profile_effect_reset(); acceptance_game_context=&g;
    auto& r=acceptance_native_reference; r.enabled=true; r.fields[0]={0,0xc400};
    assert(acceptance_profile_ordinary_arm(2));
    const auto requested=acceptance_profile_ordinary_status(6);
    acceptance_profile_render_buffer(g.cpu.oam,0);
    assert(acceptance_profile_ordinary_status(5) && acceptance_profile_ordinary_status(1)==0);
    assert(!r.failure && !r.raster.id);
    r.latest.id=1; r.latest.entry=1; r.latest.returned=2; r.latest.closed=3;
    acceptance_profile_render_buffer(g.cpu.oam,0);
    assert(acceptance_profile_ordinary_status(5));
    r.on_dma(0xc4,0);
    acceptance_profile_render_buffer(g.cpu.oam,23);
    assert(acceptance_profile_ordinary_status(5) && !r.raster.id);
    acceptance_profile_render_buffer(g.cpu.oam,0);
    assert(!acceptance_profile_ordinary_status(5) && acceptance_profile_ordinary_status(1)==1);
    assert(r.raster.id==1 && acceptance_profile_ordinary_status(6)==requested);
    const auto source_failure=acceptance_static_intent.failure;
    for(unsigned k=0;k<5;++k) for(unsigned f=0;f<64;++f) (void)acceptance_profile_reference(k,f);
    assert(acceptance_static_intent.failure==source_failure);
    acceptance_profile_effect_reset(); r.enabled=true; r.fail(14);
    assert(acceptance_profile_ordinary_arm(2));
    acceptance_profile_render_buffer(g.cpu.oam,0);
    assert(acceptance_profile_ordinary_status(5) && acceptance_profile_ordinary_status(1)==0);
    // Ordinary physical capture with the reference mode disabled keeps its original commit.
    acceptance_profile_effect_reset(); assert(acceptance_profile_ordinary_arm(2));
    acceptance_profile_render_buffer(g.cpu.oam,0);
    assert(acceptance_profile_ordinary_status(1)==1 && !acceptance_profile_ordinary_status(5));
}
'''
        with tempfile.TemporaryDirectory() as directory:
            cpp = Path(directory) / "prologue.cpp"
            exe = Path(directory) / "prologue"
            cpp.write_text(source)
            subprocess.run(["c++", "-std=c++17", "-I", str(SUPPORT), str(cpp), "-o", str(exe)], check=True)
            subprocess.run([str(exe)], check=True)

    def test_runner_diagnostic_sidecar_is_bounded_and_precedes_failure(self):
        runner = (SUPPORT.parent.parent / "tools/smoke_libretro.cpp").read_text()
        diagnostic = runner[runner.index("static std::ofstream reference_status_log;"):runner.index("static int ordinary_fd = -1;")]
        failure = runner[runner.index("static void fail("):runner.index("// OUTF v1:")]
        source = r'''
#include <fstream>
#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdint>
static unsigned input_frame=56,ordinary_count=1;
static uint64_t value(unsigned section,unsigned field) {
    if(section==0 && field==1) return 14;
    return section*100+field;
}
static auto native_reference_value=value;
static unsigned scalar(unsigned f){return f;}
static auto ordinary_status=scalar;
static unsigned (*ordinary_failure)(unsigned)=nullptr;
DIAGNOSTIC
FAILURE
int main(int argc,char**argv) {
    reference_status_log.open(argv[1]);
    reference_snapshot("activation_request");
    fail("native sentinel");
}
'''.replace("DIAGNOSTIC", diagnostic).replace("FAILURE", failure)
        with tempfile.TemporaryDirectory() as directory:
            cpp = Path(directory) / "diagnostic.cpp"
            exe = Path(directory) / "diagnostic"
            sidecar = Path(directory) / "diagnostic.jsonl"
            cpp.write_text(source)
            subprocess.run(["c++", "-std=c++17", str(cpp), "-o", str(exe)], check=True)
            result = subprocess.run([str(exe), str(sidecar)], text=True, capture_output=True)
            self.assertEqual(result.returncode, 1)
            self.assertIn("reason=14 latest=217 dma=221 raster=225", result.stderr)
            self.assertTrue(result.stderr.endswith("native sentinel\n"))
            import json
            records = [json.loads(line) for line in sidecar.read_text().splitlines()]
            self.assertEqual([r["event"] for r in records], ["activation_request", "failure"])
            self.assertEqual(len(records[-1]["first_failure"]), 64)
            self.assertEqual(records[-1]["status"][32], 232)
            self.assertEqual(len(records[-1]["configured_receiver_key"]), 18)
            # Reuse the same real helper to prove the fixed event cap.
            cpp.write_text(source.replace('fail("native sentinel");', 'for(unsigned i=0;i<100;++i) reference_snapshot("ready");'))
            subprocess.run(["c++", "-std=c++17", str(cpp), "-o", str(exe)], check=True)
            subprocess.run([str(exe), str(sidecar)], check=True)
            self.assertEqual(len(sidecar.read_text().splitlines()), 8)

    def test_actual_row_reference_counterexamples(self):
        with tempfile.TemporaryDirectory() as directory:
            exe = Path(directory) / "reader-tests"
            subprocess.run(["rustc", "--edition=2021", "--test", str(SUPPORT / "native_reference.rs"), "-o", str(exe)], check=True)
            subprocess.run([str(exe)], check=True)


if __name__ == "__main__":
    unittest.main()
