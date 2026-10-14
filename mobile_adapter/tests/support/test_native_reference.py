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
    auto cold=base; cold.on_dma(0xc4,0); cold.row(0); assert(cold.failure==12);
    auto wrong_dma=base; wrong_dma.latest.id=1; wrong_dma.on_dma(0xd0,1); assert(wrong_dma.failure==11);
}
'''
        with tempfile.TemporaryDirectory() as directory:
            cpp = Path(directory) / "reference.cpp"
            exe = Path(directory) / "reference"
            cpp.write_text(source)
            subprocess.run(["c++", "-std=c++17", "-Wall", "-Wextra", "-Werror", "-I", str(SUPPORT), str(cpp), "-o", str(exe)], check=True)
            subprocess.run([str(exe)], check=True)

    def test_actual_row_reference_counterexamples(self):
        with tempfile.TemporaryDirectory() as directory:
            exe = Path(directory) / "reader-tests"
            subprocess.run(["rustc", "--edition=2021", "--test", str(SUPPORT / "native_reference.rs"), "-o", str(exe)], check=True)
            subprocess.run([str(exe)], check=True)


if __name__ == "__main__":
    unittest.main()
