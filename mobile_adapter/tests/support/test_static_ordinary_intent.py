"""Focused source-lifetime and exact native-appearance observer counterexamples."""
from pathlib import Path
import subprocess
import tempfile
import unittest

SUPPORT = Path(__file__).resolve().parent


class StaticOrdinaryIntentTests(unittest.TestCase):
    def test_actual_unwindowed_world_return_readiness(self):
        with tempfile.TemporaryDirectory() as directory:
            exe = Path(directory) / "observer"
            subprocess.run(["c++", "-std=c++17", "-O0", "-I", str(SUPPORT),
                            str(SUPPORT / "test_effect_observer.cpp"), "-o", str(exe)], check=True)
            subprocess.run([str(exe)], check=True)

    def test_supplement_bank_binding_and_default_off(self):
        source = r'''#include "static_ordinary_intent.hpp"
#include <cassert>
int main() {
 using namespace static_ordinary_intent;
 static_assert(input_bytes==444);
 Observer o;o.enabled=true;o.objects=0xd000;o.object_bank=1;
 o.camera_x=0xd200;o.camera_y=0xd201;
 o.points={{{1,0x4000},{1,0x4100},{1,0x4200},{2,0x4300}}};
 unsigned value=10,reads=0;
 auto read=[&](unsigned bank,unsigned at)->unsigned {
  if(at==0xc000)return 0x10;
  if(at==0xc001)return 0x40;
  if(at>=0xc380&&at<0xc386){++reads;assert(bank==(at-0xc380)%3);return value+at-0xc380;}
  return 0;
 };
 auto enter=[&]{o.before(1,0x4000,0xc000,0,read);};
 auto ret=[&]{o.before(1,0x4011,0xc000,0,read);o.after(1,0x4010,0xc002,true,read);};
 auto close=[&]{o.before(1,0x4200,0xc000,0,read);};
 enter();ret();close();assert(!reads&&!o.get(1)->operands_captured);
 for(unsigned i=0;i<6;++i)o.operand_fields[i]={i%3,0xc380+i};
 o.operands_configured=true;o.reset();assert(!o.enabled&&!o.failure&&o.operands_configured);
 o.enabled=true;
 enter();value=30;
 o.after(2,0x4010,0xc002,true,read);assert(o.active&&reads==6);
 ret();value=50;close();value=70;
 const auto first=*o.get(1);
 for(unsigned i=0;i<6;++i){assert(first.operands_entry[i]==10+i);assert(first.operands_return[i]==30+i);assert(first.operands_close[i]==50+i);}
 for(unsigned n=0;n<7;++n){enter();ret();close();}
 assert(!o.failure&&o.next_id==8);enter();ret();close();assert(o.failure==6);
 assert(o.sources[0].operands_entry==first.operands_entry&&o.sources[0].operands_close==first.operands_close);
 o.reset();assert(!o.failure&&!o.next_id&&o.operands_configured&&!o.pending.operands_captured);
 o.enabled=true;enter();ret();close();o.release(1);
 for(unsigned n=0;n<8;++n){enter();ret();close();}
 assert(!o.failure&&o.next_id==9&&o.sources[0].id==9);
 assert(!o.get(1)&&o.failure==8);
}
'''
        with tempfile.TemporaryDirectory() as directory:
            cpp = Path(directory) / "operands.cpp"
            exe = Path(directory) / "operands"
            cpp.write_text(source)
            subprocess.run(["c++", "-std=c++17", "-Wall", "-Wextra", "-Werror", "-I", str(SUPPORT), str(cpp), "-o", str(exe)], check=True)
            subprocess.run([str(exe)], check=True)

    def test_source_return_closure_and_bounded_retention(self):
        source = r'''
#include "static_ordinary_intent.hpp"
#include <cassert>
int main() {
    using namespace static_ordinary_intent;
    Observer o;
    o.enabled = true; o.objects = 0xd000; o.object_bank = 1;
    o.camera_x = 0xd200; o.camera_y = 0xd201;
    o.points = {{{1,0x4000},{1,0x4100},{1,0x4200},{2,0x4300}}};
    unsigned memory[65536] = {}; memory[0xc000] = 0x10; memory[0xc001] = 0x40;
    memory[0xd000] = 7; memory[0xd200] = 19;
    auto read = [&](unsigned bank, unsigned at) {
        if (at == 0xc000 || at == 0xc001) assert(bank == 8);
        else assert(bank == 1);
        return memory[at];
    };
    auto produce = [&] {
        o.before(1,0x4000,0xc000,0,read);
        o.before(1,0x4100,0xbffe,0xd000,read);
        // An unrelated return cannot complete the producer.
        o.after(1,0x4010,0xc000,true,read); assert(o.active);
        o.before(1,0x4011,0xc000,0,read);
        o.after(1,0x4010,0xc002,true,read); assert(!o.active && o.has_return);
        o.before(1,0x4200,0xc000,0,read);
    };
    produce(); assert(!o.failure && o.latest == 1);
    const auto* s = o.get(1); assert(s && s->emitted == 1 && s->original[0] == 7);
    assert(s->original[input_bytes - 2] == 19);
    assert(s->entry < s->returned && s->returned < s->closed);
    memory[0xd200] = 99;
    o.before(2,0x4300,0xc000,0,read);
    assert(o.opportunity == 1 && o.get(1)->at_close[input_bytes - 2] == 19);
    // No Clear initializer event is needed to publish a genuine producer.
    for (unsigned i = 0; i < 7; ++i) produce();
    assert(!o.failure && o.latest == 8);
    produce(); assert(o.failure == 6 && o.latest == 8);
    o.fail(99); assert(o.failure == 6); // immutable first failure
    Observer direct; direct.enabled = true; direct.started = true; direct.points[2] = {1,0x4200};
    direct.before(1,0x4200,0xc000,0,read); assert(direct.failure == 5);
    Observer missing; missing.enabled = true; missing.started = true; missing.points[3] = {2,0x4300};
    missing.before(2,0x4300,0xc000,0,read); assert(missing.failure == 7);
    Observer evicted; evicted.enabled = true; assert(!evicted.get(1) && evicted.failure == 8);
}
'''
        with tempfile.TemporaryDirectory() as directory:
            cpp = Path(directory) / "source.cpp"
            exe = Path(directory) / "source"
            cpp.write_text(source)
            subprocess.run(["c++", "-std=c++17", "-Wall", "-Wextra", "-Werror", "-I", str(SUPPORT), str(cpp), "-o", str(exe)], check=True)
            subprocess.run([str(exe)], check=True)

    def test_opportunity_requires_current_closed_live_source(self):
        source = r'''
#include "static_ordinary_intent.hpp"
#include <cassert>
int main() {
    using namespace static_ordinary_intent;
    Observer base;
    base.enabled = true; base.objects = 0xd000; base.object_bank = 1;
    base.camera_x = 0xd200; base.camera_y = 0xd201;
    base.points = {{{1,0x4000},{1,0x4100},{1,0x4200},{2,0x4300}}};
    unsigned value = 7;
    auto read = [&](unsigned, unsigned at) -> unsigned {
        return at == 0xc000 ? 0x10 : at == 0xc001 ? 0x40 : value;
    };
    auto enter = [&](Observer& o) { o.before(1,0x4000,0xc000,0,read); };
    auto returned = [&](Observer& o) {
        o.before(1,0x4011,0xc000,0,read);
        o.after(1,0x4010,0xc002,true,read);
    };
    auto close = [&](Observer& o) { o.before(1,0x4200,0xc000,0,read); };
    auto opportunity = [&](Observer& o) { o.before(2,0x4300,0xc000,0,read); };
    auto same_source = [](const Source& a, const Source& b) {
        assert(a.id == b.id && a.entry == b.entry && a.returned == b.returned);
        assert(a.closed == b.closed && a.emitted == b.emitted);
        assert(a.original == b.original && a.at_return == b.at_return && a.at_close == b.at_close);
    };
    // Bootstrap ignores pre-observation events, but never credits a source.
    close(base); opportunity(base);
    assert(!base.started && !base.failure && !base.opportunity && !base.latest);
    enter(base); returned(base); close(base);
    const auto unreferenced = base;
    opportunity(base);
    assert(!base.failure && base.latest == 1 && base.opportunity == 1);
    const auto first = *base.get(1);
    opportunity(base); assert(!base.failure && base.opportunity == 1);
    value = 23;
    // Each unfinished state must fail independently at the opportunity itself.
    for (bool complete_return : {false, true}) {
        auto o = base;
        enter(o);
        if (complete_return) returned(o);
        assert(o.active == !complete_return && o.has_return == complete_return);
        const auto pending = o.pending;
        opportunity(o);
        assert(o.failure == 7 && o.latest == 1 && o.next_id == 1 && o.opportunity == 1);
        same_source(*o.get(1), first); same_source(o.pending, pending);
        o.fail(99); o.get(999); enter(o); returned(o); close(o); opportunity(o);
        assert(o.failure == 7 && o.latest == 1 && o.next_id == 1 && o.opportunity == 1);
        same_source(*o.get(1), first); same_source(o.pending, pending);
    }
    // A genuine new close selects its own snapshots, then permits repeats.
    auto fresh = base;
    enter(fresh); returned(fresh); close(fresh); opportunity(fresh);
    assert(!fresh.failure && fresh.opportunity == 2 && fresh.latest == 2);
    assert(fresh.get(2)->original[0] == 23 && fresh.get(2)->at_close[0] == 23);
    same_source(*fresh.get(1), first);
    opportunity(fresh); assert(!fresh.failure && fresh.opportunity == 2);
    fresh.release(1); assert(!fresh.failure && fresh.released == 1);
    fresh.release(2); assert(fresh.failure == 9 && fresh.released == 1);
    opportunity(fresh); assert(fresh.failure == 9 && fresh.opportunity == 2);
    // A source released before acquiring an opportunity cannot be resurrected.
    auto released = unreferenced;
    released.release(1); assert(!released.failure && released.released == 1);
    opportunity(released);
    assert(released.failure == 8 && !released.opportunity && released.latest == 1);
    same_source(released.sources[0], first);
    // Exercise an actual ring replacement and rejection of its obsolete token.
    auto ring = unreferenced; ring.release(1);
    for (unsigned i = 0; i < source_capacity; ++i) {
        enter(ring); returned(ring); close(ring);
    }
    assert(!ring.failure && ring.latest == 9 && ring.sources[0].id == 9);
    auto obsolete = ring;
    obsolete.latest = 1; // Inject a stale candidate, never an accepted reference.
    opportunity(obsolete);
    assert(obsolete.failure == 8 && !obsolete.opportunity);
    same_source(obsolete.sources[0], ring.sources[0]);
    opportunity(ring); assert(!ring.failure && ring.opportunity == 9);
    assert(!ring.get(1) && ring.failure == 8 && ring.opportunity == 9);
}
'''
        with tempfile.TemporaryDirectory() as directory:
            cpp = Path(directory) / "chronology.cpp"
            exe = Path(directory) / "chronology"
            cpp.write_text(source)
            subprocess.run(["c++", "-std=c++17", "-Wall", "-Wextra", "-Werror", "-I", str(SUPPORT), str(cpp), "-o", str(exe)], check=True)
            subprocess.run([str(exe)], check=True)

    def test_runtime_source_option_is_default_off_and_rejects_unsupported_scene(self):
        import ast
        import runtime
        import inspect
        self.assertFalse(inspect.signature(runtime.execute).parameters["static_sources"].default)
        module = ast.parse(Path(runtime.__file__).read_text())
        execute = next(n for n in module.body if isinstance(n, ast.FunctionDef) and n.name == "execute")
        # Execute the real argument validation and client command forwarding in
        # isolation, without preparing a core or starting any external service.
        validation = next(n for n in execute.body if isinstance(n, ast.If) and
                          isinstance(n.test, ast.BoolOp) and
                          any(isinstance(v, ast.Name) and v.id == "static_sources" for v in n.test.values))
        namespace = {"static_sources": True, "fx": {"scene": "pool"}, "action": "idle",
                     "fault": "none", "identity": None, "moving_peers": False}
        tree = ast.fix_missing_locations(ast.Module(body=[validation], type_ignores=[]))
        with self.assertRaises(ValueError):
            exec(compile(tree, runtime.__file__, "exec"), namespace)
        namespace["fx"] = {"scene": "empty"}
        exec(compile(tree, runtime.__file__, "exec"), namespace)
        client = next(n for n in execute.body if isinstance(n, ast.FunctionDef) and n.name == "client")
        forwarding = next(n for n in client.body if isinstance(n, ast.If) and
                          isinstance(n.test, ast.Name) and n.test.id == "static_sources")
        tree = ast.fix_missing_locations(ast.Module(body=[forwarding], type_ignores=[]))
        for requested in (False, True):
            args = {"static_sources": requested, "command": ["runner"], "directory": Path("/fixture/role0")}
            exec(compile(tree, runtime.__file__, "exec"), args)
            expected = ["runner", "--static-intent-sources", Path("/fixture/role0/static-sources.jsonl")] if requested else ["runner"]
            self.assertEqual(args["command"], expected)

    def test_exact_appearance_and_paired_record_counterexamples(self):
        with tempfile.TemporaryDirectory() as directory:
            exe = Path(directory) / "reader-tests"
            subprocess.run(["rustc", "--edition=2021", "--test", str(SUPPORT / "static_ordinary_intent.rs"), "-o", str(exe)], check=True)
            subprocess.run([str(exe)], check=True)


if __name__ == "__main__":
    unittest.main()
