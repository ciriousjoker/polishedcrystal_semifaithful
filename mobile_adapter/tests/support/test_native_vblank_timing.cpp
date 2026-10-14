#include "native_vblank_timing.hpp"
#include <cassert>
#include <memory>
using namespace native_vblank_timing;
static Sample sample(uint16_t pc, uint16_t sp = 0xd000, uint16_t bank = 0, uint32_t t = 0) {
    Sample s; s.pc = pc; s.sp = sp; s.bank = bank; s.core_clock = t;
    s.ly = 144; s.lcdc = 0x80; s.speed = 1; return s;
}
static std::unique_ptr<Observer> fresh(std::size_t limit = Observer::max_events) {
    auto o = std::make_unique<Observer>(); assert(o->configure(nullptr, 0, limit)); return o;
}
static void irq(Observer& o, uint32_t t=0) {
    assert(o.irq_dispatch(sample(0x4567,0xd000,5,t), sample(0x40,0xcffe,0,t), 0x4567));
}
static void instruction(Observer& o, Sample a, Sample b, unsigned op, unsigned cycles) {
    assert(o.before(a, [](uint16_t) { return uint16_t(0x8888); }));
    assert(o.after(b,op,cycles));
}
static void close(Observer& o, uint32_t t=0) {
    instruction(o,sample(0x200,0xcffe,0,t),sample(0x4567,0xd000,5,t+16),0xd9,16);
}
int main() {
    // Unwindowed CPU T, including pre-IRQ instructions; no invented 20 T entry.
    auto o=fresh(); instruction(*o,sample(0x100),sample(0x101,0xd000,0,4),0,4);
    irq(*o,4); close(*o,4); assert(o->finish());
    assert(o->instruction_t()==20 && o->core_t()==20 && o->completed()==1);
    assert(o->size()==2 && o->event(0)->instruction_t==4 && !o->event(2));
    // 32-bit core wrap and GDMA stall remain distinct from instruction T.
    o=fresh(); instruction(*o,sample(1,0xd000,0,0xfffffffcu),sample(2,0xd000,0,4),0,4);
    instruction(*o,sample(2,0xd000,0,104),sample(3,0xd000,0,108),0,4);
    assert(o->core_t()==112 && o->instruction_t()==8);
    o=fresh(); assert(o->before(sample(1,0xd000,0,100),[](uint16_t){return 0;}));
    assert(!o->after(sample(2,0xd000,0,99),0,4)); assert(o->failure()==Failure::clock_discontinuity);
    // Non-EXSACT_CORE HALT can legitimately report zero instruction T in this strict core.
    o=fresh(); instruction(*o,sample(1),sample(2),0x76,0);
    assert(o->instruction_t()==0);
    o=fresh(); assert(o->before(sample(1),[](uint16_t){return 0;}));
    assert(!o->after(sample(2),0,3)); assert(o->failure()==Failure::clock_overflow);
    // Merely visiting vector 40 never establishes genuine IRQ credit.
    o=fresh(); instruction(*o,sample(0x40),sample(0x41,0xd000,0,4),0,4);
    assert(!o->active() && o->size()==0 && o->completed()==0);
    for(unsigned bad=0;bad<3;++bad) {
        o=fresh(); auto post=sample(0x40,0xcffe); auto pc=uint16_t(0x4567);
        if(bad==0) post.sp=0xcfff; if(bad==1) post.bank=1; if(bad==2) ++pc;
        assert(!o->irq_dispatch(sample(0x4567,0xd000,5),post,pc));
        assert(o->failure()==Failure::irq_entry);
    }
    for(unsigned bad=0;bad<4;++bad) {
        o=fresh(); irq(*o); auto end=sample(0x4567,0xd000,5,16);
        if(bad==0) end.pc++; if(bad==1) end.sp++; if(bad==2) end.bank++;
        assert(o->before(sample(0x200,0xcffe),[](uint16_t){return 0;}));
        assert(!o->after(end,bad==3?0xc9:0xd9,16));
        assert(o->completed()==0 && o->failure()==Failure::return_mismatch);
    }
    // Ordinary nested RET and nested real timer RETI cannot close the outer IRQ.
    o=fresh(); irq(*o);
    instruction(*o,sample(0x300,0xcffc),sample(0x301,0xcffe,0,16),0xc9,16);
    assert(o->active() && o->completed()==0);
    assert(o->irq_dispatch(sample(0x201,0xcffe,0,16),sample(0x50,0xcffc,0,16),0x201));
    instruction(*o,sample(0x350,0xcffc,0,16),sample(0x201,0xcffe,0,32),0xd9,16);
    assert(o->active()); close(*o,32); assert(o->completed()==1);
    o=fresh(); irq(*o); assert(!o->irq_dispatch(sample(0x201,0xcffe),sample(0x40,0xcffc),0x201));
    assert(o->failure()==Failure::nested_vblank);
    // Jobs are exact bank/PC entry plus actual matching full return.
    o=std::make_unique<Observer>(); Point points[]={{0,0x300,1,true},{2,0x4500,2,false}};
    assert(o->configure(points,2)); assert(o->job_return_bank(1,2)); irq(*o);
    assert(o->before(sample(0x300,0xcffc),[](uint16_t){return uint16_t(0x4501);}));
    assert(o->after(sample(0x301,0xcffc,0,4),0,4));
    instruction(*o,sample(0x302,0xcffc,0,4),sample(0x4501,0xcffe,2,20),0xc9,16);
    close(*o,20); assert(o->size()==4 && o->event(2)->kind==Kind::job_return);
    // A configured but skipped LY-gated point produces no fabricated event.
    assert(o->event(1)->id==1 && o->event(2)->id==1);
    // Conditional RET not taken cannot fake a job return.
    o=std::make_unique<Observer>(); assert(o->configure(points,2));
    assert(o->job_return_bank(1,2)); irq(*o);
    assert(o->before(sample(0x300,0xcffc),[](uint16_t){return uint16_t(0x4501);}));
    assert(o->after(sample(0x301,0xcffc,0,8),0xc0,8)); assert(o->size()==2);
    instruction(*o,sample(0x302,0xcffc,0,8),sample(0x4501,0xcffe,2,28),0xc0,20);
    close(*o,28); assert(o->completed()==1);
    // Entry without configured caller-bank provenance must fail.
    o=std::make_unique<Observer>(); assert(o->configure(points,2)); irq(*o);
    assert(!o->before(sample(0x300,0xcffc),[](uint16_t){return uint16_t(0x4501);}));
    assert(o->failure()==Failure::configuration);
    // Fixed event storage and nested IRQ/job stack never silently truncate.
    o=fresh(); irq(*o);
    for(unsigned i=1;i<Observer::max_depth;++i)
        assert(o->irq_dispatch(sample(0x201,uint16_t(0xd000 - 2*i)),sample(0x50,uint16_t(0xcffe - 2*i)),0x201));
    assert(!o->irq_dispatch(sample(0x201,0xcfe0),sample(0x50,0xcfde),0x201));
    assert(o->failure()==Failure::stack_overflow);
    o=std::make_unique<Observer>(); Point duplicate[]={{0,0x300,1,false},{0,0x300,2,false}};
    assert(!o->configure(duplicate,2)); assert(o->failure()==Failure::configuration);
    // DMA start is not completion; HRAM delay completion is explicitly distinct.
    o=fresh(); irq(*o); assert(o->dma_start(sample(0xff80,0xcffc),0xc0));
    assert(o->event(1)->kind==Kind::dma_start); assert(!o->finish());
    assert(o->failure()==Failure::scope_open);
    o=fresh(); irq(*o); assert(o->dma_start(sample(0xff80,0xcffc),0xc0));
    assert(o->dma_wait_complete(sample(0xff88,0xcffc,0,640))); close(*o,640);
    assert(o->event(2)->kind==Kind::dma_wait_complete && o->instruction_t()==16 && o->core_t()==656);
    // No first partial interval credit; retained and repeated writes are genuine.
    assert(o->output_row0(sample(0x100,0xd000,5,700))); assert(o->intervals()==0);
    assert(o->output_row0(sample(0x100,0xd000,5,800))); assert(o->retained()==1);
    assert(o->dma_start(sample(0xff80,0xcffc,0,810),0xc0));
    assert(o->dma_start(sample(0xff80,0xcffc,0,820),0xc0));
    assert(o->output_row0(sample(0x100,0xd000,5,900)));
    assert(o->intervals()==2 && o->repeated()==1 && o->event(o->size()-1)->value==2);
    o=fresh(1); irq(*o); assert(o->before(sample(0x200,0xcffe),[](uint16_t){return 0;}));
    assert(!o->after(sample(0x4567,0xd000,5,16),0xd9,16));
    assert(o->failure()==Failure::event_overflow); o->restored(); assert(o->failure()==Failure::event_overflow);
    o=fresh(); o->restored(); assert(o->failure()==Failure::restored && !o->finish());
    o=fresh(); assert(!o->after(sample(1),0,4)); assert(o->failure()==Failure::instruction_pair);
    o=fresh(); irq(*o); assert(!o->finish()); assert(o->failure()==Failure::scope_open);
    // Review94 counterexample: IRQ1 cannot return with an unfinished wait and
    // later let IRQ2 complete it at arbitrary PC2222. Failure is permanent.
    o=fresh(); irq(*o); assert(o->dma_start(sample(0xff80,0xcffc),0xc0));
    assert(o->before(sample(0x200,0xcffe),[](uint16_t){return 0;}));
    assert(!o->after(sample(0x4567,0xd000,5,16),0xd9,16));
    assert(o->failure()==Failure::dma_pair && o->active() && o->completed()==0);
    assert(o->size()==2); // No fabricated IRQ return, and no lost pending pair.
    assert(!o->irq_dispatch(sample(0x4567,0xd000,5,16),sample(0x40,0xcffe,0,16),0x4567));
    assert(!o->dma_wait_complete(sample(0x2222,0xcffe,0,16)));
    assert(!o->finish() && o->failure()==Failure::dma_pair);
    // Review94 paired dispatch rejection must latch even if caller continues.
    o=fresh(); assert(o->before(sample(0x100),[](uint16_t){return 0;}));
    assert(!o->irq_dispatch(sample(0x101),sample(0x40,0xcffe),0x101));
    assert(o->failure()==Failure::instruction_pair);
    assert(!o->after(sample(0x101,0xd000,0,4),0,4));
    o->restored(); assert(!o->finish() && o->failure()==Failure::instruction_pair);
    // Zero starts need no completion; multiple completed pairs retain the same
    // owner and are independently recorded with FF46 page provenance.
    o=fresh(); irq(*o); close(*o); assert(o->finish());
    o=fresh(); irq(*o);
    for (unsigned i=0;i<2;++i) {
        assert(o->dma_start(sample(0xff80,0xcffc,0,640*i),uint8_t(0xc0+i)));
        assert(o->dma_wait_complete(sample(0xff88,0xcffc,0,640*(i+1))));
    }
    close(*o,1280); assert(o->finish());
    assert(o->event(1)->value==0xc0 && o->event(3)->value==0xc1);
    for (unsigned i=1;i<6;++i) assert(o->event(i)->scope==o->event(0)->scope);
    // A second unfinished start fails instead of overwriting its owner/page.
    o=fresh(); irq(*o); assert(o->dma_start(sample(0xff80,0xcffc),0xc0));
    assert(!o->dma_start(sample(0xff80,0xcffc),0xc1));
    assert(o->size()==2 && o->event(1)->value==0xc0 && !o->finish());
    assert(o->failure()==Failure::dma_pair);
    // A nested IRQ cannot complete an outer wait or overwrite its pending pair.
    for (unsigned start=0;start<2;++start) {
        o=fresh(); irq(*o); assert(o->dma_start(sample(0xff80,0xcffc),0xc0));
        assert(o->irq_dispatch(sample(0x201,0xcffe),sample(0x50,0xcffc),0x201));
        assert(o->event(2)->scope!=o->event(1)->scope);
        if (start) assert(!o->dma_start(sample(0xff80,0xcffa),0xc1));
        else assert(!o->dma_wait_complete(sample(0xff88,0xcffa)));
        assert(o->failure()==Failure::dma_pair && !o->finish());
    }
    // Nested IRQ without its own DMA can return, resuming the outer wait.
    o=fresh(); irq(*o); assert(o->dma_start(sample(0xff80,0xcffc),0xc0));
    assert(o->irq_dispatch(sample(0x201,0xcffe),sample(0x50,0xcffc),0x201));
    instruction(*o,sample(0x350,0xcffc),sample(0x201,0xcffe,0,16),0xd9,16);
    assert(o->event(2)->scope==o->event(3)->scope);
    assert(o->dma_wait_complete(sample(0xff88,0xcffc,0,640)));
    assert(o->event(1)->scope==o->event(4)->scope); close(*o,640); assert(o->finish());
    // An unfinished nested IRQ wait fails at that IRQ return, before outer RETI.
    o=fresh(); irq(*o);
    assert(o->irq_dispatch(sample(0x201,0xcffe),sample(0x50,0xcffc),0x201));
    assert(o->dma_start(sample(0xff80,0xcffa),0xc0));
    assert(o->before(sample(0x350,0xcffc),[](uint16_t){return 0;}));
    assert(!o->after(sample(0x201,0xcffe,0,16),0xd9,16));
    assert(o->failure()==Failure::dma_pair && o->completed()==0 && !o->finish());
    // Nested IRQs may own their own completed pair when none is pending.
    o=fresh(); irq(*o);
    assert(o->irq_dispatch(sample(0x201,0xcffe),sample(0x50,0xcffc),0x201));
    assert(o->dma_start(sample(0xff80,0xcffa),0xc0));
    assert(o->dma_wait_complete(sample(0xff88,0xcffa,0,640)));
    instruction(*o,sample(0x350,0xcffc,0,640),sample(0x201,0xcffe,0,656),0xd9,16);
    assert(o->event(1)->scope==o->event(2)->scope && o->event(2)->scope==o->event(3)->scope);
    close(*o,656); assert(o->finish());
    const auto first_scope=o->event(0)->scope;
    irq(*o,672); close(*o,672); assert(o->finish() && o->completed()==2);
    assert(o->event(6)->scope!=first_scope && o->event(6)->irq==2);
    // The same ownership rule covers authenticated job frames, not only IRQs.
    for (unsigned nested=0;nested<2;++nested) {
        o=std::make_unique<Observer>(); assert(o->configure(points,2));
        assert(o->job_return_bank(1,2)); irq(*o);
        if (nested) assert(o->dma_start(sample(0xff80,0xcffc),0xc0));
        assert(o->before(sample(0x300,0xcffc),[](uint16_t){return uint16_t(0x4501);}));
        assert(o->after(sample(0x301,0xcffc,0,4),0,4));
        if (nested) {
            assert(!o->dma_wait_complete(sample(0xff88,0xcffa,0,4)));
        } else {
            assert(o->dma_start(sample(0xff80,0xcffa,0,4),0xc0));
            assert(o->before(sample(0x302,0xcffc,0,4),[](uint16_t){return 0;}));
            assert(!o->after(sample(0x4501,0xcffe,2,20),0xc9,16));
        }
        assert(o->failure()==Failure::dma_pair && !o->finish());
    }
    // An independently authenticated HRAM return may itself close the owner
    // job. Atomic after completion sees post-instruction clocks before pop.
    o=std::make_unique<Observer>(); assert(o->configure(points,2));
    assert(o->job_return_bank(1,2)); irq(*o);
    assert(o->before(sample(0x300,0xcffc),[](uint16_t){return uint16_t(0x4501);}));
    assert(o->after(sample(0x301,0xcffc,0,4),0,4));
    assert(o->dma_start(sample(0xff80,0xcffa,0,4),0xc0));
    assert(o->before(sample(0x302,0xcffc,0,640),[](uint16_t){return 0;}));
    assert(o->after(sample(0x4501,0xcffe,2,656),0xc9,16,true));
    assert(o->event(3)->kind==Kind::dma_wait_complete);
    assert(o->event(4)->kind==Kind::job_return);
    assert(o->event(2)->scope==o->event(3)->scope && o->event(3)->scope==o->event(4)->scope);
    assert(o->event(3)->instruction_t==20 && o->event(3)->core_t==656);
    close(*o,656); assert(o->finish());
    // A completion flag cannot make a non-return or wrong SP return complete.
    for (unsigned bad=0;bad<2;++bad) {
        o=fresh(); irq(*o); assert(o->dma_start(sample(0xff80,0xcffc),0xc0));
        assert(o->before(sample(0xff88,0xcffc),[](uint16_t){return 0;}));
        assert(!o->after(sample(0x201,bad?0xcffc:0xcffe,0,16),bad?0xc9:0,16,true));
        assert(o->failure()==Failure::dma_pair && !o->finish());
    }
    // CB D9 is SET 3,C, not RETI: callers preserve the original CB byte.
    o=fresh(); irq(*o);
    instruction(*o,sample(0x200,0xcffe),sample(0x202,0xcffe,0,8),0xcb,8);
    close(*o,8); assert(o->finish() && o->completed()==1);
    // Deliberately violating that caller precondition reproduces the pitfall.
    o=fresh(); irq(*o); assert(o->before(sample(0x200,0xcffe),[](uint16_t){return 0;}));
    assert(!o->after(sample(0x202,0xcffe,0,8),0xd9,8));
    assert(o->failure()==Failure::return_mismatch);

}
