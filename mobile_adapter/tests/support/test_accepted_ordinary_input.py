"""Focused standalone wire authority, bounded lifetime, and causal counterexamples."""
from pathlib import Path
import os
import subprocess
import tempfile
import unittest

SUPPORT = Path(__file__).resolve().parent


class AcceptedOrdinaryInputTests(unittest.TestCase):
    def test_accepted_state_machine(self):
        source = r'''
#include "accepted_ordinary_input.hpp"
#include <cassert>
#include <vector>
using namespace accepted_ordinary_input;
using Wire = std::vector<uint8_t>;
void put(Wire& b, unsigned i, unsigned n) { b[i] = n >> 8; b[i+1] = n; }
Wire welcome() {
    Wire b(23); b[0]=0x50; b[1]=0x43; b[2]=1; b[3]=17;
    put(b,6,15); put(b,8,99); put(b,10,3); put(b,12,1); b[22]=4;
    for (unsigned i=14;i<22;++i) b[i]=i;
    return b;
}
Wire roster(unsigned n, unsigned kind=19) {
    Wire b(13+17*n); b[0]=0x50; b[1]=0x43; b[2]=1; b[3]=kind;
    put(b,4,0x1234); put(b,6,5+17*n); put(b,8,99); put(b,10,3); b[12]=n;
    for (unsigned i=0;i<n;++i) {
        unsigned r=13+i*17;
        put(b,r,i+1); put(b,r+2,7); put(b,r+4,11);
        b[r+6]=4; b[r+7]=5; put(b,r+8,0x1234); put(b,r+10,0x5678);
        b[r+12]=0xab; b[r+13]=0xcd; b[r+14]=0xef;
        b[r+15]=0xf1; b[r+16]=i;
    }
    return b;
}
// Independent fixture expansion, not Observer::parse_peer or its expected bytes.
Bytes20 expanded(const Wire& wire, unsigned i) {
    unsigned r=13+i*17; Bytes20 out{};
    for (unsigned j=0;j<12;++j) out[j]=wire[r+j];
    unsigned x=unsigned(wire[r+12])*16+wire[r+13]/16;
    unsigned y=unsigned(wire[r+13]%16)*256+wire[r+14];
    out[12]=x/256; out[13]=x%256; out[14]=y/256; out[15]=y%256;
    out[16]=wire[r+15]/16; out[17]=wire[r+15]%2;
    out[18]=wire[r+16]; out[19]=(wire[r+15]/2)%8; return out;
}
CommittedSlots slots(const Wire& b) {
    CommittedSlots s{}; for (unsigned i=0;i<b[12];++i) s[i]=expanded(b,i); return s;
}
Observer fresh() { Observer o; auto w=welcome(); assert(o.welcome(1,w.data(),w.size())); return o; }
void accept(Observer& o, const Wire& b) {
    auto tx=o.capture(o.instruction+1,b.data(),b.size()); assert(tx);
    assert(o.commit(o.instruction+1,tx,b.data(),13,slots(b)));
}
int main() {
    // Cold 0/1/2/3 memberships are all first-class accepted definitions.
    auto o=fresh();
    for (unsigned n=0;n<=3;++n) { auto b=roster(n); accept(o,b); assert(o.current.count==n); }
    assert(o.current.peers[2].x==0xabc && o.current.peers[2].y==0xdef);
    assert(o.current.peers[2].facing==15 && o.current.peers[2].avatar==2);
    assert(o.lobby==1 && o.capacity==4 && o.token[7]==21);
    // Literal native130 accepted wire: unavailable upload is membership, not pose.
    // Expected bank2 bytes are independently transcribed from protocol/ROM layout.
    const Wire actual_welcome={80,67,1,17,0,1,0,15,0,1,0,1,0,1,120,117,72,189,3,32,194,193,4};
    const Wire actual_empty={80,67,1,19,0,0,0,5,0,1,0,1,0};
    const Wire actual_unavailable={80,67,1,19,0,0,0,22,0,1,0,1,1,0,2,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0};
    const Wire actual_available={80,67,1,19,0,0,0,22,0,1,0,1,1,0,2,0,1,0,3,11,23,133,224,0,1,9,0,64,1,1};
    const Bytes20 unavailable_bytes={0,2,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
    const Bytes20 available_bytes={0,2,0,1,0,3,11,23,133,224,0,1,0,144,0,64,0,1,1,0};
    Observer actual;
    assert(actual.welcome(150784,actual_welcome.data(),actual_welcome.size()));
    auto actual_tx=actual.capture(224515,actual_empty.data(),actual_empty.size());
    assert(actual.commit(224540,actual_tx,actual_empty.data(),13,CommittedSlots{}));
    actual_tx=actual.capture(3113396,actual_unavailable.data(),actual_unavailable.size());
    CommittedSlots actual_slots{}; actual_slots[0]=unavailable_bytes;
    assert(actual.commit(3113811,actual_tx,actual_unavailable.data(),13,actual_slots));
    assert(actual.latest==2 && actual.current.count==1 && actual.current.peers[0].status==0);
    assert(actual.get(2)->peers[0].expanded==unavailable_bytes);
    actual_tx=actual.capture(3456360,actual_available.data(),actual_available.size());
    // Synthetic due opportunity during capture/commit proves unchanged latch.
    assert(actual.opportunity(3456500,254,3450559) && actual.latched.input==2);
    actual_slots[0]=available_bytes;
    unsigned actual_reads=0;
    auto actual_bank_read=[&](unsigned bank,unsigned at) -> uint8_t {
        assert(bank==2 && at>=0xd100 && at<0xd100+148); ++actual_reads;
        return actual_slots[(at-0xd100)/64][(at-0xd100)%64];
    };
    assert(actual.commit_bank2(3456803,actual_tx,actual_available.data(),13,0xd100,actual_bank_read));
    assert(actual_reads==60);
    assert(actual.latest==3 && actual.current.peers[0].status==1 && actual.latched.input==2);
    assert(actual.get(2)->peers[0].expanded==unavailable_bytes && actual.get(2)->peers[0].status==0);
    assert(actual.get(3)->peers[0].expanded==available_bytes);
    assert(actual.opportunity(3456804,255,3456803) && actual.latched.input==3);
    // Every committed byte remains authoritative during the initializing change.
    for (unsigned field=0;field<20;++field) {
        Observer bad; assert(bad.welcome(1,actual_welcome.data(),actual_welcome.size()));
        accept(bad,actual_unavailable);
        auto t=bad.capture(4,actual_available.data(),actual_available.size());
        auto corrupt=actual_slots; corrupt[0][field]^=1;
        assert(!bad.commit(5,t,actual_available.data(),13,corrupt));
        assert(bad.failure==Failure::committed_input && bad.current.peers[0].status==0);
    }
    // Status bit is authority: nonzero unavailable snapshots cannot initialize;
    // status1 with otherwise zero fields does initialize and cannot be a bypass.
    for (unsigned kind: {19u,24u}) {
        auto first=fresh(); auto unavailable=roster(1); unavailable[28]&=0xfe;
        accept(first,unavailable); assert(first.current.peers[0].status==0);
        auto changed=unavailable; changed[3]=kind; changed[25]^=1; changed[29]=3;
        accept(first,changed); assert(first.current.peers[0].status==0 && first.latest==2);
        changed[28]|=1; accept(first,changed); assert(first.current.peers[0].status==1);
        auto revision=changed; put(revision,17,12); accept(first,revision);
        for (unsigned byte=19;byte<30;++byte) {
            auto bad=first; auto unsupported=revision; unsupported[byte]^=1;
            auto t=bad.capture(bad.instruction+1,unsupported.data(),unsupported.size()); assert(t);
            assert(!bad.commit(bad.instruction+1,t,unsupported.data(),13,slots(unsupported)));
            assert(bad.failure==Failure::static_transition && bad.latest==4);
        }
        auto zero=fresh(); auto placeholder=roster(1);
        for (unsigned byte=19;byte<30;++byte) placeholder[byte]=0;
        placeholder[28]=1; accept(zero,placeholder);
        assert(zero.current.peers[0].status==1);
        placeholder[3]=kind; placeholder[25]=1;
        auto t=zero.capture(zero.instruction+1,placeholder.data(),placeholder.size()); assert(t);
        assert(!zero.commit(zero.instruction+1,t,placeholder.data(),13,slots(placeholder)));
        assert(zero.failure==Failure::static_transition);
    }
    // Availability follows accepted identity across roster order, not a physical slot.
    auto reordered=fresh(); auto pair=roster(2); pair[28]&=0xfe; accept(reordered,pair);
    auto swapped=pair;
    std::copy(pair.begin()+30,pair.begin()+47,swapped.begin()+13);
    std::copy(pair.begin()+13,pair.begin()+30,swapped.begin()+30);
    swapped[45]|=1; swapped[42]^=1; accept(reordered,swapped);
    assert(reordered.current.peers[0].id==2 && reordered.current.peers[0].status==1);
    assert(reordered.current.peers[1].id==1 && reordered.current.peers[1].status==1);
    // Removal ends ownership of the static pose; a new generation can join
    // only after removal. A retained generation replacement still fails strictly.
    auto rejoin=fresh(); accept(rejoin,roster(1)); accept(rejoin,roster(0));
    auto joined=roster(1); put(joined,15,8); joined[28]&=0xfe;
    accept(rejoin,joined); assert(rejoin.current.peers[0].status==0);
    joined[25]^=1; joined[28]|=1; accept(rejoin,joined);
    assert(rejoin.current.peers[0].status==1);
    for (unsigned kind: {19u,24u}) {
        auto bad=fresh(); auto unavailable=roster(1); unavailable[28]&=0xfe; accept(bad,unavailable);
        unavailable[3]=kind; put(unavailable,15,8); unavailable[28]|=1;
        auto t=bad.capture(bad.instruction+1,unavailable.data(),unavailable.size()); assert(t);
        assert(!bad.commit(bad.instruction+1,t,unavailable.data(),13,slots(unavailable)));
        assert(bad.failure==Failure::identity_transition && !bad.latest && bad.current.count==0);
    }
    // Trainer IDs have no input field and cannot collapse three identities.
    assert(o.current.peers[0].id!=o.current.peers[1].id && o.current.count==3);
    // The pending candidate spans arbitrary decode phases, without publication.
    auto phased=fresh(); auto b=roster(3); auto tx=phased.capture(2,b.data(),b.size());
    assert(tx && phased.latest==0 && phased.pending);
    assert(phased.opportunity(3,1,2) && phased.latched.input==0);
    assert(phased.opportunity(4,1,2) && phased.latched.input==0);
    assert(phased.commit(5,tx,b.data(),13,slots(b)) && phased.latest==1);
    assert(phased.latched.input==0); // Later inner work cannot change the outer latch.
    assert(phased.opportunity(6,2,5) && phased.latched.input==1);
    assert(phased.latched.source==2 && phased.latched.source_closed==5);
    auto before=fresh(); accept(before,roster(1));
    assert(before.opportunity(4,1,3) && before.latched.input==1);
    // Copy survives packed buffer mutation; matching committed independent bytes win.
    auto frozen=fresh(); auto wire=roster(1); auto expected=slots(wire);
    tx=frozen.capture(2,wire.data(),wire.size()); wire[29]=3;
    assert(frozen.candidate.frame[29]==0);
    assert(frozen.commit(3,tx,wire.data(),13,expected));
    auto mutated=fresh(); wire=roster(1); tx=mutated.capture(2,wire.data(),wire.size()); wire[29]=3;
    assert(!mutated.commit(3,tx,wire.data(),13,slots(wire)) && mutated.latest==0);
    assert(mutated.failure==Failure::committed_input);
    // Wrong receiver, malformed/short frames, duplicates, self, zero and invalid fields.
    for (unsigned mode=0;mode<10;++mode) {
        auto bad=fresh(); wire=roster(2);
        if (mode==0) wire[9]=98;
        if (mode==1) wire.pop_back();
        if (mode==2) put(wire,30,1);
        if (mode==3) put(wire,13,99);
        if (mode==4) put(wire,13,0);
        if (mode==5) put(wire,15,0);
        if (mode==6) wire[28]=0xfc; // movement6
        if (mode==7) wire[29]=4;
        if (mode==8) wire[6]=1;
        if (mode==9) wire[12]=4;
        assert(!bad.capture(2,wire.data(),wire.size()) && !bad.latest);
    }
    auto replaced=fresh(); wire=roster(1); tx=replaced.capture(2,wire.data(),wire.size());
    assert(!replaced.capture(3,wire.data(),wire.size()));
    assert(replaced.failure==Failure::pending_replaced);
    replaced.fail(Failure::malformed); assert(replaced.failure==Failure::pending_replaced);
    auto wrongtx=fresh(); tx=wrongtx.capture(2,wire.data(),wire.size());
    assert(!wrongtx.commit(3,tx+1,wire.data(),13,slots(wire)));
    auto overwrite=fresh(); tx=overwrite.capture(2,wire.data(),wire.size()); wire[4]^=1;
    assert(!overwrite.commit(3,tx,wire.data(),13,slots(wire)));
    auto cancelled=fresh(); wire=roster(1); tx=cancelled.capture(2,wire.data(),wire.size());
    assert(cancelled.cancel(3,tx) && !cancelled.latest);
    assert(!cancelled.commit(4,tx,wire.data(),13,slots(wire)));
    // All twenty committed input bytes are checked, including X/Y/facing/avatar.
    for (unsigned field=0;field<20;++field) {
        auto bad=fresh(); auto input=slots(wire); tx=bad.capture(2,wire.data(),wire.size());
        input[0][field]^=1;
        assert(!bad.commit(3,tx,wire.data(),13,input) && bad.failure==Failure::committed_input);
    }
    // Roster removal replaces membership; stale tail slots are not live identities.
    auto removed=fresh(); accept(removed,roster(3)); auto one=roster(1);
    std::copy(b.begin()+47,b.begin()+64,one.begin()+13); // retain old peer3 in new slot0
    auto stale=slots(b); stale[0]=expanded(one,0);
    tx=removed.capture(removed.instruction+1,one.data(),one.size());
    assert(removed.commit(removed.instruction+1,tx,one.data(),13,stale));
    assert(removed.current.count==1 && removed.current.peers[0].id==3);
    // Revision-only delta preserves membership; wire sequence is not transaction identity.
    auto delta=fresh(); accept(delta,roster(3)); wire=roster(1,24); put(wire,17,12);
    auto ds=slots(roster(3)); ds[0]=expanded(wire,0);
    tx=delta.capture(delta.instruction+1,wire.data(),wire.size());
    assert(delta.commit(delta.instruction+1,tx,wire.data(),13,ds));
    assert(delta.current.count==3 && delta.current.peers[0].revision==12);
    // Unknown and empty deltas, generation changes and fixed pose changes fail.
    for (unsigned mode=0;mode<5;++mode) {
        auto bad=fresh(); accept(bad,roster(1)); wire=roster(mode==1?0:1,24);
        if (mode==0) put(wire,13,55);
        if (mode==2) put(wire,15,8);
        if (mode==3) wire[25]^=1;
        if (mode==4) wire[29]^=1;
        tx=bad.capture(bad.instruction+1,wire.data(),wire.size()); assert(tx);
        assert(!bad.commit(bad.instruction+1,tx,wire.data(),13,slots(wire)));
        if (mode==0 || mode==2) assert(bad.current.count==0 && !bad.latest);
        else assert(bad.current.count==1); // Sticky failure cannot erase due demand into PASS.
    }
    auto reconnect=fresh(); accept(reconnect,roster(1)); auto w=welcome();
    assert(!reconnect.welcome(reconnect.instruction+1,w.data(),w.size()));
    assert(reconnect.lifetime==2 && reconnect.current.count==0 && reconnect.failure==Failure::lifetime);
    auto reset=fresh(); accept(reset,roster(1)); reset.retire(reset.instruction+1);
    assert(!reset.online && reset.lifetime==2 && reset.failure==Failure::retired);
    auto stale_life=fresh(); wire=roster(1); tx=stale_life.capture(2,wire.data(),wire.size());
    ++stale_life.lifetime;
    assert(!stale_life.commit(3,tx,wire.data(),13,slots(wire)));
    auto banked=fresh(); wire=roster(1); tx=banked.capture(2,wire.data(),wire.size());
    auto physical=slots(wire); unsigned reads=0;
    auto read=[&](unsigned bank,unsigned at) -> uint8_t {
        assert(bank==2 && at>=0xd100 && at<0xd100+148); ++reads;
        return physical[(at-0xd100)/64][(at-0xd100)%64];
    };
    assert(banked.commit_bank2(3,tx,wire.data(),13,0xd100,read) && reads==60);
    auto invalid_bank=fresh(); tx=invalid_bank.capture(2,wire.data(),wire.size());
    assert(!invalid_bank.commit_bank2(3,tx,wire.data(),13,0xdfff,read) && reads==60);
    auto ord_overflow=fresh(); ord_overflow.instruction=UINT64_MAX;
    assert(!ord_overflow.opportunity(UINT64_MAX,1,2) && ord_overflow.failure==Failure::overflow);
    auto late_capture=fresh(); wire=roster(1);
    assert(!late_capture.capture(2,wire.data(),wire.size(),2) && !late_capture.latest);
    auto source_reuse=fresh(); assert(source_reuse.opportunity(3,1,2));
    assert(!source_reuse.opportunity(5,1,4) && source_reuse.failure==Failure::missing_source);
    auto no_source=fresh(); assert(!no_source.opportunity(2,0,0));
    auto future=fresh(); assert(!future.opportunity(2,1,2));
    auto ordinal=fresh(); assert(!ordinal.opportunity(1,1,1));
    auto overflow=fresh(); overflow.next_transaction=UINT64_MAX;
    assert(!overflow.capture(2,wire.data(),wire.size()) && overflow.failure==Failure::overflow);
    auto pressure=fresh(); for (unsigned i=0;i<8;++i) accept(pressure,roster(0));
    tx=pressure.capture(pressure.instruction+1,wire.data(),wire.size()); assert(tx);
    assert(!pressure.commit(pressure.instruction+1,tx,wire.data(),13,slots(wire)));
    assert(pressure.failure==Failure::definition_pressure && pressure.latest==8);
    auto ring=fresh(); for (unsigned i=0;i<8;++i) accept(ring,roster(0));
    assert(ring.release_through(7)); accept(ring,roster(0)); assert(ring.get(9));
    assert(!ring.get(1) && ring.failure==Failure::missing_definition);
}
'''
        cache = Path('/Volumes/Storage/Cache/mobileadapter/m2-accepted-input-88-20261014')
        cache.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(dir=cache) as directory:
            cpp = Path(directory) / 'accepted.cpp'
            exe = Path(directory) / 'accepted'
            cpp.write_text(source)
            env = dict(os.environ, TMPDIR=str(cache))
            subprocess.run(['c++', '-std=c++17', '-Wall', '-Wextra', '-Werror',
                            '-I', str(SUPPORT), str(cpp), '-o', str(exe)], check=True, env=env)
            subprocess.run([str(exe)], check=True, env=env)


if __name__ == '__main__':
    unittest.main()
