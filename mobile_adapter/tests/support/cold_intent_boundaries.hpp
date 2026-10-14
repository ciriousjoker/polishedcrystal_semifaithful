// Tiny diagnostic ledger. Never owns or mutates the source/input observers.
#pragma once
#include <array>
#include <cstdint>
namespace cold_intent_boundaries {
constexpr uint64_t absent = ~uint64_t(0);
enum Kind : unsigned { activation=1, source_entry, source_return, source_close, outer, row0, raster, display, callback };
enum Phase : unsigned { frontend=0, before_cpu, after_cpu, render_begin, render_line, display_begin, frontend_callback };
enum Field : unsigned { kind, phase, instruction, host, ly, raster_id, display_id,
    source, pending_entry, pending_return, pending_close, source_active, source_returned,
    latch_op, latch_instruction, latch_source, latch_closed, latch_input,
    current_input, lifetime, receiver, generation, input_active, input_failure,
    world_ready, scene_ready, map_group, map_number, map_status, script,
    cpu_phase, source_failure, invalid, field_count };
using Record = std::array<uint64_t, field_count>;
struct Ledger {
    bool configured=false, active=false, missing=false, first_row=false;
    unsigned count=0, cpu=frontend;
    std::array<Record,16> records{};
    Record witness{};
    const void* display_buffer=nullptr;
    bool callback_pending=false;
    uint64_t display_raster=absent, display_token=absent;
    void reset() { bool declaration=configured; *this=Ledger{}; configured=declaration; }
    static bool causal(const Record& r) {
        return r[latch_op] && r[latch_source] && r[latch_closed] &&
            r[latch_closed]<r[latch_instruction] &&
            (r[latch_instruction]<r[instruction] ||
             (r[latch_instruction]==r[instruction] && r[cpu_phase]==after_cpu));
    }
    void push(Record r) {
        if (!configured || (!active && r[kind]!=activation)) return;
        if (r[kind]==activation) { if(active) return; active=true; }
        if (r[kind]==row0 && !first_row) {
            first_row=true;
            if (!causal(r)) { missing=true; witness=r; }
        }
        if(count<records.size()) records[count++]=r;
    }
    // ABI1: section0/index0 status [ABI,declared,active,count,missing,first-row,fields];
    // section1 bounded records, section2/index0 immutable missing witness.
    // No getter validates, releases or changes either independent observer.
    uint64_t value(unsigned section,unsigned index,unsigned field) const {
        if(section==0) {
            const uint64_t s[]={1,configured,active,count,missing,first_row,field_count};
            return index==0 && field<7 ? s[field] : absent;
        }
        if(field>=field_count) return absent;
        if(section==1) return index<count ? records[index][field] : absent;
        if(section==2) return missing && index==0 ? witness[field] : absent;
        return absent;
    }
};
}
