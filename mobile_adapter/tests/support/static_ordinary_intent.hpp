// Bounded independent ordinary-source observations. No feature descriptors or caches.
#pragma once
#include <array>
#include <cstdint>
#include <limits>
namespace static_ordinary_intent {
constexpr unsigned actor_count = 13, actor_stride = 34, source_capacity = 8;
constexpr unsigned camera_bytes = 2, input_bytes = actor_count * actor_stride + camera_bytes;
struct Point { unsigned bank = 0, pc = 0; };
struct Source {
    uint64_t id = 0, entry = 0, returned = 0, closed = 0;
    std::array<unsigned char, input_bytes> original{}, at_return{}, at_close{};
    uint16_t emitted = 0;
};
// Each definition remains live until the consumer explicitly releases it. Ring
// pressure is an observer failure, never permission to replace a referenced ID.
struct Observer {
    std::array<Point, 4> points{}; // producer, emitter, pre-limiter, outer opportunity
    unsigned objects = 0, object_bank = 0, camera_x = 0, camera_y = 0;
    uint64_t instruction = 0, next_id = 0, released = 0, latest = 0, opportunity = 0;
    unsigned failure = 0, pre_sp = 0, call_sp = 0, return_pc = 0, return_bank = 0;
    bool enabled = false, started = false, active = false, has_return = false;
    Source pending{};
    std::array<Source, source_capacity> sources{};
    void fail(unsigned reason) { if (!failure) failure = reason; }
    void reset() {
        const auto configured_points = points;
        const auto configured_objects = objects, configured_bank = object_bank;
        const auto configured_x = camera_x, configured_y = camera_y;
        *this = Observer{};
        points = configured_points; objects = configured_objects; object_bank = configured_bank;
        camera_x = configured_x; camera_y = configured_y;
    }
    template<class Read> std::array<unsigned char, input_bytes> capture(Read read) const {
        std::array<unsigned char, input_bytes> out{};
        for (unsigned i = 0; i < actor_count * actor_stride; ++i)
            out[i] = read(object_bank, objects + i);
        out[input_bytes - 2] = read(object_bank, camera_x);
        out[input_bytes - 1] = read(object_bank, camera_y);
        return out;
    }
    bool matches(unsigned i, unsigned bank, unsigned pc) const {
        return points[i].pc && points[i].pc == pc && points[i].bank == bank;
    }
    template<class Read> void before(unsigned bank, unsigned pc, unsigned sp, unsigned bc, Read read) {
        if (!enabled || failure) return;
        if (instruction == std::numeric_limits<uint64_t>::max()) { fail(1); return; }
        ++instruction;
        pre_sp = sp;
        // Arming may happen mid-call. Observation starts at the next genuine
        // producer entry, and its first closed source is the read-only arm marker.
        if (!started && !matches(0, bank, pc)) return;
        started = true;
        if (matches(0, bank, pc)) {
            if (active || has_return) { fail(2); return; }
            active = true; pending = Source{}; pending.entry = instruction;
            pending.original = capture(read);
            call_sp = sp; return_pc = read(8, sp) | read(8, (sp + 1) & 0xffff) << 8;
            return_bank = return_pc < 0x4000 ? 0 : bank;
        }
        if (matches(1, bank, pc)) {
            if (!active || bc < objects || (bc - objects) % actor_stride ||
                    (bc - objects) / actor_stride >= actor_count) { fail(3); return; }
            unsigned bit = 1u << ((bc - objects) / actor_stride);
            if (pending.emitted & bit) { fail(4); return; }
            pending.emitted |= bit;
        }
        if (matches(2, bank, pc)) {
            if (!has_return || active) { fail(5); return; }
            if (next_id - released >= source_capacity) { fail(6); return; }
            pending.closed = instruction; pending.at_close = capture(read);
            pending.id = ++next_id; sources[(next_id - 1) % source_capacity] = pending;
            latest = next_id; has_return = false;
        }
        if (matches(3, bank, pc)) {
            // Observe the opportunity regardless of feature readiness. A missing
            // completed source is explicit evidence, not deferred eligibility.
            // A newer unfinished producer invalidates the previous definition
            // for this opportunity. Repeats after a genuine close remain valid.
            if (active || has_return || !latest) { fail(7); return; }
            // Releasing a definition must not let it acquire a new reference.
            if (!get(latest)) return;
            opportunity = latest;
        }
    }
    template<class Read> void after(unsigned bank, unsigned pc, unsigned sp, bool returned, Read read) {
        if (!enabled || failure || !active || !returned) return;
        if (pre_sp == call_sp && sp == ((call_sp + 2) & 0xffff) &&
                pc == return_pc && bank == return_bank) {
            pending.returned = instruction; pending.at_return = capture(read);
            active = false; has_return = true;
        }
    }
    const Source* get(uint64_t id) {
        if (!id || id <= released || id > next_id || next_id - id >= source_capacity) {
            fail(8); return nullptr;
        }
        const auto& source = sources[(id - 1) % source_capacity];
        if (source.id != id) { fail(8); return nullptr; }
        return &source;
    }
    void release(uint64_t id) {
        if (id < released || id > next_id || (opportunity && id >= opportunity)) { fail(9); return; }
        released = id;
    }
};
}
