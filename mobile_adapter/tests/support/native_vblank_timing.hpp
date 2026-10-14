#ifndef MOBILE_NATIVE_VBLANK_TIMING_HPP
#define MOBILE_NATIVE_VBLANK_TIMING_HPP

#include <array>
#include <cstddef>
#include <cstdint>
#include <limits>

// Read-only observation, not a timing acceptance oracle. CPU T are instruction
// cycles in strict ef247c6. Core clock also includes GDMA stalls, but omits the
// hardware 20 T IRQ dispatch. LY is batch-updated by this core. No dot is inferred.
namespace native_vblank_timing {
enum class Failure : uint8_t {
    none, configuration, clock_overflow, clock_discontinuity, instruction_pair,
    irq_entry, nested_vblank, stack_overflow, return_mismatch, event_overflow,
    scope_open, dma_pair, restored
};
enum class Kind : uint8_t {
    irq_entry, irq_return, point, job_entry, job_return, dma_start,
    dma_wait_complete, output_interval
};
struct Sample {
    uint16_t pc = 0, sp = 0, bank = 0;
    uint32_t core_clock = 0;
    uint8_t ly = 0, lcdc = 0, mode = 0, speed = 0;
    // Named by the immutable run configuration, never synthesized job success.
    std::array<uint8_t, 8> pending{};
};
struct Point {
    uint16_t bank = 0, pc = 0, id = 0;
    bool job = false;
};
struct Event {
    Kind kind{};
    uint16_t id = 0;
    uint64_t instruction_t = 0, core_t = 0, irq = 0, scope = 0;
    Sample sample{};
    uint32_t value = 0;
};
class Observer {
public:
    static constexpr std::size_t max_points = 32, max_events = 4096, max_depth = 16;
    // A 32-bit get_clock wrap is supported. Larger gaps/restores cannot be
    // distinguished without an explicit restore notification: fail at >=2^31 T.
    bool configure(const Point* points, std::size_t count, std::size_t limit = max_events) {
        if (configured_ || count > max_points || !limit || limit > max_events || (count && !points))
            return fail(Failure::configuration);
        for (std::size_t i = 0; i < count; ++i) {
            if (!points[i].id) return fail(Failure::configuration);
            for (std::size_t j = 0; j < i; ++j)
                if (points[j].id == points[i].id ||
                    (points[j].pc == points[i].pc && points[j].bank == points[i].bank))
                    return fail(Failure::configuration);
            points_[i] = points[i];
        }
        point_count_ = count; limit_ = limit; configured_ = true; return true;
    }
    // Called only in the actual irq_process branch, after the real push/vector
    // assignment. 'pre' is after HALT PC correction; stacked_pc is read back from
    // actual stack RAM. A PC=0040 before_instruction alone is not an IRQ entry.
    bool irq_dispatch(const Sample& pre, const Sample& post, uint16_t stacked_pc) {
        if (!healthy()) return false;
        if (paired_) return fail(Failure::instruction_pair);
        if (!clock(pre) || !clock(post)) return false;
        if (post.pc < 0x40 || post.pc > 0x60 || (post.pc & 7) || post.bank ||
            post.sp != uint16_t(pre.sp - 2) || stacked_pc != pre.pc)
            return fail(Failure::irq_entry);
        if (!active_ && post.pc != 0x40) return true;
        if (active_ && post.pc == 0x40) return fail(Failure::nested_vblank);
        if (depth_ == max_depth) return fail(Failure::stack_overflow);
        if (!active_) { active_ = true; seen_irq_ = true; if (!increment(irq_)) return false; }
        if (!increment(scope_)) return false;
        frames_[depth_++] = {post.sp, pre.pc, pre.bank, 0, true, scope_};
        return emit(Kind::irq_entry, post.pc, post);
    }
    template<class Read16>
    bool before(const Sample& s, Read16 read_stack) {
        if (!healthy() || paired_) return fail(Failure::instruction_pair);
        if (!clock(s)) return false;
        pre_ = s; paired_ = true;
        if (!active_) return true;
        for (std::size_t i = 0; i < point_count_; ++i) {
            const auto& p = points_[i];
            if (s.pc != p.pc || s.bank != p.bank) continue;
            if (p.job) {
                if (!return_bank_set_[i]) return fail(Failure::configuration);
                if (depth_ == max_depth) return fail(Failure::stack_overflow);
                const uint16_t pc = read_stack(s.sp);
                // Caller bank is supplied by actual bank mapping at return:
                // banked job entry alone cannot recover a different caller bank.
                // Scope return bank must therefore be configured via job_return_bank.
                if (!increment(scope_)) return false;
                frames_[depth_++] = {s.sp, pc, return_bank_[i], p.id, false, scope_};
            }
            return emit(p.job ? Kind::job_entry : Kind::point, p.id, s);
        }
        return true;
    }
    // Set from symbol/caller provenance, not from the eventual observed return.
    bool job_return_bank(uint16_t id, uint16_t bank) {
        if (seen_irq_ || paired_) return fail(Failure::configuration);
        for (std::size_t i = 0; i < point_count_; ++i)
            if (points_[i].id == id && points_[i].job) { return_bank_[i] = bank; return_bank_set_[i] = true; return true; }
        return fail(Failure::configuration);
    }
    // opcode must be the preserved original first opcode byte. In strict
    // cpu::exec the local op_code is overwritten by the CB subopcode: passing
    // that local here can falsely classify CB D9 as RETI. Pass 0xCB instead.
    // If the authenticated HRAM return also closes its configured job frame,
    // pass authenticated_dma_wait_return=true here. The caller must validate
    // the exact HRAM PC/SP/caller from pre/post instruction provenance first.
    // This records completion after clocks advance but before the owner pops;
    // no speculative completion callback before post-instruction observation.
    bool after(const Sample& s, unsigned opcode, unsigned cycles,
               bool authenticated_dma_wait_return = false) {
        if (!healthy() || !paired_) return fail(Failure::instruction_pair);
        paired_ = false;
        if ((!cycles && opcode != 0x76) || (cycles & 3) || instruction_t_ > UINT64_MAX - cycles)
            return fail(Failure::clock_overflow);
        instruction_t_ += cycles;
        if (!clock(s)) return false;
        const bool ret = opcode == 0xc9 || opcode == 0xd9 || ((opcode & 0xe7) == 0xc0 && cycles == 20);
        if (authenticated_dma_wait_return) {
            if (!ret || s.sp != uint16_t(pre_.sp + 2)) return fail(Failure::dma_pair);
            if (!dma_wait_complete(s)) return false;
        }
        if (!active_ || !ret) return true;
        const auto f = frames_[depth_ - 1];
        if (pre_.sp != f.sp) return true; // Ordinary inner return, no fake closure.
        if (s.sp != uint16_t(f.sp + 2) || s.pc != f.pc || s.bank != f.bank || (f.irq && opcode != 0xd9))
            return fail(Failure::return_mismatch);
        // Never let an unfinished wait escape its validated owner, even when
        // a later IRQ/job would reuse the same stack address.
        if (dma_pending_ && dma_owner_ == f.scope) return fail(Failure::dma_pair);
        --depth_;
        if (!emit(f.irq ? Kind::irq_return : Kind::job_return, f.id, s, 0, f.scope)) return false;
        if (!depth_) { active_ = false; if (!increment(completed_)) return false; }
        return true;
    }
    // io_write(FF46), inside an instruction: sample has pre-instruction CPU T.
    // The strict core copies OAM synchronously. This is not physical completion.
    // Outside IRQ: interval count only, no pending HRAM wait. Inside IRQ: zero
    // or multiple sequential pairs are allowed, with at most one unfinished
    // pair. Its owner is the current validated IRQ/job frame. A nested start
    // cannot overwrite it; an unresolved owner return fails without clearing it.
    bool dma_start(const Sample& s, uint8_t page) {
        if (!healthy() || !clock(s)) return false;
        if (seen_irq_ && !increment(dma_starts_)) return false;
        if (!active_) return true;
        if (dma_pending_) return fail(Failure::dma_pair);
        dma_pending_ = true; dma_owner_ = frames_[depth_ - 1].scope;
        return emit(Kind::dma_start, 0, s, page);
    }
    // Exact configured HRAM delay return, never labeled physical DMA completion.
    // Caller must authenticate actual HRAM PC/SP/caller independently before
    // invoking this API. This check only enforces chronology: completion must
    // occur in the same still-open validated frame as its start, not a nested
    // IRQ/job or a later frame. No raw-memory opcode inference is performed.
    bool dma_wait_complete(const Sample& s) {
        if (!healthy() || !clock(s)) return false;
        if (!active_ || !dma_pending_ || dma_owner_ != frames_[depth_ - 1].scope)
            return fail(Failure::dma_pair);
        dma_pending_ = false; return emit(Kind::dma_wait_complete, 0, s);
    }
    // Genuine raster row0 only. First boundary is partial and earns no interval.
    // value is actual FF46 writes since previous row0; zero means retained OAM,
    // not proof that all intended figures were served.
    bool output_row0(const Sample& s) {
        if (!healthy() || !clock(s)) return false;
        if (!seen_irq_) return true;
        if (!row0_) { row0_ = true; last_dma_ = dma_starts_; return true; }
        const uint64_t delta = dma_starts_ - last_dma_; last_dma_ = dma_starts_;
        if (delta > UINT32_MAX) return fail(Failure::clock_overflow);
        if (!increment(intervals_) || (!delta && !increment(retained_)) ||
            (delta > 1 && !increment(repeated_))) return false;
        return emit(Kind::output_interval, 0, s, uint32_t(delta));
    }
    bool finish() { return !healthy() ? false : (active_ || paired_ || dma_pending_ ? fail(Failure::scope_open) : true); }
    void restored() { fail(Failure::restored); }
    Failure failure() const { return failure_; }
    bool active() const { return active_; }
    uint64_t instruction_t() const { return instruction_t_; }
    uint64_t core_t() const { return core_t_; }
    uint64_t completed() const { return completed_; }
    uint64_t intervals() const { return intervals_; }
    uint64_t retained() const { return retained_; }
    uint64_t repeated() const { return repeated_; }
    std::size_t size() const { return count_; }
    const Event* event(std::size_t i) const { return i < count_ ? &events_[i] : nullptr; }
private:
    struct Frame { uint16_t sp, pc, bank, id; bool irq; uint64_t scope; };
    bool increment(uint64_t& n) {
        if (n == UINT64_MAX) return fail(Failure::clock_overflow);
        ++n; return true;
    }
    bool fail(Failure f) { if (failure_ == Failure::none) failure_ = f; return false; }
    bool healthy() { return failure_ == Failure::none && (configured_ || fail(Failure::configuration)); }
    bool clock(const Sample& s) {
        if (!clock_seen_) { last_clock_ = s.core_clock; clock_seen_ = true; return true; }
        const uint32_t delta = s.core_clock - last_clock_;
        if (delta >= 0x80000000u) return fail(Failure::clock_discontinuity);
        if (core_t_ > UINT64_MAX - delta) return fail(Failure::clock_overflow);
        core_t_ += delta; last_clock_ = s.core_clock; return true;
    }
    bool emit(Kind k, uint16_t id, const Sample& s, uint32_t value = 0, uint64_t scope = 0) {
        if (count_ == limit_) return fail(Failure::event_overflow);
        events_[count_++] = {k, id, instruction_t_, core_t_, irq_,
                              scope ? scope : (depth_ ? frames_[depth_ - 1].scope : 0), s, value}; return true;
    }
    std::array<Point, max_points> points_{};
    std::array<uint16_t, max_points> return_bank_{};
    std::array<bool, max_points> return_bank_set_{};
    std::array<Event, max_events> events_{};
    std::array<Frame, max_depth> frames_{};
    Sample pre_{};
    Failure failure_ = Failure::none;
    std::size_t point_count_ = 0, limit_ = 0, count_ = 0, depth_ = 0;
    uint64_t instruction_t_ = 0, core_t_ = 0, irq_ = 0, completed_ = 0;
    uint64_t scope_ = 0, dma_owner_ = 0;
    uint64_t dma_starts_ = 0, last_dma_ = 0, intervals_ = 0, retained_ = 0, repeated_ = 0;
    uint32_t last_clock_ = 0;
    bool configured_ = false, clock_seen_ = false, paired_ = false, active_ = false;
    bool seen_irq_ = false, dma_pending_ = false, row0_ = false;
};
} // namespace native_vblank_timing
#endif
