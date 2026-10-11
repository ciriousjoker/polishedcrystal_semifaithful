// Read-only instruction observer for the pinned DoubleCherryGB test build.
// Included by cpu.cpp only. No CPU state, timing, networking or I/O is changed.
#include <algorithm>
#include <cstdint>
#include <cstdio>

struct MobileProfileSpan {
    bool active = false;
    cpu_regs before = {};
    byte* rom = nullptr;
    byte* wram = nullptr;
    byte wram_selector = 0, rom_shadow = 0;
    uint32_t clock = 0;
    uint16_t minimum_sp = 0xffff;
    uint16_t return_pc = 0;
    uint64_t count = 0, maximum_cycles = 0, total_cycles = 0, failures = 0;
    uint16_t lowest_sp = 0xffff;
};
static MobileProfileSpan mobile_irq_profile, mobile_tick_profile;
static unsigned mobile_tick_pc, mobile_tick_bank;
static unsigned mobile_stack_cursor_ranges[10][3] = {};
static uint64_t mobile_stack_cursor_samples = 0;
static unsigned mobile_stack_cursor_lowest = 0xffff;

extern "C" void mobile_profile_stack_cursor(unsigned index, unsigned bank, unsigned start, unsigned end) {
    if (index < 10) {
        mobile_stack_cursor_ranges[index][0] = bank;
        mobile_stack_cursor_ranges[index][1] = start;
        mobile_stack_cursor_ranges[index][2] = end;
    }
}
static void mobile_profile_stack(MobileProfileSpan& span, gb* game, const cpu_regs& regs) {
    // Vanilla video/gfx/tileset routines save the real SP in wSPBuffer or BC
    // and use POP as a copy iterator with interrupts disabled. Only audited PC/bank ranges can be
    // excluded, and every normal/mobile stack address remains measured.
    if (&span == &mobile_tick_profile && regs.SP < 0xc000 && !regs.I) {
        for (const auto& range : mobile_stack_cursor_ranges) {
            bool bank_matches = range[0] == 0 ? regs.PC < 0x4000 :
                game->get_mbc()->get_rom() == game->get_rom()->get_rom() + (range[0] - 1) * 0x4000;
            if (range[1] && bank_matches && regs.PC >= range[1] && regs.PC < range[2]) {
                mobile_stack_cursor_samples++;
                mobile_stack_cursor_lowest = std::min(mobile_stack_cursor_lowest, unsigned(regs.SP));
                return;
            }
        }
    }
    span.minimum_sp = std::min(span.minimum_sp, regs.SP);
}
static uint64_t mobile_normal_speed_irqs = 0, mobile_bank_switch_completions = 0;
static uint64_t mobile_serial_frame_cycles = 0, mobile_serial_max_frame_cycles = 0;
static unsigned mobile_vblank_address;
static byte mobile_last_vblank;
static uint64_t mobile_serial_vblank_cycles = 0, mobile_serial_max_vblank_cycles = 0;
static unsigned mobile_restore_bank, mobile_restore_pc, mobile_saved_facings, mobile_objects;
static uint64_t mobile_restore_count = 0, mobile_restore_failures = 0;

struct MobileRenderSpan {
    bool active = false;
    unsigned sp = 0, return_pc = 0;
    uint32_t clock = 0;
    uint64_t maximum_cycles = 0, count = 0, total_cycles = 0;
};
static MobileRenderSpan mobile_render_spans[11];
static unsigned mobile_overworld_points[13][2] = {};
static uint64_t mobile_world_gaps[64] = {};
static unsigned mobile_world_gap_count = 0;
static unsigned char mobile_world_frames[800] = {};
extern "C" uint64_t mobile_profile_world_gap(unsigned index) {
    return index < mobile_world_gap_count ? mobile_world_gaps[index] : 0;
}
static unsigned mobile_observed_frames = 0, mobile_last_world_frame = 0;
static uint64_t mobile_active_window_cycles = 0;
static uint64_t mobile_prepare_waits = 0, mobile_world_updates = 0, mobile_world_max_gap = 0;

extern "C" void mobile_profile_overworld(unsigned index, unsigned bank, unsigned pc) {
    if (index < 13) {
        mobile_overworld_points[index][0] = bank;
        mobile_overworld_points[index][1] = pc;
    }
}

extern "C" void mobile_profile_restore(unsigned bank, unsigned pc, unsigned saved, unsigned objects) {
    mobile_restore_bank = bank;
    mobile_restore_pc = pc;
    mobile_saved_facings = saved;
    mobile_objects = objects;
}

extern "C" void mobile_profile_vblank(unsigned address) {
    mobile_vblank_address = address;
}

extern "C" void mobile_profile_configure(unsigned bank, unsigned pc) {
    mobile_tick_bank = bank;
    mobile_tick_pc = pc;
}
extern "C" uint64_t mobile_profile_metric(unsigned index) {
    const uint64_t values[] = {
        mobile_irq_profile.count, mobile_irq_profile.maximum_cycles,
        mobile_irq_profile.total_cycles, mobile_irq_profile.failures,
        mobile_irq_profile.lowest_sp, mobile_tick_profile.count,
        mobile_tick_profile.maximum_cycles, mobile_tick_profile.total_cycles,
        mobile_tick_profile.failures, mobile_tick_profile.lowest_sp,
        mobile_serial_max_frame_cycles, mobile_normal_speed_irqs, mobile_bank_switch_completions,
        mobile_serial_max_vblank_cycles,
        mobile_restore_count, mobile_restore_failures,
        mobile_stack_cursor_samples, mobile_stack_cursor_lowest,
        mobile_render_spans[0].maximum_cycles, mobile_render_spans[1].maximum_cycles,
        mobile_prepare_waits, mobile_world_updates, mobile_world_max_gap,
    };
    if (index < sizeof(values) / sizeof(values[0])) return values[index];
    index -= sizeof(values) / sizeof(values[0]);
    if (index < 33) {
        const auto& span = mobile_render_spans[index / 3];
        return index % 3 == 0 ? span.count : index % 3 == 1 ? span.total_cycles : span.maximum_cycles;
    }
    unsigned present = 0, duplicates = 0;
    for (unsigned count : mobile_world_frames) {
        present += count != 0;
        duplicates += count > 1 ? count - 1 : 0;
    }
    const uint64_t tail[] = {mobile_active_window_cycles, present, duplicates};
    index -= 33;
    return index < sizeof(tail) / sizeof(tail[0]) ? tail[index] : 0;
}
extern "C" void mobile_profile_frame() {
    mobile_observed_frames++;
    mobile_serial_max_frame_cycles = std::max(mobile_serial_max_frame_cycles, mobile_serial_frame_cycles);
    mobile_serial_frame_cycles = 0;
}
static void mobile_profile_enter(MobileProfileSpan& span, gb* game, const cpu_regs& regs, int clock) {
    // Charge the hardware interrupt entry separately from observed opcodes.
    if (&span == &mobile_irq_profile) mobile_serial_vblank_cycles += 20;
    span.active = true;
    span.before = regs;
    span.rom = game->get_mbc()->get_rom();
    span.wram = game->get_cpu()->get_ram_bank();
    span.wram_selector = game->get_cregs()->SVBK;
    span.rom_shadow = game->get_cpu()->read(0xff87);
    span.clock = static_cast<uint32_t>(clock);
    span.minimum_sp = regs.SP;
    span.return_pc = game->get_cpu()->readw(regs.SP);
}
static void mobile_profile_exit(MobileProfileSpan& span, gb* game, const cpu_regs& regs, int clock, bool irq) {
    uint64_t elapsed = static_cast<uint32_t>(clock) - span.clock;
    span.count++;
    span.total_cycles += elapsed;
    span.maximum_cycles = std::max(span.maximum_cycles, elapsed);
    span.lowest_sp = std::min(span.lowest_sp, span.minimum_sp);
    unsigned failure = 0;
    if (span.before.AF.w != regs.AF.w) failure |= 1;
    if (span.before.BC.w != regs.BC.w) failure |= 2;
    if (span.before.DE.w != regs.DE.w) failure |= 4;
    if (span.before.HL.w != regs.HL.w) failure |= 8;
    if (regs.SP != span.before.SP + (irq ? 0 : 2)) failure |= 16;
    if (regs.PC != (irq ? span.before.PC : span.return_pc)) failure |= 32;
    if (span.rom != game->get_mbc()->get_rom()) {
        // Bankswitch writes its HRAM shadow at $0008, then the mapper at
        // $000a. FarCall can complete that same pending switch inside an IRQ.
        // The interrupted instruction remains in ROM0 and writes the same A
        // after RETI. Accept only this exact, source-audited transient state.
        bool pending_switch = irq && span.before.PC == 0x000a &&
            span.rom_shadow == span.before.AF.b.h &&
            game->get_mbc()->get_rom() == game->get_rom()->get_rom() +
                ((span.rom_shadow ? span.rom_shadow : 1) - 1) * 0x4000;
        if (pending_switch) mobile_bank_switch_completions++;
        else failure |= 64;
    }
    if (span.wram != game->get_cpu()->get_ram_bank()) failure |= 128;
    if (span.wram_selector != game->get_cregs()->SVBK) failure |= 256;
    if (span.rom_shadow != game->get_cpu()->read(0xff87)) failure |= 512;
    if (span.before.I != regs.I) failure |= 1024;
    if (failure) {
        if (span.failures < 3) std::fprintf(stderr, "PROFILE_FAILURE irq=%u mask=%u pc=%04x/%04x sp=%04x/%04x svbk=%u/%u shadow=%u/%u\n",
            irq, failure, span.before.PC, regs.PC, span.before.SP, regs.SP,
            span.wram_selector, game->get_cregs()->SVBK, span.rom_shadow, game->get_cpu()->read(0xff87));
        span.failures++;
    }
    if (irq && !game->get_cpu()->get_speed()) mobile_normal_speed_irqs++;
    if (irq) mobile_serial_frame_cycles += elapsed;
    span.active = false;
}
static void mobile_profile_before_instruction(gb* game, const cpu_regs& regs, int clock) {
    for (unsigned index = 0; index < 13; ++index) {
        auto* point = mobile_overworld_points[index];
        bool bank_matches = point[0] == 0 ? regs.PC < 0x4000 :
            game->get_mbc()->get_rom() == game->get_rom()->get_rom() + (point[0] - 1) * 0x4000;
        if (!point[1] || regs.PC != point[1] || !bank_matches) continue;
        if (index < 2 || index >= 4) {
            auto& span = mobile_render_spans[index < 2 ? index : index - 2];
            span.active = true;
            span.sp = regs.SP;
            span.return_pc = game->get_cpu()->readw(regs.SP);
            span.clock = static_cast<uint32_t>(clock);
        } else if (index == 2 && mobile_render_spans[0].active) {
            mobile_prepare_waits++;
        } else if (index == 3 && mobile_observed_frames >= 3600 && mobile_observed_frames < 4400) {
            if (mobile_world_updates) {
                auto gap = mobile_observed_frames - mobile_last_world_frame;
                mobile_world_max_gap = std::max(mobile_world_max_gap, uint64_t(gap));
                if (gap > 1 && mobile_world_gap_count < 64)
                    mobile_world_gaps[mobile_world_gap_count++] = (uint64_t(gap) << 32) | mobile_observed_frames;
            }
            mobile_world_frames[mobile_observed_frames - 3600]++;
            mobile_last_world_frame = mobile_observed_frames;
            mobile_world_updates++;
        }
    }
    if (mobile_restore_pc && regs.PC == mobile_restore_pc &&
        game->get_mbc()->get_rom() == game->get_rom()->get_rom() + (mobile_restore_bank - 1) * 0x4000) {
        mobile_restore_count++;
        for (unsigned object = 0; object < 13; ++object) {
            auto saved = game->get_cpu()->read(mobile_saved_facings + object);
            if (saved != 255 && saved != game->get_cpu()->read(mobile_objects + object * 34 + 13))
                mobile_restore_failures++;
        }
    }
    if (mobile_vblank_address) {
        auto frame = game->get_cpu()->read(mobile_vblank_address);
        if (frame != mobile_last_vblank) {
            mobile_serial_max_vblank_cycles = std::max(mobile_serial_max_vblank_cycles, mobile_serial_vblank_cycles);
            mobile_serial_vblank_cycles = 0;
            mobile_last_vblank = frame;
        }
    }
    if (!mobile_tick_profile.active && mobile_tick_pc && regs.PC == mobile_tick_pc &&
        game->get_mbc()->get_rom() == game->get_rom()->get_rom() + (mobile_tick_bank - 1) * 0x4000)
        mobile_profile_enter(mobile_tick_profile, game, regs, clock);
}
static void mobile_profile_after_instruction(gb* game, const cpu_regs& regs, int clock, unsigned opcode, unsigned cycles) {
    if (mobile_observed_frames >= 3600 && mobile_observed_frames < 4400 && opcode != 0x76)
        mobile_active_window_cycles += cycles;
    bool returned_from_render = opcode == 0xc9 || opcode == 0xd9 ||
                               ((opcode & 0xe7) == 0xc0 && cycles == 20);
    for (auto& span : mobile_render_spans) {
        if (span.active && returned_from_render && regs.SP == span.sp + 2 && regs.PC == span.return_pc) {
            auto elapsed = uint64_t(static_cast<uint32_t>(clock) - span.clock);
            span.count++;
            span.total_cycles += elapsed;
            span.maximum_cycles = std::max(span.maximum_cycles, elapsed);
            span.active = false;
        }
    }
    if (mobile_irq_profile.active) {
        mobile_serial_vblank_cycles += cycles;
        mobile_profile_stack(mobile_irq_profile, game, regs);
        if (opcode == 0xd9) mobile_profile_exit(mobile_irq_profile, game, regs, clock, true);
    }
    if (!mobile_tick_profile.active) return;
    auto& tick = mobile_tick_profile;
    mobile_profile_stack(tick, game, regs);
    bool returned = opcode == 0xc9 || opcode == 0xd9 ||
                    ((opcode & 0xe7) == 0xc0 && cycles == 20);
    if (returned && regs.SP == tick.before.SP + 2 && regs.PC == tick.return_pc)
        mobile_profile_exit(tick, game, regs, clock, false);
}
