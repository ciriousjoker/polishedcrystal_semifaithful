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
static unsigned mobile_save_bank, mobile_save_pc, mobile_save_size;
static unsigned mobile_save_slots, mobile_save_backups, mobile_save_object_offset, mobile_save_objects;
static MobileProfileSpan mobile_save_profile;
static byte mobile_save_expected[4096];
static byte* mobile_save_page = nullptr;
static unsigned mobile_save_start;
static uint64_t mobile_save_projected = 0, mobile_save_byte_failures = 0;
extern "C" void mobile_profile_save(unsigned bank, unsigned pc, unsigned size,
                                  unsigned slots, unsigned backups, unsigned object_offset, unsigned objects) {
    mobile_save_bank = bank; mobile_save_pc = pc; mobile_save_size = size;
    mobile_save_slots = slots; mobile_save_backups = backups;
    mobile_save_object_offset = object_offset;
    mobile_save_objects = objects;
}
extern "C" uint64_t mobile_profile_save_metric(unsigned index) {
    const uint64_t values[] = {mobile_save_profile.count, mobile_save_profile.failures,
        mobile_save_byte_failures, mobile_save_projected, mobile_save_profile.maximum_cycles, mobile_save_profile.lowest_sp};
    return index < 6 ? values[index] : 0;
}
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
    if (regs.SP < 0xc000 && !regs.I) {
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
    unsigned vblank = 0, stage = 0, ly = 0;
    unsigned minimum_ly = 154, maximum_ly = 0;
    uint16_t lowest_sp = 0xffff;
    uint64_t crossings = 0;
    uint64_t maximum_cycles = 0, count = 0, total_cycles = 0;
};
static MobileRenderSpan mobile_render_spans[34];
extern "C" uint64_t mobile_profile_plan_metric(unsigned index) {
    if (index >= 32) return 0;
    const auto& span = mobile_render_spans[26 + index / 4];
    switch (index % 4) {
        case 0: return span.count;
        case 1: return span.total_cycles;
        case 2: return span.maximum_cycles;
        default: return span.crossings;
    }
}
static unsigned mobile_scene_stage_address;
static uint64_t mobile_scene_steps[10][6] = {};
static uint64_t mobile_scene_window_steps[10][7] = {};
static unsigned mobile_scene_entry_lines[10][154] = {};
extern "C" void mobile_profile_scene(unsigned stage_address) {
    mobile_scene_stage_address = stage_address;
}
extern "C" uint64_t mobile_profile_scene_metric(unsigned index) {
    return index < 60 ? mobile_scene_steps[index / 6][index % 6] : 0;
}
extern "C" uint64_t mobile_profile_scene_window_metric(unsigned index) {
    return index < 70 ? mobile_scene_window_steps[index / 7][index % 7] : 0;
}
extern "C" uint64_t mobile_profile_scene_entry(unsigned index) {
    if (index >= 20) return 154;
    const auto& lines = mobile_scene_entry_lines[index / 2];
    uint64_t total = 0, seen = 0;
    for (unsigned count : lines) total += count;
    if (!total) return 154;
    const uint64_t target = (total * (index % 2 ? 95 : 50) + 99) / 100;
    for (unsigned ly = 0; ly < 154; ly++) {
        seen += lines[ly];
        if (seen >= target) return ly;
    }
    return 154;
}
static unsigned mobile_native_shadow_address, mobile_native_descriptor_address, mobile_native_object_address;
static byte mobile_native_shadow[160], mobile_native_descriptors[13 * 8], mobile_native_identities[13 * 2];
static unsigned mobile_native_facing_bank, mobile_native_template_delta;
static byte mobile_native_figures[13][64], mobile_native_figure_counts[13];
extern "C" void mobile_profile_native_facing(unsigned bank) {
    mobile_native_facing_bank = bank;
}
extern "C" void mobile_profile_native_template_offset(unsigned delta) {
    mobile_native_template_delta = delta & 0xffff;
}

extern "C" unsigned mobile_profile_native_figure_byte(unsigned index) {
    if (index < 13 * 64) return mobile_native_figures[index / 64][index % 64];
    index -= 13 * 64;
    return index < 13 ? mobile_native_figure_counts[index] : 0;
}
extern "C" void mobile_profile_native_oam(unsigned shadow, unsigned descriptors, unsigned objects) {
    mobile_native_shadow_address = shadow;
    mobile_native_descriptor_address = descriptors;
    mobile_native_object_address = objects;
}
extern "C" unsigned mobile_profile_native_oam_byte(unsigned index) {
    if (index < 160) return mobile_native_shadow[index];
    if (index < 264) return mobile_native_descriptors[index - 160];
    if (index < 290) return mobile_native_identities[index - 264];
    return 0;
}
static unsigned mobile_overworld_points[36][2] = {};
static bool mobile_native_graphics_frame = false, mobile_native_graphics_last_frame = false;
static unsigned mobile_native_wait_frame_ly = 255, mobile_native_wait_last_frame_ly = 255;
extern "C" uint64_t mobile_profile_native_wait_ly() {
    return mobile_native_wait_last_frame_ly;
}
extern "C" uint64_t mobile_profile_native_graphics_wait() {
    return mobile_native_graphics_last_frame;
}
extern "C" uint64_t mobile_profile_pose_metric(unsigned index) {
    const auto& span = mobile_render_spans[18];
    const uint64_t values[] = {span.count, span.maximum_cycles, span.total_cycles,
        span.minimum_ly, span.maximum_ly, span.crossings};
    return index < 6 ? values[index] : 0;
}
extern "C" uint64_t mobile_profile_composition_metric(unsigned index) {
    if (index >= 24) return 0;
    const auto& span = mobile_render_spans[14 + index / 6];
    const uint64_t values[] = {span.count, span.maximum_cycles, span.total_cycles,
        span.minimum_ly, span.maximum_ly, span.crossings};
    return values[index % 6];
}
extern "C" uint64_t mobile_profile_transport_metric(unsigned index) {
    if (index >= 18) return 0;
    const auto& span = mobile_render_spans[20 + index / 6];
    const uint64_t values[] = {span.count, span.maximum_cycles, span.total_cycles,
        span.minimum_ly, span.maximum_ly, span.crossings};
    return values[index % 6];
}
extern "C" uint64_t mobile_profile_aligned_metric(unsigned index) {
    if (index >= 18) return 0;
    const auto& span = mobile_render_spans[23 + index / 6];
    const uint64_t values[] = {span.count, span.maximum_cycles, span.total_cycles,
        span.minimum_ly, span.maximum_ly, span.crossings};
    return values[index % 6];
}
static uint64_t mobile_world_gaps[64] = {};
static unsigned mobile_world_gap_count = 0;
static unsigned char mobile_world_frames[800] = {};
extern "C" uint64_t mobile_profile_world_gap(unsigned index) {
    return index < mobile_world_gap_count ? mobile_world_gaps[index] : 0;
}
static unsigned mobile_observed_frames = 0, mobile_last_world_frame = 0;
static unsigned mobile_world_start = 3600;
static unsigned mobile_composition_entry_lines[5][154] = {};
extern "C" uint64_t mobile_profile_composition_entry(unsigned index) {
    if (index >= 10) return 0;
    const auto& lines = mobile_composition_entry_lines[index / 2];
    unsigned total = 0, seen = 0;
    for (auto count : lines) total += count;
    if (!total) return 154;
    unsigned target = index % 2 ? (total * 95 + 99) / 100 : (total + 1) / 2;
    for (unsigned line = 0; line < 154; ++line) {
        seen += lines[line];
        if (seen >= target) return line;
    }
    return 154;
}
extern "C" void mobile_profile_window(unsigned first) {
    mobile_world_start = first;
}
static uint64_t mobile_active_window_cycles = 0;
static uint64_t mobile_upload_waits = 0;
extern "C" uint64_t mobile_profile_idle_metric(unsigned index) {
    if (index == 0) return mobile_upload_waits;
    if (index == 9) return mobile_render_spans[11].lowest_sp;
    if (index == 10) return mobile_render_spans[12].lowest_sp;
    if (index == 11) return mobile_render_spans[13].maximum_cycles;
    if (index == 12) return mobile_render_spans[13].count;
    if (index == 13) return mobile_render_spans[13].crossings;
    if (index == 14) return mobile_render_spans[13].lowest_sp;
    if (index > 8) return 0;
    const auto& span = mobile_render_spans[11 + (index - 1) / 4];
    const uint64_t values[] = {span.count, span.maximum_cycles, span.total_cycles, span.crossings};
    return values[(index - 1) % 4];
}
static uint64_t mobile_prepare_waits = 0, mobile_world_updates = 0, mobile_world_max_gap = 0;

// Diagnostics retain both host bins and independently observed raster epochs.
// They do not replace or relax existing cadence counters.
static unsigned mobile_phase_delay, mobile_phase_occurred, mobile_phase_skip;
static unsigned mobile_phase_last_ly = 0, mobile_phase_generation = 0;
static bool mobile_phase_lcd_enabled = false;
static uint64_t mobile_phase_epoch = 0;
static uint64_t mobile_world_phases[4096][8] = {};
static unsigned mobile_world_phase_count = 0;
extern "C" void mobile_profile_world_phase_configure(unsigned delay, unsigned occurred, unsigned skip) {
    mobile_phase_delay = delay; mobile_phase_occurred = occurred; mobile_phase_skip = skip;
}
extern "C" uint64_t mobile_profile_world_phase(unsigned row, unsigned field) {
    if (field == 8) return mobile_world_phase_count;
    return row < mobile_world_phase_count && field < 8 ? mobile_world_phases[row][field] : 0;
}

extern "C" void mobile_profile_overworld(unsigned index, unsigned bank, unsigned pc) {
    if (index < 36) {
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
    mobile_native_wait_last_frame_ly = mobile_native_wait_frame_ly;
    mobile_native_wait_frame_ly = 255;
    mobile_native_graphics_last_frame = mobile_native_graphics_frame;
    mobile_native_graphics_frame = mobile_render_spans[19].active;
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
    const unsigned raster_ly = game->get_regs()->LY;
    const bool raster_enabled = (game->get_regs()->LCDC & 0x80) != 0;
    if (raster_enabled && !mobile_phase_lcd_enabled) mobile_phase_generation++;
    if (raster_enabled && mobile_phase_lcd_enabled && raster_ly < mobile_phase_last_ly &&
            mobile_phase_last_ly >= 144) mobile_phase_epoch++;
    mobile_phase_last_ly = raster_ly;
    mobile_phase_lcd_enabled = raster_enabled;
    const bool native_save_copy = mobile_save_bank == 0 && mobile_save_pc == 0x20;
    if (mobile_save_pc && regs.PC == mobile_save_pc && mobile_save_size <= sizeof(mobile_save_expected) &&
        (native_save_copy ? regs.HL.w == mobile_save_backups && regs.BC.w == mobile_save_size &&
                            regs.DE.w >= 0xa000 && regs.DE.w + mobile_save_size <= 0xc000 :
         game->get_mbc()->get_rom() == game->get_rom()->get_rom() + (mobile_save_bank - 1) * 0x4000)) {
        mobile_profile_enter(mobile_save_profile, game, regs, clock);
        mobile_save_page = game->get_mbc()->get_sram();
        mobile_save_start = regs.DE.w - (native_save_copy ? 0 : mobile_save_size) - 0xa000;
        auto* ram = game->get_cpu()->get_ram();
        for (unsigned i = 0; i < mobile_save_size; ++i)
            mobile_save_expected[i] = native_save_copy ? game->get_cpu()->read(regs.HL.w + i) :
                                                        mobile_save_page[mobile_save_start + i];
        // The native CopyBytes already produced the complete vanilla payload.
        // Only still-owned slots may differ from that same-state native copy.
        for (unsigned i = 0; mobile_save_slots && i < 3; ++i) {
            unsigned slot = ram[mobile_save_slots - 0xc000 + i];
            if (!slot || slot >= 13) continue;
            auto* object = ram + 0x1000 + mobile_save_objects - 0xd000 + 34 * slot;
            if (object[1] != 254 || (object[5] & 0x22) != 0x22) continue;
            auto* original = mobile_save_backups < 0xd000 ?
                ram + mobile_save_backups - 0xc000 + 34 * i :
                ram + 0x5000 + mobile_save_backups - 0xd000 + 38 * i;
            unsigned offset = mobile_save_object_offset + 34 * slot;
            for (unsigned j = 0; j < 34; ++j) mobile_save_expected[offset + j] = original[j];
            mobile_save_projected++;
        }
    }
    for (unsigned index = 0; index < 36; ++index) {
        auto* point = mobile_overworld_points[index];
        // Bank accessors are unnecessary for virtually every instruction. The
        // PC comparison is sufficient to reject an inactive watchpoint first.
        if (!point[1] || regs.PC != point[1]) continue;
        bool bank_matches = point[0] == 0 ? regs.PC < 0x4000 :
            game->get_mbc()->get_rom() == game->get_rom()->get_rom() + (point[0] - 1) * 0x4000;
        if (!bank_matches) continue;
        if (index == 2) mobile_native_wait_frame_ly = game->get_cpu()->read(0xff44);
        if (index == 2 && mobile_render_spans[11].active) {
            mobile_upload_waits++;
            if (mobile_upload_waits < 10) std::fprintf(stderr, "UPLOAD_DELAY frame=%u ly=%u\n", mobile_observed_frames, game->get_cpu()->read(0xff44));
        }
        if (index < 2 || index >= 4) {
            auto& span = mobile_render_spans[index < 2 ? index : index - 2];
            if (index == 21) mobile_native_graphics_frame = true;
            span.active = true;
            span.sp = regs.SP;
            span.return_pc = game->get_cpu()->readw(regs.SP);
            span.clock = static_cast<uint32_t>(clock);
            span.vblank = game->get_cpu()->read(mobile_vblank_address);
            const unsigned ly = game->get_cpu()->read(0xff44);
            span.minimum_ly = std::min(span.minimum_ly, ly);
            span.maximum_ly = std::max(span.maximum_ly, ly);
            if (ly < 154 && mobile_observed_frames >= mobile_world_start && mobile_observed_frames < mobile_world_start + 800) {
                if (index == 14) mobile_composition_entry_lines[0][ly]++;
                if (index >= 16 && index < 20) mobile_composition_entry_lines[index - 15][ly]++;
            }
            if (index == 16 && mobile_scene_stage_address) {
                span.stage = game->get_cpu()->read(mobile_scene_stage_address);
                span.ly = ly;
                if (span.stage < 10 && ly < 154 && mobile_observed_frames >= mobile_world_start &&
                        mobile_observed_frames < mobile_world_start + 800)
                    mobile_scene_entry_lines[span.stage][ly]++;
            }
            if (index == 13 || index == 14 || index == 15) {
                span.stage = game->get_cpu()->read(regs.HL.w + 34);
                span.ly = game->get_cpu()->read(0xff44);
            }
        } else if (index == 2 && mobile_render_spans[0].active) {
            mobile_prepare_waits++;
        } else if (index == 3 && mobile_observed_frames >= mobile_world_start && mobile_observed_frames < mobile_world_start + 800) {
            if (mobile_phase_delay && mobile_world_phase_count < 4096) {
                auto* row = mobile_world_phases[mobile_world_phase_count++];
                row[0] = mobile_observed_frames; row[1] = mobile_phase_epoch;
                row[2] = mobile_phase_generation; row[3] = raster_ly;
                row[4] = game->get_cpu()->read(mobile_vblank_address);
                row[5] = game->get_cpu()->read(mobile_phase_occurred);
                row[6] = game->get_cpu()->read(mobile_phase_delay);
                row[7] = game->get_cpu()->read(mobile_phase_skip);
            }
            if (mobile_world_updates) {
                auto gap = mobile_observed_frames - mobile_last_world_frame;
                mobile_world_max_gap = std::max(mobile_world_max_gap, uint64_t(gap));
                if (gap > 1 && mobile_world_gap_count < 64)
                    mobile_world_gaps[mobile_world_gap_count++] = (uint64_t(gap) << 32) | mobile_observed_frames;
            }
            mobile_world_frames[mobile_observed_frames - mobile_world_start]++;
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
    if (mobile_observed_frames >= mobile_world_start && mobile_observed_frames < mobile_world_start + 800 && opcode != 0x76)
        mobile_active_window_cycles += cycles;
    bool returned_from_render = opcode == 0xc9 || opcode == 0xd9 ||
                               ((opcode & 0xe7) == 0xc0 && cycles == 20);
    if (mobile_save_profile.active && returned_from_render &&
        regs.SP == mobile_save_profile.before.SP + 2 && regs.PC == mobile_save_profile.return_pc) {
        if (game->get_mbc()->get_sram() != mobile_save_page) mobile_save_byte_failures++;
        for (unsigned i = 0; i < mobile_save_size; ++i)
            if (mobile_save_page[mobile_save_start + i] != mobile_save_expected[i]) mobile_save_byte_failures++;
        if (mobile_save_bank == 0 && mobile_save_pc == 0x20) {
            // Vanilla CopyBytes consumes BC and advances both pointers. Its
            // ordinary AF result is caller-clobbered; banks and SP stay fixed.
            mobile_save_profile.before.AF = regs.AF;
            mobile_save_profile.before.BC.w = 0;
            mobile_save_profile.before.DE.w += mobile_save_size;
            mobile_save_profile.before.HL.w += mobile_save_size;
        }
        mobile_profile_exit(mobile_save_profile, game, regs, clock, false);
    }
    if (mobile_save_profile.active) mobile_profile_stack(mobile_save_profile, game, regs);
    for (auto& span : mobile_render_spans) {
        if (span.active) {
            MobileProfileSpan stack;
            mobile_profile_stack(stack, game, regs);
            span.lowest_sp = std::min(span.lowest_sp, stack.minimum_sp);
        }
        if (span.active && returned_from_render && regs.SP == span.sp + 2 && regs.PC == span.return_pc) {
            auto elapsed = uint64_t(static_cast<uint32_t>(clock) - span.clock);
            if (&span == &mobile_render_spans[10] && mobile_native_descriptor_address) {
                // Snapshot vanilla's finished OAM before idle-time composition.
                // Reads physical pages directly; no selector or game byte changes.
                const auto* ram = game->get_cpu()->get_ram();
                for (unsigned i = 0; i < 160; ++i)
                    mobile_native_shadow[i] = ram[mobile_native_shadow_address - 0xc000 + i];
                for (unsigned i = 0; i < 104; ++i)
                    mobile_native_descriptors[i] = ram[(mobile_native_descriptor_address < 0xd000 ?
                        mobile_native_descriptor_address - 0xc000 :
                        0x5000 + mobile_native_descriptor_address - 0xd000) + i];
                for (unsigned i = 0; i < 13; ++i) {
                    const auto* actor = ram + 0x1000 + mobile_native_object_address - 0xd000 + i * 34;
                    mobile_native_identities[i * 2] = actor[0];
                    mobile_native_identities[i * 2 + 1] = actor[1];
                    const auto* view = mobile_native_descriptors + i * 8;
                    const unsigned count = view[6] & 31;
                    mobile_native_figure_counts[i] = 0;
                    if (!count || count > 16) continue;
                    auto* expected = mobile_native_figures[i];
                    if (view[6] & 128) {
                        if (unsigned(view[7]) + count * 4 > 160) continue;
                        std::copy_n(mobile_native_shadow + view[7], count * 4, expected);
                    } else {
                        // The ordinary engine retained a facing template for
                        // this actor even when vanilla's finite OAM was full.
                        // Interpret its immutable native data, without running
                        // a game routine, modifying RAM or charging game cycles.
                        // Reverse the audited upper-bank mirror address. The oracle
                        // always reads the unchanged original native facing ROM.
                        const unsigned pointer = ((view[4] | unsigned(view[5]) << 8) -
                                                  mobile_native_template_delta) & 0xffff;
                        if (!mobile_native_facing_bank || pointer < 0x4000 || pointer + count * 4 > 0x8000) continue;
                        const auto* facing = game->get_rom()->get_rom() +
                            mobile_native_facing_bank * 0x4000 + pointer - 0x4000;
                        for (unsigned q = 0; q < count; ++q) {
                            const auto* source = facing + q * 4;
                            unsigned tile = ((source[2] & 4 ? 0 : view[2]) + source[3]) & 255;
                            if ((view[3] & 8) && tile >= 128) tile -= 64;
                            unsigned attributes = ((source[2] & 2 ? view[7] : 0) | source[2]) & 0xe0;
                            attributes |= view[3];
                            if (source[2] & 8) attributes = (attributes + 1) & 255;
                            expected[q * 4] = byte(view[1] + source[0]);
                            expected[q * 4 + 1] = byte(view[0] + source[1]);
                            expected[q * 4 + 2] = byte(tile);
                            expected[q * 4 + 3] = byte(attributes);
                        }
                    }
                    mobile_native_figure_counts[i] = byte(count);
                }
            }
            if (&span == &mobile_render_spans[14] && mobile_scene_stage_address && span.stage < 10) {
                auto& step = mobile_scene_steps[span.stage];
                step[0]++;
                step[1] = std::max(step[1], elapsed);
                step[2] += elapsed;
                step[3] = std::max(step[3], uint64_t(span.ly));
                step[4] = std::max(step[4], uint64_t(game->get_cpu()->read(0xff44)));
                const unsigned next_stage = game->get_cpu()->read(mobile_scene_stage_address);
                step[5] |= 1u << (next_stage < 10 ? next_stage : 15);
                if (mobile_observed_frames >= mobile_world_start && mobile_observed_frames < mobile_world_start + 800) {
                    auto& window = mobile_scene_window_steps[span.stage];
                    window[0]++;
                    window[1] = std::max(window[1], elapsed);
                    window[2] += elapsed;
                    window[3] = std::max(window[3], uint64_t(span.ly));
                    window[4] = std::max(window[4], uint64_t(game->get_cpu()->read(0xff44)));
                    window[5] |= 1u << (next_stage < 10 ? next_stage : 15);
                    if (game->get_cpu()->read(mobile_vblank_address) != span.vblank) window[6]++;
                }
            }
            span.count++;
            span.total_cycles += elapsed;
            span.maximum_cycles = std::max(span.maximum_cycles, elapsed);
            if (game->get_cpu()->read(mobile_vblank_address) != span.vblank) span.crossings++;
            if (&span == &mobile_render_spans[11] && elapsed > 8000 && mobile_observed_frames >= mobile_world_start && mobile_observed_frames < mobile_world_start + 800) std::fprintf(stderr, "LONG_UPLOAD frame=%u cycles=%llu ly=%u/%u stage=%u crossing=%u\n", mobile_observed_frames, (unsigned long long)elapsed, span.ly, game->get_cpu()->read(0xff44), span.stage, game->get_cpu()->read(mobile_vblank_address) != span.vblank);
            if (game->get_cpu()->read(mobile_vblank_address) != span.vblank &&
                &span >= &mobile_render_spans[11] && &span < &mobile_render_spans[19])
                std::fprintf(stderr, "BACKGROUND_CROSS kind=%u frame=%u cycles=%llu ly=%u/%u\n",
                    unsigned(&span - mobile_render_spans), mobile_observed_frames,
                    (unsigned long long)elapsed, span.ly, game->get_cpu()->read(0xff44));
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
