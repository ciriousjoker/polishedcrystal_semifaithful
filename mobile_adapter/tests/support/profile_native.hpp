// Read-only capture of native figure demand before the ordinary OAM capacity test.
// Fixture labels add zero instructions. The same observer sees both ROM variants.
static unsigned acceptance_native_bank, acceptance_native_entry, acceptance_native_facing;
static unsigned acceptance_native_hram[4], acceptance_native_slot = 13;
static unsigned acceptance_native_stride = 34, acceptance_native_object_bank = 1;
static unsigned acceptance_native_sprite_field = 0, acceptance_native_map_field = 1;
static unsigned acceptance_marker_points[5][2] = {}, acceptance_marker_counts[5] = {};
static unsigned acceptance_save_bank, acceptance_save_entry, acceptance_save_sp, acceptance_save_return;
static bool acceptance_save_active = false;
static unsigned acceptance_menu_bank, acceptance_menu_entry, acceptance_menu_sp, acceptance_menu_return;
static bool acceptance_menu_active = false;
extern "C" void acceptance_profile_save_routine(unsigned bank, unsigned pc) {
    acceptance_save_bank = bank; acceptance_save_entry = pc;
}
extern "C" void acceptance_profile_marker(unsigned index, unsigned bank, unsigned pc) {
    if (index < 5) { acceptance_marker_points[index][0] = bank; acceptance_marker_points[index][1] = pc; }
}
extern "C" unsigned acceptance_profile_marker_count(unsigned index) {
    return index < 5 ? acceptance_marker_counts[index] : 0;
}
extern "C" unsigned acceptance_profile_save_active() {
    return acceptance_save_active;
}
extern "C" void acceptance_profile_menu_routine(unsigned bank, unsigned pc) {
    acceptance_menu_bank = bank; acceptance_menu_entry = pc;
}
extern "C" unsigned acceptance_profile_menu_active() {
    return acceptance_menu_active;
}
extern "C" void acceptance_profile_native(unsigned bank, unsigned entry, unsigned facing,
                                         unsigned x, unsigned y, unsigned tile, unsigned flags,
                                         unsigned stride, unsigned object_bank, unsigned sprite_field, unsigned map_field) {
    acceptance_native_bank = bank; acceptance_native_entry = entry; acceptance_native_facing = facing;
    acceptance_native_hram[0] = x; acceptance_native_hram[1] = y;
    acceptance_native_hram[2] = tile; acceptance_native_hram[3] = flags;
    acceptance_native_stride = stride; acceptance_native_object_bank = object_bank;
    acceptance_native_sprite_field = sprite_field; acceptance_native_map_field = map_field;
}
// Capture simulation only after the entire native object update returns. Host
// frame boundaries can intersect that routine and are not a completed state.
static unsigned acceptance_simulation_bank, acceptance_simulation_entry;
static unsigned acceptance_simulation_sp, acceptance_simulation_return, acceptance_simulation_return_bank;
static unsigned acceptance_instruction_sp;
static unsigned acceptance_simulation_object_bank, acceptance_simulation_objects;
static unsigned acceptance_simulation_stride, acceptance_simulation_map, acceptance_simulation_original;
static unsigned acceptance_simulation_fields[64] = {}, acceptance_simulation_field_count;
static bool acceptance_simulation_active = false;
static std::vector<std::pair<unsigned, unsigned>> acceptance_simulation_records;
static unsigned acceptance_simulation_overflow = 0;
extern "C" void acceptance_profile_simulation(unsigned bank, unsigned pc, unsigned object_bank,
        unsigned objects, unsigned stride, unsigned map, unsigned original) {
    acceptance_simulation_bank = bank; acceptance_simulation_entry = pc;
    acceptance_simulation_object_bank = object_bank; acceptance_simulation_objects = objects;
    acceptance_simulation_stride = stride; acceptance_simulation_map = map;
    acceptance_simulation_original = original;
}
extern "C" void acceptance_profile_simulation_field(unsigned index, unsigned offset) {
    if (index < 64) {
        acceptance_simulation_fields[index] = offset;
        acceptance_simulation_field_count = std::max(acceptance_simulation_field_count, index + 1);
    }
}
extern "C" unsigned acceptance_profile_simulation_record(unsigned index, unsigned field) {
    if (field == 2) return acceptance_simulation_records.size();
    if (field == 3) return acceptance_simulation_overflow;
    if (index >= acceptance_simulation_records.size()) return 0;
    return field == 0 ? acceptance_simulation_records[index].first : acceptance_simulation_records[index].second;
}

// Acceptance-only instruction evidence. Limits are part of the ABI. Any invalid
// configuration, stack exhaustion or event exhaustion sets the overflow gate.
static constexpr unsigned acceptance_effect_spans = 9, acceptance_effect_watches = 16;
static constexpr unsigned acceptance_effect_bytes = 160, acceptance_effect_capacity = 4096;
struct AcceptanceEffectWatch { unsigned bank = 0, address = 0, length = 0; };
struct AcceptanceEffectCall { unsigned id, sp, pc, bank; };
struct AcceptanceEffectRecord {
    unsigned fields[22] = {};
    unsigned char samples[acceptance_effect_watches][acceptance_effect_bytes] = {};
};
static unsigned acceptance_effect_points[acceptance_effect_spans][2] = {};
static unsigned acceptance_effect_aliases[acceptance_effect_spans][2] = {};
static bool acceptance_effect_enabled = false, acceptance_observation_invalid = false;
static gb* acceptance_game_context = nullptr;
static bool acceptance_video_enabled = false;
extern "C" unsigned acceptance_profile_serial_status(unsigned field) {
    if (!acceptance_game_context || acceptance_observation_invalid || field > 1) return ~0u;
    return acceptance_game_context->get_cpu()->read(field == 0 ? 0xff02 : 0xffff);
}
static AcceptanceEffectWatch acceptance_effect_watch_points[acceptance_effect_watches];
static std::vector<AcceptanceEffectCall> acceptance_effect_calls;
static std::vector<AcceptanceEffectRecord> acceptance_effect_records;
static unsigned acceptance_effect_sequence = 0, acceptance_effect_overflow = 0;
static unsigned acceptance_effect_returns[acceptance_effect_spans] = {};
extern "C" unsigned acceptance_profile_effect_returns(unsigned id) {
    return id < acceptance_effect_spans ? acceptance_effect_returns[id] : 0;
}
static unsigned acceptance_effect_teardown_bank = ~0u, acceptance_effect_teardown_return = 0;
extern "C" void acceptance_profile_effect_teardown(unsigned bank, unsigned pc) {
    if (!pc || pc >= 0x8000 || (pc >= 0x4000 && !bank)) { ++acceptance_effect_overflow; return; }
    acceptance_effect_teardown_bank = bank; acceptance_effect_teardown_return = pc;
}
static cpu_regs acceptance_effect_instruction_regs = {};
static unsigned acceptance_native_scene_fields[8][5] = {}, acceptance_native_scene_count = 0;
static unsigned acceptance_native_scene_ready = 0;
static bool acceptance_native_world_completed = false;
static bool acceptance_native_scene_is_ready(gb* game);
extern "C" void acceptance_profile_effect_span(unsigned id, unsigned bank, unsigned pc) {
    if (id >= acceptance_effect_spans || pc >= 0x8000 || (pc >= 0x4000 && !bank)) {
        ++acceptance_effect_overflow; return;
    }
    acceptance_effect_points[id][0] = bank; acceptance_effect_points[id][1] = pc;
    acceptance_effect_enabled = false;
    for (const auto& point : acceptance_effect_points) acceptance_effect_enabled |= point[1] != 0;
}
extern "C" void acceptance_profile_effect_alias(unsigned id, unsigned bank, unsigned pc) {
    if (id >= acceptance_effect_spans || !pc || pc >= 0x8000 || (pc >= 0x4000 && !bank)) {
        ++acceptance_effect_overflow; return;
    }
    acceptance_effect_aliases[id][0] = bank; acceptance_effect_aliases[id][1] = pc;
}
extern "C" void acceptance_profile_effect_watch(unsigned id, unsigned bank, unsigned address, unsigned length) {
    if (id >= acceptance_effect_watches || length > acceptance_effect_bytes || bank > 7 || address + length > 0x10000) {
        ++acceptance_effect_overflow; return;
    }
    acceptance_effect_watch_points[id] = {bank, address, length};
}
extern "C" unsigned acceptance_profile_effect_active(unsigned id) {
    return std::count_if(acceptance_effect_calls.begin(), acceptance_effect_calls.end(),
        [id](const AcceptanceEffectCall& call) { return call.id == id; });
}
extern "C" unsigned acceptance_profile_effect_record(unsigned index, unsigned field) {
    if (field == 32) return acceptance_effect_records.size();
    if (field == 33) return acceptance_effect_overflow;
    return index < acceptance_effect_records.size() && field < 22 ? acceptance_effect_records[index].fields[field] : 0;
}
extern "C" unsigned acceptance_profile_effect_sample(unsigned index, unsigned watch, unsigned offset) {
    return index < acceptance_effect_records.size() && watch < acceptance_effect_watches &&
        offset < acceptance_effect_watch_points[watch].length ? acceptance_effect_records[index].samples[watch][offset] : 0;
}
extern "C" void acceptance_profile_native_scene_field(unsigned index, unsigned bank, unsigned address,
        unsigned expected, unsigned mask) {
    if (index >= 8 || bank > 7 || address > 0xffff) { ++acceptance_effect_overflow; return; }
    acceptance_native_scene_fields[index][0] = bank; acceptance_native_scene_fields[index][1] = address;
    acceptance_native_scene_fields[index][2] = expected; acceptance_native_scene_fields[index][3] = mask;
    acceptance_native_scene_fields[index][4] = 1;
}
extern "C" void acceptance_profile_native_scene_enable(unsigned count) {
    if (count != 0 && count != 8) { ++acceptance_effect_overflow; return; }
    acceptance_native_scene_count = count;
    acceptance_native_scene_ready = 0;
    acceptance_native_world_completed = false;
}
extern "C" unsigned acceptance_profile_native_scene_ready() { return acceptance_native_scene_ready; }
// Explicit, bounded timing observation. Disabled paths execute no sampler.
#include "native_vblank_timing.hpp"
static native_vblank_timing::Observer acceptance_timing;
static native_vblank_timing::Point acceptance_timing_points[32];
static unsigned acceptance_timing_point_count = 0, acceptance_timing_pending[8] = {};
static unsigned acceptance_timing_mode_address = 0, acceptance_timing_map_anims = 0;
static unsigned acceptance_timing_footprint = 0, acceptance_timing_hram = 0;
static unsigned acceptance_timing_dma_bank = 0, acceptance_timing_dma_return = 0;
static unsigned acceptance_timing_vblank_return = 0, acceptance_timing_chooser = 0;
static unsigned acceptance_timing_far_return = 0;
static bool acceptance_timing_configured = false, acceptance_timing_running = false;
static bool acceptance_timing_done = false;
static unsigned acceptance_timing_fault = 0, acceptance_timing_map_guard = 0;
static native_vblank_timing::Sample acceptance_timing_pre, acceptance_timing_first_failure;
static unsigned acceptance_timing_dma_sp = 0;
static void acceptance_timing_check(bool ok) {
    if (!ok && !acceptance_timing_fault) {
        acceptance_timing_fault = 1; acceptance_timing_first_failure = acceptance_timing_pre;
    }
    if (!ok) acceptance_timing_running = false;
}
static unsigned acceptance_timing_bank(gb* game, unsigned pc) {
    return pc < 0x4000 || pc >= 0x8000 ? 0 :
        1 + (game->get_mbc()->get_rom() - game->get_rom()->get_rom()) / 0x4000;
}
static unsigned acceptance_timing_read16(gb* game, unsigned sp) {
    return game->get_cpu()->read(sp) | game->get_cpu()->read((sp + 1) & 0xffff) << 8;
}
static native_vblank_timing::Sample acceptance_timing_sample(gb* game, const cpu_regs& regs) {
    native_vblank_timing::Sample s;
    s.pc = regs.PC; s.sp = regs.SP; s.bank = acceptance_timing_bank(game, regs.PC);
    s.core_clock = uint32_t(game->get_cpu()->get_clock());
    s.ly = game->get_cpu()->read(0xff44); s.lcdc = game->get_cpu()->read(0xff40);
    s.speed = game->get_cpu()->get_speed(); s.mode = game->get_cpu()->read(acceptance_timing_mode_address);
    for (unsigned i = 0; i < 8; ++i) s.pending[i] = game->get_cpu()->read(acceptance_timing_pending[i]);
    return s;
}
extern "C" unsigned acceptance_profile_timing_point(unsigned bank, unsigned pc, unsigned job) {
    if (acceptance_timing_configured || acceptance_timing_point_count == 32 || !pc || (pc >= 0x8000 && pc < 0xff80) || ((pc < 0x4000 || pc >= 0x8000) && bank)) return 0;
    const unsigned id = ++acceptance_timing_point_count;
    acceptance_timing_points[id - 1] = {uint16_t(bank), uint16_t(pc), uint16_t(id), bool(job)};
    return id;
}
extern "C" unsigned acceptance_profile_timing_field(unsigned index, unsigned address) {
    if (acceptance_timing_configured || index >= 8 || address < 0xff80 || address > 0xfffe) return 0;
    acceptance_timing_pending[index] = address; return 1;
}
extern "C" unsigned acceptance_profile_timing_config(unsigned mode, unsigned anims, unsigned footprint,
        unsigned hram, unsigned dma_bank, unsigned dma_return, unsigned vblank_return,
        unsigned chooser, unsigned far_return) {
    if (acceptance_timing_configured || !mode || !anims || !footprint || hram < 0xff80 || hram + 4 > 0xfffe ||
        !vblank_return || vblank_return >= 0x4000 || (chooser && (!far_return || far_return >= 0x4000))) return 0;
    for (auto address : acceptance_timing_pending) if (!address) return 0;
    acceptance_timing_mode_address = mode; acceptance_timing_map_anims = anims;
    acceptance_timing_footprint = footprint; acceptance_timing_hram = hram;
    acceptance_timing_dma_bank = dma_bank; acceptance_timing_dma_return = dma_return;
    acceptance_timing_vblank_return = vblank_return; acceptance_timing_chooser = chooser;
    acceptance_timing_far_return = far_return;
    acceptance_timing_configured = acceptance_timing.configure(acceptance_timing_points, acceptance_timing_point_count);
    for (unsigned i = 0; i < acceptance_timing_point_count; ++i)
        if (acceptance_timing_points[i].job) acceptance_timing_check(acceptance_timing.job_return_bank(i + 1, 0));
    return acceptance_timing_configured && !acceptance_timing_fault;
}
extern "C" unsigned acceptance_profile_timing_arm() {
    if (!acceptance_timing_configured || acceptance_timing_running || acceptance_timing_done || acceptance_timing_fault || !acceptance_game_context) return 0;
    acceptance_timing_map_guard = acceptance_game_context->get_cpu()->read(acceptance_timing_map_anims);
    acceptance_timing_running = true; return 1;
}
static void acceptance_timing_irq(gb* game, const native_vblank_timing::Sample& pre,
        const cpu_regs& regs, unsigned stacked_pc) {
    if (!acceptance_timing_running) return;
    const auto post = acceptance_timing_sample(game, regs);
    acceptance_timing_pre = pre;
    // First scope supports ordinary VBlank0 only. No alternate VBlank table.
    if (post.pc == 0x40 && post.mode != 0) { acceptance_timing_check(false); return; }
    acceptance_timing_check(acceptance_timing.irq_dispatch(pre, post, stacked_pc));
}
static void acceptance_timing_before(gb* game, const cpu_regs& regs) {
    if (!acceptance_timing_running) return;
    acceptance_timing_pre = acceptance_timing_sample(game, regs);
    if (game->get_cpu()->read(acceptance_timing_map_anims) != acceptance_timing_map_guard ||
        game->get_cpu()->read(acceptance_timing_footprint)) { acceptance_timing_check(false); return; }
    // Independently authenticate tail-call caller frames from frozen ROM binding.
    if (regs.PC == acceptance_timing_points[0].pc && acceptance_timing_pre.bank == 0 &&
        acceptance_timing_read16(game, regs.SP) != acceptance_timing_vblank_return) { acceptance_timing_check(false); return; }
    if (acceptance_timing.active() && acceptance_timing_chooser && regs.PC == acceptance_timing_chooser &&
        acceptance_timing_read16(game, regs.SP) != acceptance_timing_far_return) { acceptance_timing_check(false); return; }
    if (regs.PC == acceptance_timing_hram && acceptance_timing.active()) {
        const unsigned char bytes[] = {0xe2, 0x05, 0x20, 0xfd, 0xc9};
        for (unsigned i = 0; i < 5; ++i)
            if (game->get_cpu()->read(regs.PC + i) != bytes[i]) { acceptance_timing_check(false); return; }
        if (acceptance_timing_read16(game, regs.SP) != acceptance_timing_dma_return) { acceptance_timing_check(false); return; }
        acceptance_timing_dma_sp = regs.SP;
    }
    acceptance_timing_check(acceptance_timing.before(acceptance_timing_pre,
        [game](unsigned sp) { return acceptance_timing_read16(game, sp); }));
}
static void acceptance_timing_after(gb* game, const cpu_regs& regs, unsigned opcode, unsigned cycles) {
    if (!acceptance_timing_running) return;
    const auto post = acceptance_timing_sample(game, regs);
    const bool dma_return = acceptance_timing.active() && opcode == 0xc9 &&
        acceptance_timing_pre.pc == acceptance_timing_hram + 4 &&
        acceptance_timing_pre.sp == acceptance_timing_dma_sp &&
        post.sp == uint16_t(acceptance_timing_dma_sp + 2) && post.pc == acceptance_timing_dma_return &&
        post.bank == acceptance_timing_dma_bank;
    acceptance_timing_check(acceptance_timing.after(post, opcode, cycles, dma_return));
    if (acceptance_timing_running && acceptance_timing.completed() == 16) {
        // Close the16th instruction pair before stopping. No17th dispatch.
        acceptance_timing_check(acceptance_timing.finish());
        acceptance_timing_running = false; acceptance_timing_done = !acceptance_timing_fault;
    }
}
extern "C" uint64_t acceptance_profile_timing_status(unsigned field) {
    const uint64_t values[] = {acceptance_timing_configured, acceptance_timing_running, acceptance_timing_done,
        acceptance_timing_fault, uint64_t(acceptance_timing.failure()), acceptance_timing.completed(),
        acceptance_timing.size(), acceptance_timing.instruction_t(), acceptance_timing.core_t(),
        acceptance_timing.intervals(), acceptance_timing.retained(), acceptance_timing.repeated(), acceptance_timing_map_guard};
    return field < sizeof(values) / sizeof(values[0]) ? values[field] : 0;
}
extern "C" uint64_t acceptance_profile_timing_event(unsigned index, unsigned field) {
    native_vblank_timing::Event first_failure;
    first_failure.sample = acceptance_timing_first_failure;
    const auto* e = index == ~0u ? &first_failure : acceptance_timing.event(index); if (!e) return 0;
    const auto& s = e->sample;
    const uint64_t values[] = {uint64_t(e->kind), e->id, e->instruction_t, e->core_t, e->irq, e->scope,
        s.pc, s.sp, s.bank, s.core_clock, s.ly, s.lcdc, s.mode, s.speed, e->value};
    if (field >= 15 && field < 23) return s.pending[field - 15];
    return field < 15 ? values[field] : 0;
}

static unsigned acceptance_effect_bank(gb* game, unsigned pc) {
    return pc < 0x4000 ? 0 : 1 + (game->get_mbc()->get_rom() - game->get_rom()->get_rom()) / 0x4000;
}
static unsigned acceptance_effect_read(gb* game, unsigned bank, unsigned address) {
    if (address >= 0xd000 && address < 0xe000)
        return game->get_cpu()->get_ram()[bank * 0x1000 + address - 0xd000];
    return game->get_cpu()->read(address);
}
static void acceptance_effect_emit(gb* game, const cpu_regs& regs, unsigned id, unsigned kind,
        const AcceptanceEffectCall& call, unsigned value = 0) {
    if (acceptance_effect_records.size() >= acceptance_effect_capacity) { ++acceptance_effect_overflow; return; }
    AcceptanceEffectRecord event;
    const unsigned values[] = {acceptance_effect_sequence, mobile_observed_frames, id, kind,
        acceptance_effect_bank(game, regs.PC), regs.PC, regs.SP, regs.AF.w, regs.BC.w,
        regs.DE.w, regs.HL.w, game->get_cpu()->read(0xff4f), game->get_cpu()->read(0xff70),
        call.sp, call.pc, call.bank, value, game->get_cpu()->read(0xff51),
        game->get_cpu()->read(0xff52), game->get_cpu()->read(0xff53),
        game->get_cpu()->read(0xff54), game->get_cpu()->read(0xff55)};
    std::copy_n(values, 22, event.fields);
    for (unsigned w = 0; w < acceptance_effect_watches; ++w) {
        const auto& watch = acceptance_effect_watch_points[w];
        for (unsigned i = 0; i < watch.length; ++i)
            event.samples[w][i] = acceptance_effect_read(game, watch.bank, watch.address + i);
    }
    acceptance_effect_records.push_back(event);
}
// Opt-in independent source observer. Existing physical and Head paths do not enable it.
#include "static_ordinary_intent.hpp"
#include "native_reference.hpp"
static native_reference::Observer acceptance_native_reference;
static static_ordinary_intent::Observer acceptance_static_intent;
// Static diagnostics have their own unwindowed completed-world provenance.
// No profiling metric or rendering permission participates in this latch.
static unsigned acceptance_static_world_fields[4][2] = {};
static unsigned acceptance_static_world_map[2] = {}, acceptance_static_world_call_map[2] = {};
static unsigned acceptance_static_world_sp = 0, acceptance_static_world_pc = 0, acceptance_static_world_bank = 0;
static bool acceptance_static_world_configured = false, acceptance_static_world_active = false;
static bool acceptance_static_world_ready = false;
static uint64_t acceptance_static_world_returns = 0;
extern "C" void acceptance_profile_static_world_field(unsigned index, unsigned bank, unsigned address) {
    if (index >= 4 || bank > 7 || address < 0xc000 || address > 0xffff) {
        acceptance_static_intent.fail(10); return;
    }
    acceptance_static_world_fields[index][0] = bank;
    acceptance_static_world_fields[index][1] = address;
    acceptance_static_world_configured = true;
    for (const auto& field : acceptance_static_world_fields)
        acceptance_static_world_configured &= field[1] != 0;
    acceptance_static_world_active = acceptance_static_world_ready = false;
}
static unsigned acceptance_static_world_value(gb* game, unsigned index) {
    const auto* field = acceptance_static_world_fields[index];
    return acceptance_effect_read(game, field[0], field[1]);
}
static bool acceptance_static_world_matches(gb* game, const unsigned* map) {
    return acceptance_static_world_value(game, 0) == map[0] &&
        acceptance_static_world_value(game, 1) == map[1] &&
        acceptance_static_world_value(game, 2) == 2 && acceptance_static_world_value(game, 3) == 0;
}
static void acceptance_static_world_before(gb* game, const cpu_regs& regs) {
    if (!acceptance_static_world_configured) return;
    if (acceptance_static_world_ready && !acceptance_static_world_matches(game, acceptance_static_world_map)) {
        acceptance_static_world_ready = false;
        if (acceptance_static_intent.enabled) acceptance_static_intent.fail(11);
    }
    if (acceptance_static_world_active && !acceptance_static_world_matches(game, acceptance_static_world_call_map))
        acceptance_static_world_active = false;
    if (acceptance_simulation_entry && regs.PC == acceptance_simulation_entry &&
            acceptance_effect_bank(game, regs.PC) == acceptance_simulation_bank) {
        for (unsigned i = 0; i < 2; ++i)
            acceptance_static_world_call_map[i] = acceptance_static_world_value(game, i);
        acceptance_static_world_active = acceptance_static_world_matches(game, acceptance_static_world_call_map);
        acceptance_static_world_sp = regs.SP;
        acceptance_static_world_pc = game->get_cpu()->read(regs.SP) | game->get_cpu()->read((regs.SP + 1) & 0xffff) << 8;
        acceptance_static_world_bank = acceptance_effect_bank(game, acceptance_static_world_pc);
    }
}

extern "C" void acceptance_profile_static_point(unsigned index, unsigned bank, unsigned pc) {
    if (index >= 4 || !pc || pc >= 0x8000 || (pc < 0x4000 ? bank != 0 : !bank)) {
        acceptance_static_intent.fail(10); return;
    }
    acceptance_static_intent.points[index] = {bank, pc};
}
extern "C" void acceptance_profile_static_inputs(unsigned bank, unsigned objects, unsigned x, unsigned y) {
    if (bank > 7 || objects < 0xc000 || objects + static_ordinary_intent::actor_count *
            static_ordinary_intent::actor_stride > 0xe000 || x < 0xc000 || y < 0xc000 || x > 0xffff || y > 0xffff) {
        acceptance_static_intent.fail(10); return;
    }
    acceptance_static_intent.object_bank = bank; acceptance_static_intent.objects = objects;
    acceptance_static_intent.camera_x = x; acceptance_static_intent.camera_y = y;
}
extern "C" unsigned acceptance_profile_static_enable() {
    for (const auto& point : acceptance_static_intent.points)
        if (!point.pc) acceptance_static_intent.fail(10);
    if (!acceptance_static_intent.objects || !acceptance_static_world_configured) acceptance_static_intent.fail(10);
    if (!acceptance_static_world_ready || acceptance_observation_invalid) return 0;
    acceptance_static_intent.enabled = !acceptance_static_intent.failure;
    if (acceptance_static_intent.enabled && acceptance_native_reference.configured)
        acceptance_native_reference.enabled = !acceptance_native_reference.failure;
    return acceptance_static_intent.enabled;
}
// Additive status fields: 0 ABI, 1 first failure, 2 count, 3 latest, 4 opportunity,
// 5 instruction, 6 world readiness, 7 genuine world returns, 8 world call active,
// 9 enabled, 10 started, 11 producer active, 12 returned/unclosed, 13..15 pending
// entry/return/close, 16..19 pre-SP/call-SP/return-PC/bank, 20 restore invalid,
// 21..22 completed map group/number, 23 world predicate configuration complete.
extern "C" uint64_t acceptance_profile_static_status(unsigned field) {
    const uint64_t values[] = {1, acceptance_static_intent.failure, acceptance_static_intent.next_id,
        acceptance_static_intent.latest, acceptance_static_intent.opportunity, acceptance_static_intent.instruction,
        acceptance_static_world_ready, acceptance_static_world_returns, acceptance_static_world_active,
        acceptance_static_intent.enabled, acceptance_static_intent.started, acceptance_static_intent.active,
        acceptance_static_intent.has_return, acceptance_static_intent.pending.entry,
        acceptance_static_intent.pending.returned, acceptance_static_intent.pending.closed,
        acceptance_static_intent.pre_sp, acceptance_static_intent.call_sp, acceptance_static_intent.return_pc,
        acceptance_static_intent.return_bank, acceptance_observation_invalid,
        acceptance_static_world_map[0], acceptance_static_world_map[1], acceptance_static_world_configured};
    return field < 24 ? values[field] : ~uint64_t(0);
}
extern "C" uint64_t acceptance_profile_static_source(uint64_t id, unsigned section, unsigned field) {
    // Diagnostic getters never fail or release a source as a side effect.
    const auto& observer = acceptance_static_intent;
    if (!id || id <= observer.released || id > observer.next_id ||
            observer.next_id - id >= static_ordinary_intent::source_capacity) return ~uint64_t(0);
    const auto* source = &observer.sources[(id - 1) % static_ordinary_intent::source_capacity];
    if (source->id != id) return ~uint64_t(0);
    if (section == 0) {
        const uint64_t values[] = {source->id, source->entry, source->returned, source->closed, source->emitted};
        return field < 5 ? values[field] : ~uint64_t(0);
    }
    if (field >= static_ordinary_intent::input_bytes) return ~uint64_t(0);
    return section == 1 ? source->original[field] : section == 2 ? source->at_return[field] :
        section == 3 ? source->at_close[field] : ~uint64_t(0);
}
extern "C" void acceptance_profile_static_release(uint64_t id) { acceptance_static_intent.release(id); }
extern "C" void acceptance_profile_reference_field(unsigned i, unsigned bank, unsigned address) {
    if (i >= acceptance_native_reference.fields.size() || bank > 7 || !address) { acceptance_native_reference.fail(13); return; }
    acceptance_native_reference.fields[i] = {bank, address};
}
extern "C" void acceptance_profile_reference_actor(unsigned i, unsigned sprite, unsigned palette, unsigned x, unsigned y) {
    if (i >= 4 || !sprite || palette >= 64) { acceptance_native_reference.fail(13); return; }
    acceptance_native_reference.sprites[i] = sprite; acceptance_native_reference.palettes[i] = palette;
    acceptance_native_reference.map_x[i] = x; acceptance_native_reference.map_y[i] = y;
}
extern "C" unsigned acceptance_profile_reference_enable() {
    acceptance_native_reference.configured = true;
    for (const auto p : acceptance_native_reference.fields) if (!p.address) acceptance_native_reference.fail(13);
    for (unsigned s : acceptance_native_reference.sprites) if (!s) acceptance_native_reference.fail(13);
    // Declaring reference mode does not observe unrelated core boot producers.
    // Runtime activation belongs to successful ordinary source enablement.
    return !acceptance_native_reference.failure;
}
extern "C" uint64_t acceptance_profile_reference(unsigned section, unsigned field) {
    const auto& r = acceptance_native_reference;
    if (!section) { const uint64_t v[] = {1, r.failure, r.raster.id, r.raster.entry, r.raster.returned, r.raster.closed};
        return field < 6 ? v[field] : ~uint64_t(0); }
    if (section == 2) { const auto v = r.status(); return field < v.size() ? v[field] : ~uint64_t(0); }
    if (section == 3) return field < r.first_failure.size() ? r.first_failure[field] : ~uint64_t(0);
    if (section == 4) return field < r.expected_key.size() ? r.expected_key[field] : ~uint64_t(0);
    return section == 1 && field < native_reference::bytes ? r.raster.data[field] : ~uint64_t(0);
}
static unsigned acceptance_reference_read(gb* game, unsigned bank, unsigned address) {
    if (bank == 9) return game->get_cpu()->get_vram()[address];
    if (bank == 10) {
        const unsigned color = game->get_lcd()->get_pal(address / 8)[(address % 8) / 2];
        return (color >> (8 * (address % 2))) & 0xff;
    }
    return bank == 8 ? game->get_cpu()->read(address) : acceptance_effect_read(game, bank, address);
}
static unsigned acceptance_oam_dma_page = ~0u, acceptance_oam_dma_sequence = 0, acceptance_oam_dma_frame = 0, acceptance_oam_dma_count = 0;
static unsigned acceptance_oam_dma_producer = 0;
static unsigned acceptance_oam_dma_wbk = 0, acceptance_oam_dma_bank = 0;
static unsigned char acceptance_oam_dma_source[160] = {};
// Called by the isolated core at io_write, before the real synchronous GDMA.
static void acceptance_profile_effect_io_write(gb* game, unsigned address, unsigned value) {
    if (address == 0xff46 && acceptance_timing_running)
        acceptance_timing_check(acceptance_timing.dma_start(acceptance_timing_sample(game, *game->get_cpu()->get_regs()), value));
    if (address == 0xff46 && acceptance_video_enabled && !acceptance_observation_invalid) {
        acceptance_oam_dma_page = value;
        acceptance_oam_dma_wbk = game->get_cpu()->read(0xff70);
        acceptance_oam_dma_bank = (value >= 0xd0 && value < 0xe0) ?
            ((acceptance_oam_dma_wbk & 7) ? acceptance_oam_dma_wbk & 7 : 1) : 0;
        for (unsigned i = 0; i < 160; ++i)
            acceptance_oam_dma_source[i] = acceptance_effect_read(game, acceptance_oam_dma_bank, value * 256 + i);
        acceptance_native_reference.context(mobile_observed_frames, acceptance_static_intent.instruction,
            game->get_cpu()->read(0xff44));
        acceptance_native_reference.on_dma(value, acceptance_oam_dma_bank);
        acceptance_oam_dma_sequence = acceptance_effect_sequence;
        // Freeze the completed native producer at the actual DMA instruction.
        // Unrelated Clear calls do not publish ordinary native OAM.
        // A later return in the same raster frame cannot change this binding.
        acceptance_oam_dma_producer = 0;
        for (unsigned i = acceptance_effect_records.size(); i > 0; --i) {
            const auto& event = acceptance_effect_records[i - 1];
            if ((event.fields[2] == 5 || (event.fields[2] == 8 && event.fields[16] != 0)) && event.fields[3] == 2 &&
                    event.fields[0] < acceptance_oam_dma_sequence) {
                acceptance_oam_dma_producer = i; break;
            }
        }
        acceptance_oam_dma_frame = mobile_observed_frames;
        ++acceptance_oam_dma_count;
    }
    if (address != 0xff55) return;
    if (!acceptance_effect_enabled || acceptance_observation_invalid) return;
    const AcceptanceEffectCall call = acceptance_effect_calls.empty() ?
        AcceptanceEffectCall{~0u, 0, 0, 0} : acceptance_effect_calls.back();
    acceptance_effect_emit(game, acceptance_effect_instruction_regs, call.id, 3, call, value);
}
static void acceptance_effect_before(gb* game, const cpu_regs& regs) {
    if (!acceptance_effect_enabled || acceptance_observation_invalid) return;
    if (acceptance_effect_sequence == ~0u) { ++acceptance_effect_overflow; return; }
    ++acceptance_effect_sequence;
    acceptance_effect_instruction_regs = regs;
    const unsigned bank = acceptance_effect_bank(game, regs.PC);
    for (unsigned id = 0; id < acceptance_effect_spans; ++id) {
        const auto* point = acceptance_effect_points[id];
        const auto* alias = acceptance_effect_aliases[id];
        const bool primary = point[1] && regs.PC == point[1] && bank == (regs.PC < 0x4000 ? 0 : point[0]);
        const bool secondary = alias[1] && regs.PC == alias[1] && bank == (regs.PC < 0x4000 ? 0 : alias[0]);
        if (!primary && !secondary) continue;
        if (acceptance_effect_calls.size() >= 64) { ++acceptance_effect_overflow; continue; }
        const unsigned pc = game->get_cpu()->read(regs.SP) | game->get_cpu()->read((regs.SP + 1) & 0xffff) << 8;
        const AcceptanceEffectCall call = {id, regs.SP, pc, acceptance_effect_bank(game, pc)};
        acceptance_effect_calls.push_back(call);
        acceptance_effect_emit(game, regs, id, 1, call);
    }
}
// Actual render-time hardware, deliberately separate from callback-time state.
// Video callbacks occur after VBlank and otherwise observe the next frame.
static constexpr unsigned acceptance_video_size = 160 + 0x4000 + 128 + 128 + 6 + 160;
static unsigned char acceptance_video_snapshot[acceptance_video_size] = {};
static unsigned acceptance_video_meta[14] = {};
// Sparse changes reconstruct every byte of the actual hardware at every row.
// Maximum 65,536 changed bytes, not full VRAM multiplied by 144 rows.
struct AcceptanceVideoChange { unsigned offset, value; };
static std::vector<AcceptanceVideoChange> acceptance_video_changes;
static unsigned char acceptance_video_previous[acceptance_video_size] = {};
static unsigned acceptance_video_rows[144][16] = {};
extern "C" unsigned acceptance_profile_video_row(unsigned row, unsigned field) {
    return row < 144 && field < 16 ? acceptance_video_rows[row][field] : ~0u;
}
extern "C" unsigned acceptance_profile_video_change(unsigned index, unsigned field) {
    if (field == 2) return acceptance_video_changes.size();
    if (index >= acceptance_video_changes.size() || field > 1) return ~0u;
    return field ? acceptance_video_changes[index].value : acceptance_video_changes[index].offset;
}
// Immutable output snapshot, captured at the actual display invocation before
// interlacing changes its parity. A callback cannot create hardware row credit.
static unsigned acceptance_palette_config[5] = {};
static bool acceptance_palette_render_started = false;
extern "C" void acceptance_profile_palette_current();
// Ordinary capture has no effect span or source-intent dependency.
// State: 0 idle (including a pending row-zero request), 1 armed, 2 complete,
// 3 failed, 4 cancelled incomplete. Status field5 distinguishes pending idle.
static unsigned acceptance_ordinary_state = 0, acceptance_ordinary_limit = 0;
static unsigned acceptance_ordinary_count = 0, acceptance_ordinary_pending = 0;
static const void* acceptance_ordinary_buffer = nullptr;
static bool acceptance_ordinary_rows_started = false;
static unsigned acceptance_ordinary_render_consumed = 0;
static bool acceptance_ordinary_requested = false;
static unsigned acceptance_ordinary_request_frame = 0, acceptance_ordinary_commit_frame = 0;
static unsigned acceptance_ordinary_request_ly = ~0u, acceptance_ordinary_commit_ly = ~0u;
// Diagnostic ABI v1: immutable first failure, scalar fields only. Reason IDs are
// stable; combined predicates retain their original evaluation order.
// 1 LCD-off direct output; 2 null row buffer; 3 row before callback;
// 4 row range; 5 incomplete prior raster; 6 unconsumed prior raster;
// 7 missing row zero; 8 row order; 9 row buffer change;
// 10 display before callback; 11 display without rows; 12 display buffer;
// 13 display without raster; 14 stale display; 15 incomplete display rows;
// 16 raster flags; 17 callback without display; 18 callback buffer;
// 19 negative window line; 20 restore boundary.
static unsigned acceptance_display_output = 0;
static const void* acceptance_display_buffer = nullptr;
static unsigned acceptance_ordinary_failure[27] = {};
static void acceptance_ordinary_fail(unsigned reason, unsigned line = ~0u,
        int window_line = 0, const void* buffer = nullptr) {
    if (acceptance_ordinary_state != 1 && !acceptance_ordinary_requested) return;
    const unsigned values[] = {1, reason, mobile_observed_frames, line, unsigned(window_line),
        acceptance_ordinary_state, acceptance_ordinary_rows_started,
        acceptance_video_meta[0], acceptance_video_meta[1], acceptance_video_meta[2],
        acceptance_video_meta[5], acceptance_video_meta[6], acceptance_video_meta[7],
        acceptance_ordinary_render_consumed, acceptance_ordinary_count, acceptance_ordinary_limit,
        acceptance_ordinary_pending, acceptance_display_output, buffer != nullptr,
        buffer == acceptance_ordinary_buffer, buffer == acceptance_display_buffer,
        acceptance_game_context != nullptr,
        acceptance_game_context ? acceptance_game_context->get_cpu()->read(0xff44) : ~0u,
        acceptance_game_context ? acceptance_game_context->get_cpu()->read(0xff40) : ~0u,
        acceptance_observation_invalid, acceptance_video_enabled, 27};
    std::copy_n(values, 27, acceptance_ordinary_failure);
    acceptance_ordinary_state = 3;
}
extern "C" unsigned acceptance_profile_ordinary_failure(unsigned field) {
    return field < 27 ? acceptance_ordinary_failure[field] : ~0u;
}
extern "C" unsigned acceptance_profile_ordinary_status(unsigned field) {
    const unsigned values[] = {1, acceptance_ordinary_state, acceptance_ordinary_limit,
        acceptance_ordinary_count, acceptance_ordinary_pending, acceptance_ordinary_requested,
        acceptance_ordinary_request_frame, acceptance_ordinary_commit_frame,
        acceptance_ordinary_request_ly, acceptance_ordinary_commit_ly};
    return field < 10 ? values[field] : ~0u;
}
extern "C" unsigned acceptance_profile_ordinary_arm(unsigned count) {
    if (!count || count > 800 || acceptance_ordinary_state == 1 || acceptance_ordinary_requested) return 0;
    std::fill_n(acceptance_ordinary_failure, 27, 0);
    // Request only. The native display of the preceding raster happens before
    // render row zero, so activation here would incorrectly reject that output.
    acceptance_ordinary_requested = true;
    acceptance_ordinary_request_frame = mobile_observed_frames;
    acceptance_ordinary_request_ly = acceptance_game_context ? acceptance_game_context->get_cpu()->read(0xff44) : ~0u;
    acceptance_ordinary_commit_frame = 0; acceptance_ordinary_commit_ly = ~0u;
    acceptance_ordinary_state = 0; acceptance_ordinary_limit = count;
    acceptance_ordinary_count = acceptance_ordinary_pending = 0;
    acceptance_video_enabled = true;
    return 1;
}
extern "C" void acceptance_profile_ordinary_cancel() {
    if (acceptance_ordinary_state == 1 || acceptance_ordinary_requested) acceptance_ordinary_state = 4;
    acceptance_ordinary_requested = false;
}
// Called from both pinned native LCD-off clear paths before they modify vframe.
// Pointer identity cannot associate those direct writes with a captured LCD raster.
extern "C" void acceptance_profile_ordinary_unsupported_output() {
    if (acceptance_ordinary_state == 1) acceptance_ordinary_fail(1);
}
extern "C" void acceptance_profile_render_buffer(const void* buffer, unsigned line) {
    acceptance_native_reference.context(mobile_observed_frames, acceptance_static_intent.instruction,
        acceptance_game_context ? acceptance_game_context->get_cpu()->read(0xff44) : ~0u);
    acceptance_native_reference.row(line);
    if (acceptance_ordinary_requested && line == 0 &&
            (!acceptance_native_reference.configured || acceptance_native_reference.ready())) {
        // This is the genuine lcd::render entry, before either row observer.
        // No previously rendered row or output contributes capture credit.
        acceptance_ordinary_requested = false;
        acceptance_ordinary_state = 1;
        acceptance_ordinary_rows_started = false; acceptance_ordinary_buffer = nullptr;
        acceptance_ordinary_render_consumed = 0;
        acceptance_ordinary_commit_frame = mobile_observed_frames;
        acceptance_ordinary_commit_ly = acceptance_game_context ? acceptance_game_context->get_cpu()->read(0xff44) : ~0u;
    }
    if (acceptance_ordinary_state != 1) return;
    unsigned reason = !buffer ? 2 : acceptance_ordinary_pending ? 3 : line >= 144 ? 4 :
        (line == 0 && acceptance_ordinary_rows_started) ?
            (acceptance_video_meta[5] != 144 ? 5 :
             acceptance_ordinary_render_consumed != acceptance_video_meta[1] ? 6 : 0) :
        line != 0 ? (!acceptance_ordinary_rows_started ? 7 :
            acceptance_video_meta[5] != line ? 8 : buffer != acceptance_ordinary_buffer ? 9 : 0) : 0;
    if (reason) { acceptance_ordinary_fail(reason, line, 0, buffer); return; }
    if (line == 0) {
        acceptance_ordinary_buffer = buffer; acceptance_ordinary_rows_started = true;
    }
}
static unsigned acceptance_display_metadata[24] = {};
static unsigned acceptance_float_bits(float value) {
    static_assert(sizeof(float) == sizeof(unsigned), "binary32 metadata required");
    unsigned bits; std::memcpy(&bits, &value, sizeof(bits)); return bits;
}
extern "C" void acceptance_profile_palette_config(unsigned flags, unsigned mode, float temperature) {
    const unsigned bits = acceptance_float_bits(temperature);
    if (acceptance_palette_render_started && acceptance_palette_config[0] &&
        (acceptance_palette_config[1] != flags || acceptance_palette_config[2] != mode || acceptance_palette_config[3] != bits))
        acceptance_palette_config[4] = 1; // Cached conversion changes are unsupported.
    acceptance_palette_config[0] = 1; acceptance_palette_config[1] = flags;
    acceptance_palette_config[2] = mode; acceptance_palette_config[3] = bits;
}
extern "C" unsigned acceptance_profile_display_field(unsigned index) {
    return index < 24 ? acceptance_display_metadata[index] : ~0u;
}
extern "C" unsigned acceptance_profile_display_matches(const void* buffer) {
    return buffer && buffer == acceptance_display_buffer;
}
extern "C" void acceptance_profile_display_begin(const void* buffer, unsigned width,
    unsigned height, unsigned depth, unsigned pitch, unsigned applied, unsigned fast,
    float factor, unsigned parity, unsigned gbc, unsigned dmg_gbc, unsigned upscale, unsigned blur) {
    if (!acceptance_video_enabled || acceptance_observation_invalid) return;
    if (acceptance_ordinary_state == 1) {
        const unsigned reason = acceptance_ordinary_pending ? 10 : !acceptance_ordinary_rows_started ? 11 :
            buffer != acceptance_ordinary_buffer ? 12 : !acceptance_video_meta[0] ? 13 :
            acceptance_ordinary_render_consumed == acceptance_video_meta[1] ? 14 :
            acceptance_video_meta[5] != 144 ? 15 : acceptance_video_meta[6] ? 16 : 0;
        if (reason) { acceptance_ordinary_fail(reason, ~0u, 0, buffer); return; }
        acceptance_ordinary_pending = acceptance_display_output + 1;
        acceptance_ordinary_render_consumed = acceptance_video_meta[1];
    }
    if (acceptance_display_output == ~0u) { ++acceptance_effect_overflow; return; }
    const unsigned snapshot[] = {1, ++acceptance_display_output, acceptance_video_meta[1],
        acceptance_video_meta[2], acceptance_video_meta[5], width, height, depth, pitch,
        applied, fast, acceptance_float_bits(factor), parity, gbc, dmg_gbc, upscale, blur,
        acceptance_palette_config[0], acceptance_palette_config[1], acceptance_palette_config[2],
        acceptance_palette_config[3], acceptance_palette_config[4], acceptance_video_meta[6], 24};
    std::copy_n(snapshot, 24, acceptance_display_metadata);
    acceptance_display_buffer = buffer;
}
// One acknowledgment per actual display invocation. A duplicate frontend callback
// returns zero; missing/ahead/wrong-buffer callbacks fail closed.
extern "C" unsigned acceptance_profile_ordinary_callback(const void* buffer) {
    if (acceptance_ordinary_state != 1) return 0;
    if (!acceptance_ordinary_pending) {
        if (acceptance_ordinary_count && buffer == acceptance_display_buffer) return 0;
        acceptance_ordinary_fail(17, ~0u, 0, buffer); return 0;
    }
    if (buffer != acceptance_display_buffer || buffer != acceptance_ordinary_buffer) {
        acceptance_ordinary_fail(18, ~0u, 0, buffer); return 0;
    }
    acceptance_ordinary_pending = 0;
    if (++acceptance_ordinary_count == acceptance_ordinary_limit) acceptance_ordinary_state = 2;
    return 1;
}
static unsigned acceptance_video_frame = 0, acceptance_video_until = 0;
extern "C" void acceptance_profile_video_enable(unsigned enabled) {
    acceptance_video_enabled = enabled != 0;
}
extern "C" unsigned acceptance_profile_video_byte(unsigned index) {
    return index < acceptance_video_size ? acceptance_video_snapshot[index] : 0;
}
extern "C" unsigned acceptance_profile_video_field(unsigned index) {
    return index < 14 ? acceptance_video_meta[index] : 0;
}
static void acceptance_video_hardware(gb* game, unsigned char* data) {
    std::memcpy(data, game->get_cpu()->get_oam(), 160);
    std::memcpy(data + 160, game->get_cpu()->get_vram(), 0x4000);
    for (unsigned pal = 0; pal < 16; ++pal) {
        std::memcpy(data + 160 + 0x4000 + pal * 8, game->get_lcd()->get_pal(pal), 8);
        // Read the exact palette conversion actually used by this renderer.
        // get_mapped_pal already includes the configured core color correction.
        std::memcpy(data + 160 + 0x4000 + 128 + pal * 8, game->get_lcd()->get_mapped_pal(pal), 8);
    }
    const unsigned registers[] = {0xff40, 0xff42, 0xff43, 0xff4a, 0xff4b, 0xff46};
    for (unsigned i = 0; i < 5; ++i) data[acceptance_video_size - 160 - 6 + i] = game->get_cpu()->read(registers[i]);
    data[acceptance_video_size - 160 - 1] = acceptance_oam_dma_page;
    std::memcpy(data + acceptance_video_size - 160, acceptance_oam_dma_source, 160);
}
extern "C" void acceptance_profile_render_line(gb* game, unsigned line, int window_line = 1) {
    if (line == 0 && acceptance_timing_running)
        acceptance_timing_check(acceptance_timing.output_row0(acceptance_timing_sample(game, *game->get_cpu()->get_regs())));
    if (!acceptance_video_enabled || acceptance_observation_invalid || line >= 144) return;
    if (acceptance_ordinary_state == 1 && window_line < 0) { acceptance_ordinary_fail(19, line, window_line); return; }
    // The real renderer getter observes final cartridge/configuration state,
    // including private RGB565 state, before first-row latching. Prior mapped
    // caches still require independent exact raw-to-mapped proof.
    acceptance_profile_palette_current();
    if (line == 0) {
        acceptance_palette_render_started = true;
        if (acceptance_video_meta[0] && acceptance_video_meta[5] != 144) ++acceptance_effect_overflow;
        ++acceptance_video_frame;
        if (acceptance_profile_effect_active(0)) acceptance_video_until = acceptance_video_frame + 16;
        std::fill_n(acceptance_video_meta, 14, 0);
        if (acceptance_ordinary_state != 1 && (acceptance_video_frame > acceptance_video_until || !acceptance_video_until)) return;
        acceptance_video_changes.clear();
        std::memset(acceptance_video_rows, 0, sizeof(acceptance_video_rows));
        acceptance_video_hardware(game, acceptance_video_snapshot);
        std::memcpy(acceptance_video_previous, acceptance_video_snapshot, acceptance_video_size);
        acceptance_video_meta[0] = 1;
        acceptance_video_meta[1] = acceptance_video_frame;
        acceptance_video_meta[2] = mobile_observed_frames;
        acceptance_video_meta[3] = acceptance_profile_effect_active(0);
        acceptance_video_meta[8] = acceptance_oam_dma_sequence;
        acceptance_video_meta[9] = acceptance_oam_dma_frame;
        acceptance_video_meta[10] = acceptance_oam_dma_count;
        acceptance_video_meta[11] = acceptance_oam_dma_wbk;
        acceptance_video_meta[12] = acceptance_oam_dma_bank;
        for (unsigned layer = 0; layer < 3; ++layer)
            if (game->get_lcd()->get_enable(layer)) acceptance_video_meta[13] |= 1u << layer;
        acceptance_video_meta[4] = acceptance_oam_dma_producer;
    }
    if (!acceptance_video_meta[0]) return;
    if (acceptance_video_meta[5] != line) acceptance_video_meta[6] |= 1;
    unsigned char actual[acceptance_video_size];
    acceptance_video_hardware(game, actual);
    for (unsigned i = 0; i < acceptance_video_size; ++i) if (actual[i] != acceptance_video_previous[i]) {
        if (acceptance_video_changes.size() >= 65536) { acceptance_video_meta[6] |= 8; ++acceptance_effect_overflow; break; }
        acceptance_video_changes.push_back({i, actual[i]});
    }
    std::memcpy(acceptance_video_previous, actual, acceptance_video_size);
    auto* row = acceptance_video_rows[line];
    row[0] = acceptance_profile_effect_active(0); row[1] = 0;
    row[1] = acceptance_oam_dma_producer;
    row[2] = acceptance_oam_dma_sequence; row[3] = acceptance_oam_dma_frame;
    row[4] = acceptance_oam_dma_count; row[5] = acceptance_oam_dma_wbk; row[6] = acceptance_oam_dma_bank;
    for (unsigned layer = 0; layer < 3; ++layer) if (game->get_lcd()->get_enable(layer)) row[7] |= 1u << layer;
    row[8] = unsigned(window_line); row[9] = acceptance_video_changes.size();
    row[10] = mobile_observed_frames; row[11] = acceptance_effect_sequence;
    // Phase is instruction-derived: before initial animation return, active
    // native Shake, and after actual Shake return, including native-owned tails.
    const bool native_animation = row[1] &&
        (acceptance_effect_records[row[1] - 1].fields[2] == 5 ||
         acceptance_effect_records[row[1] - 1].fields[16] == 1);
    row[12] = acceptance_effect_returns[0] ? 2 : (native_animation ? 1 : 0);
    ++acceptance_video_meta[5]; acceptance_video_meta[7] = line;

}

// Runs after the opcode completes, before the next irq_process can change PC/SP.
static void acceptance_profile_after(gb* game, const cpu_regs& regs, unsigned opcode, unsigned cycles) {
    acceptance_timing_after(game, regs, opcode, cycles);
    if (acceptance_observation_invalid) return;
    const bool returned = opcode == 0xc9 || opcode == 0xd9 ||
        ((opcode & 0xe7) == 0xc0 && cycles == 20);
    if (returned && acceptance_native_reference.emitter_active)
        acceptance_native_reference.context(mobile_observed_frames, acceptance_static_intent.instruction,
            game->get_cpu()->read(0xff44));
    acceptance_native_reference.after(acceptance_effect_bank(game, regs.PC), regs.PC, regs.SP,
        acceptance_instruction_sp, returned, [game](unsigned b, unsigned a) { return acceptance_reference_read(game, b, a); });
    acceptance_static_intent.after(acceptance_effect_bank(game, regs.PC), regs.PC, regs.SP, returned,
        [game](unsigned bank, unsigned address) { return bank == 8 ? game->get_cpu()->read(address) : acceptance_effect_read(game, bank, address); });
    if (returned && acceptance_static_world_active && acceptance_instruction_sp == acceptance_static_world_sp &&
            regs.SP == ((acceptance_static_world_sp + 2) & 0xffff) && regs.PC == acceptance_static_world_pc &&
            acceptance_effect_bank(game, regs.PC) == acceptance_static_world_bank) {
        acceptance_static_world_active = false;
        if (acceptance_static_world_matches(game, acceptance_static_world_call_map)) {
            std::copy_n(acceptance_static_world_call_map, 2, acceptance_static_world_map);
            acceptance_static_world_ready = true;
            ++acceptance_static_world_returns;
        }
    }
    if (!returned) return;
    // Observe completed native world updates before readiness. The memory
    // predicates alone can match during partially initialized map setup.
    if (acceptance_native_scene_count && acceptance_simulation_active &&
        acceptance_instruction_sp == acceptance_simulation_sp &&
        regs.SP == ((acceptance_simulation_sp + 2) & 0xffff) &&
        regs.PC == acceptance_simulation_return &&
        acceptance_effect_bank(game, regs.PC) == acceptance_simulation_return_bank) {
        // Check the selected map and all initial prerequisites at this return.
        // The completed-world provenance must never cross a map boundary.
        acceptance_native_world_completed = true;
        acceptance_native_scene_ready = acceptance_native_scene_is_ready(game);
    }
    if (!acceptance_effect_enabled || acceptance_effect_calls.empty()) return;
    const auto call = acceptance_effect_calls.back();
    if (acceptance_effect_instruction_regs.SP == call.sp &&
        regs.SP == ((call.sp + 2) & 0xffff) && regs.PC == call.pc &&
        acceptance_effect_bank(game, regs.PC) == call.bank) {
        acceptance_effect_emit(game, regs, call.id, 2, call,
            call.id == 8 && acceptance_profile_effect_active(0) ?
                (call.pc == acceptance_effect_teardown_return && call.bank == acceptance_effect_teardown_bank ? 1 : 2) : 0);
        ++acceptance_effect_returns[call.id];
        acceptance_effect_calls.pop_back();
    }
}
static bool acceptance_native_scene_is_ready(gb* game) {
    if (!acceptance_native_scene_count || acceptance_observation_invalid) return false;
    for (unsigned i = 0; i < acceptance_native_scene_count; ++i) {
        const auto* field = acceptance_native_scene_fields[i];
        if (!field[4]) { acceptance_native_scene_ready = 0; acceptance_native_world_completed = false; return false; }
        if (i >= 2 && acceptance_native_scene_ready) continue;
        const unsigned value = acceptance_effect_read(game, field[0], field[1]) & field[3];
        // UINT_MAX means nonzero; all other values mean exact masked equality.
        if (field[2] == ~0u ? value == 0 : value != field[2]) {
            // A failed initial prerequisite invalidates this completion too.
            // Once ready, transient predicates above are deliberately skipped.
            acceptance_native_world_completed = false;
            if (i < 2) {
                acceptance_native_scene_ready = 0;
                acceptance_native_world_completed = false;
                acceptance_simulation_active = false;
            }
            return false;
        }
    }
    return acceptance_native_world_completed;
}

static void acceptance_profile_before(gb* game, const cpu_regs& regs) {
    acceptance_timing_before(game, regs);
    if (acceptance_observation_invalid) return;
    acceptance_game_context = game;
    acceptance_instruction_sp = regs.SP;
    acceptance_static_world_before(game, regs);
    acceptance_static_intent.before(acceptance_effect_bank(game, regs.PC), regs.PC, regs.SP, regs.BC.w,
        [game](unsigned bank, unsigned address) { return bank == 8 ? game->get_cpu()->read(address) : acceptance_effect_read(game, bank, address); });
    if (acceptance_static_intent.enabled && acceptance_static_intent.started) {
        const unsigned b = acceptance_effect_bank(game, regs.PC);
        if (acceptance_static_intent.matches(0, b, regs.PC) || acceptance_static_intent.matches(1, b, regs.PC) ||
                acceptance_static_intent.matches(2, b, regs.PC))
            acceptance_native_reference.context(mobile_observed_frames, acceptance_static_intent.instruction,
                game->get_cpu()->read(0xff44));
        acceptance_native_reference.before(acceptance_static_intent,
            acceptance_static_intent.matches(0, b, regs.PC), acceptance_static_intent.matches(1, b, regs.PC),
            acceptance_static_intent.matches(2, b, regs.PC), regs.BC.w, regs.SP, b,
            [game](unsigned rb, unsigned a) { return acceptance_reference_read(game, rb, a); });
    }
    acceptance_effect_before(game, regs);
    acceptance_native_scene_ready = acceptance_native_scene_is_ready(game);
    if (acceptance_simulation_active && regs.SP == acceptance_simulation_sp + 2 &&
            regs.PC == acceptance_simulation_return) {
        acceptance_simulation_active = false;
        if (acceptance_native_scene_count ? acceptance_native_scene_ready : acceptance_marker_counts[0]) {
            const auto* ram = game->get_cpu()->get_ram();
            unsigned hash = 2166136261u;
            const unsigned offset = acceptance_simulation_objects < 0xd000 ?
                acceptance_simulation_objects - 0xc000 :
                acceptance_simulation_object_bank * 0x1000 + acceptance_simulation_objects - 0xd000;
            for (unsigned slot = 1; slot < 13; ++slot) {
                const auto* actor = ram + offset + slot * acceptance_simulation_stride;
                const unsigned map = actor[acceptance_simulation_map];
                if (map && map <= acceptance_simulation_original)
                    for (unsigned i = 0; i < acceptance_simulation_field_count; ++i)
                        hash = (hash ^ actor[acceptance_simulation_fields[i]]) * 16777619u;
            }
            if (acceptance_simulation_records.size() < 16000)
                acceptance_simulation_records.emplace_back(mobile_observed_frames, hash);
            else ++acceptance_simulation_overflow;
        }
    }
    if (acceptance_simulation_entry && regs.PC == acceptance_simulation_entry &&
            game->get_mbc()->get_rom() == game->get_rom()->get_rom() + (acceptance_simulation_bank - 1) * 0x4000) {
        acceptance_simulation_active = true;
        acceptance_simulation_sp = regs.SP;
        acceptance_simulation_return = game->get_cpu()->read(regs.SP) | game->get_cpu()->read(regs.SP + 1) << 8;
        acceptance_simulation_return_bank = acceptance_effect_bank(game, acceptance_simulation_return);
    }

    if (acceptance_menu_active && regs.SP == acceptance_menu_sp + 2 && regs.PC == acceptance_menu_return)
        acceptance_menu_active = false;
    if (acceptance_menu_entry && regs.PC == acceptance_menu_entry &&
        acceptance_effect_bank(game, regs.PC) == (regs.PC < 0x4000 ? 0 : acceptance_menu_bank)) {
        acceptance_menu_active = true;
        acceptance_menu_sp = regs.SP;
        acceptance_menu_return = game->get_cpu()->read(regs.SP) | game->get_cpu()->read(regs.SP + 1) << 8;
    }
    if (acceptance_save_active && regs.SP == acceptance_save_sp + 2 && regs.PC == acceptance_save_return)
        acceptance_save_active = false;
    if (acceptance_save_entry && regs.PC == acceptance_save_entry &&
        game->get_mbc()->get_rom() == game->get_rom()->get_rom() + (acceptance_save_bank - 1) * 0x4000) {
        acceptance_save_active = true;
        acceptance_save_sp = regs.SP;
        acceptance_save_return = game->get_cpu()->read(regs.SP) | game->get_cpu()->read(regs.SP + 1) << 8;
    }
    for (unsigned i = 0; i < 5; ++i) {
        const auto* point = acceptance_marker_points[i];
        if (point[1] && regs.PC == point[1] && game->get_mbc()->get_rom() == game->get_rom()->get_rom() + (point[0] - 1) * 0x4000)
            ++acceptance_marker_counts[i];
    }
    if (!acceptance_native_entry || (regs.PC != acceptance_native_entry && regs.PC != acceptance_native_facing)) return;
    if (game->get_mbc()->get_rom() != game->get_rom()->get_rom() + (acceptance_native_bank - 1) * 0x4000) return;
    if (regs.PC == acceptance_native_entry) {
        const unsigned start = regs.BC.w;
        acceptance_native_slot = start >= mobile_native_object_address &&
            (start - mobile_native_object_address) % acceptance_native_stride == 0 ? (start - mobile_native_object_address) / acceptance_native_stride : 13;
        if (acceptance_native_slot == 0 && !mobile_native_descriptor_address)
            std::fill_n(mobile_native_descriptors, 104, 0);
    } else if (acceptance_native_slot < 13 && !mobile_native_descriptor_address) {
        auto* view = mobile_native_descriptors + acceptance_native_slot * 8;
        view[0] = game->get_cpu()->read(acceptance_native_hram[0]);
        view[1] = game->get_cpu()->read(acceptance_native_hram[1]);
        view[2] = game->get_cpu()->read(acceptance_native_hram[2]);
        view[3] = regs.DE.b.h;
        view[4] = regs.HL.b.l;
        view[5] = regs.HL.b.h;
        view[6] = regs.AF.b.h;
        view[7] = game->get_cpu()->read(acceptance_native_hram[3]);
    }
}

extern "C" void acceptance_profile_effect_reset() {
    acceptance_timing = native_vblank_timing::Observer{};
    // Load/reset starts a fresh experiment. Runner-declared ROM bindings and
    // fields precede core load and remain immutable across that boundary.
    acceptance_timing_running = acceptance_timing_done = false;
    acceptance_timing_fault = acceptance_timing_dma_sp = acceptance_timing_map_guard = 0;
    acceptance_timing_pre = acceptance_timing_first_failure = native_vblank_timing::Sample{};
    if (acceptance_timing_configured) {
        acceptance_timing_check(acceptance_timing.configure(acceptance_timing_points, acceptance_timing_point_count));
        for (unsigned i = 0; i < acceptance_timing_point_count; ++i)
            if (acceptance_timing_points[i].job) acceptance_timing_check(acceptance_timing.job_return_bank(i + 1, 0));
    }
    acceptance_static_intent.reset();
    acceptance_native_reference.reset();
    acceptance_static_world_active = acceptance_static_world_ready = false;
    acceptance_static_world_returns = 0;
    std::fill_n(acceptance_static_world_map, 2, 0);
    acceptance_observation_invalid = false;
    acceptance_game_context = nullptr;
    acceptance_effect_records.clear(); acceptance_effect_calls.clear();
    std::fill_n(acceptance_effect_returns, acceptance_effect_spans, 0);
    acceptance_effect_sequence = acceptance_effect_overflow = acceptance_native_scene_ready = 0;
    acceptance_native_world_completed = false;
    acceptance_video_frame = acceptance_video_until = 0;
    std::fill_n(acceptance_ordinary_failure, 27, 0);
    acceptance_ordinary_requested = false;
    acceptance_ordinary_request_frame = acceptance_ordinary_commit_frame = 0;
    acceptance_ordinary_request_ly = acceptance_ordinary_commit_ly = ~0u;
    acceptance_ordinary_state = acceptance_ordinary_limit = acceptance_ordinary_count = acceptance_ordinary_pending = 0;
    acceptance_ordinary_rows_started = false; acceptance_ordinary_buffer = nullptr;
    acceptance_ordinary_render_consumed = 0;
    std::fill_n(acceptance_palette_config, 5, 0);
    acceptance_palette_render_started = false;
    std::fill_n(acceptance_display_metadata, 24, 0);
    acceptance_display_output = 0; acceptance_display_buffer = nullptr;
    acceptance_oam_dma_page = ~0u;
    acceptance_oam_dma_producer = 0;
    acceptance_oam_dma_wbk = acceptance_oam_dma_bank = 0;
    std::fill_n(acceptance_oam_dma_source, 160, 0);
    acceptance_oam_dma_sequence = acceptance_oam_dma_frame = acceptance_oam_dma_count = 0;
    std::fill_n(acceptance_video_meta, 14, 0);
    acceptance_video_changes.clear(); std::memset(acceptance_video_rows, 0, sizeof(acceptance_video_rows));
    acceptance_simulation_records.clear(); acceptance_simulation_overflow = 0;
    acceptance_simulation_active = acceptance_menu_active = acceptance_save_active = false;
    std::fill_n(acceptance_marker_counts, 5, 0);
}

// Host observation is not serialized. Restored CPU state cannot prove missed entries.
extern "C" void acceptance_profile_restore_boundary() {
    if (acceptance_timing_configured) { acceptance_timing.restored(); acceptance_timing_check(false); }
    if (acceptance_native_reference.configured) {
        acceptance_native_reference.fail(15);
        acceptance_native_reference.enabled = false;
    }
    acceptance_static_world_active = acceptance_static_world_ready = false;
    if (acceptance_static_world_configured) acceptance_static_intent.fail(12);
    if (acceptance_ordinary_state == 1 || acceptance_ordinary_requested) {
        acceptance_ordinary_fail(20);
        acceptance_ordinary_requested = false;
    }
    if (!acceptance_effect_enabled && !acceptance_native_scene_count && !acceptance_static_world_configured &&
            !acceptance_native_reference.configured) return;
    acceptance_observation_invalid = true;
    ++acceptance_effect_overflow;
    acceptance_effect_calls.clear();
    acceptance_native_scene_ready = 0;
    acceptance_native_world_completed = false;
    std::fill_n(acceptance_video_meta, 14, 0);
    acceptance_simulation_active = acceptance_menu_active = acceptance_save_active = false;
}
