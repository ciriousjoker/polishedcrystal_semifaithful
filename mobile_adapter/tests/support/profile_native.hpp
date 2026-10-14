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
static constexpr unsigned acceptance_effect_spans = 8, acceptance_effect_watches = 16;
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
static unsigned acceptance_oam_dma_page = ~0u, acceptance_oam_dma_sequence = 0, acceptance_oam_dma_frame = 0, acceptance_oam_dma_count = 0;
static unsigned acceptance_oam_dma_wbk = 0, acceptance_oam_dma_bank = 0;
static unsigned char acceptance_oam_dma_source[160] = {};
// Called by the isolated core at io_write, before the real synchronous GDMA.
static void acceptance_profile_effect_io_write(gb* game, unsigned address, unsigned value) {
    if (address == 0xff46 && acceptance_video_enabled && !acceptance_observation_invalid) {
        acceptance_oam_dma_page = value;
        acceptance_oam_dma_wbk = game->get_cpu()->read(0xff70);
        acceptance_oam_dma_bank = (value >= 0xd0 && value < 0xe0) ?
            ((acceptance_oam_dma_wbk & 7) ? acceptance_oam_dma_wbk & 7 : 1) : 0;
        for (unsigned i = 0; i < 160; ++i)
            acceptance_oam_dma_source[i] = acceptance_effect_read(game, acceptance_oam_dma_bank, value * 256 + i);
        acceptance_oam_dma_sequence = acceptance_effect_sequence;
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
    if (!acceptance_video_enabled || acceptance_observation_invalid || line >= 144) return;
    if (line == 0) {
        if (acceptance_video_meta[0] && acceptance_video_meta[5] != 144) ++acceptance_effect_overflow;
        ++acceptance_video_frame;
        if (acceptance_profile_effect_active(0)) acceptance_video_until = acceptance_video_frame + 16;
        std::fill_n(acceptance_video_meta, 14, 0);
        if (acceptance_video_frame > acceptance_video_until || !acceptance_video_until) return;
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
        for (unsigned i = acceptance_effect_records.size(); i > 0; --i) {
            const auto& event = acceptance_effect_records[i - 1];
            if ((event.fields[2] == 5 || event.fields[2] == 0) && event.fields[3] == 2 &&
                event.fields[0] <= acceptance_oam_dma_sequence) {
                acceptance_video_meta[4] = i; break;
            }
        }
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
    for (unsigned i = acceptance_effect_records.size(); i > 0; --i) {
        const auto& event = acceptance_effect_records[i - 1];
        if (event.fields[2] == 5 && event.fields[3] == 2 && event.fields[0] <= acceptance_oam_dma_sequence) {
            row[1] = i; break;
        }
    }
    row[2] = acceptance_oam_dma_sequence; row[3] = acceptance_oam_dma_frame;
    row[4] = acceptance_oam_dma_count; row[5] = acceptance_oam_dma_wbk; row[6] = acceptance_oam_dma_bank;
    for (unsigned layer = 0; layer < 3; ++layer) if (game->get_lcd()->get_enable(layer)) row[7] |= 1u << layer;
    row[8] = unsigned(window_line); row[9] = acceptance_video_changes.size();
    row[10] = mobile_observed_frames; row[11] = acceptance_effect_sequence;
    // Phase is instruction-derived: before initial animation return, active
    // native Shake, and after actual Shake return, including native-owned tails.
    row[12] = acceptance_effect_returns[0] ? 2 : (row[1] ? 1 : 0);
    ++acceptance_video_meta[5]; acceptance_video_meta[7] = line;

}

// Runs after the opcode completes, before the next irq_process can change PC/SP.
static void acceptance_profile_after(gb* game, const cpu_regs& regs, unsigned opcode, unsigned cycles) {
    if (acceptance_observation_invalid) return;
    const bool returned = opcode == 0xc9 || opcode == 0xd9 ||
        ((opcode & 0xe7) == 0xc0 && cycles == 20);
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
        acceptance_effect_emit(game, regs, call.id, 2, call);
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
    if (acceptance_observation_invalid) return;
    acceptance_game_context = game;
    acceptance_instruction_sp = regs.SP;
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
    acceptance_observation_invalid = false;
    acceptance_game_context = nullptr;
    acceptance_effect_records.clear(); acceptance_effect_calls.clear();
    std::fill_n(acceptance_effect_returns, acceptance_effect_spans, 0);
    acceptance_effect_sequence = acceptance_effect_overflow = acceptance_native_scene_ready = 0;
    acceptance_native_world_completed = false;
    acceptance_video_frame = acceptance_video_until = 0;
    acceptance_oam_dma_page = ~0u;
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
    if (!acceptance_effect_enabled && !acceptance_native_scene_count) return;
    acceptance_observation_invalid = true;
    ++acceptance_effect_overflow;
    acceptance_effect_calls.clear();
    acceptance_native_scene_ready = 0;
    acceptance_native_world_completed = false;
    std::fill_n(acceptance_video_meta, 14, 0);
    acceptance_simulation_active = acceptance_menu_active = acceptance_save_active = false;
}
