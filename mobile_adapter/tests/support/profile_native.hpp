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
static unsigned acceptance_simulation_sp, acceptance_simulation_return;
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
static bool acceptance_effect_enabled = false, acceptance_observation_invalid = false;
static AcceptanceEffectWatch acceptance_effect_watch_points[acceptance_effect_watches];
static std::vector<AcceptanceEffectCall> acceptance_effect_calls;
static std::vector<AcceptanceEffectRecord> acceptance_effect_records;
static unsigned acceptance_effect_sequence = 0, acceptance_effect_overflow = 0;
static cpu_regs acceptance_effect_instruction_regs = {};
static unsigned acceptance_native_scene_fields[8][5] = {}, acceptance_native_scene_count = 0;
static unsigned acceptance_native_scene_ready = 0;
extern "C" void acceptance_profile_effect_span(unsigned id, unsigned bank, unsigned pc) {
    if (id >= acceptance_effect_spans || pc >= 0x8000 || (pc >= 0x4000 && !bank)) {
        ++acceptance_effect_overflow; return;
    }
    acceptance_effect_points[id][0] = bank; acceptance_effect_points[id][1] = pc;
    acceptance_effect_enabled = false;
    for (const auto& point : acceptance_effect_points) acceptance_effect_enabled |= point[1] != 0;
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
// Called by the isolated core at io_write, before the real synchronous GDMA.
static void acceptance_profile_effect_io_write(gb* game, unsigned address, unsigned value) {
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
        if (!point[1] || regs.PC != point[1] || bank != (regs.PC < 0x4000 ? 0 : point[0])) continue;
        if (acceptance_effect_calls.size() >= 64) { ++acceptance_effect_overflow; continue; }
        const unsigned pc = game->get_cpu()->read(regs.SP) | game->get_cpu()->read((regs.SP + 1) & 0xffff) << 8;
        const AcceptanceEffectCall call = {id, regs.SP, pc, acceptance_effect_bank(game, pc)};
        acceptance_effect_calls.push_back(call);
        acceptance_effect_emit(game, regs, id, 1, call);
    }
}
// Runs after the opcode completes, before the next irq_process can change PC/SP.
static void acceptance_profile_after(gb* game, const cpu_regs& regs, unsigned opcode, unsigned cycles) {
    if (!acceptance_effect_enabled || acceptance_observation_invalid || acceptance_effect_calls.empty()) return;
    const bool returned = opcode == 0xc9 || opcode == 0xd9 ||
        ((opcode & 0xe7) == 0xc0 && cycles == 20);
    if (!returned) return;
    const auto call = acceptance_effect_calls.back();
    if (acceptance_effect_instruction_regs.SP == call.sp &&
        regs.SP == ((call.sp + 2) & 0xffff) && regs.PC == call.pc &&
        acceptance_effect_bank(game, regs.PC) == call.bank) {
        acceptance_effect_emit(game, regs, call.id, 2, call);
        acceptance_effect_calls.pop_back();
    }
}
static bool acceptance_native_scene_is_ready(gb* game) {
    if (!acceptance_native_scene_count || acceptance_observation_invalid) return false;
    for (unsigned i = 0; i < acceptance_native_scene_count; ++i) {
        const auto* field = acceptance_native_scene_fields[i];
        if (!field[4]) { acceptance_native_scene_ready = 0; return false; }
        if (i >= 2 && acceptance_native_scene_ready) continue;
        const unsigned value = acceptance_effect_read(game, field[0], field[1]) & field[3];
        // UINT_MAX means nonzero; all other values mean exact masked equality.
        if (field[2] == ~0u ? value == 0 : value != field[2]) {
            if (i < 2) acceptance_native_scene_ready = 0;
            return false;
        }
    }
    return true;
}

static void acceptance_profile_before(gb* game, const cpu_regs& regs) {
    if (acceptance_observation_invalid) return;
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
    }

    if (acceptance_menu_active && regs.SP == acceptance_menu_sp + 2 && regs.PC == acceptance_menu_return)
        acceptance_menu_active = false;
    if (acceptance_menu_entry && regs.PC == acceptance_menu_entry &&
        game->get_mbc()->get_rom() == game->get_rom()->get_rom() + (acceptance_menu_bank - 1) * 0x4000) {
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
    acceptance_effect_records.clear(); acceptance_effect_calls.clear();
    acceptance_effect_sequence = acceptance_effect_overflow = acceptance_native_scene_ready = 0;
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
    acceptance_simulation_active = acceptance_menu_active = acceptance_save_active = false;
}
