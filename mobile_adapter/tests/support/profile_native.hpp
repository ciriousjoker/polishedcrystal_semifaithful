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
static void acceptance_profile_before(gb* game, const cpu_regs& regs) {
    if (acceptance_simulation_active && regs.SP == acceptance_simulation_sp + 2 &&
            regs.PC == acceptance_simulation_return) {
        acceptance_simulation_active = false;
        if (acceptance_marker_counts[0]) {
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
