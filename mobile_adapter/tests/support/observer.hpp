// Acceptance-only read access. This header never mutates emulated memory.
static std::map<std::string, Binding> acceptance_symbols;
static uint64_t acceptance_audio_previous = 0;
static std::vector<unsigned char> acceptance_rom;

static unsigned acceptance_data(const std::string& name, unsigned offset = 0) {
    const auto binding = acceptance_symbols.at(name);
    const size_t at = binding.bank * 0x4000 + binding.address - (binding.address >= 0x4000 ? 0x4000 : 0) + offset;
    if (at >= acceptance_rom.size()) fail("Derived acceptance ROM data is out of bounds");
    return acceptance_rom[at];
}
static unsigned acceptance_word(const std::string& name) {
    return acceptance_data(name) | acceptance_data(name, 1) << 8;
}

static void acceptance_load_symbols(const std::string& path, const std::string& rom_path) {
    std::ifstream file(path);
    std::string line, location, name;
    while (std::getline(file, line)) {
        std::istringstream row(line);
        if (!(row >> location >> name)) continue;
        auto colon = location.find(':');
        if (colon == std::string::npos) continue;
        acceptance_symbols[name] = {unsigned(std::stoul(location.substr(0, colon), nullptr, 16)),
                                   unsigned(std::stoul(location.substr(colon + 1), nullptr, 16))};
    }
    std::ifstream rom(rom_path, std::ios::binary);
    acceptance_rom.assign(std::istreambuf_iterator<char>(rom), {});
}

static void acceptance_capture(std::map<std::string, unsigned>& values,
                               const unsigned char* sram, size_t sram_size) {
    auto hash_span = [&](const char* begin, const char* end) {
        const auto first = acceptance_symbols.at(begin), last = acceptance_symbols.at(end);
        if (first.bank != last.bank || first.address > last.address) fail("Acceptance span changed");
        unsigned result = 2166136261u;
        for (unsigned at = first.address; at < last.address; ++at)
            result = (result ^ observe(first.bank, at, sram, sram_size)) * 16777619u;
        return result;
    };
    values["vAcceptancePartyHash"] = hash_span("wPartyCount", "wPokemonDataEnd");
    values["vAcceptanceInventoryHash"] = hash_span("wNumItems", "wItemsEnd");
    const auto trainer = acceptance_symbols.at("wPlayerID");
    values["vAcceptanceTrainerID"] = observe(trainer.bank,trainer.address,sram,sram_size) << 8 |
                                    observe(trainer.bank,trainer.address+1,sram,sram_size);
    const auto objects = acceptance_symbols.at("wObjectStructs");
    const auto end = acceptance_symbols.at("wObjectStructsEnd");
    const unsigned stride = acceptance_word("AcceptanceObjectStride");
    const unsigned count = acceptance_word("AcceptanceObjectCount");
    if (count < 13 || end.address - objects.address != count * stride) fail("Native pool cannot represent the required twelve ordinary objects plus player");
    unsigned hash = 2166136261u, occupied = 0;
    for (unsigned slot = 0; slot < 13; ++slot) {
        const unsigned start = objects.address + slot * stride;
        const unsigned sprite = observe(objects.bank, start + acceptance_word("AcceptanceObjectSprite"), sram, sram_size);
        if (sprite) occupied |= 1u << slot;
        values["vNativeObject" + std::to_string(slot) + "Sprite"] = sprite;
        values["vNativeObject" + std::to_string(slot) + "Map"] = observe(objects.bank, start + acceptance_word("AcceptanceObjectMap"), sram, sram_size);
        // Compare simulation fields, excluding graphics allocation and palette slots.
        const unsigned fields = acceptance_symbols.at("AcceptanceSimulationFieldsEnd").address - acceptance_symbols.at("AcceptanceSimulationFields").address;
        if (slot) for (unsigned i = 0; i < fields; ++i)
            hash = (hash ^ observe(objects.bank, start + acceptance_data("AcceptanceSimulationFields", i), sram, sram_size)) * 16777619u;
    }
    values["vNPCOccupiedMask"] = occupied;
    values["vAcceptanceNativeHash"] = hash;
    std::set<unsigned> native_colors;
    for (unsigned slot = 0; slot < 13; ++slot) {
        const unsigned at = video_native_start[slot];
        if (!video_native_complete[slot] || at >= 160) continue;
        unsigned color = 2166136261u;
        for (unsigned i = 0; i < 8; ++i)
            color = (color ^ video_object_palettes[(video_oam[at + 3] & 7) * 8 + i]) * 16777619u;
        values["vNativeSource" + std::to_string(slot) + "PaletteHash"] = color;
        if (slot) native_colors.insert(color);
    }
    values["vAcceptancePaletteDemand"] = native_colors.size();
    unsigned oam_hash = 2166136261u;
    for (auto byte : video_oam) oam_hash = (oam_hash ^ byte) * 16777619u;
    values["vAcceptanceOAMHash"] = oam_hash;
    const auto local = acceptance_symbols.at("wPlayerCurrentOAMSlot");
    const unsigned start = observe(local.bank, local.address, sram, sram_size);
    const unsigned facing = observe(objects.bank, objects.address + acceptance_word("AcceptanceObjectFacing"), sram, sram_size);
    auto facing_quads = [&](unsigned index) {
        const unsigned pointer = acceptance_data("Facings", index * 2) | acceptance_data("Facings", index * 2 + 1) << 8;
        const unsigned bank = acceptance_symbols.at("Facings").bank;
        const unsigned offset = bank * 0x4000 + pointer - 0x4000;
        return pointer >= 0x4000 && offset < acceptance_rom.size() ? unsigned(acceptance_rom[offset]) : 0;
    };
    const unsigned quad_count = facing_quads(facing);
    bool complete = quad_count > 0 && quad_count <= 16 && start + quad_count * 4 <= 160;
    auto figure_tiles = [&](unsigned at, unsigned quads) {
        std::vector<std::vector<unsigned char>> tiles;
        for (unsigned q = 0; q < quads && at + q * 4 + 3 < 160; ++q) {
            unsigned offset = ((video_oam[at + q * 4 + 3] & 8) >> 3) * 0x2000 + video_oam[at + q * 4 + 2] * 16;
            tiles.emplace_back(video_native_tiles + offset, video_native_tiles + offset + 16);
        }
        std::sort(tiles.begin(), tiles.end());
        unsigned hash = 2166136261u;
        for (const auto& tile : tiles) for (auto byte : tile) hash = (hash ^ byte) * 16777619u;
        return hash;
    };
    unsigned palette_hash = 2166136261u, tile_hash = 2166136261u;
    for (unsigned q = 0; complete && q < quad_count; ++q) {
        const unsigned at = start + q * 4;
        complete &= video_oam[at] > 0 && video_oam[at] < 160;
        for (int row = int(video_oam[at]) - 16; row < int(video_oam[at]) - 8; ++row) {
            if (row < 0 || row >= 144) continue;
            unsigned earlier = 0;
            for (unsigned candidate = 0; candidate < at; candidate += 4)
                if (int(video_oam[candidate]) - 16 <= row && row < int(video_oam[candidate]) - 8) ++earlier;
            if (earlier >= 10) complete = false;
        }
        const unsigned bank = (video_oam[at + 3] & 8) >> 3;
        const unsigned tile = video_oam[at + 2];
        for (unsigned byte = 0; byte < 16; ++byte)
            tile_hash = (tile_hash ^ video_native_tiles[bank * 0x2000 + tile * 16 + byte]) * 16777619u;
    }
    if (start <= 144) for (unsigned byte = 0; byte < 8; ++byte)
        palette_hash = (palette_hash ^ video_object_palettes[(video_oam[start + 3] & 7) * 8 + byte]) * 16777619u;
    values["vAcceptanceLocalComplete"] = complete;
    values["vAcceptanceLocalTileHash"] = figure_tiles(start, quad_count);
    values["vAcceptanceLocalPaletteHash"] = palette_hash;
    for (unsigned peer = 1; peer <= 3; ++peer) {
        const std::string prefix = "wMobilePeer" + std::to_string(peer);
        const unsigned peer_quads = values.count(prefix + "NativeFacing") ? facing_quads(values.at(prefix + "NativeFacing")) : 0;
        const bool ready = values.count(prefix + "Visible") && values.at(prefix + "Visible") == 1 &&
                           values.count(prefix + "NativeStage") && values.at(prefix + "NativeStage") == 7;
        unsigned selected = 0;
        const unsigned at = values.count(prefix + "OAMStart") ? values.at(prefix + "OAMStart") : 160;
        if (ready && peer_quads && peer_quads <= 16 && at + peer_quads * 4 <= 160) for (unsigned q = 0; q < peer_quads; ++q) {
            const unsigned current = at + q * 4;
            bool drawn = video_oam[current] > 0 && video_oam[current] < 160;
            for (int row = int(video_oam[current]) - 16; row < int(video_oam[current]) - 8; ++row) {
                if (row < 0 || row >= 144) continue;
                unsigned earlier = 0;
                for (unsigned candidate = 0; candidate < current; candidate += 4)
                    if (int(video_oam[candidate]) - 16 <= row && row < int(video_oam[candidate]) - 8) ++earlier;
                if (earlier >= 10) drawn = false;
            }
            selected += drawn;
        }
        values["vAcceptancePeer" + std::to_string(peer) + "Complete"] = peer_quads && selected == peer_quads;
        values["vAcceptancePeer" + std::to_string(peer) + "Partial"] = selected > 0 && selected < peer_quads;
        values["vAcceptancePeer" + std::to_string(peer) + "TileHash"] = figure_tiles(at, peer_quads);
    }
    values["vAcceptanceFrameAudio"] = audio_samples - acceptance_audio_previous;
    acceptance_audio_previous = audio_samples;
    // The exact source spans and both checksum-covered save copies remain observable.
    values["vAcceptanceSaveMain"] = hash_span("sGameData", "sGameDataEnd");
    values["vAcceptanceSaveBackup"] = hash_span("sBackupGameData", "sBackupGameDataEnd");
}
