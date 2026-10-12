// Native emulator probe with scripted input, battery saves, and symbol observers.
// Pinned DoubleCherryGB backend; Python runners own scenario assertions.
// Build with the pinned core's libretro include directory and run one process
// per instance. Acceptance tests boot battery saves. Explicit snapshot flags are
// for regenerating manual-play start states with the current ROM/core only.
#include <libretro.h>
#include <zlib.h>
#include <dlfcn.h>
#include <cstdarg>
#include <cstdint>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

static std::map<std::string, std::string> options;
static std::string system_directory;
static bool loaded = false;
static bool updated = true;
static bool adapter_option_requested = false;
static bool adapter_enabled = true;
static unsigned input_frame = 0;
static unsigned video_frames = 0, width = 0, height = 0;
static uint64_t audio_samples = 0, audio_batches = 0;
static retro_pixel_format pixel_format = RETRO_PIXEL_FORMAT_0RGB1555;
static std::vector<unsigned char> image;
static unsigned char video_oam[160] = {};
static unsigned (*native_oam_byte)(unsigned) = nullptr;
static unsigned (*native_figure_byte)(unsigned) = nullptr;
static unsigned video_native_source_count[13] = {}, video_native_complete[13] = {};
static unsigned video_native_source_sprite[13] = {}, video_native_source_map[13] = {};
static unsigned char video_object_palettes[64] = {};
static unsigned char video_peer_tiles[128] = {};
static unsigned char video_native_tiles[0x4000] = {};
static unsigned char video_virtual_owners[3] = {};
static unsigned char video_virtual_frames[3] = {};
static unsigned char video_virtual_movements[3] = {};
static unsigned char video_virtual_cache_poses[3][64] = {};
static std::ofstream input_events;
static std::set<std::pair<unsigned, unsigned>> logged_inputs;
static std::vector<retro_memory_descriptor> memory_maps;
static unsigned restoring_native_slots[3] = {};
struct InputAction { unsigned first, end, button; };
static std::vector<InputAction> input_actions = {{700, 710, RETRO_DEVICE_ID_JOYPAD_START}};
struct ConditionalAction {
    unsigned earliest, button, duration;
    std::map<std::string, unsigned> conditions;
    bool fired = false;
};
static std::vector<ConditionalAction> conditional_actions;
struct WalkWaypoint { unsigned earliest, x, y, timeout, started = 0; };
static std::vector<WalkWaypoint> walk_waypoints;
static size_t walk_index = 0;
static unsigned walk_button = 0xffff, walk_ready_frame = 0;
struct Binding { unsigned bank, address; };
static std::map<std::string, Binding> bindings;
static const std::map<std::string, unsigned> buttons = {
    {"A", RETRO_DEVICE_ID_JOYPAD_A}, {"B", RETRO_DEVICE_ID_JOYPAD_B},
    {"START", RETRO_DEVICE_ID_JOYPAD_START}, {"SELECT", RETRO_DEVICE_ID_JOYPAD_SELECT},
    {"UP", RETRO_DEVICE_ID_JOYPAD_UP}, {"DOWN", RETRO_DEVICE_ID_JOYPAD_DOWN},
    {"LEFT", RETRO_DEVICE_ID_JOYPAD_LEFT}, {"RIGHT", RETRO_DEVICE_ID_JOYPAD_RIGHT}
};

static void fail(const std::string& message) {
    std::cerr << message << '\n';
    std::exit(1);
}

static const unsigned char* descriptor(unsigned start) {
    for (const auto& entry : memory_maps)
        if (entry.start == start && entry.ptr)
            return static_cast<const unsigned char*>(entry.ptr) + entry.offset;
    return nullptr;
}

static unsigned observe(unsigned bank, unsigned address, const unsigned char* sram, size_t sram_size) {
    // The pinned core exposes physical WRAM through the C000 descriptor. Its
    // cpu::ram is 8 * 4 KiB and SVBK selects ram + bank * 0x1000. Do not call
    // its broken SYSTEM_RAM/VIDEO_RAM accessors, which select other instances.
    if (address >= 0xc000 && address < 0xe000 && bank < 8) {
        const auto* ram = descriptor(0xc000);
        if (!ram) fail("WRAM memory map unavailable");
        size_t offset = address < 0xd000 ? address - 0xc000 : bank * 0x1000 + address - 0xd000;
        return ram[offset];
    }
    if (address >= 0xff80 && address < 0xffff) {
        const auto* hram = descriptor(0xff80);
        if (!hram) fail("HRAM memory map unavailable");
        return hram[address - 0xff80];
    }
    if (address >= 0xa000 && address < 0xc000) {
        size_t offset = bank * 0x2000 + address - 0xa000;
        if (!sram || offset >= sram_size) fail("SRAM observer outside battery buffer");
        return sram[offset];
    }
    fail("Unsupported observer region");
    return 0;
}

static void load_bindings(const std::string& path) {
    const std::set<std::string> watched = {
        "hCGB", "hCrashCode", "hVBlankCounter", "hDelayFrameLY", "hInMenu", "wJumptableIndex",
        "hNbTilesToCopy", "hRequested1bpp", "hRequested2bpp", "hHours", "hMinutes", "wTimeOfDay",
        "wInputFlags", "wGameLogicPaused",
        "wSaveFileExists", "wMapGroup", "wMapNumber", "wXCoord", "wYCoord",
        "wPlayerMapX", "wPlayerMapY", "wPlayerSpriteX", "wPlayerSpriteY",
        "wPlayerSpriteXOffset", "wPlayerSpriteYOffset", "wPlayerFacing", "wPlayerDirection",
        "wPlayerStepFrame", "wPlayerBGMapOffsetX", "wPlayerBGMapOffsetY", "wMapStatus", "wScriptRunning",
        "wObjectStructs", "wEventFlags", "wCurBox", "wPlayerCurrentOAMSlot",
        "wPlayerGender", "wPlayerSprite", "wPlayerPalette", "wPalFlags", "wPalState", "wPalFadeDelayFrames",
        "wPlayerWalking", "wPlayerStepDuration", "wPlayerState", "wPanningAroundTinyMap",
        "hUsedWeatherSpriteIndex", "hUsedOAMIndex",
        "wMenuCursorY", "wMenuSelection", "wOptionsMenuIsInitial",
        "sCheckValue1", "sCheckValue2", "sSaveVersion", "sChecksum",
        "sBackupCheckValue1", "sBackupCheckValue2"
    };
    std::ifstream symbols(path);
    if (!symbols) fail("Cannot read matching symbols");
    std::string line;
    while (std::getline(symbols, line)) {
        std::istringstream row(line);
        std::string location, name;
        if (!(row >> location >> name) || location[0] == ';') continue;
        if (!watched.count(name) && name.rfind("wMobile", 0) != 0 && name.rfind("wLoadedObjPal", 0) != 0) continue;
        auto colon = location.find(':');
        if (colon == std::string::npos) {
            if (name.rfind("wMobileVirtual", 0) != 0) fail("Invalid symbol location");
            const unsigned address = std::stoul(location, nullptr, 16);
            const bool resident_pose = (name.rfind("wMobileVirtualQueued", 0) == 0 ||
                                        name == "wMobileVirtualBackgroundFrame") &&
                                       address >= 0xcf0b && address <= 0xcf14;
            if (!resident_pose && (address < 0xcf44 || address >= 0xdd00))
                fail("Virtual alias outside audited private WRAM");
            bindings[name] = {address < 0xd000 ? 0u : 5u, address};
            continue;
        }
        bindings[name] = {static_cast<unsigned>(std::stoul(location.substr(0, colon), nullptr, 16)),
                          static_cast<unsigned>(std::stoul(location.substr(colon + 1), nullptr, 16))};
    }
}

static std::map<std::string, unsigned> capture(const unsigned char* sram, size_t sram_size) {
    const std::set<std::string> words = {"sChecksum", "wMobileRemaining", "wMobileTxPointer",
        "wMobileRxPointer", "wMobileSequence", "wMobileAttemptTicks", "wMobileIRQCount",
        "wMobilePresenceCount", "wMobileMapEpoch", "wMobilePeer1DrawX", "wMobilePeer1DrawY",
        "wMobilePeer2DrawX", "wMobilePeer2DrawY", "wMobilePeer3DrawX", "wMobilePeer3DrawY"};
    const std::set<std::string> wire_words = {"wMobilePlayerId", "wMobileGeneration", "wMobileInstanceKey",
        "wMobilePeer1Id", "wMobilePeer1Generation", "wMobilePeer1Revision",
        "wMobilePeer1Instance", "wMobilePeer1Epoch", "wMobilePeer1X", "wMobilePeer1Y",
        "wMobilePeer2Id", "wMobilePeer2Generation", "wMobilePeer2Revision",
        "wMobilePeer2Instance", "wMobilePeer2Epoch", "wMobilePeer2X", "wMobilePeer2Y",
        "wMobilePeer3Id", "wMobilePeer3Generation", "wMobilePeer3Revision",
        "wMobilePeer3Instance", "wMobilePeer3Epoch", "wMobilePeer3X", "wMobilePeer3Y"};
    std::map<std::string, unsigned> values;
    for (const auto& entry : bindings) {
        unsigned bank = entry.second.bank, address = entry.second.address;
        // End labels can be at the end of a region and are not state bytes.
        if (entry.first.find("End") != std::string::npos && entry.first != "wMobileGuardEnd") continue;
        unsigned value = observe(bank, address, sram, sram_size);
        if (entry.first == "sSaveVersion") value = (value << 8) | observe(bank, address + 1, sram, sram_size);
        if (words.count(entry.first)) value |= observe(bank, address + 1, sram, sram_size) << 8;
        if (wire_words.count(entry.first)) value = (value << 8) | observe(bank, address + 1, sram, sram_size);
        values[entry.first] = value;
    }
    if (bindings.count("wMobileLocalStaging")) {
        const auto local = bindings.at("wMobileLocalStaging");
        for (unsigned axis = 0; axis < 2; ++axis) {
            unsigned address = local.address + 6 + axis * 2;
            values[axis ? "vMobileLocalY" : "vMobileLocalX"] =
                (observe(local.bank, address, sram, sram_size) << 8) |
                observe(local.bank, address + 1, sram, sram_size);
        }
    }
    if (bindings.count("wObjectStructs") && descriptor(0x10000)) {
        auto objects = bindings.at("wObjectStructs");
        unsigned palette = observe(objects.bank, objects.address + 6, sram, sram_size) & 7;
        unsigned hash = 2166136261u;
        for (unsigned i = 0; i < 8; ++i)
            hash = (hash ^ video_object_palettes[palette * 8 + i]) * 16777619u;
        values["vLocalPaletteHash"] = hash;
    }
    for (const char* peer : {"1", "2", "3"}) {
        unsigned tile_hash = 2166136261u;
        unsigned tile_start = peer[0] == '1' ? 0 : 64;
        for (unsigned i = 0; i < 64; ++i)
            tile_hash = (tile_hash ^ video_peer_tiles[tile_start + i]) * 16777619u;
        values[std::string("vMobilePeer") + peer + "TileHash"] = tile_hash;
        if (bindings.count(std::string("wMobileAvatarCache") + peer)) {
            auto cache = bindings.at(std::string("wMobileAvatarCache") + peer);
            unsigned cache_hash = 2166136261u;
            for (unsigned i = 0; i < 384; ++i)
                cache_hash = (cache_hash ^ observe(cache.bank, cache.address + i, sram, sram_size)) * 16777619u;
            values[std::string("vMobilePeer") + peer + "CacheHash"] = cache_hash;
        }
        std::string prefix = std::string("wMobilePeer") + peer;
        if (!values.count(prefix + "OAMStart")) continue;
        unsigned slot = values.at(prefix + "OAMStart");
        if (bindings.count("wMobileVirtualActorMode")) {
            values[std::string("vMobilePeer") + peer + "NativeSlot"] = 0;
            unsigned cache_hash = 2166136261u;
            for (unsigned char byte : video_virtual_cache_poses[peer[0] - '1'])
                cache_hash = (cache_hash ^ byte) * 16777619u;
            values[std::string("vMobilePeer") + peer + "CacheHash"] = cache_hash;
            unsigned window = 3;
            for (unsigned index = 0; index < 3; ++index)
                if (video_virtual_owners[index] == unsigned(peer[0] - '0')) window = index;
            slot = 160;
            unsigned palette = values.at(prefix + "Palette") & 7;
            if (window < 3) {
                const unsigned base = 100 + window * 4;
                for (unsigned i = 0; i <= 144; i += 4) {
                    unsigned mask = 0;
                    bool complete = true;
                    bool visible = false;
                    for (unsigned q = 0; q < 4; ++q) {
                        unsigned tile = video_oam[i + q * 4 + 2];
                        const unsigned bank = (video_oam[i + q * 4 + 3] & 8) / 8;
                        const unsigned ordinal = window == 2 && bank == 1 && tile == 63 ? 3 :
                            bank == 0 && tile >= base && tile < base + (window == 2 ? 3 : 4) ? tile - base : 4;
                        if (ordinal >= 4) {
                            complete = false; break;
                        }
                        visible |= video_oam[i + q * 4] >= 9 && video_oam[i + q * 4] < 160;
                        mask |= 1u << ordinal;
                    }
                    // A canonical figure crossing the viewport boundary still
                    // has four coherent quads, although only some select rows.
                    if (complete && visible && mask == 15) { slot = i; palette = video_oam[i + 3] & 7; break; }
                }
                unsigned hash = 2166136261u;
                for (unsigned i = 0; i < 64; ++i) {
                    const unsigned address = window == 2 && i >= 48 ? 0x2000 + 63 * 16 + i - 48 : base * 16 + i;
                    hash = (hash ^ video_native_tiles[address]) * 16777619u;
                }
                values[std::string("vMobilePeer") + peer + "TileHash"] = hash;
            }
            if (descriptor(0x10000)) {
                unsigned hash = 2166136261u;
                for (unsigned i = 0; i < 8; ++i) hash = (hash ^ video_object_palettes[palette * 8 + i]) * 16777619u;
                values[std::string("vMobilePeer") + peer + "PaletteHash"] = hash;
            }
            values[prefix + "Palette"] = palette;
            values[prefix + "LoadedFrame"] = video_virtual_frames[peer[0] - '1'];
            values[prefix + "LoadedMovement"] = video_virtual_movements[peer[0] - '1'];
            values[prefix + "OAMStart"] = slot;
        } else if (bindings.count("wMobileNativeActorMode")) {
            const auto slots = bindings.at("wMobileSavedFacings");
            const auto objects = bindings.at("wObjectStructs");
            unsigned actor = observe(slots.bank, slots.address + peer[0] - '1', sram, sram_size);
            values[std::string("vMobilePeer") + peer + "NativeSlot"] = actor;
            const auto backup_name = std::string("wMobileActorBackup") + peer;
            if (bindings.count(backup_name)) {
                const auto backup = bindings.at(backup_name);
                values[std::string("vMobilePeer") + peer + "BackupMap"] = observe(backup.bank, backup.address + 1, sram, sram_size);
                unsigned hash = 2166136261u;
                for (unsigned field = 0; field < 34; ++field)
                    hash = (hash ^ observe(backup.bank, backup.address + field, sram, sram_size)) * 16777619u;
                values[std::string("vMobilePeer") + peer + "BackupHash"] = hash;
                values[std::string("vMobilePeer") + peer + "BackupSprite"] = observe(backup.bank, backup.address, sram, sram_size);
                const unsigned pool = peer[0] - '1';
                const unsigned role = values.at(prefix + "NativeRole");
                if (role == 2 && actor > 0 && actor < 13) restoring_native_slots[pool] = actor;
                unsigned restored_actor = actor;
                bool restored = role == 2 && values.at(prefix + "NativeStage") == 7;
                if (role == 0 && restoring_native_slots[pool]) {
                    restored_actor = restoring_native_slots[pool];
                    const unsigned object = objects.address + restored_actor * 34;
                    restored = observe(objects.bank, object, sram, sram_size) ==
                                   observe(backup.bank, backup.address, sram, sram_size) &&
                               observe(objects.bank, object + 1, sram, sram_size) ==
                                   observe(backup.bank, backup.address + 1, sram, sram_size);
                    restoring_native_slots[pool] = 0;
                }
                if (restored && restored_actor > 0 && restored_actor < 13 &&
                        observe(backup.bank, backup.address, sram, sram_size) != 191) {
                    const auto original = bindings.at(std::string("wMobileActorTiles") + peer);
                    unsigned saved_hash = 2166136261u, restored_hash = 2166136261u;
                    const auto* actual = descriptor(0x8000);
                    if (!actual) fail("Native VRAM observer unavailable");
                    unsigned tile_bank = restored_actor < 8 ? 0 : 1;
                    unsigned base = tile_bank * 0x2000 + (tile_bank ? restored_actor - 8 : restored_actor) * 192;
                    for (unsigned byte = 0; byte < 384; ++byte) {
                        unsigned offset = base + (byte < 192 ? byte : byte - 192 + (tile_bank ? 0x400 : 0x800));
                        saved_hash = (saved_hash ^ observe(original.bank, original.address + byte, sram, sram_size)) * 16777619u;
                        restored_hash = (restored_hash ^ actual[offset]) * 16777619u;
                    }
                    values[std::string("vMobilePeer") + peer + "OriginalTileHash"] = saved_hash;
                    values[std::string("vMobilePeer") + peer + "RestoredTileHash"] = restored_hash;
                }
            }
            slot = 160;
            if (actor > 0 && actor < 13) {
                unsigned object = objects.address + actor * 34;
                for (unsigned field = 0; field < 34; ++field)
                    values[std::string("vMobilePeer") + peer + "NativeField" + std::to_string(field)] = observe(objects.bank, object + field, sram, sram_size);
                unsigned base = observe(objects.bank, object + 2, sram, sram_size);
                unsigned facing = observe(objects.bank, object + 13, sram, sram_size);
                unsigned bank = base < 128 ? 1 : 0;
                unsigned source_frame = facing & 12;
                if (source_frame == 12) source_frame = 8;
                if (facing & 1) source_frame += 12;
                unsigned palette = observe(objects.bank, object + 6, sram, sram_size) & 7;
                values[prefix + "Palette"] = palette;
                if (descriptor(0x10000)) {
                    unsigned color_hash = 2166136261u;
                    for (unsigned i = 0; i < 8; ++i)
                        color_hash = (color_hash ^ video_object_palettes[palette * 8 + i]) * 16777619u;
                    values[std::string("vMobilePeer") + peer + "PaletteHash"] = color_hash;
                }
                if (bindings.count("wMobilePrivatePalColors")) {
                    auto colors = bindings.at("wMobilePrivatePalColors");
                    unsigned color_hash = 2166136261u;
                    for (unsigned i = 0; i < 8; ++i)
                        color_hash = (color_hash ^ observe(colors.bank, colors.address + (peer[0] - '1') * 8 + i,
                                                          sram, sram_size)) * 16777619u;
                    values[std::string("vMobilePeer") + peer + "PrivatePaletteHash"] = color_hash;
                }
                // Native InitSprites submits each four-quad figure backwards.
                // Inspect a complete coherent hardware group, including its
                // actual previous-frame facing, rather than adjoining actors.
                for (unsigned i = 0; i <= 144; i += 4) {
                    unsigned first = video_oam[i + 14];
                    unsigned base_tile = base & 127;
                    unsigned offset = first >= base_tile ? first - base_tile : 256;
                    bool walking = offset >= (bank ? 64u : 128u);
                    if (walking) offset -= bank ? 64u : 128u;
                    if (offset != 0 && offset != 4 && offset != 8) continue;
                    bool complete = true;
                    unsigned mask = 0;
                    bool visible = false;
                    for (unsigned q = 0; q < 4; ++q) {
                        unsigned t = video_oam[i + q * 4 + 2];
                        if (t < first || t >= first + 4 ||
                            (video_oam[i + q * 4 + 3] & 15) != (bank * 8 + palette)) {
                            complete = false; break;
                        }
                        mask |= 1u << (t - first);
                        visible |= video_oam[i + q * 4] > 0 && video_oam[i + q * 4] < 160;
                    }
                    if (complete && visible && mask == 15) {
                        slot = i; source_frame = offset + (walking ? 12 : 0); break;
                    }
                }
                unsigned tile = (base & 127) + source_frame % 12;
                if (source_frame >= 12) tile += bank ? 64 : 128;
                unsigned hash = 2166136261u;
                for (unsigned i = 0; i < 64; ++i)
                    hash = (hash ^ video_native_tiles[bank * 0x2000 + tile * 16 + i]) * 16777619u;
                values[std::string("vMobilePeer") + peer + "TileHash"] = hash;
                values[prefix + "LoadedFrame"] = source_frame;
                values[prefix + "OAMStart"] = slot;
            }
        }
        if (slot > 144) {
            values[std::string("vMobilePeer") + peer + "OAM0"] = 160;
            for (unsigned i = 1; i < 16; ++i) values[std::string("vMobilePeer") + peer + "OAM" + std::to_string(i)] = 0;
            continue;
        }
        for (unsigned i = 0; i < 16; ++i)
            values[std::string("vMobilePeer") + peer + "OAM" + std::to_string(i)] = video_oam[slot + i];
    }
    if (bindings.count("wObjectStructs") && bindings.count("wMobileSavedFacings")) {
        const auto objects = bindings.at("wObjectStructs");
        const auto saved = bindings.at("wMobileSavedFacings");
        unsigned occupied = 0, suppressed = 0, restored = 0, positions = 2166136261u;
        for (unsigned i = 0; i < 13; ++i) {
            unsigned start = objects.address + i * 34;
            if (observe(objects.bank, start, sram, sram_size)) {
                occupied |= 1u << i;
                if (i) {
                    positions = (positions ^ observe(objects.bank, start + 16, sram, sram_size)) * 16777619u;
                    positions = (positions ^ observe(objects.bank, start + 17, sram, sram_size)) * 16777619u;
                }
            }
            values["vNativeObject" + std::to_string(i) + "Map"] = observe(objects.bank, start + 1, sram, sram_size);
            for (const auto& field : std::map<std::string, unsigned>{{"Sprite", 0}, {"Flags1", 4}, {"Flags2", 5}, {"Facing", 13}})
                values["vNativeObject" + std::to_string(i) + field.first] = observe(objects.bank, start + field.second, sram, sram_size);
            unsigned facing = observe(saved.bank, saved.address + i, sram, sram_size);
            if (!bindings.count("wMobileNativeActorMode") && i && facing != 255) {
                suppressed |= 1u << i;
                if (facing == observe(objects.bank, start + 13, sram, sram_size)) restored |= 1u << i;
            }
        }
        values["vNPCPositionsHash"] = positions;
        values["vNPCOccupiedMask"] = occupied;
        values["vNPCSuppressedMask"] = suppressed;
        values["vNPCRestoredMask"] = restored;
    }
    if (bindings.count("wEventFlags") && bindings.count("wCurBox")) {
        const auto start = bindings.at("wEventFlags");
        unsigned hash = 2166136261u;
        for (unsigned address = start.address; address < bindings.at("wCurBox").address; ++address)
            hash = (hash ^ observe(start.bank, address, sram, sram_size)) * 16777619u;
        values["vStoryFlagsHash"] = hash;
    }
    unsigned scanlines[144] = {};
    if (native_oam_byte) {
        for (unsigned slot = 0; slot < 13; ++slot) {
            const std::string prefix = "vNativeSource" + std::to_string(slot);
            values[prefix + "Count"] = video_native_source_count[slot];
            values[prefix + "Complete"] = video_native_complete[slot];
            values[prefix + "Sprite"] = video_native_source_sprite[slot];
            values[prefix + "Map"] = video_native_source_map[slot];
        }
    }
    for (unsigned quad = 0; quad < 40; ++quad) {
        values["vOAM" + std::to_string(quad) + "Y"] = video_oam[quad * 4];
        for (int line = int(video_oam[quad * 4]) - 16; line < int(video_oam[quad * 4]) - 8; ++line)
            if (line >= 0 && line < 144) scanlines[line]++;
    }
    unsigned maximum = 0;
    for (unsigned count : scanlines) maximum = std::max(maximum, count);
    values["vOAMMaxScanline"] = maximum;
    if (bindings.count("wMobileNPCOptional") && values.count("wMobileLocalOAMStart")) {
        const auto optional = bindings.at("wMobileNPCOptional");
        unsigned clipped = 0;
        const unsigned start = values.at("wMobileLocalOAMStart");
        // Classification covers the peer prefix and native OAM in place.
        // $ff in the first (protected peer) entry marks the unclassified fast path.
        if (observe(optional.bank, optional.address, sram, sram_size) != 255)
            for (unsigned quad = 0; quad < 40 && start + quad * 4 < 160; ++quad)
                if (observe(optional.bank, optional.address + quad, sram, sram_size) == 1 &&
                    !video_oam[start + quad * 4]) clipped++;
        values["vNPCClippedQuads"] = clipped;
    }
    return values;
}

static void json_values(std::ostream& output, const std::map<std::string, unsigned>& values) {
    output << '{';
    bool first = true;
    for (const auto& entry : values) {
        output << (first ? "" : ",") << '\"' << entry.first << "\":" << entry.second;
        first = false;
    }
    output << '}';
}

static void log_message(enum retro_log_level, const char* format, ...) {
    va_list args;
    va_start(args, format);
    std::vfprintf(stderr, format, args);
    va_end(args);
}

static bool environment(unsigned command, void* data) {
    switch (command) {
    case RETRO_ENVIRONMENT_SET_MEMORY_MAPS: {
        const auto* maps = static_cast<const retro_memory_map*>(data);
        memory_maps.assign(maps->descriptors, maps->descriptors + maps->num_descriptors);
        return true;
    }
    case RETRO_ENVIRONMENT_GET_LOG_INTERFACE:
        static_cast<retro_log_callback*>(data)->log = log_message;
        return true;
    case RETRO_ENVIRONMENT_SET_CORE_OPTIONS_V2: {
        auto* definitions = static_cast<retro_core_options_v2*>(data)->definitions;
        for (auto* entry = definitions; entry && entry->key; ++entry)
            if (entry->default_value) options[entry->key] = entry->default_value;
        return true;
    }
    case RETRO_ENVIRONMENT_GET_VARIABLE: {
        auto* variable = static_cast<retro_variable*>(data);
        if (!std::strcmp(variable->key, "dcgb_emulated_gameboys"))
            variable->value = "1";
        else if (!std::strcmp(variable->key, "dcgb_mobile_adatper_enabled")) {
            variable->value = loaded && adapter_enabled ? "1" : "0";
            adapter_option_requested = adapter_option_requested || loaded;
        }
        else if (!std::strcmp(variable->key, "dcgb_mobile_adatper_use_relay_server"))
            variable->value = "0";
        else {
            auto found = options.find(variable->key);
            variable->value = found == options.end() ? nullptr : found->second.c_str();
        }
        return variable->value != nullptr;
    }
    case RETRO_ENVIRONMENT_GET_VARIABLE_UPDATE:
        *static_cast<bool*>(data) = updated;
        updated = false;
        return true;
    case RETRO_ENVIRONMENT_GET_SYSTEM_DIRECTORY:
    case RETRO_ENVIRONMENT_GET_SAVE_DIRECTORY:
        *static_cast<const char**>(data) = system_directory.c_str();
        return true;
    case RETRO_ENVIRONMENT_GET_CAN_DUPE:
        *static_cast<bool*>(data) = true;
        return true;
    case RETRO_ENVIRONMENT_SET_PIXEL_FORMAT:
        pixel_format = *static_cast<retro_pixel_format*>(data);
        return pixel_format == RETRO_PIXEL_FORMAT_RGB565 || pixel_format == RETRO_PIXEL_FORMAT_XRGB8888;
    case RETRO_ENVIRONMENT_SET_INPUT_DESCRIPTORS:
    case RETRO_ENVIRONMENT_SET_PERFORMANCE_LEVEL:
    case RETRO_ENVIRONMENT_SET_MESSAGE:
    case RETRO_ENVIRONMENT_SET_MESSAGE_EXT:
    case RETRO_ENVIRONMENT_SET_GEOMETRY:
    case RETRO_ENVIRONMENT_SET_SUPPORT_ACHIEVEMENTS:
        return true;
    default:
        return false;
    }
}

static void video(const void* data, unsigned w, unsigned h, size_t pitch) {
    ++video_frames;
    if (const auto* oam = descriptor(0xfe00)) std::memcpy(video_oam, oam, sizeof(video_oam));
    if (native_oam_byte) {
        unsigned row_demand[144] = {};
        bool selected[40];
        for (unsigned q = 0; q < 40; ++q) {
            selected[q] = true;
            for (int row = int(video_oam[q * 4]) - 16; row < int(video_oam[q * 4]) - 8; ++row) {
                if (row < 0 || row >= 144) continue;
                if (row_demand[row]++ >= 10) selected[q] = false;
            }
        }
        for (unsigned slot = 0; slot < 13; ++slot) {
            const unsigned flags = native_oam_byte(160 + slot * 8 + 6);
            const unsigned count = flags & 31;
            const unsigned start = native_oam_byte(160 + slot * 8 + 7);
            video_native_source_sprite[slot] = native_oam_byte(264 + slot * 2);
            video_native_source_map[slot] = native_oam_byte(265 + slot * 2);
            video_native_source_count[slot] = 0;
            video_native_complete[slot] = 0;
            const unsigned actual_count = native_figure_byte ? native_figure_byte(13 * 64 + slot) : count;
            if (!actual_count || actual_count > 16 ||
                (!native_figure_byte && (!(flags & 128) || start + count * 4 > 160))) continue;
            const auto source_byte = [&](unsigned index) {
                return native_figure_byte ? native_figure_byte(slot * 64 + index) : native_oam_byte(start + index);
            };
            bool visible = false;
            for (unsigned quad = 0; quad < actual_count; ++quad) {
                const unsigned y = source_byte(quad * 4);
                visible |= y >= 9 && y < 160;
            }
            if (!visible) continue;
            video_native_source_count[slot] = actual_count;
            // The native renderer writes facing quads backwards; an independent
            // renderer may write the same complete figure forwards. Compare all
            // four-byte hardware tuples, preserving multiplicity, not ordering.
            for (unsigned target = 0; target + actual_count * 4 <= 160; target += 4) {
                unsigned matched = 0;
                for (unsigned q = 0; q < actual_count; ++q) {
                    for (unsigned candidate = 0; candidate < actual_count; ++candidate) {
                        if (matched & (1u << candidate)) continue;
                        bool equal = true;
                        for (unsigned field = 0; field < 4; ++field)
                            if (video_oam[target + candidate * 4 + field] != source_byte(q * 4 + field)) {
                                equal = false; break;
                            }
                        if (equal) { matched |= 1u << candidate; break; }
                    }
                }
                bool fully_selected = true;
                for (unsigned q = 0; q < actual_count; ++q) fully_selected &= selected[target / 4 + q];
                if (fully_selected && matched == (1u << actual_count) - 1) {
                    video_native_complete[slot] = 1; break;
                }
            }
        }
    }
    if (bindings.count("wMobileVirtualActorMode") && descriptor(0xc000)) {
        const auto* ram = descriptor(0xc000);
        const auto owners = bindings.at("wMobileVirtualWindowOwners");
        for (unsigned index = 0; index < 3; ++index) {
            video_virtual_owners[index] = ram[owners.address - 0xc000 + index];
            const auto frame = bindings.at("wMobilePeer" + std::to_string(index + 1) + "LoadedFrame");
            video_virtual_frames[index] = ram[frame.bank * 0x1000 + frame.address - 0xd000];
            const auto movement = bindings.at("wMobilePeer" + std::to_string(index + 1) + "LoadedMovement");
            video_virtual_movements[index] = ram[movement.bank * 0x1000 + movement.address - 0xd000];
            const auto cache = bindings.at("wMobileAvatarCache" + std::to_string(index + 1));
            const unsigned pose = video_virtual_frames[index];
            if (pose <= 20)
                std::memcpy(video_virtual_cache_poses[index],
                    ram + cache.bank * 0x1000 + cache.address - 0xd000 + pose * 16, 64);
        }
    }
    if (const auto* colors = descriptor(0x10000))
        std::memcpy(video_object_palettes, colors, sizeof(video_object_palettes));
    if (const auto* vram = descriptor(0x8000)) {
        std::memcpy(video_peer_tiles, vram + 0x640, sizeof(video_peer_tiles));
        // The pinned core's cpu::vram contains both physical 8 KiB banks.
        std::memcpy(video_native_tiles, vram, sizeof(video_native_tiles));
    }
    if (!data) return;
    width = w;
    height = h;
    image.resize(size_t(w) * h * 3);
    for (unsigned y = 0; y < h; ++y) {
        auto* row = static_cast<const unsigned char*>(data) + y * pitch;
        for (unsigned x = 0; x < w; ++x) {
            unsigned r, g, b;
            if (pixel_format == RETRO_PIXEL_FORMAT_RGB565) {
                uint16_t color;
                std::memcpy(&color, row + x * 2, sizeof(color));
                r = ((color >> 11) & 31) * 255 / 31;
                g = ((color >> 5) & 63) * 255 / 63;
                b = (color & 31) * 255 / 31;
            } else {
                uint32_t color;
                std::memcpy(&color, row + x * 4, sizeof(color));
                r = (color >> 16) & 255;
                g = (color >> 8) & 255;
                b = color & 255;
            }
            size_t offset = (size_t(y) * w + x) * 3;
            image[offset] = r;
            image[offset + 1] = g;
            image[offset + 2] = b;
        }
    }
}

static size_t audio(const int16_t*, size_t frames) {
    audio_samples += frames;
    ++audio_batches;
    return frames;
}
static void write_image(const std::string& path) {
    std::ofstream output(path, std::ios::binary);
    output << "P6\n" << width << ' ' << height << "\n255\n";
    output.write(reinterpret_cast<const char*>(image.data()), image.size());
    if (!output) fail("Cannot write checkpoint image");
}
static void input_poll() {}
static int16_t input_state(unsigned port, unsigned device, unsigned, unsigned id) {
    if (port != 0 || device != RETRO_DEVICE_JOYPAD) return 0;
    if (id == walk_button) return 1;
    for (const auto& action : input_actions)
        if (id == action.button && input_frame >= action.first && input_frame < action.end) {
            if (input_events.is_open() && logged_inputs.insert({action.first, id}).second) {
                auto timestamp = std::chrono::duration_cast<std::chrono::microseconds>(
                    std::chrono::steady_clock::now().time_since_epoch()).count();
                input_events << "{\"frame\":" << input_frame << ",\"button\":" << id
                             << ",\"wall_us\":" << timestamp << "}\n";
            }
            return 1;
        }
    return 0;
}

template<class Function> static Function symbol(void* core, const char* name) {
    auto function = reinterpret_cast<Function>(dlsym(core, name));
    if (!function) { std::cerr << "Missing symbol: " << name << '\n'; std::exit(1); }
    return function;
}

int main(int argc, char** argv) {
    if (argc < 5 || (argc - 5) % 2) {
        std::cerr << "Usage: smoke-libretro CORE ROM OUTPUT.ppm SYSTEM_DIRECTORY [OPTION VALUE]...\n"
                     "Options: --frames N --input SCRIPT --symbols SYM --state-out JSON\n"
                     "         --save-in SAV --save-out SAV --adapter 0|1 --pace-from FRAME --trace-out JSONL\n"
                     "         --checkpoint-dir EXISTING_DIRECTORY --profile-out JSON --pace-multiplier 1|2|3|4\n"
                     "         --snapshot-out STATE --snapshot-in STATE (manual-play preparation only)\n"
                     "Input: FRAME BUTTON DURATION, ON MIN_FRAME SYMBOL=VALUE[,SYMBOL=VALUE] BUTTON DURATION, or WALK MIN_FRAME X Y TIMEOUT\n";
        return 1;
    }
    unsigned frame_limit = 1200;
    std::map<std::string, std::string> arguments;
    for (int i = 5; i < argc; i += 2) {
        std::string key = argv[i];
        if (key != "--frames" && key != "--input" && key != "--symbols" &&
            key != "--rtc-in" && key != "--rtc-out" && key != "--ram-out" && key != "--state-out" && key != "--save-in" && key != "--save-out" &&
            key != "--adapter" && key != "--pace-from" && key != "--trace-out" &&
            key != "--checkpoint-dir" && key != "--profile-out" && key != "--profile-window-start" &&
            key != "--snapshot-out" && key != "--test-snapshot-out" && key != "--snapshot-in" && key != "--input-events" &&
            key != "--finish-barrier" && key != "--pace-multiplier" && key != "--expected-peers" && key != "--expected-online") fail("Unknown option: " + key);
        arguments[key] = argv[i + 1];
    }
    if (arguments.count("--frames")) {
        try { frame_limit = std::stoul(arguments.at("--frames")); }
        catch (...) { fail("Invalid frame count"); }
        if (frame_limit < 1200 || frame_limit > 100000) fail("Frame count must be between 1200 and 100000");
    }
    if (arguments.count("--adapter")) {
        if (arguments.at("--adapter") != "0" && arguments.at("--adapter") != "1") fail("Invalid adapter flag");
        adapter_enabled = arguments.at("--adapter") == "1";
    }
    if (arguments.count("--symbols")) load_bindings(arguments.at("--symbols"));
    if (arguments.count("--input-events")) {
        input_events.open(arguments.at("--input-events"));
        if (!input_events) fail("Cannot write input timestamps");
    }
    if ((!conditional_actions.empty() || arguments.count("--trace-out")) && bindings.empty())
        fail("State observation requires matching symbols");
    if (arguments.count("--input")) {
        std::ifstream script(arguments.at("--input"));
        if (!script) fail("Cannot read input script");
        input_actions.clear();
        std::string line;
        while (std::getline(script, line)) {
            std::istringstream row(line.substr(0, line.find('#')));
            if (line.substr(0, 5) == "WALK ") {
                std::string tag;
                WalkWaypoint waypoint;
                if (!(row >> tag >> waypoint.earliest >> waypoint.x >> waypoint.y >> waypoint.timeout) ||
                    waypoint.earliest >= frame_limit || waypoint.x >= 128 || waypoint.y >= 128 ||
                    !waypoint.timeout || waypoint.timeout > 600 || !bindings.count("wXCoord") ||
                    !bindings.count("wYCoord") || !bindings.count("wPlayerStepDuration"))
                    fail("Invalid walk waypoint or missing symbols");
                walk_waypoints.push_back(waypoint);
                continue;
            }
            if (line.substr(0, 3) == "ON ") {
                std::string on, conditions, button;
                unsigned earliest, duration;
                if (!(row >> on >> earliest >> conditions >> button >> duration) ||
                    !buttons.count(button) || !duration || earliest >= frame_limit)
                    fail("Invalid conditional input action");
                ConditionalAction action = {earliest, buttons.at(button), duration, {}, false};
                std::istringstream list(conditions);
                std::string condition;
                while (std::getline(list, condition, ',')) {
                    auto equals = condition.find('=');
                    if (equals == std::string::npos || !bindings.count(condition.substr(0, equals)))
                        fail("Conditional input references unknown symbol");
                    action.conditions[condition.substr(0, equals)] = std::stoul(condition.substr(equals + 1));
                }
                if (action.conditions.empty()) fail("Conditional input requires a condition");
                conditional_actions.push_back(action);
                continue;
            }
            unsigned first, duration;
            std::string button;
            if (!(row >> first)) { if (!row.eof()) fail("Invalid input script line"); else continue; }
            if (!(row >> button >> duration) || !buttons.count(button) || !duration ||
                first >= frame_limit || duration > frame_limit - first) fail("Invalid input action");
            input_actions.push_back({first, first + duration, buttons.at(button)});
        }
    }
    system_directory = argv[4];
    void* core = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    if (!core) { std::cerr << dlerror() << '\n'; return 1; }
    using Metric = uint64_t(*)(unsigned);
    Metric metric = nullptr;
    Metric idle_metric = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_idle_metric"));
    Metric composition_metric = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_composition_metric"));
    auto native_graphics_wait = reinterpret_cast<uint64_t(*)()>(dlsym(core, "mobile_profile_native_graphics_wait"));
    auto native_wait_ly = reinterpret_cast<uint64_t(*)()>(dlsym(core, "mobile_profile_native_wait_ly"));
    uint64_t previous_profile_cycles[5] = {};
    void (*profile_frame)() = nullptr;
    if (arguments.count("--profile-out")) {
        metric = symbol<Metric>(core, "mobile_profile_metric");
        profile_frame = symbol<void(*)()>(core, "mobile_profile_frame");
        if (arguments.count("--profile-window-start")) {
            const auto first = std::stoul(arguments.at("--profile-window-start"));
            if (first > frame_limit - 800) fail("Profile window exceeds scenario");
            symbol<void(*)(unsigned)>(core, "mobile_profile_window")(first);
        }
        if (!arguments.count("--symbols")) fail("Profiling requires symbols");
        std::ifstream symbols(arguments.at("--symbols"));
        std::string location, name;
        bool configured = false;
        std::map<std::string, std::pair<unsigned, unsigned>> native_addresses;
        std::string line;
        while (std::getline(symbols, line)) {
            std::istringstream row(line);
            if (!(row >> location >> name)) continue;
            auto colon = location.find(':');
            if (colon == std::string::npos) continue; // RGBDS comment/header lines
            native_addresses[name] = {std::stoul(location.substr(0, colon), nullptr, 16),
                std::stoul(location.substr(colon + 1), nullptr, 16)};
            if (name == "hVBlankCounter") {
                symbol<void(*)(unsigned)>(core, "mobile_profile_vblank")(
                    std::stoul(location.substr(colon + 1), nullptr, 16));
            }
            if (name == "MobileRestoreFacingsEnd") {
                symbol<void(*)(unsigned, unsigned, unsigned, unsigned)>(core, "mobile_profile_restore")(
                    std::stoul(location.substr(0, colon), nullptr, 16),
                    std::stoul(location.substr(colon + 1), nullptr, 16),
                    bindings.at("wMobileSavedFacings").address, bindings.at("wObjectStructs").address);
            }
            if (name != "MobileTick") continue;
            symbol<void(*)(unsigned, unsigned)>(core, "mobile_profile_configure")(
                std::stoul(location.substr(0, colon), nullptr, 16),
                std::stoul(location.substr(colon + 1), nullptr, 16));
            configured = true;
        }
        if (!configured && bindings.count("wMobileOwner")) fail("MobileTick symbol missing");
        if (bindings.count("wMobileVirtualActorMode")) {
            const auto begin = native_addresses.at("wPlayerData").second;
            symbol<void(*)(unsigned, unsigned, unsigned, unsigned, unsigned, unsigned, unsigned)>(core, "mobile_profile_save")(
                0, 0x20, native_addresses.at("wPlayerDataEnd").second - begin,
                0, begin, 0, 0);
        } else if (native_addresses.count("MobileNativeSaveObjects")) {
            auto save = native_addresses.at("MobileNativeSaveObjects");
            const auto begin = native_addresses.at("wPlayerData").second;
            const auto objects = native_addresses.at("wObjectStructs").second;
            symbol<void(*)(unsigned, unsigned, unsigned, unsigned, unsigned, unsigned, unsigned)>(core, "mobile_profile_save")(
                save.first, save.second, native_addresses.at("wPlayerDataEnd").second - begin,
                bindings.count("wMobileVirtualActorMode") ? 0 : native_addresses.at("wMobileSavedFacings").second,
                native_addresses.at(native_addresses.count("wMobileNativeOriginals") ?
                    "wMobileNativeOriginals" : "wMobileActorBackup1").second, objects - begin, objects);
        }
        if (auto observer = reinterpret_cast<void(*)(unsigned, unsigned, unsigned)>(dlsym(core, "mobile_profile_overworld"))) {
            const char* points[] = {"MobilePrepareSprites", "MobileRenderSprites", "DelayFrame", "HandleNPCStep", "MobileBudgetNPCScanlines",
                "MobileCheckNPCScanlines", "MobileCanSuppress", "MobileBudgetWeather",
                "MobileCountObjectOAM", "MobilePreparePeerGraphics", "MobileProjectPeer", "MobileCapture", "_UpdateSprites", "MobileNativeUpload", "MobileNativeIdle", "MobileNativePrepareSpritesWork",
                "MobileVirtualComposeStep", "MobileVirtualNativeGroup", "MobileVirtualPeerGroup", "MobileVirtualMergePalettes", "MobileVirtualCommitPose", "GetUsedSprite",
                "MobilePrepareCommand", "MobileAppendPending", "MobileValidateResponseSlice"};
            for (unsigned i = 0; i < sizeof(points) / sizeof(points[0]); ++i) {
                if (native_addresses.count(points[i])) {
                    auto point = native_addresses.at(points[i]);
                    observer(i, point.first, point.second);
                }
            }
        }
        if (bindings.count("wMobileVirtualActorMode")) {
            if (auto scene = reinterpret_cast<void(*)(unsigned)>(dlsym(core, "mobile_profile_scene")))
                scene(bindings.at("wMobileVirtualStage").address);
            auto observer = symbol<void(*)(unsigned, unsigned, unsigned)>(core, "mobile_profile_native_oam");
            observer(native_addresses.at("wShadowOAM").second,
                     bindings.at("wMobileVirtualDescriptors").address,
                     native_addresses.at("wObjectStructs").second);
            native_oam_byte = symbol<unsigned(*)(unsigned)>(core, "mobile_profile_native_oam_byte");
            if (auto facing = reinterpret_cast<void(*)(unsigned)>(dlsym(core, "mobile_profile_native_facing"))) {
                facing(native_addresses.at("Facings").first);
                native_figure_byte = symbol<unsigned(*)(unsigned)>(core, "mobile_profile_native_figure_byte");
            }
        }
        const std::pair<const char*, const char*> cursor_ranges[] = {
            {"HBlankCopy1bpp", "ClearBGPalettes"},
            {"UpdateBGMapBuffer", "WaitTop"},
            {"UpdateBGMap.DoCustomSourceTiles", "LYOverrideStackCopy"},
            {"_Serve2bppRequest", "AnimateTileset"},
            {"AnimateFountainTile", "AnimateFountainTile.FountainTilePointers"},
            {"AnimateTowerPillarTiles1", "AnimateTowerPillarTiles1.TowerPillarTiles1Pointers"},
            {"AnimateTowerPillarTiles2", "AnimateTowerPillarTiles2.TowerPillarTiles2Pointers"},
            {"AnimateTowerPillarTiles3", "AnimateTowerPillarTiles3.TowerPillarTiles3Pointers"},
            {"AnimateTowerPillarTiles4", "AnimateTowerPillarTiles4.TowerPillarTiles4Pointers"},
            {"WriteFourTilesHLToDE", "FlickeringCaveEntrancePalette"},
        };
        for (unsigned i = 0; i < sizeof(cursor_ranges) / sizeof(cursor_ranges[0]); ++i) {
            auto start = native_addresses.at(cursor_ranges[i].first);
            auto end = native_addresses.at(cursor_ranges[i].second);
            if (start.first != end.first || start.second >= end.second || end.second > 0x8000)
                fail("Invalid audited stack-copy range");
            symbol<void(*)(unsigned, unsigned, unsigned, unsigned)>(core, "mobile_profile_stack_cursor")(
                i, start.first, start.second, end.second);
        }
    }
    symbol<void(*)(retro_environment_t)>(core, "retro_set_environment")(environment);
    symbol<void(*)(retro_video_refresh_t)>(core, "retro_set_video_refresh")(video);
    symbol<void(*)(retro_audio_sample_batch_t)>(core, "retro_set_audio_sample_batch")(audio);
    symbol<void(*)(retro_input_poll_t)>(core, "retro_set_input_poll")(input_poll);
    symbol<void(*)(retro_input_state_t)>(core, "retro_set_input_state")(input_state);
    symbol<void(*)()>(core, "retro_init")();
    std::ifstream rom_file(argv[2], std::ios::binary);
    if (!rom_file) { std::cerr << "Cannot read ROM\n"; return 1; }
    std::vector<char> rom((std::istreambuf_iterator<char>(rom_file)), {});
    retro_game_info game = {argv[2], rom.data(), rom.size(), nullptr};
    if (!symbol<bool(*)(const retro_game_info*)>(core, "retro_load_game")(&game)) return 1;
    retro_system_av_info av_info = {};
    symbol<void(*)(retro_system_av_info*)>(core, "retro_get_system_av_info")(&av_info);
    loaded = true;
    updated = true;
    auto* sram = static_cast<unsigned char*>(symbol<void*(*)(unsigned)>(core, "retro_get_memory_data")(RETRO_MEMORY_SAVE_RAM));
    size_t sram_size = symbol<size_t(*)(unsigned)>(core, "retro_get_memory_size")(RETRO_MEMORY_SAVE_RAM);
    if (arguments.count("--save-in")) {
        std::ifstream save(arguments.at("--save-in"), std::ios::binary);
        if (!save) fail("Cannot read battery save");
        std::vector<char> bytes((std::istreambuf_iterator<char>(save)), {});
        if (!sram || bytes.size() != sram_size) fail("Expected raw SRAM-sized save without an RTC trailer");
        std::memcpy(sram, bytes.data(), sram_size);
    }
    if (arguments.count("--rtc-in")) {
        // Persist only the external cartridge clock, as a frontend battery
        // sidecar. No CPU, game RAM, SRAM or instruction state is restored.
        uint32_t state[3] = {};
        std::ifstream file(arguments.at("--rtc-in"), std::ios::binary);
        file.read(reinterpret_cast<char*>(state), sizeof(state));
        if (!file || file.peek() != std::char_traits<char>::eof()) fail("Invalid cartridge RTC sidecar");
        if (!symbol<bool(*)(uint32_t*, bool)>(core, "mobile_frontend_rtc")(state, true))
            fail("RTC sidecar does not match the frontend clock mode");
    }
    if (arguments.count("--snapshot-in")) {
        std::ifstream file(arguments.at("--snapshot-in"), std::ios::binary);
        if (!file) fail("Cannot read manual-play snapshot");
        std::vector<char> bytes((std::istreambuf_iterator<char>(file)), {});
        auto size = symbol<size_t(*)()>(core, "retro_serialize_size")();
        if (!size || bytes.size() != size) fail("Snapshot size does not match this core");
        if (!symbol<bool(*)(const void*, size_t)>(core, "retro_unserialize")(bytes.data(), bytes.size()))
            fail("Core rejected manual-play snapshot");
    }
    auto run = symbol<void(*)()>(core, "retro_run");
    std::ofstream trace;
    gzFile compressed_trace = nullptr;
    if (arguments.count("--trace-out")) {
        const auto& path = arguments.at("--trace-out");
        if (path.size() >= 3 && path.substr(path.size() - 3) == ".gz") {
            compressed_trace = gzopen(path.c_str(), "wb3");
            if (!compressed_trace) fail("Cannot create compressed trace output");
        } else {
            trace.open(path);
            if (!trace) fail("Cannot create trace output");
        }
    }
    const bool trace_enabled = trace.is_open() || compressed_trace;
    unsigned pace_from = frame_limit;
    if (arguments.count("--pace-from")) pace_from = std::stoul(arguments.at("--pace-from"));
    unsigned pace_multiplier = 1;
    if (arguments.count("--pace-multiplier")) {
        const auto& value = arguments.at("--pace-multiplier");
        if (value != "1" && value != "2" && value != "3" && value != "4") fail("Pace multiplier must be 1..4");
        pace_multiplier = std::stoul(value);
    }
    if (av_info.timing.fps <= 0) fail("Invalid emulator frame rate");
    std::chrono::steady_clock::time_point paced_start;
    std::map<std::string, unsigned> previous;
    std::map<std::string, unsigned> checkpoint_previous;
    const bool barrier_enabled = arguments.count("--finish-barrier");
    bool announced_ready = false;
    std::string deferred_failure;
    auto settle_started = std::chrono::steady_clock::time_point{};
    for (input_frame = 0; input_frame < frame_limit ||
         (barrier_enabled && std::ifstream(arguments.at("--finish-barrier")).good()); ++input_frame) {
        if (input_frame >= frame_limit) {
            if (input_frame == frame_limit) settle_started = std::chrono::steady_clock::now();
            if (std::chrono::steady_clock::now() - settle_started > std::chrono::seconds(20)) {
                // Preserve traces and read-only metrics for a failed handshake.
                deferred_failure = "Lobby observation barrier did not settle";
                break;
            }
            auto live = capture(sram, sram_size);
            if (!announced_ready && live.at("wMobileOnline") == (arguments.count("--expected-online") ? std::stoul(arguments.at("--expected-online")) : 1) && live.at("wMobilePeerCount") == (arguments.count("--expected-peers") ? std::stoul(arguments.at("--expected-peers")) : 2)) {
                std::ofstream(arguments.at("--state-out") + ".ready") << "ready\n";
                announced_ready = true;
            }
            // Keep pumping until all clients are ready. Freezing an earlier
            // client here would expire its wall-clock server presence lease.
        }
        if (input_frame == pace_from) paced_start = std::chrono::steady_clock::now();
        if (input_frame >= pace_from) {
            auto offset = std::chrono::duration<double>((input_frame - pace_from) / (av_info.timing.fps * pace_multiplier));
            std::this_thread::sleep_until(paced_start + std::chrono::duration_cast<std::chrono::steady_clock::duration>(offset));
        }
        run();
        if (profile_frame) profile_frame();
        if (!trace_enabled && conditional_actions.empty() && walk_waypoints.empty()) continue;
        auto values = capture(sram, sram_size);
        if (profile_frame && native_graphics_wait)
            values["vNativeGraphicsWait"] = native_graphics_wait();
        if (profile_frame && native_wait_ly)
            values["vNativeWaitEntryLY"] = native_wait_ly();
        if (metric && idle_metric && composition_metric) {
            const uint64_t totals[] = {metric(7), idle_metric(3), idle_metric(7),
                                       composition_metric(2), composition_metric(14)};
            const char* names[] = {"Tick", "Upload", "Idle", "Compose", "Peer"};
            for (unsigned i = 0; i < 5; ++i) {
                values[std::string("vProfile") + names[i] + "Cycles"] = totals[i] - previous_profile_cycles[i];
                previous_profile_cycles[i] = totals[i];
            }
        }
        if (walk_index < walk_waypoints.size()) {
            auto& target = walk_waypoints[walk_index];
            if (input_frame >= target.earliest && input_frame >= walk_ready_frame) {
                if (!target.started) target.started = input_frame;
                if (input_frame - target.started >= target.timeout)
                    fail("Walk waypoint timed out at " + std::to_string(walk_index) +
                         ": player=" + std::to_string(values.at("wXCoord")) + "," +
                         std::to_string(values.at("wYCoord")) + "; target=" +
                         std::to_string(target.x) + "," + std::to_string(target.y));
                // Release on the native next-tile coordinate, before the completed
                // step updates global coordinates and can accept another held step.
                if (values.at("wPlayerMapX") == target.x + 4 && values.at("wPlayerMapY") == target.y + 4)
                    walk_button = 0xffff;
                if (values.at("wXCoord") == target.x && values.at("wYCoord") == target.y &&
                    values.at("wPlayerStepDuration") == 0) {
                    ++walk_index;
                    walk_button = 0xffff;
                    walk_ready_frame = input_frame + 3;
                } else if (values.at("wPlayerStepDuration") == 0 &&
                           (values.at("wPlayerMapX") != target.x + 4 || values.at("wPlayerMapY") != target.y + 4) &&
                           values.at("wScriptRunning") == 0 && values.at("hInMenu") == 0) {
                    int dx = int(target.x) - int(values.at("wXCoord"));
                    int dy = int(target.y) - int(values.at("wYCoord"));
                    if (std::abs(dx) + std::abs(dy) != 1)
                        fail("Walk waypoint is not adjacent: index=" + std::to_string(walk_index) + "; player=" + std::to_string(values.at("wXCoord")) + "," + std::to_string(values.at("wYCoord")) + "; target=" + std::to_string(target.x) + "," + std::to_string(target.y) + "; frame=" + std::to_string(input_frame));
                    walk_button = dx ? buttons.at(dx > 0 ? "RIGHT" : "LEFT") : buttons.at(dy > 0 ? "DOWN" : "UP");
                }
            }
        }
        values["vWalkReachedWaypoints"] = unsigned(walk_index);
        for (auto& action : conditional_actions) {
            if (action.fired || input_frame < action.earliest) continue;
            bool matches = true;
            for (const auto& condition : action.conditions)
                if (!values.count(condition.first) || values.at(condition.first) != condition.second) matches = false;
            if (matches) {
                if (action.duration >= frame_limit - input_frame) fail("Conditional action exceeds frame limit");
                action.fired = true;
                input_actions.push_back({input_frame + 1, input_frame + 1 + action.duration, action.button});
            }
        }
        std::map<std::string, unsigned> progress;
        for (const auto& entry : values)
            if (entry.first == "wMobileStep" || entry.first == "wMobileOwner" || entry.first == "wMobileError" ||
                entry.first == "wMobileSuccessCount" || entry.first == "wMobileRoundTrips" ||
                entry.first == "wXCoord" || entry.first == "wYCoord" || entry.first == "hInMenu" ||
                entry.first == "wMobileOnline" || entry.first == "wMobilePeerCount" ||
                entry.first == "wMobilePresenceCount" || entry.first == "wMobilePeer1X" ||
                entry.first == "wMobilePeer1Y" || entry.first == "wMobilePeer2X" || entry.first == "wMobilePeer2Y" ||
                entry.first == "wMobileIRQCount" || entry.first == "wMobileWire" ||
                entry.first == "wMobileRenderedCount" || entry.first == "wMobileDeferred" ||
                entry.first == "wMobilePeer1ScreenX" || entry.first == "wMobilePeer1ScreenY" ||
                entry.first == "wMobilePeer2ScreenX" || entry.first == "wMobilePeer2ScreenY")
                progress.insert(entry);
        for (const auto& entry : values)
            if (entry.first.rfind("vMobile", 0) == 0 || entry.first.rfind("vNPC", 0) == 0 || entry.first.rfind("vOAM", 0) == 0 || entry.first.rfind("vNativeSource", 0) == 0)
                progress.insert(entry);
        // A temporal visibility oracle needs every connected video frame.
        // Change-only traces omit repeated visible frames and can turn one
        // missing frame into a false multi-frame absence during a cold join.
        const bool temporal_frame = values.count("wMobileVirtualActorMode") &&
                                    values.at("wMobileOnline");
        if (trace_enabled && (progress != previous || temporal_frame || profile_frame)) {
            auto wall_us = std::chrono::duration_cast<std::chrono::microseconds>(
                std::chrono::steady_clock::now().time_since_epoch()).count();
            std::ostringstream record;
            record << "{\"frame\":" << input_frame << ",\"wall_us\":" << wall_us << ",\"symbols\":";
            json_values(record, values);
            record << "}\n";
            const auto line = record.str();
            if (compressed_trace) {
                if (gzwrite(compressed_trace, line.data(), line.size()) != int(line.size()))
                    fail("Compressed trace write failed");
            } else {
                trace << line;
                if (!trace) fail("Trace write failed");
            }
            previous = progress;
            if (input_frame >= pace_from && arguments.count("--checkpoint-dir")) {
                std::map<std::string, unsigned> checkpoint;
                for (const char* key : {"wMobileStep", "wMobileOwner", "wMobileError", "wMobileSuccessCount",
                                       "wMobileOnline", "wMobilePeerCount", "wXCoord", "wYCoord", "hInMenu"})
                    if (values.count(key)) checkpoint[key] = values.at(key);
                // IRQ and partial-packet counters change every frame. Screenshots
                // document semantic phases; the complete trace retains byte work.
                if (checkpoint != checkpoint_previous)
                    write_image(arguments.at("--checkpoint-dir") + "/frame-" + std::to_string(input_frame) + ".ppm");
                checkpoint_previous = checkpoint;
            }
        }
    }

    if (compressed_trace && gzclose(compressed_trace) != Z_OK) fail("Compressed trace close failed");
    trace.close();
    bool missing_input = false;
    for (const auto& action : conditional_actions) {
        if (action.fired) continue;
        std::cerr << "Conditional input not reached: earliest=" << action.earliest
                  << " button=" << action.button << '\n';
        missing_input = true;
    }

    if (walk_index != walk_waypoints.size() && deferred_failure.empty())
        deferred_failure = "Walk route did not complete";

    std::set<unsigned> colors;
    for (size_t i = 0; i + 2 < image.size(); i += 3)
        colors.insert((unsigned(image[i]) << 16) | (unsigned(image[i + 1]) << 8) | image[i + 2]);
    // LCD-off transitions can omit video callbacks while CPU execution continues.
    if (width != 160 || height != 144 || colors.size() < 2 || video_frames < frame_limit * 9 / 10 ||
        !adapter_option_requested) {
        std::cerr << "Expected at least " << frame_limit * 9 / 10
                  << " native-size video frames and a nonuniform final image; got "
                  << video_frames << " frames, " << width << 'x' << height << ", "
                  << colors.size() << " colors, adapter option requested: "
                  << adapter_option_requested << '\n';
        return 1;
    }
    write_image(argv[3]);
    if (metric) {
        const char* names[] = {"serial_count", "serial_max_cycles", "serial_total_cycles", "serial_failures",
            "serial_lowest_sp", "tick_count", "tick_max_cycles", "tick_total_cycles", "tick_failures",
            "tick_lowest_sp", "serial_max_frame_cycles", "serial_normal_speed_count", "serial_bank_switch_completions",
            "serial_max_vblank_cycles", "npc_restore_count", "npc_restore_failures",
            "tick_stack_cursor_samples", "tick_stack_cursor_lowest_sp",
            "prepare_max_cycles", "render_max_cycles", "prepare_delay_frames",
            "world_updates_in_800_frames", "world_max_frame_gap",
            "prepare_count", "prepare_total", "prepare_max",
            "render_count", "render_total", "render_max",
            "npc_budget_count", "npc_budget_total", "npc_budget_max",
            "scanlines_count", "scanlines_total", "scanlines_max",
            "eligibility_count", "eligibility_total", "eligibility_max",
            "weather_count", "weather_total", "weather_max",
            "oam_count_count", "oam_count_total", "oam_count_max",
            "graphics_count", "graphics_total", "graphics_max",
            "projection_count", "projection_total", "projection_max",
            "capture_count", "capture_total", "capture_max",
            "native_sprites_count", "native_sprites_total", "native_sprites_max",
            "active_cycles_in_800_frames", "world_frames_with_update", "world_duplicate_updates"};
        std::ofstream profile(arguments.at("--profile-out"));
        profile << '{';
        for (unsigned i = 0; i < sizeof(names) / sizeof(names[0]); ++i)
            profile << (i ? "," : "") << '\"' << names[i] << "\":" << metric(i);
        if (auto gaps = reinterpret_cast<uint64_t(*)(unsigned)>(dlsym(core, "mobile_profile_world_gap"))) {
            profile << ",\"world_gaps\":[";
            for (unsigned i = 0; auto gap = gaps(i); ++i) {
                profile << (i ? "," : "") << "{\"frame\":" << uint32_t(gap)
                        << ",\"gap\":" << (gap >> 32) << '}';
            }
            profile << ']';
        }
        if (auto idle = reinterpret_cast<uint64_t(*)(unsigned)>(dlsym(core, "mobile_profile_idle_metric"))) {
            const char* idle_names[] = {"native_upload_delay_frames", "native_upload_count", "native_upload_max_cycles",
                "native_upload_total_cycles", "native_upload_vblank_crossings", "native_idle_count",
                "native_idle_max_cycles", "native_idle_total_cycles", "native_idle_vblank_crossings",
                "native_upload_lowest_sp", "native_idle_lowest_sp", "native_position_max_cycles",
                "native_position_count", "native_position_vblank_crossings", "native_position_lowest_sp"};
            for (unsigned i = 0; i < sizeof(idle_names) / sizeof(idle_names[0]); ++i)
                profile << ",\"" << idle_names[i] << "\":" << idle(i);
        }
        if (auto saved = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_save_metric"))) {
            const char* save_names[] = {"save_copy_count", "save_copy_register_failures", "save_copy_byte_failures",
                "save_projected_objects", "save_copy_max_cycles", "save_copy_lowest_sp"};
            for (unsigned i = 0; i < 6; ++i) profile << ",\"" << save_names[i] << "\":" << saved(i);
        }
        if (auto composition = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_composition_metric"))) {
            const char* stages[] = {"compose", "native_group", "peer_group", "merge_palette"};
            const char* fields[] = {"count", "max_cycles", "total_cycles", "min_entry_ly", "max_entry_ly", "vblank_crossings"};
            for (unsigned stage = 0; stage < 4; ++stage)
                for (unsigned field = 0; field < 6; ++field)
                    profile << ",\"" << stages[stage] << '_' << fields[field] << "\":" << composition(stage * 6 + field);
        }
        if (auto pose = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_pose_metric"))) {
            const char* names[] = {"count", "max_cycles", "total_cycles", "min_entry_ly", "max_entry_ly", "vblank_crossings"};
            for (unsigned i = 0; i < 6; ++i)
                profile << ",\"pose_commit_" << names[i] << "\":" << pose(i);
        }
        if (auto transport = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_transport_metric"))) {
            const char* stages[] = {"command_prepare", "application_parse", "response_checksum"};
            const char* fields[] = {"count", "max_cycles", "total_cycles", "min_entry_ly", "max_entry_ly", "vblank_crossings"};
            for (unsigned stage = 0; stage < 3; ++stage)
                for (unsigned field = 0; field < 6; ++field)
                    profile << ",\"" << stages[stage] << '_' << fields[field] << "\":" << transport(stage * 6 + field);
        }
        if (auto scene = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_scene_metric"))) {
            const char* fields[] = {"count", "max_cycles", "total_cycles", "max_entry_ly", "max_exit_ly", "return_stage_mask"};
            for (unsigned stage = 0; stage < 9; ++stage)
                for (unsigned field = 0; field < 6; ++field)
                    profile << ",\"scene_stage" << stage << '_' << fields[field] << "\":" << scene(stage * 6 + field);
        }
        if (auto entry = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_composition_entry"))) {
            const char* stages[] = {"idle", "compose", "native_group", "peer_group", "merge_palette"};
            for (unsigned stage = 0; stage < 5; ++stage)
                profile << ",\"" << stages[stage] << "_window_entry_ly_p50\":" << entry(stage * 2)
                        << ",\"" << stages[stage] << "_window_entry_ly_p95\":" << entry(stage * 2 + 1);
        }
        profile << ",\"world_window_start\":" << (arguments.count("--profile-window-start") ? std::stoul(arguments.at("--profile-window-start")) : 3600);
        profile << ",\"video_frames\":" << video_frames << ",\"audio_batches\":" << audio_batches
                << ",\"audio_samples\":" << audio_samples << "}\n";
        if (!profile) fail("Cannot write profiling result");
    }
    if (arguments.count("--save-out")) {
        if (!sram || !sram_size) fail("Battery buffer unavailable");
        std::ofstream save(arguments.at("--save-out"), std::ios::binary);
        save.write(reinterpret_cast<const char*>(sram), sram_size);
        if (!save) fail("Cannot write battery save");
    }
    if (arguments.count("--rtc-out")) {
        uint32_t state[3] = {};
        if (!symbol<bool(*)(uint32_t*, bool)>(core, "mobile_frontend_rtc")(state, false))
            fail("Cannot persist cartridge RTC");
        std::ofstream file(arguments.at("--rtc-out"), std::ios::binary);
        file.write(reinterpret_cast<const char*>(state), sizeof(state));
        if (!file) fail("Cannot write cartridge RTC sidecar");
    }
    if (arguments.count("--snapshot-out") || arguments.count("--test-snapshot-out")) {
        // Human-play fixtures stay offline. The explicit test option captures
        // live CPU/RAM state to verify recovery when peripheral sockets are lost.
        auto values = capture(sram, sram_size);
        if (!arguments.count("--test-snapshot-out"))
          for (const auto& expected : std::map<std::string, unsigned>{
                {"hCGB", 1}, {"hCrashCode", 0}, {"hInMenu", 0},
                {"wMapStatus", 2}, {"wScriptRunning", 0}, {"wMobileOwner", 0}})
            if (!values.count(expected.first) || values.at(expected.first) != expected.second)
                fail("Manual-play snapshot requires idle overworld: " + expected.first);
        auto size = symbol<size_t(*)()>(core, "retro_serialize_size")();
        if (!size || size > 16 * 1024 * 1024) fail("Invalid manual-play snapshot size");
        std::vector<char> bytes(size, 0);
        if (!symbol<bool(*)(void*, size_t)>(core, "retro_serialize")(bytes.data(), bytes.size()))
            fail("Core could not serialize manual-play snapshot");
        std::ofstream file(arguments.at(arguments.count("--test-snapshot-out") ?
                          "--test-snapshot-out" : "--snapshot-out"), std::ios::binary);
        file.write(bytes.data(), bytes.size());
        if (!file) fail("Cannot write manual-play snapshot");
    }
    if (arguments.count("--ram-out")) {
        const auto* physical = descriptor(0xc000);
        if (!physical) fail("Physical WRAM observer unavailable");
        std::ofstream ram(arguments.at("--ram-out"), std::ios::binary);
        ram.write(reinterpret_cast<const char*>(physical), 8 * 0x1000);
        if (!ram) fail("Cannot preserve read-only WRAM evidence");
    }
    if (arguments.count("--state-out")) {
        std::ofstream hardware_oam(system_directory + "/hardware-oam.bin", std::ios::binary);
        hardware_oam.write(reinterpret_cast<const char*>(video_oam), sizeof(video_oam));
        if (!hardware_oam) fail("Cannot preserve hardware OAM evidence");
        if (!arguments.count("--symbols")) fail("--state-out requires matching --symbols");
        auto values = capture(sram, sram_size);
        if (!values.count("hCrashCode") || !values.count("hCGB")) fail("Required observer symbols missing");
        if (values.at("hCrashCode")) fail("ROM crash marker is nonzero");
        if (values.at("hCGB") != 1) fail("ROM did not detect Game Boy Color");
        std::ofstream state(arguments.at("--state-out"));
        state << "{\n  \"frame_calls\": " << input_frame << ",\n  \"symbols\": {";
        bool first = true;
        for (const auto& value : values) {
            state << (first ? "\n" : ",\n") << "    \"" << value.first << "\": " << value.second;
            first = false;
        }
        state << "\n  }\n}\n";
        if (!state) fail("Cannot write observer result");
    }

    if (barrier_enabled && deferred_failure.empty() && !missing_input) {
        auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(15);
        while (std::ifstream(arguments.at("--finish-barrier") + ".exit").good()) {
            if (std::chrono::steady_clock::now() >= deadline) fail("Final capture barrier timed out");
            std::this_thread::sleep_for(std::chrono::milliseconds(2));
        }
    }
    symbol<void(*)()>(core, "retro_unload_game")();
    symbol<void(*)()>(core, "retro_deinit")();
    if (missing_input || !deferred_failure.empty()) {
        if (!deferred_failure.empty()) std::cerr << deferred_failure << '\n';
        dlclose(core);
        return 1;
    }
    std::cout << "Emulator probe passed: " << frame_limit << " frame calls, " << video_frames << " video frames, 160x144, " << colors.size()
              << " colors; Mobile Adapter " << (adapter_enabled ? "enabled" : "disabled") << ". Check scenario assertions separately.\n";
    dlclose(core);
    return 0;
}
