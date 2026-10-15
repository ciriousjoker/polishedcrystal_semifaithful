// Native emulator probe with scripted input, battery saves, and symbol observers.
// Pinned DoubleCherryGB backend; Python runners own scenario assertions.
// Build with the pinned core's libretro include directory and run one process
// per instance. Acceptance tests boot battery saves. Explicit snapshot flags are
// for regenerating manual-play start states with the current ROM/core only.
#include <libretro.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <poll.h>
#include <cerrno>
#include <csignal>
#include <algorithm>
#include <zlib.h>
#include <dlfcn.h>
#include <cstdarg>
#include <cstdint>
#include <chrono>
#include <cmath>
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
static bool acceptance_enabled = false;
static unsigned video_frames = 0, width = 0, height = 0;
static uint64_t video_wall_us = 0;
static std::map<std::string, unsigned> video_figure_values;
static void capture_video_figures();
static uint64_t audio_samples = 0, audio_batches = 0;
static retro_pixel_format pixel_format = RETRO_PIXEL_FORMAT_0RGB1555;
static std::vector<unsigned char> image;
static unsigned char video_oam[160] = {};
static unsigned (*effect_serial_status)(unsigned) = nullptr;
static unsigned (*effect_video_byte)(unsigned) = nullptr;
static unsigned (*effect_video_field)(unsigned) = nullptr;
static unsigned (*effect_display_field)(unsigned) = nullptr;
static unsigned (*effect_display_matches)(const void*) = nullptr;
static unsigned effect_display_last = 0;
static unsigned (*ordinary_effect_video_byte)(unsigned) = nullptr;
static unsigned (*ordinary_effect_video_field)(unsigned) = nullptr;
static unsigned (*ordinary_effect_display_field)(unsigned) = nullptr;
static unsigned (*ordinary_effect_display_matches)(const void*) = nullptr;
static unsigned (*ordinary_effect_video_row)(unsigned,unsigned) = nullptr;
static unsigned (*ordinary_effect_video_change)(unsigned,unsigned) = nullptr;
static unsigned (*ordinary_status)(unsigned) = nullptr;
static unsigned (*ordinary_failure)(unsigned) = nullptr;
static unsigned (*ordinary_callback)(const void*) = nullptr;
static uint64_t (*native_reference_value)(unsigned,unsigned) = nullptr;
static uint64_t (*cold_value)(unsigned,unsigned,unsigned) = nullptr;
static void (*cold_callback)(const void*) = nullptr;
static std::ofstream cold_log;
static unsigned cold_written = 0;
static bool cold_missing_written = false;
static void cold_flush() {
    if (!cold_value) return;
    const char* fields[] = {"kind","phase","instruction","host_frame","ly","raster_id","display_id",
        "source","pending_entry","pending_return","pending_close","source_active","source_returned",
        "latched_opportunity","latched_instruction","latched_source","latched_closed","latched_input",
        "current_input","lifetime","receiver","generation","input_active","input_failure",
        "world_ready","scene_ready","map_group","map_number","map_status","script",
        "cpu_phase","source_failure","invalid"};
    auto row = [&](unsigned section,unsigned index,const char* event) {
        cold_log << "{\"event\":\"" << event << "\",\"index\":" << index;
        for(unsigned f=0;f<33;++f) {
            cold_log << ",\"" << fields[f] << "\":";
            const auto value=cold_value(section,index,f);
            if(value==~uint64_t(0)) cold_log << "null"; else cold_log << value;
        }
        cold_log << "}\n";
    };
    const auto count=cold_value(0,0,3);
    while(cold_written<count) { row(1,cold_written,"boundary"); ++cold_written; }
    if(cold_value(0,0,4) && !cold_missing_written) {
        row(2,0,"MissingColdIntent"); cold_missing_written=true;
    }
    cold_log.flush();
}
static unsigned ordinary_start = 0, ordinary_count = 0;
static std::ofstream reference_status_log;
static unsigned reference_status_count = 0;
static void reference_snapshot(const char* event) {
    if (!native_reference_value || !reference_status_log.is_open() || reference_status_count >= 8) return;
    ++reference_status_count;
    reference_status_log << "{\"event\":\"" << event << "\",\"runner_host\":" << input_frame
        << ",\"attempted_stream_ordinal\":" << ordinary_count << ",\"status\":[";
    for (unsigned i = 0; i < 64; ++i) reference_status_log << (i ? "," : "") << native_reference_value(2, i);
    reference_status_log << "],\"first_failure\":[";
    for (unsigned i = 0; i < 64; ++i) reference_status_log << (i ? "," : "") << native_reference_value(3, i);
    reference_status_log << "],\"configured_receiver_key\":[";
    for (unsigned i = 0; i < 18; ++i) reference_status_log << (i ? "," : "") << native_reference_value(4, i);
    reference_status_log << "],\"ordinary_request_commit\":[";
    if (ordinary_status) for (unsigned i = 0; i < 10; ++i) reference_status_log << (i ? "," : "") << ordinary_status(i);
    reference_status_log << "]}\n"; reference_status_log.flush();
}
static int ordinary_fd = -1;
static void ordinary_capture(const void*, unsigned, unsigned, size_t);

static unsigned (*effect_video_row)(unsigned,unsigned) = nullptr;
static unsigned (*effect_video_change)(unsigned,unsigned) = nullptr;
struct EffectVideoFrame {
    unsigned host_frame, video_frame, width, height, format, private_page, private_bank;
    unsigned metadata[14], display[24];
    std::vector<unsigned char> hardware, pixels;
    std::vector<unsigned> rows, changes;
};
static std::vector<EffectVideoFrame> effect_video_frames;
static unsigned effect_video_overflow = 0, effect_video_last = 0;
static unsigned effect_private_oam_page = ~0u, effect_private_oam_bank = ~0u;
static unsigned (*native_oam_byte)(unsigned) = nullptr;
static unsigned (*native_figure_byte)(unsigned) = nullptr;
static unsigned video_native_source_count[13] = {}, video_native_complete[13] = {};
static unsigned video_native_vanilla_complete[13] = {};
static unsigned video_native_start[13] = {};
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
#include "conditional_input.hpp"
static std::vector<InputAction> input_actions = {{700, 710, RETRO_DEVICE_ID_JOYPAD_START}};
static std::vector<ConditionalAction> conditional_actions;
static bool native_headbutt_control = false;
static HeadbuttControlUpRelease headbutt_control_up;
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
    if (native_reference_value) {
        reference_snapshot("failure");
        std::cerr << "native_reference_failure_v1 reason=" << native_reference_value(0, 1)
            << " latest=" << native_reference_value(2, 17) << " dma=" << native_reference_value(2, 21)
            << " raster=" << native_reference_value(2, 25) << " first_witness=";
        for (unsigned i = 0; i < 64; ++i) std::cerr << (i ? "," : "") << native_reference_value(3, i);
        std::cerr << '\n';
    }
    if (ordinary_failure) {
        std::cerr << "ordinary_first_failure_v1 runner_host=" << input_frame
                  << " stream_ordinal=" << ordinary_count << " snapshot=";
        for (unsigned i = 0; i < 27; ++i) std::cerr << (i ? "," : "") << ordinary_failure(i);
        std::cerr << " request_commit=";
        for (unsigned i = 5; i < 10; ++i) std::cerr << (i == 5 ? "" : ",") << ordinary_status(i);
        std::cerr << '\n';
    }
    std::cerr << message << '\n';
    std::exit(1);
}

// OUTF v1: LE u32 magic, version, ordinal, payload bytes, CRC32, then payload.
// Payload: host frame, display[24], metadata[14], change count, hardware bytes,
// rows[144][16], change pairs, tightly packed native RGB565 pixels.
// END1 footer carries requested count. Missing footer never means completion.
static void ordinary_write(const std::vector<unsigned char>& bytes,
    std::chrono::steady_clock::time_point deadline = std::chrono::steady_clock::now() + std::chrono::seconds(1)) {
    size_t offset = 0;
    while (offset < bytes.size()) {
        if (std::chrono::steady_clock::now() >= deadline) fail("Ordinary capture backpressure timeout/truncated record");
        const auto written = write(ordinary_fd, bytes.data() + offset, bytes.size() - offset);
        if (std::chrono::steady_clock::now() >= deadline) fail("Ordinary capture backpressure timeout/truncated record");
        if (written > 0) { offset += size_t(written); continue; }
        if (written < 0 && errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR)
            fail("Ordinary capture pipe disconnected");
        if (std::chrono::steady_clock::now() >= deadline) fail("Ordinary capture backpressure timeout/truncated record");
        pollfd fd = {ordinary_fd, POLLOUT, 0};
        if (poll(&fd, 1, 10) < 0 && errno != EINTR) fail("Ordinary capture pipe poll failed");
    }
    if (std::chrono::steady_clock::now() >= deadline) fail("Ordinary capture backpressure timeout/truncated record");
}
static void ordinary_u32(std::vector<unsigned char>& data, unsigned value) {
    for (unsigned i = 0; i < 4; ++i) data.push_back((value >> (8 * i)) & 255);
}
static void ordinary_capture(const void* data, unsigned w, unsigned h, size_t pitch) {
    if (!ordinary_status || ordinary_status(1) == 0 || ordinary_status(1) == 2) return;
    if (ordinary_status(1) != 1) fail("Ordinary capture unsupported/incomplete output coverage");
    if (!ordinary_callback(data)) {
        if (ordinary_status(1) != 1) fail("Ordinary callback ahead or wrong buffer");
        return;
    }
    unsigned d[24]; for (unsigned i = 0; i < 24; ++i) d[i] = ordinary_effect_display_field(i);
    float factor; std::memcpy(&factor, &d[11], sizeof(factor));
    if (!data || w != 160 || h != 144 || pitch != 320 || pixel_format != RETRO_PIXEL_FORMAT_RGB565 ||
        d[0] != 1 || d[23] != 24 || d[4] != 144 || d[5] != w || d[6] != h || d[7] != 16 || d[8] != pitch ||
        d[9] > 1 || d[10] > 1 || d[12] > 1 || d[13] != 1 || d[14] || d[15] != 1 || d[16] ||
        d[17] != 1 || d[18] != 11 || (d[19] != 1 && d[19] != 2) || d[20] || d[21] || d[22] ||
        !std::isfinite(factor) || factor < 1 || factor > 2 || (d[9] && !d[10]) || !ordinary_effect_display_matches(data) ||
        d[2] != ordinary_effect_video_field(1) || d[3] != ordinary_effect_video_field(2))
        fail("Unsupported ordinary render mode or display binding");
    const unsigned changes = ordinary_effect_video_change(0, 2);
    if (changes > 65536) fail("Ordinary row change overflow");
    std::vector<unsigned char> payload;
    payload.reserve(600000);
    ordinary_u32(payload, input_frame);
    for (unsigned value : d) ordinary_u32(payload, value);
    for (unsigned i = 0; i < 14; ++i) ordinary_u32(payload, ordinary_effect_video_field(i));
    ordinary_u32(payload, changes);
    for (unsigned i = 0; i < 160 + 0x4000 + 128 + 128 + 6 + 160; ++i) payload.push_back(ordinary_effect_video_byte(i));
    for (unsigned row = 0; row < 144; ++row) {
        if (ordinary_effect_video_row(row, 8) > 0x7fffffffu) fail("Unsupported ordinary window line");
        for (unsigned field = 0; field < 16; ++field)
            ordinary_u32(payload, ordinary_effect_video_row(row, field));
    }
    for (unsigned i = 0; i < changes; ++i) for (unsigned field = 0; field < 2; ++field)
        ordinary_u32(payload, ordinary_effect_video_change(i, field));
    const auto* pixels = static_cast<const unsigned char*>(data);
    payload.insert(payload.end(), pixels, pixels + 160 * 144 * 2);
    std::vector<unsigned char> header;
    for (unsigned value : {0x4654554fu, 1u, ++ordinary_count, unsigned(payload.size()), unsigned(crc32(0, payload.data(), payload.size()))})
        ordinary_u32(header, value);
    // Header and payload share one absolute record completion budget.
    const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(1);
    if (native_reference_value) {
        if (native_reference_value(0, 1) || !native_reference_value(0, 2)) fail("Disabled native reference failed or lacks actual DMA source");
        std::vector<unsigned char> reference;
        for (unsigned v : {ordinary_count, d[1], d[2]}) ordinary_u32(reference, v);
        for (unsigned f = 2; f < 6; ++f) {
            const auto v = native_reference_value(0, f);
            ordinary_u32(reference, unsigned(v)); ordinary_u32(reference, unsigned(v >> 32));
        }
        for (unsigned i = 0; i < 972; ++i) reference.push_back(native_reference_value(1, i));
        std::vector<unsigned char> rh;
        for (unsigned v : {0x3146524eu, 2u, ordinary_count, unsigned(reference.size()), unsigned(crc32(0, reference.data(), reference.size()))}) ordinary_u32(rh, v);
        ordinary_write(rh, deadline); ordinary_write(reference, deadline);
    }
    ordinary_write(header, deadline); ordinary_write(payload, deadline);
    if (ordinary_status(1) == 2) {
        header.clear(); ordinary_u32(header, 0x31444e45u); ordinary_u32(header, ordinary_count);
        // END has its own absolute completion budget, then closes immediately.
        ordinary_write(header);
        close(ordinary_fd); ordinary_fd = -1;
    }
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
        "hROMBank", "hJoypadDown", "hJoyPressed", "hCGB", "hCrashCode", "hVBlankCounter", "hDelayFrameLY", "hInMenu", "wJumptableIndex",
        "hNbTilesToCopy", "hRequested1bpp", "hRequested2bpp", "hHours", "hMinutes", "wTimeOfDay",
        "hScriptBank", "hScriptPos", "wMenuCursorX", "wMenuCursorY", "wInputFlags", "wGameLogicPaused", "wBattleMode", "wBattleMenuFlags", "wBattleMenuCursorBuffer",
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
    // Exported EQU aliases have no bank in RGBDS symbols. This exact private
    // bank-5 layout is audited against mobile_adapter/rom/ram.asm. NativeRGB
    // owns 64 bytes at DB4B..DB8A; the source-bound compact prefix metadata
    // owns DB40..DB43. Other retained aliases occupy D840..D9FF.
    const std::map<std::string, unsigned> stable_aliases = {
        {"wMobileCompactNativeLength", 0xdb40},
        {"wMobileCompactNativeMask", 0xdb41},
        {"wMobileCompactNativePalette", 0xdb43},
        {"wMobileCompactKeepPublication", 0xdb44},
        {"wMobileStableOriginalYs", 0xd840},
        {"wMobileStableOwners", 0xd868},
        {"wMobileStablePhases", 0xd890},
        {"wMobileStableBindingValid", 0xd8a0},
        {"wMobileStableEligible", 0xd8b0},
        {"wMobileStableContinuous", 0xd8b2},
        {"wMobileStablePhaseMasks", 0xd8b4},
        {"wMobileStableLastActual", 0xd8b8},
        {"wMobileStableLastEligible", 0xd8ba},
        {"wMobileStableDue", 0xd8bc},
        {"wMobileStableNativeComplete", 0xd8be},
        {"wMobileStablePendingNative", 0xd8c0},
        {"wMobileStablePendingPrivate", 0xd8c2},
        {"wMobileStableRetired", 0xd8c4},
        {"wMobileStableOutputOffsets", 0xd8c6},
        {"wMobileStablePhasePaletteBindings", 0xd8d6},
        {"wMobileStableSourceValid", 0xd8e6},
        {"wMobileStableActorViewValid", 0xd8e7},
        {"wMobileStableCapacityValid", 0xd8e8},
        {"wMobileStablePendingValid", 0xd8e9},
        {"wMobileStableFinalDecision", 0xd8ea},
        {"wMobileStableHardwarePaletteProvenance", 0xd8eb},
        {"wMobileStableOutputPhase", 0xd8ec},
        {"wMobileStableNativeCandidateValid", 0xd8ed},
        {"wMobileStableProducerActive", 0xd8ee},
        {"wMobileStableCurrentOwner", 0xd8ef},
        {"wMobileStableTransactionScratch", 0xd8f0},
        {"wMobileStableRemoteBindings", 0xd8fe},
        {"wMobileStableScratch", 0xd90a},
        {"wMobileStableRawXTileFlags", 0xd92a},
        {"wMobileStableNativeRGB", 0xdb4b},
        {"wMobileStableCameraX", 0xd8f0},
        {"wMobileStableCameraY", 0xd8f2},
        {"wMobileStableUnknownCount", 0xd8f4},
        {"wMobileStablePoseMask", 0xd8f5},
        {"wMobileStablePoseToken", 0xd8f6},
        {"wMobileStablePoseBatch", 0xd90a},
        {"wMobileStablePoseGeneration", 0xd922},
        {"wMobileStablePosePreparing", 0xd923},
        {"wMobileStablePoseCanceled", 0xd924},
    };
    std::ifstream symbols(path);
    if (!symbols) fail("Cannot read matching symbols");
    std::string line;
    while (std::getline(symbols, line)) {
        std::istringstream row(line);
        std::string location, name;
        if (!(row >> location >> name) || location[0] == ';') continue;
        if (!watched.count(name) && name.rfind("wMobile", 0) != 0 && name.rfind("wLoadedObjPal", 0) != 0 &&
            name.rfind("wAcceptance", 0) != 0) continue;
        auto colon = location.find(':');
        if (name.rfind("wMobileStable", 0) == 0 || name.rfind("wMobileCompact", 0) == 0) {
            const auto alias = stable_aliases.find(name);
            if (alias == stable_aliases.end()) fail("Unknown retained-source alias: " + name);
            // Validate the complete location, rather than accepting a numeric
            // prefix or allowing a typed row to bypass the private-bank guard.
            const auto hex = [](const std::string& value) {
                return !value.empty() && value.find_first_not_of("0123456789abcdefABCDEF") == std::string::npos;
            };
            const std::string bank_text = colon == std::string::npos ? "5" : location.substr(0, colon);
            const std::string address_text = colon == std::string::npos ? location : location.substr(colon + 1);
            if (!hex(bank_text) || !hex(address_text) || bank_text.size() > 2 || address_text.size() > 4)
                fail("Invalid retained-source symbol location");
            const unsigned bank = std::stoul(bank_text, nullptr, 16);
            const unsigned address = std::stoul(address_text, nullptr, 16);
            if (bank != 5 || address != alias->second)
                fail("Retained-source alias outside audited bank-5 layout");
            bindings[name] = {bank, address};
            continue;
        }
        if (colon == std::string::npos) {
            if (name.rfind("wMobileVirtual", 0) != 0 && name != "wMobileNativeShapeKeys" &&
                name != "wMobileNativeShapes") fail("Invalid symbol location");
            const unsigned address = std::stoul(location, nullptr, 16);
            const bool resident_pose = (name.rfind("wMobileVirtualQueued", 0) == 0 ||
                                        name == "wMobileVirtualBackgroundFrame") &&
                                       address >= 0xcf0b && address <= 0xcf14;
            const bool resident_pose_batch =
                (name == "wMobileVirtualPoseBatchMask" && address == 0xcf15) ||
                (name == "wMobileVirtualPoseBatchIndex" && address == 0xcf16);
            if (!resident_pose && !resident_pose_batch && (address < 0xcf44 || address >= 0xdd00))
                fail("Virtual alias outside audited private WRAM");
            bindings[name] = {address < 0xd000 ? 0u : 5u, address};
            continue;
        }
        bindings[name] = {static_cast<unsigned>(std::stoul(location.substr(0, colon), nullptr, 16)),
                          static_cast<unsigned>(std::stoul(location.substr(colon + 1), nullptr, 16))};
    }
}

static std::map<std::string, unsigned> capture(const unsigned char* sram, size_t sram_size) {
    const std::set<std::string> words = {"hScriptPos", "sChecksum", "wMobileRemaining", "wMobileTxPointer",
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
                        const unsigned ordinal = window == 2 && bank == 1 ?
                            (tile == 63 ? 1 : tile == 127 ? 2 : tile == 255 ? 3 : 4) :
                            bank == 0 && tile >= base && tile < base + (window == 2 ? 1 : 4) ? tile - base : 4;
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
                    const unsigned third_slots[] = {108 * 16, 0x2000 + 63 * 16,
                        0x2000 + 127 * 16, 0x2000 + 255 * 16};
                    const unsigned address = window == 2 ? third_slots[i / 16] + i % 16 : base * 16 + i;
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
            values[prefix + "VanillaComplete"] = video_native_vanilla_complete[slot];
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
    if (cold_callback) cold_callback(data);
    video_wall_us = std::chrono::duration_cast<std::chrono::microseconds>(
        std::chrono::steady_clock::now().time_since_epoch()).count();
    ++video_frames;
    if (const auto* oam = descriptor(0xfe00)) std::memcpy(video_oam, oam, sizeof(video_oam));
    if (native_oam_byte) {
        unsigned row_demand[144] = {};
        bool selected[40], native_selected[40];
        unsigned native_row_demand[144] = {};
        for (unsigned q = 0; q < 40; ++q) {
            selected[q] = native_selected[q] = true;
            for (int row = int(native_oam_byte(q * 4)) - 16; row < int(native_oam_byte(q * 4)) - 8; ++row) {
                if (row < 0 || row >= 144) continue;
                if (native_row_demand[row]++ >= 10) native_selected[q] = false;
            }
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
            video_native_vanilla_complete[slot] = 0;
            video_native_start[slot] = 160;
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
            if ((flags & 128) && start + actual_count * 4 <= 160) {
                bool complete = true;
                for (unsigned q = start / 4; q < start / 4 + actual_count; ++q)
                    complete &= native_selected[q];
                video_native_vanilla_complete[slot] = complete;
            }
            // The native renderer writes facing quads backwards; an independent
            // renderer may write the same complete figure forwards. Compare all
            // four-byte hardware tuples, preserving multiplicity, not ordering.
            for (unsigned target = 0; target + actual_count * 4 <= 160; target += 4) {
                unsigned matched = 0;
                for (unsigned q = 0; q < actual_count; ++q) {
                    for (unsigned candidate = 0; candidate < actual_count; ++candidate) {
                        if (matched & (1u << candidate)) continue;
                        bool equal = true;
                        for (unsigned field = 0; field < 4; ++field) {
                            const unsigned mask = acceptance_enabled && field == 3 ? 0xf8 : 0xff;
                            if ((video_oam[target + candidate * 4 + field] & mask) != (source_byte(q * 4 + field) & mask)) {
                                equal = false; break;
                            }
                        }
                        if (equal) { matched |= 1u << candidate; break; }
                    }
                }
                bool fully_selected = true;
                for (unsigned q = 0; q < actual_count; ++q) fully_selected &= selected[target / 4 + q];
                if (fully_selected && matched == (1u << actual_count) - 1) {
                    video_native_complete[slot] = 1; video_native_start[slot] = target; break;
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
    capture_video_figures();
    ordinary_capture(data, w, h, pitch);
    if (effect_video_field && effect_video_field(0) && effect_video_field(1) != effect_video_last) {
        effect_video_last = effect_video_field(1);
        if (effect_video_frames.size() >= 256) ++effect_video_overflow;
        else {
            EffectVideoFrame frame;
            frame.host_frame = input_frame; frame.video_frame = video_frames;
            frame.width = w; frame.height = h; frame.format = pixel_format;
            frame.private_page = effect_private_oam_page; frame.private_bank = effect_private_oam_bank;
            for (unsigned i = 0; i < 14; ++i) frame.metadata[i] = effect_video_field(i);
            for (unsigned i = 0; i < 24; ++i) frame.display[i] = effect_display_field(i);
            const auto* d = frame.display;
            float display_factor; std::memcpy(&display_factor, &d[11], sizeof(display_factor));
            if (d[0] != 1 || d[23] != 24 || d[1] <= effect_display_last ||
                d[2] != frame.metadata[1] || d[3] != frame.metadata[2] || d[4] != 144 ||
                d[5] != w || d[6] != h || d[7] != 16 || d[8] != pitch ||
                d[9] > 1 || d[10] > 1 || d[12] > 1 || d[13] != 1 || d[14] != 0 ||
                d[15] != 1 || d[16] != 0 || d[17] != 1 || d[18] != 11 ||
                (d[19] != 1 && d[19] != 2) || !std::isfinite(display_factor) || display_factor < 1.0f || display_factor > 2.0f || d[20] != 0 || d[21] || d[22] ||
                (d[9] && !d[10]) || !effect_display_matches(data))
                fail("Missing, stale or unsupported actual display metadata/callback binding");
            effect_display_last = d[1];
            frame.hardware.resize(160 + 0x4000 + 128 + 128 + 6 + 160);
            for (unsigned i = 0; i < frame.hardware.size(); ++i) frame.hardware[i] = effect_video_byte(i);
            for (unsigned row = 0; row < 144; ++row) for (unsigned f = 0; f < 16; ++f)
                frame.rows.push_back(effect_video_row(row, f));
            const unsigned changes = effect_video_change(0, 2);
            if (changes > 65536) fail("Native row change capacity exceeded");
            for (unsigned i = 0; i < changes; ++i) for (unsigned f = 0; f < 2; ++f)
                frame.changes.push_back(effect_video_change(i, f));
            if (data && w == 160 && h == 144 && pixel_format == RETRO_PIXEL_FORMAT_RGB565) {
                frame.pixels.resize(w * h * 2);
                for (unsigned y = 0; y < h; ++y)
                    std::memcpy(frame.pixels.data() + y * w * 2,
                        static_cast<const unsigned char*>(data) + y * pitch, w * 2);
            }
            effect_video_frames.push_back(std::move(frame));
        }
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
                             << ",\"duration\":" << action.end - input_frame
                             << ",\"wall_us\":" << timestamp << "}\n";
            }
            if (native_headbutt_control)
                headbutt_control_up.returned(action, input_frame, RETRO_DEVICE_ID_JOYPAD_UP);
            return 1;
        }
    return 0;
}

template<class Function> static Function symbol(void* core, const char* name) {
    auto function = reinterpret_cast<Function>(dlsym(core, name));
    if (!function) { std::cerr << "Missing symbol: " << name << '\n'; std::exit(1); }
    return function;
}

// Additional read-only observations belong to the acceptance suite.
#include "../tests/support/observer.hpp"

int main(int argc, char** argv) {
    if (argc < 5 || (argc - 5) % 2) {
        std::cerr << "Usage: smoke-libretro CORE ROM OUTPUT.ppm SYSTEM_DIRECTORY [OPTION VALUE]...\n"
                     "Options: --frames N --input SCRIPT --symbols SYM --state-out JSON\n"
                     "         --save-in SAV --save-out SAV --adapter 0|1 --pace-from FRAME --trace-out JSONL\n"
                     "         --checkpoint-dir EXISTING_DIRECTORY --profile-out JSON --pace-multiplier 1|2|3|4\n"
                     "         --ordinary-stream FIFO --ordinary-count 1..800 --ordinary-start FRAME | --ordinary-gate PATH (functional capture only)\n"
                     "         --functional-horizon 1000..4000 (gated acceptance completion only)\n"
                     "         --snapshot-out STATE --snapshot-in STATE (manual-play preparation only)\n"
                     "Input: FRAME BUTTON DURATION, ON MIN_FRAME SYMBOL=VALUE[,SYMBOL=VALUE] BUTTON DURATION, AFTER/CHECKED_AFTER DELAY CONDITIONS BUTTON DURATION, or WALK MIN_FRAME X Y TIMEOUT\n";
        return 1;
    }
    unsigned frame_limit = 1200;
    std::map<std::string, std::string> arguments;
    std::map<std::string, std::vector<std::string>> effect_options;
    std::map<unsigned, unsigned> effect_watch_lengths;
    std::vector<unsigned> effect_span_ids;
    for (int i = 5; i + 1 < argc; i += 2) {
        const std::string key = argv[i];
        if (key == "--effect-span" || key == "--effect-watch" || key == "--native-scene-field")
            effect_options[key].push_back(argv[i + 1]);
    }
    const bool effect_enabled = !effect_options.empty();
    if (effect_enabled) for (int i = 5; i < argc; i += 2)
        if (std::string(argv[i]) == "--snapshot-in" || std::string(argv[i]) == "--test-snapshot-roundtrip")
            fail("Effect/native-scene observation does not support snapshot restore");
    for (int i = 5; i < argc; i += 2) {
        std::string key = argv[i];
        if (key != "--native-initializer-probe" && key != "--cold-intent-boundaries" && key != "--accepted-ordinary-inputs" && key != "--native-vblank-timing" && key != "--disabled-native-reference" && key != "--static-intent-sources" && key != "--ordinary-gate" && key != "--ordinary-stream" && key != "--ordinary-count" && key != "--ordinary-start" && key != "--frames" && key != "--input" && key != "--symbols" &&
            key != "--rtc-in" && key != "--rtc-out" && key != "--ram-out" && key != "--state-out" && key != "--save-in" && key != "--save-out" &&
            key != "--adapter" && key != "--pace-from" && key != "--trace-out" &&
            key != "--checkpoint-dir" && key != "--profile-out" && key != "--profile-window-start" &&
            key != "--snapshot-out" && key != "--test-snapshot-out" && key != "--snapshot-in" && key != "--input-events" &&
            key != "--finish-barrier" && key != "--pace-multiplier" && key != "--expected-peers" && key != "--expected-online" &&
            key != "--native-headbutt-control" && key != "--effect-video" && key != "--native-object-count" && key != "--effect-span" && key != "--effect-watch" && key != "--native-scene-field" && key != "--acceptance-menu-routine" && key != "--measurement-gate" && key != "--measurement-warmup" && key != "--functional-horizon" && key != "--acceptance") fail("Unknown option: " + key);
        arguments[key] = argv[i + 1];
    }
    const bool initializer_requested = arguments.count("--native-initializer-probe");
    if (initializer_requested) {
        for (const char* required : {"--accepted-ordinary-inputs", "--static-intent-sources", "--symbols", "--ordinary-stream", "--ordinary-count"})
            if (!arguments.count(required)) fail(std::string("Initializer diagnostic requires ") + required);
        const auto& capture_text = arguments.at("--ordinary-count");
        const bool capture_decimal = !capture_text.empty() && capture_text.size() <= 3 &&
            std::all_of(capture_text.begin(), capture_text.end(), [](char c) { return c >= '0' && c <= '9'; });
        const auto capture_count = capture_decimal ? std::stoul(capture_text) : 0;
        if (capture_count < 64 || capture_count > 800 || arguments.count("--ordinary-start") || arguments.count("--ordinary-gate") || arguments.count("--cold-intent-boundaries"))
            fail("Initializer diagnostic requires64..800 startup outputs and its genuine native anchor");
    }
    const bool cold_requested = arguments.count("--cold-intent-boundaries");
    if (cold_requested) for (const char* required : {"--accepted-ordinary-inputs", "--static-intent-sources", "--symbols"})
        if (!arguments.count(required)) fail(std::string("Cold boundary diagnostic requires ") + required);
    const bool accepted_inputs_requested = arguments.count("--accepted-ordinary-inputs");
    if (accepted_inputs_requested) {
        for (const char* required : {"--static-intent-sources", "--symbols", "--profile-out", "--frames", "--acceptance", "--adapter", "--input"})
            if (!arguments.count(required)) fail(std::string("Accepted input diagnostic requires ") + required);
        if (arguments.at("--acceptance") != "1" || arguments.at("--adapter") != "1" ||
                effect_options["--native-scene-field"].size() != 8 || !effect_options["--effect-span"].empty() ||
                !effect_options["--effect-watch"].empty()) fail("Accepted inputs require stationary ordinary native scene declarations");
        for (const char* unsupported : {"--native-vblank-timing", "--disabled-native-reference", "--ordinary-stream", "--snapshot-in", "--snapshot-out", "--test-snapshot-out", "--test-snapshot-roundtrip", "--native-headbutt-control", "--effect-video", "--measurement-gate", "--expected-online", "--expected-peers"})
            if (arguments.count(unsupported) && !(initializer_requested && std::string(unsupported)=="--ordinary-stream")) fail(std::string("Accepted input diagnostic excludes ") + unsupported);
    }
    const bool timing_requested = arguments.count("--native-vblank-timing");
    if (timing_requested) {
        for (const char* required : {"--frames", "--symbols", "--state-out", "--profile-out", "--adapter", "--acceptance", "--profile-window-start"})
            if (!arguments.count(required)) fail(std::string("Native timing requires ") + required);
        if (arguments.at("--profile-window-start") != "0") fail("Native timing requires an explicit world observation window starting at zero");
        if (arguments.at("--acceptance") != "1" || effect_options["--native-scene-field"].size() != 8 ||
            !effect_options["--effect-span"].empty() || !effect_options["--effect-watch"].empty())
            fail("Native timing requires eight declared ordinary native scene fields without effect actions");
        for (const char* unsupported : {"--snapshot-in", "--snapshot-out", "--test-snapshot-out", "--test-snapshot-roundtrip",
                "--ordinary-stream", "--static-intent-sources", "--native-headbutt-control", "--effect-video", "--finish-barrier"})
            if (arguments.count(unsupported)) fail(std::string("Native timing excludes ") + unsupported);
        if (arguments.at("--adapter") == "1" && (!arguments.count("--measurement-gate") ||
            arguments.count("--expected-online") || arguments.count("--expected-peers")))
            fail("Enabled timing requires the genuine four-client ready gate");
        if (arguments.at("--adapter") == "0" && arguments.count("--measurement-gate"))
            fail("Disabled timing arms from its declared completed native world boundary without a lobby gate");
    }
    const bool disabled_reference = arguments.count("--disabled-native-reference");
    if (disabled_reference && (arguments.at("--disabled-native-reference") != "1" ||
            !arguments.count("--static-intent-sources") || !arguments.count("--ordinary-stream") ||
            arguments.count("--ordinary-gate") || arguments.count("--ordinary-start") ||
            !arguments.count("--adapter") || arguments.at("--adapter") != "0" ||
            !arguments.count("--acceptance") || arguments.at("--acceptance") != "1"))
        fail("Disabled reference requires its own acceptance/source/physical stream mode and adapter0");
    if (arguments.count("--static-intent-sources") && (!arguments.count("--profile-out") ||
            !arguments.count("--symbols") || arguments.count("--snapshot-in")))
        fail("Static intent source extraction requires acceptance/profile/symbols and no restore");
    if ((arguments.count("--ordinary-gate") || arguments.count("--ordinary-count") || arguments.count("--ordinary-start")) && !arguments.count("--ordinary-stream"))
        fail("Ordinary capture bounds require a stream");
    if (arguments.count("--frames")) {
        try { frame_limit = std::stoul(arguments.at("--frames")); }
        catch (...) { fail("Invalid frame count"); }
        if (frame_limit < 1200 || frame_limit > 100000) fail("Frame count must be between 1200 and 100000");
    }
    if (arguments.count("--adapter")) {
        if (arguments.at("--adapter") != "0" && arguments.at("--adapter") != "1") fail("Invalid adapter flag");
        adapter_enabled = arguments.at("--adapter") == "1";
    }
    if (arguments.count("--effect-video") && (!effect_enabled || arguments.at("--effect-video") != "1"))
        fail("Effect video requires configured native spans and value 1");
    if (effect_enabled && (!arguments.count("--profile-out") || !arguments.count("--symbols")))
        fail("Effect observation requires profile output and symbols");
    if (arguments.count("--symbols")) load_bindings(arguments.at("--symbols"));
    acceptance_enabled = arguments.count("--acceptance") && arguments.at("--acceptance") == "1";
    unsigned functional_horizon = 1000;
    if (arguments.count("--functional-horizon")) {
        if (!acceptance_enabled || !arguments.count("--measurement-gate") || !arguments.count("--profile-out"))
            fail("Functional horizon requires gated acceptance profiling");
        try { functional_horizon = parse_functional_horizon(arguments.at("--functional-horizon")); }
        catch (const std::exception& error) { fail(error.what()); }
    }
    const bool menu_observer_requested = arguments.count("--acceptance-menu-routine");
    const bool mobile_menu_instruction = menu_observer_requested && arguments.at("--acceptance-menu-routine") == "MobileMenuInput";
    if (menu_observer_requested && arguments.at("--acceptance-menu-routine") != "MobileMenuInput" &&
            arguments.at("--acceptance-menu-routine") != "DoMenuJoypadLoop")
        fail("Unsupported native menu observation routine");
    if (mobile_menu_instruction && (arguments.at("--acceptance-menu-routine") != "MobileMenuInput" ||
            !acceptance_enabled || !adapter_enabled || !arguments.count("--profile-out")))
        fail("Mobile menu instruction observation requires enabled acceptance profiling");
    if (arguments.count("--native-headbutt-control")) {
        const auto& spans = effect_options["--effect-span"];
        const bool headbutt_spans =
            std::find(spans.begin(), spans.end(), "0:ShakeHeadbuttTree") != spans.end() &&
            std::find(spans.begin(), spans.end(), "6:TryHeadbuttOW") != spans.end();
        try {
            validate_headbutt_control_option(arguments.at("--native-headbutt-control"),
                acceptance_enabled, adapter_enabled, arguments.count("--profile-out"),
                !effect_options["--native-scene-field"].empty(), headbutt_spans,
                menu_observer_requested && arguments.at("--acceptance-menu-routine") == "DoMenuJoypadLoop");
        } catch (const std::exception& error) { fail(error.what()); }
        native_headbutt_control = true;
    }
    unsigned acceptance_menu_source_bank = 0, acceptance_menu_source_pc = 0;
    if (arguments.count("--symbols")) acceptance_load_symbols(arguments.at("--symbols"), argv[2]);
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
            // Optional actions are bounded branches of native recovery. The
            // effect completion and physical movement remain required actions.
            const bool optional = line.rfind("OPTIONAL ", 0) == 0;
            if (optional) line = line.substr(9);
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
            if (line.substr(0, 3) == "ON " || line.substr(0, 6) == "AFTER " ||
                    line.substr(0, 14) == "CHECKED_AFTER ") {
                try {
                    conditional_actions.push_back(parse_conditional_action(
                        row, optional, frame_limit, buttons, [](const std::string& name) {
                            return bindings.count(name) ||
                                (acceptance_enabled && name.rfind("vAcceptance", 0) == 0);
                        }));
                } catch (const std::exception& error) { fail(error.what()); }
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
    // Diagnostic extraction only. This option cannot award intent acceptance.
    Metric input_status = nullptr, input_failure_context = nullptr;
    uint64_t (*input_event)(unsigned,unsigned,unsigned) = nullptr;
    unsigned (*input_drain)(unsigned) = nullptr;
    unsigned input_events_written = 0;
    std::ofstream input_log, initializer_log;
    uint64_t (*initializer_value)(unsigned,unsigned,unsigned) = nullptr;
    unsigned initializer_written = 0;
    auto initializer_array = [&](unsigned section,unsigned index,unsigned count) {
        initializer_log << '[';
        for(unsigned f=0;f<count;++f)initializer_log << (f ? "," : "") << initializer_value(section,index,f);
        initializer_log << ']';
    };
    auto initializer_flush = [&](bool final) {
        if(!initializer_value)return;
        while(initializer_written < initializer_value(0,0,5)) {
            const unsigned i=initializer_written++;
            initializer_log << "{\"row\":" << i << ",\"binding\":";initializer_array(2,i,22);
            initializer_log << ",\"sample\":";initializer_array(3,i,567);
            if(initializer_value(2,i,0)==2) {
                initializer_log << ",\"source\":";initializer_array(4,i,1576);
                initializer_log << ",\"definition\":";initializer_array(5,i,113);
                initializer_log << ",\"outer_scene\":";initializer_array(6,i,13);
            }
            initializer_log << "}\n";
        }
        if(final) {
            initializer_log << "{\"status\":";initializer_array(0,0,27);
            if(initializer_value(0,0,3)) {initializer_log << ",\"initializer\":";initializer_array(1,0,567);}
            initializer_log << "}\n";
        }
        initializer_log.flush();
        if(!initializer_log)fail("Initializer evidence write failed");
    };
    bool input_ready_written = false;
    Metric static_status = nullptr;
    uint64_t (*static_source)(uint64_t,unsigned,unsigned) = nullptr;
    uint64_t (*static_operands)(uint64_t,unsigned,unsigned) = nullptr;
    std::string static_operand_provenance;
    uint64_t (*static_map_preimage)(uint64_t,unsigned,unsigned) = nullptr;
    std::string static_map_provenance;
    void (*static_release)(uint64_t) = nullptr;
    unsigned (*static_enable)() = nullptr;
    std::ofstream static_log, static_status_log;
    uint64_t static_written = 0;
    bool static_started = false;
    uint64_t previous_profile_cycles[5] = {};
    void (*profile_frame)() = nullptr;
    using TimingValue = uint64_t(*)(unsigned);
    using TimingEvent = uint64_t(*)(unsigned,unsigned);
    TimingValue timing_status = nullptr, scene_diagnostic = nullptr;
    TimingEvent timing_event = nullptr, scene_field = nullptr, scene_snapshot = nullptr;
    std::string timing_prearm_load, timing_prearm_arm;
    std::vector<std::pair<std::string, std::pair<unsigned,unsigned>>> timing_bindings;
    unsigned timing_arm_frame = 0, timing_done_frame = 0;
    bool timing_armed = false;
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
        if (timing_requested) {
            timing_status = symbol<TimingValue>(core, "acceptance_profile_timing_status");
            timing_event = symbol<TimingEvent>(core, "acceptance_profile_timing_event");
            scene_diagnostic = symbol<TimingValue>(core, "acceptance_profile_scene_diagnostic");
            scene_field = symbol<TimingEvent>(core, "acceptance_profile_scene_field");
            scene_snapshot = symbol<TimingEvent>(core, "acceptance_profile_scene_snapshot");
            auto binding = [&](const std::string& name) {
                if (!native_addresses.count(name)) fail("Missing timing symbol: " + name);
                const auto p = native_addresses.at(name); timing_bindings.push_back({name,p}); return p;
            };
            std::ifstream source_rom(argv[2], std::ios::binary);
            std::vector<unsigned char> bytes((std::istreambuf_iterator<char>(source_rom)), {});
            auto offset = [&](std::pair<unsigned,unsigned> p) -> size_t {
                const size_t n = p.first * 0x4000 + (p.second & 0x3fff);
                if (n >= bytes.size()) fail("Timing symbol outside ROM"); return n;
            };
            const auto vblank = binding("VBlank0"), game_time = binding("VBlank.doGameTime");
            if (vblank.first || game_time.first) fail("Native VBlank callers must be ROM0");
            const auto hram = binding("hPushOAM"), dma_source = binding("OAMDMACode");
            const unsigned char expected[] = {0xe2,0x05,0x20,0xfd,0xc9};
            const size_t source = offset(dma_source);
            if (source + 5 > bytes.size() || !std::equal(expected, expected + 5, bytes.begin() + source))
                fail("Original HRAM DMA code changed");
            auto point = symbol<unsigned(*)(unsigned,unsigned,unsigned)>(core, "acceptance_profile_timing_point");
            const char* jobs[] = {"VBlank0", "UpdateBGMapBuffer", "UpdateCGBPals", "DMATransfer", "AnimateTileset",
                "PlaceFootprints", "UpdateBGMap", "Serve2bppRequest", "Serve1bppRequest", "PushOAM"};
            for (const auto* name : jobs) { const auto p = binding(name);
                if (p.first || !point(p.first,p.second,1)) fail("Invalid ROM0 timing job"); }
            for (const char* name : {"VBlankUpdateSound", "_UpdateSound", "VBlank.doGameTime", "_Serve2bppRequest", "_Serve1bppRequest", "UpdateBGMap.DoTiles", "UpdateBGMap.DoAttributes", "UpdateBGMap.DoBGMap1Tiles", "UpdateBGMap.DoBGMap1Attributes", "UpdateBGMap.DoCustomSourceTiles"}) {
                const auto p = binding(name); if (!point(p.first,p.second,0)) fail("Invalid timing work/sound point");
            }
            // HRAM is a mark, never a second job sharing PushOAM's return frame.
            // The wrapper authenticates the copied HRAM entry and RET separately.
            if (!point(0,hram.second,0)) fail("Invalid HRAM timing point");
            const auto hl_tail = binding("_hl_");
            const size_t return_offset = offset(game_time);
            if (return_offset < 3 || bytes[return_offset-3] != 0xcd ||
                unsigned(bytes[return_offset-2] | bytes[return_offset-1] << 8) != hl_tail.second)
                fail("Changed native VBlank0 tailcall caller");
            const char* pending[] = {"hBGMapUpdate", "hBGMapTileCount", "hCGBPalUpdate", "hDMATransfer",
                "hBGMapMode", "hRequested2bpp", "hRequested1bpp", "hOAMUpdate"};
            for (unsigned i = 0; i < 8; ++i) {
                const auto p = binding(pending[i]);
                if (!symbol<unsigned(*)(unsigned,unsigned)>(core,"acceptance_profile_timing_field")(i,p.second))
                    fail("Invalid timing pending field");
            }
            unsigned dma_return = 0, dma_bank = 0, chooser = 0, far_return = 0;
            auto unique_call_return = [&](std::pair<unsigned,unsigned> begin, unsigned length, unsigned target) {
                unsigned found = 0, result = 0; const size_t first = offset(begin);
                if (first + length > bytes.size()) fail("Timing caller range outside ROM");
                for (unsigned i = 0; i + 2 < length; ++i)
                    if (bytes[first+i] == 0xcd && unsigned(bytes[first+i+1] | bytes[first+i+2] << 8) == target) {
                        ++found; result = begin.second + i + 3;
                    }
                if (found != 1) fail("Missing or ambiguous timing CALL provenance"); return result;
            };
            const auto native_end = binding("VBlank2");
            if (native_end.first || native_end.second <= vblank.second) fail("Invalid native VBlank0 caller range");
            // Each configured native job is reached by the original ROM0 CALL.
            // UpdateBGMap is the sole conditional CALL in this scope.
            for (unsigned i = 1; i < sizeof(jobs) / sizeof(jobs[0]); ++i) {
                const auto target = native_addresses.at(jobs[i]);
                const size_t first = offset(vblank); unsigned matches = 0;
                for (unsigned n = 0; n + 2 < native_end.second - vblank.second; ++n) {
                    const unsigned call = i == 6 ? 0xdc : 0xcd;
                    if (bytes[first+n] == call && unsigned(bytes[first+n+1] | bytes[first+n+2] << 8) == target.second) ++matches;
                }
                if (matches != 1) fail(std::string("Changed native VBlank0 CALL provenance: ") + jobs[i]);
            }
            if (adapter_enabled) {
                const auto caller = binding("MobileVirtualPushOAM"); dma_bank = caller.first;
                dma_return = unique_call_return(caller, 256, hram.second);
                chooser = binding("MobileVirtualChooseOutput").second;
                const auto chooser_point = native_addresses.at("MobileVirtualChooseOutput");
                if (!point(chooser_point.first,chooser,1)) fail("Invalid chooser timing point");
                far_return = binding("_ReturnFarCall").second;
                const auto far_call = binding("_DoFarCall.call_hl");
                const auto hl = binding("_hl_");
                if (far_call.first || unique_call_return(far_call,3,hl.second) != far_return)
                    fail("Changed original ROM0 farcall return trampoline");
            } else {
                const auto end = binding("VBlank2");
                if (end.first || end.second <= vblank.second) fail("Invalid VBlank0 caller range");
                dma_return = unique_call_return(vblank,end.second-vblank.second,native_addresses.at("PushOAM").second);
            }
            const auto mode = binding("hVBlank"), anims = binding("hMapAnims"), footprint = binding("wFootprintQueue");
            if (footprint.second < 0xc000 || footprint.second >= 0xd000) fail("Unsupported footprint queue bank");
            const auto config = symbol<unsigned(*)(unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned)>(core,"acceptance_profile_timing_config");
            if (!config(mode.second,anims.second,footprint.second,hram.second,dma_bank,dma_return,game_time.second,chooser,far_return))
                fail("Native timing configuration failed");
        }
        if (effect_enabled) {
            // Each repeated option is ID:SYMBOL[:LENGTH] or INDEX:SYMBOL:EXPECTED:MASK.
            // EXPECTED=nonzero is a readiness predicate; zero is valid for the player map index.
            auto parts = [](const std::string& value) {
                std::vector<std::string> result; std::istringstream stream(value); std::string part;
                while (std::getline(stream, part, ':')) result.push_back(part);
                return result;
            };
            std::set<unsigned> spans, watches, scene_fields;
            for (const auto& value : effect_options["--effect-span"]) {
                const auto fields = parts(value);
                if (fields.size() != 2) fail("Effect span requires ID:SYMBOL");
                const auto id = std::stoul(fields[0], nullptr, 0);
                if (id >= 9 || (id == 8 && fields[1] != "ClearSpriteAnims") || !spans.insert(id).second || !native_addresses.count(fields[1]))
                    fail("Invalid or missing effect span: " + value);
                const auto point = native_addresses.at(fields[1]);
                symbol<void(*)(unsigned,unsigned,unsigned)>(core, "acceptance_profile_effect_span")(id, point.first, point.second);
                if (id == 5 && fields[1] == "DoNextFrameForAllSprites_OW") {
                    if (!native_addresses.count("DoNextFrameForAllSprites")) fail("Missing initial native animation routine");
                    const auto alias = native_addresses.at("DoNextFrameForAllSprites");
                    symbol<void(*)(unsigned,unsigned,unsigned)>(core, "acceptance_profile_effect_alias")(id, alias.first, alias.second);
                }
                if (id == 8) {
                    if (!native_addresses.count("ShakeHeadbuttTree.hide_loop")) fail("Missing native teardown caller");
                    const auto caller = native_addresses.at("ShakeHeadbuttTree.hide_loop");
                    symbol<void(*)(unsigned,unsigned)>(core, "acceptance_profile_effect_teardown")(caller.first, caller.second + 8);
                }
                effect_span_ids.push_back(id);
            }
            for (const auto& value : effect_options["--effect-watch"]) {
                const auto fields = parts(value);
                if (fields.size() != 3) fail("Effect watch requires ID:SYMBOL:LENGTH");
                const auto id = std::stoul(fields[0], nullptr, 0), length = std::stoul(fields[2], nullptr, 0);
                if (id >= 16 || !length || length > 160 || !watches.insert(id).second || !native_addresses.count(fields[1]))
                    fail("Invalid or missing effect watch: " + value);
                const auto point = native_addresses.at(fields[1]);
                symbol<void(*)(unsigned,unsigned,unsigned,unsigned)>(core, "acceptance_profile_effect_watch")(id, point.first, point.second, length);
                effect_watch_lengths[id] = length;
            }
            for (const auto& value : effect_options["--native-scene-field"]) {
                const auto fields = parts(value);
                if (fields.size() != 4) fail("Native scene field requires INDEX:SYMBOL:EXPECTED:MASK");
                const auto id = std::stoul(fields[0], nullptr, 0);
                if (id >= 8 || !scene_fields.insert(id).second || !native_addresses.count(fields[1]))
                    fail("Invalid or missing native scene field: " + value);
                const auto point = native_addresses.at(fields[1]);
                const auto expected = fields[2] == "nonzero" ? ~0u : std::stoul(fields[2], nullptr, 0);
                symbol<void(*)(unsigned,unsigned,unsigned,unsigned,unsigned)>(core, "acceptance_profile_native_scene_field")(
                    id, point.first, point.second, expected, std::stoul(fields[3], nullptr, 0));
            }
            if (!scene_fields.empty()) {
                if (scene_fields.size() != 8) fail("Native scene readiness requires all eight predicates");
                symbol<void(*)(unsigned)>(core, "acceptance_profile_native_scene_enable")(8);
            }
            // Explicit ABI checks prevent a legacy core from silently yielding empty evidence.
            symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_effect_active");
            symbol<unsigned(*)(unsigned,unsigned)>(core, "acceptance_profile_effect_record");
            symbol<unsigned(*)(unsigned,unsigned,unsigned)>(core, "acceptance_profile_effect_sample");
            symbol<unsigned(*)()>(core, "acceptance_profile_native_scene_ready");
            effect_serial_status = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_serial_status");
            if (arguments.count("--effect-video")) {
                const auto private_oam = native_addresses.find("wMobileVirtualOutput");
                if (private_oam != native_addresses.end()) {
                    effect_private_oam_page = private_oam->second.second >> 8; effect_private_oam_bank = private_oam->second.first;
                }
                effect_video_byte = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_video_byte");
                effect_video_field = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_video_field");
                effect_display_field = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_display_field");
                effect_display_matches = symbol<unsigned(*)(const void*)>(core, "acceptance_profile_display_matches");
                effect_video_row = symbol<unsigned(*)(unsigned,unsigned)>(core, "acceptance_profile_video_row");
                effect_video_change = symbol<unsigned(*)(unsigned,unsigned)>(core, "acceptance_profile_video_change");
                symbol<void(*)(unsigned)>(core, "acceptance_profile_video_enable")(1);
            }
        }
        if (arguments.count("--static-intent-sources")) {
            if (!acceptance_enabled) fail("Static source extraction requires acceptance mode");
            auto point = symbol<void(*)(unsigned,unsigned,unsigned)>(core, "acceptance_profile_static_point");
            const char* anchors[] = {"InitSprites", native_addresses.count("MobileNativeBatch.InitSprite") ?
                "MobileNativeBatch.InitSprite" : "InitSprites.InitSprite",
                "_UpdateSprites.hide_unused_sprites", "NextOverworldFrame"};
            for (unsigned i = 0; i < 4; ++i) {
                if (!native_addresses.count(anchors[i])) fail(std::string("Missing static source anchor: ") + anchors[i]);
                auto p = native_addresses.at(anchors[i]); point(i, p.first, p.second);
            }
            auto objects = native_addresses.at("wObjectStructs");
            auto x = native_addresses.at("wPlayerBGMapOffsetX");
            auto y = native_addresses.at("wPlayerBGMapOffsetY");
            if (x.first != objects.first || y.first != objects.first || acceptance_word("AcceptanceObjectStride") != 34)
                fail("Static source original-input layout mismatch");
            symbol<void(*)(unsigned,unsigned,unsigned,unsigned)>(core, "acceptance_profile_static_inputs")(
                objects.first, objects.second, x.second, y.second);
            const char* world_fields[] = {"wMapGroup", "wMapNumber", "wMapStatus", "wScriptRunning"};
            auto world_field = symbol<void(*)(unsigned,unsigned,unsigned)>(core, "acceptance_profile_static_world_field");
            for (unsigned i = 0; i < 4; ++i) {
                const auto p = native_addresses.at(world_fields[i]); world_field(i, p.first, p.second);
            }
            static_status = symbol<Metric>(core, "acceptance_profile_static_status");
            static_source = symbol<uint64_t(*)(uint64_t,unsigned,unsigned)>(core, "acceptance_profile_static_source");
            static_release = symbol<void(*)(uint64_t)>(core, "acceptance_profile_static_release");
            static_enable = symbol<unsigned(*)()>(core, "acceptance_profile_static_enable");
            if (static_status(0) != 1) fail("Static source ABI mismatch");
            if (accepted_inputs_requested) {
                auto operand = symbol<unsigned(*)(unsigned,unsigned,unsigned)>(core, "acceptance_profile_static_operand");
                static_operands = symbol<uint64_t(*)(uint64_t,unsigned,unsigned)>(core, "acceptance_profile_static_operands");
                const char* operands[] = {"wXCoord", "wYCoord", "wPlayerStepFlags", "wPlayerStepVectorX", "wPlayerStepVectorY", "wPlayerState"};
                std::ostringstream provenance;
                provenance << "[";
                for (unsigned i = 0; i < 6; ++i) {
                    if (!native_addresses.count(operands[i])) fail(std::string("Missing original source operand: ") + operands[i]);
                    const auto p = native_addresses.at(operands[i]);
                    if (!operand(i, p.first, p.second)) fail("Invalid original source operand binding");
                    provenance << (i ? "," : "") << "[\"" << operands[i] << "\"," << p.first << ',' << p.second << ']';
                }
                provenance << "]"; static_operand_provenance = provenance.str();
                auto map_field = symbol<unsigned(*)(unsigned,unsigned,unsigned)>(core, "acceptance_profile_static_map_field");
                static_map_preimage = symbol<uint64_t(*)(uint64_t,unsigned,unsigned)>(core, "acceptance_profile_static_map_preimage");
                const char* map_fields[] = {"wMapWidth", "wMapHeight", "wMapTileset", "wOverworldMapBlocks"};
                std::ostringstream map_provenance;
                map_provenance << "[";
                for (unsigned i = 0; i < 4; ++i) {
                    if (!native_addresses.count(map_fields[i])) fail(std::string("Missing original map field: ") + map_fields[i]);
                    const auto p = native_addresses.at(map_fields[i]);
                    if (!map_field(i, p.first, p.second)) fail("Invalid original map field binding");
                    map_provenance << (i ? "," : "") << "[\"" << map_fields[i] << "\"," << p.first << ',' << p.second << ']';
                }
                map_provenance << "]"; static_map_provenance = map_provenance.str();
                auto point = symbol<unsigned(*)(unsigned,unsigned,unsigned,unsigned)>(core,"acceptance_profile_input_point");
                auto field = symbol<unsigned(*)(unsigned,unsigned,unsigned)>(core,"acceptance_profile_input_field");
                std::ifstream source_rom(argv[2],std::ios::binary);
                std::vector<unsigned char> bytes((std::istreambuf_iterator<char>(source_rom)),{});
                input_log.open(arguments.at("--accepted-ordinary-inputs"));
                if (!input_log) fail("Cannot open accepted input diagnostic");
                input_log << "{\"abi\":2,\"scope\":\"stationary accepted wire inputs only\",\"points\":[";
                const char* names[] = {"MobileConsumeFrame.welcome","MobileConsumeFrame.accepted","MobileExpandRoster",
                    "MobileConsumeFrame.roster_done","MobileConsumeFrame.bad","MobileAppendPending.bad","MobileAppendPending.copy",
                    "MobileBegin","MobileReleaseNoIME","MobileConsumeFrame.left","MobileMapEntered","MobileTick.frame_pending"};
                for (unsigned i=0;i<12;++i) {
                    if (!native_addresses.count(names[i])) fail(std::string("Missing accepted input anchor: ")+names[i]);
                    auto p = native_addresses.at(names[i]);
                    if (i == 11) p.second += 6; // original emergency clear, after conditional branch
                    const size_t offset = p.first*0x4000+(p.second&0x3fff);
                    if (offset+2 > bytes.size()) fail("Accepted input anchor outside ROM");
                    const unsigned code = bytes[offset] | bytes[offset+1]<<8;
                    if (i == 11 && bytes[offset] != 0xaf) fail("Emergency clear anchor changed");
                    if (!point(i,p.first,p.second,code)) fail("Invalid accepted input point");
                    input_log << (i ? "," : "") << "[\"" << names[i] << "\"," << p.first << ',' << p.second << ',' << code << ']';
                }
                input_log << "],\"fields\":[";
                const char* fields[] = {"wMobileApp","wMobileAppCount","wMobileDecodePhase","wMobilePeer1","wMobileOnline",
                    "wPlayerBGMapOffsetX","wPlayerBGMapOffsetY","wXCoord","wYCoord","wPlayerFacing","wPlayerGender",
                    "wPlayerState","wPlayerStepDuration","wPlayerDirection","wPlayerSpriteYOffset","wTimeOfDay","wTimeOfDayPal","wPlayerWalking"};
                for (unsigned i=0;i<18;++i) {
                    if (!native_addresses.count(fields[i])) fail("Missing accepted input field");
                    const auto p=native_addresses.at(fields[i]);
                    if (!field(i,p.first,p.second)) fail("Invalid accepted input field");
                    input_log << (i ? "," : "") << "[\"" << fields[i] << "\"," << p.first << ',' << p.second << ']';
                }
                input_log << "]}\n"; input_log.flush();
                if (!input_log || !symbol<unsigned(*)()>(core,"acceptance_profile_input_config")()) fail("Accepted input configuration failed");
                input_status=symbol<Metric>(core,"acceptance_profile_input_status");
                input_failure_context=symbol<Metric>(core,"acceptance_profile_input_failure_context");
                input_event=symbol<uint64_t(*)(unsigned,unsigned,unsigned)>(core,"acceptance_profile_input_event");
                input_drain=symbol<unsigned(*)(unsigned)>(core,"acceptance_profile_input_drain");
                if (input_status(0)!=2) fail("Accepted input ABI mismatch");
                if (initializer_requested) {
                    if (acceptance_data("AcceptanceScenarioID") != 0 || acceptance_data("AcceptanceLighting") != 255 ||
                        acceptance_data("AcceptanceHour") != 12 || acceptance_data("AcceptanceNPCOverride") != 0 ||
                        acceptance_data("AcceptanceScenarioLoad") != 1 || acceptance_data("AcceptanceAvatar") > 3 ||
                        acceptance_data("AcceptanceSpawn",0) != 11 || acceptance_data("AcceptanceSpawn",1) != 23)
                        fail("Initializer diagnostic requires finite EMPTY hour12 scenario-load1 fixture");
                    auto field = symbol<unsigned(*)(unsigned,unsigned,unsigned)>(core,"acceptance_profile_initializer_field");
                    const char* fields[] = {"wShadowOAM", "hUsedOAMIndex", "wLoadedObjPal0", "wLoadedObjPalType",
                        "wLoadedObjPalGlows", "wLoadedObjPalPrevGlows", "wObjectGlowTypes", "wObjectPrevGlowTypes",
                        "wObjectGlowFadeActive", "wTimeOfDay", "wTimeOfDayPal", "wPalState", "hHours", "wPalFadeDelayFrames",
                        "wPalFlags", "wCurPalStates", "wCurWeather", "wMapTimeOfDay", "wTimeOfDayPalset",
                        "wMapGroup", "wMapNumber", "wEnvironment", "hAcceptanceLighting", "wNeededObjPalGlow"};
                    initializer_log.open(arguments.at("--native-initializer-probe"));
                    if(!initializer_log)fail("Cannot open initializer diagnostic");
                    initializer_log << "{\"abi\":1,\"scope\":\"finite startup diagnostic, no acceptance claim\",\"role\":" << acceptance_data("AcceptanceAvatar")
                        << ",\"experiment_lifetime\":1,\"lighting_fields\":[";
                    for(unsigned i=0;i<24;++i) {
                        const auto p=native_addresses.at(fields[i]);
                        if(!field(i,p.first,p.second))fail("Initializer field configuration failed");
                        initializer_log << (i?",":"") << "[\"" << fields[i] << "\"," << p.first << ',' << p.second << ']';
                    }
                    if(!symbol<unsigned(*)(unsigned)>(core,"acceptance_profile_initializer_limit")(std::stoul(arguments.at("--ordinary-count"))))
                        fail("Initializer capture capacity rejected");
                    const auto anchor=native_addresses.at("RefreshMapSprites");
                    if(!symbol<unsigned(*)(unsigned,unsigned,unsigned,unsigned)>(core,"acceptance_profile_initializer_config")(anchor.first,anchor.second,11,23))
                        fail("Initializer configuration failed");
                    initializer_value=symbol<uint64_t(*)(unsigned,unsigned,unsigned)>(core,"acceptance_profile_initializer_value");
                    initializer_log << "],\"anchor\":[" << anchor.first << ',' << anchor.second << "]}\n";initializer_log.flush();
                }
                if (cold_requested) {
                    if (acceptance_data("AcceptanceScenarioID") != 0 || acceptance_data("AcceptanceLighting") != 255 ||
                        acceptance_data("AcceptanceHour") != 12 || acceptance_data("AcceptanceNPCOverride") != 0 ||
                        acceptance_data("AcceptanceAvatar") > 3 || acceptance_data("AcceptanceSpawn",0) != 11 ||
                        acceptance_data("AcceptanceSpawn",1) != 23)
                        fail("Cold boundary diagnostic requires the finite EMPTY hour12 fixture");
                    cold_value=symbol<uint64_t(*)(unsigned,unsigned,unsigned)>(core,"acceptance_profile_cold_value");
                    cold_callback=symbol<void(*)(const void*)>(core,"acceptance_profile_cold_callback");
                    if (!symbol<unsigned(*)()>(core,"acceptance_profile_cold_config")() || cold_value(0,0,0)!=1 || cold_value(0,0,6)!=33)
                        fail("Cold boundary diagnostic configuration/ABI mismatch");
                    cold_log.open(arguments.at("--cold-intent-boundaries"));
                    if (!cold_log) fail("Cannot open cold boundary diagnostic");
                    cold_log << "{\"abi\":1,\"scope\":\"cold LCD boundary diagnostic only\",\"capacity\":16,\"declared_experiment_lifetime\":1,"
                        "\"kind_names\":[\"absent\",\"activation\",\"source_entry\",\"source_return\",\"source_close\",\"outer\",\"row0\",\"raster\",\"display\",\"callback\"],"
                        "\"phase_names\":[\"frontend\",\"beforeCPU\",\"afterCPU\",\"renderbegin\",\"render_line\",\"display\",\"callback\"]}\n";
                    cold_log.flush();
                }
            }
            if (disabled_reference) {
                if (native_addresses.count("MobileNativeBatch") || acceptance_data("AcceptanceScenarioID") != 0 ||
                        acceptance_data("AcceptanceLighting") != 255 || acceptance_data("AcceptanceHour") != 12 ||
                        acceptance_data("AcceptanceNPCOverride") != 0 || acceptance_data("AcceptanceAvatar") > 3 ||
                        acceptance_data("AcceptanceSpawn", 0) != 11 || acceptance_data("AcceptanceSpawn", 1) != 23)
                    fail("Disabled reference requires original EMPTY four-avatar hour12 fixture");
                const unsigned role = acceptance_data("AcceptanceAvatar");
                auto actor_config = symbol<void(*)(unsigned,unsigned,unsigned,unsigned,unsigned)>(core, "acceptance_profile_reference_actor");
                unsigned control = 0;
                for (unsigned a = 0; a < 4; ++a) {
                    const unsigned avatar = a == 0 ? role : (control >= role ? control + 1 : control);
                    const unsigned sprite = acceptance_data("PlayerStateSprites", avatar * 6);
                    if (sprite != acceptance_data("AcceptanceFigures", avatar)) fail("Avatar fixture differs from original PlayerStateSprites");
                    const unsigned palette = acceptance_data("SpriteHeaders", (sprite - 1) * 4 + 3);
                    if (palette & 0xc0) fail("Reference avatar is not native walking sprite");
                    if (a && acceptance_data("AcceptanceControlPeers", (a - 1) * 3) != sprite)
                        fail("Declared control actor is missing or wrong");
                    actor_config(a, sprite, palette & 63,
                        a ? acceptance_data("AcceptanceControlPeers", (a - 1) * 3 + 2) : acceptance_data("AcceptanceSpawn", 2) + 4,
                        a ? acceptance_data("AcceptanceControlPeers", (a - 1) * 3 + 1) : acceptance_data("AcceptanceSpawn", 3) + 4);
                    if (a) ++control;
                }
                const char* fields[] = {"wShadowOAM", "hUsedOAMIndex", "wLoadedObjPal0", "wLoadedObjPalType",
                    "wLoadedObjPalGlows", "wLoadedObjPalPrevGlows", "wObjectGlowTypes", "wObjectPrevGlowTypes",
                    "wObjectGlowFadeActive", "wTimeOfDay", "wTimeOfDayPal", "wPalState", "hHours", "wPalFadeDelayFrames",
                    "wPalFlags", "wCurPalStates", "wCurWeather", "wMapTimeOfDay", "wTimeOfDayPalset",
                    "wMapGroup", "wMapNumber", "wEnvironment", "hAcceptanceLighting", "wNeededObjPalGlow"};
                auto configure = symbol<void(*)(unsigned,unsigned,unsigned)>(core, "acceptance_profile_reference_field");
                for (unsigned i = 0; i < sizeof(fields) / sizeof(fields[0]); ++i) { const auto p = native_addresses.at(fields[i]); configure(i, p.first, p.second); }
                native_reference_value = symbol<uint64_t(*)(unsigned,unsigned)>(core, "acceptance_profile_reference");
                reference_status_log.open(arguments.at("--static-intent-sources") + ".reference-status.jsonl");
                if (!reference_status_log) fail("Cannot open native reference diagnostic evidence");
                if (!symbol<unsigned(*)()>(core, "acceptance_profile_reference_enable")()) fail("Disabled reference configuration failed");
                reference_snapshot("configured");
            }
            static_log.open(arguments.at("--static-intent-sources"));
            if (!static_log) fail("Cannot open static source evidence");
            static_status_log.open(arguments.at("--static-intent-sources") + ".status.jsonl");
            if (!static_status_log) fail("Cannot open static status evidence");
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
                "MobilePrepareCommand", "MobileAppendPending", "MobileValidateResponseSlice",
                "MobileVirtualAlignedRawRectangle", "MobileVirtualAlignedNativeDescriptor",
                "MobileVirtualAdvanceNativeClass", "MobileVirtualCurrentColors", "MobilePlanSignature",
                "MobilePlanGatherPeer", "MobilePlanLookup", "MobilePlanSeedResources",
                "MobilePlanSelectNatives", "MobilePlanEmit", "MobilePlanEmitNativeView"};
            for (unsigned i = 0; i < sizeof(points) / sizeof(points[0]); ++i) {
                if (native_addresses.count(points[i])) {
                    auto point = native_addresses.at(points[i]);
                    observer(i, point.first, point.second);
                }
            }
        }
        if (acceptance_enabled) {
            if (arguments.count("--native-object-count")) {
                if (effect_options["--native-scene-field"].size() != 8) fail("Original object count requires native scene predicates");
                acceptance_original_objects = std::stoul(arguments.at("--native-object-count"));
                if (acceptance_original_objects > 12) fail("Original object count exceeds native pool");
            }
            const char* markers[] = {"AcceptanceReadyMark", "AcceptanceDialogMark", "AcceptanceContinue.loaded", "AcceptanceContinue.failed", "LinkCommunications"};
            for (unsigned i = 0; i < 5; ++i) {
                auto mark = native_addresses.at(markers[i]);
                symbol<void(*)(unsigned,unsigned,unsigned)>(core,"acceptance_profile_marker")(i,mark.first,mark.second);
            }
            const std::string native_emitter = native_addresses.count("MobileNativeBatch.InitSprite") ?
                "MobileNativeBatch" : "InitSprites";
            auto point = native_addresses.at(native_emitter + ".InitSprite");
            auto facing = native_addresses.at(native_emitter + ".AcceptanceFacing");
            symbol<void(*)(unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned)>(core, "acceptance_profile_native")(
                point.first, point.second, facing.second, native_addresses.at("hCurSpriteXPixel").second,
                native_addresses.at("hCurSpriteYPixel").second, native_addresses.at("hCurSpriteTile").second,
                native_addresses.at("hCurSpriteOAMFlags").second, acceptance_word("AcceptanceObjectStride"),
                native_addresses.at("wObjectStructs").first, acceptance_word("AcceptanceObjectSprite"), acceptance_word("AcceptanceObjectMap"));
            auto object_update = native_addresses.at("HandleMapObjects");
            symbol<void(*)(unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned)>(core, "acceptance_profile_simulation")(
                object_update.first, object_update.second, native_addresses.at("wObjectStructs").first,
                native_addresses.at("wObjectStructs").second, acceptance_word("AcceptanceObjectStride"),
                acceptance_word("AcceptanceObjectMap"),
                (acceptance_original_objects == ~0u ? acceptance_data("AcceptanceScenarioCounts", acceptance_data("AcceptanceScenarioID")) : acceptance_original_objects));
            auto simulation_field = symbol<void(*)(unsigned,unsigned)>(core, "acceptance_profile_simulation_field");
            const unsigned simulation_fields = acceptance_symbols.at("AcceptanceSimulationFieldsEnd").address -
                                               acceptance_symbols.at("AcceptanceSimulationFields").address;
            if (simulation_fields > 64) fail("Simulation field observer capacity exceeded");
            for (unsigned i = 0; i < simulation_fields; ++i)
                simulation_field(i, acceptance_data("AcceptanceSimulationFields", i));
            auto save_start = native_addresses.at("wPlayerData");
            auto save_routine = native_addresses.at("SaveGameData");
            symbol<void(*)(unsigned,unsigned)>(core,"acceptance_profile_save_routine")(save_routine.first,save_routine.second);
            auto menu_routine = native_addresses.at(menu_observer_requested ? arguments.at("--acceptance-menu-routine") : "StartMenu");
            acceptance_menu_source_bank = menu_routine.first;
            acceptance_menu_source_pc = menu_routine.second;
            symbol<void(*)(unsigned,unsigned)>(core,"acceptance_profile_menu_routine")(menu_routine.first,menu_routine.second);
            symbol<void(*)(unsigned,unsigned,unsigned,unsigned,unsigned,unsigned,unsigned)>(core,"mobile_profile_save")(
                0, 0x20, native_addresses.at("wPlayerDataEnd").second - save_start.second, 0, save_start.second, 0, 0);
            symbol<void(*)(unsigned,unsigned,unsigned)>(core,"mobile_profile_native_oam")(
                native_addresses.at("wShadowOAM").second,
                bindings.count("wMobileVirtualDescriptors") ? bindings.at("wMobileVirtualDescriptors").address : 0,
                native_addresses.at("wObjectStructs").second);
            symbol<void(*)(unsigned)>(core,"mobile_profile_native_facing")(native_addresses.at("Facings").first);
            native_oam_byte = symbol<unsigned(*)(unsigned)>(core,"mobile_profile_native_oam_byte");
            native_figure_byte = symbol<unsigned(*)(unsigned)>(core,"mobile_profile_native_figure_byte");
        }
        if (bindings.count("wMobileVirtualActorMode")) {
            if (auto scene = reinterpret_cast<void(*)(unsigned)>(dlsym(core, "mobile_profile_scene")))
                scene(bindings.at("wMobileVirtualStage").address);
            if (auto publication = reinterpret_cast<void(*)(unsigned)>(dlsym(core, "mobile_profile_plan_publication")))
                publication(bindings.at("wMobileVirtualPublished").address);
            auto observer = symbol<void(*)(unsigned, unsigned, unsigned)>(core, "mobile_profile_native_oam");
            observer(native_addresses.at("wShadowOAM").second,
                     bindings.at("wMobileVirtualDescriptors").address,
                     native_addresses.at("wObjectStructs").second);
            native_oam_byte = symbol<unsigned(*)(unsigned)>(core, "mobile_profile_native_oam_byte");
            if (auto facing = reinterpret_cast<void(*)(unsigned)>(dlsym(core, "mobile_profile_native_facing"))) {
                facing(native_addresses.at("Facings").first);
                if (native_addresses.count("MobileCopiedFacingStepDown0")) {
                    auto offset = symbol<void(*)(unsigned)>(core, "mobile_profile_native_template_offset");
                    offset((native_addresses.at("MobileCopiedFacingStepDown0").second -
                            native_addresses.at("FacingStepDown0").second) & 0xffff);
                }
                native_figure_byte = symbol<unsigned(*)(unsigned)>(core, "mobile_profile_native_figure_byte");
            }
        }
        if (auto configure = reinterpret_cast<void(*)(unsigned,unsigned,unsigned)>(
                dlsym(core, "mobile_profile_world_phase_configure"))) {
            configure(native_addresses.at("hDelayFrameLY").second,
                      native_addresses.at("hVBlankOccurred").second,
                      native_addresses.at("wOverworldDelaySkip").second);
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
    // At most activation, first failure and final status: bounded diagnostic evidence.
    unsigned static_snapshots = 0;
    auto static_snapshot = [&](const char* event) {
        if (!static_status || static_snapshots >= 3) return;
        ++static_snapshots;
        const auto live = capture(sram, sram_size);
        static_status_log << "{\"static_status\":\"" << event << "\",\"frame\":" << input_frame
            << ",\"profile_window_start\":" << (arguments.count("--profile-window-start") ? std::stoul(arguments.at("--profile-window-start")) : 3600)
            << ",\"profile_world_updates\":" << (metric ? metric(21) : 0)
            << ",\"scene_marker\":" << symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_marker_count")(0)
            << ",\"native_scene_ready\":" << symbol<unsigned(*)()>(core, "acceptance_profile_native_scene_ready")()
            << ",\"started\":" << static_started << ",\"written\":" << static_written << ",\"observer\":[";
        for (unsigned i = 0; i < 24; ++i) static_status_log << (i ? "," : "") << static_status(i);
        static_status_log << "],\"predicates\":{";
        const char* names[] = {"wMapGroup", "wMapNumber", "wMapStatus", "wScriptRunning"};
        for (unsigned i = 0; i < 4; ++i)
            static_status_log << (i ? "," : "") << '\"' << names[i] << "\":" << live.at(names[i]);
        static_status_log << "}}\n"; static_status_log.flush();
        if (!static_status_log) fail("Static status evidence write failed");
    };
    if (acceptance_enabled && !arguments.count("--save-in") && !arguments.count("--snapshot-in")) {
        // A fresh erased battery image gives both controls identical unused
        // bytes. Some cores allocate SRAM without initializing its contents.
        if (!sram || !sram_size) fail("Acceptance requires battery memory");
        std::memset(sram, 0xff, sram_size);
        std::ofstream erased(system_directory + "/initial-battery.sav", std::ios::binary);
        erased.write(reinterpret_cast<const char*>(sram), sram_size);
        if (!erased) fail("Cannot retain initial battery evidence");
    }
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
    auto acceptance_save_metric = reinterpret_cast<uint64_t(*)(unsigned)>(dlsym(core, "mobile_profile_save_metric"));
    auto acceptance_marker_count = reinterpret_cast<unsigned(*)(unsigned)>(dlsym(core, "acceptance_profile_marker_count"));
    auto acceptance_save_active = reinterpret_cast<unsigned(*)()>(dlsym(core, "acceptance_profile_save_active"));
    auto acceptance_menu_active = reinterpret_cast<unsigned(*)()>(dlsym(core, "acceptance_profile_menu_active"));
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
    if (arguments.count("--ordinary-stream")) {
        if (arguments.count("--effect-video") || arguments.count("--snapshot-in") || arguments.count("--test-snapshot-roundtrip") || arguments.count("--snapshot-out") || arguments.count("--test-snapshot-out") || arguments.count("--measurement-gate"))
            fail("Ordinary capture cannot share Head capture or restore state");
        const auto requested_count = arguments.count("--ordinary-count") ? std::stoul(arguments.at("--ordinary-count")) : 800;
        if (!requested_count || requested_count > 800) fail("Invalid ordinary capture count");
        ordinary_count = requested_count;
        const auto requested_start = arguments.count("--ordinary-start") ? std::stoul(arguments.at("--ordinary-start")) : 0;
        if (requested_start >= frame_limit) fail("Invalid ordinary capture start");
        ordinary_start = requested_start;
        if (!ordinary_count || ordinary_count > 800 || ordinary_start >= frame_limit) fail("Invalid ordinary capture bounds");
        if (disabled_reference || initializer_requested) ordinary_start = ~0u; // Control request commits only after authenticated source/DMA readiness.
        std::signal(SIGPIPE, SIG_IGN);
        ordinary_fd = open(arguments.at("--ordinary-stream").c_str(), O_WRONLY | O_NONBLOCK);
        struct stat info;
        if (ordinary_fd < 0 || fstat(ordinary_fd, &info) || !S_ISFIFO(info.st_mode)) fail("Ordinary stream requires a FIFO with a live reader");
        ordinary_status = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_ordinary_status");
        ordinary_failure = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_ordinary_failure");
        if (ordinary_status(0) != 1) fail("Ordinary capture ABI mismatch");
        ordinary_callback = symbol<unsigned(*)(const void*)>(core, "acceptance_profile_ordinary_callback");
        ordinary_effect_video_byte = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_video_byte");
        ordinary_effect_video_field = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_video_field");
        ordinary_effect_display_field = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_display_field");
        ordinary_effect_display_matches = symbol<unsigned(*)(const void*)>(core, "acceptance_profile_display_matches");
        ordinary_effect_video_row = symbol<unsigned(*)(unsigned,unsigned)>(core, "acceptance_profile_video_row");
        ordinary_effect_video_change = symbol<unsigned(*)(unsigned,unsigned)>(core, "acceptance_profile_video_change");
        // Count is now owned by the observer; the runner stores only its stream ordinal.
        ordinary_count = 0;
    }
    const bool ordinary_gate = arguments.count("--ordinary-gate");
    bool ordinary_armed = false, ordinary_announced = false, ordinary_completed = false;
    bool ordinary_requested = false, ordinary_commit_reported = false;
    bool reference_ready_reported = false;
    unsigned ordinary_request_frame = 0;
    if (ordinary_gate && (arguments.count("--ordinary-start") || !acceptance_enabled || !profile_frame || !arguments.count("--state-out")))
        fail("Ordinary gate requires acceptance readiness and excludes a fixed start");
    auto ordinary_ready = [&]() {
        const auto live = capture(sram, sram_size);
        const bool scene = effect_options["--native-scene-field"].empty()
            ? acceptance_marker_count && acceptance_marker_count(0)
            : symbol<unsigned(*)()>(core, "acceptance_profile_native_scene_ready")();
        return scene && metric && metric(21) > 0 && live.at("wMapStatus") == 2 &&
            live.at("wScriptRunning") == 0 && live.at("wMobileOnline") == 1 && live.at("wMobilePeerCount") == 3;
    };
    unsigned measurement_start = arguments.count("--profile-window-start") ? std::stoul(arguments.at("--profile-window-start")) : 3600;
    const bool measurement_gate = arguments.count("--measurement-gate");
    bool measurement_released = !measurement_gate, measurement_ready = false;
    // Independent operands, including those skipped by the unchanged arm gate.
    auto timing_prearm_snapshot = [&]() {
        const auto live = capture(sram,sram_size);
        // Disabled symbols omit adapter fields; absence differs from a valid zero.
        auto optional_adapter_value = [&](const char* name) {
            const auto found = live.find(name);
            return found == live.end() ? std::string("null") : std::to_string(found->second);
        };
        std::ostringstream out;
        out << "{\"host_frame\":" << input_frame
            << ",\"profile_window_start\":" << measurement_start
            << ",\"metric21\":" << (metric ? metric(21) : 0)
            << ",\"metric_available\":" << bool(metric)
            << ",\"native_scene_ready\":" << symbol<unsigned(*)()>(core,"acceptance_profile_native_scene_ready")()
            << ",\"map_status\":" << live.at("wMapStatus") << ",\"script_running\":" << live.at("wScriptRunning")
            << ",\"adapter_enabled\":" << adapter_enabled << ",\"measurement_gate\":" << measurement_gate
            << ",\"measurement_released\":" << measurement_released << ",\"measurement_ready\":" << measurement_ready
            << ",\"online\":" << optional_adapter_value("wMobileOnline") << ",\"peers\":" << optional_adapter_value("wMobilePeerCount")
            << ",\"live_scene_status\":[";
        for (unsigned f=0;f<15;++f) out << (f ? "," : "") << scene_diagnostic(f);
        out << "],\"live_fields\":[";
        for (unsigned i=0;i<8;++i) {
            out << (i ? ",[" : "[");
            for (unsigned f=0;f<7;++f) out << (f ? "," : "") << scene_field(i,f);
            out << ']';
        }
        out << "]}"; return out.str();
    };
    if (timing_requested) timing_prearm_load = timing_prearm_snapshot();
    auto align_raster = reinterpret_cast<unsigned(*)()>(dlsym(core, "mobile_frontend_align_raster"));
    auto raster_state = reinterpret_cast<unsigned(*)()>(dlsym(core, "mobile_frontend_raster_state"));
    unsigned raster_alignment_lines = 0, raster_alignment_frame = 0;
    unsigned raster_boundary_failures = 0;
    bool raster_aligned = false;
    if (acceptance_enabled && profile_frame && (!align_raster || !raster_state))
        fail("Acceptance core is missing ordinary raster alignment support");
    const unsigned measurement_warmup = arguments.count("--measurement-warmup") ? std::stoul(arguments.at("--measurement-warmup")) : 300;
    if (measurement_gate && (!acceptance_enabled || !profile_frame || measurement_warmup > 3000)) fail("Invalid measurement gate configuration");
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
        if (ordinary_requested && ordinary_status && ordinary_status(5) && input_frame - ordinary_request_frame >= 4)
            fail("Ordinary capture request did not reach a genuine row zero");
        if (ordinary_gate && !ordinary_armed) {
            const bool ready = ordinary_ready();
            if (ready && !ordinary_announced) {
                std::ofstream(arguments.at("--state-out") + ".ordinary-ready") << input_frame << '\n';
                ordinary_announced = true;
            }
            if (std::ifstream(arguments.at("--ordinary-gate")).good()) {
                if (!ready) fail("Ordinary gate released without current actual readiness");
                // Align before requesting. The next genuine render row zero
                // commits capture, since this alignment already rendered row zero.
                if (raster_state() != 0x100) {
                    raster_alignment_lines = align_raster();
                    if (!raster_alignment_lines || raster_state() != 0x100)
                        fail("Ordinary gate could not reach a real raster boundary");
                }
                if (!ordinary_ready()) fail("Ordinary readiness changed during pre-arm alignment");
                const unsigned count = arguments.count("--ordinary-count") ? std::stoul(arguments.at("--ordinary-count")) : 800;
                if (!symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_ordinary_arm")(count)) fail("Ordinary capture arm failed");
                ordinary_armed = true;
                ordinary_requested = true; ordinary_request_frame = input_frame;
            }
        }
        if (ordinary_status && !ordinary_gate && !initializer_requested && input_frame == ordinary_start) {
            const unsigned count = arguments.count("--ordinary-count") ? std::stoul(arguments.at("--ordinary-count")) : 800;
            if (!symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_ordinary_arm")(count)) fail("Ordinary capture arm failed");
            ordinary_requested = true; ordinary_request_frame = input_frame;
        }
        if (input_frame == pace_from) paced_start = std::chrono::steady_clock::now();
        if (input_frame >= pace_from) {
            auto offset = std::chrono::duration<double>((input_frame - pace_from) / (av_info.timing.fps * pace_multiplier));
            std::this_thread::sleep_until(paced_start + std::chrono::duration_cast<std::chrono::steady_clock::duration>(offset));
        }
        if (static_status && !static_started) {
            const auto live = capture(sram, sram_size);
            const bool scene = effect_options["--native-scene-field"].empty()
                ? acceptance_marker_count && acceptance_marker_count(0)
                : symbol<unsigned(*)()>(core, "acceptance_profile_native_scene_ready")();
            if ((accepted_inputs_requested || scene) && static_status(6) && live.at("wMapStatus") == 2 && live.at("wScriptRunning") == 0) {
                if (!static_enable()) { static_snapshot("enable_failed"); fail("Static source enable failed; see static status snapshot"); }
                static_started = true;
                if (disabled_reference) {
                    const unsigned count = arguments.count("--ordinary-count") ? std::stoul(arguments.at("--ordinary-count")) : 16;
                    if (!symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_ordinary_arm")(count)) fail("Disabled reference arm failed");
                    ordinary_requested = true; ordinary_request_frame = input_frame;
                    reference_snapshot("activation_request");
                }
                static_snapshot("activated");
            }
        }
        // Check the idle scene on both sides of each captured execution. A
        // transient change wholly inside run() is not observable here.
        if (ordinary_gate && ordinary_armed && !ordinary_completed && !ordinary_ready())
            fail("Ordinary capture lost live readiness before execution");
        const auto raster_before = raster_state ? raster_state() : 0;
        run();
        initializer_flush(false);
        cold_flush();
        if (cold_value && !cold_log) fail("Cold boundary diagnostic write failed");
        if (cold_value && cold_value(0,0,4)) fail("MissingColdIntent: first actual postactivation row zero has no causal completed source and outer latch");
        if (ordinary_gate && ordinary_armed && !ordinary_completed && !ordinary_ready())
            fail("Ordinary capture lost live readiness after execution");
        const auto raster_after = raster_state ? raster_state() : 0;
        if (raster_aligned && input_frame >= measurement_start && input_frame < measurement_start + 800 &&
                (raster_before != 0x100 || raster_after != 0x100)) raster_boundary_failures++;
        if (profile_frame) profile_frame();
        if (native_reference_value && !reference_ready_reported && native_reference_value(2, 51)) {
            reference_snapshot("ready"); reference_ready_reported = true;
        }
        if (ordinary_requested && !ordinary_commit_reported && ordinary_status &&
                !ordinary_status(5) && (ordinary_status(1) == 1 || ordinary_status(1) == 2)) {
            std::cerr << "ordinary_arm_v1 request_frame=" << ordinary_status(6)
                      << " request_ly=" << ordinary_status(8)
                      << " commit_frame=" << ordinary_status(7)
                      << " commit_ly=" << ordinary_status(9)
                      << " commit_ordinal=0 observed_stream_ordinal=" << ordinary_count << '\n';
            ordinary_commit_reported = true;
            reference_snapshot("commit");
        }
        if (ordinary_gate && !ordinary_completed && ordinary_status(1) == 2) {
            if (!ordinary_ready()) fail("Ordinary capture completed outside live readiness");
            const unsigned count = arguments.count("--ordinary-count") ? std::stoul(arguments.at("--ordinary-count")) : 800;
            if (!ordinary_armed || ordinary_fd >= 0 || ordinary_count != count || ordinary_status(2) != count)
                fail("Ordinary completion lacks exact count/footer");
            frame_limit = input_frame + 1;
            ordinary_completed = true;
            std::ofstream(arguments.at("--state-out") + ".ordinary-complete") << ordinary_count << '\n';
        }
        if (effect_enabled) {
            const auto event_record = symbol<unsigned(*)(unsigned,unsigned)>(core, "acceptance_profile_effect_record");
            if (event_record(0, 33))
                fail("Native effect evidence overflow: count=" + std::to_string(event_record(0, 32)) +
                     " overflow=" + std::to_string(event_record(0, 33)));
        }
        if (input_status) {
            // Preserve the immutable witness before event limits, writes or release can fail.
            const auto input_failure = input_status(3), input_failure_instruction = input_status(4);
            if (input_failure) {
                input_log << "{\"first_failure\":" << input_failure << ",\"instruction\":" << input_failure_instruction << "}\n";
                input_log.flush();
                if (!input_log) fail("Accepted input first failure write failed");
                const char* context_fields[] = {"version","reason","failure_instruction","hook_instruction",
                    "bank","pc","point","trigger","field","field_bank","field_address","old","new",
                    "source","source_closed","source_failure","source_enabled","world_ready","world_active",
                    "original_online","accepted_online","lifetime","definition","pending","latch_input",
                    "latch_source","latch_source_closed","active","pair","source_active","source_has_return",
                    "committed","latch_instruction","observer_instruction"};
                input_log << "{\"failure_context\":{";
                for (unsigned f=0;f<34;++f)
                    input_log << (f ? "," : "") << '\"' << context_fields[f] << "\":" << input_failure_context(f);
                input_log << "}}\n";
                input_log.flush();
                if (!input_log) fail("Accepted input failure context write failed");
            }
            const unsigned count = input_status(5);
            if (count > 32768 - input_events_written) fail("Accepted input event export limit exceeded (32768)");
            input_events_written += count;
            for (unsigned i=0;i<count;++i) {
                input_log << "{\"event\":[";
                for (unsigned f=0;f<14;++f) input_log << (f ? "," : "") << input_event(i,0,f);
                input_log << "],\"wire\":[";
                for (unsigned f=0;f<input_event(i,0,12);++f) input_log << (f ? "," : "") << input_event(i,1,f);
                input_log << "],\"expanded\":[";
                for (unsigned f=0;f<input_event(i,0,13)*20;++f) input_log << (f ? "," : "") << input_event(i,2,f);
                input_log << "]}\n";
            }
            input_log.flush();
            if (!input_log) fail("Accepted input event write failed");
            if (input_failure)
                fail("Accepted input observer failed: reason="+std::to_string(input_failure)+" instruction="+std::to_string(input_failure_instruction));
            if (!input_drain(count)) fail("Accepted input durable drain failed");
            if (input_status(2) && !input_ready_written) {
                input_log << "{\"ready_frame\":" << input_frame << ",\"predicate\":\"vAcceptanceInputReady\"}\n";
                input_log.flush(); if (!input_log) fail("Accepted input readiness write failed");
                input_ready_written=true;
            }
        }
        if (static_started) {
            if (native_reference_value && native_reference_value(0, 1)) fail("Disabled reference observer failed: " + std::to_string(native_reference_value(0, 1)));
            if (static_status(1)) {
                static_snapshot("observer_failed");
                fail("Independent static source observation failed: reason=" + std::to_string(static_status(1)) +
                    " active=" + std::to_string(static_status(11)) + " returned_unclosed=" + std::to_string(static_status(12)));
            }
            while (static_written < static_status(2)) {
                ++static_written;
                // Keep bounded raw evidence from the first sixteen sources.
                // Continue checking the observer and release only unreferenced IDs.
                if (accepted_inputs_requested && static_written > 4096) fail("Accepted input source export limit exceeded (4096)");
                if (static_written <= 16 || accepted_inputs_requested) {
                    static_log << "{\"source\":[";
                    for (unsigned f = 0; f < 5; ++f) static_log << (f ? "," : "") << static_source(static_written, 0, f);
                    static_log << "],\"inputs\":[";
                    for (unsigned section = 1; section <= 3; ++section) {
                        static_log << (section > 1 ? ",[" : "[");
                        for (unsigned b = 0; b < 444; ++b) static_log << (b ? "," : "") << static_source(static_written, section, b);
                        static_log << "]";
                    }
                    static_log << "]";
                    if (static_operands) {
                        static_log << ",\"original_operand_fields\":" << static_operand_provenance << ",\"original_operands\":[";
                        for (unsigned section = 1; section <= 3; ++section) {
                            static_log << (section > 1 ? ",[" : "[");
                            for (unsigned b = 0; b < 6; ++b) static_log << (b ? "," : "") << static_operands(static_written, section, b);
                            static_log << "]";
                        }
                        static_log << "]";
                    }
                    if (static_map_preimage) {
                        static_log << ",\"original_map_fields\":" << static_map_provenance << ",\"original_map_preimage\":[";
                        for (unsigned section = 1; section <= 3; ++section) {
                            static_log << (section > 1 ? ",[" : "[");
                            for (unsigned b = 0; b < 73; ++b) static_log << (b ? "," : "") << static_map_preimage(static_written, section, b);
                            static_log << "]";
                        }
                        static_log << "]";
                    }
                    static_log << "}\n"; static_log.flush();
                    if (!static_log) fail("Static source evidence write failed");
                }
            }
            const uint64_t live = static_status(4) ? std::min(static_status(3), static_status(4)) : static_status(3);
            if (live > 1) static_release(live - 1);
        }
        if (!timing_requested && !trace_enabled && conditional_actions.empty() && walk_waypoints.empty()) continue;
        auto values = capture(sram, sram_size);
        values.insert(video_figure_values.begin(), video_figure_values.end());
        if (input_status) values["vAcceptanceInputReady"] = input_status(2) && !input_status(3);
        if (effect_enabled) {
            const auto record = symbol<unsigned(*)(unsigned,unsigned)>(core, "acceptance_profile_effect_record");
            values["vAcceptanceSerialSC"] = effect_serial_status(0);
            values["vAcceptanceSerialIE"] = effect_serial_status(1);
            values["vAcceptanceHeadbuttAsked"] = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_effect_returns")(6) != 0;
            values["vAcceptanceHeadbuttFinished"] = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_effect_returns")(0);
            values["vAcceptanceHeadbuttAnimationStarted"] = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_effect_returns")(5) != 0;
            values["vAcceptanceEffectCount"] = record(0, 32);
            values["vAcceptanceEffectOverflow"] = record(0, 33);
            values["vAcceptanceNativeSceneReady"] = symbol<unsigned(*)()>(core, "acceptance_profile_native_scene_ready")();
            for (const auto id : effect_span_ids)
                values["vAcceptanceEffectActive" + std::to_string(id)] =
                    symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_effect_active")(id);
        }
        if (acceptance_enabled) {
            acceptance_capture(values, sram, sram_size);
            values["vAcceptanceSaveCount"] = acceptance_save_metric ? acceptance_save_metric(0) : 0;
            values["vAcceptanceSaveActive"] = acceptance_save_active ? acceptance_save_active() : 0;
            values["vAcceptanceMenuActive"] = acceptance_menu_active ? acceptance_menu_active() : 0;
            // Active comes from the core's mapped-ROM + PC entry hook and matching
            // stack return, not the bank sampled at the host video boundary.
            values["vAcceptanceMenuSourceBank"] = acceptance_menu_source_bank;
            values["vAcceptanceMenuSourcePC"] = acceptance_menu_source_pc;
            values["vMobileMenuInstructionActive"] = mobile_menu_instruction && values["vAcceptanceMenuActive"];
            const char* marker_names[] = {"vAcceptanceReady", "vAcceptanceDialogDone", "vAcceptanceLoaded", "vAcceptanceLoadFailed", "vAcceptanceLinkEntered"};
            for (unsigned i = 0; i < 5; ++i) values[marker_names[i]] = acceptance_marker_count ? acceptance_marker_count(i) : 0;
            values["vProfileWorldUpdates"] = metric ? metric(21) : 0;
        }
        if (native_headbutt_control)
            values["vAcceptanceHeadbuttControlUpReleased"] =
                headbutt_control_up.completed_frame(input_frame, values.at("hJoypadDown"));
        if (!ordinary_status && acceptance_enabled && profile_frame && !raster_aligned && (effect_options["--native-scene-field"].empty() ? values.at("vAcceptanceReady") : values.at("vAcceptanceNativeSceneReady")) &&
                values.at("wMapStatus") == 2 && values.at("wScriptRunning") == 0 &&
                (raster_state() & 0x100) &&
                (!bindings.count("wMobileOwner") ||
                 (values.at("wMobileOnline") == (arguments.count("--expected-online") ? std::stoul(arguments.at("--expected-online")) : 1) &&
                  values.at("wMobilePeerCount") == (arguments.count("--expected-peers") ? std::stoul(arguments.at("--expected-peers")) : 3)))) {
            // Advance the existing core scanline path to one real LCD-on wrap.
            // No CPU/RAM/ROM state is injected and no in-window realignment occurs.
            raster_alignment_lines = align_raster();
            if (!raster_alignment_lines || raster_state() != 0x100)
                fail("Ready fixture did not reach a real raster boundary");
            raster_alignment_frame = input_frame;
            raster_aligned = true;
        }
        if (measurement_gate) {
            // These files coordinate host processes only. Never alter game RAM,
            // CPU registers or the game's normal initialization/connection path.
            if (!measurement_ready && (effect_options["--native-scene-field"].empty() ? values.at("vAcceptanceReady") : values.at("vAcceptanceNativeSceneReady")) &&
                (effect_options["--native-scene-field"].empty() || values.at("vProfileWorldUpdates") > 0) &&
                values.at("wMobileOnline") == (arguments.count("--expected-online") ? std::stoul(arguments.at("--expected-online")) : 1) &&
                values.at("wMobilePeerCount") == (arguments.count("--expected-peers") ? std::stoul(arguments.at("--expected-peers")) : 3)) {
                std::ofstream(arguments.at("--state-out") + ".measurement-ready") << input_frame << '\n';
                measurement_ready = true;
            }
            if (!measurement_released && std::ifstream(arguments.at("--measurement-gate")).good()) {
                if (!measurement_ready) fail("Measurement released before actual lobby readiness");
                try { frame_limit = functional_frame_limit(input_frame, measurement_warmup, functional_horizon); }
                catch (const std::exception& error) { fail(error.what()); }
                measurement_start = frame_limit - functional_horizon;
                symbol<void(*)(unsigned)>(core, "mobile_profile_window")(measurement_start);
                measurement_released = true;
            }
        }
        if (timing_requested && timing_armed && timing_status(2) && !timing_done_frame) timing_done_frame = input_frame;
        if (timing_requested && !timing_armed) {
            const bool scene = symbol<unsigned(*)()>(core, "acceptance_profile_native_scene_ready")() &&
                metric && metric(21) > 0 && values.at("wMapStatus") == 2 && values.at("wScriptRunning") == 0;
            const bool ready = scene && (!adapter_enabled || (measurement_released && measurement_ready &&
                values.at("wMobileOnline") == 1 && values.at("wMobilePeerCount") == 3));
            if (ready) {
                timing_prearm_arm = timing_prearm_snapshot();
                if (!symbol<unsigned(*)()>(core, "acceptance_profile_timing_arm")()) fail("Native timing arm failed");
                timing_arm_frame = input_frame; timing_armed = true;
            }
        }
        values["vAcceptanceFaultArmed"] = measurement_gate && std::ifstream(arguments.at("--measurement-gate") + ".fault").good();
        values["vAcceptanceMeasuring"] = measurement_released && input_frame >= measurement_start;
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
            try {
                InputAction delivered;
                if (schedule_conditional_action(action, input_frame, frame_limit, values, delivered))
                    input_actions.push_back(delivered);
            } catch (const std::exception& error) { fail(error.what()); }
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
        if (trace_enabled && (progress != previous || temporal_frame || profile_frame || acceptance_enabled)) {
            auto wall_us = std::chrono::duration_cast<std::chrono::microseconds>(
                std::chrono::steady_clock::now().time_since_epoch()).count();
            std::ostringstream record;
            record << "{\"frame\":" << input_frame << ",\"wall_us\":" << wall_us << ",\"video_wall_us\":" << video_wall_us << ",\"symbols\":";
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
        if (!conditional_action_required_missing(action)) continue;
        std::cerr << "Conditional input not reached: earliest=" << action.earliest
                  << " button=" << action.button << '\n';
        missing_input = true;
    }

    if (walk_index != walk_waypoints.size() && deferred_failure.empty())
        deferred_failure = "Walk route did not complete";

    std::set<unsigned> colors;
    for (size_t i = 0; i + 2 < image.size(); i += 3)
        colors.insert((unsigned(image[i]) << 16) | (unsigned(image[i + 1]) << 8) | image[i + 2]);
    reference_snapshot("final");
    initializer_flush(true);
    if(initializer_value && (initializer_value(0,0,6) || initializer_written!=std::stoul(arguments.at("--ordinary-count")) || !initializer_value(0,0,3) || !initializer_value(0,0,13)))
        fail("Initializer diagnostic incomplete or failed; see retained raw startup evidence");
    cold_flush();
    if (cold_value && (!cold_log || !cold_value(0,0,2) || !cold_value(0,0,5)))
        fail("Cold boundary diagnostic incomplete: no actual postactivation LCD row zero");
    if (input_status && (!input_ready_written || input_status(3))) fail("Accepted input diagnostic never became ready or failed");
    if (static_status) static_snapshot(static_started ? "final" : "never_armed");
    if (static_status && (!static_started || !static_written || static_status(1)))
        fail("Static source extraction incomplete: started=" + std::to_string(static_started) +
            " written=" + std::to_string(static_written) + " readiness=" + std::to_string(static_status(6)) +
            " world_returns=" + std::to_string(static_status(7)) + " reason=" + std::to_string(static_status(1)) +
            " active=" + std::to_string(static_status(11)) + " returned_unclosed=" + std::to_string(static_status(12)) +
            " frame=" + std::to_string(input_frame) + " profile_world_updates=" + std::to_string(metric ? metric(21) : 0));
    if (ordinary_status) {
        if (ordinary_status(1) != 2 || ordinary_count != ordinary_status(2)) {
            symbol<void(*)()>(core, "acceptance_profile_ordinary_cancel")();
            fail("Ordinary capture ended incomplete");
        }
        if (ordinary_fd >= 0) close(ordinary_fd); ordinary_fd = -1;
    }
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
    if (measurement_gate && !measurement_released) fail("Actual four-client lobby did not become ready");
    write_image(argv[3]);
    if (timing_requested) {
        std::ofstream timing(arguments.at("--native-vblank-timing"));
        timing << "{\"schema\":\"native-vblank-timing-v1\",\"scope\":\""
               << (adapter_enabled ? "warm-established-all-four-ready" : "disabled-completed-native-world")
               << "\",\"arm_frame\":" << timing_arm_frame
               << ",\"done_host_frame\":" << timing_done_frame
               << ",\"target_completed_irqs\":16,\"event_cap\":4096,\"non_rom_bank\":0,"
                  "\"clock_limit\":\"unsigned32-wrap-gap-below-2^31\","
                  "\"core_limits\":\"batched-LY; synchronous-OAM; missing-hardware-20T-IRQ; core-T-includes-GDMA-and-HALT\","
                  "\"footprints\":\"empty-queue-required\",\"pending_names\":["
                  "\"hBGMapUpdate\",\"hBGMapTileCount\",\"hCGBPalUpdate\",\"hDMATransfer\",\"hBGMapMode\","
                  "\"hRequested2bpp\",\"hRequested1bpp\",\"hOAMUpdate\"],\"bindings\":{";
        bool first = true;
        for (const auto& b : timing_bindings) {
            if (!first) timing << ','; first = false;
            timing << '"' << b.first << "\":[" << b.second.first << ',' << b.second.second << ']';
        }
        timing << "},\"prearm\":{\"live_status_fields\":[\"enabled\",\"scene_count\",\"scene_ready\",\"world_completed\",\"simulation_active\","
                  "\"observation_invalid\",\"effect_overflow\",\"entries\",\"taken_return_candidates\",\"matched_returns\",\"invalidations\",\"overwritten_entries\",\"live_failmask\",\"context_available\",\"core_observed_frame\"],"
                  "\"live_field_fields\":[\"bank\",\"address\",\"expected\",\"mask\",\"configured\",\"raw\",\"masked\"],"
                  "\"unavailable_live_value\":18446744073709551615,\"after_load\":" << timing_prearm_load
               << ",\"before_arm\":" << (timing_prearm_arm.empty() ? "null" : timing_prearm_arm)
               << ",\"final_live\":" << timing_prearm_snapshot()
               << ",\"boundary_names\":[\"first_entry\",\"last_entry\",\"first_taken_return_candidate\",\"last_taken_return_candidate\",\"first_matched_return\",\"last_matched_return\"],"
                  "\"boundary_fields\":[\"valid\",\"core_observed_frame\",\"post_bank\",\"post_pc\",\"post_sp\",\"pre_bank\",\"pre_pc\",\"pre_sp\","
                  "\"entry_sp\",\"expected_return_sp\",\"expected_return_bank\",\"expected_return_pc\",\"opcode\",\"return_mismatch_mask\","
                  "\"world_completed\",\"scene_ready\",\"simulation_active\",\"all_initial_fields_failmask\",\"observation_invalid\","
                  "\"masked0\",\"masked1\",\"masked2\",\"masked3\",\"masked4\",\"masked5\",\"masked6\",\"masked7\"],"
                  "\"return_mismatch_bits\":[\"pre_sp\",\"post_sp\",\"return_pc\",\"return_bank\"],\"boundaries\":[";
        for (unsigned i=0;i<6;++i) {
            timing << (i ? ",[" : "[");
            for (unsigned f=0;f<27;++f) timing << (f ? "," : "") << scene_snapshot(i,f);
            timing << ']';
        }
        timing << ']';
        timing << "},\"status_fields\":[\"configured\",\"running\",\"done\",\"fault\",\"observer_failure\",\"completed_irqs\",\"event_count\",\"instruction_t\",\"core_t\",\"row0_intervals\",\"retained\",\"repeated\",\"hMapAnims_guard\"],\"first_failure_sample\":[";
        for (unsigned f = 6; f < 23; ++f) timing << (f == 6 ? "" : ",") << timing_event(~0u,f);
        timing << "],\"status\":[";
        for (unsigned i = 0; i < 13; ++i) timing << (i ? "," : "") << timing_status(i);
        timing << "],\"event_fields\":[\"kind\",\"id\",\"instruction_t\",\"core_t\",\"irq\",\"scope\","
                  "\"pc\",\"sp\",\"bank\",\"core_clock\",\"ly\",\"lcdc\",\"mode\",\"speed\",\"value\","
                  "\"pending0\",\"pending1\",\"pending2\",\"pending3\",\"pending4\",\"pending5\",\"pending6\",\"pending7\"],\"events\":[";
        for (unsigned i = 0; i < timing_status(6); ++i) {
            timing << (i ? ",[" : "[");
            for (unsigned f = 0; f < 23; ++f) timing << (f ? "," : "") << timing_event(i,f);
            timing << ']';
        }
        timing << "]}"; timing.close();
        if (!timing || !timing_armed || !timing_status(2) || timing_status(3) || timing_status(4) || timing_status(5) != 16)
            fail("Incomplete or invalid bounded native timing evidence");
    }
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
            if (auto plan = reinterpret_cast<uint64_t(*)(unsigned)>(dlsym(core, "mobile_profile_plan_metric"))) {
                const char* names[] = {"color_audit", "signature", "gather", "lookup", "seed", "select", "emit", "native_view"};
                const char* fields[] = {"count", "total_cycles", "max_cycles", "crossings"};
                for (unsigned i = 0; i < 8; ++i)
                    for (unsigned j = 0; j < 4; ++j)
                        profile << ",\"plan_" << names[i] << '_' << fields[j] << "\":" << plan(i * 4 + j);
            }
            if (auto outcome = reinterpret_cast<uint64_t(*)(unsigned)>(dlsym(core, "mobile_profile_plan_outcome"))) {
                const char* names[] = {"emit_published", "emit_unpublished", "native_view_complete", "native_view_aborted"};
                const char* fields[] = {"count", "total_cycles", "max_cycles", "crossings"};
                for (unsigned window = 0; window < 2; ++window)
                    for (unsigned i = 0; i < 4; ++i)
                        for (unsigned j = 0; j < 4; ++j)
                            profile << ",\"plan_" << names[i] << (window ? "_window_" : "_") << fields[j]
                                    << "\":" << outcome(window * 16 + i * 4 + j);
            }
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
        if (auto aligned = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_aligned_metric"))) {
            const char* stages[] = {"aligned_raw", "aligned_descriptor", "native_class_advance"};
            const char* fields[] = {"count", "max_cycles", "total_cycles", "min_entry_ly", "max_entry_ly", "vblank_crossings"};
            for (unsigned stage = 0; stage < 3; ++stage)
                for (unsigned field = 0; field < 6; ++field)
                    profile << ",\"" << stages[stage] << '_' << fields[field] << "\":" << aligned(stage * 6 + field);
        }
        if (auto scene = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_scene_metric"))) {
            const char* fields[] = {"count", "max_cycles", "total_cycles", "max_entry_ly", "max_exit_ly", "return_stage_mask"};
            for (unsigned stage = 0; stage < 10; ++stage)
                for (unsigned field = 0; field < 6; ++field)
                    profile << ",\"scene_stage" << stage << '_' << fields[field] << "\":" << scene(stage * 6 + field);
        }
        if (auto scene = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_scene_window_metric"))) {
            const char* fields[] = {"count", "max_cycles", "total_cycles", "max_entry_ly", "max_exit_ly", "return_stage_mask", "vblank_crossings"};
            for (unsigned stage = 0; stage < 10; ++stage)
                for (unsigned field = 0; field < 7; ++field)
                    profile << ",\"scene_stage" << stage << "_window_" << fields[field] << "\":" << scene(stage * 7 + field);
        }
        if (auto entry = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_scene_entry"))) {
            for (unsigned stage = 0; stage < 10; ++stage)
                profile << ",\"scene_stage" << stage << "_window_entry_ly_p50\":" << entry(stage * 2)
                        << ",\"scene_stage" << stage << "_window_entry_ly_p95\":" << entry(stage * 2 + 1);
        }
        if (auto entry = reinterpret_cast<Metric>(dlsym(core, "mobile_profile_composition_entry"))) {
            const char* stages[] = {"idle", "compose", "native_group", "peer_group", "merge_palette"};
            for (unsigned stage = 0; stage < 5; ++stage)
                profile << ",\"" << stages[stage] << "_window_entry_ly_p50\":" << entry(stage * 2)
                        << ",\"" << stages[stage] << "_window_entry_ly_p95\":" << entry(stage * 2 + 1);
        }
        if (auto phase = reinterpret_cast<uint64_t(*)(unsigned,unsigned)>(
                dlsym(core, "mobile_profile_world_phase"))) {
            profile << ",\"world_update_phases\":[";
            const auto count = phase(0, 8);
            for (unsigned row = 0; row < count; ++row) {
                if (row) profile << ',';
                profile << '[';
                for (unsigned field = 0; field < 8; ++field) {
                    if (field) profile << ',';
                    profile << phase(row, field);
                }
                profile << ']';
            }
            profile << ']';
        }
        if (acceptance_enabled) {
            auto record = symbol<unsigned(*)(unsigned,unsigned)>(core, "acceptance_profile_simulation_record");
            profile << ",\"completed_native_updates\":[";
            for (unsigned i = 0; i < record(0, 2); ++i) {
                if (i) profile << ',';
                profile << '[' << record(i, 0) << ',' << record(i, 1) << ']';
            }
            profile << "],\"completed_native_update_overflow\":" << record(0, 3);
        }
        if (effect_enabled) {
            const auto record = symbol<unsigned(*)(unsigned,unsigned)>(core, "acceptance_profile_effect_record");
            const auto sample = symbol<unsigned(*)(unsigned,unsigned,unsigned)>(core, "acceptance_profile_effect_sample");
            const unsigned count = record(0, 32), overflow = record(0, 33);
            profile << ",\"native_effect_event_count\":" << count
                    << ",\"native_effect_event_overflow\":" << overflow
                    << ",\"native_effect_script_bindings\":{";
            bool script_first = true;
            for (const char* name : {"AskHeadbuttScript", "AutoHeadbuttScript"}) {
                if (!script_first) profile << ',';
                script_first = false;
                const auto point = acceptance_symbols.at(name);
                profile << '\"' << name << "\":[" << point.bank << ',' << point.address << ']';
            }
            profile << "},\"native_effect_source_bindings\":{\"teardown_return\":[";
            const auto teardown = acceptance_symbols.at("ShakeHeadbuttTree.hide_loop");
            profile << teardown.bank << ',' << teardown.address + 8;
            profile << "],\"initial_return\":[";
            const auto initial = acceptance_symbols.at("ShakeHeadbuttTree");
            const auto initial_end = acceptance_symbols.at("ShakeHeadbuttTree.got_gfx");
            const auto clear = acceptance_symbols.at("ClearSpriteAnims");
            unsigned initial_return = 0;
            for (unsigned pc = initial.address; pc + 2 < initial_end.address; ++pc) {
                const unsigned at = initial.bank * 0x4000 + pc - 0x4000;
                if (at + 2 >= rom.size()) fail("Native initial source outside ROM");
                if (static_cast<unsigned char>(rom[at]) == 0xcd &&
                    static_cast<unsigned char>(rom[at + 1]) == (clear.address & 255) &&
                    static_cast<unsigned char>(rom[at + 2]) == (clear.address >> 8)) {
                    if (initial_return) fail("Ambiguous native initialization source");
                    initial_return = pc + 3;
                }
            }
            if (!initial_return) fail("Missing native initialization source");
            profile << initial.bank << ',' << initial_return;
            profile << "]},\"native_effect_events\":[";
            for (unsigned i = 0; i < count; ++i) {
                if (i) profile << ',';
                profile << "{\"fields\":[";
                for (unsigned field = 0; field < 22; ++field) {
                    if (field) profile << ',';
                    profile << record(i, field);
                }
                profile << "],\"watches\":{";
                bool first = true;
                for (const auto& watch : effect_watch_lengths) {
                    if (!first) profile << ',';
                    first = false; profile << '\"' << watch.first << "\":[";
                    for (unsigned byte = 0; byte < watch.second; ++byte) {
                        if (byte) profile << ',';
                        profile << sample(i, watch.first, byte);
                    }
                    profile << ']';
                }
                profile << "}}";
            }
            profile << ']';
            if (overflow) fail("Native effect evidence overflow");
            if (effect_video_field) {
                profile << ",\"native_effect_video_overflow\":" << effect_video_overflow
                        << ",\"native_effect_video_frames\":[";
                auto bytes = [&](const auto& data) {
                    profile << '[';
                    for (unsigned i = 0; i < data.size(); ++i) { if (i) profile << ','; profile << unsigned(data[i]); }
                    profile << ']';
                };
                for (unsigned i = 0; i < effect_video_frames.size(); ++i) {
                    if (i) profile << ',';
                    const auto& frame = effect_video_frames[i];
                    profile << "{\"host_frame\":" << frame.host_frame << ",\"video_frame\":" << frame.video_frame
                            << ",\"width\":" << frame.width << ",\"height\":" << frame.height
                            << ",\"format\":" << frame.format << ",\"render_metadata\":[";
                    for (unsigned f = 0; f < 14; ++f) { if (f) profile << ','; profile << frame.metadata[f]; }
                    profile << "],\"display_metadata\":[";
                    for (unsigned f = 0; f < 24; ++f) { if (f) profile << ','; profile << frame.display[f]; }
                    profile << "],\"hardware\":"; bytes(frame.hardware);
                    profile << ",\"private_oam_page\":" << frame.private_page << ",\"private_oam_bank\":" << frame.private_bank;
                    profile << ",\"rows\":"; bytes(frame.rows);
                    profile << ",\"changes\":"; bytes(frame.changes);
                    profile << ",\"pixels\":"; bytes(frame.pixels); profile << '}';
                }
                profile << ']';
                if (effect_video_overflow) fail("Native effect video evidence overflow");
            }
        }
        profile << ",\"raster_alignment_lines\":" << raster_alignment_lines
                << ",\"raster_alignment_frame\":" << raster_alignment_frame
                << ",\"raster_boundary_failures\":" << raster_boundary_failures;
        profile << ",\"world_window_start\":" << measurement_start;
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
        values.insert(video_figure_values.begin(), video_figure_values.end());
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
        values.insert(video_figure_values.begin(), video_figure_values.end());
        if (effect_enabled) {
            const auto record = symbol<unsigned(*)(unsigned,unsigned)>(core, "acceptance_profile_effect_record");
            values["vAcceptanceSerialSC"] = effect_serial_status(0);
            values["vAcceptanceSerialIE"] = effect_serial_status(1);
            values["vAcceptanceHeadbuttAsked"] = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_effect_returns")(6) != 0;
            values["vAcceptanceHeadbuttFinished"] = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_effect_returns")(0);
            values["vAcceptanceHeadbuttAnimationStarted"] = symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_effect_returns")(5) != 0;
            values["vAcceptanceEffectCount"] = record(0, 32);
            values["vAcceptanceEffectOverflow"] = record(0, 33);
            values["vAcceptanceNativeSceneReady"] = symbol<unsigned(*)()>(core, "acceptance_profile_native_scene_ready")();
            for (const auto id : effect_span_ids)
                values["vAcceptanceEffectActive" + std::to_string(id)] =
                    symbol<unsigned(*)(unsigned)>(core, "acceptance_profile_effect_active")(id);
        }
        if (acceptance_enabled) {
            acceptance_capture(values, sram, sram_size);
            const char* marker_names[] = {"vAcceptanceReady", "vAcceptanceDialogDone", "vAcceptanceLoaded", "vAcceptanceLoadFailed", "vAcceptanceLinkEntered"};
            for (unsigned i = 0; i < 5; ++i) values[marker_names[i]] = acceptance_marker_count ? acceptance_marker_count(i) : 0;
            values["vProfileWorldUpdates"] = metric ? metric(21) : 0;
        }
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
