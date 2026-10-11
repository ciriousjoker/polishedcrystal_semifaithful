// Native emulator probe with scripted input, battery saves, and symbol observers.
// Pinned DoubleCherryGB backend; Python runners own scenario assertions.
// Build with the pinned core's libretro include directory and run one process
// per instance. Acceptance tests boot battery saves. Explicit snapshot flags are
// for regenerating manual-play start states with the current ROM/core only.
#include <libretro.h>
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
static std::ofstream input_events;
static std::set<std::pair<unsigned, unsigned>> logged_inputs;
static std::vector<retro_memory_descriptor> memory_maps;
struct InputAction { unsigned first, end, button; };
static std::vector<InputAction> input_actions = {{700, 710, RETRO_DEVICE_ID_JOYPAD_START}};
struct ConditionalAction {
    unsigned earliest, button, duration;
    std::map<std::string, unsigned> conditions;
    bool fired = false;
};
static std::vector<ConditionalAction> conditional_actions;
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
        "hCGB", "hCrashCode", "hVBlankCounter", "hInMenu", "wJumptableIndex",
        "wSaveFileExists", "wMapGroup", "wMapNumber", "wXCoord", "wYCoord",
        "wPlayerMapX", "wPlayerMapY", "wPlayerSpriteX", "wPlayerSpriteY",
        "wPlayerSpriteXOffset", "wPlayerSpriteYOffset", "wPlayerFacing", "wPlayerDirection",
        "wPlayerStepFrame", "wPlayerBGMapOffsetX", "wPlayerBGMapOffsetY", "wMapStatus", "wScriptRunning",
        "wObjectStructs", "wEventFlags", "wCurBox", "wPlayerCurrentOAMSlot",
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
        if (!watched.count(name) && name.rfind("wMobile", 0) != 0) continue;
        auto colon = location.find(':');
        if (colon == std::string::npos) fail("Invalid symbol location");
        bindings[name] = {static_cast<unsigned>(std::stoul(location.substr(0, colon), nullptr, 16)),
                          static_cast<unsigned>(std::stoul(location.substr(colon + 1), nullptr, 16))};
    }
}

static std::map<std::string, unsigned> capture(const unsigned char* sram, size_t sram_size) {
    const std::set<std::string> words = {"sChecksum", "wMobileRemaining", "wMobileTxPointer",
        "wMobileRxPointer", "wMobileSequence", "wMobileAttemptTicks", "wMobileIRQCount",
        "wMobilePresenceCount", "wMobileMapEpoch", "wMobilePeer1DrawX", "wMobilePeer1DrawY",
        "wMobilePeer2DrawX", "wMobilePeer2DrawY"};
    const std::set<std::string> wire_words = {"wMobilePlayerId", "wMobileGeneration", "wMobileInstanceKey",
        "wMobilePeer1Id", "wMobilePeer1Generation", "wMobilePeer1Revision",
        "wMobilePeer1Instance", "wMobilePeer1Epoch", "wMobilePeer1X", "wMobilePeer1Y",
        "wMobilePeer2Id", "wMobilePeer2Generation", "wMobilePeer2Revision",
        "wMobilePeer2Instance", "wMobilePeer2Epoch", "wMobilePeer2X", "wMobilePeer2Y"};
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
    for (const char* peer : {"1", "2"}) {
        std::string prefix = std::string("wMobilePeer") + peer;
        if (!values.count(prefix + "OAMStart")) continue;
        unsigned slot = values.at(prefix + "OAMStart");
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
        unsigned occupied = 0, suppressed = 0, restored = 0;
        for (unsigned i = 0; i < 13; ++i) {
            unsigned start = objects.address + i * 34;
            if (observe(objects.bank, start, sram, sram_size)) occupied |= 1u << i;
            unsigned facing = observe(saved.bank, saved.address + i, sram, sram_size);
            if (i && facing != 255) {
                suppressed |= 1u << i;
                if (facing == observe(objects.bank, start + 13, sram, sram_size)) restored |= 1u << i;
            }
        }
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
    for (unsigned quad = 0; quad < 40; ++quad)
        for (int line = int(video_oam[quad * 4]) - 16; line < int(video_oam[quad * 4]) - 8; ++line)
            if (line >= 0 && line < 144) scanlines[line]++;
    unsigned maximum = 0;
    for (unsigned count : scanlines) maximum = std::max(maximum, count);
    values["vOAMMaxScanline"] = maximum;
    if (bindings.count("wMobileNPCOptional") && bindings.count("wMobileOAMOutput")) {
        auto optional = bindings.at("wMobileNPCOptional");
        auto output = bindings.at("wMobileOAMOutput");
        unsigned clipped = 0;
        for (unsigned quad = 0; quad < 40; ++quad)
            if (observe(optional.bank, optional.address + quad, sram, sram_size) &&
                !observe(output.bank, output.address + quad * 4, sram, sram_size)) clipped++;
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
                     "         --checkpoint-dir EXISTING_DIRECTORY --profile-out JSON --pace-multiplier 1|3\n"
                     "         --snapshot-out STATE --snapshot-in STATE (manual-play preparation only)\n"
                     "Input: FRAME BUTTON DURATION, or ON MIN_FRAME SYMBOL=VALUE[,SYMBOL=VALUE] BUTTON DURATION\n";
        return 1;
    }
    unsigned frame_limit = 1200;
    std::map<std::string, std::string> arguments;
    for (int i = 5; i < argc; i += 2) {
        std::string key = argv[i];
        if (key != "--frames" && key != "--input" && key != "--symbols" &&
            key != "--state-out" && key != "--save-in" && key != "--save-out" &&
            key != "--adapter" && key != "--pace-from" && key != "--trace-out" &&
            key != "--checkpoint-dir" && key != "--profile-out" &&
            key != "--snapshot-out" && key != "--snapshot-in" && key != "--input-events" &&
            key != "--finish-barrier" && key != "--pace-multiplier") fail("Unknown option: " + key);
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
    void (*profile_frame)() = nullptr;
    if (arguments.count("--profile-out")) {
        metric = symbol<Metric>(core, "mobile_profile_metric");
        profile_frame = symbol<void(*)()>(core, "mobile_profile_frame");
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
        if (!configured) fail("MobileTick symbol missing");
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
    if (arguments.count("--trace-out")) {
        trace.open(arguments.at("--trace-out"));
        if (!trace) fail("Cannot create trace output");
    }
    unsigned pace_from = frame_limit;
    if (arguments.count("--pace-from")) pace_from = std::stoul(arguments.at("--pace-from"));
    unsigned pace_multiplier = 1;
    if (arguments.count("--pace-multiplier")) {
        const auto& value = arguments.at("--pace-multiplier");
        if (value != "1" && value != "3") fail("Pace multiplier must be 1 or 3");
        pace_multiplier = std::stoul(value);
    }
    if (av_info.timing.fps <= 0) fail("Invalid emulator frame rate");
    std::chrono::steady_clock::time_point paced_start;
    std::map<std::string, unsigned> previous;
    const bool barrier_enabled = arguments.count("--finish-barrier");
    bool announced_ready = false;
    auto settle_started = std::chrono::steady_clock::time_point{};
    for (input_frame = 0; input_frame < frame_limit ||
         (barrier_enabled && std::ifstream(arguments.at("--finish-barrier")).good()); ++input_frame) {
        if (input_frame >= frame_limit) {
            if (input_frame == frame_limit) settle_started = std::chrono::steady_clock::now();
            if (std::chrono::steady_clock::now() - settle_started > std::chrono::seconds(20))
                fail("Lobby observation barrier did not settle");
            auto live = capture(sram, sram_size);
            if (!announced_ready && live.at("wMobileOnline") == 1 && live.at("wMobilePeerCount") == 2) {
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
        if (!trace.is_open() && conditional_actions.empty()) continue;
        auto values = capture(sram, sram_size);
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
            if (entry.first.rfind("vMobile", 0) == 0 || entry.first.rfind("vNPC", 0) == 0 || entry.first == "vOAMMaxScanline")
                progress.insert(entry);
        if (trace.is_open() && progress != previous) {
            auto wall_us = std::chrono::duration_cast<std::chrono::microseconds>(
                std::chrono::steady_clock::now().time_since_epoch()).count();
            trace << "{\"frame\":" << input_frame << ",\"wall_us\":" << wall_us << ",\"symbols\":";
            json_values(trace, values);
            trace << "}\n";
            previous = progress;
            if (input_frame >= pace_from && arguments.count("--checkpoint-dir"))
                write_image(arguments.at("--checkpoint-dir") + "/frame-" + std::to_string(input_frame) + ".ppm");
        }
    }

    bool missing_input = false;
    for (const auto& action : conditional_actions) {
        if (action.fired) continue;
        std::cerr << "Conditional input not reached: earliest=" << action.earliest
                  << " button=" << action.button << '\n';
        missing_input = true;
    }

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
            "tick_stack_cursor_samples", "tick_stack_cursor_lowest_sp"};
        std::ofstream profile(arguments.at("--profile-out"));
        profile << '{';
        for (unsigned i = 0; i < sizeof(names) / sizeof(names[0]); ++i)
            profile << (i ? "," : "") << '\"' << names[i] << "\":" << metric(i);
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
    if (arguments.count("--snapshot-out")) {
        // Never snapshot an active Mobile Adapter session: peripheral sockets
        // are not included in the pinned core's serialization format.
        auto values = capture(sram, sram_size);
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
        std::ofstream file(arguments.at("--snapshot-out"), std::ios::binary);
        file.write(bytes.data(), bytes.size());
        if (!file) fail("Cannot write manual-play snapshot");
    }
    if (arguments.count("--state-out")) {
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

    if (barrier_enabled) {
        auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(15);
        while (std::ifstream(arguments.at("--finish-barrier") + ".exit").good()) {
            if (std::chrono::steady_clock::now() >= deadline) fail("Final capture barrier timed out");
            std::this_thread::sleep_for(std::chrono::milliseconds(2));
        }
    }
    symbol<void(*)()>(core, "retro_unload_game")();
    symbol<void(*)()>(core, "retro_deinit")();
    if (missing_input) {
        dlclose(core);
        return 1;
    }
    std::cout << "Emulator probe passed: " << frame_limit << " frame calls, " << video_frames << " video frames, 160x144, " << colors.size()
              << " colors; Mobile Adapter " << (adapter_enabled ? "enabled" : "disabled") << ". Check scenario assertions separately.\n";
    dlclose(core);
    return 0;
}
