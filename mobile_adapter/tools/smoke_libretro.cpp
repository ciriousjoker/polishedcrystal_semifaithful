// Native emulator probe with scripted input, battery saves, and symbol observers.
// Pinned DoubleCherryGB backend, not a multiplayer acceptance test.
// Build with the pinned core's libretro include directory and run one process
// per instance. Full CPU/WRAM snapshots are never loaded. Save inputs are read only.
#include <libretro.h>
#include <dlfcn.h>
#include <cstdarg>
#include <cstdint>
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
#include <vector>

static std::map<std::string, std::string> options;
static std::string system_directory;
static bool loaded = false;
static bool updated = true;
static bool adapter_option_requested = false;
static unsigned input_frame = 0;
static unsigned video_frames = 0, width = 0, height = 0;
static retro_pixel_format pixel_format = RETRO_PIXEL_FORMAT_0RGB1555;
static std::vector<unsigned char> image;
static std::vector<retro_memory_descriptor> memory_maps;
struct InputAction { unsigned first, end, button; };
static std::vector<InputAction> input_actions = {{700, 710, RETRO_DEVICE_ID_JOYPAD_START}};
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
            variable->value = loaded ? "1" : "0";
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

static size_t audio(const int16_t*, size_t frames) { return frames; }
static void input_poll() {}
static int16_t input_state(unsigned port, unsigned device, unsigned, unsigned id) {
    if (port != 0 || device != RETRO_DEVICE_JOYPAD) return 0;
    for (const auto& action : input_actions)
        if (id == action.button && input_frame >= action.first && input_frame < action.end) return 1;
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
                     "         --save-in SAV --save-out SAV\n"
                     "Input script lines: FIRST_FRAME BUTTON DURATION (blank lines and # comments allowed)\n";
        return 1;
    }
    unsigned frame_limit = 1200;
    std::map<std::string, std::string> arguments;
    for (int i = 5; i < argc; i += 2) {
        std::string key = argv[i];
        if (key != "--frames" && key != "--input" && key != "--symbols" &&
            key != "--state-out" && key != "--save-in" && key != "--save-out") fail("Unknown option: " + key);
        arguments[key] = argv[i + 1];
    }
    if (arguments.count("--frames")) {
        try { frame_limit = std::stoul(arguments.at("--frames")); }
        catch (...) { fail("Invalid frame count"); }
        if (frame_limit < 1200 || frame_limit > 100000) fail("Frame count must be between 1200 and 100000");
    }
    if (arguments.count("--input")) {
        std::ifstream script(arguments.at("--input"));
        if (!script) fail("Cannot read input script");
        input_actions.clear();
        std::string line;
        while (std::getline(script, line)) {
            std::istringstream row(line.substr(0, line.find('#')));
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
    auto run = symbol<void(*)()>(core, "retro_run");
    for (input_frame = 0; input_frame < frame_limit; ++input_frame) run();
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
    std::ofstream output(argv[3], std::ios::binary);
    output << "P6\n" << width << ' ' << height << "\n255\n";
    output.write(reinterpret_cast<const char*>(image.data()), image.size());
    if (!output) return 1;
    if (arguments.count("--save-out")) {
        if (!sram || !sram_size) fail("Battery buffer unavailable");
        std::ofstream save(arguments.at("--save-out"), std::ios::binary);
        save.write(reinterpret_cast<const char*>(sram), sram_size);
        if (!save) fail("Cannot write battery save");
    }
    if (arguments.count("--state-out")) {
        if (!arguments.count("--symbols")) fail("--state-out requires matching --symbols");
        const std::set<std::string> watched = {
            "hCGB", "hCrashCode", "hVBlankCounter", "hInMenu", "wJumptableIndex",
            "wSaveFileExists", "wMapGroup", "wMapNumber", "wXCoord", "wYCoord",
            "wPlayerMapX", "wPlayerMapY",
            "wMenuCursorY", "wMenuSelection", "wOptionsMenuIsInitial",
            "sCheckValue1", "sCheckValue2", "sSaveVersion", "sChecksum",
            "sBackupCheckValue1", "sBackupCheckValue2"
        };
        std::ifstream symbols(arguments.at("--symbols"));
        if (!symbols) fail("Cannot read matching symbols");
        std::map<std::string, unsigned> values;
        std::string line;
        while (std::getline(symbols, line)) {
            std::istringstream row(line);
            std::string location, name;
            if (!(row >> location >> name) || location[0] == ';') continue;
            if (!watched.count(name)) continue;
            auto colon = location.find(':');
            if (colon == std::string::npos) fail("Invalid symbol location");
            unsigned bank = std::stoul(location.substr(0, colon), nullptr, 16);
            unsigned address = std::stoul(location.substr(colon + 1), nullptr, 16);
            unsigned value = observe(bank, address, sram, sram_size);
            if (name == "sSaveVersion") value = (value << 8) | observe(bank, address + 1, sram, sram_size);
            if (name == "sChecksum") value |= observe(bank, address + 1, sram, sram_size) << 8;
            values[name] = value;
        }
        if (!values.count("hCrashCode") || !values.count("hCGB")) fail("Required observer symbols missing");
        if (values.at("hCrashCode")) fail("ROM crash marker is nonzero");
        if (values.at("hCGB") != 1) fail("ROM did not detect Game Boy Color");
        std::ofstream state(arguments.at("--state-out"));
        state << "{\n  \"frame_calls\": " << frame_limit << ",\n  \"symbols\": {";
        bool first = true;
        for (const auto& value : values) {
            state << (first ? "\n" : ",\n") << "    \"" << value.first << "\": " << value.second;
            first = false;
        }
        state << "\n  }\n}\n";
        if (!state) fail("Cannot write observer result");
    }
    symbol<void(*)()>(core, "retro_unload_game")();
    symbol<void(*)()>(core, "retro_deinit")();
    std::cout << "Emulator probe passed: " << frame_limit << " frame calls, " << video_frames << " video frames, 160x144, " << colors.size()
              << " colors; Mobile Adapter enabled. No network exchange tested.\n";
    dlclose(core);
    return 0;
}
