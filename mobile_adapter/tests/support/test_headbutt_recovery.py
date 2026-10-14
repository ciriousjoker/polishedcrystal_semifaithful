from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import runtime


class NativeHeadbuttRecovery(unittest.TestCase):
    def fixture(self, root, bank=3, begin=0x4200):
        (root / 'macros/scripts').mkdir(parents=True)
        (root / 'macros/scripts/events.asm').write_text(
            '\tconst_def\n\tconst randomwildmon_command\n'
            '\tconst startbattle_command\n\tconst reloadmapafterbattle_command\n'
            '\tconst end_command\n')
        rom = root / 'fixture.gbc'
        data = bytearray((bank + 1) * 0x4000)
        end = begin + 24
        at = runtime.rom_offset((bank, end - 4))
        data[at:at + 4] = bytes((0, 1, 2, 3))
        rom.write_bytes(data)
        return rom, {'AutoHeadbuttScript': (bank, begin),
                     'AutoHeadbuttScript.no_battle': (bank, end),
                     'DoMenuJoypadLoop': (5, 0x5234)}

    def test_relocated_script_drives_text_and_native_run_recovery(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            rom, names = self.fixture(root, bank=8, begin=0x5100)
            with patch.object(runtime, 'ROOT', root):
                text = runtime.native_headbutt_recovery_inputs(rom, names)
            rows = text.splitlines()
            self.assertEqual(len(rows), 210)
            self.assertTrue(all(row.startswith('OPTIONAL CHECKED_AFTER ') for row in rows))
            self.assertTrue(all('hScriptBank=8,hScriptPos=20758' in row for row in rows))
            self.assertTrue(all('vAcceptanceHeadbuttFinished=1,wScriptRunning=255,wBattleMode=1' in row for row in rows))
            self.assertTrue(all(row.endswith(' B 8') for row in rows[::3] + rows[1::3]))
            self.assertTrue(all(row.endswith(' A 8') for row in rows[2::3]))
            self.assertNotIn('hInMenu', text)
            self.assertIn('CHECKED_AFTER 0 ', text)
            self.assertIn('CHECKED_AFTER 2760 ', text)
            self.assertTrue(all('vAcceptanceMenuActive=0' in row for row in rows[::3]))
            self.assertTrue(all('vAcceptanceMenuActive=1,vAcceptanceMenuSourceBank=5,vAcceptanceMenuSourcePC=21044,wBattleMenuFlags=7' in row
                                for row in rows[1::3] + rows[2::3]))
            self.assertTrue(all('wMenuCursorY=1 B 8' in row for row in rows[1::3]))
            self.assertTrue(all('wMenuCursorX=2,wMenuCursorY=2,hJoypadDown=0 A 8' in row
                                for row in rows[2::3]))
            self.assertEqual([int(row.split()[2]) for row in rows[2::3]],
                             list(range(16, 2777, 40)))

    def test_physical_run_pulses_require_cursor_source_and_release(self):
        import subprocess
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            rom, names = self.fixture(root)
            with patch.object(runtime, 'ROOT', root):
                rows = runtime.native_headbutt_recovery_inputs(rom, names)
            header = runtime.ROOT / 'mobile_adapter/tools/conditional_input.hpp'
            source = root / 'run-input.cpp'
            source.write_text('#include "' + str(header) + '"\n' + r'''
#include <cassert>
#include <vector>
int main() {
    std::istringstream script(R"INPUT(''' + rows + r''')INPUT");
    std::vector<ConditionalAction> actions;
    std::string line;
    while (std::getline(script, line)) {
        std::istringstream row(line.substr(9));
        actions.push_back(parse_conditional_action(row, true, 3200,
            {{"A", 1}, {"B", 2}}, [](const std::string&) { return true; }));
    }
    auto values = actions[2].conditions;
    std::vector<InputAction> pulses;
    unsigned confirmations = 0, last_down = 0, text_until = 0;
    bool selected = false;
    for (unsigned frame = 0; frame < 800; ++frame) {
        unsigned down = 0;
        for (const auto& pulse : pulses)
            if (frame >= pulse.first && frame < pulse.end) down |= pulse.button;
        if (down) last_down = frame;
        if (down == 2 && frame >= 60 && frame >= text_until) selected = true;
        values["hJoypadDown"] = down;
        values["vAcceptanceMenuActive"] = frame >= 20 && frame >= text_until;
        values["vAcceptanceMenuSourcePC"] = frame < 40 ? 0 : 0x5234;
        values["wMenuCursorX"] = frame < 60 ? 1 : 2;
        values["wMenuCursorY"] = selected ? 2 : 1;
        for (auto& action : actions) {
            InputAction pulse;
            if (!schedule_conditional_action(action, frame, 3200, values, pulse)) continue;
            if (pulse.button == 1) {
                assert(frame >= 60 && selected && frame >= text_until);
                assert(down == 0 && frame - last_down >= 16);
                assert(pulse.end - pulse.first == 8);
                ++confirmations;
                // Native failure leaves menu ownership until the pulse ends,
                // then enters text before the Run cursor becomes ready again.
                if (confirmations == 1) text_until = pulse.end + 35;
            }
            pulses.push_back(pulse);
        }
    }
    assert(confirmations >= 2);
    for (const auto& a : pulses) for (const auto& b : pulses)
        if (a.button != b.button)
            assert(a.end <= b.first || b.end <= a.first);
}
''')
            binary = root / 'run-input'
            subprocess.run(['c++', '-std=c++17', str(source), '-o', str(binary)], check=True)
            subprocess.run([str(binary)], check=True)

    def test_horizon_is_headbutt_only_and_preserves_control_replay(self):
        for action in ("native-headbutt", "native-headbutt-held"):
            self.assertEqual(runtime.functional_horizon_options(True, True, action),
                             ["--functional-horizon", "3200"])
            self.assertEqual(runtime.functional_horizon_options(True, False, action), [])
            self.assertEqual(runtime.functional_horizon_options(False, True, action), [])
        for action in ("idle", "mobile-menu-open", "native-headbutt-other", "resume"):
            self.assertEqual(runtime.functional_horizon_options(True, True, action), [])
        # Retain enough completion time after the final physical recovery pulse.
        self.assertLess(runtime.HEADBUTT_RECOVERY_WINDOW + 8,
                        runtime.HEADBUTT_FUNCTIONAL_HORIZON)

    def test_actual_frontend_bindings_parse_run_inputs_and_reject_unknowns(self):
        import os
        import re
        import subprocess
        frontend = (runtime.ROOT / 'mobile_adapter/tools/smoke_libretro.cpp').read_text()
        begin = frontend.index('static void load_bindings(')
        end = frontend.index('static std::map<std::string, unsigned> capture(', begin)
        loader = frontend[begin:end]
        binding = re.search(r'^struct Binding .*$', frontend, re.M).group()
        known = re.search(
            r'\[\]\(const std::string& name\) \{\s*return bindings.count\(name\) \|\|'
            r'\s*\(acceptance_enabled && name.rfind\("vAcceptance", 0\) == 0\);\s*\}',
            frontend).group()
        callback = frontend[frontend.index('static int16_t input_state('):
                            frontend.index('template<class Function>', frontend.index('static int16_t input_state('))]
        button_map = frontend[frontend.index('static const std::map<std::string, unsigned> buttons = {'):
                              frontend.index('static void fail(', frontend.index('static const std::map<std::string, unsigned> buttons = {'))]
        option_block = frontend[frontend.index('    if (arguments.count("--native-headbutt-control"))'):
                                frontend.index('    unsigned acceptance_menu_source_bank')]
        # Exercise the unchanged production loader and its former whitelist,
        # rather than declaring every generated condition to be a known symbol.
        old_loader = loader.replace('load_bindings(', 'load_old_bindings(', 1)
        self.assertEqual(old_loader.count('"wMenuCursorX", '), 1)
        old_loader = old_loader.replace('"wMenuCursorX", ', '', 1)
        header = runtime.ROOT / 'mobile_adapter/tools/conditional_input.hpp'
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            rom, names = self.fixture(root)
            with patch.object(runtime, 'ROOT', root):
                inputs = runtime.native_headbutt_control_inputs(rom, names, 0, 0)
            script = root / 'recovery.input'
            script.write_text(inputs)
            fixture = root / 'realistic.sym'
            fixture.write_text(
                '; RGBDS symbol export\n'
                '00:ff96 hJoypadDown\n00:ffb5 hScriptBank\n00:ffb6 hScriptPos\n'
                '00:ce51 wMenuCursorX\n00:ce52 wMenuCursorY\n'
                '00:d438 wScriptRunning\n01:d226 wBattleMode\n'
                '00:ce59 wBattleMenuFlags\n00:c100 wUnlistedCursor\n'
                '00:d100 wMapStatus\n00:d101 wXCoord\n00:d102 wYCoord\n'
                '00:d103 wPlayerDirection\n00:ff9a hInMenu\n')
            prelude = ('#include "' + str(header) + '"\n#include <cassert>\n'
                       '#include <fstream>\n#include <set>\n#include <vector>\n#include <iostream>\n'
                       '#include <chrono>\n#include <cstdint>\n#include <algorithm>\n' + binding + '\n'
                       'static std::map<std::string, Binding> bindings;\n'
                       'static bool acceptance_enabled = true;\n'
                       'static void fail(const std::string& message) '
                       '{ throw std::runtime_error(message); }\n')
            harness = root / 'frontend-bindings.cpp'
            callback_state = r'''
// Stable libretro joypad IDs; the button-name map below is actual production.
static const unsigned RETRO_DEVICE_JOYPAD = 1, RETRO_DEVICE_ID_JOYPAD_B = 0,
    RETRO_DEVICE_ID_JOYPAD_SELECT = 2, RETRO_DEVICE_ID_JOYPAD_START = 3,
    RETRO_DEVICE_ID_JOYPAD_UP = 4, RETRO_DEVICE_ID_JOYPAD_DOWN = 5,
    RETRO_DEVICE_ID_JOYPAD_LEFT = 6, RETRO_DEVICE_ID_JOYPAD_RIGHT = 7,
    RETRO_DEVICE_ID_JOYPAD_A = 8;
static unsigned input_frame = 0, walk_button = ~0u;
static std::vector<InputAction> input_actions;
static std::ofstream input_events;
static std::set<std::pair<unsigned,unsigned>> logged_inputs;
static bool native_headbutt_control = true;
static HeadbuttControlUpRelease headbutt_control_up;
'''
            option_function = (
                'static void validate_option(std::map<std::string,std::string> arguments,\n'
                ' std::map<std::string,std::vector<std::string>> effect_options,\n'
                ' bool acceptance_enabled, bool adapter_enabled, bool menu_observer_requested) {\n'
                + option_block + '}\n')
            harness.write_text(prelude + loader + old_loader + callback_state + button_map + callback + option_function +
                'static bool parses(const std::string& path, unsigned frame_limit = 3200) {\n'
                '  std::ifstream stream(path); std::string line;\n'
                '  try { while (std::getline(stream, line)) {\n'
                "    if (line.empty() || line[0] == '#') continue;\n"
                '    bool optional = line.rfind("OPTIONAL ", 0) == 0;\n'
                '    std::istringstream row(optional ? line.substr(9) : line);\n'
                '    parse_conditional_action(row, optional, frame_limit, buttons, ' + known + ');\n'
                '  } } catch (const std::runtime_error&) { return false; }\n'
                '  return true;\n}\n' + r'''
static unsigned native_joy_mask(unsigned button) {
    if (button == RETRO_DEVICE_ID_JOYPAD_A) return 1;
    if (button == RETRO_DEVICE_ID_JOYPAD_B) return 2;
    if (button == RETRO_DEVICE_ID_JOYPAD_UP) return 64;
    if (button == RETRO_DEVICE_ID_JOYPAD_LEFT) return 32;
    assert(false); return 0;
}
static void drives_branch(const std::string& path, bool wild) {
    std::ifstream stream(path); std::string line;
    std::vector<ConditionalAction> actions;
    while (std::getline(stream, line)) {
        if (line.empty() || line[0] == '#') continue;
        const bool optional = line.rfind("OPTIONAL ", 0) == 0;
        std::istringstream row(optional ? line.substr(9) : line);
        actions.push_back(parse_conditional_action(row, optional, 3200,
            buttons,
''' + known + r'''));
    }
    // Explicit source state, not copied action conditions. Production binding
    // admission above applies to every scheduled physical input below.
    std::map<std::string,unsigned> values = {
        {"vAcceptanceNativeSceneReady",1}, {"wMapStatus",2},
        {"wScriptRunning",0}, {"wBattleMode",0}, {"wXCoord",46}, {"wYCoord",8},
        {"vAcceptanceHeadbuttAsked",0}, {"vAcceptanceHeadbuttFinished",0},
        {"vAcceptancePartyCount",1}, {"vAcceptancePartyMove",29},
        {"wPlayerDirection",4}, {"hJoypadDown",0}, {"hInMenu",0},
        {"vAcceptanceHeadbuttAskScript",0}, {"vAcceptanceHeadbuttUseScript",0},
        {"vAcceptanceMenuActive",0}, {"vAcceptanceMenuSourceBank",5},
        {"vAcceptanceMenuSourcePC",0x5234}, {"wMenuCursorX",1}, {"wMenuCursorY",1},
        {"wBattleMenuFlags",0}, {"hScriptBank",3}, {"hScriptPos",0x4216}
    };
    input_actions.clear(); headbutt_control_up = {}; walk_button = ~0u;
    auto& pulses = input_actions;
    unsigned up = 0, use = 0, yes = 0, confirmations = 0, left = 0;
    unsigned last_down = 0, retry_at = 0, escaped_at = 0, asked_at = 0;
    bool selected = false;
    for (unsigned frame = 0; frame < 1100; ++frame) {
        input_frame = frame;
        unsigned down = 0;
        for (const auto& pulse : pulses)
            if (frame >= pulse.first && frame < pulse.end) {
                assert(input_state(0, RETRO_DEVICE_JOYPAD, 0, pulse.button) == 1);
                down |= native_joy_mask(pulse.button);
            }
        for (const auto& pulse : pulses) {
            if (frame == pulse.first && pulse.button == 4) {
                ++up; values["wPlayerDirection"] = 4;
            }
            if (frame == pulse.first && pulse.button == RETRO_DEVICE_ID_JOYPAD_A) {
                if (frame < 150) { ++use; asked_at = frame + 2; }
                else if (frame < 330) ++yes;
                else {
                    assert(wild && selected && down == 1);
                    ++confirmations; selected = false;
                    if (confirmations == 1) retry_at = pulse.end + 35;
                    else escaped_at = pulse.end + 50;
                }
            }
            if (frame == pulse.first && pulse.button == RETRO_DEVICE_ID_JOYPAD_LEFT) ++left;
        }
        if (down) last_down = frame;
        values["hJoypadDown"] = down;
        values["vAcceptanceHeadbuttControlUpReleased"] = headbutt_control_up.completed_frame(frame, down);
        // Already facing UP, ready only after both absolute floors, and native
        // ownership changes after UP. Delivery plus current source must gate A.
        values["hInMenu"] = frame >= 110 && frame < 140;
        values["vAcceptanceNativeSceneReady"] = frame >= 85;
        values["vAcceptancePartyMove"] = frame < 95 ? 0 : 29;
        values["vAcceptanceHeadbuttAsked"] = asked_at && frame >= asked_at;
        values["wScriptRunning"] = values["vAcceptanceHeadbuttAsked"] ? 255 : 0;
        values["vAcceptanceHeadbuttAskScript"] = values["vAcceptanceHeadbuttAsked"] && frame < 280;
        values["vAcceptanceHeadbuttUseScript"] = frame >= 280 && frame < 330;
        values["vAcceptanceMenuActive"] = frame >= 200 && frame < 280;
        // Wrong routine ownership must reset YES readiness.
        values["vAcceptanceMenuSourcePC"] = frame < 240 ? 0x1111 : 0x5234;
        values["vAcceptanceHeadbuttFinished"] = frame >= 370;
        if (wild && frame >= 370 && (!escaped_at || frame < escaped_at)) {
            values["wBattleMode"] = 1;
            // An unrelated suspended script cannot authorize native Run.
            values["hScriptPos"] = frame < 460 ? 0x4000 : 0x4216;
            bool menu = frame >= 420 && (!retry_at || frame >= retry_at);
            values["vAcceptanceMenuActive"] = menu;
            values["wBattleMenuFlags"] = menu ? 7 : 0;
            if (menu && down == 2) selected = true;
            values["wMenuCursorX"] = selected ? 2 : 1;
            values["wMenuCursorY"] = selected ? 2 : 1;
        } else if (frame >= 370) {
            values["wBattleMode"] = 0;
            values["vAcceptanceMenuActive"] = 0;
            const unsigned done = wild ? escaped_at + 50 : 440;
            if (frame >= done) values["wScriptRunning"] = 0;
        }
        for (auto& action : actions) {
            InputAction pulse;
            if (!schedule_conditional_action(action, frame, 3200, values, pulse)) continue;
            if (pulse.button == RETRO_DEVICE_ID_JOYPAD_A && frame < 150) {
                assert(frame >= 140 && values["hInMenu"] == 0);
                assert(values["vAcceptanceHeadbuttControlUpReleased"] == 1);
                assert(down == 0 && frame - last_down >= 16);
            }
            if (pulse.button == RETRO_DEVICE_ID_JOYPAD_A && frame >= 150) {
                assert(down == 0 && frame - last_down >= 16);
                assert(values["vAcceptanceMenuSourcePC"] == 0x5234);
                if (frame >= 330) {
                    assert(values["wMenuCursorX"] == 2 && values["wMenuCursorY"] == 2);
                    assert(values["wBattleMode"] == 1);
                }
            }
            if (pulse.button == RETRO_DEVICE_ID_JOYPAD_LEFT) {
                assert(values["vAcceptanceHeadbuttFinished"] == 1);
                assert(values["wScriptRunning"] == 0 && values["wBattleMode"] == 0);
            }
            pulses.push_back(pulse);
        }
    }
    assert(up == 1 && use == 1 && yes == 1 && left == 1);
    assert(asked_at != 0);
    bool mid_a_ask = false;
    for (const auto& pulse : pulses)
        if (pulse.button == RETRO_DEVICE_ID_JOYPAD_A && pulse.first < asked_at && asked_at < pulse.end) mid_a_ask = true;
    assert(mid_a_ask);
    assert(wild ? confirmations >= 2 : confirmations == 0);
    for (const auto& action : actions) assert(!conditional_action_required_missing(action));
    for (const auto& a : pulses) for (const auto& b : pulses)
        if (a.button != b.button && !(a.end <= b.first || b.end <= a.first)) {
            std::cerr << "overlap " << wild << " " << a.button << ":" << a.first << ":" << a.end
                      << " " << b.button << ":" << b.first << ":" << b.end << "\n";
            assert(false);
        }
}
static void checks_returned_input_barrier() {
    input_actions = {{100,108,4}};
    headbutt_control_up = {};
    for (unsigned frame = 100; frame < 150; ++frame)
        assert(!headbutt_control_up.completed_frame(frame, 0)); // never polled
    input_frame = 100;
    headbutt_control_up = {};
    assert(input_state(1, RETRO_DEVICE_JOYPAD, 0, 4) == 0);
    assert(input_state(0, 99, 0, 4) == 0);
    assert(input_state(0, RETRO_DEVICE_JOYPAD, 0, 1) == 0);
    assert(!headbutt_control_up.delivered);
    walk_button = 4;
    assert(input_state(0, RETRO_DEVICE_JOYPAD, 0, 4) == 1);
    assert(!headbutt_control_up.delivered); // a waypoint is not scripted UP8
    walk_button = ~0u;
    for (const auto& wrong : std::vector<InputAction>{{100,107,4},{100,109,4},{100,108,1}}) {
        input_actions = {wrong}; headbutt_control_up = {};
        assert(input_state(0, RETRO_DEVICE_JOYPAD, 0, wrong.button) == 1);
        for (unsigned frame = 100; frame < 150; ++frame)
            assert(!headbutt_control_up.completed_frame(frame, 0));
    }
    input_actions = {{100,108,4}}; headbutt_control_up = {};
    native_headbutt_control = false;
    assert(input_state(0, RETRO_DEVICE_JOYPAD, 0, 4) == 1);
    assert(!headbutt_control_up.delivered);
    native_headbutt_control = true;
    assert(input_state(0, RETRO_DEVICE_JOYPAD, 0, 4) == 1);
    for (unsigned frame = 100; frame < 108; ++frame)
        assert(!headbutt_control_up.completed_frame(frame, 64));
    for (unsigned frame = 108; frame < 123; ++frame) {
        assert(!headbutt_control_up.completed_frame(frame, 0));
        assert(!headbutt_control_up.completed_frame(frame, 0)); // no duplicate credit
    }
    assert(headbutt_control_up.completed_frame(123, 0));
    assert(!headbutt_control_up.completed_frame(124, 1)); // held A resets release
    for (unsigned frame = 125; frame < 140; ++frame)
        assert(!headbutt_control_up.completed_frame(frame, 0));
    assert(headbutt_control_up.completed_frame(140, 0));
    assert(!headbutt_control_up.completed_frame(142, 0)); // gaps cannot earn credit
    input_frame = 151;
    assert(input_state(0, RETRO_DEVICE_JOYPAD, 0, 4) == 0);
}
static void checks_control_option() {
    std::map<std::string,std::string> options = {
        {"--native-headbutt-control","1"}, {"--profile-out","profile.json"},
        {"--acceptance-menu-routine","DoMenuJoypadLoop"}
    };
    std::map<std::string,std::vector<std::string>> effects = {
        {"--effect-span",{"0:ShakeHeadbuttTree","6:TryHeadbuttOW"}},
        {"--native-scene-field",{"0:wMapGroup:1:255"}}
    };
    validate_option(options, effects, true, false, true);
    for (unsigned bad = 0; bad < 8; ++bad) {
        auto o = options; auto e = effects;
        bool acceptance = true, adapter = false, menu = true;
        if (bad == 0) o["--native-headbutt-control"] = "0";
        if (bad == 1) o["--native-headbutt-control"] = "garbage";
        if (bad == 2) acceptance = false;
        if (bad == 3) adapter = true;
        if (bad == 4) o.erase("--profile-out");
        if (bad == 5) e.erase("--native-scene-field");
        if (bad == 6) e["--effect-span"] = {"0:ShakeHeadbuttTree"};
        if (bad == 7) { menu = false; o["--acceptance-menu-routine"] = "MobileMenuInput"; }
        bool rejected = false;
        try { validate_option(o, e, acceptance, adapter, menu); }
        catch (const std::runtime_error&) { rejected = true; }
        assert(rejected);
    }
}
int main(int argc, char** argv) {
    assert(argc == 5);
    checks_control_option();
    checks_returned_input_barrier();
    load_bindings(argv[1]);
    assert(bindings.count("wMenuCursorX") && bindings.count("hJoypadDown"));
    assert(!bindings.count("wUnlistedCursor"));
    assert(parses(argv[2]));
    assert(parses(argv[4], 14513));
    drives_branch(argv[2], false);
    drives_branch(argv[2], true);
    std::istringstream unknown("CHECKED_AFTER 16 wUnlistedCursor=2 A 8");
    bool rejected = false;
    try { parse_conditional_action(unknown, true, 3200, buttons,
''' + known + r'''); }
    catch (const std::runtime_error&) { rejected = true; }
    assert(rejected);
    bindings.clear();
    load_old_bindings(argv[1]);
    assert(!bindings.count("wMenuCursorX") && !parses(argv[2]));
    bindings.clear();
    load_bindings(argv[3]);
    assert(!bindings.count("wMenuCursorX") && !parses(argv[2]));
}
''')
            binary = root / 'frontend-bindings'
            subprocess.run(['c++', '-std=c++17', str(harness), '-o', str(binary)], check=True)
            symbols = [fixture]
            if os.environ.get('MOBILE_BINDING_TEST_SYMBOLS'):
                symbols.append(Path(os.environ['MOBILE_BINDING_TEST_SYMBOLS']))
            for symbol_file in symbols:
                missing = root / 'missing-cursor.sym'
                missing.write_text('\n'.join(
                    line for line in symbol_file.read_text().splitlines()
                    if not re.search(r'\s+wMenuCursorX\s*$', line)) + '\n')
                actual_script = root / 'actual-control.input'
                actual_rom = symbol_file.with_suffix('.gbc')
                actual_script.write_text(runtime.native_headbutt_control_inputs(
                    actual_rom, runtime.symbols(symbol_file), 11313, 0)
                    if actual_rom.exists() else inputs)
                subprocess.run([str(binary), str(symbol_file), str(script), str(missing),
                                str(actual_script)], check=True)

    def test_control_roles_and_measurement_bounds_preserve_native_chord_policy(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            rom, names = self.fixture(root)
            with patch.object(runtime, 'ROOT', root):
                text = runtime.native_headbutt_control_inputs(rom, names, 11313, 0)
                siblings = [runtime.native_headbutt_control_inputs(rom, names, 11313, i)
                            for i in (1, 2, 3)]
            rows = [row for row in text.splitlines() if not row.startswith('#')]
            self.assertEqual(len(rows), 250)
            self.assertTrue(rows[0].startswith('ON 11363 '))
            self.assertTrue(rows[1].startswith('ON 11388 '))
            self.assertEqual(sum(row.endswith(' UP 8') for row in rows), 1)
            self.assertEqual(sum(row.endswith(' LEFT 12') for row in rows), 1)
            self.assertTrue(all(not line or line.startswith('#')
                                for sibling in siblings for line in sibling.splitlines()))
            self.assertNotIn('wMobile', text)
            self.assertNotIn('vAcceptanceMeasuring', text)
            self.assertNotIn('SELECT', text)
            self.assertTrue(all(row.split()[-1] != '160' for row in rows))
            self.assertIn('vAcceptancePartyCount=1,vAcceptancePartyMove=29', text)
            self.assertIn('hInMenu=0', rows[0])
            self.assertIn('vAcceptanceHeadbuttControlUpReleased=1', rows[1])
            self.assertTrue(all('hJoypadDown=0' in row for row in rows
                                if row.endswith((' B 1', ' B 8'))))
            self.assertIn('wScriptRunning=0,wBattleMode=0,vAcceptanceNativeSceneReady=1,wMapStatus=2', rows[-1])

    def test_changed_native_script_fails_before_generating_inputs(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            rom, names = self.fixture(root)
            data = bytearray(rom.read_bytes())
            data[runtime.rom_offset(names['AutoHeadbuttScript.no_battle']) - 3] = 255
            rom.write_bytes(data)
            with patch.object(runtime, 'ROOT', root):
                with self.assertRaisesRegex(RuntimeError, 'script tail changed'):
                    runtime.native_headbutt_recovery_inputs(rom, names)


class DurableRenderLineBinding(unittest.TestCase):
    def test_pinned_window_member_and_old_hook_rejection(self):
        from test_runtime import ObserverHooks
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            ObserverHooks().source(root)
            runtime.extend_profile(root)
            lcd = root / 'gb_core/lcd.cpp'
            self.assertIn('acceptance_profile_render_line(ref_gb, scanline, now_win_line);', lcd.read_text())
            snapshot = {path: path.read_bytes() for path in root.rglob('*') if path.is_file()}
            runtime.extend_profile(root)
            self.assertEqual(snapshot, {path: path.read_bytes() for path in root.rglob('*') if path.is_file()})
            lcd.write_text(lcd.read_text().replace('gb*, unsigned, int', 'gb*, unsigned')
                           .replace('ref_gb, scanline, now_win_line', 'ref_gb, scanline'))
            before = {path: path.read_bytes() for path in root.rglob('*') if path.is_file()}
            with self.assertRaisesRegex(RuntimeError, 'Incompatible prior LCD observer hook'):
                runtime.extend_profile(root)
            self.assertEqual(before, {path: path.read_bytes() for path in root.rglob('*') if path.is_file()})


if __name__ == '__main__':
    unittest.main()
