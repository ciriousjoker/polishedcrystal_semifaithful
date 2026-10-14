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
                inputs = runtime.native_headbutt_recovery_inputs(rom, names)
            script = root / 'recovery.input'
            script.write_text(inputs)
            fixture = root / 'realistic.sym'
            fixture.write_text(
                '; RGBDS symbol export\n'
                '00:ff96 hJoypadDown\n00:ffb5 hScriptBank\n00:ffb6 hScriptPos\n'
                '00:ce51 wMenuCursorX\n00:ce52 wMenuCursorY\n'
                '00:d438 wScriptRunning\n01:d226 wBattleMode\n'
                '00:ce59 wBattleMenuFlags\n00:c100 wUnlistedCursor\n')
            prelude = ('#include "' + str(header) + '"\n#include <cassert>\n'
                       '#include <fstream>\n#include <set>\n' + binding + '\n'
                       'static std::map<std::string, Binding> bindings;\n'
                       'static bool acceptance_enabled = true;\n'
                       'static void fail(const std::string& message) '
                       '{ throw std::runtime_error(message); }\n')
            harness = root / 'frontend-bindings.cpp'
            harness.write_text(prelude + loader + old_loader +
                'static bool parses(const std::string& path) {\n'
                '  std::ifstream stream(path); std::string line;\n'
                '  try { while (std::getline(stream, line)) {\n'
                '    std::istringstream row(line.substr(9));\n'
                '    parse_conditional_action(row, true, 3200, {{"A",1},{"B",2}}, ' + known + ');\n'
                '  } } catch (const std::runtime_error&) { return false; }\n'
                '  return true;\n}\n' + r'''
int main(int argc, char** argv) {
    assert(argc == 4);
    load_bindings(argv[1]);
    assert(bindings.count("wMenuCursorX") && bindings.count("hJoypadDown"));
    assert(!bindings.count("wUnlistedCursor"));
    assert(parses(argv[2]));
    std::istringstream unknown("CHECKED_AFTER 16 wUnlistedCursor=2 A 8");
    bool rejected = false;
    try { parse_conditional_action(unknown, true, 3200, {{"A",1}},
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
                subprocess.run([str(binary), str(symbol_file), str(script), str(missing)], check=True)

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
