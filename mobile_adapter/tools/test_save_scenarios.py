"""Host-only scenario selection and portable-save integrity checks."""
import hashlib
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from probe_emulator import read_symbols, validate_battery

import dev_session
import save_scenarios as scenes


class ScenarioContracts(unittest.TestCase):
    def test_all_committed_scenarios_have_four_distinct_validated_batteries(self):
        entries = scenes.catalog()
        self.assertEqual([e["id"] for e in entries], ["bedroom", "game-corner", "goldenrod-city"])
        for entry in entries:
            hashes = set()
            for avatar in scenes.AVATARS:
                record = entry["saves"][avatar]
                data = (scenes.ROOT / record["file"]).read_bytes()
                self.assertEqual(len(data), 32768)
                self.assertEqual(hashlib.sha256(data).hexdigest(), record["sha256"])
                self.assertEqual(record["player_gender"], scenes.AVATARS.index(avatar))
                self.assertEqual(record["checksums"]["s"], record["checksums"]["sBackup"])
                hashes.add(record["sha256"])
            self.assertEqual(len(hashes), 4)

    def test_session_saves_differ_only_in_figure_id_and_checksums(self):
        symbols = read_symbols(dev_session.REPO / "polishedcrystal-mobile-3.2.3.sym")
        def offset(name):
            bank, address = symbols[name]
            return bank * 8192 + address - 0xa000
        allowed = set()
        for prefix in ("s", "sBackup"):
            start = offset(prefix + "PlayerData")
            allowed.update(range(start, start + 2))
            allowed.update(start + symbols[name][1] - symbols["wPlayerData"][1]
                           for name in scenes.APPEARANCE_FIELDS)
            checksum = offset(prefix + "Checksum")
            allowed.update(range(checksum, checksum + 2))
        with tempfile.TemporaryDirectory() as folder:
            for entry in scenes.catalog():
                source = scenes.battery(entry, "chris", symbols).read_bytes()
                for index, avatar in enumerate(scenes.AVATARS):
                    path = Path(folder) / f"{entry['id']}-{avatar}.sav"
                    scenes.session_battery(entry, avatar, symbols, path, 10001 + index)
                    result = path.read_bytes()
                    changed = {i for i, (a, b) in enumerate(zip(source, result)) if a != b}
                    self.assertTrue(changed <= allowed)
                    validate_battery(path, symbols)
                    for prefix in ("s", "sBackup"):
                        start = offset(prefix + "PlayerData")
                        self.assertEqual(result[start:start + 2], (10001 + index).to_bytes(2, "big"))
                        self.assertEqual(result[start + 2], index)

    def test_duplicate_trainer_ids_are_supported_by_fixture_generation(self):
        symbols = read_symbols(dev_session.REPO / "polishedcrystal-mobile-3.2.3.sym")
        with tempfile.TemporaryDirectory() as folder:
            for avatar in scenes.AVATARS:
                path = scenes.session_battery(scenes.choose("bedroom"), avatar, symbols,
                    Path(folder) / f"{avatar}.sav", 12345)
                validate_battery(path, symbols)
                self.assertEqual(path.read_bytes()[0x2008:0x200a], bytes((0x30, 0x39)))

    def test_prompt_retries_bad_choice_and_accepts_number_or_id(self):
        replies = iter(["0", "four", "2"])
        with patch("builtins.print"):
            self.assertEqual(scenes.choose(interactive=True, ask=lambda _: next(replies))["id"], "game-corner")
            self.assertEqual(scenes.choose(interactive=True, ask=lambda _: "goldenrod-city")["id"], "goldenrod-city")

    def test_empty_prompt_selects_bedroom_and_explicit_choice_never_prompts(self):
        with patch("builtins.print"):
            self.assertEqual(scenes.choose(interactive=True, ask=lambda _: "")["id"], "bedroom")
        self.assertEqual(scenes.choose("game-corner", interactive=False, ask=lambda _: self.fail("Unexpected prompt"))["id"], "game-corner")

    def test_unattended_start_requires_explicit_known_scenario(self):
        with self.assertRaisesRegex(ValueError, "--scenario"):
            scenes.choose(interactive=False)
        with self.assertRaisesRegex(ValueError, "Unknown scenario"):
            scenes.choose("unknown", interactive=False)

    def test_catalog_cannot_escape_its_save_directory(self):
        entry = {"saves": {"chris": {"file": "../outside.sav", "sha256": ""}}}
        with self.assertRaisesRegex(ValueError, "inside"):
            scenes.battery(entry, "chris", {})

    def test_start_snapshot_must_match_selected_scene_and_character(self):
        entry = scenes.choose("game-corner")
        expected = scenes.expected(entry, "crys")
        state = {"hCrashCode": 0, "hCGB": 1, "hInMenu": 0, "wMapStatus": 2,
                 "wScriptRunning": 0, "wMobileOwner": 0, **expected}
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "start.json"
            path.write_text(json.dumps({"symbols": state}))
            dev_session.verify_idle(path, expected)
            for key in ("wMapNumber", "wXCoord", "wPlayerGender"):
                wrong = {**state, key: state[key] + 1}
                path.write_text(json.dumps({"symbols": wrong}))
                with self.assertRaisesRegex(ValueError, key):
                    dev_session.verify_idle(path, expected)


if __name__ == "__main__":
    unittest.main()
