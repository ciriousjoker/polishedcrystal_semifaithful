"""Host-only scenario selection and portable-save integrity checks."""
import hashlib
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

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
