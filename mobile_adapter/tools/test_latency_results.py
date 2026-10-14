"""Strict failure-oracle tests using callback observations, without emulators."""
import copy
import json
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest

from probe_lobby import latency_results


class LatencyResultsTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.output = Path(self.directory.name)
        self.args = SimpleNamespace(output=self.output, players=4, test_one_way_delay_ms=0,
                                    declared_upstream_one_way_delay_ms=0)
        self.events = [{"frame": 3650 + index * 90, "button": 7 if index % 2 == 0 else 6,
                        "wall_us": 1000000 + index * 1000000} for index in range(20)]
        self.rows = [[] for _ in range(4)]
        x = 100
        for event in self.events:
            direction = 1 if event["button"] == 7 else -1
            for client in range(4):
                self.rows[client].append(self.row(client, event["wall_us"] - 10000, x))
                self.rows[client].append(self.row(client, event["wall_us"] + (20000 if client == 0 else 70000), x + direction))
            x += direction

    def row(self, client, timestamp, x):
        prefix = "vVideoPeer1" if client else "vVideoLocal"
        values = {prefix + "Complete": 1, prefix + "Partial": 0, prefix + "Coherent": 1,
                  prefix + "Quads": 4, prefix + "Left": x, prefix + "Facing": 8,
                  prefix + "TileHash": 123, prefix + "PaletteHash": 456}
        if client:
            values.update({prefix + "Id": 42, prefix + "Generation": 1, prefix + "Instance": 2,
                           prefix + "Epoch": 3, prefix + "MapGroup": 24, prefix + "MapNumber": 5,
                           prefix + "Ready": 1, prefix + "DrawX": x, prefix + "DrawY": 80})
        else:
            values.update(vVideoLocalX=x, vVideoLocalY=80)
        return {"video_wall_us": timestamp, "wall_us": timestamp + 999999, "symbols": values}

    def result(self):
        for client, rows in enumerate(self.rows):
            directory = self.output / str(client)
            directory.mkdir(exist_ok=True)
            (directory / "trace.jsonl").write_text("\n".join(map(json.dumps, rows)))
        (self.output / "0/input-events.jsonl").write_text("\n".join(map(json.dumps, self.events)))
        return latency_results(self.args, [{"wMobilePlayerId": 42}])

    def test_all_sixty_samples_use_callback_time(self):
        result = self.result()
        self.assertEqual(result["samples"], 60)
        self.assertEqual(result["p95_ms"], 50)
        self.assertEqual(result["input_to_remote_p95_ms"], 70)

    def test_missing_and_partial_baselines_are_not_skipped(self):
        for suffix, value in (("Complete", 0), ("Partial", 1), ("Coherent", 0), ("Ready", 0)):
            with self.subTest(suffix=suffix):
                original = copy.deepcopy(self.rows)
                self.rows[1][0]["symbols"]["vVideoPeer1" + suffix] = value
                with self.assertRaises(ValueError):
                    self.result()
                self.rows = original

    def test_invalid_changed_samples_fail(self):
        for suffix, value in (("Complete", 0), ("Partial", 1), ("Coherent", 0), ("Ready", 0),
                              ("Id", 99), ("Generation", 9), ("Instance", 9), ("Epoch", 9),
                              ("MapNumber", 9), ("TileHash", 0), ("PaletteHash", 0), ("Quads", 0)):
            with self.subTest(suffix=suffix):
                original = copy.deepcopy(self.rows)
                self.rows[1][1]["symbols"]["vVideoPeer1" + suffix] = value
                with self.assertRaises(ValueError):
                    self.result()
                self.rows = original

    def test_negative_latency_fails(self):
        self.rows[1][1]["video_wall_us"] = self.events[0]["wall_us"] + 10000
        with self.assertRaisesRegex(ValueError, "Negative"):
            self.result()

    def test_missing_local_motion_fails(self):
        self.rows[0][1]["symbols"]["vVideoLocalX"] = 100
        self.rows[0][2]["symbols"]["vVideoLocalX"] = 100
        with self.assertRaisesRegex(ValueError, "Missing local motion"):
            self.result()

    def test_unsettled_baseline_fails(self):
        self.rows[1][0]["symbols"]["vVideoPeer1DrawY"] = 81
        with self.assertRaisesRegex(ValueError, "Unsettled"):
            self.result()

    def test_missing_callback_timestamp_fails(self):
        del self.rows[1][0]["video_wall_us"]
        with self.assertRaisesRegex(ValueError, "timestamp"):
            self.result()

    def test_wrong_direction_fails(self):
        self.rows[1][1]["symbols"]["vVideoPeer1Left"] = 99
        with self.assertRaisesRegex(ValueError, "Unrelated remote"):
            self.result()

    def test_missing_recipient_motion_fails(self):
        self.rows[1][1]["symbols"]["vVideoPeer1Left"] = 100
        self.rows[1][2]["symbols"]["vVideoPeer1Left"] = 100
        with self.assertRaisesRegex(ValueError, "No directional"):
            self.result()


if __name__ == "__main__":
    unittest.main()
