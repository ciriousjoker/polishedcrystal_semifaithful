"""Exercise actual selection limits, including incomplete and offscreen figures."""
import unittest
import random
from scanline_selection import inspect_native_frame, missing_final_peers, select_scanlines, virtual_temporal_report


def figures(count, y=64):
    return [value for _ in range(count) for value in (y, y, y + 8, y + 8)] + [0] * (40 - count * 4)


class ScanlineSelectionTests(unittest.TestCase):
    def test_four_players_and_one_npc_fit_exactly(self):
        demand, _ = select_scanlines(figures(5))
        self.assertEqual(max(demand), 10)

    def test_second_npc_is_dropped_as_rows_not_a_whole_game_object(self):
        demand, selected = select_scanlines(figures(6))
        self.assertEqual(demand[48], 12)
        self.assertTrue({0, 1, 4, 5, 8, 9, 12, 13}.issubset(selected[48]))
        self.assertFalse({20, 21} & selected[48])

    def test_eight_pixel_offset_overlaps_only_half_a_figure(self):
        ys = figures(5)
        ys[20:24] = [72, 72, 80, 80]
        demand, _ = select_scanlines(ys)
        self.assertEqual((demand[48], demand[56], demand[64]), (10, 12, 2))

    def test_hidden_y_frees_selection_but_horizontal_hiding_does_not(self):
        demand, selected = select_scanlines([64] * 10 + [64] + [0] * 29)
        self.assertEqual(demand[48], 11)
        self.assertNotIn(10, selected[48])  # Selection never consults X.
        demand, _ = select_scanlines([64] * 10 + [0] * 30)
        self.assertEqual(max(demand), 10)

    def test_visible_edge_rows_and_sixteen_pixel_mode(self):
        demand, _ = select_scanlines([8] + [0] * 39, height=16)
        self.assertEqual(demand[:9], [1] * 8 + [0])
        self.assertEqual(max(select_scanlines([0] * 40)[0]), 0)

    def test_native_peer_selection_fails_even_with_complete_oam_membership(self):
        state = {f"vOAM{index}Y": y for index, y in enumerate(figures(6))}
        state.update(wMobilePeerCount=1, wMobilePeer1OAMStart=80, vOAMMaxScanline=12)
        result = inspect_native_frame(state)
        self.assertEqual(result["dropped_peers"], [{"peer": 1, "object_rows_not_selected": 32}])

    def test_reported_peer_visibility_cannot_pass_with_hidden_oam(self):
        state = {"wMobileOnline": 1, "wMobilePeerCount": 2,
                 "wMobilePeer1Visible": 1, "wMobilePeer1OAMStart": 48, "vMobilePeer1OAM0": 160,
                 "wMobilePeer2Visible": 0, "wMobilePeer2OAMStart": 160, "vMobilePeer2OAM0": 160}
        self.assertEqual(missing_final_peers(state), [1])
        state["vMobilePeer1OAM0"] = 76
        self.assertEqual(missing_final_peers(state), [])
        state["wMobilePeer1OAMStart"] = 160
        self.assertEqual(missing_final_peers(state), [1])
        state["wMobileOnline"] = 0
        self.assertEqual(missing_final_peers(state), [])

    def test_temporal_visibility_uses_hardware_and_bounds_continuous_absence(self):
        def rows(visible_frames):
            return [{"frame": frame, "symbols": {
                "wMobileOnline": 1, "wMobilePeerCount": 1, "wMobileRenderedCount": 0,
                "wMobilePeer1Visible": 1, "wMobilePeer1NativeStage": 7,
                "wMobilePeer1Id": 12, "wMobilePeer1Generation": 1,
                "wMobilePeer1OAMStart": 16 if frame in visible_frames else 160,
                "vMobilePeer1OAM0": 64 if frame in visible_frames else 160,
            }} for frame in range(12)]
        report = virtual_temporal_report(rows({0, 3, 6, 9}))
        self.assertTrue(report["passed"])
        self.assertEqual(report["maximum_absent_frames"], 2)
        report = virtual_temporal_report(rows({0, 9}))
        self.assertFalse(report["passed"])
        self.assertEqual(report["maximum_absent_frames"], 8)
        self.assertEqual(report["witnesses"][0]["frame"], 4)

    def test_temporal_segments_reset_on_scene_exit_or_identity_replacement(self):
        base = {"wMobileOnline": 1, "wMobilePeerCount": 1,
                "wMobilePeer1Visible": 1, "wMobilePeer1NativeStage": 7,
                "wMobilePeer1Id": 1, "wMobilePeer1Generation": 1,
                "wMobilePeer1OAMStart": 160, "vMobilePeer1OAM0": 160}
        rows = [{"frame": frame, "symbols": dict(base)} for frame in range(8)]
        rows[3]["symbols"]["wMobilePeer1Visible"] = 0
        for row in rows[5:]:
            row["symbols"]["wMobilePeer1Generation"] = 2
        self.assertTrue(virtual_temporal_report(rows)["passed"])

    def test_frozen_start_menu_occlusion_resets_only_the_paused_segment(self):
        base = {"wMobileOnline": 1, "wMobilePeerCount": 1,
                "wMobilePeer1Visible": 1, "wMobilePeer1NativeStage": 7,
                "wMobilePeer1Id": 1, "wMobilePeer1Generation": 1,
                "wMobilePeer1OAMStart": 160, "vMobilePeer1OAM0": 160,
                "wScriptRunning": 0}
        rows = [{"frame": frame, "symbols": dict(base)} for frame in range(12)]
        for row in rows[3:9]:
            row['symbols']['wScriptRunning'] = 255
        self.assertTrue(virtual_temporal_report(rows)['passed'])
        rows.extend({'frame': frame, 'symbols': dict(base)} for frame in range(12, 14))
        self.assertFalse(virtual_temporal_report(rows)['passed'])

    def test_interval_proof_matches_clipped_hardware_scanlines(self):
        # Independent oracle: actual 8x8 OAM selection across 144 screen rows.
        # Include partial figures at either viewport edge and equal/nearby tops.
        rng = random.Random(17423)
        populations = [[1] * 6, [159] * 6, [1, 16, 16, 16, 16, 16],
                       [1, 17, 17, 17, 17, 17], [64] * 5, [64] * 6]
        populations += [[rng.randrange(1, 160) for _ in range(rng.randrange(1, 11))]
                        for _ in range(1200)]
        for tops in populations:
            hardware = [y for top in tops for y in (top, top, top + 8, top + 8)]
            hardware += [0] * (40 - len(hardware))
            ordered = sorted(tops)
            interval_overflow = any(ordered[i + 5] - ordered[i] < 16
                                    for i in range(len(ordered) - 5))
            self.assertEqual(interval_overflow, max(select_scanlines(hardware)[0]) > 10, tops)

    def test_unknown_or_truncated_observations_cannot_pass(self):
        with self.assertRaises(ValueError):
            select_scanlines([0] * 39)
        with self.assertRaises(ValueError):
            select_scanlines([256] * 40)
        with self.assertRaises(KeyError):
            inspect_native_frame({})


if __name__ == "__main__":
    unittest.main()
