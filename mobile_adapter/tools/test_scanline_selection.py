"""Exercise actual selection limits, including incomplete and offscreen figures."""
import unittest
from scanline_selection import inspect_native_frame, missing_final_peers, select_scanlines


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

    def test_unknown_or_truncated_observations_cannot_pass(self):
        with self.assertRaises(ValueError):
            select_scanlines([0] * 39)
        with self.assertRaises(ValueError):
            select_scanlines([256] * 40)
        with self.assertRaises(KeyError):
            inspect_native_frame({})


if __name__ == "__main__":
    unittest.main()
