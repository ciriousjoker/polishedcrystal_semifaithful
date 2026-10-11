"""Map conflict finding must retain protected actors and respect time masks."""
import hashlib
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import audit_native_leases
from audit_native_scanlines import classify, pair_overlap


def actor(x, y, movement=6, radius=0, time=15, kind=3, command=0):
    return bytes([1, y + 4, x + 4, movement, radius, 0, time, kind, command, 0, 0, 255, 255])


class MapScanlineTests(unittest.TestCase):
    def test_adjacent_required_trainers_are_flagged(self):
        first, second = actor(6, 12), actor(7, 12)
        self.assertEqual(pair_overlap(first, second), "initial-row")
        self.assertEqual(classify(first, 2, [], []), "required-trainer")

    def test_disjoint_time_masks_do_not_invent_a_pair(self):
        self.assertIsNone(pair_overlap(actor(6, 12, time=1), actor(7, 12, time=2)))

    def test_wandering_envelope_can_create_a_new_conflict(self):
        self.assertEqual(pair_overlap(actor(6, 12, movement=2, radius=0x10), actor(7, 14)), "movement-envelope")

    def test_static_nonoverlap_and_distant_rows_are_not_flagged(self):
        self.assertIsNone(pair_overlap(actor(6, 12), actor(7, 14)))
        self.assertIsNone(pair_overlap(actor(0, 12), actor(10, 12)))

    def test_replaceability_is_a_reviewed_permission_not_inferred_from_appearance(self):
        self.assertEqual(classify(actor(6, 12, kind=0), 2, [], []), "protected-needs-review")
        self.assertEqual(classify(actor(6, 12, kind=0), 2, [2], []), "replaceable")
        self.assertEqual(classify(actor(6, 12, kind=5), 2, [], ["pokemart"]), "required-shop")


class MapSourceGuardTests(unittest.TestCase):
    def test_protected_map_geometry_changes_require_review(self):
        # This map has no approved lease candidates. Its geometry still matters.
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            sources = ("maps/Protected.asm", "maps/Shared.ablk", "data/tilesets/shared_collision.asm")
            for name in sources:
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(b"original")
            manifest = root / "policy.json"
            manifest.write_text(json.dumps({"generator_sha256": hashlib.sha256(
                Path(audit_native_leases.__file__).read_bytes()).hexdigest(),
                "source_sha256": {name: hashlib.sha256((root / name).read_bytes()).hexdigest() for name in sources},
                "unreferenced_constants": []}))
            with patch.object(audit_native_leases, "REPO", root), patch.object(
                    audit_native_leases, "MANIFEST", manifest), patch("sys.argv", ["audit", "--check-sources"]):
                audit_native_leases.main()
                for name in sources:
                    with self.subTest(source=name):
                        (root / name).write_bytes(b"changed")
                        with self.assertRaisesRegex(ValueError, "Native lease source changed"):
                            audit_native_leases.main()
                        (root / name).write_bytes(b"original")


if __name__ == "__main__":
    unittest.main()
