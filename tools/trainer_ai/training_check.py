#!/usr/bin/env python3
"""Small storage boundary checks; no model training or large allocations."""
import argparse
import json
from pathlib import Path
import tempfile
from training_store import QuotaExceeded, ShardStore

PROVENANCE = dict(source_digest='fixture', rules_profile='storage-only',
                  model_digest='none', scenario_suite='storage-boundary', seed=1,
                  objective='whole-fight')


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--artifact-dir', required=True)
    args = parser.parse_args()
    Path(args.artifact_dir).mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(dir=args.artifact_dir) as temporary:
        root = Path(temporary) / 'data'
        store = ShardStore(root, quota=1_000_000, allocation_unit=4096,
                           shard_bytes=20000)
        scans = [0]
        original_occupancy = store.occupancy

        def counted_occupancy():
            scans[0] += 1
            return original_occupancy()

        store.occupancy = counted_occupancy
        bulk = store.write_shard((b'x' * 8 for _ in range(1000)), PROVENANCE)
        assert bulk['records'] == 1000 and scans[0] == 1, scans
        first = store.write_shard([b'known existing data'], PROVENANCE)
        original = (root / first['file']).read_bytes()
        manifest_before = (root / 'manifest.json').read_bytes()
        try:
            store.write_shard([b'x' * 21000], PROVENANCE)
            raise AssertionError('oversize shard accepted')
        except QuotaExceeded:
            pass
        assert (root / first['file']).read_bytes() == original
        assert (root / 'manifest.json').read_bytes() == manifest_before
        residues = list(root.glob('*.tmp'))
        assert residues and store.occupancy() < store.quota
        baseline = store.occupancy()
        # Simulate interruption: an unlisted sealed file and abandoned temp file.
        (root / 'crashed.sealed').write_bytes(b'z' * 8192)
        (root / 'interrupted.tmp').write_bytes(b'z' * 8192)
        assert store.occupancy() >= baseline + 16384
        store.quota = store.occupancy() + 4096
        try:
            store.write_shard([b'new'], PROVENANCE)
            raise AssertionError('quota boundary accepted')
        except QuotaExceeded:
            pass
        assert (root / first['file']).read_bytes() == original
        assert (root / 'manifest.json').read_bytes() == manifest_before
        assert store.occupancy() <= store.quota
        (root / 'foreign-link').symlink_to(root / first['file'])
        try:
            store.occupancy()
            raise AssertionError('symlink accepted')
        except ValueError:
            pass
    print(json.dumps(dict(storage_checks='passed',
                          checks=['one occupancy scan for 1000 small records', 'bounded shard', 'existing data preserved',
                                  'interrupted occupancy', 'quota rejection',
                                  'unknown sealed file occupancy', 'symlink rejection'])))


if __name__ == '__main__':
    main()
