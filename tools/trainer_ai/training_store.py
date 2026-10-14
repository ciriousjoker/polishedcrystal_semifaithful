#!/usr/bin/env python3
"""Bounded, single-host shard storage. No automatic deletion or reclamation."""
import contextlib
import fcntl
import hashlib
import json
import os
from pathlib import Path
import uuid

DEFAULT_ROOT = Path('/Volumes/Shared/polished_ai/training_data')
DEFAULT_QUOTA = 100_000_000_000


class QuotaExceeded(RuntimeError):
    pass


class ShardStore:
    """All cooperating writers hold one exclusive lock for the complete write.

    The allocation unit is a conservative floor, not a filesystem quota. Other
    processes must not write into this managed directory. Local flock and atomic
    rename are required; distributed/network filesystem guarantees are absent.
    """
    def __init__(self, root=DEFAULT_ROOT, quota=DEFAULT_QUOTA,
                 allocation_unit=131072, shard_bytes=8 * 1024 * 1024):
        self.root = Path(root)
        if quota <= 0 or allocation_unit <= 0 or shard_bytes <= 0:
            raise ValueError('positive limits required')
        self.quota, self.unit, self.shard_bytes = quota, allocation_unit, shard_bytes
        if self.root.is_symlink():
            raise ValueError('symlink store root is unsupported')
        self.root.mkdir(parents=True, exist_ok=True)
        with self.lock():
            config = self.root / 'config.json'
            expected = dict(version=1, quota_bytes=quota, allocation_unit=allocation_unit,
                            shard_bytes=shard_bytes)
            if config.exists():
                if json.loads(config.read_text()) != expected:
                    raise ValueError('store configuration differs')
            else:
                if any(p.name != '.writer.lock' for p in self.root.iterdir()):
                    raise ValueError('refusing to adopt a nonempty unmanaged directory')
                self._atomic_json(config, expected)
                self._atomic_json(self.root / 'manifest.json', dict(version=1, shards=[]))
            if self.occupancy() > quota:
                raise QuotaExceeded('existing occupancy exceeds quota')

    @contextlib.contextmanager
    def lock(self):
        lock_path = self.root / '.writer.lock'
        if lock_path.is_symlink():
            raise ValueError('symlink lock is unsupported')
        with lock_path.open('a+b') as handle:
            fcntl.flock(handle.fileno(), fcntl.LOCK_EX)
            try:
                # Refresh once per transaction to include crash residue and
                # changes made by previous cooperating writers.
                self._charged = self.occupancy()
                yield
            finally:
                self.__dict__.pop('_charged', None)
                fcntl.flock(handle.fileno(), fcntl.LOCK_UN)

    def allocated(self, size):
        return ((size + self.unit - 1) // self.unit) * self.unit

    def occupancy(self):
        """Includes directories, unknown files, metadata and abandoned temp files."""
        total = 0
        for path in [self.root, *self.root.rglob('*')]:
            st = path.lstat()
            if path.is_symlink() or not (path.is_dir() or path.is_file()):
                raise ValueError('symlinks and special files are unsupported')
            total += max(getattr(st, 'st_blocks', 0) * 512,
                         self.allocated(st.st_size))
        return total

    def _reserve(self, additional):
        if self._charged + additional > self.quota:
            raise QuotaExceeded('storage ceiling would be exceeded')

    def _atomic_json(self, target, value):
        data = (json.dumps(value, sort_keys=True) + '\n').encode()
        # Account for a complete second metadata copy before replacement.
        self._reserve(self.allocated(len(data)) + self.unit)
        old_charge = 0
        if target.exists():
            st = target.stat()
            old_charge = max(st.st_blocks * 512, self.allocated(st.st_size))
        temporary = self.root / ('.metadata-' + uuid.uuid4().hex + '.tmp')
        with temporary.open('xb') as out:
            out.write(data)
            out.flush()
            os.fsync(out.fileno())
        # Keep directory-growth slack charged until the next locked rescan.
        self._charged += self.allocated(len(data)) + self.unit
        os.replace(temporary, target)
        self._charged -= old_charge

    def write_shard(self, records, provenance):
        """Stream byte records into one bounded shard, retaining failed temp data.

        Each record is length-prefixed. Callers own schema and compression.
        Provenance must identify source/rules/model/scenario/seeds and outcome
        scope; storage does not certify simulator fidelity or training strength.
        """
        required = {'source_digest', 'rules_profile', 'model_digest',
                    'scenario_suite', 'seed', 'objective'}
        if not required <= provenance.keys():
            raise ValueError('missing replay provenance fields')
        with self.lock():
            manifest = json.loads((self.root / 'manifest.json').read_text())
            name = 'shard-' + uuid.uuid4().hex
            temporary = self.root / (name + '.tmp')
            sealed = self.root / (name + '.sealed')
            header = (json.dumps(dict(version=1, provenance=provenance),
                                 sort_keys=True) + '\n').encode()
            # Allow one directory allocation unit for the new entry as well.
            self._reserve(self.allocated(len(header)) + self.unit)
            size = 0
            digest = hashlib.sha256()
            count = 0
            with temporary.open('xb') as out:
                self._charged += self.unit  # Conservative directory-entry slack.
                for chunk in self._chunks(header, records):
                    next_size = size + len(chunk)
                    if next_size > self.shard_bytes:
                        raise QuotaExceeded('shard size limit exceeded')
                    # Reserve conservative allocated growth before each write.
                    growth = self.allocated(next_size) - self.allocated(size)
                    if growth:
                        self._reserve(growth)
                        self._charged += growth
                    out.write(chunk)
                    digest.update(chunk)
                    size = next_size
                    count += 1
                out.flush()
                os.fsync(out.fileno())
            entry = dict(file=sealed.name, bytes=size, records=count - 1,
                         sha256=digest.hexdigest(), provenance=provenance)
            manifest['shards'].append(entry)
            # Reserve manifest replacement before sealing. No manifest entry may
            # point at an incomplete file. Crashes can leave unlisted sealed data.
            encoded_size = len((json.dumps(manifest, sort_keys=True) + '\n').encode())
            self._reserve(self.allocated(encoded_size))
            os.replace(temporary, sealed)
            self._atomic_json(self.root / 'manifest.json', manifest)
            return entry

    @staticmethod
    def _chunks(header, records):
        yield header
        for record in records:
            if not isinstance(record, bytes):
                raise TypeError('records must be bytes')
            yield len(record).to_bytes(8, 'little') + record
