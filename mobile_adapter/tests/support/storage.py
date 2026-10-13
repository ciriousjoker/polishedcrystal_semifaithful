"""Reclaim only identified, inactive generated acceptance runs on low space."""
import contextlib
import fcntl
import json
from pathlib import Path
import shutil
import subprocess
import time

RESERVE = 10 * 1024 ** 3
HEADROOM = 512 * 1024 ** 2


@contextlib.contextmanager
def active_run(output):
    output.mkdir(parents=True, exist_ok=True)
    with (output / ".active.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_SH)
        marker = output / ".acceptance-run.json"
        if not marker.exists():
            marker.write_text(json.dumps({"kind": "mobile-acceptance", "created": time.time()}))
        yield


def identified_run(path):
    if path.is_symlink() or not path.is_dir():
        return False
    marker = path / ".acceptance-run.json"
    if marker.exists():
        try:
            return json.loads(marker.read_text()).get("kind") == "mobile-acceptance"
        except (OSError, ValueError):
            return False
    # Legacy runs from this suite have both independent provenance records.
    return (path.name.startswith("acceptance-suite-") and
            (path / "source-fingerprint.txt").is_file() and
            (path / "prepared/manifest.json").is_file())


def ensure_free_space(cache, current, reserve=RESERVE, headroom=HEADROOM, free=None):
    cache, current = Path(cache).resolve(), Path(current).resolve()
    cache.mkdir(parents=True, exist_ok=True)
    free = free or (lambda: shutil.disk_usage(cache).free)
    required = reserve + headroom
    deleted = []
    with (cache / ".acceptance-cleanup.lock").open("a") as cleanup:
        fcntl.flock(cleanup, fcntl.LOCK_EX)
        if free() >= required:
            return deleted
        candidates = sorted((p for p in cache.iterdir() if identified_run(p) and p.resolve() != current),
                            key=lambda p: p.stat().st_mtime)
        for path in candidates:
            if not (path / ".acceptance-run.json").exists():
                # Old backends predate the OS lease. Their command line includes
                # the output directory; fail closed if the process list is unavailable.
                try:
                    commands = subprocess.check_output(["ps", "-axo", "command="], text=True)
                except (OSError, subprocess.SubprocessError):
                    continue
                if str(path) in commands:
                    continue
            with (path / ".active.lock").open("a") as active:
                try:
                    fcntl.flock(active, fcntl.LOCK_EX | fcntl.LOCK_NB)
                except BlockingIOError:
                    continue
                print(f"Storage reserve: removing inactive acceptance run {path}", flush=True)
                shutil.rmtree(path)
                deleted.append(path)
            if free() >= required:
                return deleted
        raise RuntimeError(f"Storage reserve cannot be restored safely: {free() / 1024**3:.2f} GiB free; "
                           f"need {required / 1024**3:.2f} GiB. Active runs and toolchains were preserved.")
