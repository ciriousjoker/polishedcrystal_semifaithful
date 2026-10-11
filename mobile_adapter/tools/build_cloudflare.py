#!/usr/bin/env python3
"""Build the pinned local Worker bundle without cloud credentials."""
import os
from pathlib import Path
import shutil
import subprocess

root = Path(__file__).resolve().parents[1] / "server/cloudflare"
tool = os.environ.get("MOBILE_WORKER_BUILD") or shutil.which("worker-build")
cache_tool = Path("/Volumes/Storage/Cache/mobileadapter/cloudflare-tools/bin/worker-build")
if not tool and cache_tool.is_file():
    tool = str(cache_tool)
if not tool:
    raise SystemExit("Install worker-build 0.8.7 with cargo +1.98.0 install worker-build --version 0.8.7 --locked")
if subprocess.check_output([tool, "--version"], text=True).strip() != "0.8.7":
    raise SystemExit("worker-build must be version 0.8.7")
# The SDK's experimental module-target panic wrapper currently fails for this
# target with "externref table required for catch wrappers". Use its supported
# classic shim until a pinned SDK update fixes that bundler path.
subprocess.run([tool, "--release", "--locked", "--no-panic-recovery"], cwd=root, check=True)
