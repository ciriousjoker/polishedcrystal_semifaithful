#!/usr/bin/env python3
"""Build the native Rust engine and package it in the macOS Flutter shell."""
import argparse
import os
from pathlib import Path
import platform
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[2]
APP = ROOT / "mobile_adapter/bridge_app"

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--flutter", default=shutil.which("flutter"))
    parser.add_argument("--cargo", default=shutil.which("cargo") or "/Volumes/Storage/Cache/cargo/bin/cargo")
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--run", action="store_true")
    args = parser.parse_args()
    if not args.flutter:
        parser.error("Install Flutter or pass --flutter /path/to/flutter")
    profile = "debug" if args.debug else "release"
    command = [args.cargo, "build", "--locked", "--manifest-path", str(ROOT / "mobile_adapter/server/Cargo.toml"),
               "-p", "mobile-bridge", "--lib"]
    if not args.debug:
        command += ["--release"]
    subprocess.run(command, cwd=ROOT, check=True)
    # Cargo's native library targets this host. Package the Flutter executable
    # for that same architecture, rather than advertising an unsupported second
    # architecture without a matching Rust engine.
    architecture = platform.machine()
    if architecture not in ("arm64", "x86_64"):
        parser.error("The macOS bundle requires a native arm64 or x86_64 host")
    environment = dict(os.environ, FLUTTER_XCODE_ARCHS=architecture)
    subprocess.run([args.flutter, "build", "macos", "--" + profile], cwd=APP, env=environment, check=True)
    bundle = APP / f"build/macos/Build/Products/{profile.title()}/mobile_adapter_bridge.app"
    library = bundle / "Contents/Frameworks/libmobile_bridge.dylib"
    shutil.copy2(ROOT / f"mobile_adapter/server/target/{profile}/libmobile_bridge.dylib", library)
    subprocess.run(["codesign", "--force", "--sign", "-", str(library)], check=True)
    subprocess.run(["codesign", "--force", "--deep", "--sign", "-", str(bundle)], check=True)
    print(bundle)
    if args.run:
        subprocess.run(["open", str(bundle)], check=True)

if __name__ == "__main__":
    main()
