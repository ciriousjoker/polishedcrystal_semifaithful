#!/usr/bin/env python3
"""Build, deploy and start the isolated Mac/Android Mobile Adapter session."""

import argparse
from datetime import datetime
import hashlib
import ipaddress
import json
from pathlib import Path
import re
import shlex
import shutil
import socket
import subprocess
import sys
import threading
import time

from probe_emulator import read_symbols, require, validate_battery

REPO = Path(__file__).resolve().parents[2]
TOOLS = Path(__file__).resolve().parent
CACHE = Path("/Volumes/Storage/Cache/mobileadapter")
SDK = Path("/Volumes/Storage/Development/Android/sdk")
FRONTEND = Path("/Volumes/Storage/Applications/RetroArch.app/Contents/MacOS/RetroArch")
CARGO = Path("/Volumes/Storage/Cache/cargo/bin/cargo")
PORT = 17423
NAME = "polishedcrystal-mobile"
PACKAGES = ("com.retroarch.aarch64", "com.retroarch")
CORE_REVISION = "03f58ca3dfb4b716f7e66a0e0467f9e85ef82abb"


def run(command, **kwargs):
    kwargs.setdefault("timeout", 120)
    kwargs.setdefault("check", True)
    return subprocess.run([str(part) for part in command], **kwargs)


def output(command):
    return run(command, capture_output=True, text=True).stdout.strip()


def config(path, values, base=""):
    keys = set(values)
    lines = [line for line in base.splitlines()
             if not re.match(r"\s*([\w]+)\s*=", line)
             or re.match(r"\s*([\w]+)\s*=", line).group(1) not in keys]
    for key, value in values.items():
        text = str(value).replace("\\", "\\\\").replace('"', '\\"')
        lines.append(f'{key} = "{text}"')
    path.write_text("\n".join(lines) + "\n")


def server_address(args):
    if args.server_ip:
        address = ipaddress.IPv4Address(args.server_ip)
    else:
        interface = args.interface
        if not interface:
            route = output(["/sbin/route", "-n", "get", "default"])
            match = re.search(r"interface:\s*(\S+)", route)
            require(match is not None, "No default interface; pass --server-ip")
            interface = match.group(1)
        address = ipaddress.IPv4Address(output(["/usr/sbin/ipconfig", "getifaddr", interface]))
    require(not address.is_unspecified and not address.is_multicast,
            "Choose the Mac's unicast LAN IPv4 address")
    require(args.mac_only or not address.is_loopback, "Android cannot connect to the Mac's loopback IP")
    return str(address)


def lan_rom(source, symbols, destination, address):
    """Change only our endpoint table and the global cartridge checksum."""
    bank, location = read_symbols(symbols)["MobileEndpoint"]
    require(1 <= bank < 128 and 0x4000 <= location <= 0x7ffa, "Invalid MobileEndpoint symbol")
    offset = bank * 0x4000 + location - 0x4000
    data = bytearray(source.read_bytes())
    require(len(data) == 2 * 1024 * 1024, "Expected the supported 2 MiB feature ROM")
    require(data[offset:offset + 6] == bytes([127, 0, 0, 1]) + PORT.to_bytes(2, "big"),
            "Default endpoint changed; update the deployment contract before patching")
    data[offset:offset + 6] = ipaddress.IPv4Address(address).packed + PORT.to_bytes(2, "big")
    checksum = (sum(data[:0x14e]) + sum(data[0x150:])) & 0xffff
    data[0x14e:0x150] = checksum.to_bytes(2, "big")
    destination.write_bytes(data)
    return hashlib.sha256(data).hexdigest()


def native_tools(args, android=False):
    source = args.cache / "sources/doublecherry"
    require(output(["git", "-C", source, "rev-parse", "HEAD"]) == CORE_REVISION,
            "DoubleCherryGB source is not the pinned tested revision")
    runner = args.cache / "smoke-libretro"
    run(["clang++", "-std=c++17", "-O2", "-I", source / "libretro/DoubleCherryEngine",
         TOOLS / "smoke_libretro.cpp", "-o", runner])
    if not android:
        return runner
    ndk = args.sdk / "ndk/29.0.14206865"
    require(ndk.exists(), "Install Android NDK 29.0.14206865, or pass --sdk with that NDK")
    build = args.cache / "build/doublecherry-android-arm64"
    run(["cmake", "-S", source, "-B", build, "-DCMAKE_BUILD_TYPE=Release",
         f"-DCMAKE_TOOLCHAIN_FILE={ndk}/build/cmake/android.toolchain.cmake",
         "-DANDROID_ABI=arm64-v8a", "-DANDROID_PLATFORM=android-23",
         "-DANDROID_STL=c++_static", "-DLIBMOBILE_BUILD_SHARED=OFF"], stdout=subprocess.DEVNULL)
    with (args.cache / "android-build.log").open("w") as log:
        run(["cmake", "--build", build, "-j4"], stdout=log, stderr=subprocess.STDOUT)
    compiler = ndk / "toolchains/llvm/prebuilt/darwin-x86_64/bin/aarch64-linux-android23-clang++"
    android_runner = args.cache / "smoke-libretro-android-arm64"
    run([compiler, "-std=c++17", "-O2", "-static-libstdc++", "-ldl",
         "-I", source / "libretro/DoubleCherryEngine", TOOLS / "smoke_libretro.cpp",
         "-o", android_runner])
    return build / "DoubleCherryGB_libretro.so", android_runner


def fixture(args, runner):
    seed = args.cache / "dev-session/start.sav"
    if not seed.exists():
        baseline = args.cache / "m1-baseline-regression/create/ingame.sav"
        if not baseline.exists():
            baseline_run = args.cache / ("dev-baseline-" + datetime.now().strftime("%Y%m%d-%H%M%S"))
            run([sys.executable, TOOLS / "probe_emulator.py", "--runner", runner,
                 "--core", args.cache / "build/doublecherry/DoubleCherryGB_libretro.dylib",
                 "--output", baseline_run])
            baseline = baseline_run / "create/ingame.sav"
        shutil.copyfile(baseline, seed)
    validate_battery(seed, read_symbols(REPO / "polishedcrystal-mobile-3.2.3.sym"))
    return seed


def profile(root, target, base="", android=False, package=None):
    for directory in (("system",) if android else ("roms", "saves", "states", "system")):
        (root / directory).mkdir(parents=True, exist_ok=True)
    config(root / "core-options.cfg", {
        "dcgb_emulated_gameboys": "1", "dcgb_mobile_adatper_enabled": "1",
        "dcgb_mobile_adatper_device_color": "8", "dcgb_mobile_adatper_use_relay_server": "0"})
    values = {
        "core_options_path": f"{target}/core-options.cfg", "game_specific_options": "false",
        # An inherited per-core setting otherwise ignores our explicit options
        # path in favour of DoubleCherryGB/DoubleCherryGB.opt.
        "global_core_options": "true",
        "savefile_directory": str(target) if android else f"{target}/saves",
        "savestate_directory": str(target) if android else f"{target}/states",
        "system_directory": f"{target}/system", "sort_savefiles_enable": "false",
        "sort_savestates_enable": "false", "sort_savefiles_by_content_enable": "false",
        "sort_savestates_by_content_enable": "false", "savestate_auto_load": "true",
        "savestate_auto_save": "false", "block_sram_overwrite": "false",
        "config_save_on_exit": "false", "auto_overrides_enable": "false",
        "auto_remaps_enable": "false", "cheevos_enable": "false", "rewind_enable": "false",
        "run_ahead_enabled": "false", "log_verbosity": "true", "log_to_file": "true",
        "libretro_log_level": "0",
        "log_to_file_timestamp": "false", "log_dir": str(target),
        "content_history_path": f"{target}/content_history.lpl", "content_history_size": "1",
        "cloud_sync_enable": "false", "cloud_sync_driver": "null",
    }
    if android:
        values.update({"input_overlay_enable": "true"})
        # Preserve the user's working touch overlay when an existing config exists.
        if not re.search(r'^input_overlay\s*=\s*"[^\"]+"', base, re.M):
            values["input_overlay"] = f"/data/data/{package}/overlays/gamepads/flat/gb.cfg"
    else:
        values.update({"video_driver": "glcore", "video_fullscreen": "false",
                       "video_scale": "3", "menu_driver": "rgui", "input_driver": "cocoa",
                       "audio_volume": "-26.0206",  # 20 * log10(0.05): 5% amplitude
                       "pause_nonactive": "false",  # Keep presence alive while using the phone/terminal.
                       "input_player1_a": "x", "input_player1_b": "z",
                       "input_player1_x": "nul", "input_player1_select": "shift",
                       "input_player1_start": "enter", "input_player1_up": "up",
                       "input_player1_down": "down", "input_player1_left": "left",
                       "input_player1_right": "right", "input_exit_emulator": "escape",
                       "input_menu_toggle": "f1"})
    config(root / "retroarch.cfg", values, base)


def verify_idle(path):
    state = json.loads(path.read_text())["symbols"]
    for name, expected in {"hCrashCode": 0, "hCGB": 1, "hInMenu": 0,
                           "wMapStatus": 2, "wScriptRunning": 0, "wMobileOwner": 0,
                           "wMapGroup": 24, "wMapNumber": 5}.items():
        require(state.get(name) == expected, f"Start-state preparation failed: {name}")


def core_install_record(core, package, details):
    # App replacement invalidates the installation marker even for the same core.
    installation = {}
    for key in ("versionCode", "firstInstallTime", "lastUpdateTime"):
        match = re.search(r"^\s*" + key + r"=([^\n]+)", details, re.M)
        require(match is not None, f"Cannot identify RetroArch installation: {key}")
        installation[key] = match.group(1).strip()
    return {"schema": 1, "package": package, "installation": installation,
            "core_sha256": hashlib.sha256(core.read_bytes()).hexdigest()}


def frontend_status(log):
    # The core constructs libmobile even when its emulation option is Off.
    # Its attachment notification is OSD-only, so initialization is not proof
    # that the adapter is attached or that the game connected to the server.
    return {"initialized": "[Mobile] Adapter initialized" in log,
            "state": bool(re.search(r"Auto-loading save state from .* succeeded\.", log))}


def labelled_server_line(line, address, android_address=None):
    match = re.search(r"\bpeer=([^\s:]+):\d+", line)
    if match:
        peer = match.group(1)
        label = "mac" if peer == address else "android" if peer == android_address else "unknown"
        return line.rstrip() + f" client={label}\n"
    return line


def android_intent(package, target):
    # ES-DE uses this activity with CONFIGFILE, LIBRETRO and ROM string extras.
    # Clear the previous task; -W waits for the one requested launch to complete.
    return ["am", "start", "-W", "-f", "0x10008000", "-n",  # NEW_TASK | CLEAR_TASK
            f"{package}/com.retroarch.browser.retroactivity.RetroActivityFuture",
            "--es", "CONFIGFILE", target + "/retroarch.cfg", "--es", "LIBRETRO",
            f"/data/data/{package}/cores/DoubleCherryGB_libretro.so",
            "--es", "ROM", target + f"/{NAME}.gbc"]


def mac_profile(args, runner, seed, address):
    root = args.cache / "dev-session/mac"
    profile(root, root)
    destination = root / f"roms/{NAME}.gbc"
    checksum = lan_rom(REPO / "polishedcrystal-mobile-3.2.3.gbc",
                       REPO / "polishedcrystal-mobile-3.2.3.sym", destination, address)
    save = root / f"saves/{NAME}.srm"
    if save.exists():
        backup = args.cache / "dev-session/backups" / datetime.now().strftime("%Y%m%d-%H%M%S-%f")
        backup.mkdir(parents=True)
        shutil.copyfile(save, backup / "mac.srm")
    shutil.copyfile(seed, save)
    with (root / "prepare.log").open("w") as log:
        run([runner, args.cache / "build/doublecherry/DoubleCherryGB_libretro.dylib",
             destination, root / "start.ppm", root / "system", "--frames", "3500",
             "--input", TOOLS / "fixtures/continue.input", "--save-in", save,
             "--symbols", REPO / "polishedcrystal-mobile-3.2.3.sym",
             "--state-out", root / "start.json", "--snapshot-out", root / f"states/{NAME}.state.auto"],
            stdout=log, stderr=subprocess.STDOUT)
    verify_idle(root / "start.json")
    (root / "build.json").write_text(json.dumps({"endpoint": f"{address}:{PORT}",
        "rom_sha256": checksum, "core_revision": CORE_REVISION,
        "snapshot_regenerated": True}, indent=2) + "\n")
    return root


class Android:
    def __init__(self, args):
        self.adb = shutil.which("adb") or str(args.sdk / "platform-tools/adb")
        devices = output([self.adb, "devices"]).splitlines()[1:]
        ready = [line.split()[0] for line in devices if len(line.split()) > 1 and line.split()[1] == "device"]
        if args.serial:
            require(args.serial in ready, "Selected ADB device is not connected and authorized")
            self.serial = args.serial
        else:
            require(len(ready) == 1, "Connect one Android phone with USB debugging; use --serial for multiple devices")
            self.serial = ready[0]
        installed = self.shell("pm", "list", "packages", "retroarch").splitlines()
        found = [pkg for pkg in PACKAGES if f"package:{pkg}" in installed]
        self.package = args.package or (found[0] if len(found) == 1 else None)
        require(self.package in PACKAGES and f"package:{self.package}" in installed,
                "Install official ARM64 RetroArch and open it once; pass --package if both versions are installed")
        require("arm64-v8a" in self.shell("getprop", "ro.product.cpu.abilist"), "This prepared core requires ARM64 Android")
        details = self.shell("dumpsys", "package", self.package)
        self.package_details = details
        require("primaryCpuAbi=arm64-v8a" in details, "Use the ARM64 RetroArch APK for this core")
        self.root = args.android_dir.rstrip("/")
        require(self.root.startswith("/storage/emulated/0/") and ".." not in self.root.split("/"),
                "Android test directory must be an absolute shared-storage directory")

    def call(self, *command, **kwargs):
        return run([self.adb, "-s", self.serial, *command], **kwargs)

    def shell(self, *command):
        return self.call("shell", shlex.join(str(part) for part in command), capture_output=True, text=True).stdout.strip()

    def exists(self, path):
        result = subprocess.run([self.adb, "-s", self.serial, "shell", shlex.join(["test", "-e", path])],
                                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        return result.returncode == 0


def deploy(args, device, mac, seed, core, runner):
    root = args.cache / "dev-session/android"
    root.mkdir(parents=True, exist_ok=True)
    existing = f"/storage/emulated/0/Android/data/{device.package}/files/retroarch.cfg"
    require(device.exists(existing), "Open RetroArch once to initialize its configuration and touch controls")
    device.call("pull", existing, root / "base.cfg", stdout=subprocess.DEVNULL)
    base = (root / "base.cfg").read_text()
    target = device.root
    profile(root, target, base, android=True, package=device.package)
    shutil.copyfile(mac / f"roms/{NAME}.gbc", root / f"{NAME}.gbc")
    shutil.copyfile(seed, root / f"{NAME}.sav")
    shutil.copyfile(seed, root / f"{NAME}.srm")
    if device.exists(target + f"/{NAME}.srm"):
        backup = args.cache / "dev-session/backups" / datetime.now().strftime("%Y%m%d-%H%M%S-%f")
        backup.mkdir(parents=True)
        device.call("pull", target + f"/{NAME}.srm", backup / "android.srm", stdout=subprocess.DEVNULL)
    temporary = "/data/local/tmp/polishedcrystal-mobileadapter"
    device.shell("mkdir", "-p", temporary)
    for source, name in [(core, "DoubleCherryGB_libretro.so"), (runner, "smoke-libretro"),
                         (seed, "start.sav"), (mac / f"roms/{NAME}.gbc", "current.gbc"),
                         (REPO / "polishedcrystal-mobile-3.2.3.sym", "current.sym"),
                         (TOOLS / "fixtures/continue.input", "continue.input")]:
        device.call("push", source, f"{temporary}/{name}", stdout=subprocess.DEVNULL)
    device.shell("chmod", "755", temporary + "/smoke-libretro")
    device.shell("mkdir", "-p", temporary + "/system")
    # Generate the Android state on Android, with its own exact core ABI/layout.
    device.shell(temporary + "/smoke-libretro", temporary + "/DoubleCherryGB_libretro.so",
                 temporary + "/current.gbc", temporary + "/start.ppm", temporary + "/system",
                 "--frames", "3500", "--input", temporary + "/continue.input",
                 "--save-in", temporary + "/start.sav", "--symbols", temporary + "/current.sym",
                 "--state-out", temporary + "/start.json", "--snapshot-out", temporary + "/start.state")
    device.call("pull", temporary + "/start.json", root / "start.json", stdout=subprocess.DEVNULL)
    verify_idle(root / "start.json")
    device.call("pull", temporary + "/start.state", root / f"{NAME}.state.auto", stdout=subprocess.DEVNULL)
    install = core_install_record(core, device.package, device.package_details)
    stamp = target + "/.core-install.json"
    previous = None
    if device.exists(stamp):
        try:
            previous = json.loads(device.shell("cat", stamp))
        except ValueError:
            pass
    require(args.reinstall_core or previous == install,
            "Android core installation is missing or changed; run once with --reinstall-core to install it explicitly")
    if args.reinstall_core:
        print("ANDROID CORE: explicit installation requested; RetroArch briefly opens for installation.", flush=True)
        # Use RetroArch's own exported installer to copy the core into private storage.
        device.shell("am", "start", "-n", f"{device.package}/com.retroarch.browser.debug.CoreSideloadActivity",
                     "--es", "LIBRETRO", temporary + "/DoubleCherryGB_libretro.so")
        deadline = time.monotonic() + 20
        while time.monotonic() < deadline:
            activities = device.shell("dumpsys", "activity", "activities")
            if re.search(r"(?:mResumedActivity|topResumedActivity).*" + re.escape(device.package)
                         + r"/.*RetroActivityFuture", activities):
                break
            time.sleep(0.2)
        else:
            raise ValueError("RetroArch core installation did not complete; open RetroArch once and check its storage permissions")
        device.shell("am", "force-stop", device.package)
        (root / ".core-install.json").write_text(json.dumps(install, indent=2) + "\n")
        device.shell("mkdir", "-p", target)
        device.call("push", root / ".core-install.json", stamp, stdout=subprocess.DEVNULL)
    else:
        print("ANDROID CORE: using installed core; no installer activity will be opened.", flush=True)
    # Only replace our named files. Other files in the user's test folder stay.
    device.shell("mkdir", "-p", device.root)
    device.shell("mkdir", "-p", device.root + "/system")
    names = [f"{NAME}.gbc", f"{NAME}.sav", f"{NAME}.srm", f"{NAME}.state.auto",
             "core-options.cfg", "retroarch.cfg"]
    for name in names:
        device.call("push", root / name, target + "/" + name + ".new", stdout=subprocess.DEVNULL)
    for name in names:
        device.shell("mv", "-f", target + "/" + name + ".new", target + "/" + name)
    # Install game-specific settings for normal ROM shortcuts as well as the
    # explicit ADB launch, without changing the user's global configuration.
    match = re.search(r'^rgui_config_directory\s*=\s*"([^\"]+)"', base, re.M)
    settings = match.group(1) if match and match.group(1) not in ("default", "") else "/storage/emulated/0/RetroArch/config"
    settings += "/DoubleCherryGB"
    overrides = root / f"{NAME}.cfg"
    config(overrides, {"savefile_directory": target, "savestate_directory": target,
                      "system_directory": target + "/system", "savestate_auto_load": "true",
                      "savestate_auto_save": "false", "sort_savefiles_enable": "false",
                      "sort_savestates_enable": "false", "sort_savefiles_by_content_enable": "false",
                      "sort_savestates_by_content_enable": "false", "cheevos_enable": "false"})
    device.shell("mkdir", "-p", settings)
    device.call("push", overrides, settings + f"/{NAME}.cfg", stdout=subprocess.DEVNULL)
    device.call("push", root / "core-options.cfg", settings + f"/{NAME}.opt", stdout=subprocess.DEVNULL)
    # A small separate playlist fixes the custom core selection for this ROM.
    playlist = root / "Polished Crystal Mobile.lpl"
    playlist.write_text(json.dumps({"version": "1.5", "items": [{
        "path": target + f"/{NAME}.gbc", "label": "Polished Crystal Mobile Adapter",
        "core_path": f"/data/data/{device.package}/cores/DoubleCherryGB_libretro.so",
        "core_name": "DoubleCherryGB", "crc32": "00000000|crc", "db_name": ""}]}, indent=2) + "\n")
    device.call("push", playlist, target + "/" + playlist.name, stdout=subprocess.DEVNULL)
    launch = android_intent(device.package, target)
    (root / "launch-adb.txt").write_text(shlex.join([device.adb, "-s", device.serial, "shell", shlex.join(launch)]) + "\n")
    print(f"ANDROID PREPARED: {target}/{NAME}.gbc with its fresh auto-start state.", flush=True)
    return launch


def launch_android(device, launch):
    logfile = device.root + "/retroarch.log"
    # Remove only our previous session's log so it cannot fake a successful start.
    device.shell("am", "force-stop", device.package)
    device.shell("rm", "-f", logfile)
    try:
        device.shell(*launch)
    except subprocess.CalledProcessError as error:
        raise ValueError(f"Android intent failed: {(error.stderr or error.stdout or str(error)).strip()}") from error
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        if device.exists(logfile):
            log = device.shell("tail", "-n", "150", logfile)
            if all(frontend_status(log).values()):
                print("ANDROID START confirmed: core initialized; fresh state loaded. Select+B joins the three-player lobby.", flush=True)
                return
        time.sleep(0.5)
    result = device.call("shell", "pidof", device.package, capture_output=True, text=True, check=False)
    alive = result.stdout.strip() if result.returncode == 0 else "none"
    raise ValueError(f"Android start was not confirmed (pid={alive}); check {logfile}. "
                     "Use --reinstall-core if the installed core was replaced or removed manually.")


def start(args, mac, address, device=None, launch=None):
    server = REPO / "mobile_adapter/server/target/debug/mobile-server"
    children = []
    stopped = threading.Event()
    log = (mac / "play.log").open("w")
    try:
        process = subprocess.Popen([str(server), "--bind", f"{address}:{PORT}"],
                                   stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, bufsize=1)
        children.append(process)
        ready = process.stdout.readline().strip()
        require(ready == f"READY {address}:{PORT}", f"Server failed to start: {ready}")
        print(f"SERVER tcp://{address}:{PORT} (fixed port {PORT})", flush=True)
        android_address = None
        if device:
            route = device.shell("ip", "-4", "route", "get", address)
            match = re.search(r"\bsrc\s+(\S+)", route)
            android_address = match.group(1) if match else None
            print(f"CLIENT ADDRESSES mac={address} android={android_address or 'unknown'} (labels by source IP)", flush=True)
        def forward():
            for line in process.stdout:
                print(labelled_server_line(line, address, android_address), end="", flush=True)
        threading.Thread(target=forward, daemon=True).start()
        children.append(subprocess.Popen([str(args.frontend), "-v", "-c", str(mac / "retroarch.cfg"),
            "-L", str(args.cache / "build/doublecherry/DoubleCherryGB_libretro.dylib"),
            str(mac / f"roms/{NAME}.gbc")], stdout=log, stderr=subprocess.STDOUT))
        def watch_mac():
            while not stopped.wait(0.5):
                if all(frontend_status((mac / "play.log").read_text(errors="replace")).values()):
                    print("MAC START confirmed: core initialized; fresh state loaded. Left Shift+Z (Select+B) joins the lobby.", flush=True)
                    return
        threading.Thread(target=watch_mac, daemon=True).start()
        if device:
            launch_android(device, launch)
        print("MAC READY: arrows, A=X, B=Z, Select=left Shift. Select+B joins/leaves; Select+A opens players/status.", flush=True)
        print("MILESTONE 2: join on both clients, then move one tile to see the other avatar. Select+B+A runs the echo diagnostic.", flush=True)
        print("Press Ctrl+C here to stop this server and Mac game instance.", flush=True)
        while all(child.poll() is None for child in children):
            time.sleep(0.25)
        require(all(child.returncode in (None, 0) for child in children), "Server or Mac frontend exited with an error; see play.log")
    finally:
        stopped.set()
        for child in reversed(children):
            if child.poll() is None:
                child.terminate()
                try:
                    child.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    child.kill()
                    child.wait()
        log.close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--server-ip", help="Mac LAN IPv4; otherwise detect its default interface")
    parser.add_argument("--interface", help="Network interface to use instead of the default route")
    parser.add_argument("--serial", help="ADB device serial when several devices are connected")
    parser.add_argument("--package", choices=PACKAGES)
    parser.add_argument("--reinstall-core", action="store_true", help="Explicitly install the Android core; normal starts never open its installer")
    parser.add_argument("--mac-only", action="store_true", help="Prepare/start Mac without an Android device")
    parser.add_argument("--prepare-only", action="store_true", help="Prepare artifacts without starting the server or GUI")
    parser.add_argument("--cache", type=Path, default=CACHE)
    parser.add_argument("--sdk", type=Path, default=SDK)
    parser.add_argument("--android-dir", default="/storage/emulated/0/Download/polishedcrystal_mobile_adapter",
                        help="Shared-storage folder for the stable ROM, save and auto-start snapshot")
    parser.add_argument("--frontend", type=Path, default=FRONTEND)
    args = parser.parse_args()
    require(sys.platform == "darwin", "This launcher is for the configured native macOS host")
    require(args.frontend.is_file(), "RetroArch frontend missing; pass --frontend")
    address = server_address(args)
    if not args.prepare_only:
        try:
            with socket.socket() as check:
                check.bind((address, PORT))
        except OSError as error:
            raise ValueError(f"Port {PORT} is in use; stop the previous session with Ctrl+C before updating") from error
    device = None if args.mac_only else Android(args)
    if device:
        device.shell("am", "force-stop", device.package)
    (args.cache / "dev-session").mkdir(parents=True, exist_ok=True)
    print(f"Preparing current build for tcp://{address}:{PORT} ...", flush=True)
    run(["make", "-j4", "mobile", f"RGBDS={args.cache}/rgbds-1.0.4/"], cwd=REPO)
    run([CARGO, "build", "--locked", "--manifest-path", REPO / "mobile_adapter/server/Cargo.toml"], cwd=REPO)
    runner = native_tools(args)
    seed = fixture(args, runner)
    mac = mac_profile(args, runner, seed, address)
    launch = None
    if device:
        core, android_runner = native_tools(args, android=True)
        launch = deploy(args, device, mac, seed, core, android_runner)
    print(f"Prepared ROM + battery save + fresh start snapshot: {mac}", flush=True)
    if not args.prepare_only:
        start(args, mac, address, device, launch)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("Development session stopped.")
    except (OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        print(f"Development session failed: {error}", file=sys.stderr)
        sys.exit(1)
