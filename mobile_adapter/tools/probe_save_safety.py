#!/usr/bin/env python3
"""Save normally while online, inject failures, then boot the exported battery.

Runs real ROMs with ordinary input and read-only observers. Does not load an
emulator state, patch save data, or write game RAM. Each case owns its server.
"""
import argparse
import gzip
import json
from pathlib import Path
import select
import socket
import socketserver
import subprocess
import threading
import time
from probe_emulator import digest, read_symbols, require, validate_battery

REPO = Path(__file__).resolve().parents[2]
CASES = ("connected", "peer-disconnect", "server-close", "malformed", "emergency")


class FaultProxy(socketserver.ThreadingTCPServer):
    daemon_threads = True
    allow_reuse_address = True

    def __init__(self, port, upstream):
        self.upstream = upstream
        self.armed = threading.Event()
        self.injected = threading.Event()
        super().__init__(("127.0.0.1", port), FaultHandler)


class FaultHandler(socketserver.BaseRequestHandler):
    def handle(self):
        pending = bytearray()
        try:
            with socket.create_connection(("127.0.0.1", self.server.upstream)) as upstream:
                for stream in (self.request, upstream):
                    stream.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
                while True:
                    readable, _, _ = select.select((self.request, upstream), (), (), .05)
                    for stream in readable:
                        data = stream.recv(4096)
                        if not data:
                            return
                        if stream is self.request:
                            upstream.sendall(data)
                            continue
                        pending.extend(data)
                        while len(pending) >= 8:
                            size = 8 + int.from_bytes(pending[6:8], "big")
                            require(size <= 128, "Test proxy received an oversized server frame")
                            if len(pending) < size:
                                break
                            frame = bytes(pending[:size])
                            del pending[:size]
                            if self.server.armed.is_set():
                                frame = b"!" + frame[1:]  # corrupt application magic only
                                self.server.injected.set()
                            self.request.sendall(frame)
        except (OSError, ValueError):
            return


def observations(path):
    # A running gzip stream may end before its footer or final JSON line.
    # Retain complete read-only observations without modifying a live trace.
    lines = []
    if path.exists():
        lines = path.read_text().splitlines()
    elif Path(str(path) + ".gz").exists():
        try:
            with gzip.open(str(path) + ".gz", "rt") as trace:
                while True:
                    try:
                        line = trace.readline()
                    except EOFError:
                        break
                    if not line:
                        break
                    lines.append(line)
        except EOFError:
            pass
    rows = []
    for line in lines:
        try:
            rows.append(json.loads(line))
        except json.JSONDecodeError:
            continue
    return rows


def command(args, rom, symbols, directory, battery, frames, script, adapter=1):
    directory.mkdir()
    (directory / "input.txt").write_text(script)
    core = args.profile_core if args.profile_core and directory.name == "primary" else args.core
    result = [str(args.runner), str(core), str(rom), str(directory / "screen.ppm"), str(directory),
            "--frames", str(frames), "--input", str(directory / "input.txt"), "--symbols", str(symbols),
            "--save-in", str(battery), "--save-out", str(directory / "ingame.sav"),
            "--state-out", str(directory / "state.json"), "--trace-out", str(directory / "trace.jsonl.gz"),
            "--adapter", str(adapter)]
    if core == args.profile_core:
        result += ["--profile-out", str(directory / "profile.json")]
    return result


def run_case(args, case, index):
    out = args.output / case
    out.mkdir()
    port = args.port + index * 2
    data = bytearray(args.rom.read_bytes())
    bank, address = read_symbols(args.symbols)["MobileEndpoint"]
    offset = bank * 0x4000 + address - 0x4000
    data[offset + 4:offset + 6] = port.to_bytes(2, "big")
    data[0x14e:0x150] = ((sum(data[:0x14e]) + sum(data[0x150:])) & 65535).to_bytes(2, "big")
    rom = out / "test.gbc"
    rom.write_bytes(data)
    symbols = out / "test.sym"
    symbols.write_bytes(args.symbols.read_bytes())
    bindings = read_symbols(symbols)
    expected_checksum = validate_battery(args.battery, bindings)["checksums"]["s"]
    bank, address = bindings['sMapData']
    start = bank * 0x2000 + address - 0xa000
    battery_data = args.battery.read_bytes()
    def saved_byte(name):
        return battery_data[start + bindings[name][1] - bindings['wCurMapData'][1]]
    original_map = (saved_byte('wMapGroup'), saved_byte('wMapNumber'))
    original_x, original_y = saved_byte('wXCoord'), saved_byte('wYCoord')
    peer_count = 1 + len(args.extra_peer_battery)
    proxy = FaultProxy(port, port + 1) if case == "malformed" else None
    server_port = port + 1 if proxy else port
    seed = (REPO / "mobile_adapter/tools/fixtures/continue.input").read_text()
    join = "\nON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 SELECT 8\nON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 B 8\n"
    primary = seed + join + f"ON 3550 wMobileOnline=1,wMobilePeerCount={peer_count} RIGHT 12\n"
    primary += "3750 START 8\n3850 DOWN 5\n3950 DOWN 5\n4050 A 5\n"
    for frame in range(4250, 5350, 100):
        primary += f"{frame} A 5\n"
    primary += "5550 B 5\n5800 RIGHT 12\n"
    if case == "emergency":
        primary += "4051 SELECT 8\n4051 B 8\n"
    peer = seed + join + f"ON 3550 wMobileOnline=1,wMobilePeerCount={peer_count} DOWN 12\n"
    processes = []
    handles = []
    # Profiled primary execution can finish later than ordinary peer cores.
    # Keep them pumping until its final capture, rather than ending the lobby.
    peer_barrier = out / 'keep-peers'
    peer_barrier.touch()
    with (out / "server.log").open("w") as log:
        server = subprocess.Popen([str(args.server), "--bind", f"127.0.0.1:{server_port}"],
                                  stdout=log, stderr=subprocess.STDOUT)
        try:
            deadline = time.monotonic() + 5
            while f"READY 127.0.0.1:{server_port}" not in (out / "server.log").read_text():
                require(server.poll() is None and time.monotonic() < deadline, "Server did not start")
                time.sleep(.02)
            if proxy:
                threading.Thread(target=proxy.serve_forever, daemon=True).start()
            clients = [("primary", args.battery, primary), ("peer", args.peer_battery, peer)]
            clients.extend((f"peer{index + 2}", battery, seed + join) for index, battery in enumerate(args.extra_peer_battery))
            for name, battery, script in clients:
                directory = out / name
                cmd = command(args, rom, symbols, directory, battery, 6500, script)
                cmd += ["--pace-from", "3250"]
                if name != 'primary':
                    cmd += ['--finish-barrier', str(peer_barrier), '--expected-peers', str(peer_count)]
                handle = (directory / "process.log").open("w")
                handles.append(handle)
                processes.append(subprocess.Popen(cmd, stdout=handle, stderr=subprocess.STDOUT))
            deadline = time.monotonic() + 75
            saved, fault_frame = None, None
            while saved is None:
                finished = processes[0].poll() is not None
                for row in observations(out / "primary/trace.jsonl"):
                    state = row["symbols"]
                    if (fault_frame is None and row['frame'] >= 4050 and state['wMenuSelection'] == 4 and
                            state['wScriptRunning'] == 255 and state['sChecksum'] == expected_checksum):
                        require(state['wMobileOnline'] == 1 and state['wMobilePeerCount'] == peer_count,
                                'Fault fixture was not connected before the native save')
                        fault_frame = row['frame']
                        if case == "peer-disconnect":
                            processes[1].terminate()
                            processes[1].wait(timeout=5)
                        elif case == "server-close":
                            server.terminate()
                            server.wait(timeout=5)
                        elif case == "malformed":
                            proxy.armed.set()
                    if (row["frame"] >= 4050 and state["sChecksum"] != expected_checksum and
                            state["wScriptRunning"] == 0 and state["hInMenu"] == 0):
                        saved = row
                        break
                # The runner flushes its last compressed observations on exit.
                # Inspect those before deciding that a completed process failed.
                if saved is not None:
                    break
                require(not finished and time.monotonic() < deadline,
                        "Connected in-game save did not complete before the deadline")
                time.sleep(.1)
            require(fault_frame is not None and fault_frame < saved['frame'], 'Fault was not injected during the native Save menu')
            require(processes[0].wait(timeout=40) == 0, "Primary failed after the injected fault")
            if proxy:
                require(proxy.injected.is_set(), "No malformed packet reached the client")
        finally:
            peer_barrier.unlink(missing_ok=True)
            for process in processes:
                if process.poll() is None:
                    process.terminate()
                process.wait(timeout=5)
            for handle in handles:
                handle.close()
            if proxy:
                proxy.shutdown()
                proxy.server_close()
            if server.poll() is None:
                server.terminate()
            server.wait(timeout=5)
    state = json.loads((out / "primary/state.json").read_text())["symbols"]
    require(state["hCrashCode"] == 0 and state["wScriptRunning"] == state["hInMenu"] == 0,
            "Failure did not retain ordinary gameplay")
    require((state["wXCoord"], state["wYCoord"]) == (original_x + 2, original_y), "Movement after fault failed")
    require(state["sChecksum"] == saved["symbols"]["sChecksum"], "Fault changed saved game data")
    rows = observations(out / "primary/trace.jsonl")
    require(any(row["symbols"]["wMenuSelection"] == 4 for row in rows), "Normal Save option was not selected")
    if case == "peer-disconnect":
        require(state["wMobileOnline"] == 1 and state["wMobilePeerCount"] == peer_count - 1,
                "Disconnected peer remained in the lobby")
    elif case == "emergency":
        require(state["wMobileOnline"] == state["wMobileOwner"] == state["wMobileWanted"] == 0,
                "Physical exit was blocked by the native Save menu")
        require(any(row["frame"] > fault_frame and row["symbols"]["wMobileError"] == 1 for row in rows),
                "Physical Save-menu exit was not observed")
    elif case != "connected":
        require(any(row["frame"] > fault_frame and row["symbols"]["wMobileError"] == 5
                    for row in rows) if case == "malformed" else
                any(row["frame"] > fault_frame and row["symbols"]["wMobileError"] for row in rows),
                "Injected fault was not observed by the ROM")
    battery = out / "primary/ingame.sav"
    battery_report = validate_battery(battery, read_symbols(symbols))
    bank, address = bindings['sPlayerData']
    object_start = bank * 0x2000 + address - 0xa000 + bindings['wObjectStructs'][1] - bindings['wPlayerData'][1]
    data = battery.read_bytes()
    require(all(data[object_start + slot * 34 + 1] != 254 for slot in range(1, 13)),
            "A remote TEMP_OBJECT leaked into the ordinary battery")
    profile = None
    if args.profile_core:
        profile = json.loads((out / 'primary/profile.json').read_text())
        # Native Continue may also refresh the backup copy before joining.
        require(profile['save_copy_count'] >= 2 and profile['save_copy_register_failures'] == 0 and
                profile['save_copy_byte_failures'] == 0,
                "Primary/backup native save copies differed from their same-state ordinary payloads")
        require(profile['save_copy_lowest_sp'] >= 0xc040, 'Insufficient native-save stack headroom')
        if case == 'connected':
            require(profile['save_projected_objects'] == 2 * peer_count,
                    "Connected save did not project every original native object into both copies")
    before = digest(battery)
    for name, frames, script, expected_x in (("continue", 3500, seed, original_x + 1),
                                            ("move", 4200, seed + "3750 RIGHT 12\n", original_x + 2)):
        directory = out / name
        cmd = command(args, rom, symbols, directory, battery, frames, script, adapter=0)
        with (directory / "process.log").open("w") as log:
            subprocess.run(cmd, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=45)
        loaded = json.loads((directory / "state.json").read_text())["symbols"]
        require(loaded["hCrashCode"] == loaded["wMobileOwner"] == 0 and loaded["wSaveFileExists"] == 1,
                "Fresh Continue failed or restored a live transport owner")
        require((loaded["wMapGroup"], loaded["wMapNumber"], loaded["wXCoord"], loaded["wYCoord"]) ==
                (*original_map, expected_x, original_y), "Saved map/position did not survive failure")
        require(loaded["wPlayerGender"] == saved["symbols"]["wPlayerGender"], "Saved character changed")
    if args.singleplayer_rom:
        directory = out / 'singleplayer-continue'
        cmd = command(args, args.singleplayer_rom, args.singleplayer_symbols, directory, battery, 4200,
                      seed + '3750 RIGHT 12\n', adapter=0)
        with (directory / 'process.log').open('w') as log:
            subprocess.run(cmd, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=45)
        loaded = json.loads((directory / 'state.json').read_text())['symbols']
        require(loaded['hCrashCode'] == 0 and loaded['wSaveFileExists'] == 1 and
                loaded['wPlayerGender'] == saved['symbols']['wPlayerGender'] and
                (loaded['wMapGroup'], loaded['wMapNumber'], loaded['wXCoord'], loaded['wYCoord']) ==
                (*original_map, original_x + 2, original_y), 'Disabled counterpart could not Continue and move')
    require(digest(battery) == before, "Fresh boot modified input battery")
    return {"case": case, "fault_during_save_at_frame": fault_frame, 'actual_clients': peer_count + 1,
            "save_completed_at_frame": saved["frame"], "battery": battery_report,
            'same_state_save_copy': {k: v for k, v in profile.items() if k.startswith('save_')} if profile else None,
            'disabled_counterpart_continue': bool(args.singleplayer_rom),
            "passed": True}


def verify_native_palette_save(args, reports):
    """Compare serialized palette state against an actual disabled-ROM save."""
    directory = args.output / 'native-palette-control'
    seed = (REPO / 'mobile_adapter/tools/fixtures/continue.input').read_text()
    script = seed + '3550 RIGHT 12\n3750 START 8\n3850 DOWN 5\n3950 DOWN 5\n4050 A 5\n'
    script += ''.join(f'{frame} A 5\n' for frame in range(4250, 5350, 100))
    script += '5550 B 5\n5800 RIGHT 12\n'
    cmd = command(args, args.singleplayer_rom, args.singleplayer_symbols, directory,
                  args.battery, 6500, script, adapter=0)
    with (directory / 'process.log').open('w') as log:
        subprocess.run(cmd, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=45)
    native = read_symbols(args.singleplayer_symbols)
    mobile = read_symbols(args.symbols)
    control = (directory / 'ingame.sav').read_bytes()
    validate_battery(directory / 'ingame.sav', native)
    ranges = (('wUsedObjectPals', 'wEmotePal'),
              ('wNeededMonPalLight', 'wObjectGlowFadeActive'))

    def palette_bytes(data, bindings, section, first, last):
        bank, address = bindings[section]
        offset = bank * 0x2000 + address - 0xa000 - bindings['wPlayerData'][1]
        return data[offset + bindings[first][1]:offset + bindings[last][1] + 1]

    for report in reports:
        data = (args.output / report['case'] / 'primary/ingame.sav').read_bytes()
        for section in ('sPlayerData', 'sBackupPlayerData'):
            for first, last in ranges:
                expected = palette_bytes(control, native, section, first, last)
                observed = palette_bytes(data, mobile, section, first, last)
                require(observed == expected,
                        f"{report['case']}: {section} palette range {first}..{last} differs "
                        f"from native Save: {observed.hex()} != {expected.hex()}")
        report['native_palette_save_equal'] = True
    return {'actual_disabled_rom_save': True, 'compared_primary_and_backup': True,
            'ranges': [list(pair) for pair in ranges], 'passed': True}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("runner", "core", "server", "battery", "peer-battery", "output"):
        parser.add_argument("--" + name, type=Path, required=True)
    parser.add_argument("--rom", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.gbc")
    parser.add_argument("--symbols", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.sym")
    parser.add_argument("--port", type=int, default=17510)
    parser.add_argument("--cases", default=",".join(CASES))
    parser.add_argument('--extra-peer-battery', type=Path, action='append', default=[])
    parser.add_argument('--profile-core', type=Path, help='Read-only instruction observer for exact same-state save-copy checks')
    parser.add_argument('--singleplayer-rom', type=Path, help='Matching disabled build for bidirectional ordinary-save compatibility')
    parser.add_argument('--singleplayer-symbols', type=Path)
    parser.add_argument('--expected-singleplayer-sha256', help='Require the immutable disabled counterpart digest')
    args = parser.parse_args()
    if args.profile_core:
        args.profile_core = args.profile_core.resolve()
    require(bool(args.singleplayer_rom) == bool(args.singleplayer_symbols), 'Provide both disabled counterpart paths')
    if args.singleplayer_rom:
        args.singleplayer_rom = args.singleplayer_rom.resolve()
        args.singleplayer_symbols = args.singleplayer_symbols.resolve()
    require(len(args.extra_peer_battery) <= 2, 'At most four actual clients are supported')
    args.extra_peer_battery = [path.resolve() for path in args.extra_peer_battery]
    for name in ("runner", "core", "server", "battery", "peer_battery", "output", "rom", "symbols"):
        setattr(args, name, getattr(args, name).resolve())
    require(not args.output.exists(), "Choose a fresh artifact directory")
    args.output.mkdir(parents=True)
    rom = args.output / "input.gbc"
    symbols = args.output / "input.sym"
    rom.write_bytes(args.rom.read_bytes())
    symbols.write_bytes(args.symbols.read_bytes())
    args.rom, args.symbols = rom, symbols
    # A disposable native-control build must not change a ROM midway through
    # an independent Save/Continue oracle. Snapshot BOTH variants before boot.
    if args.singleplayer_rom:
        native_rom = args.output / 'singleplayer.gbc'
        native_symbols = args.output / 'singleplayer.sym'
        native_rom.write_bytes(args.singleplayer_rom.read_bytes())
        native_symbols.write_bytes(args.singleplayer_symbols.read_bytes())
        args.singleplayer_rom, args.singleplayer_symbols = native_rom, native_symbols
        if args.expected_singleplayer_sha256:
            require(digest(native_rom) == args.expected_singleplayer_sha256,
                    'Disabled counterpart does not match the expected native ROM')
    inputs = [args.battery, args.peer_battery, *args.extra_peer_battery]
    hashes = [digest(path) for path in inputs]
    cases = args.cases.split(",")
    require(cases and all(case in CASES for case in cases), "Unknown save safety case")
    reports = [run_case(args, case, index) for index, case in enumerate(cases)]
    palette_control = verify_native_palette_save(args, reports) if args.singleplayer_rom else None
    require([digest(path) for path in inputs] == hashes, "An input save changed")
    report = {"rom_sha256": digest(args.rom), "core_sha256": digest(args.core), "cases": reports,
              "native_palette_control": palette_control, "passed": True, "limitations": ["Finite emulator checks do not prove absence of every possible defect",
                  "Frontend disk-flush failures and power loss during the native save write are separate risks"]}
    if args.singleplayer_rom:
        report["disabled_rom_sha256"] = digest(args.singleplayer_rom)
    (args.output / "evidence.json").write_text(json.dumps(report, indent=2) + "\n")
    print("Online Save, failure recovery, checksums and fresh offline Continue passed.")


if __name__ == "__main__":
    main()
