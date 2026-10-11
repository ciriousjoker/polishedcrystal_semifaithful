#!/usr/bin/env python3
"""Save normally while online, inject failures, then boot the exported battery.

Runs real ROMs with ordinary input and read-only observers. Does not load an
emulator state, patch save data, or write game RAM. Each case owns its server.
"""
import argparse
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
CASES = ("peer-disconnect", "server-close", "malformed")


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
    if not path.exists():
        return []
    # The writer may currently be appending a line. Ignore only incomplete JSON.
    rows = []
    for line in path.read_text().splitlines():
        try:
            rows.append(json.loads(line))
        except json.JSONDecodeError:
            continue
    return rows


def command(args, rom, symbols, directory, battery, frames, script, adapter=1):
    directory.mkdir()
    (directory / "input.txt").write_text(script)
    return [str(args.runner), str(args.core), str(rom), str(directory / "screen.ppm"), str(directory),
            "--frames", str(frames), "--input", str(directory / "input.txt"), "--symbols", str(symbols),
            "--save-in", str(battery), "--save-out", str(directory / "ingame.sav"),
            "--state-out", str(directory / "state.json"), "--trace-out", str(directory / "trace.jsonl"),
            "--adapter", str(adapter)]


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
    expected_checksum = validate_battery(args.battery, read_symbols(symbols))["checksums"]["s"]
    proxy = FaultProxy(port, port + 1) if case == "malformed" else None
    server_port = port + 1 if proxy else port
    seed = (REPO / "mobile_adapter/tools/fixtures/continue.input").read_text()
    join = "\nON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 SELECT 8\nON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 B 8\n"
    primary = seed + join + "ON 3550 wMobileOnline=1,wMobilePeerCount=1 RIGHT 12\n"
    primary += "3750 START 8\n3850 DOWN 5\n3950 DOWN 5\n4050 A 5\n"
    for frame in range(4250, 5350, 100):
        primary += f"{frame} A 5\n"
    primary += "5550 B 5\n5800 RIGHT 12\n"
    peer = seed + join + "ON 3550 wMobileOnline=1,wMobilePeerCount=1 DOWN 12\n"
    processes = []
    handles = []
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
            for name, battery, script in (("primary", args.battery, primary), ("peer", args.peer_battery, peer)):
                directory = out / name
                cmd = command(args, rom, symbols, directory, battery, 6500, script)
                cmd += ["--pace-from", "3250"]
                handle = (directory / "process.log").open("w")
                handles.append(handle)
                processes.append(subprocess.Popen(cmd, stdout=handle, stderr=subprocess.STDOUT))
            deadline = time.monotonic() + 75
            saved = None
            while saved is None:
                require(processes[0].poll() is None and time.monotonic() < deadline,
                        "Connected in-game save did not complete before the deadline")
                for row in observations(out / "primary/trace.jsonl"):
                    state = row["symbols"]
                    if (row["frame"] >= 4050 and state["sChecksum"] != expected_checksum and
                            state["wMobileOnline"] == 1 and state["wScriptRunning"] == 0 and
                            state["hInMenu"] == 0):
                        saved = row
                        break
                time.sleep(.1)
            if case == "peer-disconnect":
                processes[1].terminate()
                processes[1].wait(timeout=5)
            elif case == "server-close":
                server.terminate()
                server.wait(timeout=5)
            else:
                proxy.armed.set()
            require(processes[0].wait(timeout=40) == 0, "Primary failed after the injected fault")
            if proxy:
                require(proxy.injected.is_set(), "No malformed packet reached the client")
        finally:
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
    require((state["wXCoord"], state["wYCoord"]) == (5, 3), "Movement after fault failed")
    require(state["sChecksum"] == saved["symbols"]["sChecksum"], "Fault changed saved game data")
    rows = observations(out / "primary/trace.jsonl")
    require(any(row["symbols"]["wMenuSelection"] == 4 for row in rows), "Normal Save option was not selected")
    if case == "peer-disconnect":
        require(state["wMobileOnline"] == 1 and state["wMobilePeerCount"] == 0,
                "Disconnected peer remained in the lobby")
    else:
        require(any(row["frame"] > saved["frame"] and row["symbols"]["wMobileError"] == 5
                    for row in rows) if case == "malformed" else
                any(row["frame"] > saved["frame"] and row["symbols"]["wMobileError"] for row in rows),
                "Injected fault was not observed by the ROM")
    battery = out / "primary/ingame.sav"
    battery_report = validate_battery(battery, read_symbols(symbols))
    before = digest(battery)
    for name, frames, script, expected_x in (("continue", 3500, seed, 4),
                                            ("move", 4200, seed + "3750 RIGHT 12\n", 5)):
        directory = out / name
        cmd = command(args, rom, symbols, directory, battery, frames, script, adapter=0)
        with (directory / "process.log").open("w") as log:
            subprocess.run(cmd, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=45)
        loaded = json.loads((directory / "state.json").read_text())["symbols"]
        require(loaded["hCrashCode"] == loaded["wMobileOwner"] == 0 and loaded["wSaveFileExists"] == 1,
                "Fresh Continue failed or restored a live transport owner")
        require((loaded["wMapGroup"], loaded["wMapNumber"], loaded["wXCoord"], loaded["wYCoord"]) ==
                (24, 5, expected_x, 3), "Saved map/position did not survive failure")
        require(loaded["wPlayerGender"] == saved["symbols"]["wPlayerGender"], "Saved character changed")
    require(digest(battery) == before, "Fresh boot modified input battery")
    return {"case": case, "saved_online_at_frame": saved["frame"], "battery": battery_report, "passed": True}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("runner", "core", "server", "battery", "peer-battery", "output"):
        parser.add_argument("--" + name, type=Path, required=True)
    parser.add_argument("--rom", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.gbc")
    parser.add_argument("--symbols", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.sym")
    parser.add_argument("--port", type=int, default=17510)
    parser.add_argument("--cases", default=",".join(CASES))
    args = parser.parse_args()
    for name in ("runner", "core", "server", "battery", "peer_battery", "output", "rom", "symbols"):
        setattr(args, name, getattr(args, name).resolve())
    require(not args.output.exists(), "Choose a fresh artifact directory")
    args.output.mkdir(parents=True)
    rom = args.output / "input.gbc"
    symbols = args.output / "input.sym"
    rom.write_bytes(args.rom.read_bytes())
    symbols.write_bytes(args.symbols.read_bytes())
    args.rom, args.symbols = rom, symbols
    hashes = [digest(args.battery), digest(args.peer_battery)]
    cases = args.cases.split(",")
    require(cases and all(case in CASES for case in cases), "Unknown save safety case")
    reports = [run_case(args, case, index) for index, case in enumerate(cases)]
    require([digest(args.battery), digest(args.peer_battery)] == hashes, "An input save changed")
    report = {"rom_sha256": digest(args.rom), "core_sha256": digest(args.core), "cases": reports,
              "passed": True, "limitations": ["Finite emulator checks do not prove absence of every possible defect",
                  "Frontend disk-flush failures and power loss during the native save write are separate risks"]}
    (args.output / "evidence.json").write_text(json.dumps(report, indent=2) + "\n")
    print("Online Save, failure recovery, checksums and fresh offline Continue passed.")


if __name__ == "__main__":
    main()
