#!/usr/bin/env python3
"""Actual-ROM TCP acceptance with isolated, paced native emulator processes.

No save states or RAM writes. This command launches emulators explicitly and is
separate from host-only tests. All artifacts survive failures for diagnosis.
"""
import argparse
from concurrent.futures import ThreadPoolExecutor
import json
from pathlib import Path
import re
import socket
import subprocess
import time
from probe_emulator import convert_png, digest, read_symbols, require, trace_text, validate_battery

REPO = Path(__file__).resolve().parents[2]
FIXTURES = Path(__file__).resolve().parent / "fixtures"
CASES = ("reconnect", "three-clients", "split", "bad-magic", "silent", "close",
         "no-server", "no-adapter", "cancel", "recover", "oversize", "truncated")


def script_for(case):
    script = (FIXTURES / "continue.input").read_text()
    script += "\nON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 SELECT 8\n"
    script += "ON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 B 8\n"
    script += "ON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 A 8\n"
    if case == "reconnect":
        script += "ON 3500 wMobileSuccessCount=1,wMobileOwner=0 SELECT 8\n"
        script += "ON 3500 wMobileSuccessCount=1,wMobileOwner=0 B 8\n"
        script += "ON 3500 wMobileSuccessCount=1,wMobileOwner=0 A 8\n"
        script += "ON 3750 wMobileSuccessCount=2,wMobileOwner=0 RIGHT 20\n"
    elif case == "recover":
        script += "ON 3600 wMobileError=5,wMobileCooldown=0,wMobileOwner=0 SELECT 8\n"
        script += "ON 3600 wMobileError=5,wMobileCooldown=0,wMobileOwner=0 B 8\n"
        script += "ON 3600 wMobileError=5,wMobileCooldown=0,wMobileOwner=0 A 8\n"
        script += "ON 3750 wMobileSuccessCount=1,wMobileOwner=0 RIGHT 20\n"
    else:
        error = {"bad-magic": 5, "silent": 2, "close": 3, "no-server": 4,
                 "no-adapter": 4, "cancel": 1, "oversize": 5, "truncated": 3}.get(case, 0)
        if case == "cancel":
            script += "ON 3320 wMobileStep=4,wMobileOwner=1 SELECT 8\n"
            script += "ON 3320 wMobileStep=4,wMobileOwner=1 B 8\n"
        script += f"ON 3500 wMobileError={error},wMobileOwner=0,wMobileAttemptCount=1 RIGHT 20\n"
    return script


def run_client(args, case, directory, profile=False):
    directory.mkdir()
    checkpoints = directory / "checkpoints"
    checkpoints.mkdir()
    script = directory / "input.txt"
    script.write_text(script_for(case))
    frames = 4250 if case == "silent" else 4000
    command = [str(args.runner), str(args.profile_core if profile else args.core), str(args.rom),
               str(directory / "screen.ppm"), str(directory), "--frames", str(frames),
               "--input", str(script), "--symbols", str(args.symbols),
               "--state-out", str(directory / "state.json"), "--trace-out", str(directory / "trace.jsonl.gz"),
               "--checkpoint-dir", str(checkpoints), "--save-in", str(args.battery),
               "--pace-from", "3250", "--adapter", "0" if case == "no-adapter" else "1"]
    if profile:
        command += ["--profile-out", str(directory / "profile.json")]
    with (directory / "process.log").open("w") as log:
        subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=45)
    state = json.loads((directory / "state.json").read_text())["symbols"]
    expected_error = {"bad-magic": 5, "silent": 2, "close": 3, "no-server": 4,
                      "no-adapter": 4, "cancel": 1, "oversize": 5, "truncated": 3}.get(case, 0)
    successes = 2 if case == "reconnect" else int(expected_error == 0)
    attempts = 2 if case in ("reconnect", "recover") else 1
    for key, expected in {"hCrashCode": 0, "hCGB": 1, "hInMenu": 0, "wScriptRunning": 0,
                          "wMobileOwner": 0, "wMobileTransferActive": 0, "wMobileWire": 0,
                          "wMobileError": expected_error, "wMobileSuccessCount": successes,
                          "wMobileRoundTrips": successes, "wMobileAttemptCount": attempts,
                          "wMapGroup": 24, "wMapNumber": 5, "wXCoord": 4, "wYCoord": 3,
                          "wPlayerMapX": 8, "wPlayerMapY": 7,
                          "wMobileGuardStart": 165, "wMobileGuardTx": 165,
                          "wMobileGuardRx": 165, "wMobileGuardEnd": 165}.items():
        require(state[key] == expected, f"{case}: {key}={state[key]}, expected {expected}")
    trace = [json.loads(line) for line in trace_text(directory / "trace.jsonl").splitlines()]
    # Adapter absence may claim and release serial between video observations.
    # The attempt, error, cleanup, guards and subsequent movement above still
    # prove this fast failure path. Live adapter scenarios must expose ownership.
    if case != "no-adapter":
        require(any(row["symbols"].get("wMobileOwner") == 1 for row in trace), "Never claimed serial")
    else:
        require(state["wMobileIRQCount"] > 0, "Missing adapter detection exchange")
    if case == "recover":
        require(any(row["symbols"].get("wMobileError") == 5 for row in trace), "Recovery never encountered fault")
    images = sorted(checkpoints.glob("*.ppm"))
    require(len(images) >= (2 if case == "no-adapter" else 3), "Missing intermediate checkpoints")
    for ppm in images + [directory / "screen.ppm"]:
        convert_png(ppm, ppm.with_suffix(".png"))
    result = {"case": case, "frame_calls": frames, "input_sha256": digest(script),
              "symbols": state, "checkpoints": len(images)}
    if profile:
        metrics = json.loads((directory / "profile.json").read_text())
        require(metrics["serial_count"] >= state["wMobileIRQCount"], "Missing IRQ samples")
        require(metrics["tick_count"] > 1000, "Missing foreground samples")
        require(metrics["serial_failures"] == metrics["tick_failures"] == 0, "Register/bank preservation failure")
        require(min(metrics["serial_lowest_sp"], metrics["tick_lowest_sp"]) >= 0xc040, "Insufficient stack headroom")
        require(metrics["serial_max_cycles"] < 2048, "ISR exceeds instruction-cycle budget")
        require(metrics["tick_max_cycles"] < 8192, "Foreground exceeds instruction-cycle budget")
        frame_cycles = 70224 if metrics["serial_normal_speed_count"] else 140448
        require(metrics["serial_max_vblank_cycles"] < frame_cycles // 2,
                "Serial uses over half the actual VBlank interval")
        require(metrics["audio_batches"] >= frames * 0.9, "Missing audio callbacks")
        result["profile"] = metrics
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for option in ("runner", "core", "server", "battery", "output"):
        parser.add_argument("--" + option, type=Path, required=True)
    parser.add_argument("--profile-core", type=Path)
    parser.add_argument("--rom", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.gbc")
    parser.add_argument("--symbols", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.sym")
    parser.add_argument("--cases", default=",".join(CASES))
    parser.add_argument("--port", type=int, default=17423)
    args = parser.parse_args()
    for name in ("runner", "core", "profile_core", "server", "battery", "output", "rom", "symbols"):
        value = getattr(args, name)
        if value is not None:
            setattr(args, name, value.resolve())
    require(not args.output.exists(), "Output exists; choose a new directory")
    args.output.mkdir(parents=True)
    require(1 <= args.port <= 65535, "Invalid integration-test port")
    if args.port != 17423:
        bank, address = read_symbols(args.symbols)["MobileEndpoint"]
        offset = bank * 0x4000 + address - 0x4000
        data = bytearray(args.rom.read_bytes())
        require(data[offset:offset + 6] == bytes([127, 0, 0, 1]) + (17423).to_bytes(2, "big"),
                "Expected the feature-owned default endpoint")
        data[offset + 4:offset + 6] = args.port.to_bytes(2, "big")
        data[0x14e:0x150] = ((sum(data[:0x14e]) + sum(data[0x150:])) & 65535).to_bytes(2, "big")
        args.rom = args.output / "test.gbc"
        args.rom.write_bytes(data)
    battery = validate_battery(args.battery, read_symbols(args.symbols))
    probe = socket.socket()
    try:
        probe.bind(("127.0.0.1", args.port))
    except OSError as error:
        raise ValueError(f"Port {args.port} is already in use; will not stop another process") from error
    finally:
        probe.close()
    results = []
    selected = args.cases.split(",")
    require(all(case in CASES for case in selected), "Unknown scenario")
    for case in selected:
        print(f"RUN {case}", flush=True)
        directory = args.output / case
        directory.mkdir()
        fault = {"cancel": "silent", "recover": "bad-magic"}.get(case, case)
        server_command = [str(args.server), "--bind", f"127.0.0.1:{args.port}"]
        if fault in ("split", "bad-magic", "silent", "close", "oversize", "truncated"):
            server_command += ["--test-fault", fault]
        if case == "recover":
            server_command += ["--test-fault-count", "1"]
        process = None
        with (directory / "server.log").open("w") as log:
            try:
                if case != "no-server":
                    process = subprocess.Popen(server_command, stdout=log, stderr=subprocess.STDOUT)
                    deadline = time.monotonic() + 5
                    while f"READY 127.0.0.1:{args.port}" not in (directory / "server.log").read_text():
                        require(process.poll() is None, "Server exited before readiness")
                        require(time.monotonic() < deadline, "Server readiness deadline exceeded")
                        time.sleep(0.02)
                if case == "three-clients":
                    with ThreadPoolExecutor(max_workers=3) as pool:
                        futures = [pool.submit(run_client, args, case, directory / str(i)) for i in range(3)]
                        group = [future.result() for future in futures]
                else:
                    group = [run_client(args, case, directory / "client", case == "reconnect" and bool(args.profile_core))]
                results += group
            finally:
                if process:
                    process.terminate()
                    try:
                        process.wait(timeout=5)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        process.wait()
        echo = re.findall(r"ECHO session=(\d+) sequence=(\d+) bytes=4", (directory / "server.log").read_text())
        expected = {"reconnect": 2, "three-clients": 3, "recover": 2,
                    "no-server": 0, "no-adapter": 0}.get(case, 1)
        require(len(echo) == expected, f"{case}: {len(echo)} server echoes, expected {expected}")
        if case == "three-clients":
            require(len({session for session, _ in echo}) == 3, "Clients did not use independent sockets")
        if case in ("reconnect", "recover"):
            require([int(sequence) for _, sequence in echo] == [1, 2], "Reconnect sequence was stale")
        print(f"PASS {case}", flush=True)
    require(digest(args.battery) == battery["sha256"], "Input battery was modified")
    report = {"rom_sha256": digest(args.rom), "symbols_sha256": digest(args.symbols),
              "runner_sha256": digest(args.runner), "core_sha256": digest(args.core),
              "profile_core_sha256": digest(args.profile_core) if args.profile_core else None,
              "server_sha256": digest(args.server), "battery": battery, "results": results}
    (args.output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    print(f"Transport acceptance passed: {len(results)} native game processes", flush=True)


if __name__ == "__main__":
    main()
