#!/usr/bin/env python3
"""Run three actual ROMs using ordinary battery saves and read-only observers.

No emulator save state or injected game RAM. Keep all artifacts for diagnosis.
"""
import argparse
from collections import deque
from concurrent.futures import ThreadPoolExecutor
import json
import math
from pathlib import Path
import socket
import select
import socketserver
import subprocess
import threading
import time
from probe_emulator import convert_png, digest, read_symbols, require, validate_battery

REPO = Path(__file__).resolve().parents[2]


class DropOnceProxy(socketserver.ThreadingTCPServer):
    """Transparent test hop, dropping one socket after live presence starts."""
    daemon_threads = True
    allow_reuse_address = True

    def __init__(self, port, upstream, drop=True, delay_ms=0):
        self.drop_enabled = drop
        self.delay = delay_ms / 1000
        self.upstream = upstream
        self.lock = threading.Lock()
        self.claimed = False
        self.dropped = False
        super().__init__(("127.0.0.1", port), ProxyHandler)


class ProxyHandler(socketserver.BaseRequestHandler):
    def handle(self):
        with self.server.lock:
            drop = self.server.drop_enabled and not self.server.claimed
            self.server.claimed = True
        with socket.create_connection(("127.0.0.1", self.server.upstream)) as upstream:
            self.request.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
            upstream.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
            started = time.monotonic()
            try:
                queues = {self.request: deque(), upstream: deque()}
                queued = {self.request: 0, upstream: 0}
                while True:
                    now = time.monotonic()
                    if drop and now - started > 1.5:
                        self.server.dropped = True
                        return
                    readable, _, _ = select.select(
                        [stream for stream in queues if queued[stream] < 65536], [], [], .002)
                    for stream in readable:
                        data = stream.recv(min(4096, 65536 - queued[stream]))
                        if not data:
                            return
                        queues[stream].append((time.monotonic() + self.server.delay, data))
                        queued[stream] += len(data)
                    for source, queue in queues.items():
                        destination = upstream if source is self.request else self.request
                        while queue and queue[0][0] <= time.monotonic():
                            _, data = queue.popleft()
                            destination.sendall(data)
                            queued[source] -= len(data)
            except (ConnectionError, OSError):
                return


def latency_results(args, states):
    events = [json.loads(line) for line in (args.output / "0/input-events.jsonl").read_text().splitlines()]
    events = [event for event in events if event["frame"] >= 3650 and event["button"] in (6, 7)]
    require(len(events) == 20, "Missing paced movement input samples")
    player = states[0]["wMobilePlayerId"]
    samples = []
    totals = []
    local = [json.loads(line) for line in (args.output / "0/trace.jsonl").read_text().splitlines()]
    for client in (1, 2):
        trace = [json.loads(line) for line in (args.output / str(client) / "trace.jsonl").read_text().splitlines()]
        for event in events:
            before = None
            for candidate in reversed(trace):
                if candidate["wall_us"] >= event["wall_us"]:
                    continue
                for index in (1, 2):
                    if (candidate["symbols"][f"wMobilePeer{index}Id"] == player
                            and 0 < candidate["symbols"][f"vMobilePeer{index}OAM0"] < 160):
                        before, peer = candidate, str(index)
                        break
                if before is not None:
                    break
            require(before is not None and event["wall_us"] - before["wall_us"] < 200000,
                    "Missing recent visible avatar baseline")
            keys = [f"vMobilePeer{peer}OAM{index}" for index in range(16)]
            def left_edge(row):
                return min(row["symbols"][keys[index]] for index in (1, 5, 9, 13))
            previous = left_edge(before)
            changed = next(row for row in trace if row["wall_us"] >= event["wall_us"]
                           and 0 < row["symbols"][keys[0]] < 160
                           and (left_edge(row) - previous) * (1 if event["button"] == 7 else -1) > 0)
            require(0 < changed["symbols"][keys[0]] < 160, "Latency sample was offscreen")
            start = [row for row in local if row["wall_us"] < event["wall_us"]][-1]["symbols"]["vMobileLocalX"]
            motion = next(row for row in local if row["wall_us"] >= event["wall_us"]
                          and (row["symbols"]["vMobileLocalX"] - start) * (1 if event["button"] == 7 else -1) > 0)
            samples.append((changed["wall_us"] - motion["wall_us"]) / 1000)
            totals.append((changed["wall_us"] - event["wall_us"]) / 1000)
    samples.sort()
    totals.sort()
    return {"samples": len(samples), "p50_ms": samples[len(samples) // 2],
            "p95_ms": samples[math.ceil(len(samples) * .95) - 1], "max_ms": samples[-1],
            "added_client_server_rtt_ms": 2 * args.test_one_way_delay_ms,
            "input_to_remote_p95_ms": totals[math.ceil(len(totals) * .95) - 1],
            "input_to_remote_max_ms": totals[-1],
            "measurement": "first captured local world motion to first directional position change of four-quad remote avatar bounds at native video callback; input-to-remote includes unchanged vanilla turn delay"}


def run_client(args, index):
    directory = args.output / str(index)
    directory.mkdir()
    script = (REPO / "mobile_adapter/tools/fixtures/continue.input").read_text()
    script += "\nON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 SELECT 8\n"
    script += "ON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 B 8\n"
    if not args.deferred and args.scenario != "camera":
        script += f"ON 3550 wMobileOnline=1,wMobilePeerCount=2 {('RIGHT', 'DOWN', 'LEFT')[index]} 12\n"
    if index == 0 and args.scenario == "menu":
        script += "3750 SELECT 8\n3750 A 8\n"
        script += "ON 3830 wMobileMenuState=1,wMenuCursorY=1 DOWN 1\n"
        script += "ON 3880 wMobileMenuState=1,wMenuCursorY=2 A 8\n"
        script += "ON 4000 wMobileMenuState=1,wMenuCursorY=2 B 8\n"
        for frame in range(4120, 4960, 120):
            script += f"{frame} B 8\n"
    if index == 0 and args.scenario == "start-menu":
        script += "3750 START 8\n4250 B 8\n"
    if index == 0 and args.scenario == "rejoin":
        script += "3750 SELECT 8\n3750 B 8\n"
        script += "ON 3850 wMobileOnline=0,wMobileOwner=0 SELECT 8\n"
        script += "ON 3850 wMobileOnline=0,wMobileOwner=0 B 8\n"
    if index == 0 and args.scenario == "warp":
        script += "3750 RIGHT 80\n3900 UP 80\n"
        for frame in range(4040, 4670, 40):
            script += f"{frame} A 8\n"
    if index == 0 and args.scenario == "connection":
        script += "3750 UP 32\n"
    if index == 0 and args.scenario == "running":
        script += "3750 B 24\n3750 RIGHT 24\n3900 LEFT 24\n"
    if index == 0 and args.scenario == "speed":
        for step in range(24):
            script += f"{3650 + step * 96} {('LEFT', 'RIGHT')[step % 2]} 72\n"
        script += "6100 LEFT 48\n"
    if index == 0 and args.scenario == "camera":
        script += "3750 UP 8\n3800 A 8\n3950 A 8\n4100 A 8\n"
        script += "ON 4250 wPanningAroundTinyMap=1,wScriptRunning=0 UP 128\n"
        script += "ON 4450 wPanningAroundTinyMap=1,wScriptRunning=0 DOWN 180\n"
        script += "4750 A 8\n"
    if args.latency and index == 0:
        for step in range(20):
            script += f"{3650 + step * 90} {('LEFT', 'RIGHT')[step % 2]} 12\n"
    (directory / "input.txt").write_text(script)
    frames = (6400 if args.latency else 6000 if args.scenario == "camera" else
              5200 if args.scenario in ("menu", "start-menu", "resume") else
              4800 if args.scenario in ("warp", "connection") else 4500)
    speed = 3 if index == 0 and args.scenario == "speed" else 1
    frames = 3250 + (frames - 3250) * speed
    command = [str(args.runner), str(args.core), str(args.rom), str(directory / "screen.ppm"),
               str(directory), "--frames", str(frames), "--pace-multiplier", str(speed),
               "--input", str(directory / "input.txt"),
               "--symbols", str(args.symbols), "--state-out", str(directory / "state.json"),
               "--trace-out", str(directory / "trace.jsonl"), "--save-in", str(args.batteries[index]),
               "--pace-from", "3250", "--adapter", "1",
               "--input-events", str(directory / "input-events.jsonl"),
               "--finish-barrier", str(args.output / "finish-barrier")]
    if args.profile:
        command += ["--profile-out", str(directory / "profile.json")]
    with (directory / "process.log").open("w") as log:
        subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=90)
    state = json.loads((directory / "state.json").read_text())["symbols"]
    for key, expected in {"hCrashCode": 0, "wMobileOnline": 1, "wMobilePeerCount": 2,
                          "wMobileRenderedCount": (0 if args.deferred else
                              (0 if index == 0 else 1) if args.scenario in ("warp", "connection") else
                              (0 if index == 2 else 1) if args.scenario == "instance" else 2),
                          "wMobileDeferred": args.deferred,
                          "wMobileError": 0, "wMobileServerError": 0,
                          "wMobileGuardStart": 165, "wMobileGuardTx": 165,
                          "wMobileGuardRx": 165, "wMobileGuardEnd": 165}.items():
        require(state[key] == expected, f"Client {index}: {key}={state[key]}, expected {expected}")
    # A stationary protected scene can receive just one full roster. Heartbeats
    # intentionally do not produce redundant pushed state.
    require(state["wMobilePresenceCount"] >= (1 if args.deferred else 2) and state["wMobileSequence"] > 3,
            "Missing live presence traffic")
    if args.profile:
        metrics = json.loads((directory / "profile.json").read_text())
        require(metrics["serial_count"] > 1000 and metrics["tick_count"] > 1000, "Insufficient instruction samples")
        require(metrics["serial_failures"] == metrics["tick_failures"] == 0, "Register/bank preservation failure")
        require(metrics["npc_restore_count"] > 100 and metrics["npc_restore_failures"] == 0,
                "NPC restore boundary observer failed")
        require(min(metrics["serial_lowest_sp"], metrics["tick_lowest_sp"]) >= 0xc040, "Insufficient stack headroom")
        require(metrics["serial_max_cycles"] < 2048 and metrics["tick_max_cycles"] < 32768,
                "Lobby instruction budget exceeded")
        frame_cycles = 70224 if metrics["serial_normal_speed_count"] else 140448
        require(metrics["serial_max_vblank_cycles"] < frame_cycles // 2,
                "Serial uses over half the actual VBlank interval")
    convert_png(directory / "screen.ppm", directory / "screen.png")
    return state


def byte_hash(data):
    value = 2166136261
    for byte in data:
        value = ((value ^ byte) * 16777619) & 0xffffffff
    return value


def verify_avatar_graphics(states):
    figures = ("chris", "kris", "crys", "beta")
    modes = ("", "_run", "_bike", "", "_surf", "")
    for state in states:
        for index in (1, 2):
            peer = f"wMobilePeer{index}"
            if not state[peer + "Visible"]:
                continue
            mode = state[peer + "Movement"]
            name = "surf_pikachu" if mode == 5 else figures[state[peer + "Avatar"]] + modes[mode]
            source = (REPO / "gfx/sprites" / (name + ".2bpp")).read_bytes()
            require(len(source) == 384, "Remote player sprite size changed")
            require(state[f"vMobilePeer{index}CacheHash"] == byte_hash(source),
                    "Remote graphics cache differs from the selected vanilla sprite")
            frame = state[peer + "LoadedFrame"]
            require(frame in (0, 4, 8, 12, 16, 20), "Invalid remote source frame")
            require(state[f"vMobilePeer{index}TileHash"] == byte_hash(source[frame * 16:(frame + 4) * 16]),
                    "Displayed remote VRAM tiles differ from the selected vanilla frame")
            palette = state[peer + "Palette"]
            require(state[f"wLoadedObjPal{palette}"] == state[peer + "Avatar"],
                    "Remote palette uses another player figure's colors")


def verify_npc_motion(output):
    for index in range(3):
        rows = [json.loads(line) for line in (output / str(index) / "trace.jsonl").read_text().splitlines()]
        stationary = [row["symbols"] for row in rows if row["frame"] >= 3800
                      and row["symbols"]["wMobileOnline"]
                      and row["symbols"]["wPlayerWalking"] == 255
                      and row["symbols"]["wScriptRunning"] == 0]
        by_occupancy = {}
        for row in stationary:
            by_occupancy.setdefault(row["vNPCOccupiedMask"], set()).add(row["vNPCPositionsHash"])
        require(any(len(hashes) > 1 for hashes in by_occupancy.values()),
                f"Client {index}: no ordinary NPC moved while the player and actor membership were unchanged")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("runner", "core", "server", "battery", "output"):
        parser.add_argument("--" + name, type=Path, required=True)
    parser.add_argument("--rom", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.gbc")
    parser.add_argument("--symbols", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.sym")
    parser.add_argument("--latency", action="store_true")
    parser.add_argument("--test-one-way-delay-ms", type=int, default=0)
    parser.add_argument("--profile", action="store_true")
    parser.add_argument("--port", type=int, default=17425)
    parser.add_argument("--scenario", choices=("basic", "menu", "start-menu", "rejoin", "warp", "connection", "running", "camera", "resume", "instance", "speed"), default="basic")
    parser.add_argument("--second-battery", type=Path, help="Second player's ordinary character-choice save")
    parser.add_argument("--require-npc-motion", action="store_true")
    parser.add_argument("--different-battery", type=Path, help="Third player's ordinary save for instance isolation")
    parser.add_argument("--movement-mode", type=int, choices=range(6), help="Assert ordinary scene's movement state")
    parser.add_argument("--exercise-pressure", action="store_true", help="Require optional rendering pressure in the chosen dense scene")
    parser.add_argument("--deferred", type=int, choices=(0, 1, 2, 3), default=0)
    args = parser.parse_args()
    for name in ("runner", "core", "server", "battery", "output", "rom", "symbols"):
        setattr(args, name, getattr(args, name).resolve())
    require(not args.output.exists(), "Choose a fresh artifact directory")
    args.output.mkdir(parents=True)
    symbols_copy = args.output / "test.sym"
    symbols_copy.write_bytes(args.symbols.read_bytes())
    args.symbols = symbols_copy
    args.batteries = [args.battery, args.second_battery.resolve() if args.second_battery else args.battery, args.different_battery.resolve() if args.different_battery else args.battery]
    for battery in args.batteries:
        validate_battery(battery, read_symbols(args.symbols))
    battery_hashes = [digest(battery) for battery in args.batteries]
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", args.port))
    bank, address = read_symbols(args.symbols)["MobileEndpoint"]
    offset = bank * 0x4000 + address - 0x4000
    data = bytearray(args.rom.read_bytes())
    data[offset + 4:offset + 6] = args.port.to_bytes(2, "big")
    data[0x14e:0x150] = ((sum(data[:0x14e]) + sum(data[0x150:])) & 65535).to_bytes(2, "big")
    args.rom = args.output / "test.gbc"
    args.rom.write_bytes(data)
    with (args.output / "server.log").open("w") as log:
        server_port = args.port + 1 if args.scenario == "resume" or args.test_one_way_delay_ms else args.port
        proxy = None
        server = subprocess.Popen([str(args.server), "--bind", f"127.0.0.1:{server_port}"],
                                  stdout=log, stderr=subprocess.STDOUT)
        try:
            deadline = time.monotonic() + 5
            while f"READY 127.0.0.1:{server_port}" not in (args.output / "server.log").read_text():
                require(server.poll() is None and time.monotonic() < deadline, "Server readiness failed")
                time.sleep(0.02)
            if args.scenario == "resume" or args.test_one_way_delay_ms:
                proxy = DropOnceProxy(args.port, server_port, drop=args.scenario == "resume",
                                      delay_ms=args.test_one_way_delay_ms)
                threading.Thread(target=proxy.serve_forever, daemon=True).start()
            barrier = args.output / "finish-barrier"
            barrier.touch()
            exit_barrier = barrier.with_name(barrier.name + ".exit")
            exit_barrier.touch()
            with ThreadPoolExecutor(max_workers=3) as pool:
                futures = [pool.submit(run_client, args, index) for index in range(3)]
                try:
                    deadline = time.monotonic() + 85
                    while not all((args.output / str(index) / "state.json.ready").exists() for index in range(3)):
                        require(time.monotonic() < deadline and not any(future.done() for future in futures),
                                "Client failed before final observation barrier")
                        time.sleep(.01)
                    barrier.unlink(missing_ok=True)
                    deadline = time.monotonic() + 12
                    while not all((args.output / str(index) / "state.json").exists() for index in range(3)):
                        require(time.monotonic() < deadline, "Final capture did not complete")
                        time.sleep(.01)
                    exit_barrier.unlink(missing_ok=True)
                    states = [future.result() for future in futures]
                finally:
                    barrier.unlink(missing_ok=True)
                    exit_barrier.unlink(missing_ok=True)
        finally:
            if proxy:
                proxy.shutdown()
                proxy.server_close()
            server.terminate()
            server.wait(timeout=5)
    ids = {state["wMobilePlayerId"] for state in states}
    require(len(ids) == 3 and 0 not in ids, "Missing distinct identities")
    if args.movement_mode is not None:
        require(all(state["wPlayerState"] == args.movement_mode for state in states), "Wrong scene movement mode")
        require(all(state[f"wMobilePeer{peer}Movement"] == args.movement_mode
                    for state in states for peer in (1, 2)), "Wrong remote movement mode")
    for state in states:
        require({state["wMobilePeer1Id"], state["wMobilePeer2Id"]} == ids - {state["wMobilePlayerId"]},
                "Peer binding mismatch")
        for peer in (1, 2):
            other = next(other for other in states if other["wMobilePlayerId"] == state[f"wMobilePeer{peer}Id"])
            require(state[f"wMobilePeer{peer}Generation"] == other["wMobileGeneration"],
                    "Stale peer generation after reconnect")
            require(state[f"wMobilePeer{peer}Avatar"] == other["wPlayerGender"], "Remote figure differs from the actual chosen character")
            for name in ("MapGroup", "MapNumber"):
                require(state[f"wMobilePeer{peer}{name}"] == other[f"w{name}"], "Stale peer map after transition")
            if other["wMobileAvailability"]:
                require(state[f"wMobilePeer{peer}X"] == other["wXCoord"] * 16 and
                        state[f"wMobilePeer{peer}Y"] == other["wYCoord"] * 16, "Stale peer position")
    if not args.latency and args.scenario in ("basic", "menu", "rejoin", "resume") and states[0]["wMapGroup"] == 24 and states[0]["wMapNumber"] == 5:
        require([(s["wXCoord"], s["wYCoord"]) for s in states] == [(4, 3), (3, 4), (2, 3)],
                "Peer avatars blocked ordinary one-tile movement")
    if args.scenario == "rejoin":
        require("LOBBY_LEAVE" in (args.output / "server.log").read_text(), "No explicit leave reached server")
        require(states[0]["wMobilePlayerId"] == 4, "Rejoin did not obtain a fresh identity")
    if args.scenario == "resume":
        require(proxy.dropped and sorted(s["wMobileGeneration"] for s in states) == [1, 1, 2],
                "Interrupted socket did not resume exactly one generation")
        require(ids == {1, 2, 3}, "Transport reconnect lost player identity")
    if args.scenario == "camera":
        rows = [json.loads(line)["symbols"] for line in (args.output / "0/trace.jsonl").read_text().splitlines()]
        require(any(r["wPanningAroundTinyMap"] and r["wMobileDeferred"] == 3 and not r["wMobileRenderedCount"] for r in rows),
                "Camera scene did not defer social rendering")
        require(states[0]["wPanningAroundTinyMap"] == 0, "Camera failed to restore ordinary scene")
        require(states[0]["vNPCOccupiedMask"].bit_count() > 1, "Camera did not retain window objects after exit")
        require(any(r["vNPCOccupiedMask"].bit_count() > 1 for r in rows if r["wPanningAroundTinyMap"]),
                "Camera never loaded retained window objects")
    if args.scenario == "running":
        rows = [json.loads(line)["symbols"] for line in (args.output / "0/trace.jsonl").read_text().splitlines()]
        require(any(r["wPlayerState"] == 1 for r in rows), "Running input never changed movement mode")
        require(any(json.loads(line)["symbols"][f"wMobilePeer{peer}Movement"] == 1
                    for index in (1, 2) for line in (args.output / str(index) / "trace.jsonl").read_text().splitlines()
                    for peer in (1, 2)), "Remote running mode was not published")
    if args.scenario == "instance":
        require(states[0]["wMobileInstanceKey"] == states[1]["wMobileInstanceKey"] != states[2]["wMobileInstanceKey"],
                "Different ordinary map progress did not isolate rendering")
    if args.scenario == "menu":
        require(states[0]["wMobileMenuState"] == 0 and states[0]["hInMenu"] == 0, "Menu failed to restore overworld")
        trace = [json.loads(line) for line in (args.output / "0/trace.jsonl").read_text().splitlines()]
        require(any(row["symbols"]["wMobileMenuState"] == 1 and row["symbols"]["wMenuCursorY"] == 2
                    for row in trace), "Roster option was never selected")
    if args.scenario == "start-menu":
        observations = [json.loads(line) for line in (args.output / "0/trace.jsonl").read_text().splitlines()]
        paused = [row["symbols"] for row in observations
                  if 3850 <= row["frame"] <= 4200 and row["symbols"]["wScriptRunning"] == 255]
        require(paused and any(not row["wMobileRenderedCount"] and row["wMobileDeferred"] == 3 for row in paused),
                "Start menu did not explicitly defer remote drawing")
        require(states[0]["hInMenu"] == 0 and states[0]["wMobileRenderedCount"] == 2,
                "Start menu failed to restore both remote figures")
    if args.require_npc_motion:
        verify_npc_motion(args.output)
    verify_avatar_graphics(states)
    if args.scenario == "basic" and states[0]["wMapGroup"] == 11 and states[0]["wMapNumber"] == 23:
        # Goldenrod Game Corner is a representative real OAM-pressure scene.
        exercised = False
        for index in range(3):
            rows = [json.loads(line)["symbols"] for line in
                    (args.output / str(index) / "trace.jsonl").read_text().splitlines()]
            affected = [row for row in rows if row["wMobileOnline"] and row["vNPCSuppressedMask"]]
            exercised |= bool(affected) or any(row["wMobileRenderedCount"] and row["vNPCClippedQuads"] for row in rows)
            # Arbitrary video observations may fall inside the temporary hide
            # window or after NPC AI changes facing. The instruction observer
            # separately checks every actor at the exact restore return point.
            require(not affected or any(row["vNPCSuppressedMask"] == row["vNPCRestoredMask"] for row in affected),
                    "Optional NPC rendering failed to restore actors")
            flags = {row["vStoryFlagsHash"] for row in rows if row["wMobileOnline"]}
            require(len(flags) == 1, "Social rendering changed story flags")
            require(all(row["vOAMMaxScanline"] <= 10 for row in rows if row["wMobileRenderedCount"]),
                    "Avatar rendering exceeded hardware scanline capacity")
        if args.exercise_pressure:
            require(exercised, "Optional NPC rendering did not exercise pressure policy")
    if args.scenario in ("warp", "connection"):
        require((states[0]["wMapGroup"], states[0]["wMapNumber"]) !=
                (states[1]["wMapGroup"], states[1]["wMapNumber"]), "Warp did not change the map")
    require([digest(battery) for battery in args.batteries] == battery_hashes, "Input save changed")
    if args.latency:
        result = latency_results(args, states)
        (args.output / "latency.json").write_text(json.dumps(result, indent=2) + "\n")
        print(json.dumps(result), flush=True)
        target = 200 if args.test_one_way_delay_ms == 0 else 300
        require(result["p95_ms"] < target, f"Visible movement p95 misses {target} ms target")
    (args.output / "summary.json").write_text(json.dumps(states, indent=2) + "\n")
    (args.output / "evidence.json").write_text(json.dumps({"scenario": args.scenario,
        "test_rom_sha256": digest(args.rom), "symbols_sha256": digest(args.symbols),
        "core_sha256": digest(args.core), "battery_sha256": battery_hashes,
        "declared_client_server_rtt_ms": args.test_one_way_delay_ms * 2,
        "client_speed_multipliers": [3, 1, 1] if args.scenario == "speed" else [1, 1, 1],
        "expected_deferral": args.deferred, "processes": 3}, indent=2) + "\n")
    print(f"PASS three actual ROMs, scenario={args.scenario}, deferral={args.deferred}: {args.output}")


if __name__ == "__main__":
    main()
