#!/usr/bin/env python3
"""Run actual multiplayer ROMs using ordinary battery saves and read-only observers.

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
from probe_emulator import convert_png, digest, read_symbols, require, trace_text, validate_battery
from scanline_selection import verify_native_scanlines

REPO = Path(__file__).resolve().parents[2]


class DropOnceProxy(socketserver.ThreadingTCPServer):
    """Transparent test hop, dropping one socket after live presence starts."""
    daemon_threads = True
    allow_reuse_address = True

    def __init__(self, port, upstream, drop=True, delay_ms=0, trace_dir=None):
        self.trace_dir = trace_dir
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
        trace = (self.server.trace_dir / f"{self.client_address[1]}.jsonl").open("w") if self.server.trace_dir else None
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
                            if trace:
                                trace.write(json.dumps({"wall_us": time.monotonic_ns() // 1000,
                                    "direction": "rom_to_bridge" if source is self.request else "bridge_to_rom",
                                    "hex": data.hex()}) + "\n")
                                trace.flush()
                            queued[source] -= len(data)
            except (ConnectionError, OSError):
                return
            finally:
                if trace:
                    trace.close()


def latency_results(args, states):
    events = [json.loads(line) for line in (args.output / "0/input-events.jsonl").read_text().splitlines()]
    events = [event for event in events if event["frame"] >= 3650 and event["button"] in (6, 7)]
    require(len(events) == 20, "Missing paced movement input samples")
    player = states[0]["wMobilePlayerId"]
    samples = []
    totals = []
    local = [json.loads(line) for line in trace_text(args.output / "0/trace.jsonl").splitlines()]
    for client in range(1, args.players):
        trace = [json.loads(line) for line in trace_text(args.output / str(client) / "trace.jsonl").splitlines()]
        for event in events:
            before = None
            for candidate in reversed(trace):
                if candidate["wall_us"] >= event["wall_us"]:
                    continue
                for index in range(1, args.players):
                    if (candidate["symbols"][f"wMobilePeer{index}Id"] == player
                            and 0 < candidate["symbols"][f"vMobilePeer{index}OAM0"] < 160):
                        before, peer = candidate, str(index)
                        break
                if before is not None:
                    break
            require(before is not None and event["wall_us"] - before["wall_us"] < 200000,
                    "Missing recent visible avatar baseline")
            local_before = next(row for row in reversed(local) if row["wall_us"] < event["wall_us"])
            require(before["symbols"][f"wMobilePeer{peer}DrawX"] == local_before["symbols"]["vMobileLocalX"],
                    f"Unsettled remote baseline for client {client}, input frame {event['frame']}")
            keys = [f"vMobilePeer{peer}OAM{index}" for index in range(16)]
            def left_edge(row):
                return min(row["symbols"][keys[index]] for index in (1, 5, 9, 13))
            previous = left_edge(before)
            changed = next((row for row in trace if row["wall_us"] >= event["wall_us"]
                           and 0 < row["symbols"][keys[0]] < 160
                           and (left_edge(row) - previous) * (1 if event["button"] == 7 else -1) > 0), None)
            require(changed is not None, f"No directional remote motion for client {client}, input frame {event['frame']}")
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
            "added_client_server_rtt_ms": 2 * (args.test_one_way_delay_ms + args.declared_upstream_one_way_delay_ms),
            "input_to_remote_p95_ms": totals[math.ceil(len(totals) * .95) - 1],
            "input_to_remote_max_ms": totals[-1],
            "measurement": "first captured local world motion to first directional position change of four-quad remote avatar bounds at native video callback; input-to-remote includes unchanged vanilla turn delay"}


def run_client(args, index):
    directory = args.output / str(index)
    directory.mkdir()
    script = (REPO / "mobile_adapter/tools/fixtures/continue.input").read_text()
    if index == 0 and args.primary_route:
        script += args.primary_route.read_text()
    script += f"\nON {args.join_frame} wMapStatus=2,wScriptRunning=0,hInMenu=0 SELECT 8\n"
    script += f"ON {args.join_frame} wMapStatus=2,wScriptRunning=0,hInMenu=0 B 8\n"
    if not args.deferred and args.scenario not in ("camera", "scanline") and not (index == 0 and args.scenario == "warp-turn"):
        script += f"ON {args.join_frame + 250} wMobileOnline=1,wMobilePeerCount={args.players - 1} {('RIGHT', 'DOWN', 'LEFT', 'UP')[index]} 12\n"
    if index == 0 and args.scenario == "menu":
        script += "3750 SELECT 8\n3750 A 8\n"
        script += "ON 3830 wMobileMenuState=1,wMenuCursorY=1 DOWN 1\n"
        script += "ON 3880 wMobileMenuState=1,wMenuCursorY=2 A 8\n"
        script += "ON 4000 wMobileMenuState=1,wMenuCursorY=2 B 8\n"
        for frame in range(4120, 4960, 120):
            script += f"{frame} B 8\n"
    if index == 0 and args.scenario == "start-menu":
        script += "3750 START 8\n4250 B 8\n"
    if index == 0 and args.scenario in ("rejoin", "disconnect"):
        script += "3750 SELECT 8\n3750 B 8\n"
        if args.scenario == "rejoin":
            script += "ON 3850 wMobileOnline=0,wMobileOwner=0 SELECT 8\n"
            script += "ON 3850 wMobileOnline=0,wMobileOwner=0 B 8\n"
    if index == 0 and args.scenario == "warp":
        script += "3750 RIGHT 80\n3900 UP 80\n"
        for frame in range(4040, 4670, 40):
            script += f"{frame} A 8\n"
    if args.scenario == "warp-turn":
        if index == 0:
            # Leave the dense scene through its actual doorway, without RAM edits.
            script += "WALK 3750 15 5 300\n"
            script += "".join(f"WALK 3750 15 {y} 300\n" for y in range(6, 13))
            script += "".join(f"WALK 3750 {x} 12 300\n" for x in range(14, 2, -1))
            script += "ON 4700 wXCoord=3,wYCoord=12 DOWN 24\n"
        else:
            # One-frame direction presses select the ordinary in-place turn.
            for frame in range(3900, 6500, 32):
                script += f"{frame} {('LEFT', 'RIGHT')[(frame // 32) % 2]} 1\n"
    if index == 0 and args.scenario == "connection":
        script += "3750 UP 32\n"
    if index == 0 and args.scenario == "running":
        script += "3750 B 24\n3750 RIGHT 24\n3900 LEFT 24\n"
    if index == 0 and args.scenario == "speed":
        for step in range(24):
            script += f"{3650 + step * 96} {('LEFT', 'RIGHT')[step % 2]} 72\n"
        script += "6100 LEFT 48\n"
    if index == 0 and args.scenario == "camera":
        if args.camera_tour:
            script += args.camera_tour.read_text()
        else:
            script += "3750 UP 8\n3800 A 8\n3950 A 8\n4100 A 8\n"
            script += "ON 4250 wPanningAroundTinyMap=1,wScriptRunning=0 UP 128\n"
            script += "ON 4450 wPanningAroundTinyMap=1,wScriptRunning=0 DOWN 180\n"
            script += "4750 A 8\n"
    if args.latency and index == 0:
        for step in range(20):
            script += f"{4000 + step * 90} {('LEFT', 'RIGHT')[step % 2]} 12\n"
    (directory / "input.txt").write_text(script)
    frames = (6800 if args.scenario == "warp-turn" else 6400 if args.latency else 6000 if args.scenario == "camera" else
              5200 if args.scenario in ("menu", "start-menu", "resume") else
              4800 if args.scenario in ("warp", "connection") else 4500)
    if args.observation_frames is not None:
        require(frames <= args.observation_frames <= 12000, "Observation extension must be between the scenario duration and 12000 frames")
        frames = args.observation_frames
    speed = args.speed_values[index] if args.speed_values else (
        args.fast_speed if index == 0 and args.scenario == "speed" else 1)
    pace_from = args.join_frame - 50
    frames = pace_from + (frames - pace_from) * speed
    client_rom = args.household_rom if args.households == 2 and index >= args.players // 2 else args.rom
    command = [str(args.runner), str(args.core), str(client_rom), str(directory / "screen.ppm"),
               str(directory), "--frames", str(frames), "--pace-multiplier", str(speed),
               "--input", str(directory / "input.txt"),
               "--symbols", str(args.symbols), "--state-out", str(directory / "state.json"),
               "--ram-out", str(directory / "physical-wram.bin"),
               "--trace-out", str(directory / "trace.jsonl.gz"), "--save-in", str(args.batteries[index]),
               "--pace-from", str(pace_from), "--adapter", "1",
               "--input-events", str(directory / "input-events.jsonl"),
               "--finish-barrier", str(args.output / "finish-barrier"),
               "--expected-online", str(int(not (args.scenario == "disconnect" and index == 0))),
               "--expected-peers", str(0 if args.scenario == "disconnect" and index == 0 else args.players - 2 if args.scenario == "disconnect" else args.players - 1)]
    if args.profile:
        command += ["--profile-out", str(directory / "profile.json"),
                    "--profile-window-start", str(args.profile_window_start)]
    with (directory / "process.log").open("w") as log:
        subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=90)
    state = json.loads((directory / "state.json").read_text())["symbols"]
    disconnected = args.scenario == "disconnect" and index == 0
    expected_peers = 0 if disconnected else args.players - (2 if args.scenario == "disconnect" else 1)
    for key, expected in {"hCrashCode": 0, "wMobileOnline": int(not disconnected), "wMobilePeerCount": expected_peers,
                          "wMobileRenderedCount": (0 if args.deferred or disconnected else
                              (0 if index == 0 else args.players - 2) if args.scenario in ("warp", "warp-turn", "connection") else
                              (0 if index == 2 else args.players - 2) if args.scenario == "instance" else expected_peers),
                          "wMobileDeferred": args.deferred,
                          "wMobileError": 1 if disconnected else 0, "wMobileServerError": 0,
                          "wMobileGuardStart": 165, "wMobileGuardTx": 165,
                          "wMobileGuardRx": 165, "wMobileGuardEnd": 165}.items():
        if "wMobileVirtualActorMode" in state and key in ("wMobileRenderedCount", "wMobileDeferred"):
            continue  # Temporal liveness is checked from hardware traces below.
        require(state[key] == expected, f"Client {index}: {key}={state[key]}, expected {expected}")
    # A stationary protected scene can receive just one full roster. Heartbeats
    # intentionally do not produce redundant pushed state.
    require(state["wMobilePresenceCount"] >= (1 if args.deferred or args.scenario == "scanline" else 2) and state["wMobileSequence"] > 3,
            "Missing live presence traffic")
    if args.profile:
        metrics = json.loads((directory / "profile.json").read_text())
        require(metrics["serial_count"] > 1000 and metrics["tick_count"] > 1000, "Insufficient instruction samples")
        require(metrics["serial_failures"] == metrics["tick_failures"] == 0, "Register/bank preservation failure")
        require(metrics["prepare_delay_frames"] == 0, "Avatar preparation waited for an extra frame")
        if args.native_actor_probe:
            require(metrics["native_upload_delay_frames"] == 0, "Native background upload called a frame wait")
            require(metrics["native_upload_vblank_crossings"] == 0 and
                    metrics["native_idle_vblank_crossings"] == 0,
                    "Background mobile work crossed the awaited native VBlank")
        if args.native_actor_probe and args.scenario in ("basic", "speed") and not args.cadence_baseline:
            require(metrics["world_frames_with_update"] == 800 and
                    metrics["world_duplicate_updates"] == 0 and metrics["world_max_frame_gap"] == 1,
                    f"Client {index}: native cadence failed: {metrics['world_frames_with_update']}/800 "
                    f"update frames, {metrics['world_duplicate_updates']} duplicates, "
                    f"max gap {metrics['world_max_frame_gap']}")
        if args.cadence_baseline:
            baseline = json.loads((args.cadence_baseline / str(index) / "profile.json").read_text())
            require(baseline.get("world_window_start", 3600) == args.profile_window_start,
                    "Cadence comparison uses different observation windows")
            require(metrics["world_frames_with_update"] >= baseline["world_frames_with_update"] and
                    metrics["world_max_frame_gap"] <= baseline["world_max_frame_gap"] and
                    metrics["world_duplicate_updates"] <= baseline["world_duplicate_updates"],
                    f"Client {index}: adapter missed extra world frames: "
                    f"{800 - metrics['world_frames_with_update']} versus "
                    f"{800 - baseline['world_frames_with_update']} baseline")
        if not args.native_actor_probe:
            require(metrics["npc_restore_count"] > 100 and metrics["npc_restore_failures"] == 0,
                    "NPC restore boundary observer failed")
        stack_fields = ['serial_lowest_sp', 'tick_lowest_sp']
        if args.native_actor_probe:
            stack_fields += ['native_upload_lowest_sp', 'native_idle_lowest_sp', 'native_position_lowest_sp']
        require(min(metrics[key] for key in stack_fields) >= 0xc040, 'Insufficient stack headroom')
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
        for index in range(1, state["wMobilePeerCount"] + 1):
            peer = f"wMobilePeer{index}"
            if not state[peer + "Visible"] or ("wMobileVirtualActorMode" in state and state[peer + "OAMStart"] > 144):
                continue
            mode = state[peer + ("LoadedMovement" if "wMobileVirtualActorMode" in state else "Movement")]
            name = "surf_pikachu" if mode == 5 else figures[state[peer + "Avatar"]] + modes[mode]
            source = (REPO / "gfx/sprites" / (name + ".2bpp")).read_bytes()
            require(len(source) == 384, "Remote player sprite size changed")
            frame = state[peer + "LoadedFrame"]
            require(frame in (0, 4, 8, 12, 16, 20), "Invalid remote source frame")
            cached = source[frame * 16:(frame + 4) * 16] if "wMobileVirtualActorMode" in state else source
            require(state[f"vMobilePeer{index}CacheHash"] == byte_hash(cached),
                    "Remote graphics cache differs from the selected native sprite/pose")
            require(state[f"vMobilePeer{index}TileHash"] == byte_hash(source[frame * 16:(frame + 4) * 16]),
                    "Displayed remote VRAM tiles differ from the selected vanilla frame")
            palette = state[peer + "Palette"]
            if "wMobileNativeOriginals" in state:
                target = next((other for other in states
                               if other["wMobilePlayerId"] == state[peer + "Id"]), None)
                require(target is not None and "vLocalPaletteHash" in target and
                        state.get(f"vMobilePeer{index}PaletteHash") == target["vLocalPaletteHash"],
                        "Remote hardware colors differ from the selected player's native colors")
            else:
                require(state[f"wLoadedObjPal{palette}"] == state[peer + "Avatar"],
                        "Remote palette uses another player figure's colors")


def verify_npc_motion(output, players):
    for index in range(players):
        rows = [json.loads(line) for line in trace_text(output / str(index) / "trace.jsonl").splitlines()]
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
    parser.add_argument("--players", type=int, choices=(2, 3, 4), default=3)
    parser.add_argument("--observation-frames", type=int, help="Extend the ordinary-speed observation without changing input timing")
    parser.add_argument("--fast-speed", type=int, choices=(1, 2, 3, 4), default=4)
    parser.add_argument("--fourth-battery", type=Path)
    parser.add_argument("--primary-route", type=Path, help="Ordinary WALK input before joining, for retained-slot capacity fixtures")
    parser.add_argument('--camera-tour', type=Path, help='Ordinary controller input for an extended camera lifecycle')
    parser.add_argument("--join-frame", type=int, default=3300)
    parser.add_argument("--latency", action="store_true")
    parser.add_argument("--trace-tcp", action="store_true", help="Record timestamped bytes through a transparent local diagnostic hop")
    parser.add_argument("--households", type=int, choices=(1, 2), default=1, help="Group actual ROMs through one or two Bridges")
    parser.add_argument("--speeds", help="Comma-separated emulator speed multipliers, one per player")
    parser.add_argument("--bridge-upstream", help="Use the Bridge binary with this local hosted-model WS origin")
    parser.add_argument("--test-one-way-delay-ms", type=int, default=0)
    parser.add_argument("--declared-upstream-one-way-delay-ms", type=int, default=0, help="Record delay injected by the independently owned hosted simulator")
    parser.add_argument("--profile", action="store_true")
    parser.add_argument("--profile-window-start", type=int, default=3600)
    parser.add_argument("--native-actor-probe", action="store_true", help="Enforce native actor resource and background cadence checks")
    parser.add_argument("--cadence-baseline", type=Path, help="Fail on any extra missed world frame relative to matching baseline profiles")
    parser.add_argument("--port", type=int, default=17425)
    parser.add_argument("--scenario", choices=("basic", "menu", "start-menu", "rejoin", "disconnect", "warp", "warp-turn", "connection", "running", "camera", "resume", "instance", "speed", "scanline"), default="basic")
    parser.add_argument("--second-battery", type=Path, help="Second player's ordinary character-choice save")
    parser.add_argument("--require-npc-motion", action="store_true")
    parser.add_argument("--different-battery", type=Path, help="Third player's ordinary save for instance isolation")
    parser.add_argument("--movement-mode", type=int, choices=range(6), help="Assert ordinary scene's movement state")
    parser.add_argument("--exercise-pressure", action="store_true", help="Require optional rendering pressure in the chosen dense scene")
    parser.add_argument("--deferred", type=int, choices=(0, 1, 2, 3), default=0)
    args = parser.parse_args()
    require(3300 <= args.join_frame <= 10000, "Join frame exceeds the bounded fixture range")
    require(args.scenario != "instance" or args.players >= 3, "Instance isolation needs at least three players")
    require(not args.primary_route or (args.scenario == "basic" and args.observation_frames and
            args.observation_frames >= args.join_frame + 1000), "A crowd walk needs a basic scenario with time to join")
    require(args.households == 1 or (args.bridge_upstream and args.scenario != "resume"
            and args.test_one_way_delay_ms == 0), "Two households require a hosted upstream without the one-socket proxy")
    args.speed_values = [int(value) for value in args.speeds.split(",")] if args.speeds else []
    require(not args.speed_values or (len(args.speed_values) == args.players
            and all(1 <= value <= 4 for value in args.speed_values)), "Speeds must provide one multiplier from 1 through 4 per player")
    require(not args.cadence_baseline or args.profile, "Cadence comparison requires --profile")
    for name in ("runner", "core", "server", "battery", "output", "rom", "symbols"):
        setattr(args, name, getattr(args, name).resolve())
    require(not args.output.exists(), "Choose a fresh artifact directory")
    args.output.mkdir(parents=True)
    symbols_copy = args.output / "test.sym"
    symbols_copy.write_bytes(args.symbols.read_bytes())
    args.symbols = symbols_copy
    args.batteries = [args.battery, args.second_battery.resolve() if args.second_battery else args.battery, args.different_battery.resolve() if args.different_battery else args.battery]
    if args.players == 4:
        args.batteries.append(args.fourth_battery.resolve() if args.fourth_battery else args.battery)
    args.batteries = args.batteries[:args.players]
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
    args.household_rom = args.rom
    if args.households == 2:
        data[offset + 4:offset + 6] = (args.port + 3).to_bytes(2, "big")
        data[0x14e:0x150] = ((sum(data[:0x14e]) + sum(data[0x150:])) & 65535).to_bytes(2, "big")
        args.household_rom = args.output / "household2.gbc"
        args.household_rom.write_bytes(data)
    with (args.output / "server.log").open("w") as log:
        server_port = args.port + 1 if args.scenario == "resume" or args.test_one_way_delay_ms or args.trace_tcp else args.port
        proxy = None
        command = [str(args.server), "--bind", f"127.0.0.1:{server_port}"]
        if args.bridge_upstream:
            command += ["--upstream", args.bridge_upstream]
        second_server = None
        second_log = None
        server = subprocess.Popen(command,
                                  stdout=log, stderr=subprocess.STDOUT)
        try:
            deadline = time.monotonic() + 5
            while f"READY 127.0.0.1:{server_port}" not in (args.output / "server.log").read_text():
                require(server.poll() is None and time.monotonic() < deadline, "Server readiness failed")
                time.sleep(0.02)
            if args.households == 2:
                second_log = (args.output / "household2-server.log").open("w")
                second_server = subprocess.Popen([str(args.server), "--bind", f"127.0.0.1:{args.port + 3}",
                    "--upstream", args.bridge_upstream], stdout=second_log, stderr=subprocess.STDOUT)
                deadline = time.monotonic() + 5
                while f"READY 127.0.0.1:{args.port + 3}" not in (args.output / "household2-server.log").read_text():
                    require(second_server.poll() is None and time.monotonic() < deadline, "Second Bridge readiness failed")
                    time.sleep(.02)
            if args.scenario == "resume" or args.test_one_way_delay_ms or args.trace_tcp:
                trace_dir = args.output / "tcp" if args.trace_tcp else None
                if trace_dir:
                    trace_dir.mkdir()
                proxy = DropOnceProxy(args.port, server_port, drop=args.scenario == "resume",
                                      delay_ms=args.test_one_way_delay_ms, trace_dir=trace_dir)
                threading.Thread(target=proxy.serve_forever, daemon=True).start()
            barrier = args.output / "finish-barrier"
            barrier.touch()
            exit_barrier = barrier.with_name(barrier.name + ".exit")
            exit_barrier.touch()
            with ThreadPoolExecutor(max_workers=args.players) as pool:
                futures = [pool.submit(run_client, args, index) for index in range(args.players)]
                try:
                    deadline = time.monotonic() + 85
                    while not all((args.output / str(index) / "state.json.ready").exists() for index in range(args.players)):
                        require(time.monotonic() < deadline and not any(future.done() for future in futures),
                                "Client failed before final observation barrier")
                        time.sleep(.01)
                    barrier.unlink(missing_ok=True)
                    deadline = time.monotonic() + 12
                    while not all((args.output / str(index) / "state.json").exists() for index in range(args.players)):
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
            if second_server is not None:
                second_server.terminate()
                second_server.wait(timeout=5)
            if second_log is not None:
                second_log.close()
    active = [state for state in states if state["wMobileOnline"]]
    ids = {state["wMobilePlayerId"] for state in active}
    require(len(ids) == args.players - int(args.scenario == "disconnect") and 0 not in ids,
            "Missing distinct identities")
    if args.movement_mode is not None:
        require(all(state["wPlayerState"] == args.movement_mode for state in states), "Wrong scene movement mode")
        require(all(state[f"wMobilePeer{peer}Movement"] == args.movement_mode
                    for state in states for peer in range(1, args.players)), "Wrong remote movement mode")
    for state in active:
        require({state[f"wMobilePeer{peer}Id"] for peer in range(1, state["wMobilePeerCount"] + 1)} == ids - {state["wMobilePlayerId"]},
                "Peer binding mismatch")
        for peer in range(1, state["wMobilePeerCount"] + 1):
            other = next(other for other in active if other["wMobilePlayerId"] == state[f"wMobilePeer{peer}Id"])
            require(state[f"wMobilePeer{peer}Generation"] == other["wMobileGeneration"],
                    "Stale peer generation after reconnect")
            require(state[f"wMobilePeer{peer}Avatar"] == other["wPlayerGender"], "Remote figure differs from the actual chosen character")
            for name in ("MapGroup", "MapNumber"):
                require(state[f"wMobilePeer{peer}{name}"] == other[f"w{name}"], "Stale peer map after transition")
            if other["wMobileAvailability"]:
                require(state[f"wMobilePeer{peer}X"] == other["wXCoord"] * 16 and
                        state[f"wMobilePeer{peer}Y"] == other["wYCoord"] * 16, "Stale peer position")
    if not args.latency and args.scenario in ("basic", "menu", "rejoin", "resume") and states[0]["wMapGroup"] == 24 and states[0]["wMapNumber"] == 5:
        require([(s["wXCoord"], s["wYCoord"]) for s in states] == [(4, 3), (3, 4), (2, 3), (3, 2)][:args.players],
                "Peer avatars blocked ordinary one-tile movement")
    if args.scenario == "rejoin":
        if not args.native_actor_probe:
            require("LOBBY_LEAVE" in (args.output / "server.log").read_text(), "No explicit leave reached server")
        else:
            rows = [json.loads(line) for line in trace_text(args.output / "0/trace.jsonl").splitlines()]
            require(any(not row["symbols"]["wMobileOnline"] and not row["symbols"]["wMobileWanted"]
                        for row in rows if 3750 <= row["frame"] < 3850),
                    "Physical disconnect intent was not observed before rejoin")
        require(states[0]["wMobilePlayerId"] == args.players + 1, "Rejoin did not obtain a fresh identity")
    if args.scenario == "resume":
        require(proxy.dropped and sorted(s["wMobileGeneration"] for s in states) == [1] * (args.players - 1) + [2],
                "Interrupted socket did not resume exactly one generation")
        require(ids == set(range(1, args.players + 1)), "Transport reconnect lost player identity")
    if args.scenario == "camera":
        rows = [json.loads(line)["symbols"] for line in trace_text(args.output / "0/trace.jsonl").splitlines()]
        require(any(r["wPanningAroundTinyMap"] and r["wMobileDeferred"] == 3 and not r["wMobileRenderedCount"] for r in rows),
                "Camera scene did not defer social rendering")
        require(states[0]["wPanningAroundTinyMap"] == 0, "Camera failed to restore ordinary scene")
        require(states[0]["vNPCOccupiedMask"].bit_count() > 1, "Camera did not retain window objects after exit")
        require(any(r["vNPCOccupiedMask"].bit_count() > 1 for r in rows if r["wPanningAroundTinyMap"]),
                "Camera never loaded retained window objects")
    if args.scenario == "running":
        rows = [json.loads(line)["symbols"] for line in trace_text(args.output / "0/trace.jsonl").splitlines()]
        require(any(r["wPlayerState"] == 1 for r in rows), "Running input never changed movement mode")
        if args.native_actor_probe:
            figures = ("chris", "kris", "crys", "beta")
            modes = ("", "_run", "_bike", "_bike", "_surf")
            valid_frames = {}
            for avatar, figure in enumerate(figures):
                valid_frames[avatar] = {byte_hash(data[offset:offset + 64])
                    for mode in modes
                    for data in [(REPO / "gfx/sprites" / (figure + mode + ".2bpp")).read_bytes()]
                    for offset in range(0, 384, 64)}
            observed = 0
            for client in range(args.players):
                for line in trace_text(args.output / str(client) / "trace.jsonl").splitlines():
                    row = json.loads(line)
                    if not 3730 <= row["frame"] <= 4100:
                        continue
                    state = row["symbols"]
                    for index in range(1, args.players):
                        peer = f"wMobilePeer{index}"
                        # A native viewport edge can clip a four-quad group.
                        # This byte oracle requires the whole figure on screen.
                        if (not state[peer + "Visible"] or
                                not 16 <= state[peer + "ScreenX"] <= 152 or
                                not 24 <= state[peer + "ScreenY"] <= 144):
                            continue
                        drawn = (state[peer + 'OAMStart'] <= 144 and
                                 0 < state[f"vMobilePeer{index}OAM0"] < 160)
                        if 'wMobileVirtualActorMode' in state and not drawn:
                            # Admission may alternate complete figures. The
                            # independent hardware gate below bounds absence;
                            # inspect tiles only when this figure was emitted.
                            continue
                        require(drawn, "A visible remote avatar disappeared during a movement-mode transition")
                        require(state[f"vMobilePeer{index}TileHash"] in valid_frames[state[peer + "Avatar"]],
                                "Movement-mode transition displayed incomplete or another avatar's tiles")
                        observed += 1
            require(observed > 100, "No intermediate movement-mode visuals were checked")

        require(any(json.loads(line)["symbols"][f"wMobilePeer{peer}Movement"] == 1
                    for index in (1, 2) for line in trace_text(args.output / str(index) / "trace.jsonl").splitlines()
                    for peer in range(1, args.players)), "Remote running mode was not published")
    if args.scenario == "instance":
        require(states[0]["wMobileInstanceKey"] == states[1]["wMobileInstanceKey"] != states[2]["wMobileInstanceKey"],
                "Different ordinary map progress did not isolate rendering")
    if args.scenario == "menu":
        require(states[0]["wMobileMenuState"] == 0 and states[0]["hInMenu"] == 0, "Menu failed to restore overworld")
        trace = [json.loads(line) for line in trace_text(args.output / "0/trace.jsonl").splitlines()]
        require(any(row["symbols"]["wMobileMenuState"] == 1 and row["symbols"]["wMenuCursorY"] == 2
                    for row in trace), "Roster option was never selected")
    if args.scenario == "start-menu":
        observations = [json.loads(line) for line in trace_text(args.output / "0/trace.jsonl").splitlines()]
        paused = [row["symbols"] for row in observations
                  if 3850 <= row["frame"] <= 4200 and row["symbols"]["wScriptRunning"] == 255]
        if args.native_actor_probe:
            if "wMobileVirtualActorMode" in states[0]:
                require(paused and all(row['wMobileLocalValid'] for row in paused),
                        "Start menu discarded the frozen overworld")
                for peer in range(1, args.players):
                    exposed = [row for row in paused if row[f'wMobilePeer{peer}Visible'] and
                               row[f'wMobilePeer{peer}ScreenX'] < 72]
                    require(not exposed or any(row[f'wMobilePeer{peer}OAMStart'] <= 144 and
                                              0 < row[f'vMobilePeer{peer}OAM0'] < 160 for row in exposed),
                            "Uncovered frozen avatar disappeared from the Start menu")
            else:
                require(paused and all(row["wMobileRenderedCount"] == args.players - 1 for row in paused),
                        "Native remote actors disappeared while the Start menu was open")
            for peer in range(1, args.players):
                require(len({(row[f"wMobilePeer{peer}DrawX"], row[f"wMobilePeer{peer}DrawY"]) for row in paused}) == 1,
                        "Paused native actor presentation did not remain frozen")
        else:
            require(paused and any(not row["wMobileRenderedCount"] and row["wMobileDeferred"] == 3 for row in paused),
                    "Start menu did not explicitly defer remote drawing")
        require(states[0]["hInMenu"] == states[0]['wScriptRunning'] == 0,
                "Start menu failed to restore the ordinary overworld")
        if "wMobileVirtualActorMode" in states[0]:
            for peer in range(1, args.players):
                require(any(row['symbols'][f'wMobilePeer{peer}OAMStart'] <= 144 and
                            0 < row['symbols'][f'vMobilePeer{peer}OAM0'] < 160
                            for row in observations if row['frame'] >= 4300),
                        "Start menu failed to restore an actual remote hardware figure")
        else:
            require(states[0]["wMobileRenderedCount"] == args.players - 1,
                    "Start menu failed to restore every remote figure")
    if args.require_npc_motion:
        verify_npc_motion(args.output, args.players)
    if args.native_actor_probe:
        verify_native_scanlines(args.output, args.players)
        for state in states:
            require(state["wMobileFacingsSuppressed"] == 1, "Native actor experiment was not enabled")
            for peer in range(1, state["wMobilePeerCount"] + 1):
                if not state[f"wMobilePeer{peer}Visible"]:
                    if args.scenario in ("warp", "warp-turn", "connection", "instance"):
                        require(state[f"vMobilePeer{peer}NativeSlot"] == 0 and
                                state[f"wMobilePeer{peer}NativeRole"] == 0,
                                "Ineligible map/instance retained a native actor lease")
                    continue
                if "wMobileVirtualActorMode" in state:
                    require(state[f"vMobilePeer{peer}NativeSlot"] == 0, "Virtual peer entered the native object pool")
                    continue
                require(0 < state[f"vMobilePeer{peer}OAM0"] < 160 and state[f"wMobilePeer{peer}OAMStart"] <= 144,
                        "Projected native actor was absent from actual hardware OAM")
    if args.native_actor_probe and args.scenario == "disconnect":
        require(all(states[0][f"wMobilePeer{peer}NativeRole"] == 0 for peer in range(1, args.players)),
                "Disconnected client retained an NPC backup lease")
        require(all(states[0][f"vNativeObject{slot}Map"] != 254 for slot in range(1, 13)),
                "Disconnected client retained a temporary native actor")
        rows = [json.loads(line)["symbols"] for line in trace_text(args.output / "0/trace.jsonl").splitlines()]
        reclaimed = {row[f"vMobilePeer{peer}BackupMap"] for row in rows for peer in range(1, args.players)
                     if row[f"wMobilePeer{peer}NativeRole"] == 1}
        restored = {states[0][f"vNativeObject{slot}Map"] for slot in range(1, 13)}
        require(reclaimed <= restored, "An audited NPC did not return after priority disconnect")
    verify_avatar_graphics(states)
    if args.native_actor_probe and states[0]["wMapGroup"] == 11 and states[0]["wMapNumber"] == 23:
        for index in range(args.players):
            rows = [json.loads(line)["symbols"] for line in trace_text(args.output / str(index) / "trace.jsonl").splitlines()]
            online = [row for row in rows if row["wMobileOnline"]]
            require(len({row["vStoryFlagsHash"] for row in online}) == 1,
                    "Native leases changed story flags")
            for row in online:
                for peer in range(1, args.players):
                    if row[f"wMobilePeer{peer}NativeRole"]:
                        require(6 <= row[f"vMobilePeer{peer}BackupMap"] <= 11,
                                "Native allocator reclaimed an unaudited map actor")
    if not args.native_actor_probe and args.scenario in ("basic", "speed", "start-menu") and states[0]["wMapGroup"] == 11 and states[0]["wMapNumber"] == 23:
        # Goldenrod Game Corner is a representative real OAM-pressure scene.
        exercised = False
        for index in range(args.players):
            rows = [json.loads(line)["symbols"] for line in
                    trace_text(args.output / str(index) / "trace.jsonl").splitlines()]
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
    if args.scenario in ("warp", "warp-turn", "connection"):
        require((states[0]["wMapGroup"], states[0]["wMapNumber"]) !=
                (states[1]["wMapGroup"], states[1]["wMapNumber"]), "Warp did not change the map")
    if args.scenario == "warp-turn":
        departed = states[0]["wMobilePlayerId"]
        checked = 0
        for client in range(1, args.players):
            rows = [json.loads(line) for line in trace_text(args.output / str(client) / "trace.jsonl").splitlines()]
            turn_rows = [row for row in rows if 5200 <= row["frame"] <= 6500]
            require(turn_rows and any(row["symbols"]["wScriptRunning"] for row in turn_rows),
                    "Map-departure regression did not exercise native turn animations")
            # Once native OAM has hidden the peer, a turn must never restore its
            # cached figure while projection still says it is offscreen/ineligible.
            hidden = False
            saw_presented = saw_hidden = False
            for row in rows:
                if row["frame"] < args.join_frame + 200:
                    continue
                symbols = row["symbols"]
                peer = next(i for i in range(1, args.players) if symbols[f"wMobilePeer{i}Id"] == departed)
                presented = any(0 < symbols[f"vMobilePeer{peer}OAM{q}"] < 160 and
                                0 < symbols[f"vMobilePeer{peer}OAM{q + 1}"] < 168 for q in (0, 4, 8, 12))
                if symbols[f"wMobilePeer{peer}Visible"]:
                    saw_presented |= presented
                    hidden = False
                elif not presented:
                    saw_hidden |= saw_presented
                    hidden = True
                elif hidden:
                    require(False, f"Hidden peer revived by native turn on client {client}, frame {row['frame']}")
            require(saw_presented and saw_hidden, "Regression did not observe a displayed peer becoming hidden")
            for row in turn_rows:
                symbols = row["symbols"]
                peer = next(i for i in range(1, args.players) if symbols[f"wMobilePeer{i}Id"] == departed)
                require((symbols[f"wMobilePeer{peer}MapGroup"], symbols[f"wMobilePeer{peer}MapNumber"]) !=
                        (symbols["wMapGroup"], symbols["wMapNumber"]), "Departed peer still reports the original map")
                require(symbols[f"vMobilePeer{peer}OAM0"] == 160,
                        f"Departed player reappeared on client {client}, frame {row['frame']}")
                checked += 1
        (args.output / "warp-turn.json").write_text(json.dumps({"checked_video_observations": checked,
            "departed_player": departed, "passed": True}, indent=2) + "\n")
    require([digest(battery) for battery in args.batteries] == battery_hashes, "Input save changed")
    if args.latency:
        result = latency_results(args, states)
        (args.output / "latency.json").write_text(json.dumps(result, indent=2) + "\n")
        print(json.dumps(result), flush=True)
        target = 200 if not (args.test_one_way_delay_ms or args.declared_upstream_one_way_delay_ms) else 300
        require(result["p95_ms"] < target, f"Visible movement p95 misses {target} ms target")
    (args.output / "summary.json").write_text(json.dumps(states, indent=2) + "\n")
    (args.output / "evidence.json").write_text(json.dumps({"scenario": args.scenario,
        "test_rom_sha256": digest(args.rom), "symbols_sha256": digest(args.symbols),
        "core_sha256": digest(args.core), "battery_sha256": battery_hashes,
        "households": args.households,
        "emulator_speeds": args.speed_values or [args.fast_speed if i == 0 and args.scenario == "speed" else 1 for i in range(args.players)],
        "declared_client_server_rtt_ms": (args.test_one_way_delay_ms + args.declared_upstream_one_way_delay_ms) * 2,
        "client_speed_multipliers": [args.fast_speed] + [1] * (args.players - 1) if args.scenario == "speed" else [1] * args.players,
        "expected_deferral": args.deferred, "processes": args.players}, indent=2) + "\n")
    print(f"PASS {args.players} actual ROMs, scenario={args.scenario}, deferral={args.deferred}: {args.output}")


if __name__ == "__main__":
    main()
