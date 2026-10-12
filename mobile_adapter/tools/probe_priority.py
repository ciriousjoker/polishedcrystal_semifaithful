#!/usr/bin/env python3
"""Physical emergency exit, including a real online state restored without adapter.

Use only an isolated endpoint and read-only ROM observers. Peripheral sockets are
deliberately not reconstructed. Never inject RAM or modify an existing battery.
"""
import argparse
import json
from pathlib import Path
import socket
import subprocess
import time
from probe_emulator import digest, read_symbols, require, trace_text, validate_battery

REPO = Path(__file__).resolve().parents[2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("runner", "core", "bridge", "battery", "output"):
        parser.add_argument("--" + name, type=Path, required=True)
    parser.add_argument("--rom", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.gbc")
    parser.add_argument("--symbols", type=Path, default=REPO / "polishedcrystal-mobile-3.2.3.sym")
    parser.add_argument("--port", type=int, default=17549)
    parser.add_argument("--peer-battery", type=Path, action="append", default=[], help="Keep up to three actual emulator peers online during recovery")
    parser.add_argument('--require-backup-sprite', type=int, help='Require and verify restoration of this actual displaced native sprite ID')
    args = parser.parse_args()
    require(not args.output.exists(), "Choose a new artifact directory")
    args.output.mkdir(parents=True)
    require(len(args.peer_battery) <= 3, "Recovery fixture accepts at most three peers")
    original = digest(args.battery)
    symbols = read_symbols(args.symbols)
    validate_battery(args.battery, symbols)
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", args.port))
    data = bytearray(args.rom.read_bytes())
    bank, address = symbols["MobileEndpoint"]
    at = bank * 0x4000 + address - 0x4000
    data[at + 4:at + 6] = args.port.to_bytes(2, "big")
    data[0x14e:0x150] = ((sum(data[:0x14e]) + sum(data[0x150:])) & 65535).to_bytes(2, "big")
    rom = args.output / "test.gbc"
    rom.write_bytes(data)
    seed = (REPO / "mobile_adapter/tools/fixtures/continue.input").read_text()
    seed += "\nON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 SELECT 8\n"
    seed += "ON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 B 8\n3750 START 8\n"

    def run(name, script, frames, adapter, snapshot_in=None, snapshot_out=None):
        directory = args.output / name
        directory.mkdir()
        (directory / "input.txt").write_text(script)
        command = [str(args.runner), str(args.core), str(rom), str(directory / "screen.ppm"), str(directory),
                   "--frames", str(frames), "--symbols", str(args.symbols),
                   "--input", str(directory / "input.txt"), "--adapter", str(adapter),
                   "--save-in", str(args.battery), "--save-out", str(directory / "result.sav"),
                   "--state-out", str(directory / "state.json"), "--trace-out", str(directory / "trace.jsonl.gz")]
        if snapshot_in:
            command += ["--snapshot-in", str(snapshot_in)]
        else:
            command += ["--pace-from", "3250"]
        if snapshot_out:
            command += ["--test-snapshot-out", str(snapshot_out)]
        with (directory / "process.log").open("w") as log:
            subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=35)
        validate_battery(directory / "result.sav", symbols)
        return [json.loads(line) for line in trace_text(directory / "trace.jsonl").splitlines()]

    peers, peer_logs = [], []
    log_path = args.output / "bridge.log"
    with log_path.open("w") as log:
        bridge = subprocess.Popen([str(args.bridge), "--bind", f"127.0.0.1:{args.port}"],
                                  stdout=log, stderr=subprocess.STDOUT)
        try:
            deadline = time.monotonic() + 5
            while f"READY 127.0.0.1:{args.port}" not in log_path.read_text():
                require(bridge.poll() is None and time.monotonic() < deadline, "Bridge did not start")
                time.sleep(.02)
            peer_hashes = [digest(path) for path in args.peer_battery]
            for index, battery in enumerate(args.peer_battery):
                validate_battery(battery, symbols)
                directory = args.output / f"peer{index}"
                directory.mkdir()
                text = (REPO / "mobile_adapter/tools/fixtures/continue.input").read_text()
                text += "\nON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 SELECT 8\n"
                text += "ON 3300 wMapStatus=2,wScriptRunning=0,hInMenu=0 B 8\n"
                text += f"ON 3550 wMobileOnline=1,wMobilePeerCount={len(args.peer_battery)} {('DOWN','LEFT','UP')[index]} 12\n"
                (directory / "input.txt").write_text(text)
                peer_log = (directory / "process.log").open("w")
                peer_logs.append(peer_log)
                peers.append(subprocess.Popen([str(args.runner), str(args.core), str(rom),
                    str(directory / "screen.ppm"), str(directory), "--frames", "8000",
                    "--input", str(directory / "input.txt"), "--symbols", str(args.symbols),
                    "--save-in", str(battery), "--pace-from", "3250", "--adapter", "1"],
                    stdout=peer_log, stderr=subprocess.STDOUT))
            snapshot = args.output / "online-menu.state"
            online = run("capture", seed, 3820, 1, snapshot_out=snapshot)
            final = online[-1]["symbols"]
            require(final["wMobileOnline"] == final["wMobileWanted"] == 1 and final["wScriptRunning"] == 255,
                    "Fixture did not capture an actual connected menu")
            require(final["wMobilePeerCount"] == len(args.peer_battery), "Recovery did not capture all actual peers")
            if args.peer_battery and final.get("wMobileNativeActorMode") is not None:
                if "wMobileVirtualActorMode" in final:
                    require(all(final[f"wMobilePeer{i}NativeStage"] == 7 and
                                final[f"vMobilePeer{i}NativeSlot"] == 0
                                for i in range(1, len(args.peer_battery) + 1)),
                            "Recovery did not capture ready independent avatars")
                    require(any(any(row['symbols'][f'wMobilePeer{i}OAMStart'] <= 144 and
                                    0 < row['symbols'][f'vMobilePeer{i}OAM0'] < 160
                                    for i in range(1, len(args.peer_battery) + 1))
                                for row in online if 3600 <= row['frame'] < 3750),
                            "No actual hardware avatars before recovery")
                else:
                    require(final["wMobileRenderedCount"] == len(args.peer_battery), "No live native avatars before recovery")
                    require(any(final[f"wMobilePeer{i}NativeRole"] == 1 for i in range(1, len(args.peer_battery) + 1)),
                            "Crowded recovery fixture did not capture a real NPC lease")
            restored = run("restore-without-adapter", "40 SELECT 8\n40 B 8\n120 B 8\n240 RIGHT 24\n", 1200,
                           0, snapshot_in=snapshot)
            live = run("connected-menu-exit", seed + "3800 SELECT 8\n3800 B 8\n3900 B 8\n4000 RIGHT 24\n", 4250, 1)
            for name, rows, frame in (("restore", restored, 40), ("menu", live, 3800)):
                after = [row for row in rows if row["frame"] >= frame + 2]
                require(after and all(row["symbols"][key] == 0 for row in after
                        for key in ("wMobileOwner", "wMobileOnline", "wMobileWanted", "wMobilePeerCount", "hCrashCode")),
                        f"{name}: emergency exit failed or was overwritten")
                require(rows[-1]["symbols"]["hInMenu"] == rows[-1]["symbols"]["wScriptRunning"] == 0,
                        f"{name}: menu did not close")
                require(rows[-1]["symbols"]["wXCoord"] != final["wXCoord"], f"{name}: ordinary movement did not resume")
                if final.get("wMobileNativeActorMode") is not None:
                    last = rows[-1]["symbols"]
                    require(all(last[f"wMobilePeer{i}NativeRole"] == 0 for i in range(1, 4)),
                            f"{name}: displaced NPC lease was not restored")
                    require(all(last[f"vNativeObject{i}Map"] != 254 for i in range(1, 13)),
                            f"{name}: temporary remote object survived emergency exit")
                    matching = [(row['symbols'][f'vMobilePeer{i}OriginalTileHash'],
                                 row['symbols'][f'vMobilePeer{i}RestoredTileHash'])
                                for row in rows for i in range(1, 4)
                                if f'vMobilePeer{i}OriginalTileHash' in row['symbols'] and
                                (args.require_backup_sprite is None or
                                 row['symbols'][f'vMobilePeer{i}BackupSprite'] == args.require_backup_sprite)]
                    if args.require_backup_sprite is not None:
                        require(matching, f'{name}: requested native sprite restoration was not observed')
                    require(all(original == actual for original, actual in matching),
                            f'{name}: native standing/walking tiles differ from the original backup')
            require([digest(path) for path in args.peer_battery] == peer_hashes, "Input peer batteries changed")
            require(digest(args.battery) == original, "Input battery was changed")
            (args.output / "summary.json").write_text(json.dumps({"passed": True,
                "cases": ["connected-menu-exit", "online-menu-state-restored-without-adapter"],
                "actual_peers": len(args.peer_battery), "rom_sha256": digest(args.rom), "battery_sha256": original}, indent=2) + "\n")
            print("Physical menu exit and compatible state recovery passed")
        finally:
            for process in peers:
                process.terminate()
                process.wait(timeout=5)
            for handle in peer_logs:
                handle.close()
            bridge.terminate()
            bridge.wait(timeout=5)


if __name__ == "__main__":
    main()
