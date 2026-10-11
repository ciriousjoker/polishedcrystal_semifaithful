"""Hardware OAM selection checks, independent of an emulator's sprite limiter.

The native overworld uses 8x8 objects. X, transparency and palette do not affect
the ten-object selection limit. These checks model selection, not pixel priority
against backgrounds or other sprites.
"""
import json
from probe_emulator import require, trace_text


def select_scanlines(ys, height=8):
    require(len(ys) == 40 and all(isinstance(y, int) and 0 <= y <= 255 for y in ys),
            "Expected forty hardware OAM Y coordinates")
    require(height in (8, 16), "Unsupported hardware object height")
    demand, selected = [], []
    for line in range(144):
        matching = [index for index, y in enumerate(ys) if y - 16 <= line < y - 16 + height]
        demand.append(len(matching))
        selected.append(set(matching[:10]))
    return demand, selected


def inspect_native_frame(state):
    ys = [state[f"vOAM{index}Y"] for index in range(40)]
    demand, selected = select_scanlines(ys)
    require(max(demand) == state["vOAMMaxScanline"], "Hardware OAM observer disagrees with selection model")
    dropped = []
    for peer in range(1, state["wMobilePeerCount"] + 1):
        start = state[f"wMobilePeer{peer}OAMStart"]
        if start > 144:
            continue  # No complete hardware group at this observation.
        require(start % 4 == 0, "Unaligned native avatar OAM group")
        missing = sum(index not in selected[line]
                      for index in range(start // 4, start // 4 + 4)
                      for line in range(max(0, ys[index] - 16), min(144, ys[index] - 8)))
        if missing:
            dropped.append({"peer": peer, "object_rows_not_selected": missing})
    return {"maximum_demand": max(demand), "overloaded_lines":
            [line for line, count in enumerate(demand) if count > 10], "dropped_peers": dropped}


def missing_final_peers(state):
    """Final stable visibility is separate from transient join/upload samples."""
    if not state["wMobileOnline"]:
        return []
    return [peer for peer in range(1, state["wMobilePeerCount"] + 1)
            if state[f"wMobilePeer{peer}Visible"] and
            (state[f"wMobilePeer{peer}OAMStart"] > 144 or
             not 0 < state[f"vMobilePeer{peer}OAM0"] < 160)]


def verify_native_scanlines(output, players):
    reports = []
    for client in range(players):
        samples, failures, maximum, witnesses = 0, 0, 0, []
        for line in trace_text(output / str(client) / "trace.jsonl").splitlines():
            row = json.loads(line)
            state = row["symbols"]
            if not state["wMobileOnline"] or not state["wMobileRenderedCount"]:
                continue
            result = inspect_native_frame(state)
            samples += 1
            maximum = max(maximum, result["maximum_demand"])
            if result["overloaded_lines"] or result["dropped_peers"]:
                failures += 1
                if len(witnesses) < 8:
                    witnesses.append({"frame": row["frame"], **result})
        final_state = json.loads((output / str(client) / "state.json").read_text())["symbols"]
        reports.append({"client": client, "samples": samples, "maximum_demand": maximum,
                        "failing_samples": failures, "witnesses": witnesses,
                        "final_missing_peers": missing_final_peers(final_state)})
    result = {"passed": all(report["samples"] > 0 and not report["failing_samples"] and
                           not report["final_missing_peers"] for report in reports),
              "object_height": 8, "model": "first ten Y-matching hardware OAM entries",
              "scope": "Frame-boundary hardware OAM selection, not background/pixel compositing or every mid-frame DMA state.",
              "clients": reports}
    (output / "native-scanlines.json").write_text(json.dumps(result, indent=2) + "\n")
    require(result["passed"], "Native scanline/visibility acceptance failed; see native-scanlines.json")
    return result
