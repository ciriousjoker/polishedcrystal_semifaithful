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
    if not state["wMobileOnline"] or "wMobileVirtualActorMode" in state:
        return []
    return [peer for peer in range(1, state["wMobilePeerCount"] + 1)
            if state[f"wMobilePeer{peer}Visible"] and
            (state[f"wMobilePeer{peer}OAMStart"] > 144 or
             not 0 < state[f"vMobilePeer{peer}OAM0"] < 160)]


def virtual_temporal_report(rows, max_absent_frames=3):
    """Bound omission after a visible, decoded avatar is ready for presentation.

    Identity changes, scene exits and decode invalidation start a new segment.
    Read actual hardware membership, never the CPU's staging rendered counter.
    This is a receiver-frame gate; fast-forward senders do not relax it.
    """
    last = {}
    maximum = 0
    witnesses = []
    for row in rows:
        state, frame = row["symbols"], row["frame"]
        active = set()
        if state["wMobileOnline"]:
            for peer in range(1, state["wMobilePeerCount"] + 1):
                prefix = f"wMobilePeer{peer}"
                if not state[prefix + "Visible"] or state[prefix + "NativeStage"] != 7:
                    continue
                identity = (peer, state[prefix + "Id"], state[prefix + "Generation"])
                active.add(identity)
                visible = (state[prefix + "OAMStart"] <= 144 and
                           0 < state[f"vMobilePeer{peer}OAM0"] < 160)
                prior = last.get(identity, frame - 1)
                absent = 0 if visible else frame - prior
                maximum = max(maximum, absent)
                if absent > max_absent_frames and len(witnesses) < 8:
                    witnesses.append({"frame": frame, "peer": peer, "absent_frames": absent})
                last[identity] = frame if visible else prior
        last = {key: value for key, value in last.items() if key in active}
    return {"passed": not witnesses, "maximum_absent_frames": maximum,
            "allowed_absent_frames": max_absent_frames, "witnesses": witnesses}


def verify_native_scanlines(output, players):
    reports = []
    for client in range(players):
        samples, failures, maximum, witnesses = 0, 0, 0, []
        temporal_seen = set()
        rows = [json.loads(line) for line in trace_text(output / str(client) / "trace.jsonl").splitlines()]
        for row in rows:
            state = row["symbols"]
            if "wMobileVirtualActorMode" in state and state["wMobileOnline"]:
                for peer in range(1, state["wMobilePeerCount"] + 1):
                    if state[f"wMobilePeer{peer}Visible"] and state[f"wMobilePeer{peer}OAMStart"] <= 144:
                        temporal_seen.add(peer)
            hardware_present = any(state[f"wMobilePeer{peer}OAMStart"] <= 144 and
                                   0 < state[f"vMobilePeer{peer}OAM0"] < 160
                                   for peer in range(1, state["wMobilePeerCount"] + 1))
            if not state["wMobileOnline"] or not (hardware_present if "wMobileVirtualActorMode" in state
                                                  else state["wMobileRenderedCount"]):
                continue
            result = inspect_native_frame(state)
            samples += 1
            maximum = max(maximum, result["maximum_demand"])
            if result["overloaded_lines"] or result["dropped_peers"]:
                failures += 1
                if len(witnesses) < 8:
                    witnesses.append({"frame": row["frame"], **result})
        final_state = json.loads((output / str(client) / "state.json").read_text())["symbols"]
        temporal = None
        never_presented = []
        if "wMobileVirtualActorMode" in final_state:
            temporal = virtual_temporal_report(rows)
            expected = {peer for peer in range(1, final_state["wMobilePeerCount"] + 1)
                        if final_state[f"wMobilePeer{peer}Visible"]}
            never_presented = sorted(expected - temporal_seen)
        reports.append({"client": client, "samples": samples, "maximum_demand": maximum,
                        "failing_samples": failures, "witnesses": witnesses,
                        "final_missing_peers": missing_final_peers(final_state),
                        "temporal_visibility": temporal,
                        "never_presented_peers": never_presented})
    result = {"passed": all(report["samples"] > 0 and not report["failing_samples"] and
                           not report["final_missing_peers"] and
                           not report["never_presented_peers"] and
                           (report["temporal_visibility"] is None or report["temporal_visibility"]["passed"])
                           for report in reports),
              "object_height": 8, "model": "first ten Y-matching hardware OAM entries",
              "scope": "Frame-boundary hardware OAM selection, not background/pixel compositing or every mid-frame DMA state.",
              "clients": reports}
    (output / "native-scanlines.json").write_text(json.dumps(result, indent=2) + "\n")
    require(result["passed"], "Native scanline/visibility acceptance failed; see native-scanlines.json")
    return result
