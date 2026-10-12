"""Hardware OAM selection checks, independent of an emulator's sprite limiter.

The native overworld uses 8x8 objects. X, transparency and palette do not affect
the ten-object selection limit. These checks model selection, not pixel priority
against backgrounds or other sprites.
"""
import json
from probe_emulator import require, trace_rows


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
    Visibility projection can lag an accepted network record. A peer that has
    already reported a menu or another scene is not eligible for drawing even
    while its old projected Visible flag remains set.
    Read actual hardware membership, never the CPU's staging rendered counter.
    This is a receiver-frame gate; fast-forward senders do not relax it. A native
    sprite reload, missing native wait, or wait starting at/after LY 136 has no
    admitted background window. The instruction observer detects a missing wait
    even when menu teardown has already reset the cached hDelayFrameLY to zero. Such frames
    reset this idle-work bound and are reported separately, without pretending
    that a remote figure was displayed. Native frame cadence has its own oracle.
    """
    last = {}
    maximum = 0
    witnesses = []
    native_busy = []
    native_graphics = []
    native_no_wait = []
    previous = None
    for row in rows:
        state, frame = row["symbols"], row["frame"]
        if previous is not None and state['wMobileOnline'] and previous['symbols']['wMobileOnline']:
            require(frame == previous['frame'] + 1, 'Temporal visibility requires every connected frame')
        previous = row
        active = set()
        eligible_world = state["wMobileOnline"] and state.get('wScriptRunning', 0) != 255
        graphics_wait = state.get('vNativeGraphicsWait', 0)
        wait_ly = state.get('vNativeWaitEntryLY', state.get('hDelayFrameLY', 0))
        if eligible_world and state.get('vNativeWaitEntryLY') == 255:
            native_no_wait.append(frame)
        if eligible_world and graphics_wait:
            native_graphics.append(frame)
        if eligible_world and (wait_ly >= 136 or graphics_wait):
            native_busy.append(frame)
        if eligible_world and wait_ly < 136 and not graphics_wait:
            for peer in range(1, state["wMobilePeerCount"] + 1):
                prefix = f"wMobilePeer{peer}"
                if (not state[prefix + "Visible"] or not state.get(prefix + 'DrawValid', 1) or
                    state[prefix + "NativeStage"] != 7 or
                    state.get(prefix + 'LoadedSprite', 0) != state.get(prefix + 'Avatar', 0) or
                    state[prefix + "Status"] != 1 or
                    state[prefix + "MapGroup"] != state["wMapGroup"] or
                    state[prefix + "MapNumber"] != state["wMapNumber"] or
                    state[prefix + "Instance"] != state["wMobileInstanceKey"]):
                    continue
                identity = (peer, state[prefix + "Id"], state[prefix + "Generation"], state.get(prefix + 'Avatar', 0))
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
            "allowed_absent_frames": max_absent_frames, "witnesses": witnesses,
            "native_busy_frames": len(native_busy), "native_busy_frame_samples": native_busy[:16],
            "native_graphics_wait_frames": len(native_graphics), "native_graphics_wait_frame_samples": native_graphics[:16],
            "native_no_wait_frames": len(native_no_wait), "native_no_wait_frame_samples": native_no_wait[:16],
            "scope": "Eligible world frames with an admitted native idle window; native-heavy graphics frames and frozen menu occlusion are separate."}


def native_figure_report(rows, max_absent_frames=16):
    """Census every immutable native source, including those outside native OAM.

    A game object without a currently visible facing starts a new segment.
    Observe hardware-complete presentation rather than allocated object slots.
    The local-player continuity rule is reported separately from ordinary
    complete-figure sharing. An unavailable source observer cannot claim a pass.
    """
    figures, last, witnesses, local_failures, maximum = {}, {}, [], [], 0
    observed = False
    for row in rows:
        state, frame = row['symbols'], row['frame']
        if not state['wMobileOnline']:
            last.clear()
            continue
        if state.get('hInMenu', 0) or state.get('wScriptRunning', 0) == 255:
            last.clear()
            continue
        active = set()
        observed |= 'vNativeSource0Count' in state
        for slot in range(13):
            prefix = f'vNativeSource{slot}'
            if not state.get(prefix + 'Count', 0):
                continue
            key = (slot, state[prefix + 'Sprite'], state[prefix + 'Map'])
            active.add(key)
            item = figures.setdefault(key, {'native_slot': slot, 'sprite': key[1],
                'map_object': key[2], 'source_frames': 0,
                'complete_hardware_frames': 0, 'maximum_absent_frames': 0})
            item['source_frames'] += 1
            complete = bool(state[prefix + 'Complete'])
            item['complete_hardware_frames'] += complete
            if slot == 0 and state.get(prefix + 'VanillaComplete', 0) and not complete and len(local_failures) < 8:
                local_failures.append({'frame': frame, 'native_slot': 0})
            prior = last.get(key, frame - 1)
            absent = 0 if complete else frame - prior
            item['maximum_absent_frames'] = max(item['maximum_absent_frames'], absent)
            maximum = max(maximum, absent)
            if slot and absent > max_absent_frames and len(witnesses) < 8:
                witnesses.append({'frame': frame, 'native_slot': slot,
                    'sprite': key[1], 'map_object': key[2], 'absent_frames': absent})
            last[key] = frame if complete else prior
        last = {key: value for key, value in last.items() if key in active}
    return {'passed': observed and bool(figures) and not witnesses and not local_failures, 'observed': observed,
        'local_continuity_witnesses': local_failures,
        'maximum_absent_frames': maximum, 'allowed_ordinary_absent_frames': max_absent_frames,
        'figures': list(figures.values()), 'witnesses': witnesses,
        'scope': 'All visible facing sources, including figures that vanilla could not fit into OAM.'}


def verify_native_scanlines(output, players):
    reports = []
    for client in range(players):
        samples, failures, maximum, witnesses = 0, 0, 0, []
        temporal_seen = set()
        trace_path = output / str(client) / "trace.jsonl"
        for row in trace_rows(trace_path):
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
            temporal = virtual_temporal_report(trace_rows(trace_path))
            expected = {peer for peer in range(1, final_state["wMobilePeerCount"] + 1)
                        if final_state[f"wMobilePeer{peer}Visible"]}
            never_presented = sorted(expected - temporal_seen)
        native = native_figure_report(trace_rows(trace_path)) if "wMobileVirtualActorMode" in final_state else None
        reports.append({"client": client, "samples": samples, "maximum_demand": maximum,
                        "failing_samples": failures, "witnesses": witnesses,
                        "final_missing_peers": missing_final_peers(final_state),
                        "temporal_visibility": temporal, "native_figures": native,
                        "never_presented_peers": never_presented})
    result = {"passed": all(report["samples"] > 0 and not report["failing_samples"] and
                           not report["final_missing_peers"] and
                           not report["never_presented_peers"] and
                           (report["native_figures"] is None or not report["native_figures"]["observed"] or
                            report["native_figures"]["passed"]) and
                           (report["temporal_visibility"] is None or report["temporal_visibility"]["passed"])
                           for report in reports),
              "object_height": 8, "model": "first ten Y-matching hardware OAM entries",
              "scope": "Frame-boundary hardware OAM selection, not background/pixel compositing or every mid-frame DMA state.",
              "clients": reports}
    (output / "native-scanlines.json").write_text(json.dumps(result, indent=2) + "\n")
    require(result["passed"], "Native scanline/visibility acceptance failed; see native-scanlines.json")
    return result
