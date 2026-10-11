"""Portable ordinary-save scenarios shared by deployment and integration tests."""
import json
from pathlib import Path
import sys
from probe_emulator import require, validate_battery

ROOT = Path(__file__).resolve().parents[1] / "tests/scenarios"
AVATARS = ("chris", "kris", "crys", "beta")


def catalog(root=ROOT):
    data = json.loads((root / "manifest.json").read_text())
    require(data["schema"] == 1, "Unknown save scenario catalog version")
    return data["scenarios"]


def choose(requested=None, *, root=ROOT, interactive=None, ask=input):
    entries = catalog(root)
    if requested:
        selected = next((entry for entry in entries if entry["id"] == requested), None)
        require(selected is not None, "Unknown scenario; choose " + ", ".join(e["id"] for e in entries))
        return selected
    if interactive is None:
        interactive = sys.stdin.isatty()
    require(interactive, "Pass --scenario bedroom, game-corner or goldenrod-city when stdin is not a terminal")
    print("Choose the starting save for both devices:")
    for number, entry in enumerate(entries, 1):
        print(f"  {number}. {entry['label']}: {entry['description']}")
    while True:
        value = ask("Scenario [1]: ").strip() or "1"
        if value.isdigit() and 1 <= int(value) <= len(entries):
            return entries[int(value) - 1]
        selected = next((entry for entry in entries if entry["id"] == value), None)
        if selected:
            return selected
        print("Choose a listed number or scenario ID.")


def battery(entry, avatar, symbols, root=ROOT):
    require(avatar in AVATARS, "Unknown player figure")
    record = entry["saves"][avatar]
    path = (root / record["file"]).resolve()
    require(path.is_relative_to(root.resolve()), "Scenario save must remain inside the fixture folder")
    metadata = validate_battery(path, symbols)
    require(metadata["sha256"] == record["sha256"], "Scenario save was modified; regenerate its catalog")
    return path


def expected(entry, avatar):
    return {"wMapGroup": entry["group"], "wMapNumber": entry["number"],
            "wXCoord": entry["x"], "wYCoord": entry["y"],
            "wPlayerGender": AVATARS.index(avatar)}
