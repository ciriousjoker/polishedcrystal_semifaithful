"""Portable ordinary-save scenarios shared by deployment and integration tests."""
import json
from pathlib import Path
import sys
from probe_emulator import require, validate_battery

ROOT = Path(__file__).resolve().parents[1] / "tests/scenarios"
AVATARS = ("chris", "kris", "crys", "beta")
# Native player-object appearance is persisted along with the selected figure.
APPEARANCE_FIELDS = ("wPlayerGender", "wPlayerSprite", "wPlayerObjectSprite", "wPlayerObjectPalette",
                     "wPlayerPalIndex", "wLoadedObjPal0", "wNeededPalIndex")


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


def session_battery(entry, avatar, symbols, destination, trainer_id):
    """Clone one ordinary save, changing only appearance, trainer ID and checksums.

    This is a development fixture transformation, never a production save path.
    All players share the Chris fixture's progress, party, clock and position.
    """
    require(avatar in AVATARS and 0 <= trainer_id <= 65535, "Invalid session player")
    data = bytearray(battery(entry, "chris", symbols).read_bytes())
    appearance = battery(entry, avatar, symbols).read_bytes()

    def offset(name):
        bank, address = symbols[name]
        require(bank < 4 and 0xa000 <= address < 0xc000, "Invalid scenario SRAM layout")
        return bank * 0x2000 + address - 0xa000

    base = symbols["wPlayerData"][1]
    id_offset = symbols["wPlayerID"][1] - base
    require(id_offset >= 0, "Invalid player data layout")
    for prefix in ("s", "sBackup"):
        start, end = offset(prefix + "GameData"), offset(prefix + "GameDataEnd")
        player = offset(prefix + "PlayerData")
        require(start <= player + id_offset < player + id_offset + 2 <= end,
                "Player fields must be inside the checksummed save")
        data[player + id_offset:player + id_offset + 2] = trainer_id.to_bytes(2, "big")
        for name in APPEARANCE_FIELDS:
            relative = symbols[name][1] - base
            require(start <= player + relative < end, "Appearance outside the ordinary player save")
            data[player + relative] = appearance[player + relative]
        checksum = offset(prefix + "Checksum")
        data[checksum:checksum + 2] = (sum(data[start:end]) & 65535).to_bytes(2, "little")
    destination = Path(destination)
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_bytes(data)
    validate_battery(destination, symbols)
    return destination
