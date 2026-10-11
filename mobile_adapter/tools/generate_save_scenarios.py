#!/usr/bin/env python3
"""Generate portable scenario batteries through the ordinary in-game Save menu."""
import argparse
import json
from pathlib import Path
import shutil
import subprocess
import sys
from save_scenarios import AVATARS, ROOT

SCENES = (
    ("bedroom", "Starter bedroom", "Simple avatar and menu test", "PlayersHouse2F", 3, 3),
    ("game-corner", "Goldenrod Game Corner", "Crowded interior with wandering patrons", "GoldenrodGameCorner", 14, 5),
    ("goldenrod-city", "Goldenrod City", "Outdoor scene with wandering and patrolling NPCs", "GoldenrodCity", 25, 18),
)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runner", type=Path, required=True)
    parser.add_argument("--core", type=Path, required=True)
    parser.add_argument("--artifacts", type=Path, required=True, help="Fresh external producer artifact directory")
    parser.add_argument("--output", type=Path, default=ROOT)
    args = parser.parse_args()
    args.artifacts.mkdir(parents=True, exist_ok=False)
    entries = []
    for key, label, description, map_name, x, y in SCENES:
        entry = {"id": key, "label": label, "description": description,
                 "map": map_name, "x": x, "y": y, "saves": {}}
        for avatar in AVATARS:
            artifact = args.artifacts / key / avatar
            subprocess.run([sys.executable, str(Path(__file__).with_name("create_scene_save.py")),
                            "--runner", str(args.runner), "--core", str(args.core),
                            "--output", str(artifact), "--map", map_name,
                            "--x", str(x), "--y", str(y), "--avatar", avatar], check=True)
            metadata = json.loads((artifact / "fixture.json").read_text())
            state = json.loads((artifact / "state.json").read_text())["symbols"]
            entry.update(group=state["wMapGroup"], number=state["wMapNumber"])
            destination = args.output / key / (avatar + ".sav")
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(artifact / "ingame.sav", destination)
            metadata["file"] = str(destination.relative_to(args.output))
            entry["saves"][avatar] = metadata
        entries.append(entry)
    (args.output / "manifest.json").write_text(json.dumps({"schema": 1, "scenarios": entries}, indent=2) + "\n")
    print(f"Created {len(entries)} scenarios, each with {len(AVATARS)} ordinary character-choice saves.")


if __name__ == "__main__":
    main()
