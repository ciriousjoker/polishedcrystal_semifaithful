#!/usr/bin/env python3
"""Source inventory and conservative acceptance gate for native trainer battles.

This tool does not infer implementation or test coverage from a table's existence.
It requires an explicit, source-bound evidence ledger. Default build only.
"""
from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
SCHEMA = 1
PHASES = ("unproven", "present-executable", "tested-routine", "tested-full-fight")


def sha(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def lines(root: Path, path: str):
    """Yield default non-FAITHFUL lines, retaining original one-based locations."""
    active, stack = True, []
    for number, raw in enumerate((root / path).read_text().splitlines(), 1):
        code = raw.split(";", 1)[0].strip()
        if code.startswith("if "):
            if code not in ("if DEF(FAITHFUL)", "if !DEF(FAITHFUL)", "if DEF(VIRTUAL_CONSOLE)"):
                raise ValueError(f"Unsupported conditional {path}:{number}: {code}")
            branch = code == "if !DEF(FAITHFUL)"
            stack.append((active, branch))
            active = active and branch
        elif code == "else":
            parent, branch = stack[-1]
            active = parent and not branch
        elif code == "endc":
            active = stack.pop()[0]
        elif active:
            yield number, code
    if stack:
        raise ValueError(f"Unterminated conditional: {path}")


def constants(root: Path, path: str, stop: str):
    result, value = [], 0
    for number, code in lines(root, path):
        if code.startswith(stop):
            break
        match = re.fullmatch(r"const_def(?: (\d+))?", code)
        if match:
            value = int(match[1] or 0)
        elif code.startswith("const "):
            result.append((code.split()[1], value, number))
            value += 1
        elif code.startswith(("const_skip", "const_next")):
            raise ValueError(f"Unhandled constant numbering: {path}:{number}")
    return result


def runtime_option_bits(root):
    wanted = {"ABILITIES_OPT", "NO_EXP_OPT", "BATTLE_SWITCH", "BATTLE_PREDICT"}
    found, value = {}, 0
    for number, code in lines(root, "constants/ram_constants.asm"):
        match = re.fullmatch(r"const_def(?: (\d+))?", code)
        if match:
            value = int(match[1] or 0)
        elif code.startswith("const "):
            name = code.split()[1]
            if name in wanted:
                found[name] = value
            value += 1
        elif code.startswith("const_skip"):
            value += int(code.split()[1]) if len(code.split()) == 2 else 1
        if set(found) == wanted:
            return found
    raise ValueError("Missing runtime option bit constants")


def source_inputs(root: Path):
    paths = set()
    for directory in ("engine", "home", "constants", "data", "macros", "ram"):
        for suffix in ("*.asm", "*.inc"):
            paths.update((root / directory).rglob(suffix))
    for suffix in ("*.asm", "*.inc"):
        paths.update(root.glob(suffix))
    return {str(path.relative_to(root)): sha(path.read_bytes()) for path in sorted(paths)}


def inventory(root: Path):
    records = []

    def add(group, name, path, number, **fields):
        records.append({"key": f"{group}:{name}", "group": group, "name": name,
                        "source": path, "line": number, "required": True,
                        "phase": "unproven", **fields})

    move_path = "data/moves/moves.asm"
    move_ids = constants(root, "constants/move_constants.asm", "DEF NUM_ATTACKS")
    moves = [(n, code[5:].split(",")) for n, code in lines(root, move_path)
             if code.startswith("move ")]
    if len(moves) != len(move_ids) - 1:
        raise ValueError("Move table/ID count mismatch")
    for (number, fields), (canonical, identifier, _) in zip(moves, move_ids[1:]):
        fields = [field.strip() for field in fields]
        if len(fields) != 8:
            raise ValueError(f"Malformed move {number}")
        add("move", canonical, move_path, number, identifier=identifier,
            animation=fields[0], effect=fields[1], power=fields[2], type=fields[3],
            accuracy=fields[4], pp=fields[5], chance=fields[6], category=fields[7])
    pointers = [(n, code[3:].strip()) for n, code in lines(root, "data/moves/effects_pointers.asm")
                if code.startswith("dw ")]
    effects = constants(root, "constants/move_effect_constants.asm", "DEF NUM_MOVE_EFECTS")
    if len(pointers) != len(effects):
        raise ValueError("Effect pointer/ID count mismatch")
    for (name, identifier, _), (number, script) in zip(effects, pointers):
        add("effect", name, "data/moves/effects_pointers.asm", number,
            identifier=identifier, script=script)
    for name, identifier, number in constants(root, "constants/ability_constants.asm", "DEF NUM_ABILITIES"):
        add("ability", name, "constants/ability_constants.asm", number,
            identifier=identifier)
    item_ids = constants(root, "constants/item_constants.asm", "DEF NUM_ITEMS")
    attributes = [(n, code[len("item_attribute "):].split(","))
                  for n, code in lines(root, "data/items/attributes.asm")
                  if code.startswith("item_attribute ")]
    if len(attributes) != len(item_ids) - 1:
        raise ValueError("Item attributes/ID count mismatch")
    item_effects = [(n, code[3:].strip())
                    for n, code in lines(root, "data/items/effects.asm")
                    if code.startswith("db ")]
    if len(item_effects) != len(item_ids):
        raise ValueError("Item effect/ID count mismatch")
    held, bag_effects = {}, {}
    for (number, values), (name, identifier, _) in zip(attributes, item_ids[1:]):
        values = [value.strip() for value in values]
        add("item", name, "data/items/attributes.asm", number, identifier=identifier,
            held_effect=values[1], parameter=values[2], required=False,
            reason="Inventory entry; held-effect and trainer-item mechanisms are separate required rules.")
        if values[5] != "ITEMMENU_NOUSE":
            effect_line, effect = item_effects[identifier]
            add("player-bag-item", name, "data/items/attributes.asm", number,
                identifier=identifier, effect=effect, battle_menu=values[5],
                target_contract="source bag dispatch, including ordinary-trainer no-effect/rejection rules")
            bag_effects.setdefault(effect, []).append(name)
        if values[1] != "0":
            held.setdefault(values[1], []).append(name)
    for effect, items in sorted(held.items()):
        first = next(n for n, values in attributes if values[1].strip() == effect)
        add("held-effect", effect, "data/items/attributes.asm", first, items=items)
    for effect, items in sorted(bag_effects.items()):
        number = next(n for n, name in item_effects if name == effect)
        add("player-bag-effect", effect, "data/items/effects.asm", number, items=items)
    in_table = False
    for number, code in lines(root, "engine/battle/ai/items.asm"):
        if code == "AI_Items:":
            in_table = True
        elif in_table and code == "db $ff":
            break
        elif in_table and code.startswith("dbw "):
            name, handler = [part.strip() for part in code[4:].split(",")]
            add("trainer-item", name, "engine/battle/ai/items.asm", number, handler="AI_Items" + handler)
    battle_path = "constants/battle_constants.asm"
    for number, code in lines(root, battle_path):
        match = re.fullmatch(r"const (SUBSTATUS_\w+|WEATHER_\w+|SWITCH_\w+|PSN|BRN|FRZ|PAR|TOX)", code)
        if match:
            name = match[1]
            group = ("volatile" if name.startswith("SUBSTATUS_") else
                     "field" if name.startswith("WEATHER_") else
                     "switch" if name.startswith("SWITCH_") else "status")
            add(group, name, battle_path, number)
        match = re.match(r"DEF ((?:HAZARDS|SCREENS|GUARD)_\w+)\s+EQU", code)
        if match:
            add("field", match[1], battle_path, number)
        if code.startswith("DEF SLP_MASK "):
            add("status", "SLP_MASK", battle_path, number)
    endturn = "engine/battle/endturn.asm"
    for number, code in lines(root, endturn):
        if code == ".endturn_loop":
            break
        match = re.fullmatch(r"call (\w+)", code)
        if match and match[1] != "CheckFaint":
            add("endturn", match[1], endturn, number)
    controller_names = {"DoBattle", "GiveExperience", "CustomTrainerActions", "RunEntryAbilities",
                        "BattleTurn", "PerformMove", "ResolveFaints", "DeferredSwitch",
                        "GetBothSwitchTarget", "GetUserSwitchTarget", "SendInUserPkmn",
                        "UserCanSwitch", "ParseEnemyAction", "ParsePlayerAction"}
    for number, code in lines(root, "engine/battle/core.asm"):
        name = code.rstrip(":")
        if name in controller_names and code.endswith(":"):
            add("controller", name, "engine/battle/core.asm", number)
    for path, name in (("engine/battle/endturn.asm", "HandleBetweenTurnEffects"),
                       ("engine/pokemon/evolve.asm", "LearnLevelMoves"),
                       ("engine/pokemon/learn.asm", "LearnMove"),
                       ("engine/pokemon/move_mon.asm", "FixPlayerEVsAndStats"),
                       ("engine/items/item_effects.asm", "_DoItemEffect")):
        for number, raw in enumerate((root / path).read_text().splitlines(), 1):
            if raw.strip().rstrip(":") == name and raw.strip().endswith(":"):
                add("controller", name, path, number)
                break
        else:
            raise ValueError(f"Missing required controller: {name}")
    for record in records:
        if record["key"] in {"controller:GiveExperience", "controller:LearnLevelMoves", "controller:LearnMove"}:
            record.update(required=False, excluded_by_scope="user-fixed-level-no-experience",
                          reason="User requires fixed levels/base stats, no experience awards or progression move learning.")
    keys = [record["key"] for record in records]
    if len(keys) != len(set(keys)):
        raise ValueError("Duplicate coverage keys")
    inputs = source_inputs(root)
    body = {"schema": SCHEMA, "profile": "default-non-faithful-ordinary-trainer-fixed-level",
            "source_inputs": inputs, "rules": records, "runtime_option_bits": runtime_option_bits(root),
            "excluded_modes": ["wild", "Safari", "link", "Battle Tower progression", "post-fight rewards UI"],
            "not_in_game": ["Wish", "Aqua Ring", "Ingrain", "Nightmare", "Taunt", "terrain"]}
    body["inventory_sha256"] = sha(json.dumps(body, sort_keys=True).encode())
    return body


def validate_configuration(configuration):
    """Declare a bounded runtime contract; disabled mechanics do not waive rows."""
    if not isinstance(configuration, dict):
        raise ValueError("Configuration must be an object")
    roles = configuration.get("roles")
    if not isinstance(roles, dict) or set(roles) != {"player", "enemy"}:
        raise ValueError("Missing explicit actor roles")
    if any(role not in ("policy", "scripted", "source-ai") for role in roles.values()):
        raise ValueError("Invalid actor role")
    options = configuration.get("options")
    if not isinstance(options, dict):
        raise ValueError("Missing explicit runtime options")
    for option, required in {"abilities": True, "experience": False, "player_bag": True, "fixed_levels": True}.items():
        if type(options.get(option)) is not bool:
            raise ValueError(f"Option must be boolean: {option}")
        if options[option] is not required:
            raise ValueError(f"Option violates fixed-level ordinary M2 contract: {option}")
    if options.get("battle_style") not in ("set", "switch", "predict"):
        raise ValueError("Invalid battle style")
    if type(options.get("banked_wram", False)) is not bool:
        raise ValueError("banked_wram capability must be boolean")


def execution_artifacts(report, originals, evidence_root, manifest, current):
    """Bind trusted runner receipts to retained bytes, not matching digest labels."""
    profiles = {"bounded-mbc3-flat-wram-div-event-tape": False,
                "bounded-mbc3-banked-wram-div-event-tape": True}
    profile = manifest.get("profile")
    if manifest.get("schema") != 1 or profile not in profiles:
        raise ValueError("Unsupported execution manifest schema/profile")
    manifest_banked = manifest.get("banked_wram", False)
    configured_banked = report["configuration"]["options"].get("banked_wram", False)
    if type(manifest_banked) is not bool or type(configured_banked) is not bool:
        raise ValueError("banked_wram capability must be boolean")
    if manifest_banked is not profiles[profile] or configured_banked is not manifest_banked:
        raise ValueError("WRAM profile/capability mismatch")
    if not isinstance(manifest.get("entries"), dict) or not manifest["entries"]:
        raise ValueError("Execution manifest has no source entries")
    retained = {item["path"]: item for item in originals}
    artifacts = report.get("execution_artifacts", {})
    payloads = {}
    for kind in ("rom", "symbols", "generator", "build_receipt"):
        path = artifacts.get(kind)
        if path not in retained:
            raise ValueError(f"Missing retained execution bytes: {kind}")
        payloads[kind] = (evidence_root / path).read_bytes()
    for kind, field in (("rom", "rom_sha256"), ("symbols", "symbols_sha256"), ("generator", "generator_sha256")):
        if sha(payloads[kind]) != manifest.get(field) or sha(payloads[kind]) != report.get(field):
            raise ValueError(f"Retained {kind} bytes mismatch")
    receipt = json.loads(payloads["build_receipt"])
    if receipt.get("schema") != 1 or receipt.get("inventory_sha256") != current["inventory_sha256"]:
        raise ValueError("Build receipt inventory mismatch")
    for field in ("rom_sha256", "symbols_sha256"):
        if receipt.get(field) != report[field]:
            raise ValueError(f"Build receipt mismatch: {field}")
    target = receipt.get("target", "")
    if not target.endswith(".gbc") or any(part in target.lower() for part in ("faithful", "_vc", "monochrome")):
        raise ValueError("Unsupported build target")
    invocations = receipt.get("assembler_invocations", [])
    if not invocations or receipt.get("effective_defines") != []:
        raise ValueError("Unsupported or missing effective build defines")
    for argv in invocations:
        if not isinstance(argv, list) or not argv or not all(isinstance(arg, str) for arg in argv):
            raise ValueError("Invalid assembler invocation")
        if Path(argv[0]).name != "rgbasm" or any(arg.startswith("-D") for arg in argv[1:]):
            raise ValueError("Unsupported assembler or build define")
    text = payloads["symbols"].decode()
    symbols = {}
    for raw in text.splitlines():
        match = re.fullmatch(r"([0-9a-fA-F]{2}):([0-9a-fA-F]{4}) (\S+)", raw.strip())
        if match:
            symbols[match[3]] = (int(match[1], 16), int(match[2], 16))
    if {"FAITHFUL", "VIRTUAL_CONSOLE", "MONOCHROME"} & set(symbols):
        raise ValueError("Symbol file declares unsupported build define")
    return retained, symbols


def runtime_case_options(case, retained, symbols, evidence_root, configuration, current):
    reference = case.get("initial_wram")
    if reference not in retained:
        raise ValueError("Missing retained initial WRAM bytes")
    wram = (evidence_root / reference).read_bytes()
    if len(wram) != 8192:
        raise ValueError("Initial WRAM snapshot must contain C000-DFFF")
    values = {}
    for name in ("wInitialOptions", "wInitialOptions2", "wOptions2"):
        bank, address = symbols[name]
        if bank != 0 or not 0xc000 <= address < 0xe000:
            raise ValueError("Option symbol outside supported WRAM")
        values[name] = wram[address - 0xc000]
    bits = current["runtime_option_bits"]
    abilities_mask = 1 << bits["ABILITIES_OPT"]
    no_exp_mask = 1 << bits["NO_EXP_OPT"]
    switch_mask = 1 << bits["BATTLE_SWITCH"]
    predict_mask = 1 << bits["BATTLE_PREDICT"]
    style_bits = values["wOptions2"] & (switch_mask | predict_mask)
    if style_bits == (switch_mask | predict_mask):
        raise ValueError("Invalid simultaneous Switch/Predict option bits")
    actual = {"abilities": bool(values["wInitialOptions"] & abilities_mask),
              "experience": not bool(values["wInitialOptions2"] & no_exp_mask),
              "battle_style": {0: "set", switch_mask: "switch", predict_mask: "predict"}[style_bits]}
    if any(configuration["options"][key] != value for key, value in actual.items()):
        raise ValueError("Initial WRAM runtime options disagree with configuration")
    contract = case.get("choice_contract", {})
    if contract.get("roles") != configuration["roles"] or contract.get("player_bag") is not configuration["options"]["player_bag"]:
        raise ValueError("Executed choice contract disagrees with roles or player bag")


def evaluate(current, ledger, evidence_root: Path):
    failures = []
    configuration = ledger.get("configuration", {})
    if not isinstance(configuration, dict):
        configuration = {}
        failures.append("Configuration must be an object")
    scope = ledger.get("acceptance_scope")
    if scope not in ("conditional-native-gameplay", "ordinary-frontend-projection"):
        failures.append("Missing explicit acceptance scope")
    if configuration.get("mode") != "ordinary-trainer" or configuration.get("ruleset") != "default-non-faithful":
        failures.append("Missing supported trainer mode/ruleset configuration")
    try:
        validate_configuration(configuration)
    except (ValueError, TypeError) as error:
        failures.append(str(error))
    if configuration.get("profile_kind") not in ("headless-iid-div", "ordinary-frontend-projection"):
        failures.append("Missing explicit execution profile kind")
    if scope == "ordinary-frontend-projection" and configuration.get("profile_kind") != scope:
        failures.append("Conditional same-hook parity cannot authorize ordinary frontend projection")
    if ledger.get("inventory_sha256") != current["inventory_sha256"]:
        failures.append("Stale or unmatched source inventory")
    rules = {rule["key"]: rule for rule in current["rules"]}
    claims = ledger.get("claims", {})
    for key in sorted(set(claims) - set(rules)):
        failures.append(f"Unknown rule: {key}")
    evidence = ledger.get("evidence", {})
    valid = set()
    whole_fight_rules = {}
    for name, entry in evidence.items():
        try:
            path = (evidence_root / entry["path"]).resolve()
            path.relative_to(evidence_root.resolve())
            if sha(path.read_bytes()) != entry["sha256"]:
                raise ValueError("Artifact digest mismatch")
            report = json.loads(path.read_text())
            # An adapter report must identify the exact source inventory and retain
            # its original report(s) as hash-bound artifacts, never just prose.
            if report.get("inventory_sha256") != current["inventory_sha256"]:
                raise ValueError("Evidence source inventory mismatch")
            for field in ("rom_sha256", "symbols_sha256", "generator_sha256", "manifest_sha256"):
                if not re.fullmatch(r"[0-9a-f]{64}", report.get(field, "")):
                    raise ValueError(f"Missing execution provenance: {field}")
            if report.get("unsupported_cases", 0) or report.get("failed_cases", 0):
                raise ValueError("Unsupported or failed scenarios")
            if report.get("full_state_comparisons", 0) < 1:
                raise ValueError("No full CPU/RAM comparison")
            if report.get("configuration") != configuration:
                raise ValueError("Evidence game configuration/roles/options/profile mismatch")
            if scope == "ordinary-frontend-projection" and report.get("oracle_kind") != "real-frontend-gameplay-projection":
                raise ValueError("Ordinary frontend projection lacks independent frontend oracle")
            originals = report.get("artifacts", [])
            if not originals:
                raise ValueError("Missing original execution artifacts")
            for artifact in originals:
                original = (evidence_root / artifact["path"]).resolve()
                original.relative_to(evidence_root.resolve())
                if sha(original.read_bytes()) != artifact["sha256"]:
                    raise ValueError("Original artifact missing or stale")
            manifests = [artifact for artifact in originals
                         if artifact["sha256"] == report["manifest_sha256"]]
            if not manifests:
                raise ValueError("Execution manifest is not retained as a hash-bound artifact")
            manifest = json.loads((evidence_root / manifests[0]["path"]).read_text())
            for field in ("rom_sha256", "generator_sha256"):
                if manifest.get(field) != report[field]:
                    raise ValueError(f"Execution manifest mismatch: {field}")
            for path, digest in current.get("source_inputs", {}).items():
                if manifest.get("sources", {}).get(path) != digest:
                    raise ValueError(f"Execution manifest source mismatch: {path}")
            retained, symbols = execution_artifacts(report, originals, evidence_root, manifest, current)
            if set(report.get("rule_keys", [])) - set(rules):
                raise ValueError("Evidence contains unknown rule IDs")
            # Require actual retained case records, not only adapter totals.
            exercised, full_fight_exercised = set(), set()
            for reference in report.get("case_reports", []):
                artifact = next((item for item in originals if item["path"] == reference), None)
                if artifact is None:
                    raise ValueError("Case report is not a retained artifact")
                case_report = json.loads((evidence_root / reference).read_text())
                if case_report.get("manifest_sha256") != report["manifest_sha256"]:
                    raise ValueError("Case report manifest mismatch")
                if case_report.get("configuration") != configuration:
                    raise ValueError("Case report configuration mismatch")
                cases = case_report.get("cases", [])
                if not cases:
                    raise ValueError("No original case records")
                for case in cases:
                    runtime_case_options(case, retained, symbols, evidence_root, configuration, current)
                    if not case.get("passed") or case.get("unsupported", True):
                        raise ValueError("Original case failed or is unsupported")
                    if case.get("full_state_comparisons", 0) < 1:
                        raise ValueError("Original case has no state comparison")
                    if configuration["options"].get("banked_wram", False):
                        components = case.get("full_state_components", [])
                        required = {"cpu", "hram", "wram-banks-0-7", "svbk"}
                        if not isinstance(components, list) or not required.issubset(set(components)):
                            raise ValueError("Banked WRAM case lacks complete bank/SVBK state comparison")
                    case_rules = set(case.get("rule_keys", []))
                    if case_rules - set(rules):
                        raise ValueError("Original case contains unknown rule IDs")
                    exercised.update(case_rules)
                    if case.get("complete_fight"):
                        full_fight_exercised.update(case_rules)
            if set(report.get("rule_keys", [])) - exercised:
                raise ValueError("Declared rules lack retained case records")
            whole_fight_rules[name] = full_fight_exercised
            valid.add(name)
        except (KeyError, ValueError, OSError, TypeError) as error:
            failures.append(f"Invalid evidence {name}: {error}")
    covered = Counter()
    for key, rule in rules.items():
        claim = claims.get(key, {})
        phase = claim.get("phase", "unproven")
        if phase not in PHASES:
            failures.append(f"Invalid phase {key}: {phase}")
            continue
        if phase != "unproven":
            refs = claim.get("evidence", [])
            if not refs or any(ref not in valid for ref in refs):
                failures.append(f"Missing valid evidence: {key}")
                continue
            for ref in refs:
                report = json.loads((evidence_root / evidence[ref]["path"]).read_text())
                if key not in report.get("rule_keys", []):
                    failures.append(f"Evidence does not exercise rule {key}: {ref}")
                if phase == "tested-full-fight" and (report.get("complete_fights", 0) < 1 or key not in whole_fight_rules[ref]):
                    failures.append(f"No complete fight evidence: {key}")
        covered[phase] += 1
        if rule["required"] and phase != "tested-full-fight":
            failures.append(f"Required rule lacks whole-fight evidence: {key}")
    return {"accepted": not failures, "failures": failures,
            "phase_counts": dict(covered), "inventory_sha256": current["inventory_sha256"],
            "acceptance_scope": scope, "configuration": configuration}


def markdown(current, result=None, ledger=None):
    counts = Counter(rule["group"] for rule in current["rules"])
    out = ["# Trainer battle source coverage", "", "Table presence is not implementation or test evidence.", "",
           f"Inventory: `{current['inventory_sha256']}`", "", "| Category | Source rules |", "| --- | ---: |"]
    out.extend(f"| {name} | {count} |" for name, count in sorted(counts.items()))
    if result:
        out.extend(["", f"M2 acceptance: **{'PASS' if result['accepted'] else 'FAIL'}**", ""])
        out.extend(f"- {failure}" for failure in result["failures"])
    out.extend(["", "## Source rules", "", "| Key | Source | Evidence | Scope |", "| --- | --- | --- | --- |"])
    for rule in current["rules"]:
        phase = (ledger or {}).get("claims", {}).get(rule["key"], {}).get("phase", "unproven")
        scope = "required" if rule["required"] else "informational: " + rule.get("reason", "excluded by explicit scope")
        out.append(f"| `{rule['key']}` | `{rule['source']}:{rule['line']}` | {phase} | {scope} |")
    return "\n".join(out) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--ledger", type=Path)
    parser.add_argument("--evidence-root", type=Path)
    args = parser.parse_args()
    current = inventory(args.root)
    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / "inventory.json").write_text(json.dumps(current, indent=2) + "\n")
    ledger = json.loads(args.ledger.read_text()) if args.ledger else {"inventory_sha256": current["inventory_sha256"], "claims": {}, "evidence": {}, "acceptance_scope": "conditional-native-gameplay",
        "configuration": {"mode": "ordinary-trainer", "ruleset": "default-non-faithful",
                          "profile_kind": "headless-iid-div", "options": {"abilities": True, "experience": False,
                          "fixed_levels": True, "player_bag": True, "battle_style": "set"}, "roles": {}}}
    result = evaluate(current, ledger, args.evidence_root or args.output)
    (args.output / "acceptance.json").write_text(json.dumps(result, indent=2) + "\n")
    (args.output / "COVERAGE.md").write_text(markdown(current, result, ledger))
    if not args.ledger:
        (args.output / "ledger-template.json").write_text(json.dumps(ledger, indent=2) + "\n")
    print(json.dumps({"accepted": result["accepted"], "rules": len(current["rules"]), "failures": len(result["failures"])}))
    return 0 if result["accepted"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
