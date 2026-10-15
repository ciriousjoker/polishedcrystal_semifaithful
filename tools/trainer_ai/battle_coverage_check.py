#!/usr/bin/env python3
"""Small regression checks for false acceptance and source inventory parsing."""
from __future__ import annotations

import copy
import json
from pathlib import Path
import tempfile
import unittest

import battle_coverage as coverage


class CoverageGateTest(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.inventory = {"inventory_sha256": "a" * 64,
                          "rules": [{"key": "effect:TEST", "required": True}],
                          "source_inputs": {"main.asm": "f" * 64},
                          "runtime_option_bits": {"ABILITIES_OPT": 1, "NO_EXP_OPT": 2,
                                                  "BATTLE_SWITCH": 6, "BATTLE_PREDICT": 7}}
        self.configuration = {"mode": "ordinary-trainer", "ruleset": "default-non-faithful",
                              "profile_kind": "headless-iid-div", "roles": {"player": "policy", "enemy": "policy"},
                              "options": {"abilities": True, "experience": False, "fixed_levels": True, "player_bag": True, "battle_style": "set"}}
        self.files = {"rom.gbc": b"synthetic test ROM", "generator.py": b"# synthetic test generator",
                      "rom.sym": b"00:c001 wInitialOptions\n00:c002 wInitialOptions2\n00:c003 wOptions2\n"}
        wram = bytearray(8192)
        wram[1] = 2
        wram[2] = 4
        self.files["initial.wram"] = bytes(wram)
        self.manifest = {"schema": 1, "profile": "bounded-mbc3-flat-wram-div-event-tape",
                         "entries": {"DoBattle": [15, 0x4000]}, "sources": {"main.asm": "f" * 64},
                         "rom_sha256": coverage.sha(self.files["rom.gbc"]),
                         "symbols_sha256": coverage.sha(self.files["rom.sym"]),
                         "generator_sha256": coverage.sha(self.files["generator.py"])}
        self.receipt = {"schema": 1, "inventory_sha256": "a" * 64,
                        "rom_sha256": self.manifest["rom_sha256"],
                        "symbols_sha256": self.manifest["symbols_sha256"],
                        "target": "polishedcrystal.gbc", "effective_defines": [],
                        "assembler_invocations": [["rgbasm", "-E", "-o", "main.o", "main.asm"]]}
        self.files["build-receipt.json"] = json.dumps(self.receipt).encode()
        self.files["oracle.json"] = json.dumps(self.manifest).encode()
        for name, content in self.files.items():
            (self.root / name).write_bytes(content)
        self.report = {"inventory_sha256": "a" * 64,
                       **{field: self.manifest[field] for field in ("rom_sha256", "symbols_sha256", "generator_sha256")},
                       "manifest_sha256": coverage.sha(self.files["oracle.json"]),
                       "unsupported_cases": 0, "failed_cases": 0,
                       "full_state_comparisons": 1, "complete_fights": 1,
                       "configuration": copy.deepcopy(self.configuration),
                       "rule_keys": ["effect:TEST"],
                       "execution_artifacts": {"rom": "rom.gbc", "symbols": "rom.sym",
                                               "generator": "generator.py", "build_receipt": "build-receipt.json"},
                       "artifacts": [{"path": name, "sha256": coverage.sha(content)}
                                     for name, content in self.files.items()]}
        self.case_report = {"manifest_sha256": self.report["manifest_sha256"],
                            "configuration": copy.deepcopy(self.configuration),
                            "cases": [{"case_id": "battle-1", "passed": True, "unsupported": False,
                                       "full_state_comparisons": 1, "complete_fight": True,
                                       "rule_keys": ["effect:TEST"], "initial_wram": "initial.wram",
                                       "choice_contract": {"roles": self.configuration["roles"], "player_bag": True}}]}
        self.report["case_reports"] = ["cases.json"]
        self.ledger = {"inventory_sha256": "a" * 64,
                       "acceptance_scope": "conditional-native-gameplay", "configuration": self.configuration,
                       "evidence": {}, "claims": {"effect:TEST": {
                           "phase": "tested-full-fight", "evidence": ["test"]}}}

    def replace_retained(self, name, content):
        (self.root / name).write_bytes(content)
        for artifact in self.report["artifacts"]:
            if artifact["path"] == name:
                artifact["sha256"] = coverage.sha(content)

    def rewrite_manifest(self):
        content = json.dumps(self.manifest).encode()
        self.replace_retained("oracle.json", content)
        self.report["manifest_sha256"] = coverage.sha(content)
        self.case_report["manifest_sha256"] = self.report["manifest_sha256"]

    def enable_banked_wram(self):
        self.configuration["options"]["banked_wram"] = True
        self.report["configuration"] = copy.deepcopy(self.configuration)
        self.case_report["configuration"] = copy.deepcopy(self.configuration)
        self.manifest["profile"] = "bounded-mbc3-banked-wram-div-event-tape"
        self.manifest["banked_wram"] = True
        self.case_report["cases"][0]["full_state_components"] = ["cpu", "hram", "wram-banks-0-7", "svbk"]
        self.rewrite_manifest()

    def result(self):
        cases = self.root / "cases.json"
        cases.write_text(json.dumps(self.case_report))
        self.report["artifacts"] = [item for item in self.report["artifacts"] if item["path"] != "cases.json"]
        self.report["artifacts"].append({"path": "cases.json", "sha256": coverage.sha(cases.read_bytes())})
        path = self.root / "adapter.json"
        path.write_text(json.dumps(self.report))
        self.ledger["evidence"]["test"] = {"path": "adapter.json", "sha256": coverage.sha(path.read_bytes())}
        return coverage.evaluate(self.inventory, self.ledger, self.root)

    def test_valid_minimal_gate(self):
        self.assertTrue(self.result()["accepted"])

    def test_stale_source_is_rejected(self):
        self.ledger["inventory_sha256"] = "e" * 64
        self.assertFalse(self.result()["accepted"])

    def test_unknown_rule_is_rejected(self):
        self.ledger["claims"]["effect:INVENTED"] = copy.deepcopy(self.ledger["claims"]["effect:TEST"])
        self.assertFalse(self.result()["accepted"])

    def test_missing_required_rule_is_rejected(self):
        self.ledger["claims"] = {}
        self.assertFalse(self.result()["accepted"])

    def test_routine_is_not_whole_fight_coverage(self):
        self.ledger["claims"]["effect:TEST"]["phase"] = "tested-routine"
        self.assertFalse(self.result()["accepted"])

    def test_no_whole_fight_is_rejected(self):
        self.report["complete_fights"] = 0
        self.assertFalse(self.result()["accepted"])

    def test_no_state_comparison_is_rejected(self):
        self.report["full_state_comparisons"] = 0
        self.assertFalse(self.result()["accepted"])

    def test_failed_or_unsupported_is_rejected(self):
        for field in ("failed_cases", "unsupported_cases"):
            with self.subTest(field=field):
                self.report[field] = 1
                self.assertFalse(self.result()["accepted"])
                self.report[field] = 0

    def test_evidence_must_exercise_rule(self):
        self.report["rule_keys"] = []
        self.assertFalse(self.result()["accepted"])

    def test_original_artifact_staleness_is_rejected(self):
        (self.root / "oracle.json").write_text("changed")
        self.assertFalse(self.result()["accepted"])

    def test_original_artifact_cannot_escape_root(self):
        self.report["artifacts"][0]["path"] = "../outside.json"
        self.assertFalse(self.result()["accepted"])

    def test_unmatched_evidence_source_is_rejected(self):
        self.report["inventory_sha256"] = "e" * 64
        self.assertFalse(self.result()["accepted"])

    def test_missing_manifest_identity_is_rejected(self):
        del self.report["manifest_sha256"]
        self.assertFalse(self.result()["accepted"])

    def test_manifest_source_is_actually_checked(self):
        self.inventory["source_inputs"]["main.asm"] = "0" * 64
        self.assertFalse(self.result()["accepted"])

    def test_manifest_rom_is_actually_checked(self):
        self.report["rom_sha256"] = "e" * 64
        self.assertFalse(self.result()["accepted"])

    def test_unknown_evidence_rule_is_rejected(self):
        self.report["rule_keys"].append("effect:INVENTED")
        self.assertFalse(self.result()["accepted"])

    def test_same_hook_cannot_authorize_frontend(self):
        self.ledger["acceptance_scope"] = "ordinary-frontend-projection"
        self.assertFalse(self.result()["accepted"])

    def test_original_case_is_required(self):
        self.case_report["cases"] = []
        self.assertFalse(self.result()["accepted"])

    def test_original_case_must_exercise_claimed_rule(self):
        self.case_report["cases"][0]["rule_keys"] = []
        self.assertFalse(self.result()["accepted"])

    def test_configuration_mismatch_is_rejected(self):
        self.report["configuration"]["options"]["experience"] = True
        self.assertFalse(self.result()["accepted"])

    def test_mandatory_runtime_option_names(self):
        for option in ("abilities", "experience", "fixed_levels", "battle_style", "player_bag"):
            with self.subTest(option=option):
                saved = self.configuration["options"].pop(option)
                self.assertFalse(self.result()["accepted"])
                self.configuration["options"][option] = saved

    def test_bad_runtime_option_types(self):
        for option in ("abilities", "experience", "fixed_levels", "player_bag"):
            with self.subTest(option=option):
                saved = self.configuration["options"][option]
                self.configuration["options"][option] = 1
                self.assertFalse(self.result()["accepted"])
                self.configuration["options"][option] = saved

    def test_unknown_or_null_actor_role(self):
        for value in (None, "unrecognized"):
            self.configuration["roles"]["player"] = value
            self.assertFalse(self.result()["accepted"])

    def test_experience_cannot_override_user_fixed_level_scope(self):
        self.configuration["options"]["experience"] = True
        self.assertFalse(self.result()["accepted"])

    def test_invalid_battle_style(self):
        self.configuration["options"]["battle_style"] = "guess"
        self.assertFalse(self.result()["accepted"])

    def test_actual_initial_wram_options_disagree(self):
        snapshot = bytearray(self.files["initial.wram"])
        snapshot[2] = 0
        self.replace_retained("initial.wram", bytes(snapshot))
        self.assertFalse(self.result()["accepted"])

    def test_actual_initial_wram_snapshot_required(self):
        del self.case_report["cases"][0]["initial_wram"]
        self.assertFalse(self.result()["accepted"])

    def test_actual_choice_contract_disagrees(self):
        self.case_report["cases"][0]["choice_contract"]["player_bag"] = False
        self.assertFalse(self.result()["accepted"])

    def test_raw_rom_and_generator_artifacts_required(self):
        for kind in ("rom", "symbols", "generator", "build_receipt"):
            with self.subTest(kind=kind):
                value = self.report["execution_artifacts"].pop(kind)
                self.assertFalse(self.result()["accepted"])
                self.report["execution_artifacts"][kind] = value

    def test_actual_rom_bytes_must_match_manifest(self):
        self.replace_retained("rom.gbc", b"different retained ROM")
        self.assertFalse(self.result()["accepted"])

    def test_actual_generator_bytes_must_match_manifest(self):
        self.replace_retained("generator.py", b"different retained generator")
        self.assertFalse(self.result()["accepted"])

    def test_faithful_define_receipt_is_rejected(self):
        self.receipt["effective_defines"] = ["FAITHFUL"]
        self.replace_retained("build-receipt.json", json.dumps(self.receipt).encode())
        self.assertFalse(self.result()["accepted"])

    def test_hidden_assembler_define_is_rejected(self):
        self.receipt["assembler_invocations"][0].append("-DFAITHFUL")
        self.replace_retained("build-receipt.json", json.dumps(self.receipt).encode())
        self.assertFalse(self.result()["accepted"])

    def test_missing_build_invocations_is_rejected(self):
        self.receipt["assembler_invocations"] = []
        self.replace_retained("build-receipt.json", json.dumps(self.receipt).encode())
        self.assertFalse(self.result()["accepted"])

    def test_build_receipt_is_rom_bound(self):
        self.receipt["rom_sha256"] = "e" * 64
        self.replace_retained("build-receipt.json", json.dumps(self.receipt).encode())
        self.assertFalse(self.result()["accepted"])

    def test_fixed_level_scope_cannot_be_disabled(self):
        self.configuration["options"]["fixed_levels"] = False
        self.assertFalse(self.result()["accepted"])

    def test_banked_wram_consistent_profile_is_accepted(self):
        self.enable_banked_wram()
        self.assertTrue(self.result()["accepted"])

    def test_flat_profile_rejects_banked_capability(self):
        self.enable_banked_wram()
        self.manifest["profile"] = "bounded-mbc3-flat-wram-div-event-tape"
        self.rewrite_manifest()
        self.assertFalse(self.result()["accepted"])

    def test_banked_profile_requires_explicit_manifest_capability(self):
        self.enable_banked_wram()
        del self.manifest["banked_wram"]
        self.rewrite_manifest()
        self.assertFalse(self.result()["accepted"])

    def test_banked_profile_requires_explicit_configured_capability(self):
        self.enable_banked_wram()
        for target in (self.configuration, self.report["configuration"], self.case_report["configuration"]):
            del target["options"]["banked_wram"]
        self.assertFalse(self.result()["accepted"])

    def test_banked_manifest_capability_must_be_boolean(self):
        self.enable_banked_wram()
        self.manifest["banked_wram"] = 1
        self.rewrite_manifest()
        self.assertFalse(self.result()["accepted"])

    def test_banked_configured_capability_must_be_boolean(self):
        self.enable_banked_wram()
        for target in (self.configuration, self.report["configuration"], self.case_report["configuration"]):
            target["options"]["banked_wram"] = 1
        self.assertFalse(self.result()["accepted"])

    def test_banked_case_must_compare_all_banks_and_svbk(self):
        self.enable_banked_wram()
        self.case_report["cases"][0]["full_state_components"] = ["cpu", "hram"]
        self.assertFalse(self.result()["accepted"])

    def test_default_source_inventory(self):
        actual = coverage.inventory(coverage.ROOT)
        groups = {}
        for rule in actual["rules"]:
            groups[rule["group"]] = groups.get(rule["group"], 0) + 1
        self.assertEqual(groups["move"], 255)
        self.assertEqual(groups["effect"], 152)
        self.assertEqual(groups["ability"], 160)
        self.assertEqual(groups["item"], 254)
        self.assertEqual(groups["trainer-item"], 14)
        self.assertEqual(groups["field"], 11)
        self.assertEqual(groups["controller"], 19)
        by_key = {rule["key"]: rule for rule in actual["rules"]}
        for name in ("GiveExperience", "LearnLevelMoves", "LearnMove"):
            self.assertFalse(by_key["controller:" + name]["required"])
            self.assertEqual(by_key["controller:" + name]["excluded_by_scope"], "user-fixed-level-no-experience")
        self.assertTrue(by_key["controller:FixPlayerEVsAndStats"]["required"])
        self.assertGreater(groups["player-bag-item"], 20)
        self.assertGreater(groups["player-bag-effect"], 5)
        self.assertFalse(coverage.evaluate(actual, {"inventory_sha256": actual["inventory_sha256"]}, self.root)["accepted"])


if __name__ == "__main__":
    unittest.main()
