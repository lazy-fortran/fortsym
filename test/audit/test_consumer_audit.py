#!/usr/bin/env python3
"""Behavioral tests for the privacy-safe consumer audit."""

from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest


SCRIPT = Path(__file__).parents[2] / "scripts" / "audit_consumer_requirements.py"
SPEC = importlib.util.spec_from_file_location("audit_consumer_requirements", SCRIPT)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("could not load consumer audit")
AUDIT = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = AUDIT
SPEC.loader.exec_module(AUDIT)


class ClassifierTests(unittest.TestCase):
    def check(self, source: str, path: str, expected: set[str]) -> None:
        actual, truncated = AUDIT.classify(source.encode(), path)
        self.assertEqual(actual, expected)
        self.assertFalse(truncated)

    def test_minimal_package_needs_no_banner(self) -> None:
        self.check(
            "D[x^2, x]",
            "minimal.m",
            {"differentiate", "safe_log_power_radical"},
        )

    def test_comments_and_strings_are_not_wolfram_calls(self) -> None:
        self.check(
            '(* D[x,x] (* FullSimplify[x] *) *)\n'
            '"Integrate[x,x]"\n'
            "Solve[x == 1, x]",
            "plain.wl",
            {"solve_reduce"},
        )

    def test_notebook_row_boxes_are_classified(self) -> None:
        self.check(
            'RowBox[{"FullSimplify","[","x","]"}]',
            "box.nb",
            {"simplify"},
        )

    def test_notebook_text_cell_is_not_executable_code(self) -> None:
        self.check(
            'Cell["An example is Integrate[x,x].", "Text"]',
            "prose.nb",
            set(),
        )

    def test_case_insensitive_bare_words_are_not_calls(self) -> None:
        self.check(
            "d = sum + gamma + factor + root",
            "not_wolfram.m",
            set(),
        )

    def test_host_wrapper_requires_an_explicit_marker(self) -> None:
        self.check(
            'run wolframscript -code "Integrate[x,x]"',
            "wrapper.sh",
            {"integrate"},
        )
        self.check(
            'run other-program -code "Integrate[x,x]"',
            "wrapper.sh",
            set(),
        )

    def test_commented_host_wrapper_is_not_counted(self) -> None:
        self.check(
            '# wolframscript -code "Integrate[x,x]"\n'
            'echo "not executed"',
            "wrapper.sh",
            set(),
        )

    def test_documentation_is_not_a_wrapper_candidate(self) -> None:
        self.assertFalse(AUDIT.is_wolfram_or_host_path("README.md"))
        self.assertTrue(AUDIT.is_wolfram_or_host_path("run_wolfram.sh"))
        self.assertTrue(AUDIT.is_wolfram_or_host_path("derivation.wl"))

    def test_exact_and_numeric_operations_are_separate(self) -> None:
        self.check(
            "Integrate[x,x]; NIntegrate[x,{x,0,1}]; "
            "Solve[x==1,x]; NSolve[x==1,x]; "
            "DSolve[y'[x]==y[x],y,x]; NDSolve[{y'[x]==y[x]},y,x]",
            "numeric.wl",
            {
                "differentiate",
                "differential_equations",
                "integrate",
                "numeric_differential_equations",
                "numeric_integrate",
                "numeric_solve",
                "solve_reduce",
            },
        )

    def test_precision_and_guarded_power_operations_are_visible(self) -> None:
        self.check(
            "N[x,80]; SetPrecision[x,100]; WorkingPrecision->120; "
            "PowerExpand[Log[x^2]]; Sqrt[x]",
            "domains.wl",
            {
                "arbitrary_precision",
                "numeric_evaluation",
                "safe_log_power_radical",
            },
        )

    def test_infix_and_global_wolfram_syntax_is_visible(self) -> None:
        self.check(
            "$Assumptions = x > 0; y = 1.25`80; "
            "z = matrix . vector; derivative = f'[x]; x^2 /; x > 0",
            "syntax.wl",
            {
                "arbitrary_precision",
                "assumptions_inequalities",
                "differentiate",
                "matrices_tensors",
                "safe_log_power_radical",
            },
        )


class PrivacySchemaTests(unittest.TestCase):
    def test_empty_output_contains_no_machine_paths_or_unapproved_names(self) -> None:
        rendered = json.dumps(AUDIT.new_aggregate(), sort_keys=True)
        self.assertNotIn("/home/", rendered)
        self.assertNotIn("github.com/", rendered)
        self.assertNotIn("gitlab.", rendered)
        self.assertIn("FortNum", rendered)
        self.assertIn("MHD1D", rendered)

    def test_bounded_command_reports_truncation(self) -> None:
        data, truncated = AUDIT.run_bounded(
            [sys.executable, "-c", "import sys; sys.stdout.write('abcdefgh')"],
            byte_limit=5,
        )
        self.assertEqual(data, b"abcdef")
        self.assertTrue(truncated)

    def test_partial_aggregates_merge_without_double_counting(self) -> None:
        local = AUDIT.new_aggregate()
        remote = AUDIT.new_aggregate()
        local["source_classes"]["local_code"][
            "repository_instances_discovered"
        ] = 2
        remote["source_classes"]["gitlab"][
            "repository_instances_discovered"
        ] = 3
        local["named_consumers"]["FortNum"]["observed_operations"] = [
            "differentiate"
        ]
        remote["named_consumers"]["FortNum"]["observed_operations"] = [
            "simplify"
        ]
        with tempfile.TemporaryDirectory() as directory:
            paths = []
            for index, aggregate in enumerate((local, remote)):
                path = Path(directory) / f"{index}.json"
                path.write_text(json.dumps(aggregate), encoding="utf-8")
                paths.append(path)
            merged = AUDIT.merge_aggregates(paths)
        self.assertEqual(
            merged["source_classes"]["local_code"][
                "repository_instances_discovered"
            ],
            2,
        )
        self.assertEqual(
            merged["source_classes"]["gitlab"][
                "repository_instances_discovered"
            ],
            3,
        )
        self.assertEqual(
            merged["named_consumers"]["FortNum"]["observed_operations"],
            ["differentiate", "simplify"],
        )

    def test_skipped_selection_cannot_be_complete(self) -> None:
        source = AUDIT.new_source_class()
        source.update(
            {
                "source_requested": True,
                "listing_completed": True,
                "selection_requested": True,
                "selection_skipped": True,
                "repository_instances_discovered": 2,
                "repository_instances_attempted": 2,
                "repository_instances_completed": 2,
            }
        )
        AUDIT.finalize_source(source)
        self.assertFalse(source["selection_completed"])
        self.assertFalse(source["overall_complete"])


if __name__ == "__main__":
    unittest.main()
