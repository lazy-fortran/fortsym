import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

from scripts.check_release_profile import ReleaseProfileError  # noqa: E402
from scripts.compatibility_profile import build_report, render_text  # noqa: E402


class CompatibilityProfileTest(unittest.TestCase):
    PROFILE = ROOT / "doc/release-profile.toml"

    def test_report_has_exact_pinned_supported_names(self):
        report = build_report(self.PROFILE, "1.14.0")
        self.assertEqual(report["profile"], "sympy-1.14.0")
        self.assertEqual(report["counts"]["supported_root"], 96)
        self.assertIn("solveset", report["supported"]["root"])
        self.assertIn("Matrix", report["supported"]["root"])
        self.assertIn("sympy.core.expr.Expr.diff", report["supported"]["methods"])
        self.assertEqual(
            report["supported"]["root"],
            sorted(set(report["supported"]["root"])),
        )

    def test_mixed_or_wrong_baseline_is_refused(self):
        with self.assertRaises(ReleaseProfileError):
            build_report(self.PROFILE, "1.13.0")

    def test_text_report_does_not_hide_supported_names(self):
        report = build_report(self.PROFILE)
        text = render_text(report)
        self.assertIn("Supported SymPy root names (96):", text)
        root_section = text.split("Supported SymPy class paths", 1)[0]
        self.assertIn("  solveset\n", text)
        self.assertIn("  exp\n", root_section)
        self.assertIn("  Arena\n", text.split("FortSym adapter-only names", 1)[1])
        self.assertIn("Refused/inventoried SymPy root names: 825", text)


if __name__ == "__main__":
    unittest.main()
