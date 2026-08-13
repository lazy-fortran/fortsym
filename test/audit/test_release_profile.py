import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

from scripts.check_release_profile import (  # noqa: E402
    ReleaseProfileError,
    validate_benchmark_report,
    validate_feature_matrix,
)


class ReleaseProfileCheckTest(unittest.TestCase):
    PROFILE = {"package": "fortsym", "sympy_version": "1.14.0"}

    @staticmethod
    def report(**overrides):
        report = {
            "schema_version": 1,
            "package": "fortsym.sympy",
            "sympy_version": "1.14.0",
            "correctness": [{"name": "identity", "correct": True}],
            "workloads": [{
                "operation": "expand",
                "scope": "warm_core",
                "fortsym": {"median_ns": 1},
                "sympy": {"median_ns": 2},
                "native_over_sympy": 0.5,
            }],
            "parity": {"enforced": True, "waivers": [], "violations": []},
        }
        report.update(overrides)
        return report

    def test_benchmark_check_requires_independent_correctness_evidence(self):
        self.assertEqual(
            validate_benchmark_report(self.report(), self.PROFILE, require_parity=True),
            1,
        )
        failed = self.report(correctness=[{"name": "identity", "correct": False}])
        with self.assertRaises(ReleaseProfileError):
            validate_benchmark_report(failed, self.PROFILE, require_parity=True)

    def test_strict_benchmark_check_rejects_unwaived_slowdown(self):
        failed = self.report(
            workloads=[{
                "operation": "expand",
                "scope": "warm_core",
                "fortsym": {"median_ns": 2},
                "sympy": {"median_ns": 1},
                "native_over_sympy": 2.0,
            }],
            parity={"enforced": True, "waivers": [],
                    "violations": ["expand:warm_core"]},
        )
        with self.assertRaises(ReleaseProfileError):
            validate_benchmark_report(failed, self.PROFILE, require_parity=True)

    def test_feature_matrix_has_to_name_its_profile_and_scope(self):
        text = (
            "<!-- release-profile: sympy-1.14.0 -->\n"
            "| Area | Native | SymEngine backend | Other backends | "
            "Required next fragment |\n"
            "SymPy FortFEM\n"
        )
        validate_feature_matrix(text, "sympy-1.14.0")
        with self.assertRaises(ReleaseProfileError):
            validate_feature_matrix(text.replace("1.14.0", "1.13.0"),
                                    "sympy-1.14.0")


if __name__ == "__main__":
    unittest.main()
