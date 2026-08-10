import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

from scripts.check_provenance import missing_provenance  # noqa: E402


class ProvenanceCheckTest(unittest.TestCase):
    def test_new_compatibility_source_requires_an_exact_entry(self):
        self.assertEqual(
            missing_provenance(["src/algebra/new_rule.f90"], ""),
            ["src/algebra/new_rule.f90"],
        )

    def test_exact_entry_satisfies_the_requirement(self):
        provenance = "| `src/algebra/new_rule.f90` | independent implementation |"
        self.assertEqual(
            missing_provenance(["src/algebra/new_rule.f90"], provenance),
            [],
        )

    def test_non_implementation_files_are_not_blocked(self):
        self.assertEqual(
            missing_provenance(
                ["test/algebra/test_new_rule.f90", "doc/new-rule.md", "src/algebra/new_rule.txt"],
                "",
            ),
            [],
        )

    def test_diff_paths_are_normalised(self):
        provenance = "The implementation is recorded as src/algebra/new_rule.f90."
        self.assertEqual(
            missing_provenance(["./src/algebra/new_rule.f90"], provenance),
            [],
        )


if __name__ == "__main__":
    unittest.main()
