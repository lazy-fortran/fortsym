#!/usr/bin/env python3
"""Validate one internally consistent SymPy compatibility release profile."""

from __future__ import annotations

import argparse
import json
import math
import sys
import tomllib
from pathlib import Path
from typing import Any

# Keep both `python scripts/check_release_profile.py ...` and module execution
# usable from a checkout.  The repository root is the import boundary for the
# other small checkers used by this aggregate gate.
_ROOT = Path(__file__).resolve().parents[1]
if str(_ROOT) not in sys.path:
    sys.path.insert(0, str(_ROOT))

from scripts.check_api_naming_policy import validate as validate_naming_policy
from scripts.check_sympy_api_differences import validate as validate_difference_ledger


class ReleaseProfileError(ValueError):
    """Raised when one release-profile artifact is missing or inconsistent."""


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise ReleaseProfileError(message)


def _load_json(path: Path) -> dict[str, Any]:
    _require(path.is_file(), f"missing JSON artifact: {path}")
    value = json.loads(path.read_text(encoding="utf-8"))
    _require(isinstance(value, dict), f"JSON artifact is not an object: {path}")
    return value


def _path(root: Path, profile: dict[str, Any], key: str) -> Path:
    value = profile.get(key)
    _require(isinstance(value, str) and value, f"profile is missing path: {key}")
    relative = Path(value)
    _require(not relative.is_absolute() and ".." not in relative.parts,
             f"profile path must stay below the repository: {key}")
    return root / relative


def _validate_inventory(inventory: dict[str, Any], profile: dict[str, Any]) -> None:
    _require(inventory.get("schema_version") == 1,
             "SymPy inventory schema is not supported")
    _require(inventory.get("package") == profile["sympy_package"],
             "SymPy inventory package does not match the release profile")
    _require(inventory.get("version") == profile["sympy_version"],
             "SymPy inventory version does not match the release profile")
    _require(
        inventory.get("import_failure_count") == len(inventory.get("import_failures", [])),
        "SymPy inventory import-failure count is stale",
    )
    _require(inventory.get("module_count") == len(inventory.get("modules", [])),
             "SymPy inventory module count is stale")
    _require(inventory.get("root_export_count") == len(inventory.get("root_exports", [])),
             "SymPy inventory root-export count is stale")


def _validate_classification(
    classification: dict[str, Any],
    profile: dict[str, Any],
) -> None:
    _require(classification.get("schema_version") == 1,
             "SymPy classification schema is not supported")
    _require(classification.get("package") == profile["sympy_package"],
             "SymPy classification package does not match the release profile")
    _require(classification.get("version") == profile["sympy_version"],
             "SymPy classification version does not match the release profile")
    _require(classification.get("source_inventory") == profile["inventory"],
             "SymPy classification points at a different inventory")
    summary = classification.get("summary", {}).get("sympy", {})
    for key, collection in (
        ("modules", classification.get("sympy", {}).get("modules", [])),
        ("module_exports", classification.get("sympy", {}).get("module_exports", [])),
        ("root_exports", classification.get("sympy", {}).get("root_exports", [])),
        ("classes", classification.get("sympy", {}).get("classes", [])),
    ):
        _require(summary.get(key) == len(collection),
                 f"SymPy classification {key} count is stale")
    _require(
        classification.get("summary", {}).get("fortran_public_exports") ==
        len(classification.get("fortran", {}).get("public_exports", [])),
        "Fortran classification export count is stale",
    )


def _validate_naming(
    naming_policy: dict[str, Any],
    naming_audit: dict[str, Any],
    profile: dict[str, Any],
) -> None:
    _require(naming_policy.get("package") == profile["package"],
             "naming policy package does not match the release profile")
    _require(naming_policy.get("sympy_version") == profile["sympy_version"],
             "naming policy version does not match the release profile")
    _require(naming_policy.get("source_audit") == profile["naming_audit"],
             "naming policy points at a different audit")
    _require(naming_audit.get("package") == profile["package"],
             "naming audit package does not match the release profile")
    _require(naming_audit.get("sympy_version") == profile["sympy_version"],
             "naming audit version does not match the release profile")
    _require(
        naming_audit.get("sources", {}).get("classification") ==
        profile["classification"],
        "naming audit points at a different classification",
    )
    try:
        validate_naming_policy(naming_policy, naming_audit)
    except (AssertionError, ValueError) as error:
        raise ReleaseProfileError(f"naming policy check failed: {error}") from error


def _validate_difference_artifacts(
    ledger: dict[str, Any],
    api_diff: dict[str, Any],
    classification: dict[str, Any],
    profile: dict[str, Any],
) -> None:
    _require(ledger.get("package") == profile["sympy_package"],
             "difference ledger package does not match the release profile")
    _require(ledger.get("version") == profile["sympy_version"],
             "difference ledger version does not match the release profile")
    _require(ledger.get("source_classification") == profile["classification"],
             "difference ledger points at a different classification")
    try:
        validate_difference_ledger(ledger, classification)
    except (AssertionError, ValueError) as error:
        raise ReleaseProfileError(f"difference-ledger check failed: {error}") from error

    _require(api_diff.get("schema_version") == 1,
             "API diff schema is not supported")
    _require(api_diff.get("package") == profile["sympy_package"],
             "API diff package does not match the release profile")
    _require(api_diff.get("base_version") == profile["sympy_version"] and
             api_diff.get("candidate_version") == profile["sympy_version"],
             "API diff versions do not match the release profile")
    _require(api_diff.get("baseline") == profile["api_baseline"],
             "API diff points at a different baseline")
    _require(api_diff.get("candidate") == profile["inventory"],
             "API diff points at a different candidate inventory")
    _require(api_diff.get("clean") is True,
             "API diff reports changes against the pinned inventory baseline")


def validate_feature_matrix(text: str, profile_name: str) -> None:
    """Validate the machine-readable anchors in the human feature matrix."""

    _require(f"<!-- release-profile: {profile_name} -->" in text,
             "feature matrix has no matching release-profile marker")
    _require(
        "| Area | Native | SymEngine backend | Other backends | Required next fragment |" in text,
        "feature matrix has no capability table",
    )
    _require("SymPy" in text and "FortFEM" in text,
             "feature matrix is missing its SymPy/FortFEM scope anchors")


def validate_benchmark_report(
    report: dict[str, Any],
    profile: dict[str, Any],
    require_parity: bool = False,
) -> int:
    """Validate benchmark correctness and, optionally, strict parity."""

    _require(report.get("schema_version") == 1,
             "benchmark report schema is not supported")
    _require(report.get("package") == f"{profile['package']}.sympy",
             "benchmark report package does not match the release profile")
    _require(report.get("sympy_version") == profile["sympy_version"],
             "benchmark report version does not match the release profile")
    correctness = report.get("correctness")
    _require(isinstance(correctness, list) and correctness,
             "benchmark report has no correctness cases")
    _require(all(isinstance(case, dict) and case.get("correct") is True
                 for case in correctness),
             "benchmark report contains a failed correctness case")

    workloads = report.get("workloads")
    _require(isinstance(workloads, list) and workloads,
             "benchmark report has no workload rows")
    identifiers: set[str] = set()
    for row in workloads:
        _require(isinstance(row, dict), "benchmark workload is not an object")
        identifier = f"{row.get('operation')}:{row.get('scope')}"
        _require(identifier not in identifiers,
                 f"benchmark report repeats workload {identifier}")
        identifiers.add(identifier)
        _require(isinstance(row.get("fortsym"), dict) and
                 isinstance(row.get("sympy"), dict),
                 f"benchmark workload is missing timings: {identifier}")
        ratio = row.get("native_over_sympy")
        _require(isinstance(ratio, (int, float)) and math.isfinite(ratio) and ratio >= 0,
                 f"benchmark workload has an invalid ratio: {identifier}")

    parity = report.get("parity")
    _require(isinstance(parity, dict), "benchmark report has no parity section")
    waivers = parity.get("waivers", [])
    violations = parity.get("violations", [])
    _require(isinstance(waivers, list) and set(waivers) <= identifiers,
             "benchmark report contains an unknown parity waiver")
    _require(isinstance(violations, list) and set(violations) <= identifiers,
             "benchmark report contains an unknown parity violation")
    if require_parity:
        _require(parity.get("enforced") is True,
                 "strict release gate needs an enforced benchmark report")
        _require(not violations,
                 "strict release gate has unwaived benchmark violations")
    return len(workloads)


def validate_profile(
    profile_path: Path,
    benchmark_report: Path | None = None,
    metadata_only: bool = False,
    require_parity: bool = False,
) -> int:
    """Validate the profile and return the number of checked benchmark rows."""

    profile = tomllib.loads(profile_path.read_text(encoding="utf-8"))
    _require(profile.get("schema_version") == 1,
             "release profile schema is not supported")
    _require(profile.get("profile") == f"sympy-{profile.get('sympy_version')}",
             "release profile name and SymPy version disagree")
    _require(profile.get("package") == "fortsym" and
             profile.get("sympy_package") == "sympy",
             "release profile package names are invalid")
    root = profile_path.resolve().parent.parent

    inventory = _load_json(_path(root, profile, "inventory"))
    classification = _load_json(_path(root, profile, "classification"))
    naming_audit = _load_json(_path(root, profile, "naming_audit"))
    api_diff = _load_json(_path(root, profile, "api_diff"))
    naming_policy_path = _path(root, profile, "naming_policy")
    ledger_path = _path(root, profile, "difference_ledger")
    feature_matrix_path = _path(root, profile, "feature_matrix")

    _validate_inventory(inventory, profile)
    _validate_classification(classification, profile)
    naming_policy = tomllib.loads(naming_policy_path.read_text(encoding="utf-8"))
    ledger = tomllib.loads(ledger_path.read_text(encoding="utf-8"))
    _validate_naming(naming_policy, naming_audit, profile)
    _validate_difference_artifacts(ledger, api_diff, classification, profile)
    validate_feature_matrix(feature_matrix_path.read_text(encoding="utf-8"),
                            profile["profile"])

    if metadata_only:
        return 0
    report_path = benchmark_report or _path(root, profile, "benchmark_report")
    report = _load_json(report_path)
    return validate_benchmark_report(report, profile, require_parity=require_parity)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("profile", type=Path)
    parser.add_argument("--benchmark-report", type=Path)
    parser.add_argument("--metadata-only", action="store_true")
    parser.add_argument("--require-parity", action="store_true")
    args = parser.parse_args(argv)
    try:
        rows = validate_profile(
            args.profile,
            benchmark_report=args.benchmark_report,
            metadata_only=args.metadata_only,
            require_parity=args.require_parity,
        )
    except (OSError, KeyError, TypeError, ValueError, json.JSONDecodeError) as error:
        raise SystemExit(f"release profile check failed: {error}") from error
    if args.metadata_only:
        print(f"validated release profile {args.profile}")
    else:
        mode = "strict" if args.require_parity else "diagnostic"
        print(f"validated {mode} release profile {args.profile} ({rows} benchmark rows)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
