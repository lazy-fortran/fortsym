#!/usr/bin/env python3
"""Report the exact SymPy names in one pinned fortsym compatibility profile."""

from __future__ import annotations

import argparse
import json
import sys
import tomllib
from pathlib import Path
from typing import Any

_ROOT = Path(__file__).resolve().parents[1]
if str(_ROOT) not in sys.path:
    sys.path.insert(0, str(_ROOT))

from scripts.check_release_profile import (  # noqa: E402
    ReleaseProfileError,
    _load_json,
    _path,
    validate_profile,
)


def _supported_names(classification: dict[str, Any], section: str, key: str) -> list[str]:
    return sorted(
        record[key]
        for record in classification["sympy"][section]
        if "python-adapter" in record.get("classification", [])
    )


def build_report(
    profile_path: Path,
    expected_sympy_version: str | None = None,
) -> dict[str, Any]:
    """Validate and materialise the exact names for *profile_path*."""

    profile = tomllib.loads(profile_path.read_text(encoding="utf-8"))
    if expected_sympy_version is not None and profile.get("sympy_version") != expected_sympy_version:
        raise ReleaseProfileError(
            "requested SymPy baseline does not match the release profile: "
            f"requested {expected_sympy_version}, profile has {profile.get('sympy_version')}"
        )

    # This validates every cross-artifact version/path edge before any names
    # are reported.  A report can therefore never silently combine baselines.
    validate_profile(profile_path, metadata_only=True)
    root = profile_path.resolve().parent.parent
    classification = _load_json(_path(root, profile, "classification"))

    root_supported = _supported_names(classification, "root_exports", "name")
    classes = _supported_names(classification, "classes", "id")
    methods = _supported_names(classification, "methods", "id")
    root_refused = sorted(
        record["name"]
        for record in classification["sympy"]["root_exports"]
        if "python-adapter" not in record.get("classification", [])
    )
    adapter_only = sorted(
        set(classification["adapter_profile"]["supported"]) - set(root_supported)
    )
    return {
        "schema_version": 1,
        "profile": profile["profile"],
        "package": profile["package"],
        "sympy_package": profile["sympy_package"],
        "sympy_version": profile["sympy_version"],
        "classification": profile["classification"],
        "supported": {
            "root": root_supported,
            "classes": classes,
            "methods": methods,
        },
        "adapter_only": adapter_only,
        "refused": {"root": root_refused},
        "counts": {
            "supported_root": len(root_supported),
            "supported_classes": len(classes),
            "supported_methods": len(methods),
            "adapter_only": len(adapter_only),
            "refused_root": len(root_refused),
        },
    }


def render_text(report: dict[str, Any], include_refused: bool = False) -> str:
    """Render a stable human-readable report without truncating support names."""

    lines = [
        f"FortSym compatibility profile: {report['profile']}",
        f"SymPy baseline: {report['sympy_version']}",
        f"Classification: {report['classification']}",
        "",
        f"Supported SymPy root names ({report['counts']['supported_root']}):",
    ]
    lines.extend(f"  {name}" for name in report["supported"]["root"])
    lines.extend([
        "",
        f"Supported SymPy class paths ({report['counts']['supported_classes']}):",
    ])
    lines.extend(f"  {name}" for name in report["supported"]["classes"])
    lines.extend([
        "",
        f"Supported SymPy method paths ({report['counts']['supported_methods']}):",
    ])
    lines.extend(f"  {name}" for name in report["supported"]["methods"])
    lines.extend([
        "",
        f"FortSym adapter-only names ({report['counts']['adapter_only']}); "
        "not SymPy root names:",
    ])
    lines.extend(f"  {name}" for name in report["adapter_only"])
    lines.append(
        f"\nRefused/inventoried SymPy root names: {report['counts']['refused_root']}"
    )
    if include_refused:
        lines.extend(f"  {name}" for name in report["refused"]["root"])
    return "\n".join(lines) + "\n"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("profile", type=Path)
    parser.add_argument("--sympy-version", help="require this exact pinned baseline")
    parser.add_argument("--format", choices=("text", "json"), default="text")
    parser.add_argument(
        "--include-refused",
        action="store_true",
        help="include all refused/inventoried root names in text output",
    )
    args = parser.parse_args(argv)
    try:
        report = build_report(args.profile, args.sympy_version)
    except (OSError, KeyError, TypeError, ValueError, json.JSONDecodeError) as error:
        raise SystemExit(f"compatibility profile refused: {error}") from error
    if args.format == "json":
        print(json.dumps(report, indent=2, sort_keys=True))
    else:
        print(render_text(report, include_refused=args.include_refused), end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
