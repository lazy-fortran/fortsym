#!/usr/bin/env python3
"""Check that newly added compatibility sources have provenance entries."""

from __future__ import annotations

import argparse
from pathlib import Path, PurePosixPath
from typing import Iterable


# Compatibility implementation lives in these roots.  Documentation, tests,
# and build machinery have their own provenance and review rules.
COMPATIBILITY_ROOTS = ("src/", "app/", "python/", "fortsym/")
SOURCE_SUFFIXES = {
    ".c",
    ".cc",
    ".cpp",
    ".cxx",
    ".f",
    ".f03",
    ".f08",
    ".f90",
    ".f95",
    ".F",
    ".F03",
    ".F08",
    ".F90",
    ".F95",
    ".h",
    ".hh",
    ".hpp",
    ".hxx",
    ".js",
    ".py",
}


def _normalise(path: str) -> str:
    """Return a repository-relative POSIX path for a diff-file entry."""

    normalised = PurePosixPath(path.strip()).as_posix()
    if normalised.startswith("./"):
        normalised = normalised[2:]
    return normalised


def is_compatibility_source(path: str) -> bool:
    """Whether *path* is an implementation source covered by issue #20."""

    path = _normalise(path)
    return path.startswith(COMPATIBILITY_ROOTS) and Path(path).suffix in SOURCE_SUFFIXES


def _has_entry(path: str, provenance: str) -> bool:
    # A path entry is deliberately required literally.  This makes the check
    # reviewable and prevents a generic statement from satisfying the gate.
    return path in provenance


def missing_provenance(changed_files: Iterable[str], provenance: str) -> list[str]:
    """Return added compatibility sources absent from the provenance record."""

    required = {
        _normalise(path)
        for path in changed_files
        if is_compatibility_source(path)
    }
    return sorted(path for path in required if not _has_entry(path, provenance))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--changed-files", type=Path, required=True)
    parser.add_argument("--provenance", type=Path, required=True)
    args = parser.parse_args(argv)

    changed_files = args.changed_files.read_text(encoding="utf-8").splitlines()
    provenance = args.provenance.read_text(encoding="utf-8")
    missing = missing_provenance(changed_files, provenance)
    if missing:
        print("Missing provenance entries for newly added compatibility sources:")
        for path in missing:
            print(f"  {path}")
        return 1

    print("Provenance check passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
