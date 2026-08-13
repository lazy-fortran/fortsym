#!/usr/bin/env python3
"""Collect pinned literature and upstream reference pages into ``.provenance``.

The downloaded material is intentionally ignored.  The URL and content hash
in the generated manifest make a local research snapshot auditable without
putting third-party documents into the source distribution.
"""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from urllib.request import Request, urlopen


DEFAULT_SOURCES = {
    "sympy-solveset": (
        "https://raw.githubusercontent.com/sympy/sympy/"
        "fe935ceb303891d1f8bea4c03b19fd9ec9464b02/"
        "doc/src/modules/solvers/solveset.rst"
    ),
    "sympy-sets": (
        "https://raw.githubusercontent.com/sympy/sympy/"
        "fe935ceb303891d1f8bea4c03b19fd9ec9464b02/"
        "doc/src/modules/sets.rst"
    ),
}


def fetch(name: str, url: str, destination: Path) -> dict[str, object]:
    request = Request(url, headers={"User-Agent": "fortsym-provenance/1"})
    with urlopen(request, timeout=30) as response:
        content = response.read()
    target = destination / f"{name}.txt"
    target.write_bytes(content)
    return {
        "name": name,
        "url": url,
        "path": target.as_posix(),
        "bytes": len(content),
        "sha256": hashlib.sha256(content).hexdigest(),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--out",
        type=Path,
        default=Path(".provenance/literature"),
        help="ignored output directory",
    )
    parser.add_argument(
        "--url",
        action="append",
        metavar="NAME=URL",
        help="replace the pinned defaults with an explicitly named source",
    )
    args = parser.parse_args()
    sources = DEFAULT_SOURCES
    if args.url:
        sources = {}
        for item in args.url:
            name, separator, url = item.partition("=")
            if not separator or not name or not url:
                parser.error("--url must have the form NAME=URL")
            sources[name] = url

    args.out.mkdir(parents=True, exist_ok=True)
    records = [fetch(name, url, args.out) for name, url in sorted(sources.items())]
    manifest = args.out / "manifest.json"
    manifest.write_text(
        json.dumps({"sources": records}, indent=2) + "\n", encoding="utf-8"
    )
    print(manifest)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
