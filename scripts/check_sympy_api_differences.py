#!/usr/bin/env python3
"""Validate the semantic/implementation difference ledger coverage."""

from __future__ import annotations

import argparse
import json
import tomllib
from pathlib import Path


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("ledger", type=Path)
    parser.add_argument("classification", type=Path)
    args = parser.parse_args()

    ledger = tomllib.loads(args.ledger.read_text(encoding="utf-8"))
    classification = json.loads(args.classification.read_text(encoding="utf-8"))
    assert ledger["package"] == classification["package"] == "sympy"
    assert ledger["version"] == classification["version"] == "1.14.0"

    root_exports = classification["sympy"]["root_exports"]
    supported = {
        item["name"]
        for item in root_exports
        if "python-adapter" in item["classification"]
    }
    semantic = ledger["semantic"]
    implementation = ledger["implementation"]
    assert semantic and implementation
    ids = [item["id"] for item in semantic + implementation]
    assert len(ids) == len(set(ids))

    semantic_names = {
        name
        for item in semantic
        for name in item["affected"]
    }
    assert semantic_names == supported
    assert all(name in supported for name in semantic_names)
    assert all(item["parity"] in {"partial", "same", "refused"} for item in semantic)
    assert all(item["affected"] for item in semantic + implementation)
    assert all(item["sympy"] and item["fortsym"] and item["difference"]
               for item in semantic + implementation)
    print(
        f"validated {len(supported)} supported root names across "
        f"{len(semantic)} semantic and {len(implementation)} implementation entries"
    )


if __name__ == "__main__":
    main()
