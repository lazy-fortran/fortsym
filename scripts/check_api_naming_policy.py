#!/usr/bin/env python3
"""Validate complete coverage of the selected API naming policy."""

from __future__ import annotations

import argparse
import json
import tomllib
from pathlib import Path


def validate(policy: dict, audit: dict) -> str:
    assert policy["package"] == audit["package"] == "fortsym"
    assert policy["sympy_version"] == audit["sympy_version"] == "1.14.0"
    audit_ids = {item["id"] for item in audit["concepts"]}
    families = policy["family"]
    family_ids = [item["id"] for item in families]
    assert len(family_ids) == len(set(family_ids))
    covered = {concept for item in families for concept in item["audit_concepts"]}
    assert covered == audit_ids
    assert all(item["canonical_native"] for item in families)
    assert all(item["decision"] for item in families)
    native_names = [name for item in families for name in item["canonical_native"]]
    assert len(native_names) == len(set(native_names))
    return f"validated {len(families)} naming families covering {len(audit_ids)} audit concepts"


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("policy", type=Path)
    parser.add_argument("audit", type=Path)
    args = parser.parse_args()
    policy = tomllib.loads(args.policy.read_text(encoding="utf-8"))
    audit = json.loads(args.audit.read_text(encoding="utf-8"))
    print(validate(policy, audit))


if __name__ == "__main__":
    main()
