#!/usr/bin/env python3
"""Create a versioned manifest or diff for two SymPy API inventories."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any, Callable


def compact_record(record: dict[str, Any], fields: tuple[str, ...]) -> dict[str, Any]:
    return {field: record[field] for field in fields if field in record}


def unique_records(
    records: list[dict[str, Any]],
    key: Callable[[dict[str, Any]], str],
) -> list[dict[str, Any]]:
    unique: dict[str, dict[str, Any]] = {}
    for record in records:
        name = key(record)
        if name in unique and unique[name] != record:
            raise ValueError(f"conflicting duplicate API manifest key: {name}")
        unique[name] = record
    return [unique[name] for name in sorted(unique)]


def manifest(inventory: dict[str, Any]) -> dict[str, Any]:
    modules = [
        {"name": module["name"]}
        for module in inventory["modules"]
    ]
    root_exports = [
        compact_record(record, ("name", "kind", "module", "qualname", "signature"))
        for record in inventory["root_exports"]
    ]
    module_exports = []
    for module in inventory["modules"]:
        for record in module["exports"]:
            if "kind" not in record:
                module_exports.append({
                    "export_module": module["name"],
                    "name": record["name"],
                    "access_error": record.get("access_error"),
                })
                continue
            module_exports.append({
                "export_module": module["name"],
                **compact_record(record, ("name", "kind", "module", "qualname", "signature")),
            })
    module_exports = unique_records(
        module_exports,
        lambda item: f"{item['export_module']}.{item['name']}",
    )
    classes = []
    for record in inventory["classes"]:
        classes.append({
            "id": record["id"],
            "kind": record["kind"],
            "bases": record["bases"],
            "methods": [
                compact_record(method, ("name", "signature", "options", "return_annotation"))
                for method in record["methods"]
            ],
        })
    return {
        "schema_version": 1,
        "package": inventory["package"],
        "version": inventory["version"],
        "modules": modules,
        "root_exports": root_exports,
        "module_exports": module_exports,
        "classes": classes,
    }


def load_manifest(path: Path) -> dict[str, Any]:
    data = json.loads(path.read_text(encoding="utf-8"))
    if data.get("modules") and isinstance(data["modules"][0], str):
        raise ValueError(f"{path} is not a recognized inventory or manifest")
    if data.get("modules") and "exports" in data["modules"][0]:
        return manifest(data)
    required = {"schema_version", "package", "version", "modules", "root_exports", "module_exports", "classes"}
    if not required <= data.keys():
        raise ValueError(f"{path} is missing manifest fields")
    return data


def keyed(items: list[dict[str, Any]], key: Callable[[dict[str, Any]], str]) -> dict[str, dict[str, Any]]:
    result = {key(item): item for item in items}
    if len(result) != len(items):
        raise ValueError("duplicate API manifest key")
    return result


def compare(
    before: list[dict[str, Any]],
    after: list[dict[str, Any]],
    key: Callable[[dict[str, Any]], str],
) -> dict[str, Any]:
    old = keyed(before, key)
    new = keyed(after, key)
    added = [new[name] for name in sorted(new.keys() - old.keys())]
    removed = [old[name] for name in sorted(old.keys() - new.keys())]
    changed = [
        {"id": name, "before": old[name], "after": new[name]}
        for name in sorted(old.keys() & new.keys())
        if old[name] != new[name]
    ]
    return {
        "added": added,
        "removed": removed,
        "changed": changed,
        "counts": {
            "added": len(added),
            "removed": len(removed),
            "changed": len(changed),
        },
    }


def diff(before: dict[str, Any], after: dict[str, Any], before_path: Path, after_path: Path) -> dict[str, Any]:
    changes = {
        "modules": compare(before["modules"], after["modules"], lambda item: item["name"]),
        "root_exports": compare(before["root_exports"], after["root_exports"], lambda item: item["name"]),
        "module_exports": compare(
            before["module_exports"],
            after["module_exports"],
            lambda item: f"{item['export_module']}.{item['name']}",
        ),
        "classes": compare(before["classes"], after["classes"], lambda item: item["id"]),
    }
    changed = any(
        group["counts"][field]
        for group in changes.values()
        for field in ("added", "removed", "changed")
    )
    return {
        "schema_version": 1,
        "package": after["package"],
        "base_version": before["version"],
        "candidate_version": after["version"],
        "baseline": before_path.as_posix(),
        "candidate": after_path.as_posix(),
        "clean": not changed,
        "changes": changes,
    }


def write_json(path: Path, value: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    manifest_parser = subparsers.add_parser("manifest")
    manifest_parser.add_argument("inventory", type=Path)
    manifest_parser.add_argument("output", type=Path)

    diff_parser = subparsers.add_parser("diff")
    diff_parser.add_argument("baseline", type=Path)
    diff_parser.add_argument("candidate", type=Path)
    diff_parser.add_argument("output", type=Path)
    diff_parser.add_argument("--fail-on-diff", action="store_true")

    args = parser.parse_args()
    if args.command == "manifest":
        inventory = json.loads(args.inventory.read_text(encoding="utf-8"))
        write_json(args.output, manifest(inventory))
        return

    before = load_manifest(args.baseline)
    after = load_manifest(args.candidate)
    if before["package"] != after["package"]:
        raise SystemExit("cannot compare different packages")
    report = diff(before, after, args.baseline, args.candidate)
    write_json(args.output, report)
    if args.fail_on_diff and not report["clean"]:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
