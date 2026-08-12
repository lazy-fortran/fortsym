#!/usr/bin/env python3
"""Classify the inventoried SymPy surface and the native public layers.

The report deliberately stores layers rather than forcing one label onto every
name.  For example, a supported ``fortsym.sympy`` name is both a Python adapter
and a delegated native operation.  This keeps the ownership boundary visible
and makes inconsistent aliases harder to introduce.
"""

from __future__ import annotations

import argparse
import ast
import collections
import json
import re
from pathlib import Path
from typing import Any, Iterable


CLASSIFICATIONS = {
    "native": "semantics are owned by the in-process Fortran implementation",
    "facade": "a user-facing convenience surface forwards to an owner",
    "python-adapter": "declared by the fortsym.sympy compatibility layer",
    "external": "provided through an external engine or library boundary",
    "delegated": "forwards to another classified owner instead of owning semantics",
    "refused": "not claimed by the supported compatibility surface",
}


def string_list_assignment(tree: ast.AST, name: str) -> set[str]:
    values: set[str] = set()
    for node in ast.walk(tree):
        if not isinstance(node, (ast.Assign, ast.AnnAssign)):
            continue
        targets = node.targets if isinstance(node, ast.Assign) else [node.target]
        if not any(isinstance(target, ast.Name) and target.id == name for target in targets):
            continue
        value = node.value
        if isinstance(value, (ast.List, ast.Tuple, ast.Set)):
            values.update(
                item.value for item in value.elts
                if isinstance(item, ast.Constant) and isinstance(item.value, str)
            )
    return values


def explicit_refusals(tree: ast.AST) -> set[str]:
    names: set[str] = set()
    for node in ast.walk(tree):
        if not isinstance(node, (ast.Assign, ast.AnnAssign)):
            continue
        targets = node.targets if isinstance(node, ast.Assign) else [node.target]
        if not isinstance(node.value, ast.Call):
            continue
        function = node.value.func
        if not isinstance(function, ast.Name) or function.id != "_unsupported":
            continue
        if not node.value.args or not isinstance(node.value.args[0], ast.Constant):
            continue
        if not isinstance(node.value.args[0].value, str):
            continue
        if any(isinstance(target, ast.Name) for target in targets):
            names.add(node.value.args[0].value)
    return names


def adapter_method_names(tree: ast.AST) -> set[str]:
    names: set[str] = set()
    for node in ast.walk(tree):
        if not isinstance(node, ast.ClassDef):
            continue
        names.update(
            child.name for child in node.body
            if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef))
            and not child.name.startswith("_")
        )
    return names


def adapter_profile(path: Path, native_path: Path | None = None) -> dict[str, Any]:
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    declared = string_list_assignment(tree, "__all__")
    refused = explicit_refusals(tree)
    methods = adapter_method_names(tree)
    if native_path is not None:
        native_tree = ast.parse(
            native_path.read_text(encoding="utf-8"), filename=str(native_path)
        )
        for node in ast.walk(native_tree):
            if isinstance(node, ast.ClassDef) and node.name == "Expr":
                methods.update(
                    child.name for child in node.body
                    if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef))
                    and not child.name.startswith("_")
                )
    return {
        "declared": declared,
        "supported": declared - refused,
        "refused": refused,
        "methods": methods,
    }


def layers(
    values: Iterable[str],
    owner: str,
    reason: str,
) -> dict[str, Any]:
    return {
        "classification": list(values),
        "owner": owner,
        "reason": reason,
    }


def supported_layers(name: str, profile: dict[str, Any]) -> dict[str, Any]:
    if name in profile["supported"]:
        return layers(
            ("python-adapter", "delegated", "native"),
            "python/fortsym/sympy/__init__.py -> python/fortsym/__init__.py -> Fortran C ABI",
            "declared SymPy subset delegated to the native expression implementation",
        )
    if name in profile["refused"]:
        return layers(
            ("refused",),
            "python/fortsym/sympy/__init__.py",
            "explicitly refused by the compatibility layer until its semantics are covered",
        )
    return layers(
        ("refused",),
        "ROADMAP.md",
        "outside the declared compatibility subset; no implementation claim is made",
    )


def classify_module_export(
    record: dict[str, Any],
    root_by_name: dict[str, dict[str, Any]],
    profile: dict[str, Any],
) -> dict[str, Any]:
    root = root_by_name.get(record["name"])
    if (
        root is not None
        and record["name"] in profile["supported"]
        and record.get("kind") == root.get("kind")
        and record.get("module") == root.get("module")
        and record.get("qualname") == root.get("qualname")
    ):
        return supported_layers(record["name"], profile)
    return layers(
        ("refused",),
        "ROADMAP.md",
        "the individual module export is not a declared supported adapter object",
    )


def class_layers(
    class_id: str,
    supported_class_ids: set[str],
    profile: dict[str, Any],
) -> dict[str, Any]:
    if class_id in supported_class_ids:
        return supported_layers(class_id.rsplit(".", 1)[-1], profile)
    return layers(
        ("refused",),
        "ROADMAP.md",
        "class is inventoried for parity but is outside the declared adapter subset",
    )


def parse_public_declarations(path: Path) -> list[tuple[str, str]]:
    lines = path.read_text(encoding="utf-8").splitlines()
    module_match = next(
        (
            re.match(r"\s*module\s+([a-zA-Z_]\w*)\s*$", line)
            for line in lines
            if re.match(r"\s*module\s+([a-zA-Z_]\w*)\s*$", line)
        ),
        None,
    )
    module_name = module_match.group(1) if module_match else path.stem
    results: list[tuple[str, str]] = []
    index = 0
    while index < len(lines):
        line = lines[index].split("!", 1)[0]
        match = re.match(r"\s*public(?:\s*,[^:]*)?\s*::\s*(.*)$", line, re.IGNORECASE)
        if match is None:
            index += 1
            continue
        statement = match.group(1).strip()
        while statement.endswith("&") and index + 1 < len(lines):
            statement = statement[:-1].rstrip()
            index += 1
            continuation = lines[index].split("!", 1)[0].strip()
            if continuation.startswith("&"):
                continuation = continuation[1:].lstrip()
            statement += " " + continuation
        for item in statement.split(","):
            name = item.strip()
            if name:
                results.append((module_name, name))
        index += 1
    return results


def fortran_classification(path: Path, root: Path) -> dict[str, Any]:
    relative = path.relative_to(root).as_posix()
    if relative.endswith("src/core/fortsym.f90") or relative.endswith(
        "src/capi/fortsym_public_capi.f90"
    ):
        return layers(
            ("facade", "native"),
            relative,
            "public convenience or C-ABI surface over the native Fortran owner",
        )
    if any(
        marker in relative
        for marker in (
            "fortsym_engine_symengine.f90",
            "fortsym_engine_yacas.f90",
            "fortsym_engine_ext.f90",
        )
    ):
        return layers(
            ("external", "delegated"),
            relative,
            "engine boundary delegates to an external symbolic implementation",
        )
    if relative.endswith("src/engine/fortsym_engine.f90") or "/council/" in relative:
        return layers(
            ("delegated",),
            relative,
            "capability or council layer delegates to a selected engine",
        )
    return layers(
        ("native",),
        relative,
        "public declaration is owned by the in-process Fortran implementation",
    )


def build_report(inventory: dict[str, Any], root: Path) -> dict[str, Any]:
    profile = adapter_profile(
        root / "python/fortsym/sympy/__init__.py",
        root / "python/fortsym/__init__.py",
    )
    root_by_name = {record["name"]: record for record in inventory["root_exports"]}
    supported_class_ids = {
        record["class_id"]
        for record in inventory["root_exports"]
        if record["name"] in profile["supported"] and "class_id" in record
    }

    root_exports = []
    for record in inventory["root_exports"]:
        root_exports.append({
            "id": f"root.{record['name']}",
            "name": record["name"],
            "kind": record["kind"],
            **supported_layers(record["name"], profile),
        })

    modules = []
    for record in inventory["modules"]:
        modules.append({
            "id": record["name"],
            "name": record["name"],
            **layers(
                ("refused",),
                "ROADMAP.md",
                "whole-module parity is not declared; symbols are classified individually",
            ),
        })

    module_exports = []
    for module in inventory["modules"]:
        for record in module["exports"]:
            item = {
                "id": f"{module['name']}.{record['name']}",
                "module": module["name"],
                "name": record["name"],
            }
            if "access_error" in record:
                item.update(layers(
                    ("refused",),
                    "doc/sympy-api-inventory.json",
                    "public name was inventoried but its module attribute could not be accessed",
                ))
            else:
                item.update({"kind": record["kind"], **classify_module_export(record, root_by_name, profile)})
            module_exports.append(item)

    classes = []
    methods = []
    for record in inventory["classes"]:
        class_item = {
            "id": record["id"],
            "name": record["name"],
            "kind": record["kind"],
            **class_layers(record["id"], supported_class_ids, profile),
        }
        classes.append(class_item)
        for method in record["methods"]:
            method_supported = (
                record["id"] in supported_class_ids
                and method["name"] in profile["methods"]
            )
            method_layers = supported_layers(method["name"], profile) if method_supported else layers(
                ("refused",),
                "ROADMAP.md",
                "method is inventoried for parity but is outside the declared adapter subset",
            )
            methods.append({
                "id": f"{record['id']}.{method['name']}",
                "class_id": record["id"],
                "name": method["name"],
                **method_layers,
            })

    fortran = []
    for path in sorted((root / "src").rglob("*.f90")):
        for module_name, name in parse_public_declarations(path):
            fortran.append({
                "id": f"{module_name}.{name}",
                "module": module_name,
                "name": name,
                **fortran_classification(path, root),
            })

    summary = {
        "sympy": {
            "modules": len(modules),
            "module_exports": len(module_exports),
            "root_exports": len(root_exports),
            "classes": len(classes),
            "methods": len(methods),
        },
        "fortran_public_exports": len(fortran),
        "classification_counts": {},
    }
    all_items = root_exports + modules + module_exports + classes + methods + fortran
    counts: collections.Counter[str] = collections.Counter(
        layer for item in all_items for layer in item["classification"]
    )
    summary["classification_counts"] = dict(sorted(counts.items()))

    return {
        "schema_version": 1,
        "package": inventory["package"],
        "version": inventory["version"],
        "source_inventory": "doc/sympy-api-inventory.json",
        "policy": {
            "classifications": CLASSIFICATIONS,
            "multi_layer": "classification is an ordered list; a wrapper may be both delegated and native",
            "sympy_modules": "module parity is refused unless an individual symbol is explicitly supported",
            "class_methods": "only methods declared by the adapter are supported; other inventoried methods remain refused",
        },
        "adapter_profile": {
            "declared_count": len(profile["declared"]),
            "supported": sorted(profile["supported"]),
            "explicit_refusals": sorted(profile["refused"]),
        },
        "summary": summary,
        "sympy": {
            "modules": modules,
            "root_exports": root_exports,
            "module_exports": module_exports,
            "classes": classes,
            "methods": methods,
        },
        "fortran": {"public_exports": fortran},
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("inventory", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    inventory = json.loads(args.inventory.read_text(encoding="utf-8"))
    report = build_report(inventory, root)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(
        json.dumps(report, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


if __name__ == "__main__":
    main()
