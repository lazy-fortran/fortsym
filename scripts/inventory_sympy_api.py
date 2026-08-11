#!/usr/bin/env python3
"""Write a deterministic public-API inventory for the pinned SymPy release.

The inventory is metadata only. It imports the installed SymPy package, records
public module/export names and inspectable signatures, and never imports
fortsym or executes user expressions. Import failures are retained in the
report so an incomplete environment cannot look complete.
"""

from __future__ import annotations

import argparse
import importlib
import inspect
import json
import pkgutil
import re
import warnings
from pathlib import Path
from types import ModuleType
from typing import Any

import sympy
from sympy.utilities.exceptions import SymPyDeprecationWarning


warnings.filterwarnings("ignore", category=SymPyDeprecationWarning)


def public_modules() -> list[str]:
    names = {"sympy"}
    for item in pkgutil.walk_packages(sympy.__path__, "sympy."):
        parts = item.name.split(".")
        if any(part == "tests" or part == "test" or part.startswith("test_")
               for part in parts):
            continue
        names.add(item.name)
    return sorted(names)


def inspect_signature(value: Any) -> inspect.Signature | None:
    try:
        return inspect.signature(value)
    except Exception:
        # Third-party lazy annotations can execute while inspect resolves a
        # signature.  A missing signature is still useful inventory data.
        return None


def short_text(value: Any) -> str:
    return " ".join(str(value).split())[:240]


def signature_data(value: Any) -> dict[str, Any]:
    inspected = inspect_signature(value)
    if inspected is None:
        return {"signature": None}

    result: dict[str, Any] = {"signature": str(inspected)}
    options = []
    for parameter in inspected.parameters.values():
        if parameter.kind not in (
            inspect.Parameter.KEYWORD_ONLY,
            inspect.Parameter.VAR_KEYWORD,
        ) and parameter.default is inspect.Parameter.empty:
            continue
        option: dict[str, Any] = {
            "name": parameter.name,
            "kind": parameter.kind.name,
        }
        if parameter.default is not inspect.Parameter.empty:
            option["default"] = short_text(parameter.default)
        options.append(option)
    if options:
        result["options"] = options
    if inspected.return_annotation is not inspect.Parameter.empty:
        result["return_annotation"] = short_text(inspected.return_annotation)
    return result


def doc_summary(value: Any) -> str | None:
    try:
        doc = inspect.getdoc(value)
    except Exception:
        return None
    if not doc:
        return None
    first = re.split(r"\n\s*\n", doc.strip(), maxsplit=1)[0]
    return " ".join(first.split())[:400]


def kind(value: Any) -> str:
    if inspect.isclass(value):
        if issubclass(value, BaseException):
            return "exception"
        return "class"
    if inspect.isroutine(value):
        return "function"
    if callable(value):
        return "callable"
    return "object"


def doc_sections(value: Any) -> list[str]:
    try:
        doc = inspect.getdoc(value) or ""
    except Exception:
        return []
    lines = doc.splitlines()
    sections = []
    for index, line in enumerate(lines[:-1]):
        title = line.strip()
        underline = lines[index + 1].strip()
        if title and underline and set(underline) <= set("=-~^`"):
            sections.append(title)
    return sorted(set(sections))


def class_id(value: type[Any]) -> str:
    module = getattr(value, "__module__", None) or "<unknown>"
    qualname = getattr(value, "__qualname__", None) or getattr(value, "__name__", "<unknown>")
    return f"{module}.{qualname}"


def describe_class(value: type[Any]) -> dict[str, Any]:
    methods = []
    for method_name in sorted(value.__dict__):
        if method_name.startswith("_"):
            continue
        try:
            method = getattr(value, method_name)
        except Exception:
            continue
        if not inspect.isroutine(method):
            continue
        method_record: dict[str, Any] = {
            "name": method_name,
            **signature_data(method),
        }
        sections = doc_sections(method)
        if sections:
            method_record["doc_sections"] = sections
        methods.append(method_record)
    result: dict[str, Any] = {
        "id": class_id(value),
        "name": getattr(value, "__name__", None),
        "module": getattr(value, "__module__", None),
        "qualname": getattr(value, "__qualname__", None),
        "kind": "exception" if issubclass(value, BaseException) else "class",
        "bases": sorted(class_id(base) for base in value.__bases__),
        "methods": methods,
    }
    return result


def describe(
    name: str,
    value: Any,
    classes: dict[str, dict[str, Any]],
    include_doc: bool = True,
) -> dict[str, Any]:
    result: dict[str, Any] = {
        "name": name,
        "kind": kind(value),
        "module": getattr(value, "__module__", None),
        "qualname": getattr(value, "__qualname__", None),
        **signature_data(value),
    }
    if inspect.isclass(value):
        identifier = class_id(value)
        if identifier not in classes:
            classes[identifier] = describe_class(value)
        result["class_id"] = identifier
    if include_doc:
        summary = doc_summary(value)
        if summary:
            result["doc"] = summary
    sections = doc_sections(value)
    if sections:
        result["doc_sections"] = sections
    return result


def module_exports(module: ModuleType) -> list[str]:
    declared = getattr(module, "__all__", None)
    if declared is not None:
        return sorted(name for name in declared if isinstance(name, str))
    return sorted(name for name in dir(module) if not name.startswith("_"))


def build_inventory() -> dict[str, Any]:
    modules = public_modules()
    failures: list[dict[str, str]] = []
    module_records: list[dict[str, Any]] = []
    classes: dict[str, dict[str, Any]] = {}
    for module_name in modules:
        try:
            module = importlib.import_module(module_name)
        except Exception as exc:  # retain the failure instead of hiding it
            failures.append({"module": module_name, "error": f"{type(exc).__name__}: {exc}"})
            module_records.append({"name": module_name, "exports": [], "import_error": failures[-1]["error"]})
            continue
        exports = []
        for name in module_exports(module):
            try:
                value = getattr(module, name)
            except Exception as exc:
                exports.append({"name": name, "access_error": f"{type(exc).__name__}: {exc}"})
                continue
            exports.append(describe(name, value, classes, include_doc=True))
        module_records.append({"name": module_name, "exports": exports})

    root_exports = []
    for name in sorted(sympy.__all__):
        try:
            value = getattr(sympy, name)
        except Exception as exc:
            root_exports.append({"name": name, "access_error": f"{type(exc).__name__}: {exc}"})
            continue
        root_exports.append(describe(name, value, classes, include_doc=True))

    return {
        "schema_version": 1,
        "package": "sympy",
        "version": sympy.__version__,
        "module_count": len(modules),
        "root_export_count": len(root_exports),
        "import_failure_count": len(failures),
        "class_method_policy": "declared public routines; resolve inherited methods through bases",
        "root_exports": root_exports,
        "modules": module_records,
        "classes": [classes[name] for name in sorted(classes)],
        "import_failures": failures,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    parser.add_argument("--version", default="1.14.0")
    args = parser.parse_args()
    inventory = build_inventory()
    if inventory["version"] != args.version:
        raise SystemExit(
            f"expected SymPy {args.version}, found {inventory['version']}"
        )
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(
        json.dumps(inventory, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


if __name__ == "__main__":
    main()
