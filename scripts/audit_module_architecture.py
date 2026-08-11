#!/usr/bin/env python3
"""Audit Fortran module ownership and internal dependency direction."""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path
from typing import Any


MODULE_PATTERN = re.compile(
    r"^\s*module\s+(?!procedure\b)([a-z][a-z0-9_]*)\b",
    re.IGNORECASE | re.MULTILINE,
)
USE_PATTERN = re.compile(
    r"^\s*use\s+(fortsym(?:_[a-z0-9_]+)?)\b",
    re.IGNORECASE | re.MULTILINE,
)
FACADE_USE_PATTERN = re.compile(
    r"^\s*use\s+fortsym\s*(?:,|$)",
    re.IGNORECASE | re.MULTILINE,
)


def find_modules(root: Path) -> tuple[dict[str, Path], list[dict[str, Any]]]:
    modules: dict[str, Path] = {}
    units: list[dict[str, Any]] = []
    for path in sorted((root / "src").rglob("*.f90")):
        names = [name.lower() for name in MODULE_PATTERN.findall(
            path.read_text(encoding="utf-8")
        )]
        unit = {
            "path": str(path.relative_to(root)),
            "owner": path.parent.name,
            "modules": names,
        }
        units.append(unit)
        for name in names:
            modules.setdefault(name, path)
    return modules, units


def find_duplicate_modules(units: list[dict[str, Any]]) -> list[str]:
    locations: dict[str, list[str]] = {}
    for unit in units:
        for name in unit["modules"]:
            locations.setdefault(name, []).append(unit["path"])
    return sorted(
        f"{name}: {', '.join(paths)}"
        for name, paths in locations.items()
        if len(paths) > 1
    )


def dependency_graph(
    root: Path, modules: dict[str, Path]
) -> tuple[dict[str, set[str]], list[str], list[str]]:
    graph = {name: set() for name in modules}
    unknown: set[str] = set()
    facade_importers: set[str] = set()
    for name, path in modules.items():
        text = path.read_text(encoding="utf-8")
        graph[name] = {dependency.lower() for dependency in USE_PATTERN.findall(text)}
        for dependency in graph[name]:
            if dependency not in modules:
                unknown.add(f"{name} -> {dependency}")
        if FACADE_USE_PATTERN.search(text):
            facade_importers.add(name)
    return graph, sorted(unknown), sorted(facade_importers)


def find_cycles(graph: dict[str, set[str]]) -> list[list[str]]:
    state: dict[str, int] = {}
    stack: list[str] = []
    cycles: list[list[str]] = []

    def visit(name: str) -> None:
        state[name] = 1
        stack.append(name)
        for dependency in sorted(graph[name]):
            if dependency not in graph:
                continue
            if state.get(dependency, 0) == 0:
                visit(dependency)
            elif state[dependency] == 1:
                start = stack.index(dependency)
                cycles.append(stack[start:] + [dependency])
        stack.pop()
        state[name] = 2

    for name in sorted(graph):
        if state.get(name, 0) == 0:
            visit(name)
    return cycles


def layer_edges(
    graph: dict[str, set[str]], modules: dict[str, Path]
) -> list[dict[str, Any]]:
    counts: dict[tuple[str, str], int] = {}
    for name, dependencies in graph.items():
        source_layer = modules[name].parent.name
        for dependency in dependencies:
            target_layer = modules[dependency].parent.name
            if source_layer != target_layer:
                key = (source_layer, target_layer)
                counts[key] = counts.get(key, 0) + 1
    return [
        {"from": source, "to": target, "uses": count}
        for (source, target), count in sorted(counts.items())
    ]


def build_report(root: Path) -> dict[str, Any]:
    modules, units = find_modules(root)
    duplicate_modules = find_duplicate_modules(units)
    module_file_mismatches = sorted(
        f"{unit['path']}: {unit['modules'][0]}"
        for unit in units
        if len(unit["modules"]) == 1
        and Path(unit["path"]).stem.lower() != unit["modules"][0]
    )
    graph, unknown_dependencies, facade_importers = dependency_graph(root, modules)
    cycles = find_cycles(graph)
    internal_edges = sum(len(dependencies) for dependencies in graph.values())
    return {
        "schema_version": 1,
        "source_root": "src",
        "audit_scope": {
            "single_responsibility": "one named Fortran module per source file, with filename and directory ownership",
            "dependency_direction": "known internal use edges, no cycles, and no implementation import of the convenience facade",
            "non_module_units": "generated source units may contain no module and are reported separately",
        },
        "summary": {
            "source_units": len(units),
            "module_units": sum(bool(unit["modules"]) for unit in units),
            "modules": len(modules),
            "internal_edges": internal_edges,
            "duplicate_modules": len(duplicate_modules),
            "module_file_mismatches": len(module_file_mismatches),
            "unknown_internal_dependencies": len(unknown_dependencies),
            "facade_importers": len(facade_importers),
            "cycles": len(cycles),
        },
        "violations": {
            "duplicate_modules": duplicate_modules,
            "module_file_mismatches": module_file_mismatches,
            "unknown_internal_dependencies": unknown_dependencies,
            "facade_importers": facade_importers,
            "cycles": cycles,
        },
        "non_module_units": [
            unit["path"] for unit in units if not unit["modules"]
        ],
        "modules": [
            {
                "name": name,
                "path": str(path.relative_to(root)),
                "owner": path.parent.name,
                "dependencies": sorted(graph[name]),
            }
            for name, path in sorted(modules.items())
        ],
        "layer_edges": layer_edges(graph, modules),
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    report = build_report(root)
    violations = report["violations"]
    if any(violations.values()):
        raise SystemExit(json.dumps(violations, indent=2))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n",
                           encoding="utf-8")
    summary = report["summary"]
    print(
        f"validated {summary['modules']} modules, "
        f"{summary['internal_edges']} internal edges, and zero structural violations"
    )


if __name__ == "__main__":
    main()
