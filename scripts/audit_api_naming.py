#!/usr/bin/env python3
"""Audit public naming boundaries before selecting canonical API names."""

from __future__ import annotations

import argparse
import ast
import json
from pathlib import Path
from typing import Any


def exported_names(path: Path) -> list[str]:
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    for node in ast.walk(tree):
        if not isinstance(node, ast.Assign):
            continue
        if not any(isinstance(target, ast.Name) and target.id == "__all__"
                   for target in node.targets):
            continue
        if not isinstance(node.value, (ast.List, ast.Tuple)):
            raise ValueError(f"{path}: __all__ must be a literal list or tuple")
        return [item.value for item in node.value.elts
                if isinstance(item, ast.Constant) and isinstance(item.value, str)]
    raise ValueError(f"{path}: no literal __all__ found")


def concept(
    identifier: str,
    description: str,
    fortran: list[str],
    python: list[str],
    adapter: list[str],
    reference: list[str],
    finding: str,
    action: str,
) -> dict[str, Any]:
    return {
        "id": identifier,
        "description": description,
        "fortran_facade": fortran,
        "python_facade": python,
        "sympy_adapter": adapter,
        "sympy_reference": reference,
        "finding": finding,
        "action": action,
    }


def build_report(root: Path, classification: dict[str, Any]) -> dict[str, Any]:
    fortran = sorted(
        item["name"]
        for item in classification["fortran"]["public_exports"]
        if item["owner"] == "src/core/fortsym.f90"
    )
    python_facade = exported_names(root / "python/fortsym/__init__.py")
    adapter = exported_names(root / "python/fortsym/sympy/__init__.py")
    concepts = [
        concept(
            "symbol-construction",
            "Create one named symbolic expression.",
            ["sym"], ["Symbol"], ["Symbol"], ["Symbol"],
            "The native Fortran facade uses the concise internal name sym. The Python surfaces use SymPy's Symbol spelling.",
            "Keep the short native candidate and the SymPy spelling at the adapter boundary; decide whether any Fortran alias is needed.",
        ),
        concept(
            "symbol-sequences",
            "Create several named symbols.",
            ["symbols"], ["symbols"], ["symbols"], ["symbols"],
            "The same spelling crosses the Fortran and Python convenience surfaces, but the accepted syntax and return shape differ.",
            "Document one canonical native behavior and keep SymPy shape adaptation in the Python layer.",
        ),
        concept(
            "integer-construction",
            "Create an exact integer.",
            ["num"], ["Integer"], ["Integer"], ["Integer"],
            "num is concise and native. Integer is the compatibility spelling.",
            "Retain both by layer and test the mapping.",
        ),
        concept(
            "rational-construction",
            "Create an exact rational.",
            ["rat"], ["Rational"], ["Rational"], ["Rational"],
            "rat is concise and native. Rational is the compatibility spelling.",
            "Retain both by layer and test the mapping.",
        ),
        concept(
            "real-construction",
            "Create a real expression.",
            ["real_expr", "real_text_expr"], ["Float"], ["Float"], ["Float"],
            "The native facade keeps _expr on value and text constructors because the suffix distinguishes expression handles from intrinsic scalar values. The adapter has one SymPy-compatible Float constructor.",
            "Retain the disambiguating native pair and keep Float as an adapter alias.",
        ),
        concept(
            "function-construction",
            "Create an applied or named function.",
            ["func", "func_in"], ["Function"], ["Function"], ["Function"],
            "func and func_in are internal/native entry points with different argument ownership. Function is the compatibility spelling.",
            "Document the ownership distinction and avoid a second public concept unless the argument distinction is observable.",
        ),
        concept(
            "elementary-functions",
            "Create an elementary applied function.",
            ["sin", "cos", "tan", "asin", "acos", "atan", "atan2", "sinh", "cosh",
             "tanh", "asinh", "acosh", "atanh", "exp", "log", "sqrt", "abs", "erf",
             "erfc", "gamma", "besselj", "legendrep", "legendreq"],
            [],
            ["sin", "cos", "tan", "exp", "log", "sqrt", "Abs"],
            ["sin", "cos", "tan", "exp", "log", "sqrt", "Abs"],
            "The Fortran facade now uses the same intrinsic spellings as fortsym_expr. The Python adapter uses SymPy spellings and currently covers a smaller head set.",
            "Keep the elementary functions as one native family and do not reintroduce per-function aliases.",
        ),
        concept(
            "constants",
            "Create standard named constants.",
            ["pi_expr", "e_expr", "i_expr"], [], ["pi", "E", "I", "oo"],
            ["pi", "E", "I", "oo"],
            "Native names describe expression constructors. Python compatibility names follow SymPy's capitalization and infinity spelling.",
            "Keep the adapter spelling and decide whether the native names should remain descriptive or shorten as a consistent family.",
        ),
        concept(
            "differentiation",
            "Differentiate an expression.",
            ["partial", "diff"], ["diff"], ["diff", "Derivative"], ["diff", "Derivative"],
            "partial constructs an explicit unevaluated node; the easy facade's diff uses the native evaluated operation. The Python layer keeps SymPy's diff and Derivative boundary.",
            "Keep partial and diff in the calculus owner, with the facade forwarding evaluated differentiation consistently.",
        ),
        concept(
            "arena-and-expression",
            "Own expression storage and expression handles.",
            ["arena_t", "expr_t", "default_arena", "reset"],
            ["Arena", "Expr"], ["Arena", "Expr"], ["Expr"],
            "Fortran types and Python handle classes are distinct language bindings. The default-state functions have no direct SymPy root equivalent, and the native facade uses short names because its module already supplies the namespace.",
            "Keep one responsibility per owner and document default versus explicit state once.",
        ),
        concept(
            "substitution",
            "Replace expressions.",
            ["subs"], ["subs"], ["subs", "Subs"], ["Expr", "Subs"],
            "The native facade forwards structural substitution from its single owning module and reports invalid handles through the common optional status outputs.",
            "Keep structural substitution in fortsym_subs and expose only the concise native subs entry point from the facade.",
        ),
        concept(
            "transformation-functions",
            "Transform an expression algebraically.",
            ["expand", "simplify", "factor"], ["factor"], ["expand", "simplify", "factor"], ["expand", "simplify", "factor"],
            "The Python adapter exposes the native transformation subset. The Fortran convenience facade now forwards all three operations to the native engine with one optional status convention.",
            "Keep one native engine owner and expose the concise transformation names from the easy facade.",
        ),
        concept(
            "identity-and-validity",
            "Inspect handle validity and arena ownership.",
            ["is_valid", "same_arena"], [], [], ["Expr"],
            "These are native safety predicates with no Python compatibility spelling.",
            "Keep them out of the SymPy adapter unless a defined SymPy behavior exists.",
        ),
        concept(
            "numeric-inspection",
            "Evaluate or inspect numeric values.",
            ["numeric_value", "numeric_text", "numeric_precision_text", "numeric_complex_text",
             "numeric_complex_text_t", "numeric_callable_t"],
            [], [], [],
            "These are fortsym-specific inspection and callback concepts, not SymPy root aliases.",
            "Keep them in a numeric module with one naming family.",
        ),
        concept(
            "backend-evidence",
            "Record backend proof and equivalence evidence.",
            ["backend_evidence_t", "backend_result_t", "backend_status_name", "serialize_expression",
             "deserialize_expression", "assess_identity", "assess_equivalence", "evidence_json",
             "emit_backend_kernel"],
            [], [], [],
            "These are fortsym verification and code-generation concepts with no SymPy facade equivalent.",
            "Keep them behind their owning verification/backend modules.",
        ),
        concept(
            "node-kinds",
            "Inspect and identify native DAG node kinds.",
            ["node_kind_name", "NK_INT", "NK_RAT", "NK_REAL", "NK_SYM", "NK_CONST",
             "NK_ADD", "NK_MUL", "NK_POW", "NK_FUNC", "NK_BIG_INT", "NK_BIG_RAT",
             "NK_BIG_REAL"],
            [], [], [],
            "These are representation-level native tags with no SymPy facade equivalent.",
            "Keep node-kind vocabulary private or in the representation owner unless consumers need it.",
        ),
        concept(
            "exact-and-constant-construction",
            "Construct exact text or named constants in the native arena.",
            ["exact", "const"], [], [], [],
            "exact and const are concise native constructors. They overlap with the more specialized numeric and constant families above.",
            "Decide whether the generic constructors remain public or become lower-level implementation entry points.",
        ),
        concept(
            "operator-overloads",
            "Use Fortran operators and character assignment for the default facade.",
            ["operator(+)" , "operator(-)", "operator(*)", "operator(/)", "operator(**)",
             "operator(==)", "operator(/=)", "assignment(=)"],
            [], [], [],
            "Operators and assignment are language syntax, not named SymPy functions. They belong to the native facade consistency audit.",
            "Keep one operator family and document assignment as symbol construction only.",
        ),
        concept(
            "backend-protocol",
            "Represent backend protocol versions, statuses, and serialized evidence.",
            ["BACKEND_PROTOCOL_VERSION", "EXPRESSION_SCHEMA", "BACKEND_PROVED",
             "BACKEND_DISPROVED", "BACKEND_UNKNOWN"],
            [], [], [],
            "These names describe fortsym's verification protocol and have no SymPy root equivalent.",
            "Keep protocol constants in the backend owner and do not mirror them into the symbolic facade.",
        ),
        concept(
            "ode-solving",
            "Solve the native ODE fragment.",
            ["solve_ode"], [], [], [],
            "solve_ode is a native specialized operation with no current Python or SymPy adapter entry.",
            "Keep it in the calculus owner until its SymPy parity contract is defined.",
        ),
    ]
    assigned = {
        name
        for item in concepts
        for layer in ("fortran_facade", "python_facade", "sympy_adapter")
        for name in item[layer]
    }
    for item in concepts:
        if not set(item["fortran_facade"]) <= set(fortran):
            raise ValueError(f"unknown Fortran facade name in {item['id']}")
        if not set(item["python_facade"]) <= set(python_facade):
            raise ValueError(f"unknown Python facade name in {item['id']}")
        if not set(item["sympy_adapter"]) <= set(adapter):
            raise ValueError(f"unknown adapter name in {item['id']}")
        root_names = {
            export["name"] for export in classification["sympy"]["root_exports"]
        }
        if not set(item["sympy_reference"]) <= root_names:
            raise ValueError(f"unknown SymPy reference name in {item['id']}")
    unmapped = sorted(set(fortran) - assigned)
    return {
        "schema_version": 1,
        "package": "fortsym",
        "sympy_version": classification["version"],
        "policy": {
            "fortran_facade": "short native names are audited as the canonical candidates",
            "python_facade": "fortsym's native Python binding is audited separately from the SymPy spelling adapter",
            "sympy_adapter": "fortsym.sympy uses SymPy names where compatibility requires them",
            "selection": "This report records the selected native vocabulary and the deliberate Python compatibility boundary.",
        },
        "sources": {
            "classification": "doc/sympy-api-classification.json",
            "fortran_facade": "src/core/fortsym.f90",
            "python_facade": "python/fortsym/__init__.py",
            "sympy_adapter": "python/fortsym/sympy/__init__.py",
        },
        "summary": {
            "fortran_facade_exports": len(fortran),
            "python_facade_exports": len(python_facade),
            "sympy_adapter_exports": len(adapter),
            "concept_groups": len(concepts),
            "assigned_names": len(assigned),
            "unmapped_fortran_exports": len(set(fortran) - assigned),
        },
        "exports": {
            "fortran_facade": [
                {"name": name, "concepts": [item["id"] for item in concepts if name in item["fortran_facade"]]}
                for name in fortran
            ],
            "python_facade": sorted(python_facade),
            "sympy_adapter": sorted(adapter),
        },
        "unmapped_fortran_exports": unmapped,
        "concepts": concepts,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("classification", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    classification = json.loads(args.classification.read_text(encoding="utf-8"))
    report = build_report(root, classification)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
