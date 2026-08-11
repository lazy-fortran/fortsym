#!/usr/bin/env python3
"""Benchmark the declared Python subset against SymPy 1.14.0.

The report separates cold end-to-end calls from warm core calls.  Every
workload is checked against a SymPy result before timing, and the report keeps
the native/SymPy ratio visible instead of hiding it in one aggregate score.
"""

from __future__ import annotations

import argparse
import json
import math
import platform
import statistics
import sys
import time
from pathlib import Path
from typing import Any, Callable

import sympy as oracle

import fortsym.sympy as native


def result_text(value: Any) -> str:
    return str(value).replace("Abs(", "abs(")


def equivalent(expected: Any, actual: Any, names: dict[str, Any]) -> bool:
    expected_text = result_text(expected)
    actual_text = str(actual)
    try:
        expected_parsed = oracle.sympify(expected_text, locals=names)
        actual_parsed = oracle.sympify(actual_text, locals=names)
        return oracle.simplify(actual_parsed - expected_parsed) == oracle.Integer(0)
    except (TypeError, ValueError, SyntaxError):
        return False


def measure(function: Callable[[], Any], warmup: int, repetitions: int, batch: int) -> dict[str, Any]:
    for _ in range(warmup):
        for _ in range(batch):
            function()
    samples = []
    for _ in range(repetitions):
        started = time.perf_counter_ns()
        for _ in range(batch):
            function()
        elapsed = time.perf_counter_ns() - started
        samples.append(elapsed / batch)
    return {
        "median_ns": statistics.median(samples),
        "min_ns": min(samples),
        "max_ns": max(samples),
        "samples_ns": samples,
    }


def workload_factories(label: str, suffix: str) -> tuple[dict[str, Any], dict[str, Any]]:
    name_x = f"{label}_x_{suffix}"
    name_y = f"{label}_y_{suffix}"
    oracle_x = oracle.Symbol(name_x)
    oracle_y = oracle.Symbol(name_y)
    native_x = native.Symbol(name_x)
    native_y = native.Symbol(name_y)
    names = {name_x: oracle_x, name_y: oracle_y, "abs": oracle.Abs}

    expressions = {
        "expand": (
            (oracle_x + oracle_y + 1) ** 4,
            (native_x + native_y + 1) ** 4,
            names,
        ),
        "differentiate": (
            oracle.exp(oracle_x * oracle_y),
            native.exp(native_x * native_y),
            names,
        ),
        "simplify": (
            oracle.sqrt(oracle_x**2),
            native.sqrt(native_x**2),
            names,
        ),
        "refine": (
            oracle.sqrt(oracle_x**2),
            native.sqrt(native_x**2),
            names,
        ),
        "relation": (
            oracle.Gt(oracle_x, 1),
            native.Gt(native_x, 1),
            names,
        ),
        "factor": (
            oracle_x**2 + 2 * oracle_x + 1,
            native_x**2 + 2 * native_x + 1,
            names,
        ),
        "assumption_query": (
            oracle.Q.positive(oracle_x),
            native.Q.positive(native_x),
            names,
        ),
    }
    return expressions, names


def build_expression(engine: Any, operation: str, suffix: str) -> tuple[Any, Any]:
    x = engine.Symbol(f"{operation}_x_{suffix}")
    if operation == "expand":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = (x + y + 1) ** 4
    elif operation == "differentiate":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = engine.exp(x * y)
    elif operation == "simplify":
        expression = engine.sqrt(x**2)
    elif operation == "factor":
        expression = x**2 + 2 * x + 1
    elif operation == "assumption_query":
        expression = engine.Q.positive(x)
    elif operation == "refine":
        expression = engine.sqrt(x**2)
    elif operation == "relation":
        expression = engine.Gt(x, 1)
    else:
        raise ValueError(f"unknown benchmark operation: {operation}")
    return expression, x


def correctness_cases() -> list[dict[str, Any]]:
    oracle_cases = workload_factories("check", "fixed")[0]
    native_cases = workload_factories("check", "fixed")[0]
    results = []
    for operation in sorted(oracle_cases):
        oracle_expression, native_expression, names = oracle_cases[operation]
        _, native_expression, _ = native_cases[operation]
        if operation == "expand":
            expected = oracle.expand(oracle_expression)
            actual = native.expand(native_expression)
        elif operation == "differentiate":
            expected = oracle.diff(oracle_expression, names[f"check_x_fixed"])
            actual = native.diff(native_expression, native.Symbol("check_x_fixed"))
        elif operation == "simplify":
            expected = oracle.simplify(oracle_expression)
            actual = native.simplify(native_expression)
        elif operation == "assumption_query":
            expected = oracle.ask(oracle_expression)
            actual = native.ask(native_expression)
        elif operation == "refine":
            expected = oracle.refine(
                oracle_expression, oracle.Q.positive(names["check_x_fixed"])
            )
            actual = native.refine(
                native_expression, native.Q.positive(native.Symbol("check_x_fixed"))
            )
        elif operation == "relation":
            expected = oracle.Gt(names["check_x_fixed"], 1)
            actual = native.Gt(native.Symbol("check_x_fixed"), 1)
        else:
            expected = oracle.factor(oracle_expression)
            actual = native.factor(native_expression)
        results.append({
            "operation": operation,
            "correct": (
                expected == actual
                if operation == "assumption_query"
                else str(expected) == str(actual)
                if operation == "relation"
                else equivalent(expected, actual, names)
            ),
            "expected": result_text(expected),
            "actual": str(actual),
        })
    return results


def benchmark_workload(
    operation: str,
    scope: str,
    warmup: int,
    repetitions: int,
    batch: int,
) -> dict[str, Any]:
    if scope == "warm_core":
        expressions, _ = workload_factories(operation, "warm")
        oracle_expression, native_expression, _ = expressions[operation]
        if operation == "differentiate":
            oracle_x = oracle.Symbol(f"{operation}_x_warm")
            native_x = native.Symbol(f"{operation}_x_warm")
            oracle_call = lambda: oracle.diff(oracle_expression, oracle_x)
            native_call = lambda: native.diff(native_expression, native_x)
        else:
            if operation == "expand":
                oracle_call = lambda: oracle.expand(oracle_expression)
                native_call = lambda: native.expand(native_expression)
            elif operation == "simplify":
                oracle_call = lambda: oracle.simplify(oracle_expression)
                native_call = lambda: native.simplify(native_expression)
            elif operation == "factor":
                oracle_call = lambda: oracle.factor(oracle_expression)
                native_call = lambda: native.factor(native_expression)
            elif operation == "assumption_query":
                oracle_call = lambda: oracle.ask(oracle_expression)
                native_call = lambda: native.ask(native_expression)
            elif operation == "refine":
                oracle_x = oracle.Symbol("refine_x_warm")
                native_x = native.Symbol("refine_x_warm")
                oracle_call = lambda: oracle.refine(
                    oracle_expression, oracle.Q.positive(oracle_x)
                )
                native_call = lambda: native.refine(
                    native_expression, native.Q.positive(native_x)
                )
            elif operation == "relation":
                oracle_x = oracle.Symbol("relation_x_warm")
                native_x = native.Symbol("relation_x_warm")
                oracle_call = lambda: oracle.Gt(oracle_x, 1)
                native_call = lambda: native.Gt(native_x, 1)
    else:
        def make_call(engine: Any) -> Callable[[], Any]:
            counter = {"value": 0}

            def call():
                suffix = str(counter["value"])
                counter["value"] += 1
                expression, variable = build_expression(engine, operation, suffix)
                if operation == "differentiate":
                    return engine.diff(expression, variable)
                if operation == "assumption_query":
                    return engine.ask(expression)
                if operation == "refine":
                    return engine.refine(expression, engine.Q.positive(variable))
                if operation == "relation":
                    return engine.Gt(variable, 1)
                return getattr(engine, operation)(expression)

            return call

        oracle_call = make_call(oracle)
        native_call = make_call(native)

    oracle_timing = measure(oracle_call, warmup, repetitions, batch)
    native_timing = measure(native_call, warmup, repetitions, batch)
    native_median = native_timing["median_ns"]
    oracle_median = oracle_timing["median_ns"]
    return {
        "operation": operation,
        "scope": scope,
        "batch": batch,
        "warmup": warmup,
        "repetitions": repetitions,
        "sympy": oracle_timing,
        "fortsym": native_timing,
        "native_over_sympy": native_median / oracle_median if oracle_median else math.inf,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=Path("-"))
    parser.add_argument("--warmup", type=int, default=3)
    parser.add_argument("--repetitions", type=int, default=7)
    parser.add_argument("--batch", type=int, default=20)
    parser.add_argument(
        "--enforce-parity",
        action="store_true",
        help="fail when a supported native workload is slower than SymPy",
    )
    parser.add_argument(
        "--waive",
        action="append",
        default=[],
        metavar="OPERATION:SCOPE",
        help="explicitly waive one parity row when enforcement is enabled",
    )
    args = parser.parse_args()
    if min(args.warmup, args.repetitions, args.batch) < 1:
        raise SystemExit("warmup, repetitions, and batch must be positive")
    if oracle.__version__ != "1.14.0":
        raise SystemExit(f"expected SymPy 1.14.0, found {oracle.__version__}")

    correctness = correctness_cases()
    if not all(case["correct"] for case in correctness):
        raise SystemExit(json.dumps({"correctness": correctness}, indent=2))

    workloads = []
    for operation in (
        "expand", "differentiate", "simplify", "refine", "relation", "factor",
        "assumption_query"
    ):
        for scope in ("cold_end_to_end", "warm_core"):
            workloads.append(benchmark_workload(
                operation, scope, args.warmup, args.repetitions, args.batch,
            ))
    workload_ids = {f"{item['operation']}:{item['scope']}" for item in workloads}
    unknown_waivers = set(args.waive) - workload_ids
    if unknown_waivers:
        raise SystemExit(
            "unknown parity waiver(s): " + ", ".join(sorted(unknown_waivers))
        )
    violations = []
    for item in workloads:
        identifier = f"{item['operation']}:{item['scope']}"
        item["parity"] = {
            "within_baseline": item["native_over_sympy"] <= 1.0,
            "waived": identifier in args.waive,
        }
        if not item["parity"]["within_baseline"] and not item["parity"]["waived"]:
            violations.append(identifier)
    report = {
        "schema_version": 1,
        "package": "fortsym.sympy",
        "sympy_version": oracle.__version__,
        "python": sys.version,
        "platform": platform.platform(),
        "processor": platform.processor(),
        "cpu_count": __import__("os").cpu_count(),
        "timer": "time.perf_counter_ns",
        "parameters": {
            "warmup": args.warmup,
            "repetitions": args.repetitions,
            "batch": args.batch,
        },
        "correctness_oracle": "SymPy 1.14.0 parse/equality plus fixed exact text checks",
        "correctness": correctness,
        "parity": {
            "enforced": args.enforce_parity,
            "waivers": sorted(args.waive),
            "violations": violations,
        },
        "workloads": workloads,
    }
    text = json.dumps(report, indent=2, sort_keys=True) + "\n"
    if str(args.output) == "-":
        print(text, end="")
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(text, encoding="utf-8")
    if args.enforce_parity and violations:
        raise SystemExit(
            "native parity failed for: " + ", ".join(violations)
        )


if __name__ == "__main__":
    main()
