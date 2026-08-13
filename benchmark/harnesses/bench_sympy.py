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

import fortsym as native_core
import fortsym.sympy as native


_PREDICATE_OPERATIONS = ("number_predicate", "algebraic_predicate")
_ASSUMPTION_OPERATIONS = (
    "assumption_query", "integer_assumption_query",
    "rational_assumption_query", "algebraic_assumption_query",
)
_CONSTRUCTION_OPERATIONS = ("power_constructor", "power_one_constructor")


def predicate_value(expression: Any, operation: str) -> Any:
    if operation == "algebraic_predicate":
        return expression.is_algebraic
    return expression.is_number


def reset_native_default_arena() -> None:
    """Keep correctness construction from seeding later workload contexts."""
    if native_core._default_arena is not None:
        native_core._default_arena.close()
    native_core._default_arena = None


def result_text(value: Any) -> str:
    return str(value).replace("Abs(", "abs(")


def free_symbol_names(expression: Any, engine: Any) -> set[str]:
    if engine is oracle:
        return {str(symbol) for symbol in expression.free_symbols}
    symbols = expression.free_symbols
    try:
        return {str(symbol) for symbol in symbols}
    finally:
        for symbol in symbols:
            symbol.close()


def imaginary_unit(engine: Any) -> Any:
    if engine is oracle:
        return engine.I
    return engine._default().constant("i")


def equivalent(expected: Any, actual: Any, names: dict[str, Any]) -> bool:
    expected_text = result_text(expected)
    actual_text = str(actual)
    try:
        expected_parsed = oracle.sympify(expected_text, locals=names)
        actual_parsed = oracle.sympify(actual_text, locals=names)
        return oracle.simplify(actual_parsed - expected_parsed) == oracle.Integer(0)
    except (TypeError, ValueError, SyntaxError):
        return False


def structurally_equivalent(expected: Any, actual: Any, names: dict[str, Any]) -> bool:
    """Compare domain expressions without subtracting infinities."""
    try:
        expected_parsed = oracle.sympify(result_text(expected), locals=names)
        actual_parsed = oracle.sympify(str(actual), locals=names)
        return actual_parsed == expected_parsed
    except (TypeError, ValueError, SyntaxError):
        return False


def compound_equivalent(expected: Any, actual: Any) -> bool:
    expected_names = {
        "StrictGreaterThan": "Greater",
        "Unequality": "Unequal",
    }
    expected_signature = []
    for argument in expected.args:
        expected_signature.append((
            expected_names.get(type(argument).__name__, type(argument).__name__),
            tuple(str(child) for child in argument.args),
        ))
    native_arguments = actual.args
    try:
        actual_signature = []
        for argument in native_arguments:
            children = argument.args
            try:
                actual_signature.append((
                    argument.name, tuple(str(child) for child in children)
                ))
            finally:
                for child in children:
                    child.close()
    finally:
        for argument in native_arguments:
            argument.close()
        actual.close()
    return expected_signature == actual_signature


def nullspace_equivalent(expected: Any, actual: Any) -> bool:
    """Compare SymPy and native lists of column matrices independently."""
    if len(expected) != len(actual):
        return False
    return all(
        left.shape == right.shape and str(left) == str(right)
        for left, right in zip(expected, actual)
    )


def rref_equivalent(expected: Any, actual: Any) -> bool:
    return (str(expected[0]) == str(actual[0]) and
            tuple(str(value) for value in expected[1]) ==
            tuple(str(value) for value in actual[1]))


def matrix_multiply_equivalent(expected: Any, actual: Any) -> bool:
    return expected.shape == actual.shape and str(expected) == str(actual)


def matrix_elementwise_equivalent(expected: Any, actual: Any) -> bool:
    return expected.shape == actual.shape and str(expected) == str(actual)


def match_equivalent(expected: Any, actual: Any) -> bool:
    if expected is None or actual is None:
        return expected is actual
    expected_values = {str(key): value for key, value in expected.items()}
    actual_values = {str(key): value for key, value in actual.items()}
    if expected_values.keys() != actual_values.keys():
        return False
    for key in expected_values:
        try:
            expected_value = oracle.sympify(str(expected_values[key]))
            actual_value = oracle.sympify(str(actual_values[key]))
            if oracle.simplify(actual_value - expected_value) != 0:
                return False
        except (TypeError, ValueError, SyntaxError):
            return False
    return True


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


def inverse_domain_expression(engine: Any) -> Any:
    if engine is oracle:
        infinity = engine.oo
    else:
        infinity = engine._default().constant("oo")
    return (
        engine.asin(infinity) + engine.acos(-infinity)
        + engine.atan(infinity) + engine.asinh(infinity)
        + engine.acosh(-infinity) + engine.atanh(infinity)
    )


def reciprocal_hyperbolic_domain_expression(engine: Any, suffix: str) -> Any:
    if engine is oracle:
        infinity = engine.oo
    else:
        infinity = engine._default().constant("oo")
    x = engine.Symbol(f"domain_reciprocal_x_{suffix}")
    y = engine.Symbol(f"domain_reciprocal_y_{suffix}")
    return (
        engine.csch(infinity) + engine.sech(-infinity) + engine.coth(infinity)
        + (x + y + 1)**4
    )


def error_function_domain_expression(engine: Any, suffix: str) -> Any:
    if engine is oracle:
        infinity = engine.oo
    else:
        infinity = engine._default().constant("oo")
    x = engine.Symbol(f"domain_error_x_{suffix}")
    y = engine.Symbol(f"domain_error_y_{suffix}")
    return (
        engine.erf(infinity) + engine.erfc(-infinity)
        + (x + y + 1)**4
    )


def gamma_domain_expression(engine: Any, suffix: str) -> Any:
    if engine is oracle:
        infinity = engine.oo
    else:
        infinity = engine._default().constant("oo")
    x = engine.Symbol(f"domain_gamma_x_{suffix}")
    y = engine.Symbol(f"domain_gamma_y_{suffix}")
    return (
        engine.gamma(infinity) + engine.loggamma(infinity)
        + engine.factorial(infinity)
        + (x + y + 1)**4
    )


def atan2_domain_expression(engine: Any, suffix: str) -> Any:
    if engine is oracle:
        infinity = engine.oo
    else:
        infinity = engine._default().constant("oo")
    x = engine.Symbol(f"domain_atan2_x_{suffix}")
    y = engine.Symbol(f"domain_atan2_y_{suffix}")
    return (
        engine.atan2(infinity, infinity)
        + engine.atan2(-infinity, -infinity)
        + (x + y + 1)**4
    )


def bessel_domain_expression(engine: Any, suffix: str) -> Any:
    if engine is oracle:
        infinity = engine.oo
    else:
        infinity = engine._default().constant("oo")
    order = engine.Symbol(f"domain_bessel_order_{suffix}")
    x = engine.Symbol(f"domain_bessel_x_{suffix}")
    y = engine.Symbol(f"domain_bessel_y_{suffix}")
    return (
        engine.besselj(order, infinity)
        + engine.besseli(order, infinity)
        + (x + y + 1)**4
    )


def legendre_domain_expression(engine: Any, suffix: str) -> Any:
    if engine is oracle:
        infinity = engine.oo
    else:
        infinity = engine._default().constant("oo")
    x = engine.Symbol(f"domain_legendre_x_{suffix}")
    y = engine.Symbol(f"domain_legendre_y_{suffix}")
    return engine.legendre(2, infinity) + (x + y + 1)**4


def complex_domain_expression(engine: Any, suffix: str) -> Any:
    name = f"domain_complex_x_{suffix}"
    real_symbol = engine.Symbol(name, real=True)
    imaginary = imaginary_unit(engine)
    return (real_symbol + imaginary)**2


def complex_abs_expression(engine: Any, suffix: str) -> Any:
    name = f"domain_abs_x_{suffix}"
    real_symbol = engine.Symbol(name, real=True)
    imaginary = imaginary_unit(engine)
    return real_symbol + imaginary


def complex_expand_expression(engine: Any, suffix: str) -> Any:
    name = f"domain_expand_complex_x_{suffix}"
    real_symbol = engine.Symbol(name, real=True)
    imaginary = imaginary_unit(engine)
    return (real_symbol + imaginary)**4


def workload_factories(label: str, suffix: str) -> tuple[dict[str, Any], dict[str, Any]]:
    name_x = f"{label}_x_{suffix}"
    name_y = f"{label}_y_{suffix}"
    oracle_x = oracle.Symbol(name_x)
    oracle_y = oracle.Symbol(name_y)
    native_x = native.Symbol(name_x)
    native_y = native.Symbol(name_y)
    integer_name = f"{label}_integer_{suffix}"
    oracle_integer = oracle.Symbol(integer_name, integer=True)
    native_integer = native.Symbol(integer_name, integer=True)
    rational_name = f"{label}_rational_{suffix}"
    oracle_rational = oracle.Symbol(rational_name, rational=True)
    native_rational = native.Symbol(rational_name, rational=True)
    native_oo = native._default().constant("oo")
    native_i = imaginary_unit(native)
    native_three_halves = native.Rational(3, 2)
    native_two_thirds = native.Rational(2, 3)
    sqrt_power_name = f"{label}_sqrt_power_{suffix}"
    oracle_sqrt_power = oracle.Symbol(sqrt_power_name)
    native_sqrt_power = native.Symbol(sqrt_power_name)
    has_composition = label in ("check", "composition")
    names = {
        name_x: oracle_x,
        name_y: oracle_y,
        sqrt_power_name: oracle_sqrt_power,
        integer_name: oracle_integer,
        rational_name: oracle_rational,
        "abs": oracle.Abs,
        "i": imaginary_unit(oracle),
    }
    if has_composition:
        composition_name = f"{label}_composition_{suffix}"
        oracle_composition = oracle.Symbol(composition_name, real=True)
        native_composition = native.Symbol(composition_name, real=True)
        names[composition_name] = oracle_composition

    expressions = {
        "expand": (
            (oracle_x + oracle_y + 1) ** 4,
            (native_x + native_y + 1) ** 4,
            names,
        ),
        "count_ops": (
            (oracle_x + oracle_y + 1) ** 2,
            (native_x + native_y + 1) ** 2,
            names,
        ),
        "free_symbols": (
            oracle_x + oracle.sin(oracle_y * oracle_x),
            native_x + native.sin(native_y * native_x),
            names,
        ),
        "subs_simultaneous": (
            oracle_x + 2*oracle_y,
            native_x + 2*native_y,
            names,
        ),
        "subs_mapping": (
            oracle.Function("subs_mapping_f")(oracle_x**2 + oracle_x),
            native.Function("subs_mapping_f")(native_x**2 + native_x),
            names,
        ),
        "xreplace": (
            sum((oracle_x + index)**2 for index in range(8)),
            sum((native_x + index)**2 for index in range(8)),
            names,
        ),
        "replace": (
            oracle_x + 1,
            native_x + 1,
            names,
        ),
        "match": (
            oracle.Function("match_f")(oracle_x**2 + oracle_x),
            native.Function("match_f")(native_x**2 + native_x),
            names,
        ),
        "match_wild": (
            oracle_x + 1,
            native_x + 1,
            names,
        ),
        "match_wild_remainder": (
            oracle_x + oracle_y + sum(
                oracle.Symbol(f"{label}_remainder_z_{index}_{suffix}")
                for index in range(24)
            ),
            native_x + native_y + sum(
                native.Symbol(f"{label}_remainder_z_{index}_{suffix}")
                for index in range(24)
            ),
            names,
        ),
        "match_wild_partition": (
            oracle_x + oracle_y + 1,
            native_x + native_y + 1,
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
        "sqrt_power": (
            oracle.Pow(
                oracle.sqrt(oracle_sqrt_power, evaluate=False),
                2,
                evaluate=False,
            ),
            native.sqrt(native_sqrt_power)**2,
            names,
        ),
        "power_constructor": (
            oracle_x**0,
            native_x**0,
            names,
        ),
        "power_one_constructor": (
            oracle_x**1,
            native_x**1,
            names,
        ),
        "domain_function": (
            oracle.sqrt(-oracle.oo, evaluate=False),
            native.sqrt(-native_oo),
            names,
        ),
        "domain_log_zero": (
            oracle.log(0),
            native.log(0),
            names,
        ),
        "domain_log_negative": (
            oracle.log(-2),
            native.log(-2),
            names,
        ),
        "domain_log_imaginary": (
            oracle.log(oracle.I),
            native.log(native_i),
            names,
        ),
        "domain_gamma_pole": (
            oracle.gamma(0),
            native.gamma(0),
            names,
        ),
        "domain_loggamma_pole": (
            oracle.loggamma(0),
            native.loggamma(0),
            names,
        ),
        "domain_factorial_pole": (
            oracle.factorial(-1),
            native.factorial(-1),
            names,
        ),
        "domain_factorial_value": (
            oracle.factorial(5),
            native.factorial(5),
            names,
        ),
        "domain_factorial_large": (
            oracle.factorial(100),
            native.factorial(100),
            names,
        ),
        "domain_atanh_pole": (
            oracle.atanh(1),
            native.atanh(1),
            names,
        ),
        "domain_atanh_imaginary": (
            oracle.atanh(oracle.I),
            native.atanh(native_i),
            names,
        ),
        "domain_atan_imaginary": (
            oracle.atan(oracle.I),
            native.atan(native_i),
            names,
        ),
        "domain_acosh_branch": (
            oracle.acosh(0),
            native.acosh(0),
            names,
        ),
        "domain_acosh_imaginary": (
            oracle.acosh(oracle.I),
            native.acosh(native_i),
            names,
        ),
        "domain_asin_imaginary": (
            oracle.asin(oracle.I),
            native.asin(native_i),
            names,
        ),
        "domain_acos_imaginary": (
            oracle.acos(oracle.I),
            native.acos(native_i),
            names,
        ),
        "domain_asin_special": (
            oracle.asin(oracle.Rational(1, 2)),
            native.asin(native.Rational(1, 2)),
            names,
        ),
        "domain_acos_special": (
            oracle.acos(oracle.Rational(1, 2)),
            native.acos(native.Rational(1, 2)),
            names,
        ),
        "domain_atan_special": (
            oracle.atan(oracle.sqrt(3)),
            native.atan(native.sqrt(3)),
            names,
        ),
        "domain_asinh_real": (
            oracle.asinh(1),
            native.asinh(1),
            names,
        ),
        "domain_sqrt_negative_square": (
            oracle.sqrt(-4),
            native.sqrt(-4),
            names,
        ),
        "domain_asinh_imaginary": (
            oracle.asinh(oracle.I),
            native.asinh(native_i),
            names,
        ),
        "domain_inverse": (
            inverse_domain_expression(oracle),
            inverse_domain_expression(native),
            names,
        ),
        "domain_reciprocal": (
            reciprocal_hyperbolic_domain_expression(oracle, suffix),
            reciprocal_hyperbolic_domain_expression(native, suffix),
            names,
        ),
        "domain_error_function": (
            error_function_domain_expression(oracle, suffix),
            error_function_domain_expression(native, suffix),
            names,
        ),
        "domain_gamma": (
            gamma_domain_expression(oracle, suffix),
            gamma_domain_expression(native, suffix),
            names,
        ),
        "domain_atan2": (
            atan2_domain_expression(oracle, suffix),
            atan2_domain_expression(native, suffix),
            names,
        ),
        "domain_bessel": (
            bessel_domain_expression(oracle, suffix),
            bessel_domain_expression(native, suffix),
            names,
        ),
        "domain_legendre": (
            legendre_domain_expression(oracle, suffix),
            legendre_domain_expression(native, suffix),
            names,
        ),
        "domain_complex": (
            complex_domain_expression(oracle, suffix),
            complex_domain_expression(native, suffix),
            names,
        ),
        "domain_abs": (
            complex_abs_expression(oracle, suffix),
            complex_abs_expression(native, suffix),
            names,
        ),
        "domain_expand_complex": (
            complex_expand_expression(oracle, suffix),
            complex_expand_expression(native, suffix),
            names,
        ),
        "domain_power": (
            oracle.Pow(-oracle.oo, oracle.Rational(3, 2), evaluate=False),
            (-native_oo)**native_three_halves,
            names,
        ),
        "domain_phase": (
            oracle.Pow(-oracle.oo, oracle.Rational(2, 3), evaluate=False),
            (-native_oo)**native_two_thirds,
            names,
        ),
        "relation": (
            oracle.Gt(oracle_x, 1),
            native.Gt(native_x, 1),
            names,
        ),
        "compound": (
            oracle.And(oracle.Gt(oracle_x, 1), oracle.Ne(oracle_x, 0)),
            native.And(native.Gt(native_x, 1), native.Ne(native_x, 0)),
            names,
        ),
        "factor": (
            oracle_x**2 + 2 * oracle_x + 1,
            native_x**2 + 2 * native_x + 1,
            names,
        ),
        "matrix_nullspace": (
            oracle.Matrix([[1, 2, 3], [2, 4, 4]]),
            native.Matrix([[1, 2, 3], [2, 4, 4]]),
            names,
        ),
        "matrix_rref": (
            oracle.Matrix([[1, 2, 3], [2, 4, 4]]),
            native.Matrix([[1, 2, 3], [2, 4, 4]]),
            names,
        ),
        "matrix_multiply": (
            oracle.Matrix([[1, 2], [3, 4]]),
            native.Matrix([[1, 2], [3, 4]]),
            names,
        ),
        "matrix_add": (
            oracle.Matrix([[1, 2], [3, 4]]),
            native.Matrix([[1, 2], [3, 4]]),
            names,
        ),
        "matrix_subtract": (
            oracle.Matrix([[1, 2], [3, 4]]),
            native.Matrix([[1, 2], [3, 4]]),
            names,
        ),
        "matrix_negate": (
            oracle.Matrix([[1, 2], [3, 4]]),
            native.Matrix([[1, 2], [3, 4]]),
            names,
        ),
        "matrix_divide": (
            oracle.Matrix([[1, 2], [3, 4]]),
            native.Matrix([[1, 2], [3, 4]]),
            names,
        ),
        "assumption_query": (
            oracle.Q.positive(oracle_x),
            native.Q.positive(native_x),
            names,
        ),
        "integer_assumption_query": (
            oracle.Q.integer(oracle_integer),
            native.Q.integer(native_integer),
            names,
        ),
        "rational_assumption_query": (
            oracle.Q.rational(oracle_rational),
            native.Q.rational(native_rational),
            names,
        ),
        "algebraic_assumption_query": (
            oracle.Q.algebraic(oracle.Integer(2)),
            native.Q.algebraic(native.Integer(2)),
            names,
        ),
        "number_predicate": (
            oracle.sin(1),
            native.sin(1),
            names,
        ),
        "algebraic_predicate": (
            oracle.sqrt(2),
            native.sqrt(2),
            names,
        ),
    }
    if has_composition:
        expressions["composition"] = (
            oracle.log(
                oracle.exp(oracle_composition, evaluate=False),
                evaluate=False,
            ),
            native.log(native.exp(native_composition)),
            names,
        )
    return expressions, names


def build_expression(engine: Any, operation: str, suffix: str) -> tuple[Any, Any]:
    if operation == "matrix_nullspace":
        return engine.Matrix([[1, 2, 3], [2, 4, 4]]), None
    if operation == "matrix_rref":
        return engine.Matrix([[1, 2, 3], [2, 4, 4]]), None
    if operation == "matrix_multiply":
        return engine.Matrix([[1, 2], [3, 4]]), None
    if operation in ("matrix_add", "matrix_subtract", "matrix_negate", "matrix_divide"):
        return engine.Matrix([[1, 2], [3, 4]]), None
    if operation == "composition":
        x = engine.Symbol(f"{operation}_x_{suffix}", real=True)
    elif operation == "integer_assumption_query":
        x = engine.Symbol(f"{operation}_x_{suffix}", integer=True)
    elif operation == "rational_assumption_query":
        x = engine.Symbol(f"{operation}_x_{suffix}", rational=True)
    elif operation == "algebraic_assumption_query":
        x = engine.Integer(2)
    elif operation in _CONSTRUCTION_OPERATIONS:
        x = engine.Symbol(f"{operation}_x_{suffix}")
    elif operation in ("number_predicate", "algebraic_predicate"):
        x = engine.Integer(1 if operation == "number_predicate" else 2)
    else:
        x = engine.Symbol(f"{operation}_x_{suffix}")
    variable = x
    if operation == "expand":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = (x + y + 1) ** 4
    elif operation == "count_ops":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = (x + y + 1) ** 2
    elif operation == "free_symbols":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = x + engine.sin(y * x)
    elif operation == "subs_simultaneous":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = x + 2*y
        variable = (x, y)
    elif operation == "subs_mapping":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = engine.Function("subs_mapping_f")(x**2 + x)
        variable = (x, y)
    elif operation == "xreplace":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = sum((x + index)**2 for index in range(8))
        variable = (x, y)
    elif operation == "replace":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = x + 1
        variable = (x, y)
    elif operation == "match":
        variable = engine.Function("match_f")(x**2 + x)
        expression = engine.Function("match_f")(x**2 + x)
    elif operation == "match_wild":
        expression = x + 1
        variable = engine.Wild(f"{operation}_a")
    elif operation == "match_wild_remainder":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = x + y + sum(
            engine.Symbol(f"{operation}_z_{index}_{suffix}")
            for index in range(24)
        )
        variable = x + engine.Wild(f"{operation}_a")
    elif operation == "match_wild_partition":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = x + y + 1
        variable = (engine.Wild(f"{operation}_a") +
                    engine.Wild(f"{operation}_b"))
    elif operation == "differentiate":
        y = engine.Symbol(f"{operation}_y_{suffix}")
        expression = engine.exp(x * y)
    elif operation == "simplify":
        expression = engine.sqrt(x**2)
    elif operation in _CONSTRUCTION_OPERATIONS:
        exponent = 0 if operation == "power_constructor" else 1
        expression = x**exponent
    elif operation == "domain_function":
        if engine is oracle:
            expression = engine.sqrt(-engine.oo, evaluate=False)
        else:
            expression = engine.sqrt(-engine._default().constant("oo"))
    elif operation == "domain_log_zero":
        expression = engine.log(0)
    elif operation == "domain_log_negative":
        expression = engine.log(-2)
    elif operation == "domain_log_imaginary":
        expression = engine.log(imaginary_unit(engine))
    elif operation == "domain_gamma_pole":
        expression = engine.gamma(0)
    elif operation == "domain_loggamma_pole":
        expression = engine.loggamma(0)
    elif operation == "domain_factorial_pole":
        expression = engine.factorial(-1)
    elif operation == "domain_factorial_value":
        expression = engine.factorial(5)
    elif operation == "domain_factorial_large":
        expression = engine.factorial(100)
    elif operation == "domain_atanh_pole":
        expression = engine.atanh(1)
    elif operation == "domain_atanh_imaginary":
        expression = engine.atanh(imaginary_unit(engine))
    elif operation == "domain_atan_imaginary":
        expression = engine.atan(imaginary_unit(engine))
    elif operation == "domain_acosh_branch":
        expression = engine.acosh(0)
    elif operation == "domain_acosh_imaginary":
        expression = engine.acosh(imaginary_unit(engine))
    elif operation == "domain_asin_imaginary":
        expression = engine.asin(imaginary_unit(engine))
    elif operation == "domain_acos_imaginary":
        expression = engine.acos(imaginary_unit(engine))
    elif operation == "domain_asin_special":
        expression = engine.asin(engine.Rational(1, 2))
    elif operation == "domain_acos_special":
        expression = engine.acos(engine.Rational(1, 2))
    elif operation == "domain_atan_special":
        expression = engine.atan(engine.sqrt(3))
    elif operation == "domain_asinh_real":
        expression = engine.asinh(1)
    elif operation == "domain_sqrt_negative_square":
        expression = engine.sqrt(-4)
    elif operation == "domain_asinh_imaginary":
        expression = engine.asinh(imaginary_unit(engine))
    elif operation == "domain_inverse":
        expression = inverse_domain_expression(engine)
    elif operation == "domain_reciprocal":
        expression = reciprocal_hyperbolic_domain_expression(engine, suffix)
    elif operation == "domain_error_function":
        expression = error_function_domain_expression(engine, suffix)
    elif operation == "domain_gamma":
        expression = gamma_domain_expression(engine, suffix)
    elif operation == "domain_atan2":
        expression = atan2_domain_expression(engine, suffix)
    elif operation == "domain_bessel":
        expression = bessel_domain_expression(engine, suffix)
    elif operation == "domain_legendre":
        expression = legendre_domain_expression(engine, suffix)
    elif operation == "domain_complex":
        expression = complex_domain_expression(engine, suffix)
    elif operation == "domain_abs":
        expression = complex_abs_expression(engine, suffix)
    elif operation == "domain_expand_complex":
        expression = complex_expand_expression(engine, suffix)
    elif operation == "domain_power":
        if engine is oracle:
            expression = engine.Pow(
                -engine.oo, engine.Rational(3, 2), evaluate=False
            )
        else:
            infinity = engine._default().constant("oo")
            expression = (-infinity)**engine.Rational(3, 2)
    elif operation == "domain_phase":
        if engine is oracle:
            expression = engine.Pow(
                -engine.oo, engine.Rational(2, 3), evaluate=False
            )
        else:
            infinity = engine._default().constant("oo")
            expression = (-infinity)**engine.Rational(2, 3)
    elif operation == "factor":
        expression = x**2 + 2 * x + 1
    elif operation == "assumption_query":
        expression = engine.Q.positive(x)
    elif operation == "integer_assumption_query":
        expression = engine.Q.integer(x)
    elif operation == "rational_assumption_query":
        expression = engine.Q.rational(x)
    elif operation == "algebraic_assumption_query":
        expression = engine.Q.algebraic(x)
    elif operation == "number_predicate":
        expression = engine.sin(x)
    elif operation == "algebraic_predicate":
        expression = engine.sqrt(x)
    elif operation == "refine":
        expression = engine.sqrt(x**2)
    elif operation == "composition":
        if engine is oracle:
            expression = engine.log(
                engine.exp(x, evaluate=False), evaluate=False
            )
        else:
            expression = engine.log(engine.exp(x))
    elif operation == "sqrt_power":
        if engine is oracle:
            expression = engine.Pow(
                engine.sqrt(x, evaluate=False), 2, evaluate=False
            )
        else:
            expression = engine.sqrt(x)**2
    elif operation == "relation":
        expression = engine.Gt(x, 1)
    elif operation == "compound":
        expression = engine.And(engine.Gt(x, 1), engine.Ne(x, 0))
    else:
        raise ValueError(f"unknown benchmark operation: {operation}")
    return expression, variable


def correctness_cases() -> list[dict[str, Any]]:
    oracle_cases = workload_factories("check", "fixed")[0]
    native_cases = workload_factories("check", "fixed")[0]
    results = []
    for name in ("sin", "cos", "tan"):
        expected = getattr(oracle, name)(oracle.zoo)
        actual = getattr(native, name)(native.zoo)
        results.append({
            "operation": f"domain_periodic_{name}",
            "correct": str(expected) == str(actual),
            "expected": result_text(expected),
            "actual": str(actual),
        })
    oracle_order = oracle.Symbol("benchmark_nan_order")
    native_order = native.Symbol("benchmark_nan_order")
    oracle_argument = oracle.Symbol("benchmark_nan_argument")
    native_argument = native.Symbol("benchmark_nan_argument")
    special_cases = {
        "besselj_nan_order": (
            oracle.besselj(oracle.nan, oracle_argument),
            native.besselj(native.nan, native_argument),
        ),
        "besseli_nan_argument": (
            oracle.besseli(oracle_order, oracle.nan),
            native.besseli(native_order, native.nan),
        ),
        "legendre_nan_degree": (
            oracle.legendre(oracle.nan, oracle_argument),
            native.legendre(native.nan, native_argument),
        ),
        "besseli_nan_order_negative_infinity": (
            oracle.besseli(oracle.nan, -oracle.oo),
            native.besseli(native.nan, -native.oo),
        ),
        "legendre_high_degree_infinity": (
            oracle.legendre(3, oracle.oo),
            native.legendre(3, native.oo),
        ),
        "legendre_negative_one": (
            oracle.legendre(-1, oracle.oo),
            native.legendre(-1, native.oo),
        ),
        "legendre_negative_two": (
            oracle.legendre(-2, -oracle.oo),
            native.legendre(-2, -native.oo),
        ),
        "legendre_negative_four": (
            oracle.legendre(-4, oracle.zoo),
            native.legendre(-4, native.zoo),
        ),
        "besseli_symbolic_negative_infinity": (
            oracle.besseli(oracle_order, -oracle.oo),
            native.besseli(native_order, -native.oo),
        ),
        "besseli_integer_negative_infinity": (
            oracle.besseli(1, -oracle.oo),
            native.besseli(1, -native.oo),
        ),
        "negative_oo_one_third_phase": (
            oracle.Pow(-oracle.oo, oracle.Rational(1, 3)),
            native.simplify((-native.oo)**native.Rational(1, 3)),
        ),
        "negative_oo_four_thirds_phase": (
            oracle.Pow(-oracle.oo, oracle.Rational(4, 3)),
            native.simplify((-native.oo)**native.Rational(4, 3)),
        ),
        "negative_oo_negative_rational_power": (
            oracle.Pow(-oracle.oo, oracle.Rational(-1, 3)),
            native.simplify((-native.oo)**native.Rational(-1, 3)),
        ),
    }
    names = {
        "benchmark_nan_order": oracle_order,
        "benchmark_nan_argument": oracle_argument,
    }
    for name, (expected, actual) in special_cases.items():
        actual_text = str(actual).replace("legendrep(", "legendre(")
        actual_text = actual_text.replace(
            "legendre(nan, 0, ", "legendre(nan, "
        )
        results.append({
            "operation": f"nan_special_{name}",
            "correct": oracle.sympify(
                actual_text, locals=names
            ) == expected,
            "expected": result_text(expected),
            "actual": actual_text,
        })
    sentinel_values = (
        ("oo", oracle.oo, native.oo),
        ("negative_oo", -oracle.oo, -native.oo),
        ("zoo", oracle.zoo, native.zoo),
        ("nan", oracle.nan, native.nan),
    )
    sentinel_functions = (
        ("re", oracle.re, native.re),
        ("im", oracle.im, native.im),
        ("abs", oracle.Abs, native.Abs),
        ("arg", oracle.arg, native.arg),
        ("conjugate", oracle.conjugate, native.conjugate),
        ("expand_complex", oracle.expand_complex, native.expand_complex),
    )
    for function_name, oracle_function, native_function in sentinel_functions:
        for value_name, oracle_value, native_value in sentinel_values:
            expected = oracle_function(oracle_value)
            actual = native_function(native_value)
            results.append({
                "operation": f"complex_sentinel_{function_name}_{value_name}",
                "correct": structurally_equivalent(expected, actual, {}),
                "expected": result_text(expected),
                "actual": str(actual),
            })
    for operation in sorted(oracle_cases):
        oracle_expression, native_expression, names = oracle_cases[operation]
        _, native_expression, _ = native_cases[operation]
        if operation == "expand":
            expected = oracle.expand(oracle_expression)
            actual = native.expand(native_expression)
        elif operation == "count_ops":
            expected = oracle.count_ops(oracle_expression)
            actual = native.count_ops(native_expression)
        elif operation == "free_symbols":
            expected = free_symbol_names(oracle_expression, oracle)
            actual = free_symbol_names(native_expression, native)
        elif operation == "subs_simultaneous":
            oracle_x = names["check_x_fixed"]
            oracle_y = names["check_y_fixed"]
            expected = oracle_expression.subs(
                {oracle_x: oracle_y, oracle_y: oracle_x},
                simultaneous=True,
            )
            native_x = native.Symbol("check_x_fixed")
            native_y = native.Symbol("check_y_fixed")
            actual = native.subs(
                native_expression,
                {native_x: native_y, native_y: native_x},
                simultaneous=True,
            )
        elif operation == "subs_mapping":
            oracle_x = names["check_x_fixed"]
            oracle_y = names["check_y_fixed"]
            expected = oracle_expression.subs({
                oracle_x: oracle_y, oracle_x**2: 2,
            })
            native_x = native.Symbol("check_x_fixed")
            native_y = native.Symbol("check_y_fixed")
            actual = native.subs(native_expression, {
                native_x: native_y, native_x**2: 2,
            })
        elif operation == "xreplace":
            oracle_x = names["check_x_fixed"]
            oracle_y = names["check_y_fixed"]
            expected = oracle_expression.xreplace({oracle_x: oracle_y})
            native_x = native.Symbol("check_x_fixed")
            native_y = native.Symbol("check_y_fixed")
            actual = native_expression.xreplace({native_x: native_y})
        elif operation == "replace":
            oracle_x = names["check_x_fixed"]
            oracle_y = names["check_y_fixed"]
            expected = oracle_expression.replace(oracle_x, oracle_y)
            native_x = native.Symbol("check_x_fixed")
            native_y = native.Symbol("check_y_fixed")
            actual = native_expression.replace(native_x, native_y)
        elif operation == "match":
            oracle_x = names["check_x_fixed"]
            native_x = native.Symbol("check_x_fixed")
            oracle_pattern = oracle.Function("match_f")(oracle_x**2 + oracle_x)
            native_pattern = native.Function("match_f")(native_x**2 + native_x)
            expected = oracle_expression.match(oracle_pattern)
            actual = native_expression.match(native_pattern)
        elif operation == "match_wild":
            oracle_pattern = oracle.Wild("match_wild_a") + 1
            native_pattern = native.Wild("match_wild_a") + 1
            expected = oracle_expression.match(oracle_pattern)
            actual = native_expression.match(native_pattern)
        elif operation == "match_wild_remainder":
            oracle_x = names["check_x_fixed"]
            native_x = native.Symbol("check_x_fixed")
            oracle_pattern = oracle_x + oracle.Wild("match_wild_remainder_a")
            native_pattern = native_x + native.Wild("match_wild_remainder_a")
            expected = oracle_expression.match(oracle_pattern)
            actual = native_expression.match(native_pattern)
        elif operation == "match_wild_partition":
            oracle_pattern = (oracle.Wild("match_wild_partition_a") +
                              oracle.Wild("match_wild_partition_b"))
            native_pattern = (native.Wild("match_wild_partition_a") +
                              native.Wild("match_wild_partition_b"))
            expected = oracle_expression.match(oracle_pattern)
            actual = native_expression.match(native_pattern)
        elif operation == "differentiate":
            expected = oracle.diff(oracle_expression, names[f"check_x_fixed"])
            actual = native.diff(native_expression, native.Symbol("check_x_fixed"))
        elif operation == "simplify":
            expected = oracle.simplify(oracle_expression)
            actual = native.simplify(native_expression)
        elif operation in _CONSTRUCTION_OPERATIONS:
            expected = oracle_expression
            actual = native_expression
        elif operation == "domain_complex":
            expected = oracle.re(oracle_expression)
            actual = native.re(native_expression)
        elif operation == "domain_abs":
            expected = oracle.Abs(oracle_expression)
            actual = native.Abs(native_expression)
        elif operation == "domain_expand_complex":
            expected = oracle.expand_complex(oracle_expression)
            actual = native.expand_complex(native_expression)
        elif operation in (
                "composition", "sqrt_power",
                "domain_function", "domain_log_zero", "domain_log_negative",
                "domain_log_imaginary", "domain_gamma_pole",
                "domain_loggamma_pole", "domain_factorial_pole",
                "domain_factorial_value",
                "domain_factorial_large",
                "domain_atanh_pole",
                "domain_atanh_imaginary",
                "domain_atan_imaginary",
                "domain_acosh_branch", "domain_acosh_imaginary",
                "domain_asin_imaginary", "domain_acos_imaginary",
                "domain_asin_special", "domain_acos_special",
                "domain_atan_special", "domain_sqrt_negative_square",
                "domain_asinh_real",
                "domain_asinh_imaginary",
                "domain_inverse",
                "domain_reciprocal", "domain_error_function", "domain_gamma",
                "domain_atan2",
                "domain_bessel", "domain_legendre",
                "domain_power", "domain_phase"):
            expected = oracle.simplify(oracle_expression)
            actual = native.simplify(native_expression)
        elif operation in _ASSUMPTION_OPERATIONS:
            expected = oracle.ask(oracle_expression)
            actual = native.ask(native_expression)
        elif operation in _PREDICATE_OPERATIONS:
            expected = predicate_value(oracle_expression, operation)
            actual = predicate_value(native_expression, operation)
        elif operation == "refine":
            expected = oracle.refine(
                oracle_expression, oracle.Q.negative(names["check_x_fixed"])
            )
            actual = native.refine(
                native_expression, native.Q.negative(native.Symbol("check_x_fixed"))
            )
        elif operation == "relation":
            expected = oracle.Gt(names["check_x_fixed"], 1)
            actual = native.Gt(native.Symbol("check_x_fixed"), 1)
        elif operation == "compound":
            expected = oracle.And(
                oracle.Gt(names["check_x_fixed"], 1),
                oracle.Ne(names["check_x_fixed"], 0),
            )
            actual = native.And(
                native.Gt(native.Symbol("check_x_fixed"), 1),
                native.Ne(native.Symbol("check_x_fixed"), 0),
            )
        elif operation == "matrix_nullspace":
            expected = oracle_expression.nullspace()
            actual = native_expression.nullspace()
        elif operation == "matrix_rref":
            expected = oracle_expression.rref()
            actual = native_expression.rref()
        elif operation == "matrix_multiply":
            expected = oracle_expression * oracle.Matrix([[2, 0], [1, 2]])
            actual = native_expression * native.Matrix([[2, 0], [1, 2]])
        elif operation == "matrix_add":
            expected = oracle_expression + oracle.Matrix([[2, 0], [1, 2]])
            actual = native_expression + native.Matrix([[2, 0], [1, 2]])
        elif operation == "matrix_subtract":
            expected = oracle_expression - oracle.Matrix([[2, 0], [1, 2]])
            actual = native_expression - native.Matrix([[2, 0], [1, 2]])
        elif operation == "matrix_negate":
            expected = -oracle_expression
            actual = -native_expression
        elif operation == "matrix_divide":
            expected = oracle_expression / 2
            actual = native_expression / 2
        else:
            expected = oracle.factor(oracle_expression)
            actual = native.factor(native_expression)
        actual_text = str(actual)
        results.append({
            "operation": operation,
            "correct": (
                match_equivalent(expected, actual)
                if operation in ("match", "match_wild", "match_wild_remainder",
                                 "match_wild_partition")
                else expected == actual
                if (operation in ("count_ops", "free_symbols") or
                        operation in _ASSUMPTION_OPERATIONS or
                        operation in _PREDICATE_OPERATIONS)
                else nullspace_equivalent(expected, actual)
                if operation == "matrix_nullspace"
                else rref_equivalent(expected, actual)
                if operation == "matrix_rref"
                else matrix_multiply_equivalent(expected, actual)
                if operation == "matrix_multiply"
                else matrix_elementwise_equivalent(expected, actual)
                if operation in ("matrix_add", "matrix_subtract", "matrix_negate", "matrix_divide")
                else str(expected) == str(actual)
                if operation == "relation"
                else compound_equivalent(expected, actual)
                if operation == "compound"
                else structurally_equivalent(expected, actual, names)
                if operation in (
                        "domain_function", "domain_log_zero",
                        "domain_log_negative", "domain_log_imaginary",
                        "domain_gamma_pole", "domain_loggamma_pole",
                        "domain_factorial_pole", "domain_factorial_value",
                        "domain_factorial_large",
                        "domain_atanh_pole",
                        "domain_atanh_imaginary",
                        "domain_atan_imaginary",
                        "domain_acosh_branch", "domain_acosh_imaginary",
                        "domain_asin_imaginary", "domain_acos_imaginary",
                        "domain_asin_special", "domain_acos_special",
                        "domain_atan_special", "domain_sqrt_negative_square",
                        "domain_asinh_real",
                        "domain_asinh_imaginary",
                        "domain_inverse",
                        "domain_reciprocal",
                        "domain_error_function", "domain_gamma", "domain_atan2",
                        "domain_bessel", "domain_legendre", "domain_complex",
                        "domain_abs",
                        "domain_power", "domain_phase")
                else equivalent(expected, actual, names)
            ),
            "expected": result_text(expected),
            "actual": actual_text,
        })
    return results


def benchmark_workload(
    operation: str,
    scope: str,
    warmup: int,
    repetitions: int,
    batch: int,
) -> dict[str, Any]:
    reset_native_default_arena()
    if scope == "warm_core":
        expressions, _ = workload_factories(operation, "warm")
        oracle_expression, native_expression, _ = expressions[operation]
        if operation == "matrix_nullspace":
            oracle_call = lambda: oracle_expression.nullspace()
            native_call = lambda: native_expression.nullspace()
        elif operation == "matrix_rref":
            oracle_call = lambda: oracle_expression.rref()
            native_call = lambda: native_expression.rref()
        elif operation == "matrix_multiply":
            oracle_right = oracle.Matrix([[2, 0], [1, 2]])
            native_right = native.Matrix([[2, 0], [1, 2]])
            oracle_call = lambda: oracle_expression * oracle_right
            native_call = lambda: native_expression * native_right
        elif operation == "matrix_add":
            oracle_right = oracle.Matrix([[2, 0], [1, 2]])
            native_right = native.Matrix([[2, 0], [1, 2]])
            oracle_call = lambda: oracle_expression + oracle_right
            native_call = lambda: native_expression + native_right
        elif operation == "matrix_subtract":
            oracle_right = oracle.Matrix([[2, 0], [1, 2]])
            native_right = native.Matrix([[2, 0], [1, 2]])
            oracle_call = lambda: oracle_expression - oracle_right
            native_call = lambda: native_expression - native_right
        elif operation == "matrix_negate":
            oracle_call = lambda: -oracle_expression
            native_call = lambda: -native_expression
        elif operation == "matrix_divide":
            oracle_call = lambda: oracle_expression / 2
            native_call = lambda: native_expression / 2
        elif operation == "differentiate":
            oracle_x = oracle.Symbol(f"{operation}_x_warm")
            native_x = native.Symbol(f"{operation}_x_warm")
            oracle_call = lambda: oracle.diff(oracle_expression, oracle_x)
            native_call = lambda: native.diff(native_expression, native_x)
        else:
            if operation == "expand":
                oracle_call = lambda: oracle.expand(oracle_expression)
                native_call = lambda: native.expand(native_expression)
            elif operation == "count_ops":
                oracle_call = lambda: oracle.count_ops(oracle_expression)
                native_call = lambda: native.count_ops(native_expression)
            elif operation == "free_symbols":
                oracle_call = lambda: oracle_expression.free_symbols
                native_call = lambda: native_expression.free_symbols
            elif operation == "subs_simultaneous":
                oracle_x = oracle.Symbol("subs_simultaneous_x_warm")
                oracle_y = oracle.Symbol("subs_simultaneous_y_warm")
                native_x = native.Symbol("subs_simultaneous_x_warm")
                native_y = native.Symbol("subs_simultaneous_y_warm")
                oracle_call = lambda: oracle_expression.subs(
                    {oracle_x: oracle_y, oracle_y: oracle_x},
                    simultaneous=True,
                )
                native_call = lambda: native.subs(
                    native_expression,
                    {native_x: native_y, native_y: native_x},
                    simultaneous=True,
                )
            elif operation == "subs_mapping":
                oracle_x = oracle.Symbol("subs_mapping_x_warm")
                oracle_y = oracle.Symbol("subs_mapping_y_warm")
                native_x = native.Symbol("subs_mapping_x_warm")
                native_y = native.Symbol("subs_mapping_y_warm")
                oracle_mapping = {
                    oracle_x: oracle_y, oracle_x**2: 2,
                }
                native_mapping = {
                    native_x: native_y, native_x**2: 2,
                }
                oracle_call = lambda: oracle_expression.subs(oracle_mapping)
                native_call = lambda: native.subs(
                    native_expression, native_mapping
                )
            elif operation == "xreplace":
                oracle_x = oracle.Symbol("xreplace_x_warm")
                oracle_y = oracle.Symbol("xreplace_y_warm")
                native_x = native.Symbol("xreplace_x_warm")
                native_y = native.Symbol("xreplace_y_warm")
                oracle_mapping = {oracle_x: oracle_y}
                native_mapping = {native_x: native_y}
                oracle_call = lambda: oracle_expression.xreplace(
                    oracle_mapping
                )
                native_call = lambda: native_expression.xreplace(
                    native_mapping
                )
            elif operation == "replace":
                oracle_x = oracle.Symbol("replace_x_warm")
                oracle_y = oracle.Symbol("replace_y_warm")
                native_x = native.Symbol("replace_x_warm")
                native_y = native.Symbol("replace_y_warm")
                oracle_call = lambda: oracle_expression.replace(
                    oracle_x, oracle_y
                )
                native_call = lambda: native_expression.replace(
                    native_x, native_y
                )
            elif operation == "match":
                oracle_pattern = oracle_expression
                native_pattern = native_expression
                oracle_call = lambda: oracle_expression.match(oracle_pattern)
                native_call = lambda: native_expression.match(native_pattern)
            elif operation == "match_wild":
                oracle_pattern = oracle.Wild("match_wild_a")
                native_pattern = native.Wild("match_wild_a")
                oracle_call = lambda: oracle_expression.match(oracle_pattern)
                native_call = lambda: native_expression.match(native_pattern)
            elif operation == "match_wild_remainder":
                oracle_x = oracle.Symbol("match_wild_remainder_x_warm")
                native_x = native.Symbol("match_wild_remainder_x_warm")
                oracle_pattern = oracle_x + oracle.Wild(
                    "match_wild_remainder_a"
                )
                native_pattern = native_x + native.Wild(
                    "match_wild_remainder_a"
                )
                oracle_call = lambda: oracle_expression.match(oracle_pattern)
                native_call = lambda: native_expression.match(native_pattern)
            elif operation == "match_wild_partition":
                oracle_pattern = (oracle.Wild("match_wild_partition_a") +
                                  oracle.Wild("match_wild_partition_b"))
                native_pattern = (native.Wild("match_wild_partition_a") +
                                  native.Wild("match_wild_partition_b"))
                oracle_call = lambda: oracle_expression.match(oracle_pattern)
                native_call = lambda: native_expression.match(native_pattern)
            elif operation == "simplify":
                oracle_call = lambda: oracle.simplify(oracle_expression)
                native_call = lambda: native.simplify(native_expression)
            elif operation == "factor":
                oracle_call = lambda: oracle.factor(oracle_expression)
                native_call = lambda: native.factor(native_expression)
            elif operation in _ASSUMPTION_OPERATIONS:
                oracle_call = lambda: oracle.ask(oracle_expression)
                native_call = lambda: native.ask(native_expression)
            elif operation in _PREDICATE_OPERATIONS:
                oracle_call = lambda: predicate_value(oracle_expression, operation)
                native_call = lambda: predicate_value(native_expression, operation)
            elif operation == "refine":
                oracle_x = oracle.Symbol("refine_x_warm")
                native_x = native.Symbol("refine_x_warm")
                oracle_call = lambda: oracle.refine(
                    oracle_expression, oracle.Q.negative(oracle_x)
                )
                native_call = lambda: native.refine(
                    native_expression, native.Q.negative(native_x)
                )
            elif operation == "domain_complex":
                oracle_call = lambda: oracle.re(oracle_expression)
                native_call = lambda: native.re(native_expression)
            elif operation == "domain_abs":
                oracle_call = lambda: oracle.Abs(oracle_expression)
                native_call = lambda: native.Abs(native_expression)
            elif operation == "domain_expand_complex":
                oracle_call = lambda: oracle.expand_complex(oracle_expression)
                native_call = lambda: native.expand_complex(native_expression)
            elif operation in (
                    "composition", "sqrt_power", "domain_function",
                    "domain_log_zero", "domain_log_negative",
                    "domain_log_imaginary", "domain_gamma_pole",
                    "domain_loggamma_pole", "domain_factorial_pole",
                    "domain_factorial_value",
                    "domain_factorial_large",
                    "domain_atanh_pole", "domain_atanh_imaginary",
                    "domain_atan_imaginary",
                    "domain_acosh_branch", "domain_acosh_imaginary",
                    "domain_asin_imaginary", "domain_acos_imaginary",
                    "domain_asin_special", "domain_acos_special",
                    "domain_atan_special", "domain_sqrt_negative_square",
                    "domain_asinh_real",
                    "domain_asinh_imaginary",
                    "domain_inverse",
                    "domain_reciprocal", "domain_error_function", "domain_gamma",
                    "domain_atan2",
                    "domain_bessel", "domain_legendre",
                    "domain_power", "domain_phase"):
                oracle_call = lambda: oracle.simplify(oracle_expression)
                native_call = lambda: native.simplify(native_expression)
            elif operation == "relation":
                oracle_x = oracle.Symbol("relation_x_warm")
                native_x = native.Symbol("relation_x_warm")
                oracle_call = lambda: oracle.Gt(oracle_x, 1)
                native_call = lambda: native.Gt(native_x, 1)
            elif operation == "compound":
                oracle_x = oracle.Symbol("compound_x_warm")
                native_x = native.Symbol("compound_x_warm")
                oracle_call = lambda: oracle.And(
                    oracle.Gt(oracle_x, 1), oracle.Ne(oracle_x, 0)
                )
                native_call = lambda: native.And(
                    native.Gt(native_x, 1), native.Ne(native_x, 0)
                )
    else:
        def make_call(engine: Any) -> Callable[[], Any]:
            counter = {"value": 0}

            def call():
                suffix = str(counter["value"])
                counter["value"] += 1
                expression, variable = build_expression(engine, operation, suffix)
                if operation == "matrix_nullspace":
                    return expression.nullspace()
                if operation == "matrix_rref":
                    return expression.rref()
                if operation == "matrix_multiply":
                    return expression * engine.Matrix([[2, 0], [1, 2]])
                if operation == "matrix_add":
                    return expression + engine.Matrix([[2, 0], [1, 2]])
                if operation == "matrix_subtract":
                    return expression - engine.Matrix([[2, 0], [1, 2]])
                if operation == "matrix_negate":
                    return -expression
                if operation == "matrix_divide":
                    return expression / 2
                if operation == "differentiate":
                    return engine.diff(expression, variable)
                if operation in _ASSUMPTION_OPERATIONS:
                    return engine.ask(expression)
                if operation == "count_ops":
                    return engine.count_ops(expression)
                if operation == "free_symbols":
                    return expression.free_symbols
                if operation == "subs_simultaneous":
                    old, replacement = variable
                    substitutions = {old: replacement, replacement: old}
                    if engine is oracle:
                        return expression.subs(substitutions, simultaneous=True)
                    return native.subs(
                        expression, substitutions, simultaneous=True
                    )
                if operation == "subs_mapping":
                    old, replacement = variable
                    substitutions = {old: replacement, old**2: 2}
                    if engine is oracle:
                        return expression.subs(substitutions)
                    return native.subs(expression, substitutions)
                if operation == "xreplace":
                    old, replacement = variable
                    substitutions = {old: replacement}
                    return expression.xreplace(substitutions)
                if operation == "replace":
                    old, replacement = variable
                    return expression.replace(old, replacement)
                if operation == "match":
                    return expression.match(variable)
                if operation in ("match_wild", "match_wild_remainder",
                                 "match_wild_partition"):
                    return expression.match(variable)
                if operation in _PREDICATE_OPERATIONS:
                    return predicate_value(expression, operation)
                if operation in _CONSTRUCTION_OPERATIONS:
                    return expression
                if operation == "refine":
                    return engine.refine(expression, engine.Q.negative(variable))
                if operation == "domain_complex":
                    return engine.re(expression)
                if operation == "domain_abs":
                    return engine.Abs(expression)
                if operation == "domain_expand_complex":
                    return engine.expand_complex(expression)
                if operation in (
                    "composition", "sqrt_power",
                    "domain_function", "domain_log_zero", "domain_log_negative",
                    "domain_log_imaginary", "domain_gamma_pole",
                    "domain_loggamma_pole", "domain_factorial_pole",
                    "domain_factorial_value",
                    "domain_factorial_large",
                    "domain_atanh_pole", "domain_atanh_imaginary",
                    "domain_atan_imaginary",
                    "domain_acosh_branch", "domain_acosh_imaginary",
                    "domain_asin_imaginary", "domain_acos_imaginary",
                    "domain_asin_special", "domain_acos_special",
                    "domain_atan_special", "domain_sqrt_negative_square",
                    "domain_asinh_real",
                    "domain_asinh_imaginary",
                    "domain_inverse",
                        "domain_reciprocal", "domain_error_function", "domain_gamma",
                        "domain_atan2",
                        "domain_bessel", "domain_legendre",
                        "domain_power", "domain_phase"):
                    return engine.simplify(expression)
                if operation == "relation":
                    return engine.Gt(variable, 1)
                if operation == "compound":
                    return engine.And(engine.Gt(variable, 1), engine.Ne(variable, 0))
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
    reset_native_default_arena()

    workloads = []
    for operation in (
        "expand", "count_ops", "free_symbols", "subs_simultaneous", "subs_mapping", "xreplace", "replace", "match", "match_wild", "match_wild_remainder", "match_wild_partition", "differentiate", "simplify", "refine", "composition", "sqrt_power", "power_constructor", "power_one_constructor", "domain_function", "domain_log_zero", "domain_log_negative", "domain_log_imaginary", "domain_gamma_pole", "domain_loggamma_pole", "domain_factorial_pole", "domain_factorial_value", "domain_factorial_large", "domain_atanh_pole", "domain_atanh_imaginary", "domain_atan_imaginary", "domain_acosh_branch", "domain_acosh_imaginary", "domain_asin_imaginary", "domain_acos_imaginary", "domain_asin_special", "domain_acos_special", "domain_atan_special", "domain_asinh_real", "domain_sqrt_negative_square", "domain_asinh_imaginary", "domain_inverse", "domain_reciprocal", "domain_error_function", "domain_gamma", "domain_atan2", "domain_bessel", "domain_legendre", "domain_complex", "domain_abs", "domain_expand_complex", "domain_power", "domain_phase", "relation", "compound", "factor", "matrix_nullspace", "matrix_rref", "matrix_multiply", "matrix_add", "matrix_subtract", "matrix_negate",
        "matrix_divide", *_ASSUMPTION_OPERATIONS, *_PREDICATE_OPERATIONS
    ):
        if operation in _PREDICATE_OPERATIONS:
            scopes = ("warm_core",)
        elif operation in _CONSTRUCTION_OPERATIONS:
            scopes = ("cold_end_to_end",)
        else:
            scopes = ("cold_end_to_end", "warm_core")
        for scope in scopes:
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
