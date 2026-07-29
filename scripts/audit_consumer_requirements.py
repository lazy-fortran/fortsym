#!/usr/bin/env python3
"""Build a privacy-safe inventory of Mathematica/Wolfram consumer operations.

Source is read transiently.  Output contains aggregate counts only: no
repository names other than the explicitly authorized FortNum and MHD1D
consumers, no paths, no snippets, no symbols, and no source hashes.
"""

from __future__ import annotations

import argparse
import concurrent.futures
from dataclasses import dataclass, field
import json
import math
import os
from pathlib import Path
import re
import selectors
import subprocess
import sys
import tempfile
import threading
import time
from typing import Any, Callable
from urllib.parse import quote


OPERATION_NAMES = {
    "arbitrary_precision": [
        "Accuracy",
        "Precision",
        "SetPrecision",
        "SetAccuracy",
        "Rationalize",
    ],
    "numeric_evaluation": ["N"],
    "complex_algebraic": [
        "ComplexExpand",
        "Root",
        "RootApproximant",
        "RootIntervals",
        "RootReduce",
        "ToRadicals",
    ],
    "differentiate": [
        "D",
        "Dt",
        "Derivative",
        "Grad",
        "Div",
        "Curl",
        "Laplacian",
    ],
    "simplify": [
        "Simplify",
        "FullSimplify",
        "Refine",
        "FunctionExpand",
    ],
    "expand_collect": [
        "Expand",
        "ExpandAll",
        "Collect",
        "Coefficient",
        "CoefficientList",
        "Exponent",
        "Variables",
    ],
    "polynomial_rational": [
        "PolynomialGCD",
        "PolynomialLCM",
        "PolynomialReduce",
        "PolynomialMod",
        "PolynomialQuotient",
        "PolynomialQuotientRemainder",
        "PolynomialRemainder",
        "Factor",
        "FactorList",
        "FactorSquareFree",
        "FactorSquareFreeList",
        "FactorTerms",
        "Numerator",
        "Denominator",
        "Cancel",
        "Together",
        "Apart",
        "Resultant",
        "GroebnerBasis",
        "SquareFreeQ",
        "InterpolatingPolynomial",
    ],
    "integrate": ["Integrate"],
    "numeric_integrate": ["NIntegrate"],
    "limits_series_asymptotics": [
        "Limit",
        "Series",
        "SeriesCoefficient",
        "InverseSeries",
        "PadeApproximant",
        "Asymptotic",
        "AsymptoticDSolveValue",
        "Residue",
    ],
    "solve_reduce": [
        "Solve",
        "SolveValues",
        "Reduce",
        "Resolve",
        "Eliminate",
    ],
    "numeric_solve": ["NSolve", "FindRoot"],
    "assumptions_inequalities": [
        "Assuming",
        "Refine",
        "Reduce",
        "Resolve",
        "ForAll",
        "Exists",
        "Inequality",
        "Element",
    ],
    "piecewise_conditions": [
        "Piecewise",
        "PiecewiseExpand",
        "ConditionalExpression",
        "Boole",
    ],
    "exact_linear_algebra": [
        "Det",
        "Inverse",
        "LinearSolve",
        "RowReduce",
        "MatrixRank",
        "NullSpace",
        "CharacteristicPolynomial",
        "MinimalPolynomial",
        "Eigenvalues",
        "Eigenvectors",
        "LUDecomposition",
        "QRDecomposition",
        "SchurDecomposition",
        "SingularValueDecomposition",
    ],
    "matrices_tensors": [
        "TensorReduce",
        "TensorContract",
        "TensorExpand",
        "KroneckerProduct",
        "ArrayReshape",
        "ArrayFlatten",
        "Dot",
        "Outer",
        "Transpose",
        "Tr",
        "MatrixPower",
        "MatrixExp",
        "MatrixLog",
    ],
    "sums_products": [
        "Sum",
        "Product",
        "RSolve",
        "GeneratingFunction",
        "FindSequenceFunction",
    ],
    "trigonometric": [
        "TrigExpand",
        "TrigFactor",
        "TrigReduce",
        "TrigToExp",
        "ExpToTrig",
        "ComplexExpand",
    ],
    "safe_log_power_radical": [
        "PowerExpand",
        "Sqrt",
        "Surd",
        "Log",
        "Exp",
    ],
    "fourier_laplace_transforms": [
        "Fourier",
        "InverseFourier",
        "FourierTransform",
        "InverseFourierTransform",
        "LaplaceTransform",
        "InverseLaplaceTransform",
        "HankelTransform",
        "ZTransform",
        "InverseZTransform",
    ],
    "special_functions": [
        "BesselI",
        "BesselJ",
        "BesselK",
        "BesselY",
        "AiryAi",
        "AiryAiPrime",
        "AiryBi",
        "AiryBiPrime",
        "Hypergeometric0F1",
        "Hypergeometric1F1",
        "Hypergeometric2F1",
        "HypergeometricPFQ",
        "HypergeometricU",
        "MeijerG",
        "EllipticE",
        "EllipticF",
        "EllipticK",
        "EllipticPi",
        "EllipticTheta",
        "WeierstrassP",
        "WeierstrassZeta",
        "Gamma",
        "Beta",
        "PolyGamma",
        "PolyLog",
        "Erf",
        "Erfc",
        "Erfi",
        "DawsonF",
        "FresnelC",
        "FresnelS",
        "ExpIntegralE",
        "ExpIntegralEi",
        "SinIntegral",
        "CosIntegral",
        "LogIntegral",
        "ChebyshevT",
        "ChebyshevU",
        "GegenbauerC",
        "HermiteH",
        "JacobiP",
        "LaguerreL",
        "LegendreP",
        "LegendreQ",
        "MathieuC",
        "MathieuS",
        "SpheroidalPS",
        "SpheroidalQS",
        "HeunG",
        "SphericalHarmonicY",
        "StruveH",
        "StruveL",
        "Zeta",
    ],
    "parse_print_codegen": [
        "FortranForm",
        "CForm",
        "InputForm",
        "FullForm",
        "ToExpression",
        "HoldComplete",
        "ExportString",
        "Compile",
    ],
    "differential_equations": ["DSolve"],
    "numeric_differential_equations": ["NDSolve"],
}

OPTION_NAMES = {
    "arbitrary_precision": ["WorkingPrecision", "AccuracyGoal", "PrecisionGoal"],
    "assumptions_inequalities": ["Assumptions"],
}

ALL_CALL_NAMES = sorted({name for names in OPERATION_NAMES.values() for name in names})
NAME_TO_OPERATIONS: dict[str, set[str]] = {}
for operation_name, call_names in OPERATION_NAMES.items():
    for call_name in call_names:
        NAME_TO_OPERATIONS.setdefault(call_name, set()).add(operation_name)
OPTION_TO_OPERATIONS: dict[str, set[str]] = {}
for operation_name, option_names in OPTION_NAMES.items():
    for option_name in option_names:
        OPTION_TO_OPERATIONS.setdefault(option_name, set()).add(operation_name)
CALL_ALTERNATION = "|".join(
    map(re.escape, sorted(ALL_CALL_NAMES, key=len, reverse=True))
)
OPTION_ALTERNATION = "|".join(
    map(re.escape, sorted(OPTION_TO_OPERATIONS, key=len, reverse=True))
)
CALL_RE = re.compile(r"\b(" + CALL_ALTERNATION + r")\s*\[")
OPTION_RE = re.compile(r"\b(" + OPTION_ALTERNATION + r")\s*->")
NOTEBOOK_CALL_RE = re.compile(
    r'["\'](' + CALL_ALTERNATION + r')["\'][\s\S]{0,80}["\']\[["\']'
)
NOTEBOOK_OPTION_RE = re.compile(
    r'["\']('
    + OPTION_ALTERNATION
    + r')["\'][\s\S]{0,80}(?:\\\\\\[Rule\\]|->)'
)
MARKER_RE = re.compile(
    r"Mathematica|Wolfram|wolframscript|MathKernel|WolframKernel"
)
GREP_MARKER = (
    r"Mathematica|Wolfram|wolframscript|MathKernel|WolframKernel|"
    r"FortranForm|GroebnerBasis|FourierTransform|LaplaceTransform"
)
CANDIDATE_SUFFIXES = {".nb", ".wl", ".wls", ".m"}
HOST_SOURCE_SUFFIXES = {
    ".bash",
    ".c",
    ".cc",
    ".cmake",
    ".cpp",
    ".csh",
    ".f",
    ".f03",
    ".f08",
    ".f90",
    ".f95",
    ".h",
    ".hpp",
    ".mk",
    ".pl",
    ".py",
    ".sh",
    ".toml",
    ".yaml",
    ".yml",
    ".zsh",
}
MAX_SOURCE_BYTES = 64 * 1024 * 1024
GITHUB_SEARCH_INTERVAL_SECONDS = 7.0


@dataclass
class ScanResult:
    candidate_files: int = 0
    files_completed: int = 0
    file_errors: int = 0
    source_truncations: int = 0
    selection_errors: int = 0
    tree_truncations: int = 0
    file_hits: list[set[str]] = field(default_factory=list)
    identity: str = ""

    @property
    def complete(self) -> bool:
        return (
            self.file_errors == 0
            and self.selection_errors == 0
            and self.source_truncations == 0
            and self.tree_truncations == 0
        )

    @property
    def operations(self) -> set[str]:
        if not self.file_hits:
            return set()
        return set().union(*self.file_hits)


class SearchPacer:
    def __init__(self, interval: float) -> None:
        self.interval = interval
        self.lock = threading.Lock()
        self.last_call = 0.0

    def wait(self) -> None:
        with self.lock:
            delay = self.interval - (time.monotonic() - self.last_call)
            if delay > 0:
                time.sleep(delay)
            self.last_call = time.monotonic()


def is_wolfram_or_host_path(path: str) -> bool:
    suffix = Path(path).suffix.lower()
    return suffix in CANDIDATE_SUFFIXES or suffix in HOST_SOURCE_SUFFIXES


def run(
    command: list[str],
    cwd: Path | None = None,
    timeout: int = 300,
    attempts: int = 3,
) -> bytes:
    last_status = -1
    for attempt in range(attempts):
        try:
            completed = subprocess.run(
                command,
                cwd=cwd,
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                check=False,
                timeout=timeout,
            )
        except (OSError, subprocess.TimeoutExpired):
            if attempt + 1 == attempts:
                raise
            time.sleep(0.5 * (2**attempt))
            continue
        last_status = completed.returncode
        if completed.returncode == 0:
            return completed.stdout
        if attempt + 1 < attempts:
            time.sleep(0.5 * (2**attempt))
    raise RuntimeError(f"command failed with exit status {last_status}")


def _run_bounded_once(
    command: list[str],
    byte_limit: int,
    cwd: Path | None = None,
    timeout: int = 300,
) -> tuple[bytes, bool]:
    process = subprocess.Popen(
        command,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
    )
    if process.stdout is None:
        process.terminate()
        raise RuntimeError("command stdout was unavailable")
    selector = selectors.DefaultSelector()
    selector.register(process.stdout, selectors.EVENT_READ)
    deadline = time.monotonic() + timeout
    chunks: list[bytes] = []
    total = 0
    try:
        while total <= byte_limit:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise subprocess.TimeoutExpired(command, timeout)
            if not selector.select(remaining):
                raise subprocess.TimeoutExpired(command, timeout)
            chunk = os.read(
                process.stdout.fileno(),
                min(64 * 1024, byte_limit + 1 - total),
            )
            if not chunk:
                break
            chunks.append(chunk)
            total += len(chunk)
        data = b"".join(chunks)
        truncated = len(data) > byte_limit
        if truncated:
            process.terminate()
        process.wait(timeout=max(0.1, deadline - time.monotonic()))
    except (OSError, subprocess.TimeoutExpired):
        process.kill()
        process.wait()
        raise
    finally:
        selector.close()
        process.stdout.close()
    if not truncated and process.returncode != 0:
        raise RuntimeError(f"command failed with exit status {process.returncode}")
    return data, truncated


def run_bounded(
    command: list[str],
    byte_limit: int,
    cwd: Path | None = None,
    timeout: int = 300,
    attempts: int = 3,
) -> tuple[bytes, bool]:
    last_error: Exception | None = None
    for attempt in range(attempts):
        try:
            return _run_bounded_once(command, byte_limit, cwd, timeout)
        except (OSError, RuntimeError, subprocess.TimeoutExpired) as error:
            last_error = error
            if attempt + 1 < attempts:
                time.sleep(0.5 * (2**attempt))
    if last_error is not None:
        raise last_error
    raise RuntimeError("bounded command did not run")


def strip_wolfram_comments_and_strings(text: str) -> str:
    output: list[str] = []
    index = 0
    comment_depth = 0
    in_string = False
    escaped = False
    while index < len(text):
        pair = text[index : index + 2]
        character = text[index]
        if comment_depth:
            if pair == "(*":
                comment_depth += 1
                output.extend("  ")
                index += 2
            elif pair == "*)":
                comment_depth -= 1
                output.extend("  ")
                index += 2
            else:
                output.append("\n" if character == "\n" else " ")
                index += 1
            continue
        if in_string:
            output.append("\n" if character == "\n" else " ")
            if escaped:
                escaped = False
            elif character == "\\":
                escaped = True
            elif character == '"':
                in_string = False
            index += 1
            continue
        if pair == "(*":
            comment_depth = 1
            output.extend("  ")
            index += 2
        elif character == '"':
            in_string = True
            output.append(" ")
            index += 1
        else:
            output.append(character)
            index += 1
    return "".join(output)


def strip_host_comments(text: str, suffix: str) -> str:
    hash_comments = suffix in {
        ".bash",
        ".cmake",
        ".csh",
        ".mk",
        ".pl",
        ".py",
        ".sh",
        ".toml",
        ".yaml",
        ".yml",
        ".zsh",
    }
    bang_comments = suffix in {".f", ".f03", ".f08", ".f90", ".f95"}
    c_comments = suffix in {".c", ".cc", ".cpp", ".h", ".hpp"}
    output: list[str] = []
    index = 0
    quote_character = ""
    escaped = False
    block_comment = False
    while index < len(text):
        pair = text[index : index + 2]
        character = text[index]
        if block_comment:
            if pair == "*/":
                block_comment = False
                output.extend("  ")
                index += 2
            else:
                output.append("\n" if character == "\n" else " ")
                index += 1
            continue
        if quote_character:
            output.append(character)
            if escaped:
                escaped = False
            elif character == "\\":
                escaped = True
            elif character == quote_character:
                quote_character = ""
            index += 1
            continue
        if character in {"'", '"'}:
            quote_character = character
            output.append(character)
            index += 1
            continue
        if c_comments and pair == "/*":
            block_comment = True
            output.extend("  ")
            index += 2
            continue
        if c_comments and pair == "//":
            newline = text.find("\n", index)
            if newline == -1:
                output.extend(" " * (len(text) - index))
                break
            output.extend(" " * (newline - index))
            index = newline
            continue
        if (hash_comments and character == "#") or (
            bang_comments and character == "!"
        ):
            newline = text.find("\n", index)
            if newline == -1:
                output.extend(" " * (len(text) - index))
                break
            output.extend(" " * (newline - index))
            index = newline
            continue
        output.append(character)
        index += 1
    return "".join(output)


def operation_hits(text: str, notebook: bool) -> set[str]:
    hits: set[str] = set()
    if notebook:
        for match in NOTEBOOK_CALL_RE.finditer(text):
            hits.update(NAME_TO_OPERATIONS[match.group(1)])
        for match in NOTEBOOK_OPTION_RE.finditer(text):
            hits.update(OPTION_TO_OPERATIONS[match.group(1)])
        if re.search(r'["\']\^["\']', text):
            hits.add("safe_log_power_radical")
        if re.search(r'["\'][A-Za-z$]\w*[\']+["\']', text):
            hits.add("differentiate")
        return hits

    for match in CALL_RE.finditer(text):
        hits.update(NAME_TO_OPERATIONS[match.group(1)])
    for match in OPTION_RE.finditer(text):
        hits.update(OPTION_TO_OPERATIONS[match.group(1)])
    if re.search(r"\$Assumptions\s*=", text):
        hits.add("assumptions_inequalities")
    if re.search(r"(?:\d+\.\d*|\d*\.\d+|\d+)`\d+", text):
        hits.add("arbitrary_precision")
    if re.search(r"\s\.\s", text):
        hits.add("matrices_tensors")
    if re.search(r"\b[A-Za-z$]\w*'+\s*\[", text):
        hits.add("differentiate")
    if "/;" in text or re.search(r"\bCondition\s*\[", text):
        hits.add("assumptions_inequalities")
    if "^" in text:
        hits.add("safe_log_power_radical")
    return hits


def classify(data: bytes, path: str) -> tuple[set[str], bool]:
    truncated = len(data) > MAX_SOURCE_BYTES
    text = data[:MAX_SOURCE_BYTES].decode("utf-8", errors="ignore")
    suffix = Path(path).suffix.lower()
    if suffix == ".nb":
        return operation_hits(text, notebook=True), truncated

    if suffix in {".wl", ".wls", ".m"}:
        cleaned = strip_wolfram_comments_and_strings(text)
        if suffix == ".m" and not (CALL_RE.search(cleaned) or MARKER_RE.search(text)):
            return set(), truncated
        return operation_hits(cleaned, notebook=False), truncated

    cleaned = strip_host_comments(text, suffix)
    if not MARKER_RE.search(cleaned):
        return set(), truncated
    return operation_hits(cleaned, notebook=False), truncated


def blank_counts() -> dict[str, int]:
    return {name: 0 for name in OPERATION_NAMES}


def new_source_class() -> dict[str, Any]:
    return {
        "source_requested": False,
        "listing_completed": False,
        "selection_requested": False,
        "selection_completed": False,
        "selection_skipped": False,
        "selection_truncations": 0,
        "overall_complete": False,
        "repository_instances_discovered": 0,
        "repository_instances_attempted": 0,
        "repository_instances_completed": 0,
        "repository_instances_incomplete": 0,
        "logical_repositories": 0,
        "candidate_files": 0,
        "files_completed": 0,
        "file_errors": 0,
        "selection_errors": 0,
        "source_truncations": 0,
        "tree_truncations": 0,
        "operation_repository_hits": blank_counts(),
        "operation_file_hits": blank_counts(),
    }


def merge_file_counts(target: dict[str, Any], result: ScanResult) -> None:
    target["candidate_files"] += result.candidate_files
    target["files_completed"] += result.files_completed
    target["file_errors"] += result.file_errors
    target["selection_errors"] += result.selection_errors
    target["source_truncations"] += result.source_truncations
    target["tree_truncations"] += result.tree_truncations
    for hits in result.file_hits:
        for operation in hits:
            target["operation_file_hits"][operation] += 1


def finalize_source(target: dict[str, Any]) -> None:
    target["selection_completed"] = (
        target["selection_requested"]
        and not target["selection_skipped"]
        and target["selection_errors"] == 0
        and target["selection_truncations"] == 0
    )
    target["overall_complete"] = (
        target["source_requested"]
        and target["listing_completed"]
        and target["selection_completed"]
        and target["repository_instances_attempted"]
        == target["repository_instances_discovered"]
        and target["repository_instances_incomplete"] == 0
        and target["file_errors"] == 0
        and target["source_truncations"] == 0
        and target["tree_truncations"] == 0
    )


def local_worktrees() -> list[tuple[Path, str]]:
    roots = [(Path("/home/ert/proj"), "local_proj"), (Path("/home/ert/code"), "local_code")]
    found: list[tuple[Path, str]] = []
    for root, source_class in roots:
        output = run(["find", str(root), "-name", ".git", "-print"])
        for raw in output.decode().splitlines():
            found.append((Path(raw).parent, source_class))
    return found


def local_candidates(repo: Path) -> ScanResult:
    names = run(["git", "ls-files"], cwd=repo).decode(errors="ignore")
    candidate_paths = {
        name
        for name in names.splitlines()
        if Path(name).suffix.lower() in CANDIDATE_SUFFIXES
        or (MARKER_RE.search(name) and is_wolfram_or_host_path(name))
    }
    selection_errors = 0
    grep_completed: subprocess.CompletedProcess[bytes] | None = None
    for attempt in range(3):
        try:
            grep_completed = subprocess.run(
                ["git", "grep", "-I", "-l", "-E", GREP_MARKER, "--"],
                cwd=repo,
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                check=False,
                timeout=300,
            )
        except (OSError, subprocess.TimeoutExpired):
            if attempt + 1 < 3:
                time.sleep(0.5 * (2**attempt))
                continue
            selection_errors = 1
        break
    if grep_completed is not None:
        if grep_completed.returncode == 0:
            candidate_paths.update(
                path
                for path in grep_completed.stdout.decode(
                    errors="ignore"
                ).splitlines()
                if is_wolfram_or_host_path(path)
            )
        elif grep_completed.returncode != 1:
            selection_errors = 1

    result = ScanResult(
        candidate_files=len(candidate_paths),
        selection_errors=selection_errors,
    )
    try:
        remote = run(["git", "config", "--get", "remote.origin.url"], cwd=repo).decode().strip()
    except RuntimeError:
        remote = ""
    try:
        common = run(
            ["git", "rev-parse", "--path-format=absolute", "--git-common-dir"], cwd=repo
        ).decode().strip()
    except RuntimeError:
        common = str(repo / ".git")
    result.identity = remote or common

    for relative in candidate_paths:
        path = repo / relative
        try:
            if not path.is_file():
                result.file_errors += 1
                continue
            with path.open("rb") as stream:
                data = stream.read(MAX_SOURCE_BYTES + 1)
            hits, truncated = classify(data, relative)
        except (OSError, ValueError):
            result.file_errors += 1
            continue
        result.files_completed += 1
        result.source_truncations += int(truncated)
        if hits:
            result.file_hits.append(hits)
    return result


def glab_json(endpoint: str, paginate: bool = False) -> Any:
    if not paginate:
        return json.loads(run(["glab", "api", endpoint], timeout=600).decode())
    items = []
    separator = "&" if "?" in endpoint else "?"
    page = 1
    while True:
        batch = glab_json(f"{endpoint}{separator}page={page}")
        items.extend(batch)
        if len(batch) < 100:
            break
        page += 1
    return items


def gitlab_project(project: dict[str, Any]) -> ScanResult:
    project_id = int(project["id"])
    branch = project.get("default_branch")
    if not branch:
        return ScanResult()
    tree = glab_json(
        f"projects/{project_id}/repository/tree?recursive=true&per_page=100",
        paginate=True,
    )
    candidates = {
        entry["path"]
        for entry in tree
        if entry.get("type") == "blob"
        and (
            Path(entry["path"]).suffix.lower() in CANDIDATE_SUFFIXES
            or (
                MARKER_RE.search(entry["path"])
                and is_wolfram_or_host_path(entry["path"])
            )
        )
    }
    selection_errors = 0
    for marker in (
        "Wolfram",
        "Mathematica",
        "MathKernel",
        "wolframscript",
        "WolframKernel",
    ):
        try:
            matches = glab_json(
                f"projects/{project_id}/search?scope=blobs&search={marker}&per_page=100",
                paginate=True,
            )
            candidates.update(
                match["path"]
                for match in matches
                if isinstance(match.get("path"), str)
                and is_wolfram_or_host_path(match["path"])
            )
        except (RuntimeError, json.JSONDecodeError, subprocess.TimeoutExpired):
            selection_errors += 1

    result = ScanResult(
        candidate_files=len(candidates),
        selection_errors=selection_errors,
    )
    for path in candidates:
        try:
            data, command_truncated = run_bounded(
                [
                    "glab",
                    "api",
                    f"projects/{project_id}/repository/files/{quote(path, safe='')}/raw"
                    f"?ref={quote(branch, safe='')}",
                ],
                byte_limit=MAX_SOURCE_BYTES,
                timeout=300,
            )
            hits, truncated = classify(data, path)
        except (RuntimeError, OSError, ValueError, subprocess.TimeoutExpired):
            result.file_errors += 1
            continue
        result.files_completed += 1
        result.source_truncations += int(truncated or command_truncated)
        if hits:
            result.file_hits.append(hits)
    return result


def gh_json(endpoint: str, paginate: bool = False) -> Any:
    if not paginate:
        return json.loads(run(["gh", "api", endpoint], timeout=600).decode())
    decoder = json.JSONDecoder()
    data = run(["gh", "api", "--paginate", endpoint], timeout=600).decode()
    offset = 0
    items = []
    while offset < len(data):
        while offset < len(data) and data[offset].isspace():
            offset += 1
        if offset == len(data):
            break
        page, offset = decoder.raw_decode(data, offset)
        items.extend(page)
    return items


def github_marker_candidates(
    projects: list[dict[str, Any]],
) -> tuple[dict[str, set[str]], dict[str, int]]:
    allowed = {project["full_name"] for project in projects}
    owner_types: dict[str, str] = {}
    for project in projects:
        owner = project["owner"]
        owner_types[owner["login"]] = owner["type"]
    candidates = {name: set() for name in allowed}
    stats = {"selection_errors": 0, "selection_truncations": 0}
    pacer = SearchPacer(GITHUB_SEARCH_INTERVAL_SECONDS)
    for owner, owner_type in sorted(owner_types.items()):
        qualifier = "org" if owner_type == "Organization" else "user"
        for marker in (
            "Wolfram",
            "Mathematica",
            "MathKernel",
            "wolframscript",
            "WolframKernel",
        ):
            query = quote(f"{marker} {qualifier}:{owner}", safe="")
            try:
                pacer.wait()
                first = gh_json(f"search/code?q={query}&per_page=100&page=1")
            except (RuntimeError, json.JSONDecodeError, subprocess.TimeoutExpired):
                stats["selection_errors"] += 1
                continue
            total = int(first.get("total_count", 0))
            pages = min(10, max(1, math.ceil(total / 100)))
            if total > 1000:
                stats["selection_truncations"] += 1
            batches = [first]
            for page in range(2, pages + 1):
                try:
                    pacer.wait()
                    batches.append(
                        gh_json(f"search/code?q={query}&per_page=100&page={page}")
                    )
                except (RuntimeError, json.JSONDecodeError, subprocess.TimeoutExpired):
                    stats["selection_errors"] += 1
                    break
            for batch in batches:
                for item in batch.get("items", []):
                    full_name = item.get("repository", {}).get("full_name", "")
                    path = item.get("path")
                    if (
                        full_name in allowed
                        and isinstance(path, str)
                        and is_wolfram_or_host_path(path)
                    ):
                        candidates[full_name].add(path)
    return candidates, stats


def github_tree(full_name: str, branch: str) -> tuple[list[dict[str, Any]], int]:
    tree = gh_json(f"repos/{full_name}/git/trees/{quote(branch, safe='')}?recursive=1")
    if not tree.get("truncated"):
        return tree.get("tree", []), 0

    with tempfile.TemporaryDirectory(prefix="fortsym-github-tree-") as directory:
        checkout = Path(directory) / "repository"
        run(
            [
                "gh",
                "repo",
                "clone",
                full_name,
                str(checkout),
                "--",
                "--filter=blob:none",
                "--depth=1",
                "--no-checkout",
                "--branch",
                branch,
            ],
            timeout=1200,
        )
        listing = run(
            ["git", "ls-tree", "-r", "-l", "HEAD"],
            cwd=checkout,
            timeout=1200,
        ).decode(errors="surrogateescape")
    entries: list[dict[str, Any]] = []
    for line in listing.splitlines():
        metadata, separator, path = line.partition("\t")
        fields = metadata.split()
        if not separator or len(fields) != 4 or fields[1] != "blob":
            continue
        try:
            size = int(fields[3])
        except ValueError:
            size = 0
        entries.append(
            {
                "path": path,
                "type": "blob",
                "sha": fields[2],
                "size": size,
            }
        )
    return entries, 0


def github_project(
    project: dict[str, Any],
    marker_paths: set[str],
) -> ScanResult:
    branch = project.get("default_branch")
    if not branch:
        return ScanResult()
    full_name = project["full_name"]
    tree, truncations = github_tree(full_name, branch)
    blobs = {
        entry["path"]: (entry["sha"], int(entry.get("size", 0)))
        for entry in tree
        if entry.get("type") == "blob" and isinstance(entry.get("sha"), str)
    }
    candidates = {
        path
        for path in blobs
        if Path(path).suffix.lower() in CANDIDATE_SUFFIXES
        or (MARKER_RE.search(path) and is_wolfram_or_host_path(path))
    }
    candidates.update(marker_paths)
    result = ScanResult(
        candidate_files=len(candidates),
        tree_truncations=truncations,
    )
    for path in candidates:
        blob = blobs.get(path)
        if not blob:
            result.file_errors += 1
            continue
        blob_id, size = blob
        if size > MAX_SOURCE_BYTES:
            result.source_truncations += 1
            continue
        try:
            data, command_truncated = run_bounded(
                [
                    "gh",
                    "api",
                    "-H",
                    "Accept: application/vnd.github.raw+json",
                    f"repos/{full_name}/git/blobs/{blob_id}",
                ],
                byte_limit=MAX_SOURCE_BYTES,
                timeout=300,
            )
            hits, truncated = classify(data, path)
        except (
            RuntimeError,
            OSError,
            ValueError,
            subprocess.TimeoutExpired,
        ):
            result.file_errors += 1
            continue
        result.files_completed += 1
        result.source_truncations += int(truncated or command_truncated)
        if hits:
            result.file_hits.append(hits)
    return result


def named_union(aggregate: dict[str, Any], name: str, operations: set[str]) -> None:
    current = set(aggregate["named_consumers"][name]["observed_operations"])
    aggregate["named_consumers"][name]["observed_operations"] = sorted(
        current | operations
    )


def new_aggregate() -> dict[str, Any]:
    return {
        "schema_version": 2,
        "privacy": {
            "aggregate_only": True,
            "repository_names_emitted": ["FortNum", "MHD1D"],
            "source_paths_emitted": False,
            "source_snippets_emitted": False,
            "source_hashes_emitted": False,
        },
        "source_classes": {
            name: new_source_class()
            for name in ("local_proj", "local_code", "gitlab", "github")
        },
        "named_consumers": {
            "FortNum": {"observed_operations": []},
            "MHD1D": {"observed_operations": []},
        },
    }


def merge_aggregates(paths: list[Path]) -> dict[str, Any]:
    merged = new_aggregate()
    for path in paths:
        source = json.loads(path.read_text(encoding="utf-8"))
        if source.get("schema_version") != merged["schema_version"]:
            raise ValueError(f"incompatible audit schema in {path.name}")
        for source_class, counts in source["source_classes"].items():
            discovered = int(counts["repository_instances_discovered"])
            if discovered == 0:
                continue
            current = merged["source_classes"][source_class]
            if current["repository_instances_discovered"] != 0:
                raise ValueError(f"duplicate source class {source_class}")
            merged["source_classes"][source_class] = counts
        for name, consumer in source["named_consumers"].items():
            named_union(
                merged,
                name,
                set(consumer.get("observed_operations", [])),
            )
    return merged


def scan_local(aggregate: dict[str, Any], workers: int) -> None:
    worktrees = local_worktrees()
    for source_class in ("local_proj", "local_code"):
        target = aggregate["source_classes"][source_class]
        target["source_requested"] = True
        target["listing_completed"] = True
        target["selection_requested"] = True
    for _, source_class in worktrees:
        aggregate["source_classes"][source_class][
            "repository_instances_discovered"
        ] += 1
    logical_hits: dict[str, dict[str, set[str]]] = {
        "local_proj": {},
        "local_code": {},
    }
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as executor:
        futures = {
            executor.submit(local_candidates, repo): (repo, source_class)
            for repo, source_class in worktrees
        }
        for future in concurrent.futures.as_completed(futures):
            repo, source_class = futures[future]
            target = aggregate["source_classes"][source_class]
            target["repository_instances_attempted"] += 1
            try:
                result = future.result()
            except (RuntimeError, OSError, subprocess.TimeoutExpired):
                target["repository_instances_incomplete"] += 1
                continue
            merge_file_counts(target, result)
            if result.complete:
                target["repository_instances_completed"] += 1
            else:
                target["repository_instances_incomplete"] += 1
            identity_hits = logical_hits[source_class].setdefault(
                result.identity, set()
            )
            identity_hits.update(result.operations)
            if repo == Path("/home/ert/code/fortnum"):
                named_union(aggregate, "FortNum", result.operations)

    for source_class, repositories in logical_hits.items():
        target = aggregate["source_classes"][source_class]
        target["logical_repositories"] = len(repositories)
        for operations in repositories.values():
            for operation in operations:
                target["operation_repository_hits"][operation] += 1
        finalize_source(target)


def scan_remote(
    aggregate: dict[str, Any],
    projects: list[dict[str, Any]],
    source_class: str,
    scanner: Callable[[dict[str, Any]], ScanResult],
    workers: int,
) -> None:
    target = aggregate["source_classes"][source_class]
    target["source_requested"] = True
    target["listing_completed"] = True
    target["selection_requested"] = True
    target["repository_instances_discovered"] = len(projects)
    target["logical_repositories"] = len(projects)
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as executor:
        futures = {executor.submit(scanner, project): project for project in projects}
        for future in concurrent.futures.as_completed(futures):
            project = futures[future]
            target["repository_instances_attempted"] += 1
            try:
                result = future.result()
            except (
                RuntimeError,
                OSError,
                json.JSONDecodeError,
                subprocess.TimeoutExpired,
            ):
                target["repository_instances_incomplete"] += 1
                continue
            merge_file_counts(target, result)
            if result.complete:
                target["repository_instances_completed"] += 1
            else:
                target["repository_instances_incomplete"] += 1
            for operation in result.operations:
                target["operation_repository_hits"][operation] += 1
            full_name = project.get("full_name", "").lower()
            if full_name == "lazy-fortran/fortnum":
                named_union(aggregate, "FortNum", result.operations)
            if full_name == "itpplasma/mhd1d":
                named_union(aggregate, "MHD1D", result.operations)
    finalize_source(target)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path)
    parser.add_argument("--workers", type=int, default=12)
    parser.add_argument("--skip-local", action="store_true")
    parser.add_argument("--skip-gitlab", action="store_true")
    parser.add_argument("--skip-github", action="store_true")
    parser.add_argument("--skip-github-marker-search", action="store_true")
    parser.add_argument("--merge-input", type=Path, nargs="+")
    args = parser.parse_args()

    if args.merge_input:
        aggregate = merge_aggregates(args.merge_input)
    else:
        aggregate = new_aggregate()
        if not args.skip_local:
            scan_local(aggregate, args.workers)
        if not args.skip_gitlab:
            gitlab_projects = glab_json(
                "projects?simple=true&per_page=100&order_by=id&sort=asc",
                paginate=True,
            )
            scan_remote(
                aggregate,
                gitlab_projects,
                "gitlab",
                gitlab_project,
                args.workers,
            )
        if not args.skip_github:
            github_projects = gh_json(
                "user/repos?affiliation=owner,collaborator,organization_member&per_page=100",
                paginate=True,
            )
            marker_paths = {
                project["full_name"]: set() for project in github_projects
            }
            search_stats = {"selection_errors": 0, "selection_truncations": 0}
            if not args.skip_github_marker_search:
                marker_paths, search_stats = github_marker_candidates(
                    github_projects
                )
            scan_remote(
                aggregate,
                github_projects,
                "github",
                lambda project: github_project(
                    project, marker_paths.get(project["full_name"], set())
                ),
                args.workers,
            )
            github = aggregate["source_classes"]["github"]
            github["selection_skipped"] = args.skip_github_marker_search
            github["selection_errors"] += search_stats["selection_errors"]
            github["selection_truncations"] += search_stats[
                "selection_truncations"
            ]
            if (
                github["selection_skipped"]
                or github["selection_errors"] > 0
                or github["selection_truncations"] > 0
            ):
                github["repository_instances_completed"] = 0
                github["repository_instances_incomplete"] = github[
                    "repository_instances_discovered"
                ]
            finalize_source(github)

    rendered = json.dumps(aggregate, indent=2, sort_keys=True) + "\n"
    if args.output:
        args.output.write_text(rendered, encoding="utf-8")
    else:
        sys.stdout.write(rendered)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
