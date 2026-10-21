#!/usr/bin/env python3
"""Partition a pinned corpus deterministically and verify complete report coverage."""
import argparse
import json
from pathlib import Path

from fortsym_bench.cli import build_summary, discover, main


def run(args):
    scripts = discover([args.corpus])
    selected = scripts[args.shard::args.count]
    if not selected:
        raise ValueError("empty corpus shard")
    paths = [str(p.with_suffix(".wl") if p.with_suffix(".wl").exists()
                 else p.with_suffix(".py")) for p in selected]
    status = main(["run", *paths, "--backend", "sympy", "mathics", "fortsym-wl",
                   "--jobs", "4", "--timeout", "60", "--report", str(args.report)])
    # Disagreements remain coverage data, as in the original cold audit.
    # Infrastructure errors must still fail, rather than silently lose a shard.
    report = json.loads(args.report.read_text())
    if set(report["scripts"]) != {str(p) for p in selected}:
        raise ValueError("incomplete corpus shard")
    print(f"shard {args.shard}/{args.count}: {len(selected)} scripts, audit exit {status}")


def merge(args):
    reports = sorted(args.directory.glob("fortsym-corpus-*.json"))
    if len(reports) != args.count:
        raise ValueError(f"expected {args.count} reports, found {len(reports)}")
    combined = {"scripts": {}}
    for path in reports:
        report = json.loads(path.read_text())
        if combined["scripts"].keys() & report["scripts"].keys():
            raise ValueError("duplicate corpus scripts")
        combined["scripts"].update(report["scripts"])
    if set(combined["scripts"]) != {str(p) for p in discover([args.corpus])}:
        raise ValueError("merged report does not cover the pinned corpus exactly")
    combined["summary"] = build_summary(combined)
    args.report.write_text(json.dumps(combined, indent=1))
    print(json.dumps(combined["summary"]))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=["run", "merge"])
    parser.add_argument("--corpus", type=Path, default=Path("bench/corpus"))
    parser.add_argument("--count", type=int, default=16)
    parser.add_argument("--shard", type=int, default=0)
    parser.add_argument("--directory", type=Path, default=Path("artifacts"))
    parser.add_argument("--report", type=Path, required=True)
    args = parser.parse_args()
    if args.count < 1 or not 0 <= args.shard < args.count:
        parser.error("invalid shard/count")
    (run if args.action == "run" else merge)(args)
