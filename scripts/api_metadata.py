#!/usr/bin/env python3
"""Read or materialize the compressed, pinned compatibility metadata."""

from __future__ import annotations

import argparse
import gzip
import hashlib
import json
import io
from pathlib import Path
from typing import Any


def read_json(path: Path) -> Any:
    if path.suffix != ".gz":
        compressed = path.with_suffix(path.suffix + ".gz")
        if compressed.is_file():
            path = compressed
    if path.suffix == ".gz":
        return json.loads(gzip.decompress(path.read_bytes()))
    return json.loads(path.read_text(encoding="utf-8"))


def materialize(directory: Path) -> None:
    for record in (directory / "api-metadata.sha256").read_text().splitlines():
        expected, name = record.split(maxsplit=1)
        path = directory / name
        data = gzip.decompress(path.with_suffix(path.suffix + ".gz").read_bytes())
        if hashlib.sha256(data).hexdigest() != expected:
            raise ValueError(f"metadata checksum mismatch: {name}")
        path.write_bytes(data)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["unpack", "pack"])
    args = parser.parse_args()
    directory = Path(__file__).resolve().parents[1] / "doc"
    if args.command == "unpack":
        materialize(directory)
        return
    records = []
    for path in sorted(directory.glob("sympy-api*.json")):
        data = path.read_bytes()
        compressed = io.BytesIO()
        with gzip.GzipFile(filename="", fileobj=compressed, mode="wb", mtime=0) as stream:
            stream.write(data)
        path.with_suffix(".json.gz").write_bytes(compressed.getvalue())
        records.append(f"{hashlib.sha256(data).hexdigest()}  {path.name}\n")
    if not records:
        raise ValueError("no expanded metadata; run unpack before pack")
    (directory / "api-metadata.sha256").write_text("".join(records))


if __name__ == "__main__":
    main()
