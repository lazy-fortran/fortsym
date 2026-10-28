"""Final-state protocol oracle, using the real native executable."""
from pathlib import Path
import subprocess
import sys
import tempfile


def run(source):
    with tempfile.TemporaryDirectory() as work:
        path = Path(work) / "fixture.wl"
        path.write_text(source)
        result = subprocess.run([sys.argv[1], str(path)], cwd=work,
                                capture_output=True, text=True, timeout=30)
    rows = [line.split("\t") for line in result.stdout.splitlines()
            if line.startswith("R\t")]
    return result, rows


result, rows = run("x=7; x=Module[{t=1},t];")
assert result.returncode == 1, (result.stdout, result.stderr)
assert rows == [], rows
assert "UNSUPPORTED: x: scoped evaluation is not implemented" in result.stderr

result, rows = run("x=7; x=9; y=x+1;")
assert result.returncode == 0, result.stderr
assert rows == [["R", "x", "9"], ["R", "y", "10"]], rows

result, rows = run("x={1,2}; x[[3]]=4; y=5;")
assert result.returncode == 0, result.stderr
assert rows == [["R", "y", "5"]], rows
assert "UNSUPPORTED: x: Part assignment index is out of range" in result.stderr

# A malformed statement is reported, not silently dropped: here a comment
# closed early by "q*)" leaves unparseable text that swallows y.
result, rows = run("x=1;\n(* note q*)^2 *)\ny=2;")
assert rows == [["R", "x", "1"]], rows
assert "UNSUPPORTED: <statement 2>: parse:" in result.stderr, result.stderr

# Comment-only statements remain silent.
result, rows = run("(* only (* nested *) comments *)\nx=1;\n(* tail *)")
assert result.returncode == 0, result.stderr
assert rows == [["R", "x", "1"]], rows
