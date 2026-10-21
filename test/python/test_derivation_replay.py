"""Isolated source -> native execution -> same standalone artifact replay gate."""
import hashlib
import json
import os
from pathlib import Path
import runpy
import subprocess
import sys
import tempfile
from fractions import Fraction


def worker(mode, path, output, artifact=None):
    if mode in ('source-native', 'export-native'):
        import fortsym.sympy as sp
        if mode == 'source-native':
            sys.modules['sympy'] = sp  # same unmodified, source-authored imports
    else:
        import sympy as sp
        assert sp.__version__ == '1.14.0'
    if mode == 'source-native':
        from fortsym import Derivation
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        with Derivation(digest) as record:
            ns = runpy.run_path(str(path), init_globals={'record': record})
        artifact.write_text(record.source)
    else:
        ns = runpy.run_path(str(path))
    x, t = sp.Symbol('x', positive=True), sp.Symbol('t', real=True)

    def number(e, point=2):
        return str(sp.simplify(e.subs(x, sp.Integer(2)).subs(t, sp.Integer(point))))

    values = {name: number(value) for name, value in ns['named'].items()}
    conditions = [(label, number(e), number(e, 1))
                  for label, e in ns.get('conditions', [])]
    if mode in ('source-native', 'export-native'):
        assert not any(name.startswith('sympy.') for name in sys.modules)
        from fortsym import _load_library
        assert Path(_load_library()._name).resolve() == Path(os.environ['FORTSYM_LIBRARY']).resolve()
    else:
        assert 'fortsym' not in sys.modules
    output.write_text(json.dumps(dict(values=values, conditions=conditions,
                                     checks=ns.get('checks', {})), sort_keys=True))


def launch(mode, source, output, artifact=None, expect_failure=False):
    env = os.environ.copy()
    env.pop('FORTSYM_REPLAY_BACKEND', None)
    if mode == 'export-native':
        env['FORTSYM_REPLAY_BACKEND'] = 'fortsym'
    command = [sys.executable]
    if mode in ('source-native', 'export-native'):
        command.append('-S')  # native worker cannot import site-installed SymPy
    else:
        env.pop('PYTHONPATH', None)
        env.pop('FORTSYM_LIBRARY', None)
    command += [__file__, '--worker', mode, str(source), str(output)]
    if artifact is not None:
        command.append(str(artifact))
    result = subprocess.run(command, env=env, capture_output=True, text=True, timeout=45)
    if expect_failure:
        assert result.returncode != 0, 'corrupted calculation unexpectedly passed'
        return None
    if result.returncode:
        raise AssertionError(result.stderr[-6000:] or result.stdout[-6000:])
    return json.loads(output.read_text())


def assert_values(actual):
    q = Fraction(9007199254740993, 10)
    expected = dict(polynomial=q+27, derivative=Fraction(27), at_two=Fraction(27),
                    positive_root=Fraction(2), cancelled=Fraction(3), exact_q=q)
    assert {name: Fraction(text) for name, text in actual['values'].items()} == expected


def assert_conditions(actual):
    # The denominator vanishes at the excluded pole and equals one at t=2.
    assert actual['conditions'] == [['excluded t=1 pole', '1', '0']]


def roundtrip():
    try:
        import sympy as sp
    except ImportError:
        raise SystemExit(77)
    if sp.__version__ != '1.14.0':
        raise SystemExit(77)
    fixture = Path(__file__).with_name('derivation_source.py')
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        artifact, output = root/'export.py', root/'output.json'
        original = launch('source-sympy', fixture, output)
        native = launch('source-native', fixture, output, artifact)
        source_hash = hashlib.sha256(fixture.read_bytes()).hexdigest()
        assert '# source-sha256: ' + source_hash in artifact.read_text()
        replay = launch('export-sympy', artifact, output)
        dropin = launch('export-native', artifact, output)
        for result in (original, native, replay, dropin):
            assert_values(result)
        for result in (replay, dropin):
            assert_conditions(result)
            assert result['checks']['unresolved square root branch'] == 'UNKNOWN'
        second = root/'second.py'
        launch('source-native', fixture, output, second)
        assert artifact.read_bytes() == second.read_bytes(), 'nondeterministic transcript'
        source = artifact.read_text()
        mutations = {
            'operation': source.replace('= sp.diff(', '= sp.Add(', 1),
            'assumptions': source.replace(', positive=True', '').replace(
                ', nonnegative=True', '').replace(', nonzero=True', ''),
            'version': source.replace('TRANSCRIPT_VERSION = 1', 'TRANSCRIPT_VERSION = 2'),
        }
        for name, changed in mutations.items():
            assert changed != source
            corrupt = root/(name+'.py')
            corrupt.write_text(changed)
            launch('export-sympy', corrupt, output, expect_failure=True)
        corrupt = root/'condition.py'
        corrupt.write_text('\n'.join(line for line in source.splitlines()
                                     if not line.startswith('conditions.append'))+'\n')
        missing = launch('export-sympy', corrupt, output)
        try:
            assert_conditions(missing)
        except AssertionError:
            pass
        else:
            raise AssertionError('omitted condition escaped the gate')
    print('four-stage scalar derivation replay and corruption oracles PASS')


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == '--worker':
        worker(sys.argv[2], Path(sys.argv[3]), Path(sys.argv[4]),
               Path(sys.argv[5]) if len(sys.argv) > 5 else None)
    else:
        roundtrip()
