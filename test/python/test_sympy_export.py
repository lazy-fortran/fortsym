"""Native-only export, escaping and refusal checks; no SymPy installation needed."""
import hashlib
import os
from pathlib import Path
import runpy
import subprocess
import sys
import tempfile
from fortsym import Arena, Derivation, FortSymError

with tempfile.TemporaryDirectory() as tmp:
    path = Path(tmp)/'native.py'
    digest = hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
    subprocess.run([sys.argv[1], str(path), digest], check=True, capture_output=True)
    env = dict(os.environ, FORTSYM_REPLAY_BACKEND='fortsym')
    subprocess.run([sys.executable, '-S', str(path)], env=env, check=True,
                   capture_output=True, timeout=30)
    with Arena() as arena:
        with Derivation(digest, arena=arena) as record:
            x = arena.symbol("x'); __import__('os').abort(); #")
            record.name("name'\\\n", x)
        path.write_text(record.source)
        subprocess.run([sys.executable, '-S', str(path)], env=env, check=True,
                       capture_output=True, timeout=30)
    # Initial arena facts are configuration, not invented operation history.
    with Arena() as arena:
        x = arena.symbol('initial_x')
        arena.assume(x, 2)
        with Derivation(digest, arena=arena) as record:
            x = arena.symbol('initial_x')
            root = arena.function('sqrt', [x*x]).simplify()
            record.name('positive_root', root)
        path.write_text(record.source)
        subprocess.run([sys.executable, '-S', str(path)], env=env, check=True,
                       capture_output=True, timeout=30)
        try:
            import sympy
        except ImportError:
            sympy = None
        if sympy is not None and sympy.__version__ == '1.14.0':
            ordinary = dict(os.environ, FORTSYM_REPLAY_BACKEND='sympy')
            ordinary.pop('FORTSYM_LIBRARY', None)
            ordinary.pop('PYTHONPATH', None)
            subprocess.run([sys.executable, str(path)], env=ordinary, check=True,
                           capture_output=True, timeout=30)
    with Arena() as arena:
        x = arena.symbol('composite_x')
        arena.assume(x + 1, 2)
        try:
            with Derivation(digest, arena=arena):
                pass
        except FortSymError as error:
            assert 'initial assumption context' in str(error)
        else:
            raise AssertionError('unsupported initial condition was dropped')
    for mode in ('budget', 'foreign', 'unsupported', 'late', 'unrecorded', 'redefinition', 'nul', 'label', 'function', 'arity'):
        with Arena() as arena, Arena() as foreign:
            existing = arena.symbol('existing')
            try:
                with Derivation(digest, arena=arena, max_steps=1 if mode=='budget' else 100) as record:
                    x = arena.symbol('x')
                    if mode == 'budget':
                        arena.integer(1)
                    elif mode == 'foreign':
                        record.name('alien', foreign.symbol('x'))
                    elif mode == 'unrecorded':
                        record.name('old', existing)
                    elif mode == 'redefinition':
                        arena.symbol('x')
                    elif mode == 'nul':
                        arena.symbol('a\x00b')
                    elif mode == 'label':
                        record.name('a\x00b', x)
                    elif mode == 'function':
                        arena.function('atan2', [x, x])
                    elif mode == 'arity':
                        arena.function('sin', [x, x])
                    elif mode == 'late':
                        result = x + arena.integer(1)
                        arena.assume(x, 2)
                    else:
                        record.refuse('unsupported branch/units option')
            except (FortSymError, RuntimeError) as error:
                assert 'derivation' in str(error) or 'record' in str(error)
            else:
                raise AssertionError('unsupported recording succeeded: '+mode)
import fortsym.sympy as sp
undefined_sine = sp.Function('sin')
for construct in (False, True):
    try:
        with Derivation(digest):
            if construct:
                sp.Function('sin')
            else:
                undefined_sine(sp.Symbol('shadowed_sine'))
    except FortSymError as error:
        assert 'uninterpreted function' in str(error)
    else:
        raise AssertionError('undefined function silently became a builtin')
print('native-only standalone export and refusal cases PASS')
