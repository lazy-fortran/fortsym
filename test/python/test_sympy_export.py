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
    for mode in ('budget', 'foreign', 'unsupported', 'late', 'unrecorded', 'redefinition'):
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
                    elif mode == 'late':
                        result = x + arena.integer(1)
                        arena.assume(x, 2)
                    else:
                        record.refuse('unsupported branch/units option')
            except (FortSymError, RuntimeError) as error:
                assert 'derivation' in str(error) or 'record' in str(error)
            else:
                raise AssertionError('unsupported recording succeeded: '+mode)
print('native-only standalone export and refusal cases PASS')
