# Bounded scalar derivation recording

`fortsym_derivation:derivation_t` owns an ordered execution record. Native
`apply` executes constructors, scalar arithmetic, differentiation, substitution,
simplification, expansion, factorization and rational cancellation through
existing native owners. `observe` records an already executed operation and its
observed result; it does not reconstruct earlier operations from that result.
Inputs must have recorded definitions in the same arena. `observe(input_steps=)`
and the optional `step=` on names/checks/assumptions disambiguate results that
share a canonical expression identity. The ordinary expression handle has no
recording overhead or hidden session state.

```fortran
use fortsym_derivation, only: derivation_t
! arena is an initialized target arena_t; digest is the source SHA256.
call record%init(arena, digest, budget=1000)
x = record%apply('symbol', [expr_t::], 'x', ok, why)
call record%assume(x, FACT_POSITIVE, ok, why)
! Continue using apply, or observe actual calls immediately as they execute.
source = record%export_sympy(ok, why)
```

The C ABI provides arena-owned begin/observe/assume/directive/export/end calls.
It copies expression identities into the record; the arena must outlive export.
Python uses the same owner and records supported constructor, arithmetic and
expression-method calls through the scalar result boundary:

```python
import hashlib
from pathlib import Path
from fortsym import Derivation
import fortsym.sympy as sp

digest = hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
with Derivation(digest, max_steps=1000) as record:
    x = sp.Symbol('x', positive=True)
    f = (x + 1)**3
    d = sp.diff(f, x)
    record.name('derivative', d)
    record.check_zero(d - (3*x*x + 6*x + 3), 'polynomial coefficients')
Path('derivation.py').write_text(record.source)
```

Run the standalone artifact with ordinary SymPy 1.14.0. Set
`FORTSYM_REPLAY_BACKEND=fortsym` to use the native facade explicitly. Ordinary
execution does not import FortSym or load its shared library. Native authoring,
recording and export need no Python. The optional independent gate executes the
same source and artifact in fresh processes; native workers exclude Python
site-packages so ordinary SymPy cannot masquerade as native evidence.

Exact integers/rationals, symbol facts, operations, named intermediates, source
SHA256, body CRC64-ECMA, observed result checks, explicit domain exclusions and
zero/UNKNOWN witnesses survive export. CRC64 is a content fingerprint, not an
authentication mechanism. Division retains its denominator as a nonzero domain
condition; an explicit condition supplies its readable label. Conditions are
recorded obligations, not assertions that an arbitrary parameter satisfies them.
`check_unknown` records the native engine's lack of a decision, never a nonzero
proof. Observed intermediate equality is replay evidence; independent residual
and domain witnesses remain necessary.

The v1 supported scope is exact scalar operations with one declaration per
symbol name in one scope, with
positive facts from the native vocabulary. Contradictory facts, changed facts
after use, foreign/unrecorded inputs, unsupported operations/options, float
lowering and exhausted budgets refuse export with a step diagnostic. Default
limits are 10,000 steps and 16 MiB of exported body. No Python control flow,
assertion interpretation, external side effects, matrix/units API, scoped
assumptions, integration/limit options or whole-course replay is claimed. Use
explicit check witnesses; unsupported scalar result calls poison the session.
Native callers that bypass `apply` must observe each operation themselves.
See the broader [export contract](derivation-export.md) for remaining scope.
