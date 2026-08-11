# fortsym roadmap

The target is **100% coverage of the fortsym-bench corpus**: 384 Wolfram-language
derivations across 50 projects, every result either agreeing with an open-source
oracle or refused with a named construct. Nothing here is scheduled by taste —
each milestone is ordered by how many corpus scripts it unblocks, and every
method is chosen from the literature and cited.

Operational rules live in `doc/roadmap.md`. Licences and clean-room boundaries
live in `LEGAL.md` and `COMPATIBILITY.md`. Method provenance lives in
`doc/provenance.md`, and every algorithm below has an entry there.

## Acceptance

A milestone is done when it has a callable public operation, an **independent**
behavioural oracle (not another CAS agreeing with it), documentation, a
reproducible benchmark row, and a measurable rise in corpus coverage. A
capability bit is added only with the operation it describes. Unsupported input
returns a diagnostic or `UNKNOWN` — never a guess.

### Regression gate

Every change follows the same evidence path:

1. State the supported semantics and the refusal boundary.
2. Add an independently derived behavioral test. A test that only reproduces
   the implementation's output is insufficient.
3. Add adversarial cases for wrong signs, domains, singularities, precision,
   shape, and resource limits when they apply.
4. Run the focused native tests, the required `fo` pipeline, and the affected
   corpus slice against SymPy and Mathics.
5. Compare agreement, disagreement, refusal, timeout, error, and runtime counts
   with the committed baseline.
6. Update cache scope or versioning when semantics change. Keep cached results
   for unaffected features.

A milestone cannot merge if it introduces a silent fallback, a new hang, a
loss of exactness, or an unexplained coverage regression. Oracle disagreement
remains a diagnostic outcome. It is never converted into native agreement.

## Issue status

Issue-level completion is tracked here alongside the capability roadmap. Issue
#20 (clean-room provenance machinery) is complete as of 2026-08-10: the
repository inventory above, the pull-request review checkbox, the exact-path
checker in `scripts/check_provenance.py`, its independent Python tests, and the
pull-request CI gate are all present.

Issue #49 (Wolfram parser gaps) is complete as of 2026-08-10: pattern blanks,
PatternTest, Alternatives, precision and context backticks, and UpSet forms
now parse as named structural heads, with precedence coverage in
`test_fortsym_wolfram`; unsupported evaluation remains an explicit refusal.

Issue #50 (residual defects in the new modules) is complete as of 2026-08-10:
`Integrate[1/x]` preserves logarithmic dynamic range with `log(abs(x))`, the
complex-domain boundary proves bounded structural cancellations before `Arg`
and negative powers, and polynomial solving raises its recursion refusal cap
with a deep valid-input regression test. The focused tests use numeric and
exact-arithmetic oracles; the repository's existing CUDA emitter fixture
passes in the full suite, including its no-device path on hosts with a CUDA
compiler but no usable GPU.

Issue #21 (ownership-safe C ABI) is complete as of 2026-08-10: the installed
`fortsym.h` contract provides opaque arena/expression handles, exact scalar and
function construction, arithmetic, inspection, substitution, differentiation,
deterministic reference-counted lifetime, and per-call status buffers. C and
C++ clients are compiled and the C client exercises cross-arena refusal,
exactness, buffer sizing, and releasing an arena before its expressions.

Issue #22 (native Python package) is complete as of 2026-08-10: the shared C
ABI now exposes native expansion, and the standard-library-only `ctypes`
package supports deterministic handles, exact arbitrary-size integers and
rationals, scalar coercion, expansion, substitution, structural equality and
hashing without importing SymPy. Source and wheel loading are documented, CMake
installs the package sources, and CI builds a wheel in a clean virtualenv. The
focused Python suite and full `fo test` pass.

Issue #23 (SymPy-compatible Python surface) is complete as of 2026-08-11:
`fortsym.sympy` is packaged and documented as a no-SymPy compatibility subset
with native kind-aware `Symbol`, `Add`, `Mul`, `Pow`, and `Function` classes,
explicit `Derivative`/`Subs` wrappers, operators, substitution, expansion,
differentiation, native simplification, and native `real`, `positive`,
`nonnegative`, and `nonzero` assumption facts. Unsupported algebra, matrix,
solver, calculus, assumptions, and option surfaces refuse by name. The focused
Python suite, C ABI assumption check, wheel build, and full `fo test` pass.

Issue #24 (documented Wolfram textual subset) is complete as of 2026-08-11:
`DIA_WOLFRAM` now has a published grammar/refusal table, stable printer/parser
round-trip coverage, and one-based source-positioned diagnostics for malformed
input. Representative forms agree with the independent Mathics oracle, while
the native test covers exactness, precedence, implicit multiplication, lists,
rules, patterns, associations, comments, and refusal paths. The GitHub issue is
now closed; native and Nix-wrapped suites both pass 38/38.

Issue #25 (bounded Wolfram command lowering) is complete as of 2026-08-11:
`fortsym_wl` separates parsing from command dispatch, validates supported
arguments, lowers results to native expressions, and reports unsupported
options and semantic forms by name. The compatibility document and focused
frontend tests define the supported subset and its differences. Larger
operation families remain independently tracked in #28–#43.

Issue #27 is complete as of 2026-08-11 for the bounded corpus contract. All
384 `.wl` sources have paired `.py` oracle modules in `fortsym-bench` commit
`48f80e5`, which also adds machine-readable per-outcome and per-backend
coverage reports, discovery/summary tests, and harness CI. The new
`corpus.yml` workflow runs SymPy, Mathics, and the native `fortsym-wl` runner
on every fortsym push and pull request, uploads the report, and attaches the
same report to published releases. CMake now builds that native runner
explicitly. The 472-test bench suite, workflow `actionlint`, native build,
and a four-binding native/oracle smoke audit pass; known oracle errors,
timeouts, refusals, and disagreements remain visible in reports rather than
being relabelled as parity.

Issue #11 remains a long-term tracker. On 2026-08-11 the native Fortran engine
gained a conservative exponential-normal-form zero fragment: products of
`Exp` terms combine exponents, `Exp[a + b]` factors, and integer powers of
`Exp` are reduced before the ordinary native simplifier runs. Exact symbolic
identities and nonidentities are tested independently; unsupported heads and
unsupported fractional periodic constants remain `UNKNOWN`. The bounded native engine is now stronger,
but SymEngine still supplies the broader exponential decision procedure, so
the tracker is not closed. The same native candidate path now also feeds the
existing exact multivariate polynomial cancellation/GCD layer, retaining only
strictly smaller verified forms such as `(x^2 - y^2)/(x - y) -> x + y` and
preserving the nonzero-denominator condition.
The next bounded slice adds a native univariate polynomial-factor candidate
for denominator-free expanded expressions, retaining it only when the
verified factored form is strictly smaller. `expand()` re-expands that
candidate before returning, while rational-function inputs remain available
to `Apart` and the existing integration rules. The native tests cover both
candidate selection and these operation-boundary contracts; the broader
external-CAS-independent engine tracker remains open.
The common `engine_t` contract now also exposes `factor`; native advertises
`CAP_FACTOR` and returns bounded, canonical factorisations with the same
nonzero-denominator condition used by simplification, while Yacas binds its
existing `Factor` operation to the shared method. This removes another
operation-specific dependency from callers without claiming that the full
multivariate factor engine is complete.
The native C ABI now exposes the same operation for denominator-free
polynomials. It refuses results that carry a cancelled-denominator condition,
because the current handle ABI has no condition field and must not silently
erase that domain information; the C test covers both the successful
factorisation and this refusal.
The standard-library-only Python facade now exposes the safe polynomial path
as `Expr.factor()` and `fortsym.factor()`, while the separate
`fortsym.sympy` compatibility layer keeps `factor` as an explicit refusal
outside the bounded polynomial subset; that subset is now covered by a
native factor implementation and tests for options and domain-condition
refusal.
The native exponential zero fragment now also decides exact Euler constants
`exp(i*n*pi)` for integer `n` and the exact half-integer turns
`exp(±i*pi/2)`/`exp(3*i*pi/2)`, while other fractional multiples and unsupported
periodic forms remain `UNKNOWN`; selected eighth-, sixth-, and twelfth-turn
roots are represented exactly with `sqrt(2)` and `sqrt(3)`. This removes another bounded periodic-constant
case from the external exponential oracle without claiming general complex
transcendental simplification. It also rewrites exact rational powers of
logarithms inside exponentials, including `exp(log(x))`, `exp(2*log(x))`, exact
rational powers, and additive logarithm factors; symbolic cofactors and
alternate `sqrt` spellings now share the rational-power form, while symbolic
cofactors remain outside the fragment. Native `zero_test` now also consumes the existing bounded
trigonometric-to-exponential rewrite, deciding the Pythagorean identity while
leaving unproved trigonometric forms unknown.
The independent native oracle also covers sine and cosine angle addition and
the corresponding hyperbolic-sine identity through the same bounded path. It
now folds the rational result before expanding only its numerator, then
re-normalises exponential products; this decides tangent addition while
preserving the bounded denominator and unsupported-form refusals. A final
normal-form pass after polynomial cancellation closes the corresponding
hyperbolic Pythagorean and `tanh` addition cases; complex tangent forms with
symbolic `i` retain the conservative `UNKNOWN` boundary.
The bounded limit polynomial helper also updates coefficient lists in place,
removing its array-temporary warnings without changing its refusal or numeric
limit behavior.

Issue #44 (plotting through fortplot) is complete as of 2026-08-11 for the
bounded adapter. `fortsym_plot` samples real curves, parametric curves, list
data, and fields, drops undefined curve samples without joining across a pole,
refuses partially undefined fields, maps the supported options, and delegates
rendering and panels to the pinned fortplot revision. The independent plot
family test covers logarithmic sampling, parametric curves, joined list data,
field refusal, `Show`, and explicit refusal of unsupported `Graphics` forms.
The remaining Wolfram plotting surface is still a named refusal or an upstream
fortplot capability question; fortsym does not grow a second renderer.

Issue #48 (verified-generation backend) is complete as of 2026-08-11 for the
bounded protocol slice. `fortsym_backend` provides a versioned canonical
expression payload, typed `PROVED`/`DISPROVED`/`UNKNOWN` evidence, escaped
machine-readable evidence records, and a kernel/cost handoff whose successful
source remains explicitly unproved until a consumer performs Fortran readback
and checks equivalence. Existing resource limits, readback, operation-cost,
provenance, JVP/VJP/HVP, and CUDA-target APIs remain the corresponding bounded
consumer surfaces. Lean/Why3 export, unrestricted tensor synthesis, and a
theorem-prover-grade certificate format are explicit follow-up boundaries.

Issue #28 (polynomial and rational algebra) is complete as of 2026-08-11:
the exact sparse multivariate layer is now registered in the library and test
builds. It provides together, cancel, apart, coefficient/collect/exponent,
exact division and GCD, numerator/denominator extraction, and bounded complete
factorisation. Independent tests check value preservation, divisibility,
coefficient reconstruction, factor multiplication, and explicit refusals for
unsupported high-degree and floating-point inputs.

Issue #29 (assumption ranges, domains, and scoped contexts) is complete as of
2026-08-11 for the bounded exact fact fragment: relation recording accepts
conjunctions, lower inequalities against exact integer/rational bounds, exact
zero/nonzero relations, and real/integer domain membership. Positive and
nonnegative implications are derived soundly, and contexts can be cloned for
scoped evaluation without leaking facts. Unsupported upper-bound inference
remains an explicit refusal.

Issue #30 (symbolic vectors and matrices) is complete as of 2026-08-11 for
the bounded exact matrix fragment: nested-list matrices support shape checks,
transpose, dot and cross products, Bareiss determinant, exact inverse, row
reduction, null space, rank, and bounded minors. Independent tests verify
determinant multiplicativity, two-sided inverse identities, transpose
involution, null-space/rank behavior, minors, and refusal of invalid shapes.

Issue #33 (complex-domain algebra) is complete as of 2026-08-11 for the
bounded direct API: exact and real-valued expressions support rectangular
splitting, `Re`, `Im`, conjugation, `Arg`, `Abs`, and `ComplexExpand`, with
unknown reality, branch-cut cases, zero arguments, and expansion blow-up
refused by name. An independent complex evaluator checks reconstruction,
realness of parts, conjugation, modulus/argument, and the refusal boundary.

Issue #36 (polynomial solving) is complete as of 2026-08-11 for the bounded
exact univariate fragment: rational-root extraction, exact quadratic roots,
repeated roots, complex conjugate pairs, and quartics whose roots are all
rational are verified by root-count and polynomial-reconstruction tests.
Non-rational irreducible cubics/quartics, systems, and non-polynomial inputs
remain named refusals rather than approximate solutions.

Issue #35 (series depth) is complete as of 2026-08-11 for the bounded native
series fragment: the existing Taylor engine now also exposes Laurent-series
construction for structurally recognised integer poles. It regularises the
pole and reuses the independent Taylor coefficient path, with tests covering
the principal part through positive order and explicit refusal of unknown
singular structures. Infinity, composition, inversion, and special-function
series remain outside this decision procedure.

Issue #34 (special functions) is complete as of 2026-08-11 for the bounded
exact identity fragment: native simplification proves `erf(0)`, `erfc(0)`,
small positive-integer Gamma values, `Gamma(1/2)`, and integer-order `J` and
`I` values at zero. The independent tests encode the defining identities and
cover the zero-order boundary. General special-function evaluation, analytic
continuation, asymptotics, and series remain outside this decision procedure.

Issue #37 (arbitrary-precision numerics) is complete as of 2026-08-11 for the
bounded expression-evaluation bridge: closed real expressions can return
MPFR-backed decimal text through 512 requested digits, supported closed
arithmetic/entire complex expressions return independent rectangular decimal
components, and `numeric_callable_t` exposes ordered real64 point evaluation
to a fortnum-style numerical algorithm. Free symbols, non-finite points,
branch-sensitive or unsupported heads, rigorous Arb enclosures, and the
quadrature/root/interpolation algorithms themselves remain explicit boundaries.

Issue #31 (native integration) is complete as of 2026-08-11 for the bounded
exact fragment: native integration now sends exact rational inputs through
partial fractions before applying its verified elementary rules. Distinct and
repeated linear factors, polynomial quotients, and the existing quadratic
arctangent/arcsine cases are covered; unsupported Hermite/Rothstein--Trager
shapes remain named refusals. Independent Simpson quadrature checks the new
antiderivatives.

Issue #32 (native limits) is complete as of 2026-08-11 for the bounded exact
limit layer: continuity substitution, capped L'Hopital reduction, rational
degree comparison at both infinities, and restricted exponential/power/log
growth ordering are implemented with two-sided domain checks. Independent
approach sampling verifies finite and infinite results, while poles,
oscillation, one-sided-only behavior, undecidable growth, and cap exhaustion
remain explicit refusals.

Issue #40 (public trig and power rewrites) is complete as of 2026-08-10:
`trig_expand`, `trig_reduce`, `trig_to_exp`, `exp_to_trig`, and guarded
`power_expand` are public operations returning rewritten expressions plus an
`ok`/diagnostic result. Independent numeric and exact-arithmetic tests verify
the rewrites, refusal paths, and assumption-sensitive power identities.

Issue #56 (Fortran renderer/parser round-trip) is complete as of 2026-08-10:
the fparse regression suite now checks an enumerated independent population
covering signed and rational literals, real kind suffixes, precedence, powers,
intrinsics, and deep expression chains. It also closes the exactness gap found
by that property: typed-real quotients emitted for exact rationals are recovered
as rational nodes in the Fortran dialect, while ordinary real quotients remain
real. The focused and full `fo test` runs pass.

Issue #57 (Fortran declaration and array readback) is complete as of
2026-08-10: `find_assignment` now selects initialized entities from attributed
declarations, including array specs and multi-entity declarations, while
rejecting pointer initialization. Fortran `[]` and `(/.../)` constructors parse
through the dialect parser, and `parse_fortran_array` returns their elements
with explicit refusals for nested and implied-do constructors. Typed
constructors are accepted when reading generated tables; the focused and full
`fo test` runs pass, including the CUDA emitter's compiler/no-device guard.

Issue #58 (typed constant-table emission) is complete as of 2026-08-10:
`emit_table` now accepts rank-one and rank-two `expr_t` and `quadratic_t`
tables, renders every element through the shared Fortran path, preserves
per-element comments, and emits typed constructors with safe continuation
layout. Independent compiler/run fixtures cover exact rationals, negative
full-precision values, matrix shape, and quadratic radicals; emitted rank-one
tables read back element-wise through `parse_fortran_array`.

Issue #60 (SymPy frontend scope) is complete as of 2026-08-10: the README and
dialect documentation record that `DIA_SYMPY` is an expression spelling for
the subprocess simplification adapter, while Python callers use the installed
package through the C ABI. A SymPy script session is not part of the supported
surface because the consumer inventory contains no derivation that requires
one; the bounded Wolfram session remains tied to the existing `.wl` corpus.
The Python package smoke tests verify that this route works without importing
SymPy.

Issue #62 (machine-readable symbolic cost record) is complete as of
2026-08-10: kernel operation counts now expose separate FLOP, structural
instruction, and FMA-candidate measures; `operation_cost` attributes totals
and per-root records while aggregating named function heads; and
`emit_kernel(..., cost_record=...)` supplies the deterministic
`fortsym.operation_cost.v1` JSON record. The codegen tests hand-verify shared
DAG accounting, FMA semantics, per-root attribution, named transcendental
counts, and the optional kernel handoff; the record documents that it is
symbolic metadata rather than a machine-disassembly claim.

Issue #65 (accuracy instrument) is complete as of 2026-08-11:
`measure_accuracy` evaluates each declared sample through the high-precision
MPFR path, compares the caller-owned binary64 kernel result in local ULPs, and
reports maximum and RMS error, the maximizing input, retained reference and
observation values, and the relative derivative-based condition number when
it is defined. The sample matrix, domain, sequence, precision bound, and
kernel refusal count are retained in the report. Its test uses an independently
constructed one-ULP perturbation rather than reusing the instrument's result.
The GitHub issue is now closed; native and Nix-wrapped suites both pass 38/38.

Issue #66 (precision as an emission choice) is complete as of 2026-08-11:
both kernel emitters expose explicit `PRECISION_REAL64`, `PRECISION_REAL32`,
and `PRECISION_MIXED` modes. The single-precision path emits `real32`/`float`
arguments, temporaries, literals, and math functions; the mixed path keeps
arithmetic lowered while placing outputs at a declared `real64` boundary.
Generated real32 source is compiled and run against an independently written
Fortran oracle, and values that cannot remain finite in real32 are refused
before emission. The default remains the historical real64 output.
The GitHub issue is now closed; native and Nix-wrapped suites both pass 38/38.

Issue #70 (the short Fortran form) is complete as of 2026-08-11: `use fortsym`
re-exports the explicit arena and expression surface with intrinsic-safe names,
character assignment creates symbols, `symbols` fills scalar names without an
array temporary, and `fortsym_reset` invalidates stale handles through arena
generation tracking. The default arena is documented as single-threaded and
`fortsym_default_arena()` bridges convenience and explicit APIs. An existing
acquisition generator uses the short form, while the independent convenience
test compares canonical structures across arenas, checks mixed construction,
and verifies reset invalidation. The Nix build and all 39 CTest cases pass, the
rewritten consumer compiles, and `fo build` exits successfully after its stale
mixed-toolchain build directory was reconfigured.

Issue #71 (guidance for the short form) is complete as of 2026-08-11: the
README, Fortran API reference, architecture note, and feature matrix point
callers to `use fortsym` for ordinary single-threaded derivations and retain
explicit arenas for concurrency, embedding, and independent state. The API
reference states the identifier naming rule and the no-LaTeX-in-symbol-names
boundary. The external `symbolic` skill now opens with character assignment,
keeps `parse_expr` explicit, and retains the explicit-arena example in its
proper scope. Its prompts-repository commit is
[`9d7e53c`](https://github.com/krystophny/prompts/commit/9d7e53c). The GitHub
issue is now closed after the documentation checks and both repository pushes.

Build hygiene is also part of the current regression gate: the global
`-Warray-temporaries` pass is clean across the library, applications, and
tests, and the Nix CI jobs add their dependency library directories to the
runtime search path before loading the native library. The CUDA emitter probe
uses a minimal host toolchain environment and exits cleanly when no device is
available, while still compiling the generated CUDA source when `nvcc` exists.

Issue #59 (Wolfram coverage against existing material) is complete as of
2026-08-11. The real flux-pumping derivation now measures 40 evaluated and 10
explicitly refused top-level bindings, or 80% reachable coverage. Bounded
function-valued rules, canonical `Pattern[name, Blank[]]` matching, and
real-valued `Table` ranges were the evidence-led gap closures. The exact run
was rerun successfully, the Wolfram frontend regression test passes, and the
binding names and refusal diagnostics are recorded in
`doc/wolfram-coverage-39.md`. Definite integration and numeric plotting remain
named refusals for separate follow-up work.

Issue #45 is complete as of 2026-08-11. The engine contract now exposes a
call-local `resource_limit_t` with node-budget and monotonic-deadline fields;
native simplify, expand, series, and solve preserve the original expression
and return an operation-specific refusal when the bound is crossed. Recursive
simplification, expansion, multinomial enumeration, and distribution charge
the same limit, while separate engine instances retain independent limits.
The native oracle checks preflight and recursive node refusals, expired
deadlines, expression preservation, and recovery after a refused call. The
policy is documented in `doc/architecture.md`.

Issue #42 is complete as of 2026-08-11. Fortran kernel emission now uses one
declared function map for standard intrinsics and the FortNum special runtime:
`loggamma` maps to `log_gamma`, ordinary and modified Bessel heads map to their
Fortran or `fortnum_special` entry points, and the generated source declares
the required module imports. Integer Bessel orders are rendered as integer
literals, unknown heads are hard refusals, and `fortran_representable` rejects
them before printing. A compiler/run fixture checks exact special-function
values independently; the current FortNum release has no elliptic entry point,
so elliptic heads remain explicit refusals until that runtime contract exists.

Issue #41 is complete as of 2026-08-11. `kernel_spec_t%bindings` now maps an
opaque applied head and its canonical derivative multi-index to a consumer
Fortran expression. Replacements support procedure calls through the `%args%`
marker as well as array elements and derived-type components; unbound heads
remain hard refusals. The kernel test compiles the generated snippet and checks
the procedure value, first derivative, and symmetric mixed Hessian slot against
an independent evaluator and finite differences.

Issue #38 is complete for the evidenced conditional fragment as of
2026-08-11. Symbolic `Piecewise`, `If`, and `Boole` nodes now simplify exact
numeric predicates, differentiate branchwise while preserving conditions, and
emit ordered Fortran `merge` expressions. Malformed branch lists, undecidable
predicates outside the emitted logical relation vocabulary, and boundary
derivative claims remain explicit refusals or caller obligations. The compiler
fixture samples both sides of every branch boundary and checks the generated
kernel against the direct definition.

## Performance contract

Performance is a release criterion, not a follow-up.

| Workload | Target | Current |
|---|---|---|
| expand, diff, simplify on the SymEngine benchmark inputs | within **1.5×** of SymEngine 0.14.0 | 2.66× on `(x+y+c)^7` |
| any corpus script | faster than SymPy on the same input | not yet measured |
| generated kernel runtime | at or below the hand-written routine it replaces | not yet measured |

Rules that follow from that contract:

- Sparse multivariate arithmetic uses **heap-based multiplication and division**
  (Monagan & Pearce 2007, 2010), which is the current state of the art for
  sparse distributed polynomials and beats geobucket at the sizes the corpus
  reaches. Dense univariate falls back to **Kronecker substitution** onto FLINT
  `fmpz_poly`.
- The arena stays hash-consed with cache-dense node layout. No allocation in
  inner loops; memoisation keyed on interned ids.
- Every claim cites a pinned result file under `benchmark/results` produced with
  fixed CPU affinity and a documented frequency governor. A run without those is
  diagnostic and cannot support a release statement (`doc/benchmarks.md`).

## Architecture and routing

Best-of-breed means choosing the strongest appropriate engine for each domain.
The native evaluator remains the exact expression and dispatch layer. It does
not absorb numerical algorithms that belong in a numerical library.

| Domain | Owner | Boundary |
|---|---|---|
| exact expressions and algebraic normalization | fortsym arena and native engine | preserves exact nodes and explicit refusal |
| sparse and dense polynomial algebra | modular algorithms with FLINT-backed kernels | promoted only after independent polynomial identities pass |
| exact elementary integration | native integration modules | bounded rules first, Hermite and Risch-class work next |
| numerical integration and fitting | fortnum | exposed through `NIntegrate`, `FindRoot`, `Interpolation`, and `Fit` adapters |
| arbitrary-precision and certified numerics | Arb or MPFR-backed numerical layer | values carry precision or error information |
| plotting and rendering | fortplot | fortsym samples expressions and passes structured data |
| differential comparison | SymPy and Mathics subprocess oracles | raw outputs and verdicts are cached separately |

The boundary is semantic. `Integrate` must preserve symbolic parameters and
exact identities. `NIntegrate` may dispatch to fortnum only after the expression,
domain, parameters, precision, and error policy are numeric and explicit.

## Measured state

Latest measured corpus-wide state, measured 2026-08-03, `fortsym-bench` at
commit `9d3c41d` with native commit `ab97c34` and comparison cache version 21:

This table is a committed native baseline. It is updated only after the root
backend and benchmark harness revisions used to produce it have been committed.
Interrupted experiments and uncommitted generated artifacts do not change the
reported state.

| | |
|---|---:|
| native cache rows with successful results | **378 / 384 (98%)** |
| native rows refusing with a named construct | 3 |
| native rows exceeding the time budget | 3 |
| native rows ending in a runner error | 0 |
| crashes | **0** |
| v74 native/SymPy audit after cache and native parity changes | 1:10.89 / 1.61 GiB |
| v75 cache-preserving oracle refresh | 6.10 s / 491 MiB |
| latest v75 warm compact raw-output and verdict audit | 0.81 s / 491 MiB |
| latest v76 UV warm compact raw-output and verdict audit | 0.90 s / 490 MiB |
| latest v77 UV warm compact raw-output and verdict audit | 1.06 s / 488 MiB |
| latest v78 UV warm compact raw-output and verdict audit | 1.00 s / 488 MiB |
| latest v79 UV warm compact raw-output and verdict audit | 1.33 s / 489 MiB |
| latest v80 UV warm compact raw-output and verdict audit | 1.10 s / 488 MiB |
| latest v81 UV warm compact raw-output and verdict audit | 1.06 s / 488 MiB |
| v81 native cache-preserving rebuild audit | 1:32.48 / 1.54 GiB |
| latest v82 UV warm compact raw-output and verdict audit | 1.06 s / 489 MiB |
| latest v83 UV warm compact raw-output and verdict audit | 0.93 s / 489 MiB |
| latest v84 UV warm compact raw-output and verdict audit | 0.95 s / 489 MiB |
| latest v85 UV warm compact raw-output and verdict audit | 1.79 s / 490 MiB |
| latest v86 UV warm compact raw-output and verdict audit | 1.11 s / 489 MiB |
| latest v87 UV warm compact raw-output and verdict audit | 1.08 s / 489 MiB |
| latest v88 UV warm compact raw-output and verdict audit | 0.93 s / 489 MiB |
| latest v89 UV warm compact raw-output and verdict audit | 0.96 s / 489 MiB |
| latest v90 UV warm compact raw-output and verdict audit | 0.94 s / 489 MiB |
| latest v91 UV warm compact raw-output and verdict audit | 0.92 s / 489 MiB |
| latest v92 UV warm compact raw-output and verdict audit | 1.01 s / 489 MiB |
| latest v93 UV warm compact raw-output and verdict audit | 1.11 s / 489 MiB |
| latest v94 UV warm compact raw-output and verdict audit | 1.04 s / 489 MiB |
| latest v95 UV warm compact raw-output and verdict audit | 0.89 s / 489 MiB |
| latest v96 UV warm compact raw-output and verdict audit | 0.89 s / 489 MiB |
| latest v97 UV warm compact raw-output and verdict audit | 0.86 s / 489 MiB |
| latest v98 UV warm compact raw-output and verdict audit | 0.93 s / 489 MiB |
| latest v99 UV warm compact raw-output and verdict audit | 1.01 s / 489 MiB |
| latest v100 UV warm compact raw-output and verdict audit | 0.93 s / 489 MiB |
| latest v101 UV warm compact raw-output and verdict audit | 0.99 s / 489 MiB |
| latest v102 UV warm compact raw-output and verdict audit | 1.44 s / 489 MiB |
| latest v103 UV warm compact raw-output and verdict audit | 1.10 s / 489 MiB |
| latest v104 UV warm compact raw-output and verdict audit | 1.09 s / 489 MiB |
| latest v105 UV warm compact raw-output and verdict audit | 1.03 s / 489 MiB |
| latest v106 UV warm compact raw-output and verdict audit | 0.87 s / 489 MiB |
| latest v107 UV warm compact raw-output and verdict audit | 1.02 s / 500 MiB |
| latest v108 UV warm compact raw-output and verdict audit | 1.10 s / 500 MiB |
| latest v109 cache-rebuild raw-output and verdict audit | 62.60 s / 553 MiB |
| latest v111 comparison-cache audit | 68.03 s / 542 MiB |
| latest v122 cached whole-corpus audit | 16.99 s / 489 MiB |
| v124 native-fingerprint refresh | 100.03 s / 803 MiB |
| latest v124 cache-only whole-corpus audit | 0.98 s / 489 MiB |
| final Part-enabled native audit | 78.42 s / 803 MiB |
| v108 cache-preserving native refresh | 121.39 s / 1.61 GiB |
| peak RSS during that refresh | 3.04 GiB |
| latest warm compact raw-output and verdict audit after SymPy v17 | 1.24 s / 456 MiB |
| current v18 focused native/SymPy/Mathics slice (11 scripts, one worker) | 42.39 s / 804 MiB |
| current v19 focused `Thread` slice (16 scripts, one worker) | 17.43 s / 508 MiB |
| current v20 focused positive-level `Map` slice (6 scripts, one worker) | 1.00 s / 404 MiB |
| current v21 focused numeric `Piecewise` slice (6 scripts, one worker) | 0.85 s / 403 MiB |
| current v22 focused numeric `Boole` slice (3 scripts, one worker) | 2.91 s / 404 MiB |
| current v23 focused numeric `Which` slice (5 scripts, one worker) | 0.71 s / 404 MiB |
| current v24 focused bounded `TrigReduce` slice (6 scripts, one worker) | 0.72 s / 404 MiB |
| current v25 focused symbolic 2x2 `Solve` slice (1 script, one worker) | 0.78 s / 404 MiB |
| current v26 focused exponential-product `Integrate` slice (3 scripts, one worker) | 0.78 s / 404 MiB |
| current v27 focused nested-limit `Integrate` slice (1 script, one worker) | 0.77 s / 402 MiB |

Read that honestly: 99% is *native scripts that ran and emitted bindings*, not
correctness. Scoring against an oracle is what makes it coverage.

The final bounded binding-level audit reports 4,034 agreements, 388 declared
differences, 7 unsupported outcomes, 74 timeouts, 59 errors, 2 unavailable
oracle rows, 205 oracle disagreements, and 345 oracle-missing bindings across
4,974 emitted bindings from 384 corpus sources; 375 scripts completed. It took
78.42 seconds at 822,224 KiB RSS (about 803 MiB). The native commit
`ab97c34` adds bounded literal/nested-list integer and `All` `Part` selectors;
unsupported symbolic, negative, out-of-range, and over-bound selectors still
refuse explicitly. Mathics3 10.0.1 is installed through UV, and raw SymPy,
Mathics, native, and comparison results remain cached. This is the stopping
point for this slice: the corpus is not yet at 100% parity, and the remaining
differences, oracle limitations, timeouts, errors, and missing bindings are
intentionally reported rather than relabeled as agreement. The
v43-v124
parity batch adds source-faithful ECNL equation strings, numeric validity
estimates, Maxwell/flux-pumping companions, math10y and Suydam recoveries,
large-step LTE reconstruction, Sympl3 field forms, normal-stability numeric
parity, math3y, Cartesian-primitive, math8y, perpendicular-block, math10y,
math6-1y, cylinder-spectrum, math11y, Mercier, Appendix-B, math14y, and math15y
recoveries, with independent behavioral tests. The v59 native work fixes implicit
scientific-literal precedence and adds vector-matrix `Dot` evaluation; v60 adds
bounded numeric `FindRoot`. The v70 cycle adds exact identity-matrix fractional
powers and source-faithful numeric `Abs`/Mathics root handling, with selective
SymPy cache invalidation. The v71 cycle adds bounded `Do` assignment loops and
six source-faithful `math10y` companion recoveries; v74 adds named-derivative
lowering, two joined-plot recoveries, and bounded source-to-Fortran `Do`
lowering. The v75 refresh records the named-derivative companion alignment; it
does not increase the agreement tally. The v76 cycle adds source-faithful
`math10y`/`math11y` recoveries and bounded stateless `For` lowering in the native
source-to-Fortran translator. The v77 cycle adds a source-faithful `math15y`
recovery and bounded stateless `While` lowering. The v78 cycle adds three
source-faithful `math10y` ellipsoid bindings and a cylinder-spectrum recovery.
The v79 cycle adds source-faithful `jDotB` and memo34 radial-product bindings.
The v80 cycle adds final `math10y` `Which` coverage and large-step recurrence
bindings; these pass focused tests but do not change the scored tally. The v81
cycle adds dynamic bounded `If` codegen, six `vector2d` fields, and a flux
access-condition correction. The v82 cycle adds a memo37 factored-derivative
binding and a source-faithful cylinder-spectrum derivative tree. The v83 cycle
adds a `math10y` theta binding, two-component density contraction, and
source-faithful perpendicular-block projection. The v84 cycle adds a symbolic
`peng` Dot preservation, physical-weighted energy, and sequential SWR recovery;
the unresolved-class counts improve, but the agreement tally does not. The v85
cycle adds bounded integer `While` increment/decrement lowering. The v86 cycle
adds `math10y` fourth-root coverage, a `sympl3_` denominator correction, and a
memo34 Bessel-kernel branch. The v87 cycle adds a Gaussian-integral `math10y`
binding, the heat-kernel `k3`, and a `gc_drift` Christoffel tensor. The v88
cycle adds the `math10y` derivative binding and `gc_drift` `gradBmod`. The v89
cycle adds second-derivative `math10y` coverage, a source-faithful pressure
slope, and scalar reassignment codegen. The v90 cycle adds strict bounded
`For` forms and another source-to-Fortran coverage gate. The v91 cycle adds
source-faithful `math10y` weighted second derivatives, heat-kernel tails, and a
two-component flux-temperature-slope binding. The v92 cycle adds transformed
`math11y` ODEs, `gc_drift` gradients, a large-step fast-free binding, four gvec
validation bindings, and a bounded Fortran adapter test. The v93 cycle adds
heat, gvec export, `math10y`, and archive `math6-1y` recoveries. The v94 cycle
adds gvec Fourier, archive `math6-1y`/`math15y`, `math11y`, and two-component
energy-identity recoveries. The v95 cycle adds ECNL, zero-family, GH NTV,
flux-handedness, and project-NTV recoveries. The v96 cycle adds geomint and
flux-pumping agreements plus source-faithful vector2d, Bacc, and cylinder-kernel
corrections; one oracle disagreement is newly exposed. The v97 cycle adds a
`math10y` Fourier binding, two flux44 bindings, two symbolic parity fixes, and a
memo37 difference reduction; the latter exposes one oracle disagreement. The v98
cycle adds gvec `II_tt`, two-component `qField`, and another `math10y` binding;
the archive plot recovery remains a declared native-serialization difference. The
v99 cycle adds gvec `II_zz`, two-component `fluxTslope`, another `math11y` binding,
and the bounded `Do` source-to-Fortran adapter. The full adapter inventory emits 1
of 384 sources and refuses 383. The v100 cycle adds ECNL, `math13y`, `math15y`,
and appA flux-coordinate recoveries. The v101 cycle adds two-component `jDotB`,
gvec Fourier headers, an archive `math6-1y` binding, and a bounded `While`
adapter; the full adapter inventory remains 1 translated / 383 refused. The v102
cycle adds large-step Hessian precision, Bacc Hopf/impedance parity, vector2d
`g12`, geomint `Aphi`, and flux impedance parity. The v103 cycle adds large-step
`gIS`, another `math10y` Fourier binding, archive `math6-1y`, two-component
kernel weighting, and Bacc uniform-loss parity. The v104 cycle adds project/GH
notebook bindings, an ECNL orbit node, archive `math6-1y`, appA flux coordinates,
and gvec Fourier-row parity. The v105 cycle adds project/GH notebook
coefficients, gvec Fourier/validation bindings, and the `gc_drift` remainder
recovery. The v106 cycle resolves four oracle disagreements across memo37,
cylinder, peng, and math5y bindings. The v107 cycle fixes Mathics
arbitrary-precision InputForm parsing, invalidates stale comparison verdicts,
and preserves compatible legacy Mathics cache rows; only 13 Curl-sensitive
rows required refresh. The v108 cycle adds exact-list native `Position` and
`Union`, accepts standalone source-to-Fortran `Null` separators, recovers the
Bacc/Rosa/Posch derivation and Sympl3 orbit clusters, and classifies malformed
Mathics list arithmetic as an explicit difference. A trial
Levi-Civita native
lowering was
reverted after its measured corpus regression. The benchmark harness now
invalidates native cache rows at version 2 and comparison verdicts at version 20.
The v109 cycle adds bounded archive-tu math6-1y and vector2d companion recovery,
equation-aware equivalence for explicitly declared rearrangements, and the
source-faithful literal-only native FileNameJoin subset. Its aggregate audit
reuses the v108 native rows; the FileNameJoin behavior is covered by focused
Fortran tests.
The v37
phase-transform and flux-coordinate companion translations, followed by the
v38 NAE/DESC and Appendix-B translations, recovered 42 agreements and 52
previously oracle-missing bindings in the warm audit while resolving one SymPy
timeout. The v39 math6-2y, math11y, two-component-energy, and
dynamo-diagnostics companions recover a further 27 agreements and 25
oracle-missing bindings; the dynamo bridge reduces its focused runtime from
about 29 seconds to 1.1 seconds. The
v40 memo-feedback companion recovers the source magnetic-norm binding with an
independent test, adding one agreement and removing one oracle-missing
binding. The v41 cylinder-spectrum companion recovers the source-faithful
force-balance and pressure-slope bindings with an independent test, adding one
agreement and removing one oracle-missing binding. The v42
Bacc/Rosa/Posch companion recovers the source magnetic-field integral with an
independent numerical quadrature test and removes one oracle disagreement. The
v43 batch is committed and cached, but the target remains open until the
declared native subset and the available oracle overlap agree.

The current native collection slice includes bounded `Array`, `ConstantArray`,
and `Outer` expansion, bounded exact `Range`, `DiagonalMatrix`, rectangular
`Diagonal`, bounded `LegendreP`, bounded exact `CharacteristicPolynomial`
for explicit square matrices up to dimension 16, and bounded non-negative
`MatrixPower`, bounded explicit-list `Total`, bounded full-rank numeric
`PseudoInverse`, diagonal/zero numeric `SingularValueList`, numeric `Max`/`Min`,
exact
`RowReduce`, `NullSpace`, `MatrixRank`, bounded exact `LinearSolve` and
`Minors`, `Exponent` including exact fractional monomials, computed scalar
`Solve` rule replacement, bounded list `Thread`, bounded positive-level `Map`, bounded numeric
`Piecewise` branch selection, numeric `Boole` and `Which` conditions, bounded
`TrigReduce`, bounded symbolic 2x2 `Solve`, bounded polynomial
gcd/quotient/remainder and rational
numerator/denominator extraction, verified exponential-product `Integrate`,
structural `Length`, recursive `Flatten`, dynamic exact dimensions,
bounded block-matrix `ArrayFlatten`, bounded Cartesian `Curl`, and opaque preservation for unsupported
dimensions or computed heads. The independent tests cover literal, rational,
symbolic, canonical empty-list, and bounded-preserved forms. Requested-
precision `N` now has a bounded native path for 17--512 decimal digits, with a
named refusal above that limit; list `Append`/`Join`, bounded list selectors,
rectangular `Diagonal`, bounded `LegendreP`, bounded `CharacteristicPolynomial`,
scalar `Joined -> True/False` overrides for `ListPlot`/`ListLinePlot`, and
multiline dot-product continuation also have
independent tests. Unsupported selector and matrix shapes remain opaque.
Relative to the prior numeric baseline, the parser slice added seven
agreements, removed one binding error, and exposed two newly visible bindings
from the translated assignment stream; the subsequent selector slice preserved
the aggregate tally, and the diagonal slice added one agreement while removing
two declared differences after both translators preserved unsupported symbolic
shapes. Its downstream `Last[Diagonal[s]]` case is correctly oracle-missing.
The Legendre slice added two agreements and removed two declared differences.
The characteristic-polynomial slice passes independent determinant tests and
the two-file corpus probe, but does not change the aggregate tally: one source
reassigns its `cp` binding later to plotting `Point` values, while the mapped
form in the other source remains outside the translated assignment subset.
The MatrixPower slice passes independent multiplication and identity tests;
the measured corpus rows still have no scored increase because their exposed
uses are inside unsupported `MatrixForm`/negative-power paths.
The Coefficient slice added independent SymPy lowering for `Coefficient` and
`CoefficientList`, native bounded multivariate `CoefficientList`, and eight
agreements while removing eight differences and one oracle disagreement. The
following Solve slice normalizes single-variable roots to Wolfram rule lists,
adds one agreement, and removes one oracle disagreement. The bounded
`FoldList[Plus, initial, list]` slice then removes one declared difference;
the refreshed native collection also makes one previously hidden binding an
agreement, for a net gain of two agreements and one fewer difference. Cache
version 19 keeps unaffected older-version SymPy rows reusable, including the
version-16 diagonal/extrema rows and older quoted-string rows, instead of
forcing another broad oracle refresh.
The bounded `ArrayFlatten` slice then evaluates rectangular block matrices and
adds three agreements while removing three declared differences. The
quoted-string translator alignment then maps SymPy string atoms to the native
comparison hash, adds 51 agreements, and removes 45 differences plus six oracle
disagreements. The bounded native `Total` slice then adds three agreements and
removes four differences across explicit scalar and vector-list sums. The
bounded full-rank numeric `PseudoInverse` slice adds two agreements and removes
two differences in the duplicated matrix-course cases. The diagonal/zero
`SingularValueList` and numeric extrema slice then adds seven agreements and
removes seven differences. The SymPy v17 transition adds bounded `Exponent`,
`PolynomialGCD`, `PolynomialQuotient`, `PolynomialRemainder`, `Numerator`, and
`Denominator` lowering; it turns one native binding into an agreement and
exposes one non-polynomial form as oracle-missing, for a net aggregate change
of one agreement, two fewer differences, and one more oracle-missing binding.
The v18 transition then lets serialized SymPy `Rule`/`RuleDelayed` heads feed
`ReplaceAll` and preserves exact fractional-monomial `Exponent` results. It
refreshed four affected SymPy rows in 10.46 seconds at 399 MiB RSS. The
current 11-script focused audit with the rebuilt native runner took 42.39
seconds at 804 MiB RSS and confirmed five computed-Solve/fractional-Exponent
bindings as agreements. The v19 bounded `Thread` transition then refreshed 16
SymPy rows in 16.51 seconds at 399 MiB RSS. Its current 16-script focused
audit took 17.43 seconds at 508 MiB RSS, converted two native `Thread`
bindings from differences to agreements, and reported 249 agreements, 58
differences, 1 unavailable oracle row, 1 timeout, 5 errors, 26 oracle
disagreements, and 58 oracle-missing bindings. These focused slices do not
close the remaining parity gap. The v20 positive-level `Map` transition then
refreshed six SymPy rows in 4.07 seconds at 403 MiB RSS. Its six-script focused
audit took 1.00 second at 404 MiB RSS; the selected rows were dominated by
plotting/file-I/O or Mathics failures, so the slice did not change the scored
native tally. The v21 bounded numeric `Piecewise` transition then ran the same
six-script focused audit in 0.85 second at 403 MiB RSS. It preserved 21
agreements, 6 differences, 3 unavailable oracle rows, 2 oracle disagreements,
and 3 oracle-missing bindings, with no scored native tally change because the
slice was dominated by symbolic/IO limitations outside this subset. The
v22 numeric `Boole` transition refreshed three SymPy rows in 2.91 seconds at
404 MiB RSS. Its focused audit preserved 21 agreements, 7 differences, 1
unavailable oracle row, 1 oracle disagreement, and 1 oracle-missing binding,
with no scored native tally change because all exposed Boole uses remain inside
larger symbolic or untranslated-function gaps. The
v23 numeric `Which` transition then served a five-script warm slice in 0.71
second at 404 MiB RSS. It preserved 60 agreements, 16 differences, 1 timeout,
1 unavailable oracle row, 3 oracle disagreements, and 73 oracle-missing
bindings; its cached timeout/unavailable outcomes were not rerun. The
v24 bounded `TrigReduce` transition then served a six-script warm slice in
0.72 second at 404 MiB RSS. It preserved 101 agreements, 48 differences, 3
unavailable oracle rows, 1 timeout, and 103 oracle-missing bindings; its cached
timeout was not rerun. The v25 bounded symbolic 2x2 `Solve` transition then
served the exposing corpus script in 0.78 second at 404 MiB RSS. It produced
33 agreements, 10 differences, 1 unavailable oracle row, and 1 oracle-missing
binding, with no timeout or runner error. The
v26 verified exponential-product `Integrate` transition then served a
three-script slice in 0.78 second at 404 MiB RSS. It produced 5 agreements, 1
difference, 1 unsupported backend outcome, 1 unavailable oracle row, and 1
oracle disagreement, with no timeout or runner error. The
v27 native definite/multiple-`Integrate` transition then evaluated the measured
nested-limit script in a warm one-worker audit in 0.77 seconds at 402 MiB RSS.
Native and SymPy now produce the complete outer-to-inner result; Mathics
retains a partial unevaluated result, so the three bindings remain explicit
oracle disagreements rather than being scored as native errors. The
subsequent native plotting slice adds independent per-dataset
`Joined -> {True, False}` coverage for `ListPlot` and `ListLinePlot`; the
focused plotting suite passes, while the broader plotting family remains open.
The highest-impact work remains the plotting family, `Solve` beyond
scalar linear cases, polynomial heads, and `NDSolve`; the bounded symbolic
`DSolve` slice is now covered below.

### The oracle ceiling (#47)

Issue #47 is complete as of 2026-08-11: the original 39% ceiling is
superseded by the refreshed benchmark measurement below, and the reporting
rule is now part of the roadmap. The remaining ceiling-raising work belongs
to the corpus tracker (#27), oracle defect reports, or a future third-oracle
task rather than this finding.

That number is now established, and it changes the target.

| | scripts | share |
|---|---:|---:|
| Mathics produces results | 255 | 66% |
| fortsym-wl completes | 379 | 99% |
| **Mathics and fortsym-wl both complete** | **252** | **66%** |

Mathics fails or is unavailable on 129 scripts: 60 errors, 69 timeouts, and
no unavailable rows. Those outcomes are cached by source digest, runner
version, and executable fingerprint, so later native audits do not rerun the
oracle.

**100% coverage is unreachable with Mathics as the sole oracle**, and no amount
of fortsym work changes that: 145 natively completed scripts currently have no
successful Mathics result to compare against. The benchmark now also compares
native results against SymPy when Mathics is unavailable and requires both
oracles to agree before scoring a shared binding. Raising the ceiling means
completing the SymPy translation/runtime, reporting the Mathics defects
upstream, or adding a third oracle for the overlap.

Until it moves, every coverage number states its denominator. "99% complete
natively" and "61% share a successful Mathics result" are different
claims.

The historical full refresh includes the SymPy refresh required by the
translator cache-version change and took 4:54. The latest LegendreP refresh of
the SymPy oracle took 6:59.96 with two workers and a 3.86 GiB peak RSS. The
quoted-string translator refresh took 2:00.16 with a 543 MiB peak RSS. The
subsequent native `Total`/`PseudoInverse`/`SingularValueList` audit took 1:14.19
with a 3.04 GiB peak RSS; the v16 SymPy transition refreshed 22 rows in 8.93
seconds; the v17 polynomial transition refreshed eight rows in 11.28 seconds;
the v18 Solve-rule/fractional-`Exponent` transition refreshed four rows in
10.46 seconds at 399 MiB RSS; the v19 bounded `Thread` transition refreshed 16
rows in 16.51 seconds at 399 MiB RSS. The latest warm audit takes 0.56 seconds
at 344 MiB RSS. The v20 positive-level `Map` transition refreshed six rows in
4.07 seconds at 403 MiB RSS.
Once raw results and comparison verdicts are cached, the same full audit takes
0.56 seconds at 344 MiB RSS. Future compatible cache transitions retain
unaffected older-version rows. These are
harness measurements, not a
capability comparison: Mathics evaluates integrals fortsym refuses, and the native path
still has one 60-second timeout.

What running the corpus has already bought, none of which was found by tests:

- A parser segfault on `-Inverse[g] . c`, from negating a term that had already
  failed to parse. Five scripts crashed; now none do.
- `wl_eval` dispatched only on `NK_FUNC`, so `1 + Integrate[x, x]` reported the
  unevaluated `Integrate` as though it were the answer. Unimplemented heads
  nested in arithmetic now refuse.
- The `Series` order convention, off by one against Mathics in both directions.
- Implicit multiplication, the single largest parse-refusal cause.
- Refusing `Transpose[jac]` for a symbolic `jac`, which disagreed with the
  oracle on a *correct* answer — Mathics leaves it unevaluated too.

The current native baseline is 2 named refusals and 1 timeout at script level;
binding-level unsupported constructs remain tracked by the benchmark report.

### What the corpus says to build next

Refusals grouped by cause, measured rather than guessed. This is the list that
orders the work, and it has repeatedly disagreed with intuition:

| Cause | Refusals |
|---|---:|
| parse | 518 → 279 |
| plotting family (Show, Plot3D, ContourPlot, Graphics, ListPlot, …) | ~300 |
| `Solve` beyond the scalar linear case | 78 |
| definite and multiple `Integrate` | ~123 |
| `N` beyond the bounded 17--512 digit path | refresh the refusal count |
| polynomial heads (`Coefficient`, `Together`, `Factor`, …) | ~60 |
| `NDSolve` (the bounded symbolic `DSolve` slice is covered below) | 45 |

Two findings worth keeping:

- **Plotting is the largest single construct family**, mostly from teaching
  material, and fortplot already provides contour, contourf, pcolormesh,
  surface, scatter, streamplot and quiver. Nothing upstream is missing.
- **239 of the 518 parse refusals were one bug**, not missing syntax: a
  statement was split at every depth-zero newline, so a derivation broken after
  a trailing operator was truncated into a prefix that still parsed. Measuring
  causes rather than counting messages is what found it.

### How defects are found here

Every module added since the corpus went live has been written, then attacked
by an independent reviewer whose brief is to produce a wrong answer with real
program output, not to approve. That pass has found, among others: an
antiderivative emitting `log` without an absolute value; a positivity predicate
treating `exp(z)` as positive for imaginary `z`, reintroducing the very
`sqrt(x*y)` sign error its own docstring quoted; a limit deciding the
power-domain rule from an exponent *after* substitution, asserting a finite
limit for a function undefined on both sides; a summation guard defeated by
integer overflow in its own term count.

None of these were caught by the modules' own tests, all of which passed. The
lesson is recorded here because it sets the standard for new work: a passing
test written by the same author as the code is weak evidence, and the review
pass is not optional.

## Milestones

Ordered by corpus impact. Counts are call sites across the 384-script corpus.

### M1 — Polynomial and rational core (#28) · 2189 sites

Issue #28 is complete for the bounded exact fragment above. The implementation
uses checked rational arithmetic, sparse generator polynomials, primitive PRS
GCD, Yun square-free decomposition, rational-root factorisation through degree
three, and exact partial-fraction recombination. Higher-degree factorisation
and floating-point coefficients remain named refusals.

Content, primitive part, exact division, GCD, square-free decomposition,
resultant, `cancel`, `together`, `apart`, `factor`.

- GCD: **sparse modular** (Zippel 1979) with **dense modular** (Brown 1971) for
  low variable counts, and **GCDHEU** (Char, Geddes & Gonnet 1989) as the fast
  path for small inputs. Monagan & Wittkopf (2000) for the sparse-modular
  refinements.
- Univariate factorisation: **Zassenhaus** with **van Hoeij lattice
  reduction** (van Hoeij 2002) to kill the exponential recombination case.
- Multivariate factorisation: **EEZ / Wang** multivariate Hensel lifting
  (Wang 1978).
- Oracle: divisibility, Bezout, and finite-field evaluation — not another CAS.

### M2 — Assumptions (#29) · 895 sites

Issue #29 is complete for the bounded exact fragment above. The public
assumption context records sound lower-bound and domain implications, and a
clone is the scope boundary for nested transformations. General inequality
solving and upper-bound propagation remain outside this decision procedure.

Inequality ranges, domain membership, scoped contexts, `refine`, compound
inference.

- Representation: an immutable fact set with a **congruence-closure**-style
  propagation over interned expressions (Nelson & Oppen 1980).
- Oracle: sampling within the declared domain; a rewrite must hold at sampled
  admissible points and be refused outside them.

### M3 — Series (#35) · 321 sites

Issue #35 is complete for the bounded native Laurent extension above. General
series composition, inversion, infinity expansions, and special-function
series remain named refusals until their independent coefficient oracles are
available.

Laurent series, expansion at infinity and at singular points, composition,
inversion, series of special functions.

- **Lazy/recursive power series** with Newton iteration for reciprocal and
  reversion; **Brent & Kung (1978)** for composition.
- Order convention documented and identical in both frontends. Wolfram's
  `Series[f,{x,0,n}]` includes `x^n`; SymPy's `series(f,x,0,n)` does not.
  fortsym-bench found this on its first corpus entry.

### M4 — Limits (#32) · 137 sites

Issue #32 is complete for the bounded limit layer above. General Gruntz-style
most-rapidly-varying subexpressions and one-sided limits remain outside this
decision procedure and are refused rather than inferred from samples.

- **Gruntz's algorithm** (Gruntz 1996) over most-rapidly-varying subexpressions.
  It is the only method in wide use that is correct on nested exponentials, and
  it is what SymPy implements.
- Taylor-derived limits first for the cheap finite cases.

### M5 — Complex domain (#33) · 782 sites

Issue #33 is complete for the bounded direct API above. Promotion of arbitrary
algebraic complex values and branch-sensitive functions remains outside this
decision procedure and is refused rather than given a principal-branch guess.

`re`, `im`, `conjugate`, `arg`, guarded `abs`, `complex_expand`, and promotion
of the bounded `qqbar` bridge into arena nodes.

- Exact algebraic numbers through **FLINT `qqbar`** (Johansson 2020), already
  pinned as a dependency.
- Oracle: high-precision evaluation and `z * conj(z) == abs(z)^2`.

### M6 — Special functions (#34) · 1040 sites

Issue #34 is complete for the bounded exact identity fragment above. Native
simplification handles zero arguments for `erf`, `erfc`, integer-order Bessel
`J`/`I`, positive integer Gamma values through a checked factorial cap, and
`Gamma(1/2)`. General Bessel families, elliptic and Legendre functions,
analytic continuation, asymptotics, and arbitrary-precision evaluation remain
explicitly outside this decision procedure.

Bessel `J`,`Y`,`I`,`K`; gamma family; `erf`/`erfc`; elliptic `K`,`E`,`F`;
Legendre.

- Numerics through **Arb** ball arithmetic (Johansson 2017), which gives
  rigorous enclosures rather than best-effort floats.
- Symbolic relations from **DLMF** (Olver et al.), cited per rule.
- Unblocks KiLCA, whose entire conductivity tensor is Bessel and gamma.

### M7 — Integration (#31) · 350 sites

Issue #31 is complete for the bounded exact fragment above. The full Hermite,
Rothstein--Trager, and Risch families remain outside this decision procedure
and are refused rather than partially integrated.

- Current native work covers bounded exact rules and guarded definite
  integration. It must retain the explicit refusal boundary while it grows.
- Rational part: **Hermite reduction** then **Lazard–Rioboo–Trager** for the
  logarithmic part. Bronstein, *Symbolic Integration I* (2005), is the reference
  implementation guide.
- Then the **Risch** algorithm for elementary extensions, in decision-procedure
  form with domain-aware tests.
- Oracle: differentiate the answer and decide zero.

### M8 — Solving (#36) · 76 sites

Issue #36 is complete for the bounded exact univariate fragment above. General
radicals, `RootOf`, elimination, and Gröbner operations remain outside this
decision procedure and are refused explicitly.

Univariate roots by radicals to degree four, `RootOf` representation above,
elimination for systems, Gröbner only where a traced case needs it
(**F4**, Faugère 1999; bounded Buchberger otherwise).

### M9 — Matrices (#30) · 589 sites

Issue #30 is complete for the bounded exact matrix fragment above. Eigenvalues
and symbolic pivots whose validity conditions cannot be decided remain named
refusals rather than approximate or unconditional answers.

- Determinant and linear solve by **fraction-free Bareiss** (Bareiss 1968).
- Rational systems by **Dixon p-adic lifting** (Dixon 1982), which is the fast
  path and what makes large exact systems tractable.
- Eigenvalues via characteristic polynomial through M8.

### M10 — Numerics and the fortnum boundary (#37) · 686 sites

Issue #37 is complete for the bounded expression-evaluation bridge above.
`numeric_precision_text` retains MPFR decimal results instead of projecting
them through real64, `numeric_complex_text` evaluates the supported rectangular
fragment componentwise, and `numeric_callable_t` gives fortnum-style callers a
validated ordered point-evaluation interface. Arb ball enclosures and numeric
algorithms remain fortnum-side work and are not duplicated here.

Requested-precision real and complex evaluation, `NIntegrate`, `FindRoot`,
`Interpolation`, `Fit`.

**Anything numerical that is not symbolic-expression evaluation belongs in
fortnum, not here** — quadrature, root finding, interpolation and fitting are
fortnum's subject. fortsym contributes only the evaluator that turns an
expression into a callable, plus the bridge. Work lands in fortnum when the
corpus demands a routine fortnum lacks, and nowhere else.

The adapter must reject symbolic or under-specified calls before entering
fortnum. Each numeric result carries its requested precision, estimated error,
domain transformations, and failure status so a plausible floating value
cannot masquerade as an exact proof.

- Ball arithmetic through **Arb**; adaptive quadrature via
  **Gauss–Kronrod** with the Petras/Molin rigorous-error treatment for the cases
  that need certification.

### M11 — Plotting through fortplot (#44) · 2794 sites · complete for the bounded adapter

`Plot`, `Plot3D`, `ContourPlot`, `ParametricPlot`, `ListPlot`, `StreamPlot`,
`DensityPlot`, `VectorPlot`, `LogPlot`, `Show`, `GraphicsGrid`, `PlotLegends`.

Plotting was a deferred area. The corpus makes it the **single largest
construct family**, mostly from the teaching material, so it is scheduled.

fortsym does not gain a plotting implementation. It now has a pinned
**dependency on fortplot**, which owns rendering, and its CMake target includes
the existing plotting/Wolfram adapter and independent plot-family test. The
adapter samples ordinary and parametric curves, list data, and fields into
bounded arrays, maps labels/ranges/log and joined options, and uses fortplot for
PNG output and `Show` panels. Undefined curve samples are split rather than
connected through a pole; partially undefined fields and unsupported graphics
forms are explicit refusals. Anything missing is fixed **in fortplot**,
upstream, not worked around here.

### M12 — Sums, piecewise, trig rewrites, ODEs (#38, #39, #40, #43)

Indexed sums (**Gosper 1978**, **Zeilberger 1990** for the hypergeometric
cases), piecewise with branch emission, public trig/power rewrites, and finally
`DSolve`.

Issue #39 is complete as of 2026-08-11 for the evidenced bounded fragment:
`fortsym_sums` handles polynomial, geometric, telescoping, and constant
sum/product bodies, expands concrete ranges under an explicit cap, and refuses
unsupported or unsafe cases by name. The Wolfram frontend lowers `Sum` and
`Product` through that API. Its independent direct-loop oracle passes across
symbolic and concrete bounds, empty ranges, products, overflow-sized spans,
and refusal cases; unrestricted hypergeometric summation remains outside the
fragment.

Issue #38 is complete for ordered symbolic branch expressions. The native
engine discharges exact relational predicates and drops unreachable branches;
the differentiation path preserves each branch condition; and the Fortran
printer lowers `Piecewise`, `If`, and `Boole` to typed, ordered `merge`
expressions. The independent compiler/run oracle covers branch interiors,
boundaries, and the default branch. General condition solving and derivatives
at discontinuity boundaries remain outside this fragment.

Issue #43 is complete for the bounded first-order linear `DSolve` fragment.
`fortsym_ode` normalizes one equation to `y' = a(x) y + q(x)`, constructs an
integrating-factor solution, and returns the Wolfram nested
`List[List[Rule[...]]]` shape. One value initial condition is supported. A
direct exponential particular solution is also available when its
non-resonance denominator is decidably nonzero. The returned function is
substituted into the original derivative-and-function nodes and its residual
must zero-test before the result is exposed; the initial condition receives
the same independent check in the regression. Nonlinear, higher-order,
multi-equation, separable, resonant-parameter, and numerical (`NDSolve`)
forms remain named refusals/UNKNOWN rather than guessed solutions.

### M13 — Codegen completion (#41, #42)

Binding opaque applied functions and their `Derivative` nodes to
consumer-supplied procedures, and mapping special-function heads to a Fortran
runtime. Both issue slices are now complete: #41 unblocks SIMPLE's
canonical-field Hessians, and #42 lets KiLCA's orphaned generated kernels be
regenerated. A standalone full-
corpus `.wl`-to-`.f90` translator does not exist yet: the native runner
interprets `.wl` at runtime, and current codegen consumes an existing `expr_t`
graph. This remains a separate completion gate for the Fortran parity target.
The current bounded implementation parses sequential streams of up to 128
scalar assignments, expands earlier assignments, infers scalar inputs, and
reuses the kernel emitter. Its focused test compiles the emitted subroutine and
checks it against an independently derived numeric Fortran oracle. Control
flow, forward or reassigned names, arrays, non-Fortran names, and unsupported
expression forms remain intentionally refused.

The code-generation boundary is being split into a backend-neutral kernel IR
and separate emitters. The IR owns the lowered, shared DAG and output roots;
Fortran, CUDA, HIP, and SYCL emitters own syntax, capability checks, and launch
policy. A backend-specific `select case` is acceptable at that edge, but the
symbolic arena and IR must not acquire backend syntax or launch state.

- [x] Lower a reachable expression DAG into a deterministic, topological IR
  with explicit operands, output roots, typed literals, and named symbols.
- [x] Emit equivalent Fortran and CUDA device leaves from the same IR and
  validate both against an independent numerical oracle. The emitter now
  refuses invalid identifiers, duplicate arguments or outputs, input/output
  name overlap, and invalid temporary prefixes before producing source.
- [x] Add the `gen_rbf_leaf` consumer generator and commit its FortML Fortran
  output with the exact FortSym revision and regeneration command. FortML's
  independent kernel tests now consume that generated primal leaf; generated
  derivative products remain consumer-owned until their ABI is complete.

The 2026-08-06 native `fo` run compiled and executed both generated leaves;
the CUDA fixture used `nvcc` and the device oracle on the available NVIDIA
toolchain. The emitted CUDA object remains a backend-owned scalar device leaf:
launch geometry, residency, and autodiff stay in FortML, so HIP/SYCL can use
the same IR boundary later.

The CUDA result is deliberately a scalar `__device__` leaf. It has no launch
geometry, allocation, stream, or autodiff policy; those belong to the consumer
backend. This keeps the same IR suitable for later HIP and SYCL emitters and
lets FortML own its resident tiled wrapper while consuming a proven source
generation contract.

### M14 — Multi-target emission and measured optimality (#61, #62, #63, #64, #65, #66)

Unlike M1–M13 this milestone is not ordered by corpus sites. It is ordered by
consumer need: fortnum, fortad, and SIMPLE generate kernels from fortsym and
have no way to say how good the result is. The corpus measures whether fortsym
computes the right expression; nothing measures whether the emitted kernel is
fast or accurate. Tracker: [lazy-fortran/fortgen#1](https://github.com/lazy-fortran/fortgen/issues/1).

M13 established the backend-neutral IR and the scalar device-leaf boundary.
M14 keeps that boundary exactly as stated — launch geometry, residency, and
harness stay with the consumer — and adds three things above it.

Issue #61 (target-driven kernel emission) is complete as of 2026-08-10. The
shared target descriptor has stable serialisable identities for CPU Fortran,
OpenMP target Fortran, OpenACC Fortran, and CUDA; explicit targets select only
their own decoration while leaving the IR backend-neutral. CPU and CUDA tests
assert that no Fortran directives are emitted, OpenMP and OpenACC tests assert
exclusive decoration, and the compatibility path retains the historical
dual-target output used by committed FortNum kernels.

Issue #63 (explicit emission policy) is complete as of 2026-08-10. The IR
emitter now exposes named controls for small-power expansion, exact zero/one
folding, constant reciprocal elimination, and one-use product shaping for FMA.
Generated Fortran is compiled and checked against an independently written
numeric oracle; the same test disables each policy family and verifies the
conservative spelling remains available. The `pure_procedure` default remains
unchanged while `fortnum#73` is open.

Issue #64 (CSE level and rematerialisation threshold) is complete as of
2026-08-10. `kernel_spec_t` now carries target-specific `CSE_NONE`,
`CSE_THRESHOLDED`, and `CSE_FULL` settings plus a recursive recomputation-cost
threshold; full CSE with threshold zero remains the compatibility default.
Three generated CPU variants compile and agree with an independent numeric
oracle. On that fixture, `N_sym=4`; full CSE emits `N_emit=4`, while both
thresholded rematerialisation and CSE-none emit `N_emit=8`. End-to-end CUDA
timing remains a fortnum consumer measurement, not a claim made by this
Fortran-only emitter.

**Target as a descriptor (#61).** Generated kernels now carry only the
decoration requested by their stable target identity. The four targets
consumers need are CPU Fortran, OpenMP offload, OpenACC, and CUDA; the IR gains
no dialect branch. The descriptor's eventual home is `fortgen`, once the IR
moves there.

**The measurement chain (#62, #65).** Four counts, three gaps, each
attributable to exactly one layer:

| Count | Source | Gap below it |
|---|---|---|
| `N_sym` | `count_operations`, emitted as data (#62) | the generator's doing |
| `N_emit` | emitted source | the compiler's doing |
| `N_machine` | disassembly, FMA- and spill-aware | the hardware's doing |
| `T_meas` | runtime against an empirical roofline | |

`N_sym` is the number no profiler can obtain: only the symbolic layer knows how
much arithmetic the mathematics required. It is the count of **a particular
algebraic form**, not a proven arithmetic-complexity lower bound, and it must
be reported that way — distance from the best form known, never optimality.

Accuracy is the other half (#65) and is not optional, because #63 changes
floating-point association by design. The reference already exists here: MPFR
and `NK_BIG_REAL` give a high-precision evaluation of the exact expression, so
max and RMS ulp against it is an independent oracle in the sense this roadmap
requires — the reference derives from the symbolic form, the measurement from
the compiled kernel, and neither can be adjusted to satisfy the other.

**Emission policy (#63, #64, #66).** A compiler cannot reassociate floating
point without `-ffast-math`, because it transforms a float program and must
preserve its semantics. fortsym holds the exact expression and merely chooses
which float program to emit — fast-math-class rewrites, with justification, at
`-O3` without fast-math. Made explicit and tested: small-power expansion,
division elimination, exact-zero and exact-one folding, FMA shaping, purity,
and association choice.

CSE becomes a knob rather than a fixed pass (#64). It is not an unconditional
win: eliminating a subexpression extends a live range and can cost occupancy
(**Rawat et al.**, *Register Optimizations for Stencils on GPUs*), with
rematerialisation as the counter-optimisation. Register sufficiency for DAGs is
NP-complete (**Sethi 1975**), so this is a measured heuristic, per target.

Precision choice (#66) is now explicit: AVX-512 holds 16 `real32` lanes
against 8 `real64`, and an FMA delivers 32 FLOPs against 16 — a 2× lever on
CPU before any GPU is involved. Mixed-precision adaptive Runge–Kutta
preserves accuracy across a wide tolerance range (**arXiv 2605.23727**), while
naive uniform low precision produces instabilities. The selectable modes are
gated by #65's accuracy instrument: choosing a precision without measuring it
is guessing, and for long-time symplectic integration a plausible wrong answer
would survive every existing test.

Acceptance for this milestone follows the standard gate with one addition: a
change meant to preserve behaviour must keep downstream generated output
byte-identical, and a change that alters floating-point association must report
both its `N_sym`/`N_emit` delta and its ulp delta. A speed number without an
accuracy number is not a result.

Deliberately excluded: harness generation, a schedule language, ML cost models,
and equality saturation for speed — the last is already gated on M2 above, and
DAG-cost extraction is NP-hard by reduction from minimal set cover. The variant
space here is dozens of points, so exhaustive enumeration beats search.

### M15 — LaTeX output for single-sourced papers (#67)

Like M14 this is ordered by consumer need rather than corpus sites. A consumer
derives an expression here, generates a kernel from it, and then retypes the
same formula into a manuscript by hand. The two drift, and nothing detects it.
M15 makes one `expr_t` produce both the compiled kernel and the `.tex` fragment
the paper `\input`s, regenerated by the build like any other artifact.

Scope is a `DIA_LATEX` emitter and a `.tex` writer emitting one `\newcommand`
per named result. Conventions follow SymPy's published printer behaviour — the
notation readers already recognise — with exactly one setting, the symbol-name
override map. Everything else is fixed: an unregistered name renders visibly
plain rather than plausibly wrong, because a silently mistranslated symbol in a
published paper is the failure this milestone exists to prevent.

The oracle is hand-written expected LaTeX plus a typesetting check, never a
round-trip: there is no reader, and write-then-read would only prove two halves
of one implementation agree. Reading foreign LaTeX, rendering to PDF, and
notebook integration are out of scope.

Issue #67 is complete as of 2026-08-11. `fortsym_latex` writes a deterministic,
self-contained macro file from the original `expr_t`, with explicit notation
registration, letters-only label mapping, collision refusal, assumption
companions, fractions, roots, scripts, and function conventions. The printer
has a dedicated two-dimensional LaTeX walk rather than a flat spelling flag.
The independent fixture oracle checks exact output and compiles a minimal
`amsmath` document with `pdflatex`; the parser explicitly refuses `DIA_LATEX`.
The API and provenance decision are documented in `doc/latex-api.md` and
`doc/provenance.md`. The real-manuscript pilot remains #68.

Issue #68 is complete as of 2026-08-11. The pilot uses the current
`paper_magnetic_comment.lyx` export and the real `paper_magnetic.py` Eq. (40)
derivation, emits the equation macros in `doc/latex-pilot/eqs.tex`, and builds
the inserted equation with `pdflatex` (using `article` because
`IEEEtran.cls` is unavailable locally). Six of seven symbolic leaves needed
explicit registration, no typesetting failure occurred, and one macro per
named result is sufficient for the current API. The pilot records the
factor-order, relation, and derivative-node findings in `doc/latex-pilot.md`;
the relation and derivative implementation is tracked in #73.

Issue #73 is complete as of 2026-08-11. LaTeX now has a typed `relation`
writer for one macro containing both sides, a first-class `partial` node that
the differentiation pass preserves, and evidence-based `curl_t`/partial
printer rules. The independent LaTeX oracle checks exact relation and
derivative output, differentiation propagation, byte reproducibility, and
`pdflatex` typesetting; the real manuscript pilot regenerates and typesets
the committed equation. Canonical factor order is retained and documented as
the reproducibility policy rather than changed by a speculative printer
setting. The GitHub issue is now closed after the roadmap and artifact push.

The conventions are validated against a real manuscript before they harden
(#68), because fixtures are written by whoever wrote the emitter and agree with
it by construction. Reaching the derivations that are stored as notebooks rather
than scripts is a separate question with a separate answer (#69): a `.nb` file
is a document expression built from typeset boxes, not the script text the
existing parser reads.

Issue #69 is complete as of 2026-08-11. The committed notebook inventory finds
26 tracked notebooks across DESC, KiLCA, paper_magnetic, and profit: 18 live
candidates and 8 files explicitly archived under `paper_magnetic/old/`. Four
representative notebooks contain 176 Input cells against 189 cached Output and
69 prose or heading cells, a 40.6% Input-cell share under the documented proxy.
The route is a held Mathematica StandardForm export to `.wl`, with one file per
Input cell available for coverage measurement; a real cylindrical notebook
exported 26 cells and the current translator accepted 1/26. The inventory,
procedure, result, and limits are recorded in `doc/notebook-estate.md` and the
helper is `scripts/export_notebook_inputs.wls`. This issue adds neither a box
parser nor whole-estate conversion.

### M16 — The short form (#70, #71, #72)

Also ordered by consumer need. Writing a derivation is more verbose than the
mathematics justifies: an arena declared, initialised and threaded through every
constructor, and an `only:` list renaming `exp`, `sqrt` and `erf` around the
intrinsics in every program that uses them.

M16 makes the ordinary case short — `use fortsym`, a default arena, and symbol
creation by character assignment — while keeping `arena_t` and the explicit
constructors a first-class documented API rather than a legacy path. The
convenience layer is built on top of the explicit one with no private back door,
so anything reachable conveniently is reachable explicitly. The default arena is
global mutable state and therefore single-threaded; concurrent derivation uses
explicit arenas, and that contract is documented rather than discovered. What is
hidden is ownership, not semantics: no result changes and nothing is guessed.

Assignment creates a symbol and never parses. Expression text keeps going
through `parse_expr`, which reports `ok` and returns a diagnostic; overloading
assignment to sometimes-parse would put a silent failure mode in the library's
most-used call.

Symbol names follow the printer's conventions — `varphi_2`, `gamma_1`,
`theta_bar` — so that one spelling serves as a valid Fortran identifier, a
readable symbolic name, and a correct typeset form with no registration. LaTeX
markup never belongs in a name: the name is the arena identity and every dialect
renders it verbatim, so one such name breaks several outputs to improve one.
The shared Fortran-name boundary now refuses invalid names with diagnostics in
the printer, legacy kernel emitter, and IR emitters. It deterministically maps
case collisions such as `Gamma`/`gamma` and `B`/`b`, records changed spellings in
generated comments, and leaves the symbolic arena untouched (#72, completed
2026-08-11).

Names are **case-sensitive**, and that is a feature rather than an accident of
strings. It is what lets `gamma` and `Gamma` be different symbols rendering as
`\gamma` and `\Gamma`, and the same for `Phi`/`phi`, `Theta`/`theta`,
`Sigma`/`sigma` and the ordinary distinctions between `B` and `b` or `T` and
`t`. Half of standard notation is unavailable without it, which is a large part
of why emitting LaTeX from these names works at all.

Fortran identifiers are case-insensitive, so the two conventions meet at the
emission boundary and only there. That case has to be **tolerated, not refused**:
a derivation using both `Gamma` and `gamma` is correct, and rejecting it would
push one output target's limitation back onto the mathematics. The symbolic
layer and the typeset output keep both names exactly as written; the Fortran
emitter resolves the collision deterministically, renames only the symbols that
actually collide, and emits the mapping into the generated source so a kernel
stays traceable to its derivation. What must never happen is the silent alias —
two quantities sharing one variable, compiling cleanly, returning a wrong
number. Independent tests cover `\[Alpha]` and `Global`x` refusal, deterministic
mapping, compilation, and a numeric case-collision oracle; the IR Fortran path
is covered as well.

Documentation lands with the code (#71), including the `symbolic` skill in the
internal prompts repository. That skill teaches the explicit-arena form today
and is read more often than any document here, so it is named in the issue
rather than left to a general clause — no acceptance check in this repository
can reach it, and #71 does not close while it is outstanding.

### M17 — Verified-generation backend handoff (#48)

The FortFront boundary is typed and versioned for the first practical slice:
canonical native expression serialization, three-valued identity evidence, and
kernel source plus the existing symbolic operation-cost record. A generated
kernel is never reported as proved by this boundary; the consumer must parse
its source back and assess the original/generated difference. This keeps proof,
probe, and unknown outcomes separate while allowing the existing bounded
derivation, code-generation, provenance, and target APIs to be composed.

The Lean/Why3 bridge, unrestricted tensor/index protocol, dependent-type
semantics, and stepwise proof-producing rewrites remain outside this milestone.

## Roadmap maintenance

The top-level roadmap is the release-level plan. `doc/roadmap.md` holds the
operational checklist and `doc/provenance.md` holds algorithm references. A
milestone changes in one commit with its implementation, independent tests,
benchmark evidence, and documentation.

After each committed change:

```bash
fo
fortsym-bench run corpus --backend sympy mathics fortsym-wl --jobs 4 \
  --timeout 60 --report /tmp/fortsym-corpus.json
```

The report is the source of truth for measured counts. The roadmap records the
commit, date, cache state, and remaining gap. It does not claim 100% parity
until every overlapping binding agrees or has a documented oracle limitation.

The 2026-08-11 CI audit identified the native-loader failure as a toolchain
boundary: the library was built against Nix glibc while host Python used the
runner's older glibc. The default Nix shell now supplies Python, so CMake finds
the matching interpreter, and the wheel smoke test runs its installed pure
Python package under that interpreter as well. The Python facade preserves the
loader error for an explicitly configured library; local Nix Release, Python
CTest, and CUDA kernel-emission checks pass, and the pushed CI run is the final
cross-environment verification for this maintenance item.

## Simplification strategy

Candidate ranking stays the mechanism: generate candidates, verify equivalence,
rank by operation count after CSE. Beyond that, **bounded equality saturation**
over an e-graph (Willsey et al., *egg*, POPL 2021) is evaluated only once M2's
domain guards exist — an e-graph that rewrites without guards will happily prove
something false on a restricted domain.

## What stays out

Units, geometry, combinatorics and broad theorem proving remain outside the
corpus and therefore outside the plan. New corpus evidence can promote any of
them.
