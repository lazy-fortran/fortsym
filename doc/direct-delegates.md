# Direct delegate experiments

The Wolfram-language runner uses FortSym's native evaluator. Its promoted exact
arithmetic and algebraic numbers call FLINT directly; requested-precision
`N` calls SymEngine. These calls can be disabled separately to measure which
results depend on them. This does not remove the libraries from the binary or
disable their use inside another dependency. In particular, this is not a
library-free native mode. The runner does not dispatch to Yacas.

Set any of these variables to `1` before starting a process:

| Variable | Direct calls refused |
| --- | --- |
| `FORTSYM_DISABLE_FLINT_EXACT` | FLINT rational normalization, arithmetic and MPFR projection |
| `FORTSYM_DISABLE_FLINT_ALGEBRAIC` | FLINT algebraic construction, normalization, arithmetic, projections, signs and SymEngine conversion checks |
| `FORTSYM_DISABLE_SYMENGINE_EVAL` | SymEngine requested-precision real evaluation |

Every other value, including an unset variable, leaves that group enabled.
The policy is read once per process, on the first delegate or counter call,
or when a Wolfram-language session initializes a requested trace.
Do not change the environment after that point. Disabled calls retain the
ordinary failure contract: no pending result, no output argument overwritten,
and no successful numeric substitute. A native symbolic operation may preserve
an unevaluated expression when its delegate is unavailable; the evaluator's
ordinary acceptance rules still apply. Requested-precision `N` reports an
explicit refusal.

`FORTSYM_COUNT_DELEGATES=1` enables atomic process-wide counters. Disabling any
group or requesting a trace also enables all counters. With these variables
unset, counter updates are skipped. The C ABI `fsym_delegate_counts` returns
attempts and successes for the three `FSYM_DELEGATE_*` identifiers. Read counts
after worker threads join; concurrent reads are individually atomic, rather
than a snapshot across groups.

An attempt is entry to a direct wrapper, including a call refused by a disabled
gate or malformed input. A success is a nonempty accepted result, a successful
projection or sign verdict, a supported algebraic conversion check, or an
accepted decimal result from requested-precision evaluation. Fetches and
library-availability probes do not count. An algebraic constructor may use
FLINT rational primitives internally; it counts once in the algebraic group.
Counts measure direct boundary activity, not elementary arithmetic operations,
exclusive engine authorship, or proof of correctness.

Set `FORTSYM_TRACE_DELEGATES=/existing/directory/run` to write
`/existing/directory/run.<pid>.json` at normal process exit. The file is created
exclusively with owner-only permissions. A crash or forced termination may
leave no trace; a missing, incomplete or rejected trace invalidates the
measurement. Standard result output is unchanged. The schema is:

```json
{"schema":"fortsym-direct-delegates-v1","pid":123,"delegates":{"flint-exact":{"enabled":true,"attempts":2,"successes":2},"flint-algebraic":{"enabled":false,"attempts":1,"successes":0},"symengine-eval":{"enabled":true,"attempts":1,"successes":1}}}
```

Run a fresh process for each configuration and compare its accepted bindings
with the same corpus and oracle outputs. A lost result demonstrates dependence
on the disabled boundary for that execution. A changed result needs separate
mathematical checking. Numerical sampling alone does not establish equivalence.
