# FortSym agent rules

Inherited home/workspace instructions remain in force. Read [README.md](README.md),
[ROADMAP.md](ROADMAP.md), [benchmark contracts](doc/benchmarks.md), and
[arithmetic runtime contracts](doc/rigorous-runtime.md) before implementation;
[PROVENANCE.md](PROVENANCE.md) governs mathematical and source attribution.
Use the Fortran skill and the controller's verified resident Fo driver.
Generate primal, JVP, VJP, and supported enclosure leaves from shared symbolic
definitions. Keep execution schedules separate from symbolic mathematics.
Retain independent value, derivative, and enclosure oracles; benchmark raw
latency and memory separately from correctness and record the caller's
certified-accuracy requirement before optimizing a hot path.
Roundoff budgets require a justified arithmetic model. Arbitrary padding of
libm results is not an enclosure proof; unsupported operations fail explicitly.
Preserve unrelated edits. Workers return exact-base patch evidence; the
controller owns integration and promotion.
