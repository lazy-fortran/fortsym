# Contributing

Keep changes focused on a documented symbolic operation or a reproducible
defect. Read the [architecture](doc/architecture.md),
[feature matrix](doc/feature-matrix.md), and the relevant API contract first.
Mesh discretization, finite-element assembly and numerical solvers belong in
their numerical libraries; fortsym owns the reusable symbolic operations.

Build and run the native tests with the commands in [README.md](README.md).
Add a regression test for changed behavior using an independent algebraic,
numerical, residual or compiler oracle. Tests must preserve domain conditions,
refuse unsupported inputs explicitly, and distinguish `UNKNOWN` from a proof
of zero or nonzero. Keep the C and Python interfaces aligned when a public
operation changes.

Generate values and derivative products from shared symbolic definitions.
Keep scheduling separate from mathematical definitions. Enclosure generation
requires the declared [runtime contract](doc/rigorous-runtime.md); arbitrary
padding of library function results does not establish an enclosure.

Measure performance separately from correctness and use matching inputs,
semantics and backend configurations, as described in
[benchmarks](doc/benchmarks.md). Record the caller's accuracy requirement before
optimizing a kernel. Credit mathematical sources and any adapted code in
[PROVENANCE.md](PROVENANCE.md), including the source license.
