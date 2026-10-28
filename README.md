# fortsym

Computer algebra for Fortran: derive expressions symbolically, generate Fortran
and CUDA kernels from them, and check generated or hand-written code against
the symbolic definition, all inside the normal build and test run. It is used
to generate and verify kernels in numerical libraries such as
[fortnum](https://github.com/lazy-fortran/fortnum) and
[fortfem](https://github.com/lazy-fortran/fortfem).

Status: research code under active development; interfaces may change.

## Features

- Own expression representation (hash-consed DAG in an arena) with a native
  Fortran core: exact integer and rational arithmetic, expansion,
  differentiation, Taylor series, substitution, exact linear solves and
  conservative zero tests. Assumptions such as `positive(x)` are explicit.
- Several engines behind one interface: SymEngine and Yacas linked in process,
  SymPy and Maxima as optional separate processes. Answers are compared and the
  smallest verified form is kept; calling code never names an engine.
- Identity decisions that return `UNKNOWN` outside their decidable fragment,
  and a clear distinction between a symbolic proof and a numeric probe.
- Kernel generation with common-subexpression elimination for Fortran (CPU,
  OpenMP target, OpenACC) and CUDA, CPU precision variants, and rigorous
  enclosure kernels.
- Derivative products (`jvp`, `vjp`, `gradient`, `hvp`) without forming
  Jacobians, and implicit-function derivatives.
- Differential geometry: coordinate charts, metrics, Christoffel symbols,
  grad/div/curl/Laplacian, typed tensors, differential forms and curvature.
- Fortran readback (recover the expression a source file computes), a native
  runner for a subset of the Wolfram language, LaTeX output, and Python
  bindings over a C ABI.

## Building

Dependencies: a Fortran compiler, CMake 3.22 or newer, a C/C++ compiler,
SymEngine, FLINT, MPFR and GMP. Yacas is fetched and built by CMake. SymPy and
Maxima are optional and detected at run time.

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
ctest --test-dir build --output-on-failure
cmake --install build --prefix /path/to/fortsym
```

`-DFORTSYM_USE_SYSTEM_DEPS=OFF` builds SymEngine from source. `nix develop`
provides the pinned dependency set used by CI. Consumers link the installed
package with

```cmake
find_package(fortsym CONFIG REQUIRED)
target_link_libraries(my_generator PRIVATE fortsym::fortsym)
```

See [doc/building.md](doc/building.md) for fpm/fo, the Nix shells and optional
engines.

## Usage

```fortran
use fortsym
type(expr_t) :: x, y, e, d
type(engine_result_t) :: derivative

x = "x"
y = "y"
e = sin(x*y)**2 + cos(x*y)

derivative = diff(e, x)
d = derivative%value
```

[`example/example_workflow.f90`](example/example_workflow.f90) goes further:
it differentiates an expression, proves an identity, asks several engines to
agree and emits a kernel. The other programs in [`example/`](example) cover
coordinate geometry, flux and Boozer coordinates, and output certificates.

## Documentation

- [Fortran API](doc/fortran-api.md)
- [Capabilities in more detail](doc/capabilities.md)
- [Building](doc/building.md)
- [Architecture of the engine layer](doc/architecture.md)
- [Feature matrix](doc/feature-matrix.md), [roadmap](doc/roadmap.md)
- [C API](doc/c-api.md), [Python bindings](python/README.md),
  [LaTeX output](doc/latex-api.md)
- [Native Wolfram-language runner](doc/wolfram-runner.md)
- [Rigorous kernels](doc/rigorous-runtime.md),
  [output certificates](doc/certificate.md)

## Related projects

- [fortgen](https://github.com/lazy-fortran/fortgen): numerical kernel IR and
  Fortran/CUDA emission backend.
- [fortplot](https://github.com/lazy-fortran/fortplot): plotting of sampled
  expressions.
- [fortsym-bench](https://github.com/lazy-fortran/fortsym-bench): benchmark and
  comparison corpus.

## License

MIT (see [LICENSE](LICENSE)). SymEngine (MIT), Yacas, FLINT, GMP and MPFR (LGPL)
are linked, the LGPL components dynamically. SymPy, Maxima and other GPL engines
run only as separate processes and are not required. Wolfram/Mathematica is not
used. [LEGAL.md](LEGAL.md) records every dependency and its obligations.
