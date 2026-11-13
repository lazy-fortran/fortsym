# Building fortsym

## Dependencies

Linked into the library (Tier 1):

- SymEngine (MIT), with MPFR and MPC support, taken from the system by default
- FLINT 3.6.0, MPFR 4.2.2, MPC and GMP (LGPL), linked dynamically
- Yacas (LGPL), fetched and built at a pinned tag by CMake

Python 3.11 or newer runs the registered metadata and Python checks.

Optional engines (Tier 2), detected at run time and skipped when absent: SymPy
and Maxima, run as separate processes. `scripts/bootstrap.sh` reports which are
available and prints, without running it, the command to install the missing
ones. [LEGAL.md](../LEGAL.md) explains why the tiers are split this way.

## CMake

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
ctest --test-dir build --output-on-failure
```

| Option | Default | Effect |
| --- | --- | --- |
| `FORTSYM_USE_SYSTEM_DEPS` | `ON` | Use the system SymEngine; `OFF` fetches and builds SymEngine, using the installed arithmetic libraries |
| `FORTSYM_BUILD_TESTING` | on for a top-level build | Build the tests |

Install with `cmake --install build --prefix /path/to/fortsym`. A consumer
built with a compatible Fortran compiler sets `CMAKE_PREFIX_PATH` to that prefix
and links the installed shared library:

```cmake
find_package(fortsym CONFIG REQUIRED)
target_link_libraries(my_generator PRIVATE fortsym::fortsym)
```

Consumers do not need SymEngine, MPFR or FLINT development packages: the
installation carries the bundled shared libraries. System arithmetic and
compiler runtime libraries must still be present. The test
`test_fortsym_installed_package` relocates an installation and builds an
external consumer against it.

The installed Yacas scripts are located relative to the shared library, so the
installation can move without an environment override. `FORTSYM_YACAS_SCRIPTS`
selects an explicit script directory when needed. Licence texts and notices are
installed under `share/licenses/fortsym`.

Some generated-kernel tests also use Flang when it is on `PATH`. On macOS,
Homebrew's OpenMP runtime is not in the default linker search path; run those
tests with:

```bash
LIBRARY_PATH="$(brew --prefix libomp)/lib${LIBRARY_PATH:+:$LIBRARY_PATH}" ctest --test-dir build --output-on-failure
```
The generated source and its OpenMP checks are unchanged by this environment
setting.

The install also places the Python package under
`share/fortsym/python/fortsym`; see the [Python README](../python/README.md).

## Nix

`flake.nix` pins the dependency closure CI builds against. FLINT and MPFR are
pinned to one version each, so a distribution that ships another version may
fail to configure; in that case use the Nix shells:

```bash
nix develop          # Tier 1 dependencies
nix develop .#tier2  # plus the optional engines
```

Without Nix or root, `.fortyard/check` provisions the same versions under
`$TMPDIR/fortsym-check`, GMP, MPFR and MPC from conda-forge and FLINT and
SymEngine built from their release sources, then runs the Tier 1 CMake build
and tests. The script names the one version that differs from `flake.lock`.

## fpm and fo

`fpm.toml` links the system SymEngine, FLINT, MPFR and GMP. The fpm build does
not fetch or build Yacas, so that engine reports itself unavailable.
The repository's contributor workflow uses
[fo](https://github.com/lazy-fortran/fo), which wraps fpm:

```bash
fo                    # static checks, build, tests, lint, format
fo test               # test suites
fo build --asan       # arena and handle lifetime checks
```
