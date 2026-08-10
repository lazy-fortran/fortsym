# fortsym Python bindings

This package is a standard-library-only `ctypes` facade over the installed
fortsym C ABI. It does not import SymPy or depend on a Fortran compiler ABI.

Set `FORTSYM_LIBRARY` to the absolute path of the shared library when using a
wheel or an installed CMake tree. A source checkout can instead use
`PYTHONPATH=python`; the package then searches `build/lib` automatically.
The CMake install places the package sources under
`share/fortsym/python/fortsym`, so that directory can be added to `PYTHONPATH`.

Expression handles own their native references and are released by `close()`
or garbage collection. Exact integers and `fractions.Fraction` values are
passed as decimal strings when they do not fit the compact C ABI scalars.
