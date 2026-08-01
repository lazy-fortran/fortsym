# Generator programs. Their output is committed under src/generated/ and CI
# re-runs them and fails on any diff, so a generated kernel can never outlive
# the code that produced it.
add_executable(gen_demo gen_demo.f90)
target_link_libraries(gen_demo PRIVATE fortsym)

add_executable(gen_legendre_recurrence gen_legendre_recurrence.f90)
target_link_libraries(gen_legendre_recurrence PRIVATE fortsym)

add_executable(gen_toroidal_kernels gen_toroidal_kernels.f90)
target_link_libraries(gen_toroidal_kernels PRIVATE fortsym)

# Reproducible end-to-end backend comparison. This remains separate from the
# correctness suite because timing assertions do not belong in CI tests.
add_executable(bench_native bench_native.f90)
target_link_libraries(bench_native PRIVATE fortsym)

add_executable(bench_algebraic bench_algebraic.f90)
target_link_libraries(bench_algebraic PRIVATE fortsym)

# Lives under benchmark/harnesses rather than app/ because fpm treats every
# file in app/ as a program and links their objects together, so a C++ main
# here collides with every Fortran app's main and breaks `fo install`.
add_executable(bench_qqbar_direct
    ${CMAKE_SOURCE_DIR}/benchmark/harnesses/bench_qqbar_direct.cpp)
target_link_libraries(bench_qqbar_direct PRIVATE fortsym::flint)
