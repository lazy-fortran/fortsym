# Generator programs. Their output is committed under src/generated/ and CI
# re-runs them and fails on any diff, so a generated kernel can never outlive
# the code that produced it.
add_executable(gen_demo gen_demo.f90)
target_link_libraries(gen_demo PRIVATE fortsym)

add_executable(gen_legendre_recurrence gen_legendre_recurrence.f90)
target_link_libraries(gen_legendre_recurrence PRIVATE fortsym)

add_executable(gen_rbf_derivatives gen_rbf_derivatives.f90)
target_link_libraries(gen_rbf_derivatives PRIVATE fortsym)

add_executable(gen_matern12_hvp gen_matern12_hvp.f90)
target_link_libraries(gen_matern12_hvp PRIVATE fortsym)

add_executable(gen_matern32_hvp gen_matern32_hvp.f90)
target_link_libraries(gen_matern32_hvp PRIVATE fortsym)

add_executable(gen_matern52_hvp gen_matern52_hvp.f90)
target_link_libraries(gen_matern52_hvp PRIVATE fortsym)

add_executable(gen_toroidal_kernels gen_toroidal_kernels.f90)
target_link_libraries(gen_toroidal_kernels PRIVATE fortsym)

# Reproducible end-to-end backend comparison. This remains separate from the
# correctness suite because timing assertions do not belong in CI tests.
add_executable(bench_native bench_native.f90)
target_link_libraries(bench_native PRIVATE fortsym)

add_executable(bench_algebraic bench_algebraic.f90)
target_link_libraries(bench_algebraic PRIVATE fortsym)

add_executable(fortsym_wl_to_f90 fortsym_wl_to_f90.f90)
target_link_libraries(fortsym_wl_to_f90 PRIVATE fortsym)

add_executable(fortsym_wl_run fortsym_wl_run.f90)
target_link_libraries(fortsym_wl_run PRIVATE fortsym)

add_executable(fortsym_latex_paper_pilot fortsym_latex_paper_pilot.f90)
target_link_libraries(fortsym_latex_paper_pilot PRIVATE fortsym)

# Lives under benchmark/harnesses rather than app/ because fpm treats every
# file in app/ as a program and links their objects together, so a C++ main
# here collides with every Fortran app's main and breaks `fo install`.
add_executable(bench_qqbar_direct
    ${CMAKE_CURRENT_SOURCE_DIR}/../benchmark/harnesses/bench_qqbar_direct.cpp)
target_link_libraries(bench_qqbar_direct PRIVATE fortsym::flint)
