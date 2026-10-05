# Generator programs. Their output is committed under src/generated/ and CI
# re-runs them and fails on any diff, so a generated kernel can never outlive
# the code that produced it.
add_executable(gen_demo gen_demo.f90)
target_link_libraries(gen_demo PRIVATE fortsym)

add_executable(gen_dual_demo gen_dual_demo.f90)
target_link_libraries(gen_dual_demo PRIVATE fortsym)

add_executable(gen_taylor_demo gen_taylor_demo.f90)
target_link_libraries(gen_taylor_demo PRIVATE fortsym)

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

add_executable(bench_chart_geometry bench_chart_geometry.f90)
target_link_libraries(bench_chart_geometry PRIVATE fortsym)

add_executable(bench_algebraic bench_algebraic.f90)
target_link_libraries(bench_algebraic PRIVATE fortsym)

add_executable(bench_complexdom bench_complexdom.f90)
target_link_libraries(bench_complexdom PRIVATE fortsym)

# Emitted runtime measurements are optional; correctness has its own short gate.
add_executable(gen_emitted_kernel_audit gen_emitted_kernel_audit.f90)
target_link_libraries(gen_emitted_kernel_audit PRIVATE fortsym)
execute_process(COMMAND git rev-parse HEAD
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    OUTPUT_VARIABLE FORTSYM_AUDIT_REVISION OUTPUT_STRIP_TRAILING_WHITESPACE)
if(NOT FORTSYM_AUDIT_REVISION)
    set(FORTSYM_AUDIT_REVISION "unknown-source-revision")
endif()
set(FORTSYM_AUDIT_SOURCE ${CMAKE_CURRENT_BINARY_DIR}/emitted_audit_kernels.f90)
add_custom_command(OUTPUT ${FORTSYM_AUDIT_SOURCE}
    COMMAND gen_emitted_kernel_audit ${FORTSYM_AUDIT_SOURCE} ${FORTSYM_AUDIT_REVISION}
    DEPENDS gen_emitted_kernel_audit
    VERBATIM)
add_library(fortsym_emitted_audit_kernels STATIC ${FORTSYM_AUDIT_SOURCE}
    ${PROJECT_SOURCE_DIR}/test/codegen/runtime/fortsym_interval_runtime.f90)
set(FORTSYM_AUDIT_MODULES ${CMAKE_CURRENT_BINARY_DIR}/audit-modules)
file(MAKE_DIRECTORY ${FORTSYM_AUDIT_MODULES})
set_target_properties(fortsym_emitted_audit_kernels PROPERTIES
    Fortran_MODULE_DIRECTORY ${FORTSYM_AUDIT_MODULES})
target_include_directories(fortsym_emitted_audit_kernels PUBLIC ${FORTSYM_AUDIT_MODULES})
add_executable(bench_emitted_kernels
    ${PROJECT_SOURCE_DIR}/benchmark/harnesses/bench_emitted_kernels.f90)
target_link_libraries(bench_emitted_kernels PRIVATE fortsym_emitted_audit_kernels)
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(fortsym_emitted_audit_kernels PRIVATE
        -O3 -fno-fast-math -ffp-contract=off)
    target_compile_options(bench_emitted_kernels PRIVATE
        -O3 -fno-fast-math -ffp-contract=off)
endif()

add_executable(fortsym_wl_to_f90 fortsym_wl_to_f90.f90)
target_link_libraries(fortsym_wl_to_f90 PRIVATE fortsym)

add_executable(fortsym_wl_run fortsym_wl_run.f90)
target_link_libraries(fortsym_wl_run PRIVATE fortsym)

add_executable(fortsym_latex_paper_pilot fortsym_latex_paper_pilot.f90)
target_link_libraries(fortsym_latex_paper_pilot PRIVATE fortsym)

add_executable(paper_magnetic_native paper_magnetic_native.f90)
target_link_libraries(paper_magnetic_native PRIVATE fortsym)

add_executable(forms_native forms_native.f90)
target_link_libraries(forms_native PRIVATE fortsym)

add_executable(tensor_native tensor_native.f90)
target_link_libraries(tensor_native PRIVATE fortsym)

add_executable(connection_native connection_native.f90)
target_link_libraries(connection_native PRIVATE fortsym)

# Lives under benchmark/harnesses rather than app/ because fpm treats every
# file in app/ as a program and links their objects together, so a C++ main
# here collides with every Fortran app's main and breaks `fo install`.
add_executable(bench_qqbar_direct
    ${CMAKE_CURRENT_SOURCE_DIR}/../benchmark/harnesses/bench_qqbar_direct.cpp)
target_link_libraries(bench_qqbar_direct PRIVATE fortsym::flint)
