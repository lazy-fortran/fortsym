# Register a test only when its source exists, so a milestone can add the
# registration ahead of the file without breaking configure.
function(fortsym_add_test name source)
    if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${source}")
        return()
    endif()
    add_executable(${name} ${source})
    target_link_libraries(${name} PRIVATE fortsym)
    add_test(NAME ${name} COMMAND ${name})
    # Yacas keeps its algebra in .ys scripts and has to be told where they are.
    # Passing it through the test environment avoids compiling the path in,
    # which would require the Fortran preprocessor.
    if(FORTSYM_YACAS_SCRIPTS)
        set_tests_properties(${name} PROPERTIES
            ENVIRONMENT "FORTSYM_YACAS_SCRIPTS=${FORTSYM_YACAS_SCRIPTS}")
    endif()
endfunction()

# --- utilities ---
fortsym_add_test(test_fortsym_string util/test_fortsym_string.f90)

# --- core representation ---
fortsym_add_test(test_fortsym_algebraic core/test_fortsym_algebraic.f90)
fortsym_add_test(test_fortsym_arena core/test_fortsym_arena.f90)
fortsym_add_test(test_fortsym_exact core/test_fortsym_exact.f90)
fortsym_add_test(test_fortsym_subs core/test_fortsym_subs.f90)

# --- exact linear algebra ---
fortsym_add_test(test_fortsym_linalg algebra/test_fortsym_linalg.f90)

# --- surface syntax ---
fortsym_add_test(test_fortsym_roundtrip io/test_fortsym_roundtrip.f90)

# --- engines ---
fortsym_add_test(test_fortsym_native engine/test_fortsym_native.f90)
fortsym_add_test(test_fortsym_identities engine/test_fortsym_identities.f90)
fortsym_add_test(test_fortsym_yacas engine/test_fortsym_yacas.f90)

# --- multi-engine adjudication ---
fortsym_add_test(test_fortsym_council council/test_fortsym_council.f90)

# --- code generation ---
fortsym_add_test(test_fortsym_enzyme codegen/test_fortsym_enzyme.f90)
fortsym_add_test(test_fortsym_kernel_emit codegen/test_fortsym_kernel_emit.f90)
fortsym_add_test(test_fortsym_kernel_ir codegen/test_fortsym_kernel_ir.f90)
fortsym_add_test(test_fortsym_kernel codegen/test_fortsym_kernel.f90)
fortsym_add_test(test_fortsym_wl_f90 codegen/test_fortsym_wl_f90.f90)

# --- verification ---
fortsym_add_test(test_fortsym_check verify/test_fortsym_check.f90)
fortsym_add_test(test_fortsym_quadratic core/test_fortsym_quadratic.f90)
fortsym_add_test(test_fortsym_quadratic_linalg algebra/test_fortsym_quadratic_linalg.f90)
fortsym_add_test(test_fortsym_quadratic_poly algebra/test_fortsym_quadratic_poly.f90)
fortsym_add_test(test_fortsym_quadratic_multipoly algebra/test_fortsym_quadratic_multipoly.f90)
fortsym_add_test(test_fortsym_rk verify/test_fortsym_rk.f90)
fortsym_add_test(test_fortsym_rk_reduced verify/test_fortsym_rk_reduced.f90)

# --- differential geometry ---
fortsym_add_test(test_fortsym_diff calculus/test_fortsym_diff.f90)
fortsym_add_test(test_fortsym_chart calculus/test_fortsym_chart.f90)
fortsym_add_test(test_fortsym_products calculus/test_fortsym_products.f90)
fortsym_add_test(test_fortsym_toroidal calculus/test_fortsym_toroidal.f90)

# --- reading Fortran back ---
fortsym_add_test(test_fortsym_fparse parse_fortran/test_fortsym_fparse.f90)

# --- C ABI over SymEngine ---
fortsym_add_test(test_fortsym_capi capi/test_fortsym_capi.f90)

# --- privacy-safe consumer evidence tooling ---
find_package(Python3 COMPONENTS Interpreter REQUIRED)
add_test(
    NAME test_consumer_audit
    COMMAND
        ${Python3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/audit/test_consumer_audit.py)
set_tests_properties(
    test_consumer_audit
    PROPERTIES ENVIRONMENT "PYTHONDONTWRITEBYTECODE=1")
