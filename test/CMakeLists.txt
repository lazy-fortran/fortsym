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
fortsym_add_test(test_fortsym_algebraic_expr core/test_fortsym_algebraic_expr.f90)
fortsym_add_test(test_fortsym_arena core/test_fortsym_arena.f90)
fortsym_add_test(test_fortsym_exact core/test_fortsym_exact.f90)
fortsym_add_test(test_fortsym_convenience core/test_fortsym_convenience.f90)
fortsym_add_test(test_fortsym_simple_codegen core/test_fortsym_simple_codegen.f90)
fortsym_add_test(test_fortsym_subs core/test_fortsym_subs.f90)

# --- exact linear algebra ---
fortsym_add_test(test_fortsym_linalg algebra/test_fortsym_linalg.f90)
fortsym_add_test(test_fortsym_integrate algebra/test_fortsym_integrate.f90)
fortsym_add_test(test_fortsym_complexdom algebra/test_fortsym_complexdom.f90)
fortsym_add_test(test_fortsym_limits algebra/test_fortsym_limits.f90)
fortsym_add_test(test_fortsym_matrix algebra/test_fortsym_matrix.f90)
fortsym_add_test(test_fortsym_poly algebra/test_fortsym_poly.f90)
fortsym_add_test(test_fortsym_polysolve algebra/test_fortsym_polysolve.f90)

# --- surface syntax ---
fortsym_add_test(test_fortsym_roundtrip io/test_fortsym_roundtrip.f90)
fortsym_add_test(test_fortsym_latex io/test_fortsym_latex.f90)
fortsym_add_test(test_fortsym_wolfram io/test_fortsym_wolfram.f90)
fortsym_add_test(test_fortsym_plotfamily io/test_fortsym_plotfamily.f90)

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
fortsym_add_test(test_fortsym_accuracy verify/test_fortsym_accuracy.f90)
fortsym_add_test(test_fortsym_check verify/test_fortsym_check.f90)
fortsym_add_test(test_fortsym_numeric verify/test_fortsym_numeric.f90)
fortsym_add_test(test_fortsym_backend codegen/test_fortsym_backend.f90)
fortsym_add_test(test_fortsym_exact_padding core/test_fortsym_exact_padding.f90)
fortsym_add_test(test_fortsym_quadratic core/test_fortsym_quadratic.f90)
fortsym_add_test(test_fortsym_quadratic_linalg algebra/test_fortsym_quadratic_linalg.f90)
fortsym_add_test(test_fortsym_quadratic_poly algebra/test_fortsym_quadratic_poly.f90)
fortsym_add_test(test_fortsym_quadratic_multipoly algebra/test_fortsym_quadratic_multipoly.f90)
fortsym_add_test(test_fortsym_rk verify/test_fortsym_rk.f90)

# --- differential geometry ---
fortsym_add_test(test_fortsym_diff calculus/test_fortsym_diff.f90)
fortsym_add_test(test_fortsym_domain calculus/test_fortsym_domain.f90)
fortsym_add_test(test_fortsym_ode calculus/test_fortsym_ode.f90)
fortsym_add_test(test_fortsym_chart calculus/test_fortsym_chart.f90)
fortsym_add_test(test_fortsym_chart_slow calculus/test_fortsym_chart_slow.f90)
fortsym_add_test(test_fortsym_chart_map calculus/test_fortsym_chart_map.f90)
fortsym_add_test(test_fortsym_geometry_metadata calculus/test_fortsym_geometry_metadata.f90)
fortsym_add_test(test_fortsym_notation calculus/test_fortsym_notation.f90)
fortsym_add_test(test_fortsym_metric calculus/test_fortsym_metric.f90)
fortsym_add_test(test_fortsym_form calculus/test_fortsym_form.f90)
fortsym_add_test(test_fortsym_flux calculus/test_fortsym_flux.f90)
fortsym_add_test(test_fortsym_magnetic calculus/test_fortsym_magnetic.f90)
fortsym_add_test(test_fortsym_boozer calculus/test_fortsym_boozer.f90)
fortsym_add_test(test_fortsym_spherical calculus/test_fortsym_spherical.f90)
fortsym_add_test(test_fortsym_magnetic_weak calculus/test_fortsym_magnetic_weak.f90)
fortsym_add_test(test_fortsym_tensor calculus/test_fortsym_tensor.f90)
fortsym_add_test(test_fortsym_connection calculus/test_fortsym_connection.f90)
fortsym_add_test(test_fortsym_products calculus/test_fortsym_products.f90)
fortsym_add_test(test_fortsym_registry calculus/test_fortsym_registry.f90)
fortsym_add_test(test_fortsym_relativity calculus/test_fortsym_relativity.f90)
fortsym_add_test(test_fortsym_spacetime_form calculus/test_fortsym_spacetime_form.f90)
fortsym_add_test(test_fortsym_toroidal calculus/test_fortsym_toroidal.f90)
fortsym_add_test(test_fortsym_easy_geometry calculus/test_fortsym_easy_geometry.f90)

# --- reading Fortran back ---
fortsym_add_test(test_fortsym_fparse parse_fortran/test_fortsym_fparse.f90)

# --- C ABI over SymEngine ---
add_executable(test_fortsym_capi capi/test_fortsym_capi.c)
target_link_libraries(test_fortsym_capi PRIVATE fortsym)
target_include_directories(test_fortsym_capi PRIVATE ${PROJECT_SOURCE_DIR}/src/capi)
add_test(NAME test_fortsym_capi COMMAND test_fortsym_capi)
add_executable(test_fortsym_spacetime_form_capi capi/test_fortsym_spacetime_form_capi.c)
target_include_directories(test_fortsym_spacetime_form_capi PRIVATE
    ${PROJECT_SOURCE_DIR}/src/capi)
target_link_libraries(test_fortsym_spacetime_form_capi PRIVATE fortsym)
add_test(NAME test_fortsym_spacetime_form_capi COMMAND test_fortsym_spacetime_form_capi)
add_executable(test_fortsym_metric_capi capi/test_fortsym_metric_capi.c)
target_include_directories(test_fortsym_metric_capi PRIVATE
    ${PROJECT_SOURCE_DIR}/src/capi)
target_link_libraries(test_fortsym_metric_capi PRIVATE fortsym)
add_test(NAME test_fortsym_metric_capi COMMAND test_fortsym_metric_capi)
add_executable(test_fortsym_flux_surface_capi capi/test_fortsym_flux_surface_capi.c)
target_include_directories(test_fortsym_flux_surface_capi PRIVATE
    ${PROJECT_SOURCE_DIR}/src/capi)
target_link_libraries(test_fortsym_flux_surface_capi PRIVATE fortsym)
add_test(NAME test_fortsym_flux_surface_capi COMMAND test_fortsym_flux_surface_capi)
add_executable(test_fortsym_flux_coordinates_capi capi/test_fortsym_flux_coordinates_capi.c)
target_include_directories(test_fortsym_flux_coordinates_capi PRIVATE
    ${PROJECT_SOURCE_DIR}/src/capi)
target_link_libraries(test_fortsym_flux_coordinates_capi PRIVATE fortsym)
add_test(NAME test_fortsym_flux_coordinates_capi COMMAND test_fortsym_flux_coordinates_capi)
add_executable(test_fortsym_header_cpp capi/test_fortsym_header.cpp)
target_include_directories(test_fortsym_header_cpp PRIVATE
    ${PROJECT_SOURCE_DIR}/src/capi)
target_link_libraries(test_fortsym_header_cpp PRIVATE fortsym)
add_test(NAME test_fortsym_header_cpp COMMAND test_fortsym_header_cpp)

# --- privacy-safe consumer evidence tooling ---
find_package(Python3 COMPONENTS Interpreter REQUIRED)
add_test(
    NAME test_fortsym_python
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/python/test_fortsym_python.py)
set_tests_properties(
    test_fortsym_python
    PROPERTIES
        ENVIRONMENT "PYTHONPATH=${PROJECT_SOURCE_DIR}/python;FORTSYM_LIBRARY=$<TARGET_FILE:fortsym>")
add_test(
    NAME test_fortsym_sympy
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/python/test_fortsym_sympy.py)
set_tests_properties(
    test_fortsym_sympy
    PROPERTIES
        ENVIRONMENT "PYTHONPATH=${PROJECT_SOURCE_DIR}/python;FORTSYM_LIBRARY=$<TARGET_FILE:fortsym>")
add_test(
    NAME test_fortsym_sympy_differential
    COMMAND
        ${Python3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/python/test_fortsym_sympy_differential.py)
set_tests_properties(
    test_fortsym_sympy_differential
    PROPERTIES
        SKIP_RETURN_CODE 77
        ENVIRONMENT "PYTHONPATH=${PROJECT_SOURCE_DIR}/python;FORTSYM_LIBRARY=$<TARGET_FILE:fortsym>")
add_test(
    NAME test_consumer_audit
    COMMAND
        ${Python3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/audit/test_consumer_audit.py)
set_tests_properties(
    test_consumer_audit
    PROPERTIES ENVIRONMENT "PYTHONDONTWRITEBYTECODE=1")
add_test(
    NAME test_provenance
    COMMAND
        ${Python3_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/provenance/test_check_provenance.py)
set_tests_properties(
    test_provenance
    PROPERTIES ENVIRONMENT "PYTHONDONTWRITEBYTECODE=1")
