add_library(fortsym SHARED)

# Fortran and C++ module sources, grouped by layer and kept alphabetical within
# each group, one per line, so parallel work rarely collides here.
target_sources(fortsym PRIVATE
    # C ABI over SymEngine
    capi/fsym_shim.cpp
    capi/fsym_yacas.cpp
    capi/fortsym_capi.f90
    capi/fortsym_public_capi.f90

    # utilities
    util/fortsym_string.f90

    # core representation
    core/fortsym_algebraic.f90
    core/fortsym_cache.f90
    core/fortsym_exact.f90
    core/fortsym_arena.f90
    core/fortsym_expr.f90
    core/fortsym.f90
    core/fortsym_assume.f90
    core/fortsym_assume_api.f90
    core/fortsym_predicates.f90
    core/fortsym_relation.f90
    core/fortsym_subs.f90

    # surface syntax
    io/fortsym_dialect.f90
    io/fortsym_parse.f90
    io/fortsym_print.f90
    io/fortsym_latex.f90
    io/fortsym_plot.f90
    io/fortsym_wl_num.f90
    io/fortsym_wl_solve.f90
    io/fortsym_wl.f90

    # engines
    engine/fortsym_engine.f90
    engine/fortsym_engine_native.f90
    engine/fortsym_engine_symengine.f90
    engine/fortsym_engine_ext.f90
    engine/fortsym_engine_yacas.f90
    engine/fortsym_proc.f90

    # exact linear algebra
    algebra/fortsym_linalg.f90
    algebra/fortsym_complexdom.f90
    algebra/fortsym_defint.f90
    algebra/fortsym_integrate.f90
    algebra/fortsym_integrate_adapter.f90
    algebra/fortsym_inequality.f90
    algebra/fortsym_limit_adapter.f90
    algebra/fortsym_linsolve_adapter.f90
    algebra/fortsym_limits.f90
    algebra/fortsym_matrix_adapter.f90
    algebra/fortsym_matrix.f90
    algebra/fortsym_poly.f90
    algebra/fortsym_polysolve.f90
    algebra/fortsym_series_adapter.f90
    algebra/fortsym_solve_adapter.f90
    algebra/fortsym_solve_rational.f90
    algebra/fortsym_sums.f90
    algebra/fortsym_trigrewrite.f90

    # multi-engine adjudication
    council/fortsym_council.f90

    # code generation
    codegen/fortsym_names.f90
    codegen/fortsym_precision.f90
    codegen/fortsym_enzyme.f90
    codegen/fortsym_kernel_target.f90
    codegen/fortsym_kernel_emit.f90
    codegen/fortsym_kernel_ir.f90
    codegen/fortsym_kernel.f90
    codegen/fortsym_backend.f90
    codegen/fortsym_wl_f90.f90
    codegen/fortsym_stability.f90

    # verification
    verify/fortsym_eval.f90
    verify/fortsym_numeric.f90
    verify/fortsym_accuracy.f90
    verify/fortsym_check.f90
    verify/fortsym_rk_trees.f90
    core/fortsym_quadratic.f90
    algebra/fortsym_quadratic_linalg.f90
    algebra/fortsym_quadratic_poly.f90
    algebra/fortsym_quadratic_multipoly.f90
    verify/fortsym_rk.f90

    # differential geometry
    calculus/fortsym_domain.f90
    calculus/fortsym_diff.f90
    calculus/fortsym_geometry_metadata.f90
    calculus/fortsym_ode.f90
    calculus/fortsym_chart.f90
    calculus/fortsym_chart_map.f90
    calculus/fortsym_metric.f90
    calculus/fortsym_volume.f90
    calculus/fortsym_index.f90
    calculus/fortsym_form.f90
    calculus/fortsym_form_tensor.f90
    calculus/fortsym_flux.f90
    calculus/fortsym_magnetic.f90
    calculus/fortsym_magnetic_weak.f90
    calculus/fortsym_products.f90
    calculus/fortsym_registry.f90
    calculus/fortsym_relativity.f90
    calculus/fortsym_spacetime_form.f90
    calculus/fortsym_spacetime_form_tensor.f90
    calculus/fortsym_spacetime_tensor.f90
    calculus/fortsym_maxwell.f90
    calculus/fortsym_tensor.f90
    calculus/fortsym_connection.f90
    calculus/fortsym_toroidal.f90

    # reading Fortran back
    parse_fortran/fortsym_fparse.f90
)

target_include_directories(fortsym
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
        $<INSTALL_INTERFACE:include>
    PRIVATE ${PROJECT_SOURCE_DIR}/src/capi)

target_link_libraries(fortsym PUBLIC
    $<BUILD_INTERFACE:fortsym::symengine>
    $<BUILD_INTERFACE:fortsym::mpfr>
    $<BUILD_INTERFACE:fortsym::flint>
    $<BUILD_INTERFACE:fortplot::fortplot>
    ${CMAKE_DL_LIBS})

# Yacas is optional at configure time; when present its shim and backend join
# the library and the scripts path is compiled in.
if(TARGET fortsym::yacas)
    target_link_libraries(fortsym PUBLIC $<BUILD_INTERFACE:fortsym::yacas>)
    set_property(SOURCE capi/fsym_yacas.cpp APPEND PROPERTY
        COMPILE_DEFINITIONS FORTSYM_HAVE_YACAS)
endif()

# The C++ shim pulls in the C++ runtime; a Fortran-driven link needs it named
# explicitly because the linker is invoked by gfortran, not g++.
target_link_libraries(fortsym PUBLIC stdc++)
