add_library(fortsym STATIC)

# Fortran and C++ module sources, grouped by layer and kept alphabetical within
# each group, one per line, so parallel work rarely collides here.
target_sources(fortsym PRIVATE
    # C ABI over SymEngine
    capi/fsym_shim.cpp
    capi/fortsym_capi.f90

    # utilities
    util/fortsym_string.f90
)

target_include_directories(fortsym
    PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY}
    PRIVATE ${PROJECT_SOURCE_DIR}/src/capi)

target_link_libraries(fortsym PUBLIC fortsym::symengine)

# The C++ shim pulls in the C++ runtime; a Fortran-driven link needs it named
# explicitly because the linker is invoked by gfortran, not g++.
target_link_libraries(fortsym PUBLIC stdc++)
