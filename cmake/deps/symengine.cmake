# SymEngine acquisition.
#
# Two paths, selected by FORTSYM_USE_SYSTEM_DEPS:
#
#   ON  (default) find_package(SymEngine). Distro packages already carry GMP and
#                 usually MPFR/MPC/FLINT/LLVM, which is what the high-precision
#                 oracle and the LLVM evaluation backend want.
#   OFF           FetchContent builds SymEngine from a pinned tag. GMP must still
#                 be present; SymEngine cannot bootstrap it, and building GMP
#                 from source here would be slower than any consumer wants.
#
# Both paths end with the same imported target, fortsym::symengine, so nothing
# downstream has to branch.

if(FORTSYM_USE_SYSTEM_DEPS)
    find_package(SymEngine REQUIRED CONFIG)

    add_library(fortsym_symengine INTERFACE)
    target_include_directories(fortsym_symengine SYSTEM
        INTERFACE ${SYMENGINE_INCLUDE_DIRS})
    target_link_libraries(fortsym_symengine INTERFACE ${SYMENGINE_LIBRARIES})

    message(STATUS "fortsym: using system SymEngine ${SymEngine_VERSION}")
else()
    include(FetchContent)

    # Pinned so a fetch build is reproducible. Bump deliberately, never floating.
    set(FORTSYM_SYMENGINE_TAG "v0.14.0" CACHE STRING
        "SymEngine git tag used by the fetch path")

    # SymEngine's own tests and benchmarks are a large share of its build time
    # and none of it is reachable from fortsym.
    set(BUILD_TESTS OFF CACHE BOOL "" FORCE)
    set(BUILD_BENCHMARKS OFF CACHE BOOL "" FORCE)
    set(INTEGER_CLASS "gmp" CACHE STRING "" FORCE)
    set(WITH_SYMENGINE_RCP ON CACHE BOOL "" FORCE)

    FetchContent_Declare(symengine
        GIT_REPOSITORY https://github.com/symengine/symengine.git
        GIT_TAG ${FORTSYM_SYMENGINE_TAG}
        GIT_SHALLOW TRUE)
    FetchContent_MakeAvailable(symengine)

    add_library(fortsym_symengine INTERFACE)
    # The built tree needs both the source root (for symengine/*.h) and the
    # binary root (for the generated symengine_config.h).
    target_include_directories(fortsym_symengine SYSTEM INTERFACE
        ${symengine_SOURCE_DIR}
        ${symengine_BINARY_DIR})
    target_link_libraries(fortsym_symengine INTERFACE symengine)

    message(STATUS "fortsym: fetching SymEngine ${FORTSYM_SYMENGINE_TAG}")
endif()

add_library(fortsym::symengine ALIAS fortsym_symengine)
