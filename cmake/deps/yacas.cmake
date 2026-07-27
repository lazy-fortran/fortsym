# Yacas acquisition.
#
# Yacas is Tier 1 -- linked in-process -- despite not being packaged for the
# target distribution, because it is small and dependency-free enough that
# building it from source does not make installation cumbersome. A configure
# and build of just the engine takes a few seconds.
#
# It is built and linked as a SHARED library. That is not a preference: Yacas is
# LGPL-2.1-or-later, and dynamic linking is what satisfies the relinking
# obligation for an MIT program (LEGAL.md section 3).
#
# Yacas is not a pure C++ engine. Its algebra lives in .ys script files that the
# engine loads at run time, so the scripts directory travels with the build and
# its location is compiled in as FORTSYM_YACAS_SCRIPTS.

option(FORTSYM_ENABLE_YACAS "Build and link the Yacas engine" ON)

if(NOT FORTSYM_ENABLE_YACAS)
    message(STATUS "fortsym: Yacas disabled")
    return()
endif()

include(FetchContent)

# Pinned so a build is reproducible. Bump deliberately, never floating.
set(FORTSYM_YACAS_TAG "v1.9.1" CACHE STRING
    "Yacas git tag used by the fetch path")

# Only the engine is wanted. The console, the Qt GUI, the Jupyter kernel and the
# Java engine are the bulk of Yacas's build time and none of it is reachable
# from fortsym.
set(ENABLE_CYACAS_CONSOLE OFF CACHE BOOL "" FORCE)
set(ENABLE_CYACAS_GUI OFF CACHE BOOL "" FORCE)
set(ENABLE_CYACAS_KERNEL OFF CACHE BOOL "" FORCE)
set(ENABLE_CYACAS_UNIT_TESTS OFF CACHE BOOL "" FORCE)
set(ENABLE_CYACAS_BENCHMARKS OFF CACHE BOOL "" FORCE)
set(ENABLE_JYACAS OFF CACHE BOOL "" FORCE)
set(ENABLE_DOCS OFF CACHE BOOL "" FORCE)

# Shared, for the LGPL relinking obligation above.
set(BUILD_SHARED_LIBS ON CACHE BOOL "" FORCE)

FetchContent_Declare(yacas
    GIT_REPOSITORY https://github.com/grzegorzmazur/yacas.git
    GIT_TAG ${FORTSYM_YACAS_TAG}
    GIT_SHALLOW TRUE)
FetchContent_MakeAvailable(yacas)

add_library(fortsym_yacas INTERFACE)
target_include_directories(fortsym_yacas SYSTEM INTERFACE
    ${yacas_SOURCE_DIR}/cyacas/libyacas/include)
target_link_libraries(fortsym_yacas INTERFACE libyacas)

# Where the engine finds its script library at run time.
set(FORTSYM_YACAS_SCRIPTS "${yacas_SOURCE_DIR}/scripts"
    CACHE PATH "Yacas .ys script library" FORCE)

add_library(fortsym::yacas ALIAS fortsym_yacas)

message(STATUS "fortsym: Yacas ${FORTSYM_YACAS_TAG}, scripts at ${FORTSYM_YACAS_SCRIPTS}")
