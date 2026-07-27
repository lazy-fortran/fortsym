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

# Only the engine is wanted. The Qt GUI, the Jupyter kernel and the Java engine
# are the bulk of Yacas's build time and none of it is reachable from fortsym.
#
# The console stays ON, which looks contradictory but is not: Yacas derives its
# internal ENABLE_CYACAS from the console/GUI/kernel switches, and with all three
# off it never adds the directory that builds libyacas at all. Turning the
# console on is the cheapest way to get the library -- it costs one small
# executable that nothing links against.
set(ENABLE_CYACAS_CONSOLE ON CACHE BOOL "" FORCE)
set(ENABLE_CYACAS_GUI OFF CACHE BOOL "" FORCE)
set(ENABLE_CYACAS_KERNEL OFF CACHE BOOL "" FORCE)
set(ENABLE_CYACAS_UNIT_TESTS OFF CACHE BOOL "" FORCE)
set(ENABLE_CYACAS_BENCHMARKS OFF CACHE BOOL "" FORCE)
set(ENABLE_JYACAS OFF CACHE BOOL "" FORCE)
set(ENABLE_DOCS OFF CACHE BOOL "" FORCE)

# Shared, for the LGPL relinking obligation above.
set(BUILD_SHARED_LIBS ON CACHE BOOL "" FORCE)

# Yacas calls include(CTest), which registers its own suite against the parent
# project. Without this, fortsym's ctest run grows from 11 tests to 64 and from
# seconds to minutes, testing Yacas rather than fortsym. fortsym uses
# enable_testing() and add_test() directly, so its own tests are unaffected.
set(_fortsym_saved_build_testing "${BUILD_TESTING}")
set(BUILD_TESTING OFF CACHE BOOL "" FORCE)

FetchContent_Declare(yacas
    GIT_REPOSITORY https://github.com/grzegorzmazur/yacas.git
    GIT_TAG ${FORTSYM_YACAS_TAG}
    GIT_SHALLOW TRUE)
FetchContent_MakeAvailable(yacas)

set(BUILD_TESTING "${_fortsym_saved_build_testing}" CACHE BOOL "" FORCE)
unset(_fortsym_saved_build_testing)

add_library(fortsym_yacas INTERFACE)
# libyacas's headers include libyacas_mp's, so both trees are needed.
target_include_directories(fortsym_yacas SYSTEM INTERFACE
    ${yacas_SOURCE_DIR}/cyacas/libyacas/include
    ${yacas_SOURCE_DIR}/cyacas/libyacas_mp/include)
target_link_libraries(fortsym_yacas INTERFACE libyacas)

# Where the engine finds its script library at run time.
set(FORTSYM_YACAS_SCRIPTS "${yacas_SOURCE_DIR}/scripts"
    CACHE PATH "Yacas .ys script library" FORCE)

add_library(fortsym::yacas ALIAS fortsym_yacas)

message(STATUS "fortsym: Yacas ${FORTSYM_YACAS_TAG}, scripts at ${FORTSYM_YACAS_SCRIPTS}")
