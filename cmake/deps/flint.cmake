# FLINT acquisition.
#
# Exact scalar and polynomial domains use FLINT through its C ABI. LEGAL.md
# permits shared linking only, so a machine exposing only a static archive is a
# configuration error rather than an invitation to produce a noncompliant
# binary.

find_path(FORTSYM_FLINT_INCLUDE_DIR
    NAMES flint/fmpq.h
    REQUIRED)
find_library(FORTSYM_FLINT_LIBRARY
    NAMES flint
    REQUIRED)

if(FORTSYM_FLINT_LIBRARY MATCHES "\\.a$")
    message(FATAL_ERROR
        "fortsym requires a shared FLINT library; found static archive "
        "${FORTSYM_FLINT_LIBRARY}")
endif()

file(READ "${FORTSYM_FLINT_INCLUDE_DIR}/flint/flint.h"
    _fortsym_flint_header)
if(NOT _fortsym_flint_header MATCHES
        "#define FLINT_VERSION \"3\\.6\\.0\"")
    message(FATAL_ERROR
        "fortsym pins FLINT 3.6.0; ${FORTSYM_FLINT_INCLUDE_DIR}/flint/flint.h "
        "reports another version")
endif()
unset(_fortsym_flint_header)

add_library(fortsym_flint INTERFACE)
target_include_directories(fortsym_flint SYSTEM INTERFACE
    "${FORTSYM_FLINT_INCLUDE_DIR}")
target_link_libraries(fortsym_flint INTERFACE "${FORTSYM_FLINT_LIBRARY}")
add_library(fortsym::flint ALIAS fortsym_flint)
