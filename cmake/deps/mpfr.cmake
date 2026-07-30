# MPFR is used directly for deterministic nearest-even projection of exact
# rationals to real64. Keep this a direct dependency rather than relying on
# FLINT or SymEngine's transitive link interfaces.
find_path(FORTSYM_MPFR_INCLUDE_DIR
    NAMES mpfr.h
    REQUIRED)
find_library(FORTSYM_MPFR_LIBRARY
    NAMES mpfr
    REQUIRED)
if(FORTSYM_MPFR_LIBRARY MATCHES "\\.a$")
    message(FATAL_ERROR
        "fortsym requires a shared MPFR library; found static archive "
        "${FORTSYM_MPFR_LIBRARY}")
endif()

file(READ "${FORTSYM_MPFR_INCLUDE_DIR}/mpfr.h"
    FORTSYM_MPFR_HEADER)
if(NOT FORTSYM_MPFR_HEADER MATCHES
        "#define MPFR_VERSION_STRING[\t ]+\"4\\.2\\.2\"")
    message(FATAL_ERROR
        "fortsym pins MPFR 4.2.2; ${FORTSYM_MPFR_INCLUDE_DIR}/mpfr.h "
        "reports another version")
endif()

add_library(fortsym_mpfr INTERFACE)
target_include_directories(fortsym_mpfr INTERFACE
    "${FORTSYM_MPFR_INCLUDE_DIR}")
target_link_libraries(fortsym_mpfr INTERFACE
    "${FORTSYM_MPFR_LIBRARY}")
add_library(fortsym::mpfr ALIAS fortsym_mpfr)
