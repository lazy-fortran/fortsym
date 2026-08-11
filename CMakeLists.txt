cmake_minimum_required(VERSION 3.22)

project(fortsym LANGUAGES Fortran C CXX)

# Standard install directory variables (CMAKE_INSTALL_BINDIR etc.).
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

# Export compile_commands.json so clangd resolves the C++ shim include paths.
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Detect standalone vs subproject build so testing defaults sanely.
set(FORTSYM_IS_STANDALONE OFF)
if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    set(FORTSYM_IS_STANDALONE ON)
endif()

option(FORTSYM_BUILD_TESTING "Build tests for fortsym" ${FORTSYM_IS_STANDALONE})

# SymEngine is the symbolic core. The default is the system package, which
# carries GMP/MPFR/FLINT already configured. Turning this OFF switches to the
# fetch-and-build path in cmake/deps/symengine.cmake for machines and CI images
# that have no SymEngine package.
option(FORTSYM_USE_SYSTEM_DEPS "Use the system SymEngine instead of fetching it" ON)

# Shared library inclusion needs PIC-clean objects.
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Pin a modern Fortran standard for free-form, full-feature builds.
set(CMAKE_Fortran_STANDARD 2018)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# C++17 is the floor: Yacas's multiprecision headers use std::string_view, and
# SymEngine is happy at any standard from 11 upward.
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

include(cmake/deps/symengine.cmake)
include(cmake/deps/mpfr.cmake)
include(cmake/deps/flint.cmake)
include(cmake/deps/yacas.cmake)

# Plotting is an adapter boundary: fortsym samples expressions and maps
# options, while fortplot owns rendering. Keep the upstream revision pinned and
# avoid pulling fortarray into the symbolic library.
include(FetchContent)
set(FORTPLOT_BUILD_FORTARRAY_ADAPTER OFF CACHE BOOL "" FORCE)
FetchContent_Declare(fortplot
    GIT_REPOSITORY https://github.com/lazy-fortran/fortplot.git
    GIT_TAG 178e6fd2f0440d557faf01cc36cd6ec2a2ef5244
    GIT_SHALLOW FALSE
)
FetchContent_MakeAvailable(fortplot)

add_subdirectory(src)
add_subdirectory(app)
add_subdirectory(example)

if(FORTSYM_BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()

install(TARGETS fortsym
    EXPORT fortsymTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
install(FILES src/capi/fortsym.h
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    FILES_MATCHING PATTERN "*.mod")
install(EXPORT fortsymTargets
    FILE fortsymTargets.cmake
    NAMESPACE fortsym::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortsym)
configure_package_config_file(
    cmake/fortsymConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/fortsymConfig.cmake
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortsym)
write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/fortsymConfigVersion.cmake
    VERSION 0.1.0
    COMPATIBILITY SameMajorVersion)
install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/fortsymConfig.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/fortsymConfigVersion.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortsym)
install(DIRECTORY python/fortsym/
    DESTINATION ${CMAKE_INSTALL_DATADIR}/fortsym/python/fortsym
    FILES_MATCHING PATTERN "*.py")
install(FILES python/README.md
    DESTINATION ${CMAKE_INSTALL_DATADIR}/fortsym)
