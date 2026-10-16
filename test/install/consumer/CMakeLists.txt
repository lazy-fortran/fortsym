cmake_minimum_required(VERSION 3.22)
project(fortsym_installed_consumer LANGUAGES Fortran)

# The installed shared-library interface needs none of these development
# packages. Disabling discovery also catches accidental dependency leakage.
set(CMAKE_DISABLE_FIND_PACKAGE_SymEngine TRUE)
set(CMAKE_DISABLE_FIND_PACKAGE_MPFR TRUE)
set(CMAKE_DISABLE_FIND_PACKAGE_FLINT TRUE)
find_package(fortsym CONFIG REQUIRED)

configure_file(main.f90.in main.f90 COPYONLY)
add_executable(consumer ${CMAKE_CURRENT_BINARY_DIR}/main.f90)
target_link_libraries(consumer PRIVATE fortsym::fortsym)
enable_testing()
add_test(NAME installed_expression_and_derivative COMMAND consumer)
