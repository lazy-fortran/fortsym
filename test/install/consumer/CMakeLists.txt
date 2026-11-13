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
if(FORTSYM_TEST_YACAS)
    configure_file(yacas.f90.in yacas.f90 COPYONLY)
    add_executable(yacas_consumer ${CMAKE_CURRENT_BINARY_DIR}/yacas.f90)
    target_link_libraries(yacas_consumer PRIVATE fortsym::fortsym)
    get_filename_component(_yacas_scripts "${FORTSYM_TEST_YACAS_SCRIPTS}" REALPATH)
    add_test(NAME installed_yacas_integration COMMAND yacas_consumer "${_yacas_scripts}")
endif()
