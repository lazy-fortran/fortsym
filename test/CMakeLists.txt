# Register a test only when its source exists, so a milestone can add the
# registration ahead of the file without breaking configure.
function(fortsym_add_test name source)
    if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${source}")
        return()
    endif()
    add_executable(${name} ${source})
    target_link_libraries(${name} PRIVATE fortsym)
    add_test(NAME ${name} COMMAND ${name})
endfunction()

# --- utilities ---
fortsym_add_test(test_fortsym_string util/test_fortsym_string.f90)

# --- core representation ---
fortsym_add_test(test_fortsym_arena core/test_fortsym_arena.f90)

# --- surface syntax ---
fortsym_add_test(test_fortsym_roundtrip io/test_fortsym_roundtrip.f90)

# --- engines ---
fortsym_add_test(test_fortsym_identities engine/test_fortsym_identities.f90)

# --- multi-engine adjudication ---
fortsym_add_test(test_fortsym_council council/test_fortsym_council.f90)

# --- code generation ---
fortsym_add_test(test_fortsym_kernel codegen/test_fortsym_kernel.f90)

# --- verification ---
fortsym_add_test(test_fortsym_check verify/test_fortsym_check.f90)

# --- C ABI over SymEngine ---
fortsym_add_test(test_fortsym_capi capi/test_fortsym_capi.f90)
