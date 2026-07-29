# Generator programs. Their output is committed under src/generated/ and CI
# re-runs them and fails on any diff, so a generated kernel can never outlive
# the code that produced it.
add_executable(gen_demo gen_demo.f90)
target_link_libraries(gen_demo PRIVATE fortsym)

# Reproducible end-to-end backend comparison. This remains separate from the
# correctness suite because timing assertions do not belong in CI tests.
add_executable(bench_native bench_native.f90)
target_link_libraries(bench_native PRIVATE fortsym)
