# Worked examples. Built so they cannot rot, but not registered as tests.
add_executable(example_workflow example_workflow.f90)
target_link_libraries(example_workflow PRIVATE fortsym)
