program test_fortsym_registry
    ! Independent registry oracle: registration order, duplicate refusal,
    ! metadata preservation, explicit probe dispatch, and the shipped geometry
    ! capability list are checked as user-visible contracts.
    use fortsym_registry, only: toolkit_probe, toolkit_registry_t, &
        registry_init, registry_register, registry_find, registry_count, &
        registry_name, registry_version, registry_owner, registry_capability, &
        registry_refusal, registry_enabled, registry_probe, &
        register_builtin_geometry, REGISTRY_NAME_LENGTH, REGISTRY_MESSAGE_LENGTH
    implicit none

    type(toolkit_registry_t) :: registry, builtins
    procedure(toolkit_probe), pointer :: probe
    logical :: accepted, available
    character(len=REGISTRY_MESSAGE_LENGTH) :: message
    character(len=REGISTRY_NAME_LENGTH + 1) :: too_long
    integer :: slot, failures

    failures = 0
    call registry_init(registry)
    call check(registry_count(registry) == 0, "registry starts empty")

    call registry_register(registry, "test", "1.2", "test_owner", &
        "a test capability", "refuses malformed test inputs", accepted, message)
    call check(accepted, "valid toolkit registers")
    call check(registry_count(registry) == 1, "registration increments count")
    slot = registry_find(registry, "test")
    call check(slot == 1, "registered toolkit is findable")
    call check(trim(registry_name(registry, slot)) == "test", &
        "registry preserves name")
    call check(trim(registry_version(registry, slot)) == "1.2", &
        "registry preserves version")
    call check(trim(registry_owner(registry, slot)) == "test_owner", &
        "registry preserves owner")
    call check(trim(registry_capability(registry, slot)) == "a test capability", &
        "registry preserves capability")
    call check(trim(registry_refusal(registry, slot)) == &
        "refuses malformed test inputs", "registry preserves refusal boundary")
    call check(registry_enabled(registry, slot), "new toolkit is enabled")

    call registry_register(registry, "test", "2.0", "other_owner", &
        "replacement", "replacement", accepted, message)
    call check(.not. accepted, "duplicate toolkit is refused")
    call check(registry_count(registry) == 1, "duplicate leaves count unchanged")
    too_long = ""
    too_long = repeat("x", len(too_long))
    call registry_register(registry, too_long, "1", "owner", "capability", &
        "refusal", accepted, message)
    call check(.not. accepted, "overlong toolkit name is refused")

    probe => available_probe
    call registry_register(registry, "probed", "1", "probe_owner", &
        "probe capability", "probe unavailable", accepted, message, probe)
    call check(accepted, "probed toolkit registers")
    call registry_probe(registry, "probed", available, message)
    call check(available, "registered probe is dispatched")
    call check(trim(message) == "probe says available", &
        "probe message is returned")
    nullify(probe)

    call registry_probe(registry, "missing", available, message)
    call check(.not. available, "missing toolkit is unavailable")

    call registry_init(builtins)
    call register_builtin_geometry(builtins, accepted, message)
    call check(accepted, "built-in geometry registration succeeds")
    call check(registry_count(builtins) == 10, &
        "all shipped geometry owners register")
    call check(registry_find(builtins, "chart") /= 0, &
        "chart owner is registered")
    call check(registry_find(builtins, "tensor") /= 0, &
        "tensor owner is registered")
    call check(registry_find(builtins, "relativity") /= 0, &
        "relativity owner is registered")
    call check(registry_find(builtins, "fourier_fem") /= 0, &
        "Fourier FEM owner is registered")

    if (failures /= 0) error stop 1
    print *, "test_fortsym_registry: all checks passed"

contains

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(*), intent(in) :: label

        if (.not. condition) then
            failures = failures + 1
            print *, "FAIL ", label
        end if
    end subroutine check

    subroutine available_probe(ok, probe_message)
        logical, intent(out) :: ok
        character(len=REGISTRY_MESSAGE_LENGTH), intent(out) :: probe_message

        ok = .true.
        probe_message = "probe says available"
    end subroutine available_probe

end program test_fortsym_registry
