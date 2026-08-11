program test_fortsym_backend
    ! The backend handoff is checked as a protocol, not by comparing its own
    ! output to a fixture. Expression payloads must round-trip, evidence must
    ! preserve the independent three-valued engine decision, and generated
    ! source must remain explicitly unproved until a consumer reads it back.
    use fortsym_string, only: str, str_t, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(*), operator(+), operator(==)
    use fortsym_parse, only: parse_expr
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_kernel, only: kernel_spec_t
    use fortsym_backend
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: expression, restored, x, roots(1)
    type(native_engine_t) :: engine
    type(backend_evidence_t) :: evidence
    type(backend_result_t) :: generated
    type(kernel_spec_t) :: spec
    type(str_t) :: serialized, json
    character(:), allocatable :: why
    logical :: ok
    integer :: nfail

    nfail = 0
    call arena%init()
    x = sym(arena, "x")
    expression = x*x + num(arena, 1)

    call serialize_expression(expression, serialized, ok, why)
    call check("expression serialization succeeds", ok)
    call check("expression schema is versioned", &
        index(chars(serialized), EXPRESSION_SCHEMA//"|") == 1)
    call deserialize_expression(arena, chars(serialized), restored, ok, why)
    call check("expression serialization round-trips", ok .and. restored == expression)

    call deserialize_expression(arena, "fortsym.expression.v0|x", restored, ok, why)
    call check("unknown expression schema is refused", .not. ok)

    engine = make_native_engine(arena)
    call assess_equivalence(engine, x, x, evidence)
    call check("identity is proved", evidence%status == BACKEND_PROVED)
    call assess_equivalence(engine, num(arena, 1), num(arena, 0), evidence)
    call check("non-identity is disproved", evidence%status == BACKEND_DISPROVED)

    expression = parse_expr(arena, "gamma(x + 1) - x*gamma(x)", ok, why)
    call check("unknown identity parses", ok)
    call assess_identity(engine, expression, evidence)
    call check("unsupported identity remains unknown", &
        evidence%status == BACKEND_UNKNOWN)
    json = evidence_json(evidence)
    call check("evidence has a stable schema", &
        index(chars(json), '"schema":"fortsym.evidence.v1"') > 0)
    call check("evidence names unknown", &
        index(chars(json), '"status":"UNKNOWN"') > 0)

    roots(1) = x*x + num(arena, 1)
    spec%name = str("backend_test")
    spec%temp_prefix = str("t")
    spec%generator = str("backend_protocol_test")
    allocate (spec%args(1), spec%outputs(1))
    spec%args(1) = str("x")
    spec%outputs(1) = str("y")
    call emit_backend_kernel(roots, spec, generated)
    call check("backend kernel emission succeeds", generated%ok)
    call check("kernel carries the existing cost schema", &
        index(chars(generated%cost_record), &
        '"schema":"fortsym.operation_cost.v1"') > 0)
    call check("generated source is not misreported as proved", &
        generated%evidence%status == BACKEND_UNKNOWN)
    call check("generation names the readback obligation", &
        index(chars(generated%evidence%reason), "readback") > 0)

    if (nfail /= 0) then
        print *, "test_fortsym_backend: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_backend: all checks passed"

contains

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        if (.not. condition) then
            print *, "FAIL: ", label
            nfail = nfail + 1
        end if
    end subroutine check

end program test_fortsym_backend
