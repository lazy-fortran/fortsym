module fortsym_backend
    ! A small, typed handoff boundary for Fortran synthesis consumers.
    !
    ! The protocol deliberately separates three things that are easy to
    ! conflate in a code generator: an expression's stable representation, a
    ! symbolic identity decision, and source emission. Emitting source is not
    ! itself a proof; a consumer must read it back and assess the resulting
    ! expression before claiming equivalence.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, is_valid, operator(-)
    use fortsym_parse, only: parse_expr
    use fortsym_print, only: print_expr
    use fortsym_engine, only: engine_t, engine_result_t, &
        VERDICT_UNKNOWN, VERDICT_TRUE, VERDICT_FALSE
    use fortsym_kernel, only: kernel_spec_t, emit_kernel
    implicit none
    private

    public :: BACKEND_PROTOCOL_VERSION, EXPRESSION_SCHEMA
    public :: BACKEND_PROVED, BACKEND_DISPROVED, BACKEND_UNKNOWN
    public :: backend_evidence_t, backend_result_t
    public :: backend_status_name, serialize_expression, deserialize_expression
    public :: assess_identity, assess_equivalence, evidence_json
    public :: emit_backend_kernel

    character(*), parameter :: BACKEND_PROTOCOL_VERSION = "fortsym.backend.v1"
    character(*), parameter :: EXPRESSION_SCHEMA = "fortsym.expression.v1"

    integer(int64), parameter :: BACKEND_PROVED = 1_int64
    integer(int64), parameter :: BACKEND_DISPROVED = 2_int64
    integer(int64), parameter :: BACKEND_UNKNOWN = 3_int64

    !> Evidence returned by a symbolic obligation. UNKNOWN is a valid result:
    !> it means the backend declined to claim a theorem.
    type :: backend_evidence_t
        integer(int64) :: status = BACKEND_UNKNOWN
        type(str_t)    :: reason
        type(str_t)    :: certificate
        integer(int64) :: probes = 0_int64
    end type backend_evidence_t

    !> Typed output of the bounded source-generation handoff.
    !>
    !> `ok` reports whether source generation completed. `evidence%status`
    !> remains UNKNOWN until the consumer performs source readback and checks
    !> the original/generated difference.
    type :: backend_result_t
        logical                :: ok = .false.
        type(str_t)            :: source
        type(str_t)            :: cost_record
        type(str_t)            :: message
        type(backend_evidence_t) :: evidence
    end type backend_result_t

contains

    pure function backend_status_name(status) result(name)
        integer(int64), intent(in) :: status
        type(str_t)                 :: name

        select case (status)
        case (BACKEND_PROVED)
            name = str("PROVED")
        case (BACKEND_DISPROVED)
            name = str("DISPROVED")
        case default
            name = str("UNKNOWN")
        end select
    end function backend_status_name

    !> Serialize one expression in the versioned canonical native dialect.
    subroutine serialize_expression(e, text, ok, why)
        type(expr_t), intent(in) :: e
        type(str_t),  intent(out) :: text
        logical,      intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        ok = .false.
        text = str("")
        why = ""
        if (.not. is_valid(e)) then
            why = "cannot serialize an invalid expression"
            return
        end if
        text = str(EXPRESSION_SCHEMA//"|"//chars(print_expr(e)))
        ok = .true.
    end subroutine serialize_expression

    !> Read the versioned expression form and reject other schemas explicitly.
    subroutine deserialize_expression(a, text, e, ok, why)
        type(arena_t), target, intent(inout) :: a
        character(*), intent(in) :: text
        type(expr_t), intent(out) :: e
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer :: prefix_length

        e = expr_t()
        ok = .false.
        why = ""
        prefix_length = len(EXPRESSION_SCHEMA) + 1
        if (len(text) < prefix_length .or. &
            text(1:prefix_length) /= EXPRESSION_SCHEMA//"|") then
            why = "unsupported expression serialization schema"
            return
        end if
        e = parse_expr(a, text(prefix_length + 1:), ok, why)
        if (.not. ok) then
            why = "invalid expression payload: "//why
        end if
    end subroutine deserialize_expression

    !> Assess a zero obligation through the selected symbolic engine.
    subroutine assess_identity(engine, e, evidence)
        class(engine_t), intent(inout) :: engine
        type(expr_t),    intent(in)    :: e
        type(backend_evidence_t), intent(out) :: evidence
        type(engine_result_t) :: result

        evidence%status = BACKEND_UNKNOWN
        evidence%reason = str("")
        evidence%certificate = str("")
        evidence%probes = 0_int64
        result = engine%zero_test(e)
        select case (result%verdict)
        case (VERDICT_TRUE)
            evidence%status = BACKEND_PROVED
            evidence%certificate = str("engine-zero-test")
        case (VERDICT_FALSE)
            evidence%status = BACKEND_DISPROVED
            evidence%reason = result%message
        case (VERDICT_UNKNOWN)
            evidence%status = BACKEND_UNKNOWN
            evidence%reason = result%message
        case default
            evidence%reason = str("engine returned an invalid verdict")
        end select
    end subroutine assess_identity

    !> Assess equality without making a separate proof claim for generation.
    subroutine assess_equivalence(engine, left, right, evidence)
        class(engine_t), intent(inout) :: engine
        type(expr_t),    intent(in)    :: left, right
        type(backend_evidence_t), intent(out) :: evidence
        type(expr_t) :: difference

        difference = left - right
        call assess_identity(engine, difference, evidence)
    end subroutine assess_equivalence

    !> Compact machine-readable evidence record for a consumer manifest.
    function evidence_json(evidence) result(text)
        type(backend_evidence_t), intent(in) :: evidence
        type(str_t) :: text
        type(strbuf_t) :: buffer

        call buffer%append('{"schema":"fortsym.evidence.v1","status":"')
        call buffer%append(chars(backend_status_name(evidence%status)))
        call buffer%append('","probes":')
        call buffer%append(str(evidence%probes))
        call buffer%append(',"reason":')
        call append_json_string(buffer, chars(evidence%reason))
        call buffer%append(',"certificate":')
        call append_json_string(buffer, chars(evidence%certificate))
        call buffer%append('}')
        text = buffer%to_str()
    end function evidence_json

    !> Generate a kernel and its existing operation-cost record through the
    !> protocol boundary. The result remains UNKNOWN until readback validation.
    subroutine emit_backend_kernel(roots, spec, result)
        type(expr_t), intent(in) :: roots(:)
        type(kernel_spec_t), intent(in) :: spec
        type(backend_result_t), intent(out) :: result
        logical :: ok
        type(str_t) :: cost_record, source
        character(:), allocatable :: message

        result%ok = .false.
        result%source = str("")
        result%cost_record = str("")
        result%message = str("")
        result%evidence%status = BACKEND_UNKNOWN
        result%evidence%reason = str("")
        result%evidence%certificate = str("")
        result%evidence%probes = 0_int64

        source = emit_kernel(roots, spec, ok, cost_record, message)
        result%ok = ok
        result%source = source
        result%cost_record = cost_record
        result%message = str(message)
        if (ok) then
            result%evidence%reason = str( &
                "source generated; readback equivalence is still required")
        else
            result%evidence%reason = str(message)
        end if
    end subroutine emit_backend_kernel

    subroutine append_json_string(buffer, text)
        type(strbuf_t), intent(inout) :: buffer
        character(*), intent(in) :: text
        integer :: k, code

        call buffer%append('"')
        do k = 1, len(text)
            code = iachar(text(k:k))
            select case (code)
            case (34)
                call buffer%append('\"')
            case (92)
                call buffer%append('\\')
            case (10)
                call buffer%append('\n')
            case (13)
                call buffer%append('\r')
            case default
                call buffer%append(text(k:k))
            end select
        end do
        call buffer%append('"')
    end subroutine append_json_string

end module fortsym_backend
