module fortsym_obligation
    ! Build-time proof obligations: a derived formula is accepted only when
    ! its defining identity has been discharged, and the ledger records how.
    !
    ! A residual is first given to the native decision procedure and then,
    ! when SymEngine is linked, to SymEngine as an independent engine. Two
    ! engines that disagree are a failure, never a vote. Only when no engine
    ! decides does the ledger fall back to a numeric probe: the residual is
    ! evaluated to `probe_digits` decimal digits at `probe_points` random exact
    ! rational points, with every opaque function value and derivative (a
    ! "jet" variable such as B(theta, phi) or its partial derivatives) replaced
    ! by an independent random rational. A probe is evidence, not a proof, and
    ! is reported as PROBED so a reader can always tell the two apart.
    use, intrinsic :: iso_fortran_env, only: int64, output_unit
    use fortsym_arena, only: arena_t, NK_SYM, NK_FUNC, NK_ADD
    use fortsym_expr, only: expr_t, rat
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE, VERDICT_FALSE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_assume, only: assumption_context_t
    use fortsym_subs, only: subs_many
    use fortsym_numeric, only: numeric_precision_text
    use fortsym_print, only: print_expr
    implicit none
    private

    public :: obligation_t, obligation_ledger_t
    public :: OBLIGATION_PROVED, OBLIGATION_PROBED, OBLIGATION_FAILED, &
        OBLIGATION_UNKNOWN
    public :: prove_zero, ledger_report, ledger_holds, obligation_status_name
    public :: record_obligation, opaque_leaves

    !> Decided zero by a symbolic engine (and not contradicted by another).
    integer, parameter :: OBLIGATION_PROVED = 1
    !> No engine decided; every high-precision probe vanished.
    integer, parameter :: OBLIGATION_PROBED = 2
    !> An engine decided nonzero, engines disagreed, or a probe did not vanish.
    integer, parameter :: OBLIGATION_FAILED = 3
    !> Undecided and probing disabled or impossible.
    integer, parameter :: OBLIGATION_UNKNOWN = 4

    type :: obligation_t
        type(str_t) :: label
        integer :: status = OBLIGATION_UNKNOWN
        type(str_t) :: evidence
    end type obligation_t

    type :: obligation_ledger_t
        type(obligation_t), allocatable :: items(:)
        integer :: n = 0
        !> Accept numeric evidence when no engine decides.
        logical :: allow_probe = .true.
        integer :: probe_points = 8
        integer :: probe_digits = 40
        !> Ask SymEngine as a second engine when the native one decides.
        logical :: cross_check = .true.
        !> Print one line per obligation as it is discharged.
        logical :: verbose = .true.
    end type obligation_ledger_t

contains

    function obligation_status_name(status) result(name)
        integer, intent(in) :: status
        character(:), allocatable :: name

        select case (status)
        case (OBLIGATION_PROVED)
            name = "PROVED"
        case (OBLIGATION_PROBED)
            name = "PROBED"
        case (OBLIGATION_FAILED)
            name = "FAILED"
        case default
            name = "UNKNOWN"
        end select
    end function obligation_status_name

    !> Discharge `residual == 0` and record the verdict under `label`.
    subroutine prove_zero(ledger, label, residual, assumptions)
        type(obligation_ledger_t), intent(inout) :: ledger
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: residual
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(obligation_t) :: item
        type(native_engine_t) :: native
        type(symengine_engine_t) :: se
        type(engine_result_t) :: r1, r2
        character(:), allocatable :: why

        item%label = str(label)
        if (.not. associated(residual%a)) then
            item%status = OBLIGATION_FAILED
            item%evidence = str("invalid residual")
            call record_obligation(ledger, item)
            return
        end if
        if (present(assumptions)) then
            native = make_native_engine(residual%a, assumptions)
        else
            native = make_native_engine(residual%a)
        end if
        r1 = native%zero_test(residual)
        r2%ok = .false.
        if (ledger%cross_check .or. .not. decided(r1)) then
            se = make_symengine_engine(residual%a)
            r2 = se%zero_test(residual)
        end if
        if (decided(r1) .and. decided(r2)) then
            if (r1%verdict /= r2%verdict) then
                item%status = OBLIGATION_FAILED
                item%evidence = str("native and symengine disagree (native "// &
                    verdict_word(r1%verdict)//", symengine "// &
                    verdict_word(r2%verdict)//"): "//chars(print_expr(residual)))
            else if (r1%verdict == VERDICT_TRUE) then
                item%status = OBLIGATION_PROVED
                item%evidence = str("native zero test, symengine agrees")
            else
                item%status = OBLIGATION_FAILED
                item%evidence = str("decided nonzero: "//chars(print_expr(residual)))
            end if
        else if (decided(r1) .or. decided(r2)) then
            if (verdict_of(r1, r2) == VERDICT_TRUE) then
                item%status = OBLIGATION_PROVED
                if (decided(r1)) then
                    item%evidence = str("native zero test")
                else
                    item%evidence = str("symengine zero test")
                end if
            else
                item%status = OBLIGATION_FAILED
                item%evidence = str("decided nonzero: "//chars(print_expr(residual)))
            end if
        else if (ledger%allow_probe) then
            call probe(ledger, residual, item%status, why)
            item%evidence = str(why)
        else
            item%status = OBLIGATION_UNKNOWN
            item%evidence = str("no engine decided; probing disabled")
        end if
        call record_obligation(ledger, item)
    end subroutine prove_zero

    logical function decided(r)
        type(engine_result_t), intent(in) :: r

        decided = r%ok .and. (r%verdict == VERDICT_TRUE .or. r%verdict == VERDICT_FALSE)
    end function decided

    function verdict_word(v) result(w)
        integer, intent(in) :: v
        character(:), allocatable :: w

        if (v == VERDICT_TRUE) then
            w = "zero"
        else
            w = "nonzero"
        end if
    end function verdict_word

    integer function verdict_of(r1, r2)
        type(engine_result_t), intent(in) :: r1, r2

        if (decided(r1)) then
            verdict_of = r1%verdict
        else
            verdict_of = r2%verdict
        end if
    end function verdict_of

    !> Append a verdict established by the caller (for example a sign check).
    subroutine record_obligation(ledger, item)
        type(obligation_ledger_t), intent(inout) :: ledger
        type(obligation_t), intent(in) :: item
        type(obligation_t), allocatable :: grown(:)

        if (.not. allocated(ledger%items)) allocate (ledger%items(16))
        if (ledger%n == size(ledger%items)) then
            allocate (grown(2*size(ledger%items)))
            grown(1:ledger%n) = ledger%items(1:ledger%n)
            call move_alloc(grown, ledger%items)
        end if
        ledger%n = ledger%n + 1
        ledger%items(ledger%n) = item
        if (ledger%verbose) then
            write (output_unit, '(a)') obligation_status_name(item%status)// &
                repeat(" ", 8 - len(obligation_status_name(item%status)))// &
                chars(item%label)//"  ["//chars(item%evidence)//"]"
        end if
    end subroutine record_obligation

    !> True when every obligation is PROVED, or PROBED and probes are allowed.
    logical function ledger_holds(ledger)
        type(obligation_ledger_t), intent(in) :: ledger
        integer :: k

        ledger_holds = .true.
        do k = 1, ledger%n
            select case (ledger%items(k)%status)
            case (OBLIGATION_PROVED)
                continue
            case (OBLIGATION_PROBED)
                if (.not. ledger%allow_probe) ledger_holds = .false.
            case default
                ledger_holds = .false.
            end select
        end do
    end function ledger_holds

    !> Summary counts, one line.
    subroutine ledger_report(ledger, unit)
        type(obligation_ledger_t), intent(in) :: ledger
        integer, intent(in), optional :: unit
        integer :: u, k, counts(4)

        u = output_unit
        if (present(unit)) u = unit
        counts = 0
        do k = 1, ledger%n
            counts(ledger%items(k)%status) = counts(ledger%items(k)%status) + 1
        end do
        write (u, '(a,i0,a,i0,a,i0,a,i0,a,i0,a)') "obligations: ", ledger%n, &
            " total, ", counts(OBLIGATION_PROVED), " proved, ", &
            counts(OBLIGATION_PROBED), " probed, ", counts(OBLIGATION_FAILED), &
            " failed, ", counts(OBLIGATION_UNKNOWN), " unknown"
    end subroutine ledger_report

    !> High-precision probe with jet substitution (see the module comment).
    subroutine probe(ledger, residual, status, why)
        type(obligation_ledger_t), intent(in) :: ledger
        type(expr_t), intent(in) :: residual
        integer, intent(out) :: status
        character(:), allocatable, intent(out) :: why
        type(expr_t), allocatable :: olds(:), news(:), terms(:)
        type(expr_t) :: v
        integer, allocatable :: ids(:)
        integer :: n, k, j, tries, good
        integer(int64) :: seed
        real(kind(1.0d0)) :: value, scale, tv
        logical :: ok, all_ok
        character(:), allocatable :: text, msg
        character(len=64) :: buf

        call opaque_leaves(residual, ids)
        n = size(ids)
        allocate (olds(n), news(n))
        do k = 1, n
            olds(k)%a => residual%a
            olds(k)%id = ids(k)
            olds(k)%generation = residual%generation
        end do
        call top_terms(residual, terms)
        seed = 1234567_int64 + int(ledger%n, int64)*7919_int64
        good = 0
        tries = 0
        status = OBLIGATION_PROBED
        do while (good < ledger%probe_points .and. tries < 4*ledger%probe_points)
            tries = tries + 1
            do k = 1, n
                news(k) = rat(residual%a, 512_int64 + mod(next(seed), 2049_int64), &
                    1024_int64)
            end do
            v = subs_many(residual, olds, news)
            call numeric_precision_text(v, ledger%probe_digits, text, ok, msg)
            if (.not. ok) cycle
            read (text, *) value
            scale = 1.0d0
            all_ok = .true.
            do j = 1, size(terms)
                v = subs_many(terms(j), olds, news)
                call numeric_precision_text(v, ledger%probe_digits, text, ok, msg)
                if (.not. ok) then
                    all_ok = .false.
                    exit
                end if
                read (text, *) tv
                scale = max(scale, abs(tv))
            end do
            if (.not. all_ok) cycle
            good = good + 1
            if (abs(value) > 10.0d0**(-(ledger%probe_digits - 8))*scale) then
                status = OBLIGATION_FAILED
                write (buf, '(es12.4)') value
                why = "probe residual "//trim(adjustl(buf))//" at a random point"
                return
            end if
        end do
        if (good < ledger%probe_points) then
            status = OBLIGATION_UNKNOWN
            why = "no engine decided; probe points could not be evaluated"
            return
        end if
        write (buf, '(i0,a,i0,a)') good, " points, ", ledger%probe_digits, " digits"
        why = "no engine decided; numeric probe with jet substitution, "//trim(buf)
    end subroutine probe

    integer(int64) function next(seed)
        integer(int64), intent(inout) :: seed

        seed = mod(48271_int64*seed, 2147483647_int64)
        next = seed
    end function next

    !> Symbols and maximal applications of non-elementary functions: the
    !> independent "jet" variables of a residual.
    subroutine opaque_leaves(e, ids)
        type(expr_t), intent(in) :: e
        integer, allocatable, intent(out) :: ids(:)
        logical, allocatable :: seen(:)
        integer, allocatable :: buffer(:)
        integer :: n

        allocate (seen(e%a%size()), source=.false.)
        allocate (buffer(e%a%size()))
        n = 0
        call walk(e%a, e%id, seen, buffer, n)
        ids = buffer(1:n)
    end subroutine opaque_leaves

    recursive subroutine walk(a, id, seen, buffer, n)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical, intent(inout) :: seen(:)
        integer, intent(inout) :: buffer(:), n
        integer :: k

        if (seen(id)) return
        seen(id) = .true.
        select case (a%kind_of(id))
        case (NK_SYM)
            n = n + 1
            buffer(n) = id
            return
        case (NK_FUNC)
            if (.not. elementary(chars(a%name_of(id)))) then
                n = n + 1
                buffer(n) = id
                return
            end if
        end select
        do k = 1, a%nargs_of(id)
            call walk(a, a%arg_of(id, k), seen, buffer, n)
        end do
    end subroutine walk

    logical function elementary(name)
        character(*), intent(in) :: name

        select case (name)
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", "sinh", "cosh", &
                "tanh", "asinh", "acosh", "atanh", "exp", "log", "sqrt", "abs", &
                "erf", "erfc", "gamma")
            elementary = .true.
        case default
            elementary = .false.
        end select
    end function elementary

    subroutine top_terms(e, terms)
        type(expr_t), intent(in) :: e
        type(expr_t), allocatable, intent(out) :: terms(:)
        integer :: k

        if (e%a%kind_of(e%id) /= NK_ADD) then
            allocate (terms(0))
            return
        end if
        allocate (terms(e%a%nargs_of(e%id)))
        do k = 1, size(terms)
            terms(k)%a => e%a
            terms(k)%id = e%a%arg_of(e%id, k)
            terms(k)%generation = e%generation
        end do
    end subroutine top_terms

end module fortsym_obligation
