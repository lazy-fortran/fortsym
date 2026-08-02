module fortsym_engine_symengine
    ! SymEngine as a fortsym backend.
    !
    ! This is the workhorse: linked in-process, MIT licensed, fastest of the
    ! available engines, and carrying the zero-decision procedure implemented in
    ! src/capi/fsym_shim.cpp. SymEngine's own simplify() is far too weak to build
    ! on -- it leaves sin^2+cos^2 and (x^2-1)/(x-1) untouched -- so the shim
    ! supplies real algorithms and this module is the Fortran face of them.
    !
    ! Conversion is asymmetric, deliberately.
    !
    !   fortsym -> SymEngine is structural: walk the arena and build the handle
    !   directly. It is short, exact, and avoids a parse on the hot path.
    !
    !   SymEngine -> fortsym goes through text. Walking SymEngine's structure
    !   back would mean mapping its sixty-odd type codes and tracking them across
    !   releases, for a conversion that happens once per call rather than once
    !   per node. The text path reuses the printer and parser that the round-trip
    !   test already covers, so it is the better-tested of the two directions.
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_long, c_char, &
        c_null_char, c_size_t
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, &
        NK_BIG_REAL
    use fortsym_expr, only: expr_t
    use fortsym_dialect, only: dialect, DIA_SYMENGINE
    use fortsym_parse, only: parse_expr_in
    use fortsym_engine, only: engine_t, engine_result_t, wall_seconds, &
        VERDICT_UNKNOWN, VERDICT_TRUE, VERDICT_FALSE, &
        CAP_ZERO_TEST, CAP_SIMPLIFY, CAP_DIFF, CAP_EXPAND, CAP_EVAL
    use fortsym_capi
    implicit none
    private

    public :: symengine_engine_t, make_symengine_engine, symengine_evalf_text

    integer, parameter :: dp = real64
    integer, parameter :: MAX_EVALF_DIGITS = 512
    integer, parameter :: MAX_EVALF_NODES = 8192

    type, extends(engine_t) :: symengine_engine_t
        !> The arena results are parsed back into. Set at construction so a
        !> result never lands in a different arena from its input.
        type(arena_t), pointer :: home => null()
    contains
        procedure :: zero_test => se_zero_test
        procedure :: simplify => se_simplify
        procedure :: diff => se_diff
        procedure :: expand => se_expand
    end type symengine_engine_t

contains

    !> Build the backend. SymEngine is linked, so it is always available; the
    !> flag exists to keep every engine uniform to the council.
    function make_symengine_engine(home) result(eng)
        type(arena_t), target, intent(inout) :: home
        type(symengine_engine_t)             :: eng

        eng%name = str("symengine")
        eng%available = .true.
        eng%in_process = .true.
        eng%caps = CAP_ZERO_TEST + CAP_SIMPLIFY + CAP_DIFF + CAP_EXPAND + CAP_EVAL
        eng%home => home
    end function make_symengine_engine

    ! ------------------------------------------------- fortsym -> SymEngine --

    !> Build a SymEngine handle for a fortsym node. The caller owns the result
    !> and must release it with basic_free_heap.
    recursive function to_symengine(a, id) result(h)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        type(c_ptr)               :: h
        type(c_ptr) :: lhs, rhs
        integer :: k, rc
        character(:), allocatable :: name

        h = basic_new_heap()

        select case (a%kind_of(id))

        case (NK_INT)
            rc = integer_set_si(h, int(a%num_of(id), c_long))

        case (NK_RAT)
            rc = rational_set_si(h, int(a%num_of(id), c_long), &
                int(a%den_of(id), c_long))

        case (NK_BIG_INT, NK_BIG_RAT)
            rc = basic_parse(h, cstr(chars(a%exact_text_of(id))))

        case (NK_BIG_REAL)
            rc = basic_parse(h, cstr(chars(a%real_text_of(id))))

        case (NK_REAL)
            rc = real_double_set_d(h, a%real_of(id))

        case (NK_SYM)
            rc = symbol_set(h, cstr(chars(a%name_of(id))))

        case (NK_CONST)
            name = chars(a%name_of(id))
            select case (name)
            case ("pi"); call basic_const_pi(h)
            case ("e");  call basic_const_E(h)
            case ("i");  call basic_const_I(h)
            case default
                rc = symbol_set(h, cstr(name))
            end select

        case (NK_ADD)
            ! Fold left. SymEngine's add is binary at the C ABI even though its
            ! internal representation is n-ary.
            call basic_free_heap(h)
            h = to_symengine(a, a%arg_of(id, 1))
            do k = 2, a%nargs_of(id)
                rhs = to_symengine(a, a%arg_of(id, k))
                lhs = h
                h = basic_new_heap()
                rc = basic_add(h, lhs, rhs)
                call basic_free_heap(lhs)
                call basic_free_heap(rhs)
            end do

        case (NK_MUL)
            call basic_free_heap(h)
            h = to_symengine(a, a%arg_of(id, 1))
            do k = 2, a%nargs_of(id)
                rhs = to_symengine(a, a%arg_of(id, k))
                lhs = h
                h = basic_new_heap()
                rc = basic_mul(h, lhs, rhs)
                call basic_free_heap(lhs)
                call basic_free_heap(rhs)
            end do

        case (NK_POW)
            lhs = to_symengine(a, a%arg_of(id, 1))
            rhs = to_symengine(a, a%arg_of(id, 2))
            rc = basic_pow(h, lhs, rhs)
            call basic_free_heap(lhs)
            call basic_free_heap(rhs)

        case (NK_FUNC)
            call apply_function(a, id, h)

        case default
            rc = integer_set_si(h, 0_c_long)
        end select
    end function to_symengine

    !> Dispatch a named function to its SymEngine entry point.
    recursive subroutine apply_function(a, id, h)
        type(arena_t), intent(in)    :: a
        integer,       intent(in)    :: id
        type(c_ptr),   intent(inout) :: h
        type(c_ptr) :: x, y
        integer :: rc
        character(:), allocatable :: name

        name = chars(a%name_of(id))
        x = to_symengine(a, a%arg_of(id, 1))

        if (name == "atan2") then
            y = to_symengine(a, a%arg_of(id, 2))
            rc = basic_atan2(h, x, y)
            call basic_free_heap(y)
            call basic_free_heap(x)
            return
        end if

        select case (name)
        case ("sin");   rc = basic_sin(h, x)
        case ("cos");   rc = basic_cos(h, x)
        case ("tan");   rc = basic_tan(h, x)
        case ("asin");  rc = basic_asin(h, x)
        case ("acos");  rc = basic_acos(h, x)
        case ("atan");  rc = basic_atan(h, x)
        case ("sinh");  rc = basic_sinh(h, x)
        case ("cosh");  rc = basic_cosh(h, x)
        case ("tanh");  rc = basic_tanh(h, x)
        case ("asinh"); rc = basic_asinh(h, x)
        case ("acosh"); rc = basic_acosh(h, x)
        case ("atanh"); rc = basic_atanh(h, x)
        case ("exp");   rc = basic_exp(h, x)
        case ("log");   rc = basic_log(h, x)
        case ("sqrt");  rc = basic_sqrt(h, x)
        case ("abs");   rc = basic_abs(h, x)
        case ("erf");   rc = basic_erf(h, x)
        case ("erfc");  rc = basic_erfc(h, x)
        case ("gamma"); rc = basic_gamma(h, x)
        case ("loggamma"); rc = basic_loggamma(h, x)
        case ("sign");  rc = basic_sign(h, x)
        case ("floor"); rc = basic_floor(h, x)
        case ("ceiling"); rc = basic_ceiling(h, x)
        case default
            ! An unknown head becomes an *applied* function symbol, keeping its
            ! arguments. Collapsing it to a bare symbol instead would make every
            ! application of the same head identical, so f(x) - f(y) would
            ! decide to zero -- a wrong ZERO, the one failure this whole design
            ! exists to prevent.
            call apply_unknown(a, id, name, h)
        end select

        call basic_free_heap(x)
    end subroutine apply_function

    !> Build an opaque applied function f(a1, ..., an), preserving the head name
    !> and every argument.
    recursive subroutine apply_unknown(a, id, name, h)
        type(arena_t), intent(in)    :: a
        integer,       intent(in)    :: id
        character(*),  intent(in)    :: name
        type(c_ptr),   intent(inout) :: h
        type(c_ptr) :: vec, arg
        integer :: k, rc

        vec = vecbasic_new()
        do k = 1, a%nargs_of(id)
            arg = to_symengine(a, a%arg_of(id, k))
            rc = vecbasic_push_back(vec, arg)
            call basic_free_heap(arg)
        end do
        rc = function_symbol_set(h, cstr(name), vec)
        call vecbasic_free(vec)
    end subroutine apply_unknown

    !> NUL-terminate for the C ABI.
    pure function cstr(s) result(c)
        character(*), intent(in) :: s
        character(len=len(s) + 1, kind=c_char) :: c
        c = s//c_null_char
    end function cstr

    ! ------------------------------------------------- SymEngine -> fortsym --

    !> Render a SymEngine handle and parse it back into the arena.
    function from_symengine(a, h, ok) result(e)
        type(arena_t), target, intent(inout) :: a
        type(c_ptr),           intent(in)    :: h
        logical,               intent(out)   :: ok
        type(expr_t)                         :: e
        character(:), allocatable :: text, message

        text = render(h)
        if (len(text) == 0) then
            ok = .false.
            return
        end if
        e = parse_expr_in(a, text, dialect(DIA_SYMENGINE), ok, message)
    end function from_symengine

    !> SymEngine's own printed form, fetched through the shim's render/fetch
    !> pair so no malloc'd pointer has to be freed on a Fortran error path.
    function render(h) result(text)
        type(c_ptr), intent(in)   :: h
        character(:), allocatable :: text
        integer(c_size_t) :: n, got
        character(kind=c_char), allocatable :: buf(:)
        integer :: k

        n = fsym_str_render(h, FSYM_STR_DEFAULT)
        if (n == 0_c_size_t) then
            text = ""
            return
        end if

        allocate (buf(n))
        got = fsym_str_fetch(buf, n)

        allocate (character(len=int(got)) :: text)
        do k = 1, int(got)
            text(k:k) = buf(k)
        end do
    end function render

    !> Evaluate a closed expression at a bounded decimal precision through
    !> SymEngine's MPFR path and return its decimal spelling. The caller keeps
    !> the spelling as a precision-bearing arena leaf; converting it to real64
    !> here would defeat the requested-precision contract.
    function symengine_evalf_text(e, digits, ok, why) result(text)
        type(expr_t),              intent(in)  :: e
        integer,                   intent(in)  :: digits
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        character(:), allocatable              :: text
        type(c_ptr) :: h, out
        integer(c_long) :: bits
        integer(c_int) :: rc
        logical :: good

        text = ""
        ok = .false.
        why = ""
        if (digits < 1 .or. digits > MAX_EVALF_DIGITS) then
            why = "requested precision exceeds the MPFR digit bound"
            return
        end if
        if (e%node_count() > MAX_EVALF_NODES) then
            why = "requested-precision expression exceeds the evaluator node bound"
            return
        end if
        if (fsym_have_mpfr() == 0_c_int) then
            why = "SymEngine was built without MPFR requested-precision support"
            return
        end if

        bits = int(real(digits + 8, dp)*3.321928094887362_dp, c_long)
        h = to_symengine(e%a, e%id)
        out = basic_new_heap()
        rc = basic_evalf(out, h, bits, 1_c_int)
        if (rc == SYMENGINE_NO_EXCEPTION) then
            text = render(out)
            good = decimal_literal(text)
            if (good) then
                ok = .true.
            else
                text = ""
                why = "SymEngine returned a non-real or non-decimal result"
            end if
        else
            why = "SymEngine MPFR evaluation failed"
        end if
        call basic_free_heap(out)
        call basic_free_heap(h)
    end function symengine_evalf_text

    !> Lexical guard for the text retained as NK_BIG_REAL. In particular this
    !> rejects symbolic, complex, NaN, and infinity results before they reach
    !> the Wolfram printer.
    pure function decimal_literal(text) result(ok)
        character(*), intent(in) :: text
        logical                   :: ok
        integer :: n, p, digits_before, digits_after, exponent_digits
        character :: c

        ok = .false.
        n = len_trim(text)
        if (n == 0) return
        p = 1
        if (text(p:p) == "+" .or. text(p:p) == "-") then
            p = p + 1
            if (p > n) return
        end if

        digits_before = 0
        do while (p <= n)
            c = text(p:p)
            if (c < "0" .or. c > "9") exit
            digits_before = digits_before + 1
            p = p + 1
        end do

        digits_after = 0
        if (p <= n) then
            if (text(p:p) == ".") then
                p = p + 1
                do while (p <= n)
                    c = text(p:p)
                    if (c < "0" .or. c > "9") exit
                    digits_after = digits_after + 1
                    p = p + 1
                end do
            end if
        end if
        if (digits_before + digits_after == 0) return

        exponent_digits = 0
        if (p <= n) then
            if (text(p:p) == "e" .or. text(p:p) == "E") then
                p = p + 1
                if (p > n) return
                if (text(p:p) == "+" .or. text(p:p) == "-") then
                    p = p + 1
                    if (p > n) return
                end if
                do while (p <= n)
                    c = text(p:p)
                    if (c < "0" .or. c > "9") exit
                    exponent_digits = exponent_digits + 1
                    p = p + 1
                end do
                if (exponent_digits == 0) return
            end if
        end if
        ok = p > n
    end function decimal_literal

    ! ------------------------------------------------------------ operations --

    function se_zero_test(self, e) result(r)
        class(symengine_engine_t), intent(inout) :: self
        type(expr_t),              intent(in)    :: e
        type(engine_result_t)                    :: r
        type(c_ptr) :: h
        real(dp) :: t0
        integer(c_int) :: v

        t0 = wall_seconds()
        h = to_symengine(e%a, e%id)
        v = fsym_zero_test(h, 0_c_long)
        call basic_free_heap(h)
        r%seconds = wall_seconds() - t0

        ! The shim's verdict encoding matches fortsym's by construction; the
        ! mapping is explicit so a change on either side is a compile-time edit
        ! rather than a silent misreading.
        select case (v)
        case (FSYM_ZERO_TRUE);  r%verdict = VERDICT_TRUE
        case (FSYM_ZERO_FALSE); r%verdict = VERDICT_FALSE
        case default;           r%verdict = VERDICT_UNKNOWN
        end select

        r%ok = .true.
        r%value = e
    end function se_zero_test

    function se_simplify(self, e) result(r)
        class(symengine_engine_t), intent(inout) :: self
        type(expr_t),              intent(in)    :: e
        type(engine_result_t)                    :: r
        type(c_ptr) :: h, out
        real(dp) :: t0
        integer(c_int) :: rc
        logical :: good

        t0 = wall_seconds()
        h = to_symengine(e%a, e%id)
        out = basic_new_heap()
        rc = fsym_simplify(out, h)

        if (rc == SYMENGINE_NO_EXCEPTION) then
            r%value = from_symengine(self%home, out, good)
            r%ok = good
            if (.not. good) r%message = str("symengine: could not read result back")
        else
            r%ok = .false.
            r%value = e
            r%message = str("symengine: simplify failed")
        end if

        call basic_free_heap(out)
        call basic_free_heap(h)
        r%seconds = wall_seconds() - t0
    end function se_simplify

    function se_diff(self, e, v) result(r)
        class(symengine_engine_t), intent(inout) :: self
        type(expr_t),              intent(in)    :: e, v
        type(engine_result_t)                    :: r
        type(c_ptr) :: h, hv, out
        real(dp) :: t0
        integer(c_int) :: rc
        logical :: good

        t0 = wall_seconds()
        h = to_symengine(e%a, e%id)
        hv = to_symengine(v%a, v%id)
        out = basic_new_heap()
        rc = basic_diff(out, h, hv)

        if (rc == SYMENGINE_NO_EXCEPTION) then
            r%value = from_symengine(self%home, out, good)
            r%ok = good
            if (.not. good) r%message = str("symengine: could not read result back")
        else
            r%ok = .false.
            r%value = e
            r%message = str("symengine: diff failed")
        end if

        call basic_free_heap(out)
        call basic_free_heap(hv)
        call basic_free_heap(h)
        r%seconds = wall_seconds() - t0
    end function se_diff

    function se_expand(self, e) result(r)
        class(symengine_engine_t), intent(inout) :: self
        type(expr_t),              intent(in)    :: e
        type(engine_result_t)                    :: r
        type(c_ptr) :: h, out
        real(dp) :: t0
        integer(c_int) :: rc
        logical :: good

        t0 = wall_seconds()
        h = to_symengine(e%a, e%id)
        out = basic_new_heap()
        rc = basic_expand(out, h)

        if (rc == SYMENGINE_NO_EXCEPTION) then
            r%value = from_symengine(self%home, out, good)
            r%ok = good
        else
            r%ok = .false.
            r%value = e
            r%message = str("symengine: expand failed")
        end if

        call basic_free_heap(out)
        call basic_free_heap(h)
        r%seconds = wall_seconds() - t0
    end function se_expand

end module fortsym_engine_symengine
