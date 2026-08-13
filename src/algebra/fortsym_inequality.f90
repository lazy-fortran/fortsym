module fortsym_inequality
    ! Exact affine inequality reduction for the symbolic equation owner.
    !
    ! This module deliberately owns the decision rather than teaching the
    ! Python adapter how to inspect polynomial coefficients.  The supported
    ! fragment is one relation in one symbol whose residual has degree at most
    ! one over exact compact rationals.  The result is an And of two native
    ! relational bounds, so it remains an ordinary expression and does not
    ! introduce a second interval representation.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_arena, only: arena_t, NK_FUNC, NK_SYM, NK_INT, NK_RAT
    use fortsym_expr, only: expr_t, is_valid, operator(-), operator(/)
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_poly, only: poly_exponent, poly_coefficient
    use fortsym_relation, only: less, less_equal
    use fortsym_string, only: chars
    implicit none
    private

    public :: solve_univariate_inequality

contains

    subroutine solve_univariate_inequality(a, expression, variable, result, ok, why)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: expression, variable
        type(expr_t), intent(out) :: result
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: residual, degree, constant, slope, root
        type(engine_result_t) :: simplified
        type(native_engine_t) :: engine
        character(:), allocatable :: relation_name, coefficient_why
        integer :: degree_value, slope_sign, constant_sign
        logical :: coefficient_ok

        result = expression
        ok = .false.
        why = ""
        if (.not. is_valid(expression) .or. .not. is_valid(variable)) then
            why = "inequality: invalid expression"
            return
        end if
        if (.not. associated(expression%a, a) .or. &
                .not. associated(variable%a, a)) then
            why = "inequality: expressions belong to different arenas"
            return
        end if
        if (variable%kind() /= NK_SYM) then
            why = "inequality variable must be a symbol"
            return
        end if
        if (expression%kind() /= NK_FUNC .or. expression%nargs() /= 2) then
            why = "inequality requires one relational expression"
            return
        end if
        relation_name = chars(expression%name())
        select case (relation_name)
        case ("Greater", "GreaterEqual", "Less", "LessEqual")
        case default
            why = "inequality relation must be <, <=, >, or >="
            return
        end select

        residual = expression%arg(1) - expression%arg(2)
        call poly_exponent(a, residual, variable, degree, coefficient_ok, &
            coefficient_why)
        if (.not. coefficient_ok) then
            why = "inequality: "//coefficient_why
            return
        end if
        if (.not. exact_integer(a, degree, degree_value)) then
            why = "inequality: polynomial degree is not an exact integer"
            return
        end if
        if (degree_value > 1) then
            why = "inequality: only affine residuals are supported"
            return
        end if

        call poly_coefficient(a, residual, variable, 0, constant, &
            coefficient_ok, coefficient_why)
        if (.not. coefficient_ok) then
            why = "inequality: "//coefficient_why
            return
        end if
        call exact_sign(a, constant, constant_sign, coefficient_ok)
        if (.not. coefficient_ok) then
            why = "inequality: coefficients must be exact compact rationals"
            return
        end if

        if (degree_value <= 0) then
            call constant_relation(a, relation_name, constant_sign, result, ok, why)
            return
        end if

        call poly_coefficient(a, residual, variable, 1, slope, coefficient_ok, &
            coefficient_why)
        if (.not. coefficient_ok) then
            why = "inequality: "//coefficient_why
            return
        end if
        call exact_sign(a, slope, slope_sign, coefficient_ok)
        if (.not. coefficient_ok .or. slope_sign == 0) then
            why = "inequality: affine slope must be a nonzero exact rational"
            return
        end if
        root = (-constant)/slope
        engine = make_native_engine(a)
        simplified = engine%simplify(root)
        if (simplified%ok) root = simplified%value
        call affine_result(a, relation_name, slope_sign, root, variable, result)
        ok = .true.
    end subroutine solve_univariate_inequality

    subroutine constant_relation(a, relation_name, sign, result, ok, why)
        type(arena_t), target, intent(inout) :: a
        character(*), intent(in) :: relation_name
        integer, intent(in) :: sign
        type(expr_t), intent(out) :: result
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        logical :: truth

        select case (relation_name)
        case ("Greater")
            truth = sign > 0
        case ("GreaterEqual")
            truth = sign >= 0
        case ("Less")
            truth = sign < 0
        case ("LessEqual")
            truth = sign <= 0
        case default
            result = expression_false(a)
            ok = .false.
            why = "inequality: unsupported relation"
            return
        end select
        if (truth) then
            result = expression_true(a)
        else
            result = expression_false(a)
        end if
        ok = .true.
        why = ""
    end subroutine constant_relation

    subroutine affine_result(a, relation_name, slope_sign, root, variable, result)
        type(arena_t), target, intent(inout) :: a
        character(*), intent(in) :: relation_name
        integer, intent(in) :: slope_sign
        type(expr_t), intent(in) :: root, variable
        type(expr_t), intent(out) :: result
        type(expr_t) :: lower, upper, infinity, negative_infinity

        infinity = expression_constant(a, "oo")
        negative_infinity = -infinity
        if (slope_sign > 0) then
            select case (relation_name)
            case ("Greater")
                lower = less(root, variable)
                upper = less(variable, infinity)
            case ("GreaterEqual")
                lower = less_equal(root, variable)
                upper = less(variable, infinity)
            case ("Less")
                lower = less(negative_infinity, variable)
                upper = less(variable, root)
            case default ! LessEqual
                lower = less(negative_infinity, variable)
                upper = less_equal(variable, root)
            end select
        else
            select case (relation_name)
            case ("Greater")
                lower = less(negative_infinity, variable)
                upper = less(variable, root)
            case ("GreaterEqual")
                lower = less(negative_infinity, variable)
                upper = less_equal(variable, root)
            case ("Less")
                lower = less(root, variable)
                upper = less(variable, infinity)
            case default ! LessEqual
                lower = less_equal(root, variable)
                upper = less(variable, infinity)
            end select
        end if
        result = binary_function(a, "And", lower, upper)
    end subroutine affine_result

    logical function exact_integer(a, expression, value) result(ok)
        type(arena_t), intent(in) :: a
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: value
        integer(int64) :: raw

        ok = .false.
        value = 0
        if (expression%kind() /= NK_INT) return
        raw = a%num_of(expression%id)
        if (raw > int(huge(0), int64) .or. raw < -int(huge(0), int64)-1_int64) return
        value = int(raw)
        ok = .true.
    end function exact_integer

    subroutine exact_sign(a, expression, sign, ok)
        type(arena_t), intent(in) :: a
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: sign
        logical, intent(out) :: ok
        integer(int64) :: numerator

        sign = 0
        ok = .false.
        select case (expression%kind())
        case (NK_INT, NK_RAT)
            numerator = a%num_of(expression%id)
            sign = merge(1, -1, numerator > 0)
            if (numerator == 0) sign = 0
            ok = .true.
        case default
            return
        end select
    end subroutine exact_sign

    function expression_constant(a, name) result(value)
        type(arena_t), target, intent(inout) :: a
        character(*), intent(in) :: name
        type(expr_t) :: value

        value%a => a
        value%id = a%const(name)
        value%generation = a%generation_value()
    end function expression_constant

    function expression_true(a) result(value)
        type(arena_t), target, intent(inout) :: a
        type(expr_t) :: value
        value = expression_constant(a, "True")
    end function expression_true

    function expression_false(a) result(value)
        type(arena_t), target, intent(inout) :: a
        type(expr_t) :: value
        value = expression_constant(a, "False")
    end function expression_false

    function binary_function(a, name, left, right) result(value)
        type(arena_t), target, intent(inout) :: a
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: left, right
        type(expr_t) :: value
        integer :: ids(2)

        ids(1) = left%id
        ids(2) = right%id
        value%a => a
        value%id = a%func(name, ids)
        value%generation = a%generation_value()
    end function binary_function

end module fortsym_inequality
