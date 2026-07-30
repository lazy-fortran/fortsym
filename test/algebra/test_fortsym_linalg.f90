program test_fortsym_linalg
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_arena, only: arena_t
    use fortsym_engine_native, only: make_native_engine, native_engine_t
    use fortsym_expr, only: &
        expr_t, num, operator(/=), operator(==), rat, sym
    use fortsym_linalg, only: &
        exact_linear_system_result_t, solve_exact_linear_system
    implicit none

    type(arena_t), target :: arena, other_arena
    type(native_engine_t) :: engine
    integer :: failed, passed

    failed = 0
    passed = 0
    call arena%init()
    call other_arena%init()
    engine = make_native_engine(arena)

    call test_integer_multiple_rhs()
    call test_rational_system()
    call test_row_permutation()
    call test_polynomial_moment_change_of_basis()
    call test_invalid_systems()

    print '(a,i0,a,i0)', "linalg: ", passed, " passed, ", failed, " failed"
    if (failed > 0) error stop 1

contains

    subroutine test_integer_multiple_rhs()
        type(exact_linear_system_result_t) :: result
        type(expr_t) :: matrix(2, 2), right_hand_sides(2, 2)

        matrix(1, :) = [num(arena, 2), num(arena, 1)]
        matrix(2, :) = [num(arena, 1), num(arena, -1)]
        right_hand_sides(:, 1) = [num(arena, 5), num(arena, 1)]
        right_hand_sides(:, 2) = [num(arena, 1), num(arena, 2)]

        result = solve_exact_linear_system(engine, matrix, right_hand_sides)
        call check(result%ok, "integer system with two right-hand sides solves")
        if (.not. result%ok) return
        call check( &
            result%values(1, 1) == num(arena, 2) .and. &
            result%values(2, 1) == num(arena, 1), &
            "first integer solution is exact")
        call check( &
            result%values(1, 2) == num(arena, 1) .and. &
            result%values(2, 2) == num(arena, -1), &
            "second integer solution is exact")
    end subroutine test_integer_multiple_rhs

    subroutine test_rational_system()
        type(exact_linear_system_result_t) :: result
        type(expr_t) :: matrix(2, 2), right_hand_side(2, 1)

        matrix = num(arena, 0)
        matrix(1, 1) = rat(arena, 1_int64, 2_int64)
        matrix(2, 2) = rat(arena, 2_int64, 3_int64)
        right_hand_side(1, 1) = rat(arena, 1_int64, 3_int64)
        right_hand_side(2, 1) = rat(arena, 5_int64, 6_int64)

        result = solve_exact_linear_system( &
            engine, matrix, right_hand_side)
        call check(result%ok, "rational system solves")
        if (.not. result%ok) return
        call check(result%values(1, 1) == &
            rat(arena, 2_int64, 3_int64), &
            "first rational solution is exact")
        call check(result%values(2, 1) == &
            rat(arena, 5_int64, 4_int64), &
            "second rational solution is exact")
    end subroutine test_rational_system

    subroutine test_row_permutation()
        type(exact_linear_system_result_t) :: original, permuted
        type(expr_t) :: matrix(3, 3), permuted_matrix(3, 3)
        type(expr_t) :: right_hand_side(3, 1), permuted_rhs(3, 1)

        matrix(1, :) = [num(arena, 1), num(arena, 2), num(arena, 0)]
        matrix(2, :) = [num(arena, 0), num(arena, 1), num(arena, 3)]
        matrix(3, :) = [num(arena, 2), num(arena, 0), num(arena, 1)]
        right_hand_side(:, 1) = [ &
            num(arena, 5), num(arena, 11), num(arena, 5)]
        permuted_matrix(1, :) = matrix(3, :)
        permuted_matrix(2, :) = matrix(1, :)
        permuted_matrix(3, :) = matrix(2, :)
        permuted_rhs(1, :) = right_hand_side(3, :)
        permuted_rhs(2, :) = right_hand_side(1, :)
        permuted_rhs(3, :) = right_hand_side(2, :)

        original = solve_exact_linear_system( &
            engine, matrix, right_hand_side)
        permuted = solve_exact_linear_system( &
            engine, permuted_matrix, permuted_rhs)
        call check(original%ok .and. permuted%ok, &
            "row-permuted systems both solve")
        if (.not. original%ok .or. .not. permuted%ok) return
        call check(expression_matrices_equal( &
            original%values, permuted%values), &
            "row permutation leaves the exact solution unchanged")
        call check( &
            original%values(1, 1) == num(arena, 1) .and. &
            original%values(2, 1) == num(arena, 2) .and. &
            original%values(3, 1) == num(arena, 3), &
            "three-by-three solution matches its independent oracle")
    end subroutine test_row_permutation

    subroutine test_polynomial_moment_change_of_basis()
        integer, parameter :: monomial_count = 6
        integer, parameter :: x_degree(monomial_count) = [0, 1, 0, 2, 1, 0]
        integer, parameter :: y_degree(monomial_count) = [0, 0, 1, 0, 1, 2]
        integer, parameter :: system_size = 2 * monomial_count
        type(exact_linear_system_result_t) :: result
        type(expr_t) :: matrix(system_size, system_size)
        type(expr_t) :: right_hand_sides(system_size, system_size)
        integer :: basis_component, basis_monomial, column
        integer :: moment_component, moment_monomial, row
        integer :: target, target_component, target_monomial
        logical :: expected

        matrix = num(arena, 0)
        right_hand_sides = num(arena, 0)
        do basis_component = 1, 2
            do basis_monomial = 1, monomial_count
                column = local_index(basis_component, basis_monomial)
                do moment_component = 1, 2
                    do moment_monomial = 1, monomial_count
                        row = local_index(moment_component, moment_monomial)
                        if (moment_component == basis_component) then
                            matrix(row, column) = triangle_monomial_integral( &
                                x_degree(moment_monomial) + &
                                x_degree(basis_monomial), &
                                y_degree(moment_monomial) + &
                                y_degree(basis_monomial))
                        end if
                        if (moment_component == 3 - basis_component) then
                            right_hand_sides(row, column) = &
                                triangle_monomial_integral( &
                                x_degree(moment_monomial) + &
                                y_degree(basis_monomial), &
                                y_degree(moment_monomial) + &
                                x_degree(basis_monomial))
                        end if
                    end do
                end do
            end do
        end do

        result = solve_exact_linear_system( &
            engine, matrix, right_hand_sides)
        call check(result%ok, &
            "twelve-by-twelve polynomial moment transform solves exactly")
        if (.not. result%ok) return
        do column = 1, system_size
            basis_component = (column - 1) / monomial_count + 1
            basis_monomial = mod(column - 1, monomial_count) + 1
            target_component = 3 - basis_component
            target_monomial = find_monomial( &
                y_degree(basis_monomial), x_degree(basis_monomial), &
                x_degree, y_degree)
            target = local_index(target_component, target_monomial)
            do row = 1, system_size
                expected = result%values(row, column) == num(arena, 0)
                if (row == target) then
                    expected = result%values(row, column) == num(arena, 1)
                end if
                call check(expected, &
                    "polynomial moment transform matches coordinate swap")
            end do
        end do
    end subroutine test_polynomial_moment_change_of_basis

    subroutine test_invalid_systems()
        type(exact_linear_system_result_t) :: result
        type(expr_t) :: mixed_matrix(1, 1), one_rhs(1, 1)
        type(expr_t) :: rectangular(2, 3), rectangular_rhs(2, 1)
        type(expr_t) :: singular(2, 2), singular_rhs(2, 1)
        type(expr_t) :: symbolic(1, 1)

        singular(1, :) = [num(arena, 1), num(arena, 2)]
        singular(2, :) = [num(arena, 2), num(arena, 4)]
        singular_rhs(:, 1) = [num(arena, 1), num(arena, 2)]
        result = solve_exact_linear_system(engine, singular, singular_rhs)
        call check(.not. result%ok, "singular system is rejected")

        rectangular = num(arena, 1)
        rectangular_rhs = num(arena, 1)
        result = solve_exact_linear_system( &
            engine, rectangular, rectangular_rhs)
        call check(.not. result%ok, "rectangular system is rejected")

        mixed_matrix(1, 1) = num(other_arena, 1)
        one_rhs(1, 1) = num(arena, 1)
        result = solve_exact_linear_system(engine, mixed_matrix, one_rhs)
        call check(.not. result%ok, "mixed-arena system is rejected")

        symbolic(1, 1) = sym(arena, "a")
        result = solve_exact_linear_system(engine, symbolic, one_rhs)
        call check(.not. result%ok, "symbolic pivot is explicitly rejected")
    end subroutine test_invalid_systems

    function triangle_monomial_integral( &
            x_degree, y_degree) result(value)
        integer, intent(in) :: x_degree, y_degree
        type(expr_t) :: value

        value = rat( &
            arena, factorial(x_degree) * factorial(y_degree), &
            factorial(x_degree + y_degree + 2))
    end function triangle_monomial_integral

    pure function factorial(argument) result(value)
        integer, intent(in) :: argument
        integer(int64) :: value

        integer :: factor

        value = 1_int64
        do factor = 2, argument
            value = value * int(factor, int64)
        end do
    end function factorial

    pure function local_index(component, monomial) result(index_)
        integer, intent(in) :: component, monomial
        integer :: index_

        index_ = (component - 1) * 6 + monomial
    end function local_index

    pure function find_monomial( &
            sought_x, sought_y, x_degrees, y_degrees) result(monomial)
        integer, intent(in) :: sought_x, sought_y
        integer, intent(in) :: x_degrees(:), y_degrees(:)
        integer :: monomial

        do monomial = 1, size(x_degrees)
            if (x_degrees(monomial) /= sought_x) cycle
            if (y_degrees(monomial) == sought_y) return
        end do
        monomial = 0
    end function find_monomial

    function expression_matrices_equal(left, right) result(equal)
        type(expr_t), intent(in) :: left(:, :), right(:, :)
        logical :: equal

        integer :: column, row

        equal = .false.
        if (size(left, 1) /= size(right, 1)) return
        if (size(left, 2) /= size(right, 2)) return
        do column = 1, size(left, 2)
            do row = 1, size(left, 1)
                if (left(row, column) /= right(row, column)) return
            end do
        end do
        equal = .true.
    end function expression_matrices_equal

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(*), intent(in) :: label

        if (condition) then
            passed = passed + 1
        else
            failed = failed + 1
            print '(a,a)', "FAIL: ", label
        end if
    end subroutine check

end program test_fortsym_linalg
