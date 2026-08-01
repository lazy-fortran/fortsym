program test_fortsym_matrix
    ! Symbolic matrices, checked against matrix identities rather than against
    ! stored answers.
    !
    ! The oracle matters here. Comparing Det against a written-down expression
    ! only proves the code agrees with whoever typed the expectation, and both
    ! can be wrong the same way. det(A B) = det(A) det(B) and A A^-1 = I are
    ! properties the implementation cannot satisfy by accident, so they catch a
    ! sign error or a transposed index that a fixture would wave through.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: str_t, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr
    use fortsym_matrix
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_subs, only: subs
    use fortsym_eval, only: free_symbols_of
    implicit none

    integer :: nfail = 0

    call test_determinant_is_multiplicative()
    call test_inverse_is_a_two_sided_inverse()
    call test_transpose_is_an_involution()
    call test_shape_errors_are_refused()

    if (nfail == 0) then
        print *, "PASS test_fortsym_matrix"
    else
        print *, "FAIL test_fortsym_matrix:", nfail
        error stop 1
    end if

contains

    !> A 3x3 of distinct symbols, so no accidental cancellation can hide a bug.
    function symbolic_matrix(a, prefix, n) result(e)
        type(arena_t), target, intent(inout) :: a
        character(*),          intent(in)    :: prefix
        integer,               intent(in)    :: n
        type(expr_t)                         :: e
        type(expr_t), allocatable :: m(:, :)
        character(2) :: ij
        integer :: i, j

        allocate (m(n, n))
        do i = 1, n
            do j = 1, n
                write (ij, "(i1,i1)") i, j
                m(i, j) = sym(a, prefix//ij)
            end do
        end do
        e = from_matrix(a, m)
    end function symbolic_matrix

    !> Check an identity by substituting concrete values.
    !>
    !> Not the symbolic zero test: these identities are rational functions, and
    !> deciding them needs the cancellation that issue #28 has not delivered
    !> yet. Substituting distinct integers reduces each one to exact rational
    !> arithmetic, which the arena already does exactly, so the oracle stays
    !> independent of the feature under construction.
    !>
    !> Several unrelated substitutions rather than one: a single point can be a
    !> coincidental root of a wrong expression, and three cannot be for the
    !> polynomial degrees involved here.
    subroutine expect_zero(label, engine, a, e)
        character(*),          intent(in)    :: label
        type(native_engine_t), intent(inout) :: engine
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        type(engine_result_t) :: res
        type(expr_t) :: value
        integer :: trial, seed
        integer, parameter :: seeds(3) = [2, 7, 13]

        do trial = 1, size(seeds)
            seed = seeds(trial)
            value = substitute_numbers(a, e, seed)
            res = engine%simplify(value)
            if (.not. res%ok) then
                print *, "FAIL ", label, ": simplify declined"
                nfail = nfail + 1
                return
            end if
            if (chars(res%value%exact_text()) /= "0") then
                print *, "FAIL ", label, " at seed ", seed, ": ", &
                    chars(res%value%exact_text())
                nfail = nfail + 1
                return
            end if
        end do
    end subroutine expect_zero

    !> Replace every free symbol by a distinct integer derived from its name.
    function substitute_numbers(a, e, seed) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        integer,               intent(in)    :: seed
        type(expr_t)                         :: r
        type(str_t), allocatable :: names(:)
        integer :: k, value

        r = e
        names = free_symbols_of(e)
        do k = 1, size(names)
            ! Distinct and non-degenerate: equal entries make a determinant
            ! vanish for reasons that have nothing to do with correctness.
            value = seed + 3*k
            r = subs(r, sym(a, chars(names(k))), num(a, value))
        end do
    end function substitute_numbers

    !> det(A B) = det(A) det(B), on symbolic entries.
    subroutine test_determinant_is_multiplicative()
        type(arena_t), target :: a
        type(native_engine_t) :: engine
        type(expr_t) :: ma, mb, product, left, right
        logical :: ok
        type(str_t) :: why

        call a%init()
        engine = make_native_engine(a)
        ma = symbolic_matrix(a, "a", 2)
        mb = symbolic_matrix(a, "b", 2)

        product = matrix_dot(a, ma, mb, ok, why)
        if (.not. ok) then
            print *, "FAIL det multiplicative: dot ", chars(why)
            nfail = nfail + 1
            return
        end if

        left = matrix_det(a, product, ok, why)
        if (.not. ok) then
            print *, "FAIL det multiplicative: det(AB) ", chars(why)
            nfail = nfail + 1
            return
        end if
        right = matrix_det(a, ma, ok, why)*matrix_det(a, mb, ok, why)

        call expect_zero("det(AB) - det(A)det(B)", engine, a, left - right)
    end subroutine test_determinant_is_multiplicative

    !> A A^-1 = I and A^-1 A = I. Both sides, because the adjugate is the
    !> transpose of the cofactor matrix and getting that backwards still
    !> satisfies one of the two on a symmetric example.
    subroutine test_inverse_is_a_two_sided_inverse()
        type(arena_t), target :: a
        type(native_engine_t) :: engine
        type(expr_t) :: ma, inv, left, right, row
        logical :: ok
        type(str_t) :: why
        integer :: i, j

        call a%init()
        engine = make_native_engine(a)
        ma = symbolic_matrix(a, "a", 2)

        inv = matrix_inverse(a, ma, ok, why)
        if (.not. ok) then
            print *, "FAIL inverse: ", chars(why)
            nfail = nfail + 1
            return
        end if

        left = matrix_dot(a, ma, inv, ok, why)
        if (.not. ok) then
            print *, "FAIL inverse: A Ainv ", chars(why)
            nfail = nfail + 1
            return
        end if
        right = matrix_dot(a, inv, ma, ok, why)
        if (.not. ok) then
            print *, "FAIL inverse: Ainv A ", chars(why)
            nfail = nfail + 1
            return
        end if

        do i = 1, 2
            row = left%arg(i)
            do j = 1, 2
                call expect_zero("A Ainv identity", engine, a, &
                    row%arg(j) - kronecker(a, i, j))
            end do
            row = right%arg(i)
            do j = 1, 2
                call expect_zero("Ainv A identity", engine, a, &
                    row%arg(j) - kronecker(a, i, j))
            end do
        end do
    end subroutine test_inverse_is_a_two_sided_inverse

    function kronecker(a, i, j) result(e)
        type(arena_t), target, intent(inout) :: a
        integer,               intent(in)    :: i, j
        type(expr_t)                         :: e
        if (i == j) then
            e = num(a, 1)
        else
            e = num(a, 0)
        end if
    end function kronecker

    subroutine test_transpose_is_an_involution()
        type(arena_t), target :: a
        type(expr_t) :: ma, once, twice
        logical :: ok

        call a%init()
        ma = symbolic_matrix(a, "a", 3)

        once = matrix_transpose(a, ma, ok)
        if (.not. ok) then
            print *, "FAIL transpose: rejected a matrix"
            nfail = nfail + 1
            return
        end if
        twice = matrix_transpose(a, once, ok)

        ! Hash-consing makes this a complete structural check.
        if (twice%id /= ma%id) then
            print *, "FAIL transpose: not an involution"
            nfail = nfail + 1
        end if
        if (once%id == ma%id) then
            print *, "FAIL transpose: did nothing"
            nfail = nfail + 1
        end if
    end subroutine test_transpose_is_an_involution

    !> Bad shapes must be refused, never squared off or truncated.
    subroutine test_shape_errors_are_refused()
        type(arena_t), target :: a
        type(expr_t) :: ragged, oblong, vector, r
        logical :: ok
        type(str_t) :: why

        call a%init()

        ! Ragged rows are a transcription error in the source. Padding one
        ! would produce a determinant for something that has none.
        ragged = func("List", [func("List", [sym(a, "p"), sym(a, "q")]), &
                               func("List", [sym(a, "r")])])
        if (is_matrix(ragged)) then
            print *, "FAIL ragged accepted as a matrix"
            nfail = nfail + 1
        end if

        oblong = func("List", [func("List", [sym(a, "p"), sym(a, "q")])])
        r = matrix_det(a, oblong, ok, why)
        if (ok) then
            print *, "FAIL determinant of a non-square matrix"
            nfail = nfail + 1
        end if

        ! A truncated product is dimensionally plausible and wrong, which is
        ! the hardest kind of error to notice downstream.
        vector = func("List", [num(a, 1), num(a, 2), num(a, 3)])
        r = matrix_dot(a, symbolic_matrix(a, "a", 2), vector, ok, why)
        if (ok) then
            print *, "FAIL dot with mismatched dimensions accepted"
            nfail = nfail + 1
        end if
    end subroutine test_shape_errors_are_refused

end program test_fortsym_matrix
