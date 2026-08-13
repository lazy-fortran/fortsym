program test_fortsym_matrix
    ! Symbolic matrices, checked against matrix identities rather than against
    ! stored answers.
    !
    ! The oracle matters here. Comparing Det against a written-down expression
    ! only proves the code agrees with whoever typed the expectation, and both
    ! can be wrong the same way. det(A B) = det(A) det(B) and A A^-1 = I are
    ! properties the implementation cannot satisfy by accident, so they catch a
    ! sign error or a transposed index that a fixture would wave through.
    use fortsym_string, only: str_t, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr
    use fortsym_matrix
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_subs, only: subs
    use fortsym_eval, only: collect_free_symbols
    implicit none

    integer :: nfail = 0

    call test_determinant_is_multiplicative()
    call test_inverse_is_a_two_sided_inverse()
    call test_transpose_is_an_involution()
    call test_rref_null_space_and_rank()
    call test_minors()
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
        call collect_free_symbols(e, names)
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

    !> RREF exposes pivot columns; the null-space basis must then annihilate
    !> the original matrix and use one independent free coordinate per vector.
    !> These are structural identities, not copies of the implementation's
    !> intermediate rows, so a sign or pivot-direction error is visible.
    subroutine test_rref_null_space_and_rank()
        type(arena_t), target :: a
        type(expr_t) :: matrix, reduced, nulls, rank_value, product, row, item
        type(expr_t) :: row1(4), row2(4), row3(4), rows(3)
        type(expr_t) :: short_row1(2), short_row2(2), short_rows(2)
        type(native_engine_t) :: engine
        type(engine_result_t) :: simplified
        logical :: ok
        type(str_t) :: why
        integer :: i, j

        call a%init()
        engine = make_native_engine(a)
        row1(1) = num(a, 1)
        row1(2) = num(a, 2)
        row1(3) = num(a, 3)
        row1(4) = num(a, 4)
        row2(1) = num(a, 2)
        row2(2) = num(a, 4)
        row2(3) = num(a, 6)
        row2(4) = num(a, 8)
        row3(1) = num(a, 0)
        row3(2) = num(a, 1)
        row3(3) = num(a, 1)
        row3(4) = num(a, 1)
        rows(1) = func("List", row1)
        rows(2) = func("List", row2)
        rows(3) = func("List", row3)
        matrix = func("List", rows)

        reduced = matrix_row_reduce(a, matrix, ok, why)
        if (.not. ok) then
            print *, "FAIL RREF: ", chars(why)
            nfail = nfail + 1
            return
        end if
        call expect_entry("RREF pivot 1", reduced, 1, 1, "1")
        call expect_entry("RREF cleared above pivot 1", reduced, 1, 2, "0")
        call expect_entry("RREF pivot 2", reduced, 2, 2, "1")
        call expect_entry("RREF dependent row", reduced, 3, 3, "0")

        nulls = matrix_null_space(a, matrix, ok, why)
        if (.not. ok) then
            print *, "FAIL null space: ", chars(why)
            nfail = nfail + 1
            return
        end if
        if (nulls%nargs() /= 2) then
            print *, "FAIL null space: expected two free directions"
            nfail = nfail + 1
            return
        end if
        call expect_vector_entry("null vector 1", nulls%arg(1), 1, "-1")
        call expect_vector_entry("null vector 1", nulls%arg(1), 2, "-1")
        call expect_vector_entry("null vector 1", nulls%arg(1), 3, "1")
        call expect_vector_entry("null vector 2", nulls%arg(2), 1, "-2")
        call expect_vector_entry("null vector 2", nulls%arg(2), 2, "-1")
        call expect_vector_entry("null vector 2", nulls%arg(2), 4, "1")

        do i = 1, nulls%nargs()
            product = matrix_dot(a, matrix, nulls%arg(i), ok, why)
            if (.not. ok) then
                print *, "FAIL null space annihilation: ", chars(why)
                nfail = nfail + 1
                cycle
            end if
            do j = 1, product%nargs()
                item = product%arg(j)
                simplified = engine%simplify(item)
                if (.not. simplified%ok .or. &
                    chars(simplified%value%exact_text()) /= "0") then
                    print *, "FAIL null space annihilation: nonzero product"
                    nfail = nfail + 1
                end if
            end do
        end do

        rank_value = matrix_rank(a, matrix, ok, why)
        if (.not. ok .or. chars(rank_value%exact_text()) /= "2") then
            print *, "FAIL matrix rank: expected 2"
            nfail = nfail + 1
        end if

        ! Full column rank has no free directions and must return the genuine
        ! empty List, not a one-element zero vector.
        short_row1(1) = num(a, 1)
        short_row1(2) = num(a, 0)
        short_row2(1) = num(a, 0)
        short_row2(2) = num(a, 1)
        short_rows(1) = func("List", short_row1)
        short_rows(2) = func("List", short_row2)
        matrix = func("List", short_rows)
        nulls = matrix_null_space(a, matrix, ok, why)
        if (.not. ok .or. nulls%nargs() /= 0) then
            print *, "FAIL full-rank null space: expected empty List"
            nfail = nfail + 1
        end if
    end subroutine test_rref_null_space_and_rank

    !> A 3x4 matrix has four maximal minors. These values are obtained from
    !> the ordinary 3x3 determinant formula independently of matrix_minors;
    !> checking all four also catches a row/column-combination ordering error.
    subroutine test_minors()
        type(arena_t), target :: a
        type(expr_t) :: matrix, minors, row
        type(expr_t) :: row1(4), row2(4), row3(4), rows(3)
        logical :: ok
        type(str_t) :: why

        call a%init()
        row1(1) = num(a, 1)
        row1(2) = num(a, 2)
        row1(3) = num(a, 3)
        row1(4) = num(a, 4)
        row2(1) = num(a, 0)
        row2(2) = num(a, 1)
        row2(3) = num(a, 4)
        row2(4) = num(a, 2)
        row3(1) = num(a, 2)
        row3(2) = num(a, 0)
        row3(3) = num(a, 1)
        row3(4) = num(a, 3)
        rows(1) = func("List", row1)
        rows(2) = func("List", row2)
        rows(3) = func("List", row3)
        matrix = func("List", rows)

        minors = matrix_minors(a, matrix, 3, ok, why)
        if (.not. ok) then
            print *, "FAIL minors: ", chars(why)
            nfail = nfail + 1
            return
        end if
        if (chars(minors%name()) /= "List" .or. minors%nargs() /= 1) then
            print *, "FAIL minors: expected one row-combination result"
            nfail = nfail + 1
            return
        end if
        row = minors%arg(1)
        if (chars(row%name()) /= "List" .or. row%nargs() /= 4) then
            print *, "FAIL minors: expected four column-combination results"
            nfail = nfail + 1
            return
        end if
        call expect_entry("minor 123", minors, 1, 1, "11")
        call expect_entry("minor 124", minors, 1, 2, "3")
        call expect_entry("minor 134", minors, 1, 3, "-10")
        call expect_entry("minor 234", minors, 1, 4, "15")
    end subroutine test_minors

    subroutine expect_entry(label, matrix, row_index, col_index, expected)
        character(*), intent(in) :: label, expected
        type(expr_t), intent(in) :: matrix
        type(expr_t) :: row, item
        integer, intent(in) :: row_index, col_index
        row = matrix%arg(row_index)
        item = row%arg(col_index)
        if (chars(item%exact_text()) /= expected) then
            print *, "FAIL ", label
            nfail = nfail + 1
        end if
    end subroutine expect_entry

    subroutine expect_vector_entry(label, vector, index, expected)
        character(*), intent(in) :: label, expected
        type(expr_t), intent(in) :: vector
        type(expr_t) :: item
        integer, intent(in) :: index
        item = vector%arg(index)
        if (chars(item%exact_text()) /= expected) then
            print *, "FAIL ", label
            nfail = nfail + 1
        end if
    end subroutine expect_vector_entry

    !> Bad shapes must be refused, never squared off or truncated.
    subroutine test_shape_errors_are_refused()
        type(arena_t), target :: a
        type(expr_t) :: ragged, oblong, vector, singular, r
        type(expr_t) :: ragged_row(2), ragged_short(1), ragged_rows(2)
        type(expr_t) :: oblong_row(2), oblong_rows(1), vector_items(3)
        type(expr_t) :: singular_row_one(2), singular_row_two(2)
        type(expr_t) :: singular_rows(2)
        logical :: ok
        type(str_t) :: why

        call a%init()

        ! Ragged rows are a transcription error in the source. Padding one
        ! would produce a determinant for something that has none.
        ragged_row(1) = sym(a, "p")
        ragged_row(2) = sym(a, "q")
        ragged_short(1) = sym(a, "r")
        ragged_rows(1) = func("List", ragged_row)
        ragged_rows(2) = func("List", ragged_short)
        ragged = func("List", ragged_rows)
        if (is_matrix(ragged)) then
            print *, "FAIL ragged accepted as a matrix"
            nfail = nfail + 1
        end if

        oblong_row(1) = sym(a, "p")
        oblong_row(2) = sym(a, "q")
        oblong_rows(1) = func("List", oblong_row)
        oblong = func("List", oblong_rows)
        r = matrix_det(a, oblong, ok, why)
        if (ok) then
            print *, "FAIL determinant of a non-square matrix"
            nfail = nfail + 1
        end if

        singular_row_one(1) = num(a, 1)
        singular_row_one(2) = num(a, 2)
        singular_row_two(1) = num(a, 2)
        singular_row_two(2) = num(a, 4)
        singular_rows(1) = func("List", singular_row_one)
        singular_rows(2) = func("List", singular_row_two)
        singular = func("List", singular_rows)
        r = matrix_inverse(a, singular, ok, why)
        if (ok) then
            print *, "FAIL inverse of a singular matrix accepted"
            nfail = nfail + 1
        end if

        ! A truncated product is dimensionally plausible and wrong, which is
        ! the hardest kind of error to notice downstream.
        vector_items(1) = num(a, 1)
        vector_items(2) = num(a, 2)
        vector_items(3) = num(a, 3)
        vector = func("List", vector_items)
        r = matrix_dot(a, symbolic_matrix(a, "a", 2), vector, ok, why)
        if (ok) then
            print *, "FAIL dot with mismatched dimensions accepted"
            nfail = nfail + 1
        end if
    end subroutine test_shape_errors_are_refused

end program test_fortsym_matrix
