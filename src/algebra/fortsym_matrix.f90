module fortsym_matrix
    ! Symbolic matrices over expr_t, represented as nested List applications.
    !
    ! A matrix is a List of Lists of equal length; a vector is a List of
    ! non-Lists. That is how the Wolfram subset carries them, so working on the
    ! representation directly avoids a conversion layer that would have to be
    ! kept in step with the parser.
    !
    ! Determinant and inverse use fraction-free Gaussian elimination (Bareiss
    ! 1968) rather than cofactor expansion. Cofactor expansion is n! operations
    ! and produces enormous unsimplified sums; Bareiss is O(n^3) and every
    ! intermediate is an exact polynomial in the entries, which is what keeps
    ! the result readable and the arithmetic exact. See doc/provenance.md.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_FUNC
    use fortsym_expr, only: expr_t, num, rat, func, func_in, same_arena, zoo_expr, operator(+), &
        operator(-), operator(*), operator(/), operator(==)
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    implicit none
    private

    public :: is_list, is_matrix, matrix_shape
    public :: matrix_transpose, matrix_add, matrix_negate, matrix_divide, matrix_dot
    public :: matrix_det, matrix_inverse
    public :: matrix_row_reduce, matrix_rref, matrix_null_space, matrix_rank, matrix_minors
    public :: to_matrix, from_matrix

    ! Exact row reduction can grow intermediate expressions quickly. Keep the
    ! operation bounded: refusing a large symbolic matrix is safer than
    ! spending an unbounded amount of time producing a result the caller may
    ! not be able to consume.
    integer, parameter :: MAX_RREF_ROWS = 64
    integer, parameter :: MAX_RREF_COLS = 64
    integer, parameter :: MAX_RREF_ENTRIES = 1024
    integer, parameter :: MAX_MINOR_OUTPUT = 1024

contains

    !> True when the expression is a List application.
    function is_list(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        yes = .false.
        if (e%kind() /= NK_FUNC) return
        yes = chars(e%name()) == "List"
    end function is_list

    !> True when every element is a List of the same length.
    !>
    !> Ragged input is rejected rather than padded: a ragged "matrix" is a
    !> transcription error in the source, and silently squaring it off would
    !> produce a determinant for something that has none.
    function is_matrix(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        type(expr_t) :: row
        integer :: k, width

        yes = .false.
        if (.not. is_list(e)) return
        if (e%nargs() == 0) return

        row = e%arg(1)
        if (.not. is_list(row)) return
        width = row%nargs()
        if (width == 0) return

        do k = 2, e%nargs()
            row = e%arg(k)
            if (.not. is_list(row)) return
            if (row%nargs() /= width) return
        end do
        yes = .true.
    end function is_matrix

    subroutine matrix_shape(e, rows, cols)
        type(expr_t), intent(in)  :: e
        integer,      intent(out) :: rows, cols
        type(expr_t) :: row

        rows = 0
        cols = 0
        if (.not. is_matrix(e)) return
        rows = e%nargs()
        row = e%arg(1)
        cols = row%nargs()
    end subroutine matrix_shape

    !> Nested Lists to a rank-2 array.
    subroutine to_matrix(e, m, ok)
        type(expr_t),              intent(in)  :: e
        type(expr_t), allocatable, intent(out) :: m(:, :)
        logical,                   intent(out) :: ok
        type(expr_t) :: row
        integer :: rows, cols, i, j

        call matrix_shape(e, rows, cols)
        ok = rows > 0 .and. cols > 0
        if (.not. ok) return

        allocate (m(rows, cols))
        do i = 1, rows
            row = e%arg(i)
            do j = 1, cols
                m(i, j) = row%arg(j)
            end do
        end do
    end subroutine to_matrix

    !> A rank-2 array back to nested Lists.
    function from_matrix(a, m) result(e)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: m(:, :)
        type(expr_t)                         :: e
        type(expr_t), allocatable :: rows(:), entries(:)
        integer :: i, j

        allocate (rows(size(m, 1)))
        allocate (entries(size(m, 2)))
        do i = 1, size(m, 1)
            do j = 1, size(m, 2)
                entries(j) = m(i, j)
            end do
            rows(i) = func("List", entries)
        end do
        e = func("List", rows)
    end function from_matrix

    function matrix_transpose(a, e, ok) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        logical,               intent(out)   :: ok
        type(expr_t)                         :: r
        type(expr_t), allocatable :: m(:, :), t(:, :)
        integer :: i, j

        r = e
        call to_matrix(e, m, ok)
        if (.not. ok) return

        allocate (t(size(m, 2), size(m, 1)))
        do i = 1, size(m, 1)
            do j = 1, size(m, 2)
                t(j, i) = m(i, j)
            end do
        end do
        r = from_matrix(a, t)
    end function matrix_transpose

    !> Add or subtract two nonempty rectangular dense matrices.
    !>
    !> The optional flag keeps one elementwise owner for both operations;
    !> callers still expose distinct add/subtract names at their public
    !> language boundaries.
    function matrix_add(a, x, y, ok, why, subtract, canonical) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: x, y
        logical, intent(out) :: ok
        type(str_t), intent(out) :: why
        logical, optional, intent(in) :: subtract
        logical, optional, intent(out) :: canonical
        type(expr_t) :: r
        type(expr_t), allocatable :: mx(:, :), my(:, :), p(:, :)
        logical :: do_subtract, fast
        integer :: i, j

        r = x
        ok = .false.
        why = str("")
        if (present(canonical)) canonical = .false.
        do_subtract = .false.
        if (present(subtract)) do_subtract = subtract
        call to_matrix(x, mx, ok)
        if (.not. ok) then
            why = str("matrix addition requires a nonempty rectangular matrix")
            return
        end if
        call to_matrix(y, my, ok)
        if (.not. ok) then
            why = str("matrix addition requires a nonempty rectangular matrix")
            return
        end if
        if (size(mx, 1) /= size(my, 1) .or. size(mx, 2) /= size(my, 2)) then
            ok = .false.
            why = str("Matrix addition requires equal dimensions")
            return
        end if
        allocate (p(size(mx, 1), size(mx, 2)))
        call try_integer_matrix_add(mx, my, p, do_subtract, fast)
        if (fast) then
            r = from_matrix(a, p)
            ok = .true.
            if (present(canonical)) canonical = .true.
            return
        end if
        do i = 1, size(mx, 1)
            do j = 1, size(mx, 2)
                if (do_subtract) then
                    p(i, j) = mx(i, j) - my(i, j)
                else
                    p(i, j) = mx(i, j) + my(i, j)
                end if
            end do
        end do
        r = from_matrix(a, p)
        ok = .true.
    end function matrix_add

    !> Negate a nonempty rectangular dense matrix.
    function matrix_negate(a, x, ok, why, canonical) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: x
        logical, intent(out) :: ok
        type(str_t), intent(out) :: why
        logical, optional, intent(out) :: canonical
        type(expr_t) :: r
        type(expr_t), allocatable :: mx(:, :), p(:, :)
        logical :: fast
        integer :: i, j

        r = x
        why = str("")
        if (present(canonical)) canonical = .false.
        call to_matrix(x, mx, ok)
        if (.not. ok) then
            why = str("matrix negation requires a nonempty rectangular matrix")
            return
        end if
        allocate (p(size(mx, 1), size(mx, 2)))
        call try_integer_matrix_negate(mx, p, fast)
        if (fast) then
            r = from_matrix(a, p)
            ok = .true.
            if (present(canonical)) canonical = .true.
            return
        end if
        do i = 1, size(mx, 1)
            do j = 1, size(mx, 2)
                p(i, j) = -mx(i, j)
            end do
        end do
        r = from_matrix(a, p)
        ok = .true.
    end function matrix_negate

    !> Divide a dense matrix by a scalar through one exact quotient path.
    function matrix_divide(a, matrix, scalar, ok, why, canonical) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: matrix, scalar
        logical, intent(out) :: ok
        type(str_t), intent(out) :: why
        logical, optional, intent(out) :: canonical
        type(expr_t) :: r
        type(expr_t), allocatable :: mx(:, :), p(:, :)
        type(expr_t) :: reciprocal
        type(str_t) :: message
        logical :: fast

        r = matrix
        ok = .false.
        why = str("")
        if (present(canonical)) canonical = .false.
        if (.not. same_arena(matrix, scalar)) then
            why = str("matrix division operands belong to different arenas")
            return
        end if
        call to_matrix(matrix, mx, ok)
        if (.not. ok) then
            why = str("matrix division requires a nonempty rectangular matrix")
            return
        end if
        call try_integer_matrix_divide(mx, scalar, p, fast)
        if (fast) then
            r = from_matrix(a, p)
            ok = .true.
            if (present(canonical)) canonical = .true.
            return
        end if
        if (scalar%kind() == NK_INT .and. scalar%int_value() == 0_int64) then
            reciprocal = zoo_expr(a)
        else
            reciprocal = num(a, 1_int64)/scalar
        end if
        r = matrix_dot(a, matrix, reciprocal, ok, message)
        why = message
    end function matrix_divide

    subroutine try_integer_matrix_divide(matrix, scalar, quotient, used)
        type(expr_t), intent(in) :: matrix(:, :), scalar
        type(expr_t), allocatable, intent(out) :: quotient(:, :)
        logical, intent(out) :: used
        integer(int64) :: denominator
        integer :: i, j

        used = .false.
        if (scalar%kind() /= NK_INT) return
        denominator = scalar%int_value()
        do i = 1, size(matrix, 1)
            do j = 1, size(matrix, 2)
                if (matrix(i, j)%kind() /= NK_INT) return
            end do
        end do
        allocate (quotient(size(matrix, 1), size(matrix, 2)))
        do i = 1, size(matrix, 1)
            do j = 1, size(matrix, 2)
                if (denominator == 0_int64) then
                    quotient(i, j) = zoo_expr(matrix(i, j)%a)
                else
                    quotient(i, j) = rat(matrix(i, j)%a, &
                        matrix(i, j)%int_value(), denominator)
                end if
            end do
        end do
        used = .true.
    end subroutine try_integer_matrix_divide

    subroutine try_integer_matrix_add(left, right, product, subtract, used)
        type(expr_t), intent(in) :: left(:, :), right(:, :)
        type(expr_t), intent(out) :: product(:, :)
        logical, intent(in) :: subtract
        logical, intent(out) :: used
        integer(int64) :: left_value, right_value
        integer :: i, j

        used = .false.
        do i = 1, size(left, 1)
            do j = 1, size(left, 2)
                if (left(i, j)%kind() /= NK_INT .or. &
                    right(i, j)%kind() /= NK_INT) return
                left_value = left(i, j)%int_value()
                right_value = right(i, j)%int_value()
                if (subtract) then
                    if (.not. integer_subtract_fits(left_value, right_value)) &
                        return
                    product(i, j) = num(left(i, j)%a, left_value - right_value)
                else
                    if (.not. integer_sum_fits(left_value, right_value)) return
                    product(i, j) = num(left(i, j)%a, left_value + right_value)
                end if
            end do
        end do
        used = .true.
    end subroutine try_integer_matrix_add

    subroutine try_integer_matrix_negate(matrix, product, used)
        type(expr_t), intent(in) :: matrix(:, :)
        type(expr_t), intent(out) :: product(:, :)
        logical, intent(out) :: used
        integer(int64) :: value
        integer :: i, j

        used = .false.
        do i = 1, size(matrix, 1)
            do j = 1, size(matrix, 2)
                if (matrix(i, j)%kind() /= NK_INT) return
                value = matrix(i, j)%int_value()
                if (value == -huge(0_int64) - 1_int64) return
                product(i, j) = num(matrix(i, j)%a, -value)
            end do
        end do
        used = .true.
    end subroutine try_integer_matrix_negate

    !> Matrix, matrix-vector and vector products.
    !>
    !> Dimension mismatch fails rather than truncating to the shorter operand.
    !> A silently truncated product is dimensionally plausible and numerically
    !> wrong, which is the hardest kind of error to notice downstream.
    function matrix_dot(a, x, y, ok, why, canonical) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: x, y
        logical,               intent(out)   :: ok
        type(str_t),           intent(out)   :: why
        logical, optional,     intent(out)   :: canonical
        type(expr_t)                         :: r
        type(expr_t), allocatable :: mx(:, :), my(:, :), p(:, :)
        logical :: xm, ym, fast
        integer :: i, j, k

        ok = .false.
        why = str("")
        if (present(canonical)) canonical = .false.
        r = x
        xm = is_matrix(x)
        ym = is_matrix(y)

        if (xm .and. .not. is_list(y)) then
            call to_matrix(x, mx, ok)
            if (.not. ok) return
            allocate (p(size(mx, 1), size(mx, 2)))
            call try_integer_matrix_scale(mx, y, p, fast)
            if (fast) then
                r = from_matrix(a, p)
                ok = .true.
                if (present(canonical)) canonical = .true.
                return
            end if
            do i = 1, size(mx, 1)
                do j = 1, size(mx, 2)
                    p(i, j) = mx(i, j)*y
                end do
            end do
            r = from_matrix(a, p)
            ok = .true.
            return
        end if

        if (ym .and. .not. is_list(x)) then
            call to_matrix(y, my, ok)
            if (.not. ok) return
            allocate (p(size(my, 1), size(my, 2)))
            call try_integer_matrix_scale(my, x, p, fast)
            if (fast) then
                r = from_matrix(a, p)
                ok = .true.
                if (present(canonical)) canonical = .true.
                return
            end if
            do i = 1, size(my, 1)
                do j = 1, size(my, 2)
                    p(i, j) = x*my(i, j)
                end do
            end do
            r = from_matrix(a, p)
            ok = .true.
            return
        end if

        if (xm .and. ym) then
            call to_matrix(x, mx, ok)
            if (.not. ok) return
            call to_matrix(y, my, ok)
            if (.not. ok) return
            if (size(mx, 2) /= size(my, 1)) then
                ok = .false.
                why = str("Dot with mismatched dimensions")
                return
            end if
            allocate (p(size(mx, 1), size(my, 2)))
            call try_integer_matrix_product(mx, my, p, fast)
            if (fast) then
                r = from_matrix(a, p)
                ok = .true.
                if (present(canonical)) canonical = .true.
                return
            end if
            do i = 1, size(mx, 1)
                do j = 1, size(my, 2)
                    p(i, j) = mx(i, 1)*my(1, j)
                    do k = 2, size(mx, 2)
                        p(i, j) = p(i, j) + mx(i, k)*my(k, j)
                    end do
                end do
            end do
            r = from_matrix(a, p)
            ok = .true.
            return
        end if

        if (xm .and. is_list(y)) then
            r = matrix_times_vector(a, x, y, ok, why)
            return
        end if

        if (is_list(x) .and. .not. xm .and. ym) then
            r = vector_times_matrix(a, x, y, ok, why)
            return
        end if

        if (is_list(x) .and. is_list(y) .and. .not. ym) then
            r = vector_dot(x, y, ok, why)
            return
        end if

        why = str("Dot on operands that are not lists")
    end function matrix_dot

    subroutine try_integer_matrix_scale(matrix, scalar, product, used)
        type(expr_t), intent(in) :: matrix(:, :), scalar
        type(expr_t), intent(out) :: product(:, :)
        logical, intent(out) :: used
        integer(int64) :: factor
        integer :: i, j

        used = .false.
        if (scalar%kind() /= NK_INT) return
        factor = scalar%int_value()
        do i = 1, size(matrix, 1)
            do j = 1, size(matrix, 2)
                if (matrix(i, j)%kind() /= NK_INT) return
                if (.not. integer_product_fits( &
                    matrix(i, j)%int_value(), factor)) return
            end do
        end do
        do i = 1, size(matrix, 1)
            do j = 1, size(matrix, 2)
                product(i, j) = num(matrix(i, j)%a, &
                    matrix(i, j)%int_value()*factor)
            end do
        end do
        used = .true.
    end subroutine try_integer_matrix_scale

    subroutine try_integer_matrix_product(left, right, product, used)
        type(expr_t), intent(in) :: left(:, :), right(:, :)
        type(expr_t), intent(out) :: product(:, :)
        logical, intent(out) :: used
        integer(int64) :: total, term
        integer :: i, j, k

        used = .false.
        do i = 1, size(left, 1)
            do k = 1, size(left, 2)
                if (left(i, k)%kind() /= NK_INT) return
            end do
        end do
        do k = 1, size(right, 1)
            do j = 1, size(right, 2)
                if (right(k, j)%kind() /= NK_INT) return
            end do
        end do
        do i = 1, size(left, 1)
            do j = 1, size(right, 2)
                total = 0_int64
                do k = 1, size(left, 2)
                    if (.not. integer_product_fits( &
                        left(i, k)%int_value(), right(k, j)%int_value())) then
                        return
                    end if
                    term = left(i, k)%int_value()*right(k, j)%int_value()
                    if (.not. integer_sum_fits(total, term)) return
                    total = total + term
                end do
                product(i, j) = num(left(1, 1)%a, total)
            end do
        end do
        used = .true.
    end subroutine try_integer_matrix_product

    logical function integer_product_fits(left, right)
        integer(int64), intent(in) :: left, right
        integer(int64), parameter :: largest = huge(0_int64)
        integer(int64), parameter :: smallest = -largest - 1_int64

        if (left == 0_int64 .or. right == 0_int64) then
            integer_product_fits = .true.
        else if (left > 0_int64) then
            if (right > 0_int64) then
                integer_product_fits = left <= largest/right
            else
                integer_product_fits = right >= smallest/left
            end if
        else if (right > 0_int64) then
            integer_product_fits = left >= smallest/right
        else if (left == smallest) then
            integer_product_fits = .false.
        else if (right == smallest) then
            integer_product_fits = .false.
        else
            integer_product_fits = (-left) <= largest/(-right)
        end if
    end function integer_product_fits

    logical function integer_sum_fits(left, right)
        integer(int64), intent(in) :: left, right
        integer(int64), parameter :: largest = huge(0_int64)
        integer(int64), parameter :: smallest = -largest - 1_int64

        if (right > 0_int64) then
            integer_sum_fits = left <= largest - right
        else
            integer_sum_fits = left >= smallest - right
        end if
    end function integer_sum_fits

    logical function integer_subtract_fits(left, right)
        integer(int64), intent(in) :: left, right
        integer(int64), parameter :: largest = huge(0_int64)
        integer(int64), parameter :: smallest = -largest - 1_int64

        if (right > 0_int64) then
            integer_subtract_fits = left >= smallest + right
        else
            integer_subtract_fits = left <= largest + right
        end if
    end function integer_subtract_fits

    function matrix_times_vector(a, x, y, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: x, y
        logical,               intent(out)   :: ok
        type(str_t),           intent(out)   :: why
        type(expr_t)                         :: r
        type(expr_t), allocatable :: m(:, :), out(:)
        integer :: i, k

        r = x
        why = str("")
        call to_matrix(x, m, ok)
        if (.not. ok) return
        if (size(m, 2) /= y%nargs()) then
            ok = .false.
            why = str("Dot with mismatched dimensions")
            return
        end if

        allocate (out(size(m, 1)))
        do i = 1, size(m, 1)
            out(i) = m(i, 1)*y%arg(1)
            do k = 2, size(m, 2)
                out(i) = out(i) + m(i, k)*y%arg(k)
            end do
        end do
        r = func("List", out)
        ok = .true.
    end function matrix_times_vector

    function vector_times_matrix(a, x, y, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: x, y
        logical,               intent(out)   :: ok
        type(str_t),           intent(out)   :: why
        type(expr_t)                         :: r
        type(expr_t), allocatable :: m(:, :), out(:)
        integer :: j, k

        r = x
        why = str("")
        call to_matrix(y, m, ok)
        if (.not. ok) return
        if (x%nargs() /= size(m, 1)) then
            ok = .false.
            why = str("Dot with mismatched dimensions")
            return
        end if

        allocate (out(size(m, 2)))
        do j = 1, size(m, 2)
            out(j) = x%arg(1)*m(1, j)
            do k = 2, size(m, 1)
                out(j) = out(j) + x%arg(k)*m(k, j)
            end do
        end do
        r = func("List", out)
        ok = .true.
    end function vector_times_matrix

    function vector_dot(x, y, ok, why) result(r)
        type(expr_t), intent(in)  :: x, y
        logical,      intent(out) :: ok
        type(str_t),  intent(out) :: why
        type(expr_t)              :: r
        integer :: k

        r = x
        why = str("")
        ok = .false.
        if (x%nargs() /= y%nargs()) then
            why = str("Dot with mismatched dimensions")
            return
        end if
        if (x%nargs() == 0) then
            why = str("Dot on empty vectors")
            return
        end if

        r = x%arg(1)*y%arg(1)
        do k = 2, x%nargs()
            r = r + x%arg(k)*y%arg(k)
        end do
        ok = .true.
    end function vector_dot

    !> Determinant by fraction-free Bareiss elimination.
    !>
    !> Every division in the inner loop is exact by Sylvester's identity, so
    !> the intermediates stay polynomial in the entries and never become
    !> rational functions that then have to be cancelled back down.
    function matrix_det(a, e, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        logical,               intent(out)   :: ok
        type(str_t),           intent(out)   :: why
        type(expr_t)                         :: r
        type(expr_t), allocatable :: m(:, :)
        type(expr_t) :: previous, swap
        integer :: n, i, j, k, pivot
        logical :: negated

        r = e
        why = str("")
        call to_matrix(e, m, ok)
        if (.not. ok) then
            why = str("Det on something that is not a matrix")
            return
        end if
        n = size(m, 1)
        if (n /= size(m, 2)) then
            ok = .false.
            why = str("Det of a non-square matrix")
            return
        end if

        negated = .false.
        previous = num(a, 1)

        do k = 1, n - 1
            ! A symbolic pivot may or may not vanish, and fortsym cannot decide
            ! that without an assumption context. Only a literal zero is
            ! treated as zero; anything else is used as written, and the caller
            ! is responsible for the domain on which that holds.
            if (is_literal_zero(m(k, k))) then
                pivot = 0
                do i = k + 1, n
                    if (.not. is_literal_zero(m(i, k))) then
                        pivot = i
                        exit
                    end if
                end do
                if (pivot == 0) then
                    r = num(a, 0)
                    ok = .true.
                    return
                end if
                do j = 1, n
                    swap = m(k, j)
                    m(k, j) = m(pivot, j)
                    m(pivot, j) = swap
                end do
                negated = .not. negated
            end if

            do i = k + 1, n
                do j = k + 1, n
                    m(i, j) = (m(i, j)*m(k, k) - m(i, k)*m(k, j))/previous
                end do
            end do
            previous = m(k, k)
        end do

        r = m(n, n)
        if (negated) r = num(a, -1)*r
        ok = .true.
    end function matrix_det

    !> Inverse as adjugate over determinant.
    !>
    !> Returned unconditionally: whether the determinant vanishes is a question
    !> about the domain, and answering it needs an assumption context. The
    !> caller sees the determinant in the denominator and can decide.
    function matrix_inverse(a, e, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        logical,               intent(out)   :: ok
        type(str_t),           intent(out)   :: why
        type(expr_t)                         :: r
        type(expr_t), allocatable :: m(:, :), inv(:, :)
        type(expr_t) :: determinant, minor_det
        type(native_engine_t) :: engine
        type(engine_result_t) :: simplified
        integer :: n, i, j

        r = e
        why = str("")
        call to_matrix(e, m, ok)
        if (.not. ok) then
            why = str("Inverse on something that is not a matrix")
            return
        end if
        n = size(m, 1)
        if (n /= size(m, 2)) then
            ok = .false.
            why = str("Inverse of a non-square matrix")
            return
        end if

        determinant = matrix_det(a, e, ok, why)
        if (.not. ok) return
        engine = make_native_engine(a)
        simplified = engine%simplify(determinant)
        if (.not. simplified%ok) then
            ok = .false.
            why = str("Could not simplify the matrix determinant")
            return
        end if
        determinant = simplified%value
        if (is_literal_zero(determinant)) then
            ok = .false.
            why = str("Inverse of a matrix with zero determinant")
            return
        end if

        allocate (inv(n, n))
        do i = 1, n
            do j = 1, n
                ! Transposed on purpose: the inverse is the adjugate, which is
                ! the transpose of the cofactor matrix. Writing inv(i, j) from
                ! cofactor(i, j) gives the inverse of the transpose instead,
                ! and for a symmetric example the tests would still pass.
                minor_det = cofactor(a, m, j, i, ok, why)
                if (.not. ok) return
                inv(i, j) = minor_det/determinant
            end do
        end do
        r = from_matrix(a, inv)
        ok = .true.
    end function matrix_inverse

    !> Reduced row-echelon form by exact Gaussian elimination.
    !>
    !> A literal zero is the only pivot known to vanish. Symbolic pivots are
    !> retained, which is the generic-domain interpretation used by the other
    !> exact matrix operations in this module; callers needing exceptional
    !> parameter values must supply their own assumptions.
    function matrix_row_reduce(a, e, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        logical,               intent(out)   :: ok
        type(str_t),           intent(out)   :: why
        type(expr_t)                         :: r
        type(expr_t), allocatable :: m(:, :)
        integer, allocatable :: pivots(:)
        integer :: rank

        r = e
        why = str("")
        call to_matrix(e, m, ok)
        if (.not. ok) then
            why = str("RowReduce on something that is not a matrix")
            return
        end if
        call rref(a, m, rank, pivots, ok, why)
        if (.not. ok) return
        r = from_matrix(a, m)
    end function matrix_row_reduce

    !> Reduced row-echelon form and zero-based pivot columns.
    !>
    !> The result is `List(reduced_matrix, List(pivot_columns))`, matching the
    !> two-value shape of SymPy `Matrix.rref()` at the expression boundary.
    !> The pivot-column list is zero-based because it is a Python-facing index
    !> result; the internal elimination remains one-based Fortran indexing.
    function matrix_rref(a, e, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: e
        logical, intent(out) :: ok
        type(str_t), intent(out) :: why
        type(expr_t) :: r, reduced, pivot_list
        type(expr_t), allocatable :: m(:, :), pivot_values(:)
        type(expr_t) :: result_parts(2)
        integer, allocatable :: pivots(:)
        integer :: rank, i

        r = e
        why = str("")
        call to_matrix(e, m, ok)
        if (.not. ok) then
            why = str("RREF on something that is not a matrix")
            return
        end if
        call rref(a, m, rank, pivots, ok, why)
        if (.not. ok) return

        reduced = from_matrix(a, m)
        if (rank == 0) then
            pivot_list = func_in(a, "List")
        else
            allocate (pivot_values(rank))
            do i = 1, rank
                pivot_values(i) = num(a, pivots(i) - 1)
            end do
            pivot_list = func("List", pivot_values)
        end if
        result_parts(1) = reduced
        result_parts(2) = pivot_list
        r = func("List", result_parts)
        ok = .true.
    end function matrix_rref

    !> A basis for the right null space of a rectangular matrix.
    !>
    !> The free variable in each basis vector is set to one. This gives the
    !> same canonical basis as the usual RREF construction and, importantly,
    !> does not depend on a numerical tolerance for exact input.
    function matrix_null_space(a, e, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        logical,               intent(out)   :: ok
        type(str_t),           intent(out)   :: why
        type(expr_t)                         :: r
        type(expr_t), allocatable :: m(:, :), vector(:), basis(:)
        integer, allocatable :: pivots(:)
        logical, allocatable :: is_pivot(:)
        type(native_engine_t) :: engine
        type(engine_result_t) :: simplified
        integer :: rank, cols, col, i, k, nfree

        r = e
        why = str("")
        call to_matrix(e, m, ok)
        if (.not. ok) then
            why = str("NullSpace on something that is not a matrix")
            return
        end if
        call rref(a, m, rank, pivots, ok, why)
        if (.not. ok) return
        engine = make_native_engine(a)

        cols = size(m, 2)
        allocate (is_pivot(cols))
        is_pivot = .false.
        do i = 1, rank
            is_pivot(pivots(i)) = .true.
        end do
        nfree = count(.not. is_pivot)
        if (nfree == 0) then
            r = func_in(a, "List")
            ok = .true.
            return
        end if

        allocate (basis(nfree), vector(cols))
        k = 0
        do col = 1, cols
            if (is_pivot(col)) cycle
            k = k + 1
            vector = num(a, 0)
            vector(col) = num(a, 1)
            do i = 1, rank
                simplified = engine%simplify(-m(i, col))
                if (.not. simplified%ok) then
                    why = str("NullSpace could not simplify a basis entry")
                    ok = .false.
                    return
                end if
                vector(pivots(i)) = simplified%value
            end do
            basis(k) = func("List", vector)
        end do
        r = func("List", basis)
        ok = .true.
    end function matrix_null_space

    !> Rank of an exact rectangular matrix, obtained from its RREF pivots.
    function matrix_rank(a, e, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        logical,               intent(out)   :: ok
        type(str_t),           intent(out)   :: why
        type(expr_t)                         :: r
        type(expr_t), allocatable :: m(:, :)
        integer, allocatable :: pivots(:)
        integer :: rank

        r = e
        why = str("")
        call to_matrix(e, m, ok)
        if (.not. ok) then
            why = str("MatrixRank on something that is not a matrix")
            return
        end if
        call rref(a, m, rank, pivots, ok, why)
        if (.not. ok) return
        r = num(a, rank)
    end function matrix_rank

    !> All exact minors of a requested order.
    !>
    !> The result is nested by the selected row combinations and then the
    !> selected column combinations, matching the Wolfram `Minors` shape. A
    !> zero order means the default maximal order. Combination counts are
    !> bounded before any submatrices are allocated, so a large input refuses
    !> rather than expanding an unexpectedly huge collection of determinants.
    function matrix_minors(a, e, order, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: e
        integer, intent(in) :: order
        logical, intent(out) :: ok
        type(str_t), intent(out) :: why
        type(expr_t) :: r, determinant
        type(expr_t), allocatable :: m(:, :), minor(:, :)
        type(expr_t), allocatable :: row_results(:), column_results(:)
        integer, allocatable :: row_choices(:, :), column_choices(:, :)
        integer, allocatable :: choice(:)
        type(native_engine_t) :: engine
        type(engine_result_t) :: simplified
        integer :: rows, cols, k, row_count, column_count
        integer :: i, j, p, q, next
        logical :: count_ok, shape_ok

        r = e
        ok = .false.
        why = str("")
        call to_matrix(e, m, shape_ok)
        if (.not. shape_ok) then
            why = str("Minors on something that is not a matrix")
            return
        end if

        rows = size(m, 1)
        cols = size(m, 2)
        k = order
        if (k == 0) k = min(rows, cols)
        if (k < 1 .or. k > min(rows, cols)) then
            why = str("Minors order is outside the matrix dimensions")
            return
        end if

        call combination_count(rows, k, row_count, count_ok)
        if (.not. count_ok) then
            why = str("Minors exceeds the built-in combination bound")
            return
        end if
        call combination_count(cols, k, column_count, count_ok)
        if (.not. count_ok) then
            why = str("Minors exceeds the built-in combination bound")
            return
        end if
        if (int(row_count, int64)*int(column_count, int64) > &
            int(MAX_MINOR_OUTPUT, int64)) then
            why = str("Minors exceeds the built-in result bound")
            return
        end if

        allocate (row_choices(k, row_count), column_choices(k, column_count))
        allocate (row_results(row_count))
        allocate (choice(k))
        engine = make_native_engine(a)
        next = 0
        call fill_combinations(rows, k, 1, 1, choice, &
            row_choices, next)
        next = 0
        call fill_combinations(cols, k, 1, 1, choice, &
            column_choices, next)
        deallocate (choice)

        do i = 1, row_count
            allocate (column_results(column_count))
            do j = 1, column_count
                allocate (minor(k, k))
                do p = 1, k
                    do q = 1, k
                        minor(p, q) = m(row_choices(p, i), column_choices(q, j))
                    end do
                end do
                ! Bareiss is the scalable route, but a symbolic pivot in a
                ! small minor can leave a denominator that is mathematically
                ! cancelled but not yet visible to the native simplifier.
                ! The direct 1x1--3x3 identities stay polynomial and therefore
                ! preserve the exact symbolic Minors result.
                if (k <= 3) then
                    determinant = small_determinant(minor)
                    ok = .true.
                else
                    determinant = matrix_det(a, from_matrix(a, minor), ok, why)
                end if
                deallocate (minor)
                if (.not. ok) return
                simplified = engine%simplify(determinant)
                if (.not. simplified%ok) then
                    why = str("Minors could not simplify a determinant")
                    ok = .false.
                    return
                end if
                column_results(j) = simplified%value
            end do
            row_results(i) = func("List", column_results)
            deallocate (column_results)
        end do

        r = func("List", row_results)
        ok = .true.
    end function matrix_minors

    !> In-place exact RREF and its pivot columns.
    subroutine rref(a, m, rank, pivots, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(inout) :: m(:, :)
        integer,                   intent(out)   :: rank
        integer, allocatable,      intent(out)   :: pivots(:)
        logical,                   intent(out)   :: ok
        type(str_t),               intent(out)   :: why
        type(expr_t) :: pivot_value, factor, swap
        type(native_engine_t) :: engine
        type(engine_result_t) :: simplified
        integer :: rows, cols, pivot_row, pivot, col, i, j

        rows = size(m, 1)
        cols = size(m, 2)
        rank = 0
        allocate (pivots(min(rows, cols)))
        pivots = 0
        ok = .false.
        why = str("")

        if (rows > MAX_RREF_ROWS .or. cols > MAX_RREF_COLS .or. &
            rows*cols > MAX_RREF_ENTRIES) then
            why = str("exact row reduction exceeds the built-in matrix bound")
            return
        end if

        engine = make_native_engine(a)

        pivot_row = 1
        do col = 1, cols
            if (pivot_row > rows) exit
            pivot = 0
            do i = pivot_row, rows
                if (.not. is_literal_zero(m(i, col))) then
                    pivot = i
                    exit
                end if
            end do
            if (pivot == 0) cycle

            if (pivot /= pivot_row) then
                do j = 1, cols
                    swap = m(pivot_row, j)
                    m(pivot_row, j) = m(pivot, j)
                    m(pivot, j) = swap
                end do
            end if

            pivot_value = m(pivot_row, col)
            do j = col, cols
                simplified = engine%simplify(m(pivot_row, j)/pivot_value)
                if (.not. simplified%ok) then
                    why = str("RowReduce could not simplify a pivot row")
                    return
                end if
                m(pivot_row, j) = simplified%value
            end do

            do i = 1, rows
                if (i == pivot_row) cycle
                factor = m(i, col)
                if (is_literal_zero(factor)) cycle
                do j = col, cols
                    simplified = engine%simplify( &
                        m(i, j) - factor*m(pivot_row, j))
                    if (.not. simplified%ok) then
                        why = str("RowReduce could not simplify an elimination row")
                        return
                    end if
                    m(i, j) = simplified%value
                end do
            end do

            rank = rank + 1
            pivots(rank) = col
            pivot_row = pivot_row + 1
        end do
        ok = .true.
    end subroutine rref

    !> Signed determinant of the matrix with row i and column j removed.
    function cofactor(a, m, i, j, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: m(:, :)
        integer,               intent(in)    :: i, j
        logical,               intent(out)   :: ok
        type(str_t),           intent(out)   :: why
        type(expr_t)                         :: r
        type(expr_t), allocatable :: minor(:, :)
        integer :: n, p, q, pi, qi

        n = size(m, 1)
        if (n == 1) then
            r = num(a, 1)
            ok = .true.
            why = str("")
            return
        end if

        allocate (minor(n - 1, n - 1))
        pi = 0
        do p = 1, n
            if (p == i) cycle
            pi = pi + 1
            qi = 0
            do q = 1, n
                if (q == j) cycle
                qi = qi + 1
                minor(pi, qi) = m(p, q)
            end do
        end do

        r = matrix_det(a, from_matrix(a, minor), ok, why)
        if (.not. ok) return
        if (mod(i + j, 2) == 1) r = num(a, -1)*r
    end function cofactor

    !> True only for an exact literal zero.
    !>
    !> A symbolic expression that happens to be zero is not detected here on
    !> purpose: deciding that is the zero test's job and it needs the
    !> assumptions this module does not carry.
    function is_literal_zero(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        character(:), allocatable :: text

        text = chars(e%exact_text())
        yes = text == "0"
    end function is_literal_zero

    subroutine combination_count(n, k, count, ok)
        integer, intent(in) :: n, k
        integer, intent(out) :: count
        logical, intent(out) :: ok
        integer(int64) :: value, numerator
        integer :: i

        value = 1_int64
        ok = .false.
        count = 0
        if (k < 0 .or. k > n) return
        do i = 1, k
            numerator = int(n - k + i, int64)
            if (value > int(MAX_MINOR_OUTPUT, int64)*int(i, int64)/numerator) &
                return
            value = value*numerator/int(i, int64)
        end do
        count = int(value)
        ok = .true.
    end subroutine combination_count

    recursive subroutine fill_combinations(n, k, start, depth, choice, choices, next)
        integer, intent(in) :: n, k, start
        integer, intent(in) :: depth
        integer, intent(inout) :: choice(:)
        integer, intent(inout) :: choices(:, :)
        integer, intent(inout) :: next
        integer :: candidate

        if (depth > k) then
            next = next + 1
            choices(:, next) = choice
            return
        end if
        do candidate = start, n - (k - depth)
            choice(depth) = candidate
            call fill_combinations(n, k, candidate + 1, depth + 1, &
                choice, choices, next)
        end do
    end subroutine fill_combinations

    function small_determinant(m) result(r)
        type(expr_t), intent(in) :: m(:, :)
        type(expr_t) :: r
        integer :: n

        n = size(m, 1)
        select case (n)
        case (1)
            r = m(1, 1)
        case (2)
            r = m(1, 1)*m(2, 2) - m(1, 2)*m(2, 1)
        case (3)
            r = m(1, 1)*m(2, 2)*m(3, 3) - &
                m(1, 1)*m(2, 3)*m(3, 2) - &
                m(1, 2)*m(2, 1)*m(3, 3) + &
                m(1, 2)*m(2, 3)*m(3, 1) + &
                m(1, 3)*m(2, 1)*m(3, 2) - &
                m(1, 3)*m(2, 2)*m(3, 1)
        case default
            ! The caller only selects this helper for orders through three.
            r = m(1, 1)
        end select
    end function small_determinant

end module fortsym_matrix
