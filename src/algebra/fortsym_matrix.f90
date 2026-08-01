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
    use fortsym_arena, only: arena_t, NK_FUNC
    use fortsym_expr, only: expr_t, num, func, func_in, operator(+), &
        operator(-), operator(*), operator(/), operator(==)
    implicit none
    private

    public :: is_list, is_matrix, matrix_shape
    public :: matrix_transpose, matrix_dot, matrix_det, matrix_inverse
    public :: to_matrix, from_matrix

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

    !> Matrix, matrix-vector and vector products.
    !>
    !> Dimension mismatch fails rather than truncating to the shorter operand.
    !> A silently truncated product is dimensionally plausible and numerically
    !> wrong, which is the hardest kind of error to notice downstream.
    function matrix_dot(a, x, y, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: x, y
        logical,               intent(out)   :: ok
        type(str_t),           intent(out)   :: why
        type(expr_t)                         :: r
        type(expr_t), allocatable :: mx(:, :), my(:, :), p(:, :)
        logical :: xm, ym
        integer :: i, j, k

        ok = .false.
        why = str("")
        r = x
        xm = is_matrix(x)
        ym = is_matrix(y)

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

        if (is_list(x) .and. is_list(y) .and. .not. ym) then
            r = vector_dot(x, y, ok, why)
            return
        end if

        why = str("Dot on operands that are not lists")
    end function matrix_dot

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

end module fortsym_matrix
