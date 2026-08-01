module fortsym_wl_num
    ! N, Chop, and the cheap exact structural heads for the Wolfram subset.
    !
    ! N[expr, p] is the operation with the most room to be confidently wrong.
    ! fortsym evaluates in real64, which carries roughly 15.95 decimal digits,
    ! so a request for 30 digits cannot be honoured and is refused rather than
    ! padded -- the padding would be indistinguishable from a correct answer.
    ! A request for 16 or fewer digits *can* be honoured: the double already
    ! holds at least that much information, and rounding to p significant
    ! digits is then a statement real64 supports.
    !
    ! Chop drops approximate numbers below a threshold. Only NK_REAL leaves are
    ! chopped: an exact rational is not an artefact of floating point and
    ! removing it would change the mathematics rather than clean it up.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_FUNC, NK_REAL, NK_ADD, NK_MUL, NK_POW
    use fortsym_expr, only: expr_t, num, func, real_expr, is_valid, &
        operator(+), operator(-), operator(*), operator(**)
    use fortsym_matrix, only: is_matrix, matrix_shape, to_matrix
    use fortsym_numeric, only: numeric_value
    implicit none
    private

    public :: wl_n, wl_chop, wl_identity_matrix, wl_cross, wl_trace
    public :: N_MAX_DIGITS, CHOP_DEFAULT

    integer, parameter :: dp = real64

    !> real64 holds 15.95 decimal digits, so 16 is the largest request it can
    !> answer without inventing information. 17 digits round-trips a double but
    !> the seventeenth digit is an artefact of the binary value, not a decimal
    !> the user asked for, so it is not offered.
    integer, parameter :: N_MAX_DIGITS = 16

    !> Wolfram's documented default Chop tolerance.
    real(dp), parameter :: CHOP_DEFAULT = 1.0e-10_dp

    !> Guard for building 10**exponent in a double.
    integer, parameter :: MAX_DECADE = 300

    integer, parameter :: MAX_IDENTITY = 512

contains

    !> N[expr] (digits <= 0) or N[expr, digits], mapped over lists elementwise.
    !>
    !> A list is mapped rather than refused because N[{a, b}] in Wolfram is
    !> {N[a], N[b]}, and that includes the nested lists that make up a matrix.
    recursive function wl_n(a, e, digits, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        integer,                   intent(in)    :: digits
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: r
        type(expr_t), allocatable :: items(:)
        real(dp) :: value, rounded
        integer :: k

        r = e
        ok = .false.
        why = ""

        if (digits > N_MAX_DIGITS) then
            why = "N with more significant digits than real64 carries: "// &
                  "fortsym evaluates in double precision, and padding the "// &
                  "request would look like a high-precision answer"
            return
        end if

        if (e%kind() == NK_FUNC) then
            if (chars(e%name()) == "List") then
                allocate (items(e%nargs()))
                do k = 1, e%nargs()
                    items(k) = wl_n(a, e%arg(k), digits, ok, why)
                    if (.not. ok) return
                end do
                if (size(items) == 0) then
                    ok = .true.
                    return
                end if
                r = func("List", items)
                ok = .true.
                return
            end if
        end if

        call numeric_value(e, value, ok, why)
        if (.not. ok) then
            r = e
            return
        end if

        rounded = value
        if (digits > 0) then
            call round_significant(value, digits, rounded, ok)
            if (.not. ok) then
                why = "N: the value's exponent is outside the range a "// &
                      "double can round in"
                r = e
                return
            end if
        end if
        r = real_expr(a, rounded)
        ok = is_valid(r)
        if (.not. ok) why = "N: the rounded value could not be built"
    end function wl_n

    !> Round to `digits` significant decimal digits.
    !>
    !> The mantissa is separated from the decade first. Forming a single
    !> 10**(digits-1-exponent) scale factor overflows for values near the ends
    !> of the double range, and an overflowed scale silently returns infinity
    !> or zero as though it were the rounded value.
    subroutine round_significant(value, digits, rounded, ok)
        real(dp), intent(in)  :: value
        integer,  intent(in)  :: digits
        real(dp), intent(out) :: rounded
        logical,  intent(out) :: ok
        real(dp) :: decade, mantissa, unit
        integer  :: exponent

        rounded = value
        ok = .true.
        if (value == 0.0_dp) return

        exponent = floor(log10(abs(value)))
        if (abs(exponent) > MAX_DECADE) then
            ok = .false.
            return
        end if
        decade = 10.0_dp**exponent
        mantissa = value/decade
        unit = 10.0_dp**(digits - 1)
        rounded = anint(mantissa*unit)/unit*decade
    end subroutine round_significant

    !> Chop[expr] or Chop[expr, delta]: approximate numbers smaller than the
    !> threshold in magnitude become exact zero, everywhere in the expression.
    !>
    !> The threshold is a magnitude on the *literal*, not on a term, so a small
    !> real multiplying the imaginary unit is chopped and the imaginary part
    !> disappears with it -- which is what makes Chop useful after a numeric
    !> decomposition.
    recursive function wl_chop(a, e, tol) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        real(dp),              intent(in)    :: tol
        type(expr_t)                         :: r
        type(expr_t), allocatable :: items(:)
        integer :: k

        r = e
        select case (e%kind())
        case (NK_REAL)
            if (abs(e%real_value()) < tol) r = num(a, 0)
        case (NK_ADD, NK_MUL, NK_POW)
            do k = 1, e%nargs()
                if (k == 1) then
                    r = wl_chop(a, e%arg(1), tol)
                    cycle
                end if
                select case (e%kind())
                case (NK_ADD); r = r + wl_chop(a, e%arg(k), tol)
                case (NK_MUL); r = r*wl_chop(a, e%arg(k), tol)
                case (NK_POW); r = r**wl_chop(a, e%arg(k), tol)
                end select
            end do
        case (NK_FUNC)
            if (e%nargs() == 0) return
            allocate (items(e%nargs()))
            do k = 1, e%nargs()
                items(k) = wl_chop(a, e%arg(k), tol)
            end do
            r = func(chars(e%name()), items)
        end select
    end function wl_chop

    !> IdentityMatrix[n] as nested Lists.
    function wl_identity_matrix(a, n, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        integer,                   intent(in)    :: n
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: r
        type(expr_t), allocatable :: rows(:), entries(:)
        integer :: i, j

        r = num(a, 0)
        ok = .false.
        why = ""
        if (n < 1) then
            why = "IdentityMatrix needs a positive size"
            return
        end if
        if (n > MAX_IDENTITY) then
            why = "IdentityMatrix larger than the built-in bound"
            return
        end if

        allocate (rows(n))
        allocate (entries(n))
        do i = 1, n
            do j = 1, n
                if (i == j) then
                    entries(j) = num(a, 1)
                else
                    entries(j) = num(a, 0)
                end if
            end do
            rows(i) = func("List", entries)
        end do
        r = func("List", rows)
        ok = .true.
    end function wl_identity_matrix

    !> Cross[x, y] for two three-component vectors.
    !>
    !> Only the three-dimensional case. Wolfram's Cross is defined for n-1
    !> vectors in n dimensions, and answering the general shape from a
    !> two-argument rule would produce a vector of the wrong length.
    function wl_cross(a, x, y, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: x, y
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: r
        type(expr_t) :: c(3)

        r = x
        ok = .false.
        why = ""
        if (.not. is_vector3(x) .or. .not. is_vector3(y)) then
            why = "Cross needs two three-component vectors"
            return
        end if

        c(1) = x%arg(2)*y%arg(3) - x%arg(3)*y%arg(2)
        c(2) = x%arg(3)*y%arg(1) - x%arg(1)*y%arg(3)
        c(3) = x%arg(1)*y%arg(2) - x%arg(2)*y%arg(1)
        r = func("List", c)
        ok = .true.
    end function wl_cross

    !> Tr[m]: the sum of the leading diagonal, over min(rows, cols) entries as
    !> Wolfram defines it for non-square input.
    function wl_trace(a, e, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: r
        type(expr_t), allocatable :: m(:, :)
        integer :: rows, cols, k
        logical :: good

        r = e
        ok = .false.
        why = ""
        if (.not. is_matrix(e)) then
            why = "Tr needs a rectangular matrix of lists"
            return
        end if
        call to_matrix(e, m, good)
        if (.not. good) then
            why = "Tr: the matrix could not be read"
            return
        end if
        call matrix_shape(e, rows, cols)

        r = m(1, 1)
        do k = 2, min(rows, cols)
            r = r + m(k, k)
        end do
        ok = .true.
    end function wl_trace

    function is_vector3(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        type(expr_t) :: item
        integer :: k

        yes = .false.
        if (e%kind() /= NK_FUNC) return
        if (chars(e%name()) /= "List") return
        if (e%nargs() /= 3) return
        ! A nested list is a matrix row, not a component: Cross of two 3x3
        ! matrices is not the cross product of two vectors.
        do k = 1, 3
            item = e%arg(k)
            if (item%kind() == NK_FUNC) then
                if (chars(item%name()) == "List") return
            end if
        end do
        yes = .true.
    end function is_vector3

end module fortsym_wl_num
