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
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_FUNC, NK_REAL, &
        NK_ADD, NK_MUL, NK_POW
    use fortsym_expr, only: expr_t, num, rat, func, func_in, real_expr, is_valid, &
        operator(+), operator(-), operator(*), operator(**)
    use fortsym_matrix, only: is_list, is_matrix, matrix_shape, to_matrix
    use fortsym_numeric, only: numeric_value
    implicit none
    private

    public :: wl_n, wl_chop, wl_identity_matrix, wl_cross, wl_trace, wl_length
    public :: wl_flatten
    public :: wl_range, wl_diagonal_matrix
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
    integer, parameter :: MAX_FLATTEN_ITEMS = 10000
    !> Keep a valid but large range opaque. Expanding it is mathematically
    !> straightforward, but downstream symbolic consumers can turn a modest
    !> scan into a multi-megabyte expression before they have a rule for the
    !> consumer head. The caller can still carry the original Range node.
    integer, parameter :: MAX_RANGE_ITEMS = 64

    integer(int64), parameter :: MAX_I64 = huge(0_int64)
    integer(int64), parameter :: MIN_I64 = -huge(0_int64) - 1_int64

    !> A small exact rational used only while constructing Range. The arena
    !> remains the public representation; keeping the loop counter here avoids
    !> building unevaluated `start + k step` trees for a list that Wolfram
    !> evaluates element by element.
    type :: fraction_t
        integer(int64) :: n = 0_int64
        integer(int64) :: d = 1_int64
    end type fraction_t

contains

    !> Length[expr]: the number of immediate elements, or zero for an atom.
    !>
    !> This is structural rather than numeric. In particular Length[f[x, y]]
    !> is two even when `f` is an opaque user head, while Length[x] is zero.
    function wl_length(a, e, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: r

        r = num(a, e%nargs())
        ok = is_valid(r)
        why = ""
        if (.not. ok) why = "Length could not build its integer result"
    end function wl_length

    !> Flatten[list] recursively removes all nested List heads.
    !>
    !> Only the one-argument form is accepted here. A level specification
    !> changes which nesting is observable and is kept as a named refusal until
    !> it has its own independent coverage; a non-list expression is already
    !> flat and is returned unchanged.
    function wl_flatten(a, e, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: r
        type(expr_t), allocatable :: items(:)
        integer :: n

        r = e
        ok = .false.
        why = ""
        if (.not. is_list(e)) then
            ok = .true.
            return
        end if

        allocate (items(MAX_FLATTEN_ITEMS))
        n = 0
        call collect_flatten(e, items, n, ok, why)
        if (.not. ok) return
        if (n == 0) then
            r = func_in(a, "List")
        else
            r = func("List", items(:n))
        end if
    end function wl_flatten

    recursive subroutine collect_flatten(e, items, n, ok, why)
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(inout) :: items(:)
        integer,                   intent(inout) :: n
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        integer :: k

        ok = .false.
        why = ""
        if (is_list(e)) then
            do k = 1, e%nargs()
                call collect_flatten(e%arg(k), items, n, ok, why)
                if (.not. ok) return
            end do
            ok = .true.
            return
        end if

        if (n >= size(items)) then
            why = "Flatten exceeds the built-in item bound"
            return
        end if
        n = n + 1
        items(n) = e
        ok = .true.
    end subroutine collect_flatten

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

    !> Range[n], Range[lo, hi] and Range[lo, hi, step] for finite exact
    !> integer/rational endpoints. A machine-real form is accepted as well,
    !> but only when a machine-real argument is present; evaluating exact
    !> constants such as Pi to a double would silently change the exact list.
    !>
    !> A symbolic or otherwise non-machine numeric range is preserved as an
    !> opaque application. That is the same boundary as the rest of the
    !> Wolfram frontend: an unevaluated valid construct is safer than a
    !> plausible list made from an arbitrary substitution.
    function wl_range(a, e, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: r
        type(fraction_t) :: lo, hi, step, current
        type(expr_t), allocatable :: items(:)
        real(dp) :: xlo, xhi, xstep
        integer :: narg, n, k
        logical :: exact_form, machine_form, good, valid
        type(expr_t) :: item

        r = e
        ok = .false.
        why = ""
        narg = e%nargs()
        if (narg < 1 .or. narg > 3) then
            why = "Range takes one to three arguments"
            return
        end if

        exact_form = .true.
        machine_form = .false.
        do k = 1, narg
            item = e%arg(k)
            if (item%kind() == NK_REAL) machine_form = .true.
            valid = fraction_of(item, current, good)
            if (.not. valid) exact_form = .false.
        end do

        if (exact_form) then
            if (narg == 1) then
                lo = fraction_of_expr(1_int64)
                hi = current
                step = fraction_of_expr(1_int64)
                good = fraction_of(e%arg(1), hi, valid)
            else
                good = fraction_of(e%arg(1), lo, valid)
                good = fraction_of(e%arg(2), hi, valid)
                if (narg == 3) then
                    good = fraction_of(e%arg(3), step, valid)
                else
                    step = fraction_of_expr(1_int64)
                end if
            end if
            if (.not. good) then
                why = "Range contains an unsupported exact number"
                return
            end if
            call normalize_fraction(lo)
            call normalize_fraction(hi)
            call normalize_fraction(step)
            call build_exact_range(a, e, lo, hi, step, r, ok, why)
            return
        end if

        if (.not. machine_form) then
            ! Keep exact symbolic forms, for example Range[0, 2 Pi, Pi/2],
            ! unevaluated instead of replacing Pi by a binary64 approximation.
            ok = .true.
            return
        end if

        if (narg == 1) then
            call numeric_value(e%arg(1), xhi, good, why)
            xlo = 1.0_dp
            xstep = 1.0_dp
        else
            call numeric_value(e%arg(1), xlo, good, why)
            if (good) call numeric_value(e%arg(2), xhi, good, why)
            xstep = 1.0_dp
            if (good .and. narg == 3) call numeric_value(e%arg(3), xstep, good, why)
        end if
        if (.not. good) then
            ok = .true.
            return
        end if
        call build_real_range(a, e, xlo, xhi, xstep, r, ok, why)
    end function wl_range

    !> DiagonalMatrix[v] as a square nested List. Entries remain arbitrary
    !> expressions; only the vector shape and the zero off the diagonal are
    !> structural facts required by the constructor.
    function wl_diagonal_matrix(a, e, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: r
        type(expr_t), allocatable :: rows(:), entries(:)
        type(expr_t) :: vector
        integer :: n, i, j

        r = e
        ok = .false.
        why = ""
        if (e%nargs() /= 1) then
            why = "DiagonalMatrix here takes one vector"
            return
        end if
        vector = e%arg(1)
        if (vector%kind() /= NK_FUNC .or. chars(vector%name()) /= "List") then
            why = "DiagonalMatrix needs a list"
            return
        end if
        n = vector%nargs()
        if (n == 0) then
            r = func_in(a, "List")
            ok = .true.
            return
        end if
        if (n > MAX_IDENTITY) then
            why = "DiagonalMatrix exceeds the built-in dimension bound"
            return
        end if

        allocate (rows(n), entries(n))
        do i = 1, n
            do j = 1, n
                if (i == j) then
                    entries(j) = vector%arg(i)
                else
                    entries(j) = num(a, 0_int64)
                end if
            end do
            rows(i) = func("List", entries)
        end do
        r = func("List", rows)
        ok = .true.
    end function wl_diagonal_matrix

    subroutine build_exact_range(a, source, lo, hi, step, r, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: source
        type(fraction_t),          intent(in)    :: lo, hi, step
        type(expr_t),              intent(out)   :: r
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(fraction_t) :: current, next
        type(expr_t), allocatable :: items(:)
        integer :: n, cmp, good

        r = source
        ok = .false.
        why = ""
        if (step%n == 0_int64) then
            why = "Range has a zero step"
            return
        end if
        allocate (items(MAX_RANGE_ITEMS))
        current = lo
        n = 0
        do
            call compare_fraction(current, hi, cmp, good)
            if (good == 0) then
                why = "Range endpoints exceed exact integer capacity"
                return
            end if
            if ((step%n > 0_int64 .and. cmp > 0) .or. &
                    (step%n < 0_int64 .and. cmp < 0)) exit
            if (n == MAX_RANGE_ITEMS) then
                r = source
                ok = .true.
                return
            end if
            n = n + 1
            items(n) = fraction_expr(a, current)
            call add_fraction(current, step, next, good)
            if (good == 0) then
                r = source
                ok = .true.
                return
            end if
            current = next
        end do

        if (n == 0) then
            r = func_in(a, "List")
        else
            r = func("List", items(1:n))
        end if
        ok = .true.
    end subroutine build_exact_range

    subroutine build_real_range(a, source, lo, hi, step, r, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: source
        real(dp),                 intent(in)     :: lo, hi, step
        type(expr_t),              intent(out)   :: r
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t), allocatable :: items(:)
        real(dp) :: span, estimate, value
        integer :: n, k

        r = source
        ok = .false.
        why = ""
        if (step == 0.0_dp) then
            why = "Range has a zero step"
            return
        end if
        if ((step > 0.0_dp .and. lo > hi) .or. &
                (step < 0.0_dp .and. lo < hi)) then
            r = func_in(a, "List")
            ok = .true.
            return
        end if
        span = (hi - lo)/step
        estimate = floor(span + 1.0e-12_dp) + 1.0_dp
        if (estimate < 0.0_dp .or. estimate > real(MAX_RANGE_ITEMS, dp)) then
            r = source
            ok = .true.
            return
        end if
        n = int(estimate)
        allocate (items(n))
        do k = 1, n
            value = lo + real(k - 1, dp)*step
            if (abs(value - hi) <= 1.0e-12_dp*max(1.0_dp, abs(hi))) value = hi
            items(k) = real_expr(a, value)
        end do
        if (n == 0) then
            r = func_in(a, "List")
        else
            r = func("List", items)
        end if
        ok = .true.
    end subroutine build_real_range

    function fraction_of(e, f, ok) result(good)
        type(expr_t),     intent(in)  :: e
        type(fraction_t), intent(out) :: f
        logical,          intent(out) :: ok
        logical                       :: good

        f = fraction_of_expr(0_int64)
        ok = .false.
        good = .false.
        select case (e%kind())
        case (NK_INT)
            f%n = e%int_value()
            f%d = 1_int64
            ok = .true.
        case (NK_RAT)
            f%n = e%int_value()
            f%d = e%den_value()
            ok = f%d > 0_int64
        end select
        good = ok
    end function fraction_of

    function fraction_of_expr(n) result(f)
        integer(int64), intent(in) :: n
        type(fraction_t)             :: f
        f%n = n
        f%d = 1_int64
    end function fraction_of_expr

    function fraction_expr(a, f) result(e)
        type(arena_t), target, intent(inout) :: a
        type(fraction_t),      intent(in)    :: f
        type(expr_t)                         :: e
        if (f%d == 1_int64) then
            e = num(a, f%n)
        else
            e = rat(a, f%n, f%d)
        end if
    end function fraction_expr

    subroutine normalize_fraction(f)
        type(fraction_t), intent(inout) :: f
        integer(int64) :: g, rem

        if (f%n == 0_int64) then
            f%d = 1_int64
            return
        end if
        rem = modulo(f%n, f%d)
        if (rem < 0_int64) rem = -rem
        g = gcd_positive(rem, f%d)
        if (g > 1_int64) then
            f%n = f%n/g
            f%d = f%d/g
        end if
    end subroutine normalize_fraction

    subroutine compare_fraction(x, y, cmp, good)
        type(fraction_t), intent(in)  :: x, y
        integer,          intent(out) :: cmp
        integer,          intent(out) :: good
        integer(int64) :: left, right

        call checked_mul(x%n, y%d, left, good)
        if (good == 0) then
            cmp = 0
            return
        end if
        call checked_mul(y%n, x%d, right, good)
        if (good == 0) then
            cmp = 0
            return
        end if
        cmp = 0
        if (left < right) cmp = -1
        if (left > right) cmp = 1
    end subroutine compare_fraction

    subroutine add_fraction(x, y, z, good)
        type(fraction_t), intent(in)  :: x, y
        type(fraction_t), intent(out) :: z
        integer,          intent(out) :: good
        integer(int64) :: g, qx, qy, left, right, n, d

        good = 1
        g = gcd_positive(x%d, y%d)
        qx = x%d/g
        qy = y%d/g
        call checked_mul(x%n, qy, left, good)
        if (good == 0) return
        call checked_mul(y%n, qx, right, good)
        if (good == 0) return
        call checked_add(left, right, n, good)
        if (good == 0) return
        call checked_mul(qx, y%d, d, good)
        if (good == 0) return
        z%n = n
        z%d = d
        call normalize_fraction(z)
    end subroutine add_fraction

    function gcd_positive(x, y) result(g)
        integer(int64), intent(in) :: x, y
        integer(int64)              :: g, a0, b0, t
        a0 = x
        b0 = y
        do while (b0 /= 0_int64)
            t = modulo(a0, b0)
            a0 = b0
            b0 = t
        end do
        g = a0
        if (g < 1_int64) g = 1_int64
    end function gcd_positive

    subroutine checked_add(x, y, z, good)
        integer(int64), intent(in)  :: x, y
        integer(int64), intent(out) :: z
        integer,          intent(out) :: good
        good = 1
        if (y > 0_int64) then
            if (x > MAX_I64 - y) then
                good = 0
                z = 0_int64
                return
            end if
        else if (y < 0_int64) then
            if (x < MIN_I64 - y) then
                good = 0
                z = 0_int64
                return
            end if
        end if
        z = x + y
    end subroutine checked_add

    subroutine checked_mul(x, y, z, good)
        integer(int64), intent(in)  :: x, y
        integer(int64), intent(out) :: z
        integer,          intent(out) :: good
        good = 1
        if (x == 0_int64 .or. y == 0_int64) then
            z = 0_int64
            return
        end if
        if (x > 0_int64 .and. y > 0_int64) then
            if (x > MAX_I64/y) good = 0
        else if (x > 0_int64 .and. y < 0_int64) then
            if (y < MIN_I64/x) good = 0
        else if (x < 0_int64 .and. y > 0_int64) then
            if (x < MIN_I64/y) good = 0
        else
            if (x < MAX_I64/y) good = 0
        end if
        if (good == 0) then
            z = 0_int64
        else
            z = x*y
        end if
    end subroutine checked_mul

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
