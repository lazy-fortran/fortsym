module fortsym_eval
    ! Numeric evaluation of an expression, natively.
    !
    ! Written in Fortran rather than delegated to an engine for three reasons:
    ! it makes the numeric probe independent of whichever engine produced the
    ! expression, which is what makes the probe a real second opinion; it has no
    ! per-call process or conversion cost, so a thousand probe points is cheap;
    ! and it is the first piece of the native engine tracked in issue #11.
    !
    ! Undefined results are reported, not signalled. A probe deliberately
    ! evaluates at arbitrary points, so hitting a pole or a branch cut is an
    ! ordinary event: the caller needs to know the point was unusable and try
    ! another, not to have the program stop.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, &
        NK_BIG_REAL, NK_ALGEBRAIC
    use fortsym_expr, only: expr_t
    use fortsym_exact, only: exact_to_real
    implicit none
    private

    public :: binding_t, eval_expr, collect_free_symbols

    integer, parameter :: dp = real64

    real(dp), parameter :: PI_VALUE = &
        3.141592653589793238462643383279502884_dp
    real(dp), parameter :: E_VALUE = &
        2.718281828459045235360287471352662498_dp

    !> Values for the free symbols of an expression.
    type :: binding_t
        type(str_t), allocatable :: names(:)
        real(dp),    allocatable :: values(:)
        integer                  :: n = 0
    end type binding_t

contains

    !> Collect free symbols into caller-owned storage. The subroutine form is
    !> used on hot and diagnostic paths so an allocatable function result is
    !> never passed through an assumed-shape argument as a hidden temporary.
    subroutine collect_free_symbols(e, names)
        type(expr_t), intent(in) :: e
        type(str_t), allocatable, intent(out) :: names(:)
        type(str_t), allocatable :: buffer(:)
        logical,     allocatable :: seen(:)
        integer :: n

        allocate (buffer(32))
        allocate (seen(e%a%size()), source=.false.)
        n = 0
        call gather(e%a, e%id, buffer, n, seen)
        allocate (names(n))
        if (n > 0) names(1:n) = buffer(1:n)
    end subroutine collect_free_symbols

    recursive subroutine gather(a, id, buffer, n, seen)
        type(arena_t),            intent(in)    :: a
        integer,                  intent(in)    :: id
        type(str_t), allocatable, intent(inout) :: buffer(:)
        integer,                  intent(inout) :: n
        logical,                  intent(inout) :: seen(:)
        type(str_t), allocatable :: bigger(:)
        integer :: k

        if (seen(id)) return
        seen(id) = .true.

        if (a%kind_of(id) == NK_SYM) then
            if (n >= size(buffer)) then
                allocate (bigger(size(buffer)*2))
                bigger(1:n) = buffer(1:n)
                call move_alloc(bigger, buffer)
            end if
            n = n + 1
            buffer(n) = a%name_of(id)
            return
        end if

        do k = 1, a%nargs_of(id)
            call gather(a, a%arg_of(id, k), buffer, n, seen)
        end do
    end subroutine gather

    !> Evaluate. `defined` is false when the result is not a usable real number:
    !> a pole, a negative logarithm, an unbound symbol, or a function this
    !> evaluator does not implement.
    function eval_expr(e, b, defined) result(v)
        type(expr_t),    intent(in)  :: e
        type(binding_t), intent(in)  :: b
        logical,         intent(out) :: defined
        real(dp)                     :: v
        defined = .true.
        v = ev(e%a, e%id, b, defined)
    end function eval_expr

    recursive function ev(a, id, b, defined) result(v)
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(binding_t), intent(in)    :: b
        logical,         intent(inout) :: defined
        real(dp)                       :: v
        integer :: k
        real(dp) :: acc, base, expo
        character(:), allocatable :: text

        v = 0.0_dp
        if (.not. defined) return

        select case (a%kind_of(id))

        case (NK_INT)
            v = real(a%num_of(id), dp)

        case (NK_RAT)
            v = real(a%num_of(id), dp)/real(a%den_of(id), dp)

        case (NK_BIG_INT, NK_BIG_RAT)
            v = exact_to_real(chars(a%exact_text_of(id)), defined)

        case (NK_BIG_REAL)
            text = chars(a%real_text_of(id))
            read (text, *, iostat=k) v
            if (k /= 0) defined = .false.

        case (NK_ALGEBRAIC)
            ! A real64 probe must not project an exact complex or algebraic
            ! value without an explicit numerical policy.
            defined = .false.

        case (NK_REAL)
            v = a%real_of(id)

        case (NK_SYM)
            v = lookup(b, chars(a%name_of(id)), defined)

        case (NK_CONST)
            select case (chars(a%name_of(id)))
            case ("pi"); v = PI_VALUE
            case ("e");  v = E_VALUE
            case default
                ! The imaginary unit has no real value; a real probe cannot
                ! evaluate here and must say so rather than invent one.
                defined = .false.
            end select

        case (NK_ADD)
            acc = 0.0_dp
            do k = 1, a%nargs_of(id)
                acc = acc + ev(a, a%arg_of(id, k), b, defined)
                if (.not. defined) return
            end do
            v = acc

        case (NK_MUL)
            acc = 1.0_dp
            do k = 1, a%nargs_of(id)
                acc = acc*ev(a, a%arg_of(id, k), b, defined)
                if (.not. defined) return
            end do
            v = acc

        case (NK_POW)
            base = ev(a, a%arg_of(id, 1), b, defined)
            if (.not. defined) return
            expo = ev(a, a%arg_of(id, 2), b, defined)
            if (.not. defined) return
            v = power(base, expo, defined)

        case (NK_FUNC)
            v = apply(a, id, b, defined)

        case default
            defined = .false.
        end select

        if (defined) then
            ! Overflow and invalid operations produce values that would poison
            ! any comparison downstream, so they are reported as undefined here.
            if (v /= v) defined = .false.
            if (abs(v) > huge(1.0_dp)/2) defined = .false.
        end if
    end function ev

    function lookup(b, name, defined) result(v)
        type(binding_t), intent(in)    :: b
        character(*),    intent(in)    :: name
        logical,         intent(inout) :: defined
        real(dp)                       :: v
        integer :: k

        v = 0.0_dp
        do k = 1, b%n
            if (len(chars(b%names(k))) == len(name)) then
                if (chars(b%names(k)) == name) then
                    v = b%values(k)
                    return
                end if
            end if
        end do
        defined = .false.
    end function lookup

    !> Real powers, with the cases that have no real value reported rather than
    !> silently producing a NaN.
    function power(base, expo, defined) result(v)
        real(dp), intent(in)    :: base, expo
        logical,  intent(inout) :: defined
        real(dp)                :: v
        integer :: n

        v = 0.0_dp

        if (base == 0.0_dp .and. expo < 0.0_dp) then
            defined = .false. ! pole
            return
        end if

        if (expo == nint(expo) .and. abs(expo) < 1.0e6_dp) then
            ! An integer exponent is valid for a negative base, and the integer
            ! form is both faster and exact for small powers.
            n = nint(expo)
            v = base**n
            return
        end if

        if (base < 0.0_dp) then
            defined = .false. ! non-integer power of a negative number
            return
        end if

        v = base**expo
    end function power

    recursive function apply(a, id, b, defined) result(v)
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(binding_t), intent(in)    :: b
        logical,         intent(inout) :: defined
        real(dp)                       :: v
        real(dp) :: x, y
        integer :: order
        character(:), allocatable :: name

        v = 0.0_dp
        name = chars(a%name_of(id))

        if (name == "besselj" .or. name == "besseli" .or. name == "besselk") then
            x = ev(a, a%arg_of(id, 1), b, defined)
            if (.not. defined) return
            if (x /= real(nint(x), dp)) then
                defined = .false.
                return
            end if
            order = nint(x)
            y = ev(a, a%arg_of(id, 2), b, defined)
            if (.not. defined) return
            if (name == "besselj") then
                v = bessel_jn(order, y)
            else if (name == "besseli") then
                v = modified_bessel_i(order, y)
            else
                v = modified_bessel_k(order, y, defined)
            end if
            return
        end if

        x = ev(a, a%arg_of(id, 1), b, defined)
        if (.not. defined) return

        if (name == "atan2") then
            y = ev(a, a%arg_of(id, 2), b, defined)
            if (.not. defined) return
            if (x == 0.0_dp .and. y == 0.0_dp) then
                defined = .false.
                return
            end if
            v = atan2(x, y)
            return
        end if

        select case (name)
        case ("sin");  v = sin(x)
        case ("cos");  v = cos(x)
        case ("tan")
            if (abs(cos(x)) < 1.0e-12_dp) then
                defined = .false.
            else
                v = tan(x)
            end if
        case ("asin")
            if (abs(x) > 1.0_dp) then
                defined = .false.
            else
                v = asin(x)
            end if
        case ("acos")
            if (abs(x) > 1.0_dp) then
                defined = .false.
            else
                v = acos(x)
            end if
        case ("atan");  v = atan(x)
        case ("sinh");  v = sinh(x)
        case ("cosh");  v = cosh(x)
        case ("tanh");  v = tanh(x)
        case ("asinh"); v = asinh(x)
        case ("acosh")
            if (x < 1.0_dp) then
                defined = .false.
            else
                v = acosh(x)
            end if
        case ("atanh")
            if (abs(x) >= 1.0_dp) then
                defined = .false.
            else
                v = atanh(x)
            end if
        case ("exp");   v = exp(x)
        case ("log")
            if (x <= 0.0_dp) then
                defined = .false.
            else
                v = log(x)
            end if
        case ("sqrt")
            if (x < 0.0_dp) then
                defined = .false.
            else
                v = sqrt(x)
            end if
        case ("abs");   v = abs(x)
        case ("erf");   v = erf(x)
        case ("erfc");  v = erfc(x)
        case ("gamma")
            if (x <= 0.0_dp) then
                defined = .false.
            else
                v = gamma(x)
            end if
        case ("loggamma")
            if (x <= 0.0_dp) then
                defined = .false.
            else
                v = log_gamma(x)
            end if
        case default
            ! An unknown head has no numeric value. Guessing one would make the
            ! probe agree with anything.
            defined = .false.
        end select
    end function apply

    !> Real integer-order modified Bessel I_n, evaluated without relying on a
    !> compiler-specific intrinsic. The series is stable for the moderate
    !> arguments used by the evaluator and preserves I_{-n}=I_n.
    pure function modified_bessel_i(order, x) result(value)
        integer, intent(in) :: order
        real(dp), intent(in) :: x
        real(dp) :: value, term, positive_x
        integer :: n, k

        n = abs(order)
        positive_x = abs(x)
        term = (positive_x/2.0_dp)**n
        do k = 1, n
            term = term/real(k, dp)
        end do
        value = term
        do k = 0, 200
            term = term*(positive_x*positive_x/4.0_dp)/ &
                (real(k + 1, dp)*real(k + n + 1, dp))
            value = value + term
            if (abs(term) < 1.0e-16_dp*max(1.0_dp, abs(value))) exit
        end do
        if (x < 0.0_dp .and. modulo(n, 2) == 1) value = -value
    end function modified_bessel_i

    function modified_bessel_k(order, x, defined) result(value)
        ! K_n(x) = integral_0^infinity exp(-x cosh(t)) cosh(n t) dt.
        ! This conservative quadrature is for real positive probes only; the
        ! symbolic expression remains available outside that evaluator domain.
        integer, intent(in) :: order
        real(dp), intent(in) :: x
        logical, intent(inout) :: defined
        real(dp) :: value, h, t, weight, term, k0, k1, next
        integer, parameter :: nsteps = 4096
        integer :: k, n, recurrence_order

        value = 0.0_dp
        if (x <= 0.0_dp) then
            defined = .false.
            return
        end if
        n = abs(order)
        if (x <= 2.0_dp) then
            call modified_bessel_k01_series(x, k0, k1)
            if (n == 0) then
                value = k0
                return
            end if
            do recurrence_order = 1, n - 1
                next = k0 + 2.0_dp*real(recurrence_order, dp)*k1/x
                k0 = k1
                k1 = next
            end do
            value = k1
            return
        end if
        h = 12.0_dp/real(nsteps, dp)
        do k = 0, nsteps
            t = real(k, dp)*h
            term = exp(-x*cosh(t))*cosh(real(n, dp)*t)
            if (k == 0 .or. k == nsteps) then
                weight = 1.0_dp
            else if (mod(k, 2) == 0) then
                weight = 2.0_dp
            else
                weight = 4.0_dp
            end if
            value = value + weight*term
        end do
        value = value*h/3.0_dp
    end function modified_bessel_k

    !> Convergent DLMF 10.31.1--2 series for K_0 and K_1.  The previous
    !> fixed-range integral silently truncated K_n(x) when x was very small:
    !> its significant t range grows like log(1/x), far beyond t=12.
    pure subroutine modified_bessel_k01_series(x, k0, k1)
        real(dp), intent(in) :: x
        real(dp), intent(out) :: k0, k1
        real(dp), parameter :: euler_gamma = &
            0.577215664901532860606512090082402431_dp
        real(dp) :: q, term, harmonic, i0, i1, s0, ds0, scale
        integer :: k

        q = x*x/4.0_dp
        term = 1.0_dp
        harmonic = 0.0_dp
        i0 = 1.0_dp
        i1 = 0.0_dp
        s0 = 0.0_dp
        ds0 = 0.0_dp
        do k = 1, 200
            term = term*q/real(k*k, dp)
            harmonic = harmonic + 1.0_dp/real(k, dp)
            i0 = i0 + term
            i1 = i1 + 2.0_dp*real(k, dp)*term/x
            s0 = s0 + harmonic*term
            ds0 = ds0 + 2.0_dp*real(k, dp)*harmonic*term/x
            scale = max(1.0_dp, abs(i0), abs(s0))
            if (abs(term) < epsilon(1.0_dp)*scale) exit
        end do
        scale = log(x/2.0_dp) + euler_gamma
        k0 = -scale*i0 + s0
        k1 = i0/x + scale*i1 - ds0
    end subroutine modified_bessel_k01_series

end module fortsym_eval
