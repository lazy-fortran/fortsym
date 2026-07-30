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
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT
    use fortsym_expr, only: expr_t
    use fortsym_exact, only: exact_to_real
    implicit none
    private

    public :: binding_t, eval_expr, free_symbols_of

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

    !> Collect the distinct free symbol names of an expression.
    function free_symbols_of(e) result(names)
        type(expr_t), intent(in) :: e
        type(str_t), allocatable :: names(:)
        type(str_t), allocatable :: buffer(:)
        logical,     allocatable :: seen(:)
        integer :: n

        allocate (buffer(32))
        allocate (seen(e%a%size()), source=.false.)
        n = 0
        call gather(e%a, e%id, buffer, n, seen)
        allocate (names(n))
        if (n > 0) names(1:n) = buffer(1:n)
    end function free_symbols_of

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

        v = 0.0_dp
        if (.not. defined) return

        select case (a%kind_of(id))

        case (NK_INT)
            v = real(a%num_of(id), dp)

        case (NK_RAT)
            v = real(a%num_of(id), dp)/real(a%den_of(id), dp)

        case (NK_BIG_INT, NK_BIG_RAT)
            v = exact_to_real(chars(a%exact_text_of(id)), defined)

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

        if (name == "besselj") then
            x = ev(a, a%arg_of(id, 1), b, defined)
            if (.not. defined) return
            if (x /= real(nint(x), dp)) then
                defined = .false.
                return
            end if
            order = nint(x)
            y = ev(a, a%arg_of(id, 2), b, defined)
            if (.not. defined) return
            v = bessel_jn(order, y)
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

end module fortsym_eval
