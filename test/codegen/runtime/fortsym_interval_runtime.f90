!> Reference real-interval runtime for rigorous kernels (doc/rigorous-runtime.md).
!>
!> An interval [lo, hi] contains the exact value. Endpoints are computed in
!> round-to-nearest and then moved outward by at least one ulp with the
!> branch-free successor/predecessor of Rump, Zimmermann, Boldo, Melquiond
!> (BIT 49, 2009), which needs no rounding-mode switch. Operations outside
!> their domain (division by an interval containing zero, square root of an
!> interval reaching below zero) return the whole real line. This is a test
!> fixture of the emitter; consumers plug in their own runtime.
module fortsym_interval_runtime
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    private
    public :: interval_t, iadd, isub, imul, idiv, ineg, iinv, isqrt, ipowi, iscale
    public :: ipoint, ienclose, ientire

    type :: interval_t
        real(dp) :: lo = 0.0_dp
        real(dp) :: hi = 0.0_dp
    end type interval_t

    real(dp), parameter :: u = epsilon(1.0_dp)/2.0_dp
    real(dp), parameter :: phi = u*(1.0_dp + 2.0_dp*u)
    real(dp), parameter :: etamin = tiny(1.0_dp)*epsilon(1.0_dp)

contains

    pure elemental function up(x) result(y)
        real(dp), intent(in) :: x
        real(dp) :: y

        y = x + (phi*abs(x) + etamin)
    end function up

    pure elemental function dn(x) result(y)
        real(dp), intent(in) :: x
        real(dp) :: y

        y = x - (phi*abs(x) + etamin)
    end function dn

    pure elemental function ientire() result(a)
        type(interval_t) :: a

        a%lo = -huge(1.0_dp)
        a%hi = huge(1.0_dp)
    end function ientire

    pure elemental function ipoint(x) result(a)
        real(dp), intent(in) :: x
        type(interval_t) :: a

        a%lo = x
        a%hi = x
    end function ipoint

    pure elemental function ienclose(m, r) result(a)
        real(dp), intent(in) :: m, r
        type(interval_t) :: a

        a%lo = dn(m - r)
        a%hi = up(m + r)
    end function ienclose

    pure elemental function ineg(a) result(b)
        type(interval_t), intent(in) :: a
        type(interval_t) :: b

        b%lo = -a%hi
        b%hi = -a%lo
    end function ineg

    pure elemental function iadd(a, b) result(s)
        type(interval_t), intent(in) :: a, b
        type(interval_t) :: s

        s%lo = dn(a%lo + b%lo)
        s%hi = up(a%hi + b%hi)
    end function iadd

    pure elemental function isub(a, b) result(s)
        type(interval_t), intent(in) :: a, b
        type(interval_t) :: s

        s%lo = dn(a%lo - b%hi)
        s%hi = up(a%hi - b%lo)
    end function isub

    pure elemental function imul(a, b) result(p)
        type(interval_t), intent(in) :: a, b
        type(interval_t) :: p
        real(dp) :: q(4)

        q = [a%lo*b%lo, a%lo*b%hi, a%hi*b%lo, a%hi*b%hi]
        p%lo = dn(minval(q))
        p%hi = up(maxval(q))
    end function imul

    pure elemental function iscale(a, x) result(p)
        type(interval_t), intent(in) :: a
        real(dp), intent(in) :: x
        type(interval_t) :: p

        if (x >= 0.0_dp) then
            p%lo = dn(a%lo*x)
            p%hi = up(a%hi*x)
        else
            p%lo = dn(a%hi*x)
            p%hi = up(a%lo*x)
        end if
    end function iscale

    pure elemental function iinv(a) result(q)
        type(interval_t), intent(in) :: a
        type(interval_t) :: q

        if (a%lo > 0.0_dp .or. a%hi < 0.0_dp) then
            q%lo = dn(1.0_dp/a%hi)
            q%hi = up(1.0_dp/a%lo)
        else
            q = ientire()
        end if
    end function iinv

    pure elemental function idiv(a, b) result(q)
        type(interval_t), intent(in) :: a, b
        type(interval_t) :: q

        if (b%lo > 0.0_dp .or. b%hi < 0.0_dp) then
            q = imul(a, iinv(b))
        else
            q = ientire()
        end if
    end function idiv

    pure elemental function isqrt(a) result(q)
        type(interval_t), intent(in) :: a
        type(interval_t) :: q

        if (a%lo < 0.0_dp) then
            q = ientire()
            return
        end if
        q%lo = max(dn(sqrt(a%lo)), 0.0_dp)
        q%hi = up(sqrt(a%hi))
    end function isqrt

    pure function pow_up(x, n) result(y)
        real(dp), intent(in) :: x
        integer, intent(in) :: n
        real(dp) :: y
        integer :: k

        y = 1.0_dp
        do k = 1, n
            y = up(y*x)
        end do
    end function pow_up

    pure function pow_dn(x, n) result(y)
        real(dp), intent(in) :: x
        integer, intent(in) :: n
        real(dp) :: y
        integer :: k

        y = 1.0_dp
        do k = 1, n
            y = max(dn(y*x), 0.0_dp)
        end do
    end function pow_dn

    !> a**n, n >= 2, from the monotonicity of t**n on each sign: an even power
    !> of an interval containing zero starts at zero, which repeated products
    !> cannot see.
    pure elemental function ipowi(a, n) result(p)
        type(interval_t), intent(in) :: a
        integer, intent(in) :: n
        type(interval_t) :: p

        if (mod(n, 2) == 0) then
            if (a%lo >= 0.0_dp) then
                p%lo = pow_dn(a%lo, n)
                p%hi = pow_up(a%hi, n)
            else if (a%hi <= 0.0_dp) then
                p%lo = pow_dn(-a%hi, n)
                p%hi = pow_up(-a%lo, n)
            else
                p%lo = 0.0_dp
                p%hi = pow_up(max(-a%lo, a%hi), n)
            end if
        else
            if (a%lo >= 0.0_dp) then
                p%lo = pow_dn(a%lo, n)
            else
                p%lo = -pow_up(-a%lo, n)
            end if
            if (a%hi >= 0.0_dp) then
                p%hi = pow_up(a%hi, n)
            else
                p%hi = -pow_dn(-a%hi, n)
            end if
        end if
    end function ipowi
end module fortsym_interval_runtime
