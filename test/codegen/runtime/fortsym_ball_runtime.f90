!> Reference complex-ball runtime for rigorous kernels (doc/rigorous-runtime.md).
!>
!> A ball (c, r) is the closed disc |z - c| <= r. Every operation returns a
!> ball containing the exact result for all inputs in the argument balls.
!> Midpoints are computed in round-to-nearest; the rounding error of each IEEE
!> operation is bounded with |fl(x op y) - x op y| <= u |x op y| (u = 2**-53),
!> complex products by sqrt(5) u |a||b| (Brent, Percival, Zimmermann 2007),
!> and every radius sum is rounded upward. Underflow is covered by an
!> absolute floor eta added to every radius. The arithmetic follows the
!> kinetic-compression kc_ball module by the same author; it is a test fixture
!> of the emitter, not a library runtime (consumers plug in their own).
module fortsym_ball_runtime
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    private
    public :: ball_t, badd, bsub, bmul, bdiv, bneg, binv, bsqrt, bpowi, bscale
    public :: bpoint, bcpoint, benclose, bre_lo, bre_hi, bim_lo, bim_hi

    type :: ball_t
        complex(dp) :: c = (0.0_dp, 0.0_dp)
        real(dp) :: r = 0.0_dp
    end type ball_t

    real(dp), parameter :: u = epsilon(1.0_dp)/2.0_dp
    real(dp), parameter :: eta = 2.0_dp*tiny(1.0_dp)
    real(dp), parameter :: phi = u*(1.0_dp + 2.0_dp*u)
    real(dp), parameter :: etamin = tiny(1.0_dp)*epsilon(1.0_dp)
    real(dp), parameter :: safe_lo = 2.0_dp**(-500), safe_hi = 2.0_dp**500

contains

    !> Rump, Zimmermann, Boldo, Melquiond, BIT 49 (2009), Algorithm 2:
    !> fl(x + fl(phi |x| + eta)) >= succ(x) for finite x in round-to-nearest.
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

    pure elemental function cabs_hi(z) result(a)
        complex(dp), intent(in) :: z
        real(dp) :: a, x, y, m

        x = real(z, dp)
        y = aimag(z)
        m = max(abs(x), abs(y))
        if (m > safe_lo .and. m < safe_hi) then
            a = up(up(up(sqrt(x*x + y*y))))
        else
            a = up(up(abs(z)))
        end if
    end function cabs_hi

    pure elemental function cabs_lo(z) result(a)
        complex(dp), intent(in) :: z
        real(dp) :: a, x, y, m

        x = real(z, dp)
        y = aimag(z)
        m = max(abs(x), abs(y))
        if (m > safe_lo .and. m < safe_hi) then
            a = max(dn(dn(dn(sqrt(x*x + y*y)))), 0.0_dp)
        else
            a = max(dn(dn(abs(z))), 0.0_dp)
        end if
    end function cabs_lo

    pure elemental function bpoint(x) result(b)
        real(dp), intent(in) :: x
        type(ball_t) :: b

        b%c = cmplx(x, 0.0_dp, dp)
        b%r = 0.0_dp
    end function bpoint

    pure elemental function bcpoint(x, y) result(b)
        real(dp), intent(in) :: x, y
        type(ball_t) :: b

        b%c = cmplx(x, y, dp)
        b%r = 0.0_dp
    end function bcpoint

    pure elemental function benclose(m, r) result(b)
        real(dp), intent(in) :: m, r
        type(ball_t) :: b

        b%c = cmplx(m, 0.0_dp, dp)
        b%r = r
    end function benclose

    pure elemental function bneg(a) result(b)
        type(ball_t), intent(in) :: a
        type(ball_t) :: b

        b%c = -a%c
        b%r = a%r
    end function bneg

    pure elemental function badd(a, b) result(s)
        type(ball_t), intent(in) :: a, b
        type(ball_t) :: s

        s%c = a%c + b%c
        s%r = up(up(a%r + b%r) + up(2.0_dp*u*cabs_hi(s%c)) + eta)
    end function badd

    pure elemental function bsub(a, b) result(s)
        type(ball_t), intent(in) :: a, b
        type(ball_t) :: s

        s%c = a%c - b%c
        s%r = up(up(a%r + b%r) + up(2.0_dp*u*cabs_hi(s%c)) + eta)
    end function bsub

    !> (A + a)(B + b) - AB = A b + a B + a b.
    pure elemental function bmul(a, b) result(p)
        type(ball_t), intent(in) :: a, b
        type(ball_t) :: p
        real(dp) :: ma, mb, prop, rnd

        p%c = a%c*b%c
        ma = cabs_hi(a%c)
        mb = cabs_hi(b%c)
        prop = up(up(up(ma*b%r) + up(a%r*mb)) + up(a%r*b%r))
        rnd = up(3.0_dp*u*up(ma*mb))
        p%r = up(up(prop + rnd) + eta)
    end function bmul

    pure elemental function bscale(a, x) result(p)
        type(ball_t), intent(in) :: a
        real(dp), intent(in) :: x
        type(ball_t) :: p

        p%c = a%c*x
        p%r = up(up(up(a%r*abs(x)) + up(2.0_dp*u*cabs_hi(p%c))) + eta)
    end function bscale

    !> Exact image of the disc under z -> 1/z: centre conj(c)/(|c|^2 - r^2),
    !> radius r/(|c|^2 - r^2); the centre rounding is added to the radius.
    pure elemental function binv(a) result(q)
        type(ball_t), intent(in) :: a
        type(ball_t) :: q
        real(dp) :: den_lo, den_hi, ac_lo, ac_hi, cr

        ac_lo = cabs_lo(a%c)
        ac_hi = cabs_hi(a%c)
        den_lo = dn(dn(ac_lo*ac_lo) - up(a%r*a%r))
        den_hi = up(up(ac_hi*ac_hi) - dn(a%r*a%r))
        if (den_lo <= 0.0_dp) then
            q%c = (0.0_dp, 0.0_dp)
            q%r = huge(1.0_dp)
            return
        end if
        q%c = conjg(a%c)/(abs(a%c)**2 - a%r**2)
        cr = up(ac_hi*up(up(1.0_dp/den_lo) - dn(1.0_dp/den_hi)))
        cr = up(cr + up(8.0_dp*u*cabs_hi(q%c)))
        q%r = up(up(up(a%r/den_lo) + cr) + eta)
    end function binv

    pure elemental function bdiv(a, b) result(q)
        type(ball_t), intent(in) :: a, b
        type(ball_t) :: q

        q = bmul(a, binv(b))
    end function bdiv

    !> Principal square root of a ball with a positive real centre x:
    !> |sqrt z - sqrt x| = |z - x|/|sqrt z + sqrt x| <= r/sqrt x, because
    !> Re sqrt z >= 0 on the principal branch. Any other centre returns an
    !> infinite ball.
    pure elemental function bsqrt(a) result(q)
        type(ball_t), intent(in) :: a
        type(ball_t) :: q
        real(dp) :: x, s

        x = real(a%c, dp)
        if (aimag(a%c) /= 0.0_dp .or. .not. (x > 0.0_dp)) then
            q%c = (0.0_dp, 0.0_dp)
            q%r = huge(1.0_dp)
            return
        end if
        s = sqrt(x)
        q%c = cmplx(s, 0.0_dp, dp)
        q%r = up(up(up(a%r/dn(s)) + up(2.0_dp*u*s)) + eta)
    end function bsqrt

    !> Nonnegative x**n rounded upward or downward by repeated products.
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

    !> a**n, n >= 2. Propagation: |z**n - c**n| <= (|c| + r)**n - |c|**n,
    !> increasing in |c|, so bounded at the upper bound of |c|. Rounding of the
    !> binary-powering centre: relative error <= (1 + sqrt(5) u)**(n-1) - 1,
    !> bounded by 3.03 (n-1) u while n u < 1e-3. Larger n falls back to
    !> repeated ball products.
    pure elemental function bpowi(a, n) result(p)
        type(ball_t), intent(in) :: a
        integer, intent(in) :: n
        type(ball_t) :: p
        complex(dp) :: base, acc
        real(dp) :: hi, prop, rnd
        integer :: m, k

        if (n < 2 .or. real(n, dp)*u > 1.0e-3_dp) then
            p = bpoint(1.0_dp)
            do k = 1, max(n, 0)
                p = bmul(p, a)
            end do
            return
        end if
        acc = (1.0_dp, 0.0_dp)
        base = a%c
        m = n
        do while (m > 0)
            if (mod(m, 2) == 1) acc = acc*base
            m = m/2
            if (m > 0) base = base*base
        end do
        p%c = acc
        hi = cabs_hi(a%c)
        prop = 0.0_dp
        if (a%r > 0.0_dp) prop = up(pow_up(up(hi + a%r), n) - pow_dn(hi, n))
        rnd = up(up(3.03_dp*real(n - 1, dp)*u)*pow_up(hi, n))
        p%r = up(up(prop + rnd) + eta)
    end function bpowi

    pure elemental function bre_lo(a) result(x)
        type(ball_t), intent(in) :: a
        real(dp) :: x

        x = dn(real(a%c, dp) - a%r)
    end function bre_lo

    pure elemental function bre_hi(a) result(x)
        type(ball_t), intent(in) :: a
        real(dp) :: x

        x = up(real(a%c, dp) + a%r)
    end function bre_hi

    pure elemental function bim_lo(a) result(x)
        type(ball_t), intent(in) :: a
        real(dp) :: x

        x = dn(aimag(a%c) - a%r)
    end function bim_lo

    pure elemental function bim_hi(a) result(x)
        type(ball_t), intent(in) :: a
        real(dp) :: x

        x = up(aimag(a%c) + a%r)
    end function bim_hi
end module fortsym_ball_runtime
