program test_fortsym_algebraic
    ! Independent oracles are derived minimal polynomials plus FLINT's
    ! documented canonical root order. Arithmetic checks also reconstruct
    ! rational norms and defining equations rather than echoing bridge output.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str_t, chars
    use fortsym_algebraic, only: algebraic_normalize, algebraic_i, &
        algebraic_from_re_im, algebraic_add, algebraic_sub, algebraic_mul, &
        algebraic_div, algebraic_conjugate, algebraic_sqrt, algebraic_pow, &
        algebraic_signs, algebraic_re, algebraic_im, algebraic_to_real
    implicit none

    integer :: nfail = 0
    integer :: real_sign, imag_sign
    logical :: ok
    real(real64) :: real_value
    type(str_t) :: zero, one, two, minus_two, imaginary, minus_imaginary
    type(str_t) :: sqrt_two, z, zbar, got, phi, real_part, imag_part

    zero = algebraic_from_re_im("0", "0", ok)
    call check_result("rational zero", zero, ok, "qqbar1:0:0,1")
    one = algebraic_from_re_im("1", "0", ok)
    call check_result("rational one", one, ok, "qqbar1:0:-1,1")
    two = algebraic_from_re_im("2", "0", ok)
    call check_result("rational two", two, ok, "qqbar1:0:-2,1")
    minus_two = algebraic_from_re_im("-2", "0", ok)
    call check_result("negative rational", minus_two, ok, "qqbar1:0:2,1")

    imaginary = algebraic_i(ok)
    call check_result("imaginary unit", imaginary, ok, "qqbar1:0:1,0,1")
    real_part = algebraic_re(chars(imaginary), ok)
    call check_result("imaginary unit real projection", real_part, ok, &
        "qqbar1:0:0,1")
    imag_part = algebraic_im(chars(imaginary), ok)
    call check_result("imaginary unit imaginary projection", imag_part, ok, &
        "qqbar1:0:-1,1")
    minus_imaginary = algebraic_conjugate(chars(imaginary), ok)
    call check_result("negative imaginary unit", minus_imaginary, ok, &
        "qqbar1:1:1,0,1")
    got = algebraic_add(chars(imaginary), chars(minus_imaginary), ok)
    call check_result("i plus conjugate is zero", got, ok, "qqbar1:0:0,1")

    ! x^2 - 2 has real roots in descending order, so principal sqrt(2)
    ! is root zero and its negative is root one.
    sqrt_two = algebraic_sqrt(chars(two), ok)
    call check_result("principal square root of two", sqrt_two, ok, &
        "qqbar1:0:-2,0,1")
    call algebraic_signs(chars(sqrt_two), real_sign, imag_sign, ok)
    call check_condition("sqrt(2) has positive-real branch", &
        ok .and. real_sign == 1 .and. imag_sign == 0)
    real_value = algebraic_to_real(chars(sqrt_two), ok)
    call check_condition("sqrt(2) has a checked binary64 projection", &
        ok .and. real_value == 1.4142135623730951_real64)
    got = algebraic_pow(chars(sqrt_two), 2_int64, ok)
    call check_result("sqrt(2) squared reconstructs two", got, ok, &
        "qqbar1:0:-2,1")
    got = algebraic_normalize("qqbar1:1:-2,0,1", ok)
    call check_result("negative root index is lossless", got, ok, &
        "qqbar1:1:-2,0,1")

    ! The principal square root of -2 lies in the upper half-plane. Nonreal
    ! conjugates with equal real/absolute-imaginary parts put that root first.
    got = algebraic_sqrt(chars(minus_two), ok)
    call check_result("principal complex square root", got, ok, &
        "qqbar1:0:2,0,1")
    call algebraic_signs(chars(got), real_sign, imag_sign, ok)
    call check_condition("sqrt(-2) has upper-half-plane branch", &
        ok .and. real_sign == 0 .and. imag_sign == 1)
    got = algebraic_conjugate(chars(got), ok)
    call check_result("complex square-root conjugate", got, ok, &
        "qqbar1:1:2,0,1")
    call algebraic_signs(chars(got), real_sign, imag_sign, ok)
    call check_condition("conjugated sqrt(-2) is in lower half-plane", &
        ok .and. real_sign == 0 .and. imag_sign == -1)
    real_value = algebraic_to_real(chars(got), ok)
    call check_condition("non-real algebraic projection is refused", .not. ok)

    ! For z = 1/2 + 3i/4, clearing denominators from
    ! (x-1/2)^2 + (3/4)^2 gives 16*x^2 - 16*x + 13.
    z = algebraic_from_re_im("1/2", "3/4", ok)
    call check_result("Gaussian rational construction", z, ok, &
        "qqbar1:0:13,-16,16")
    real_part = algebraic_re(chars(z), ok)
    call check_result("Gaussian real projection", real_part, ok, &
        "qqbar1:0:-1,2")
    imag_part = algebraic_im(chars(z), ok)
    call check_result("Gaussian imaginary projection", imag_part, ok, &
        "qqbar1:0:-3,4")
    zbar = algebraic_conjugate(chars(z), ok)
    call check_result("Gaussian rational conjugate", zbar, ok, &
        "qqbar1:1:13,-16,16")
    got = algebraic_add(chars(z), chars(zbar), ok)
    call check_result("Gaussian trace is one", got, ok, "qqbar1:0:-1,1")
    got = algebraic_mul(chars(z), chars(zbar), ok)
    call check_result("Gaussian norm is 13/16", got, ok, &
        "qqbar1:0:-13,16")

    got = algebraic_from_re_im("1", "1", ok)
    real_part = algebraic_re(chars(got), ok)
    call check_result("mixed real projection", real_part, ok, "qqbar1:0:-1,1")
    imag_part = algebraic_im(chars(got), ok)
    call check_result("mixed imaginary projection", imag_part, ok, &
        "qqbar1:0:-1,1")
    got = algebraic_pow(chars(got), 2_int64, ok)
    call check_result("(1+i)^2 is 2i", got, ok, "qqbar1:0:4,0,1")

    ! phi = (1 + sqrt(5))/2 is the descending real root of x^2-x-1.
    got = algebraic_from_re_im("5", "0", ok)
    got = algebraic_sqrt(chars(got), ok)
    got = algebraic_add(chars(one), chars(got), ok)
    phi = algebraic_div(chars(got), chars(two), ok)
    call check_result("golden ratio minimal polynomial", phi, ok, &
        "qqbar1:0:-1,-1,1")
    got = algebraic_mul(chars(phi), chars(phi), ok)
    got = algebraic_sub(chars(got), chars(phi), ok)
    call check_result("phi squared minus phi reconstructs one", got, ok, &
        "qqbar1:0:-1,1")

    ! (x^2-2)(x-3) has sorted real roots 3, sqrt(2), -sqrt(2).
    ! Normalization must discard the reducible input polynomial.
    got = algebraic_normalize("qqbar1:1:6,-2,-3,1", ok)
    call check_result("reducible serialization normalizes", got, ok, &
        "qqbar1:0:-2,0,1")
    got = algebraic_normalize("qqbar1:1:1,-2,1", ok)
    call check_result("repeated root normalizes", got, ok, "qqbar1:0:-1,1")

    got = algebraic_div(chars(one), chars(zero), ok)
    call expect_failure("division by zero is refused", got, ok)
    got = algebraic_pow(chars(zero), -1_int64, ok)
    call expect_failure("negative power of zero is refused", got, ok)
    got = algebraic_pow(chars(two), 65_int64, ok)
    call expect_failure("oversized algebraic power is refused", got, ok)
    got = algebraic_normalize("qqbar1:2:1,0,1", ok)
    call expect_failure("out-of-range root index is refused", got, ok)
    got = algebraic_normalize("qqbar1:0:1,0,0", ok)
    call expect_failure("trailing zero polynomial is refused", got, ok)
    got = algebraic_normalize("qqbar1:0:not-an-integer,1", ok)
    call expect_failure("malformed coefficient is refused", got, ok)
    got = algebraic_normalize("qqbar1:0:1"//achar(0)//",1", ok)
    call expect_failure("embedded NUL is refused", got, ok)
    got = algebraic_normalize( &
        "qqbar1:0:-2,"//repeat("0,", 32)//"1", ok)
    call expect_failure("degree above the local bound is refused", got, ok)
    got = algebraic_normalize( &
        "qqbar1:0:"//"1"//repeat("0", 1300)//",1", ok)
    call expect_failure("height above the local bound is refused", got, ok)
    got = algebraic_normalize(repeat("1", 64*1024 + 1), ok)
    call expect_failure("oversized serialization is refused", got, ok)

    if (nfail /= 0) error stop 1
    print *, "test_fortsym_algebraic: all checks passed"

contains

    subroutine check_condition(label, condition)
        character(*), intent(in) :: label
        logical,      intent(in) :: condition

        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine check_condition

    subroutine check_result(label, value, succeeded, expected)
        character(*), intent(in) :: label, expected
        type(str_t),   intent(in) :: value
        logical,       intent(in) :: succeeded

        if (.not. succeeded .or. chars(value) /= expected) then
            nfail = nfail + 1
            print *, "FAIL ", label
            print *, "  got:  ", chars(value)
            print *, "  want: ", expected
        end if
    end subroutine check_result

    subroutine expect_failure(label, value, succeeded)
        character(*), intent(in) :: label
        type(str_t),   intent(in) :: value
        logical,       intent(in) :: succeeded

        if (succeeded .or. len(chars(value)) /= 0) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine expect_failure

end program test_fortsym_algebraic
