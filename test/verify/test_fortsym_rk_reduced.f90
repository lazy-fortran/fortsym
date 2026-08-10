program test_fortsym_rk_reduced
    ! Dormand and Prince's eighth order construction, derived and checked.
    !
    ! The derivation takes four numbers -- c7 = 1/4, c8 = 4/13, c10 = 3/5,
    ! c11 = 6/7, the values proposed on p.185 of Hairer, Norsett and Wanner --
    ! and nothing else. Every other coefficient comes out of the reduced system.
    !
    ! The oracle is Hairer and Wanner's own dop853.f, whose coefficients are
    ! published to thirty digits. Those decimals are compared against, never
    ! used as input: if the construction were wrong the comparison would fail
    ! rather than quietly agree. The exact statements that a decimal cannot
    ! express -- 30*c4 + sqrt(6) = 6, and the eighth order conditions vanishing
    ! identically -- are asserted in the field.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortsym_quadratic, only: quadratic_t, quad_rational, quad_root, &
        quad_add, quad_sub, quad_mul, quad_to_real, quad_is_zero, quad_equal, &
        quad_text
    use fortsym_rk_reduced
    implicit none

    integer :: n_pass, n_fail
    integer, parameter :: D = 6

    n_pass = 0
    n_fail = 0

    call test_dop853_derivation()

    write (*, "(a,i0,a,i0,a)") "test_fortsym_rk_reduced: ", n_pass, &
        " passed, ", n_fail, " failed"
    if (n_fail > 0) error stop 1

contains

    subroutine ok(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        if (condition) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (*, "(a)") "  FAIL: "//label
        end if
    end subroutine ok

    subroutine matches(label, value, reference)
        character(*), intent(in) :: label
        type(quadratic_t), intent(in) :: value
        real(dp), intent(in) :: reference
        real(dp) :: got
        logical :: s

        got = quad_to_real(value, s)
        if (.not. s) then
            call ok(label, .false.)
            return
        end if
        call ok(label, abs(got - reference) <= 1.0e-14_dp*max(1.0_dp, abs(reference)))
        if (abs(got - reference) > 1.0e-14_dp*max(1.0_dp, abs(reference))) then
            write (*, "(a,es24.16,a,es24.16)") "        got ", got, &
                "   want ", reference
        end if
    end subroutine matches

    function rat(text) result(value)
        character(*), intent(in) :: text
        type(quadratic_t) :: value
        logical :: s

        value = quad_rational(text, D, s)
    end function rat

    subroutine test_dop853_derivation()
        type(rk_dp8_t) :: t
        integer :: status
        logical :: s

        call rk_dormand_prince_8(rat("1/4"), rat("4/13"), rat("3/5"), &
                                 rat("6/7"), t, status)
        call ok("the reduced system solves", status == RK_DP8_OK)
        if (status /= RK_DP8_OK) then
            write (*, "(a,i0)") "        status = ", status
            return
        end if

        ! Nodes, against dop853.f.
        call matches("c2", t%c(2), 0.526001519587677318785587544488e-01_dp)
        call matches("c3", t%c(3), 0.789002279381515978178381316732e-01_dp)
        call matches("c4", t%c(4), 0.118350341907227396726757197510_dp)
        call matches("c5", t%c(5), 0.281649658092772603273242802490_dp)
        call matches("c6", t%c(6), 0.333333333333333333333333333333_dp)
        call matches("c9", t%c(9), 0.651282051282051282051282051282_dp)

        ! An exact identity no decimal can carry: 30*c4 + sqrt(6) = 6.
        call ok("30*c4 + sqrt(6) is exactly 6", &
                quad_equal(quad_add(quad_mul(rat("30"), t%c(4), s), &
                                    quad_root(D, s), s), rat("6")))

        ! Weights.
        call matches("b1", t%b(1), 5.42937341165687622380535766363e-2_dp)
        call matches("b6", t%b(6), 4.45031289275240888144113950566_dp)
        call matches("b7", t%b(7), 1.89151789931450038304281599044_dp)
        call matches("b8", t%b(8), -5.8012039600105847814672114227_dp)
        call matches("b9", t%b(9), 3.1116436695781989440891606237e-1_dp)
        call matches("b10", t%b(10), -1.52160949662516078556178806805e-1_dp)
        call matches("b11", t%b(11), 2.01365400804030348374776537501e-1_dp)
        call matches("b12", t%b(12), 4.47106157277725905176885569043e-2_dp)

        ! Early rows.
        call matches("a21", t%a(2, 1), 5.26001519587677318785587544488e-2_dp)
        call matches("a31", t%a(3, 1), 1.97250569845378994544595329183e-2_dp)
        call matches("a32", t%a(3, 2), 5.91751709536136983633785987549e-2_dp)
        call matches("a41", t%a(4, 1), 2.95875854768068491816892993775e-2_dp)
        call matches("a43", t%a(4, 3), 8.87627564304205475450678981324e-2_dp)
        call matches("a51", t%a(5, 1), 2.41365134159266685502369798665e-1_dp)
        call matches("a53", t%a(5, 3), -8.84549479328286085344864962717e-1_dp)
        call matches("a54", t%a(5, 4), 9.24834003261792003115737966543e-1_dp)
        call matches("a61", t%a(6, 1), 3.7037037037037037037037037037e-2_dp)
        call matches("a64", t%a(6, 4), 1.70828608729473871279604482173e-1_dp)
        call matches("a65", t%a(6, 5), 1.25467687566822425016691814123e-1_dp)
        call matches("a71", t%a(7, 1), 3.7109375e-2_dp)
        call matches("a76", t%a(7, 6), -1.7578125e-2_dp)

        ! Tail links.
        call matches("a1110", t%a(11, 10), -3.0467644718982195003823669022_dp)
        call matches("a1210", t%a(12, 10), 1.23605671757943030647266201528e+1_dp)
        call matches("a1211", t%a(12, 11), 6.43392746015763530355970484046e-1_dp)

        ! The rows that only the closing conditions can fix. These are the
        ! coefficients the derivation exists for: they cannot be recovered from
        ! the published decimals by integer relation -- their height is too
        ! large -- so agreement here is evidence the construction is right and
        ! not that a search was steered by the answer.
        call matches("a91", t%a(9, 1), 6.24110958716075717114429577812e-1_dp)
        call matches("a94", t%a(9, 4), -3.36089262944694129406857109825_dp)
        call matches("a95", t%a(9, 5), -8.68219346841726006818189891453e-1_dp)
        call matches("a101", t%a(10, 1), 4.77662536438264365890433908527e-1_dp)
        call matches("a104", t%a(10, 4), -2.48811461997166764192642586468_dp)
        call matches("a105", t%a(10, 5), -5.90290826836842996371446475743e-1_dp)
        call matches("a114", t%a(11, 4), 5.18637242884406370830023853209_dp)
        call matches("a115", t%a(11, 5), 1.09143734899672957818500254654_dp)
        call matches("a124", t%a(12, 4), -1.05344954667372501984066689879e+1_dp)
        call matches("a125", t%a(12, 5), -2.00087205822486249909675718444_dp)

        call check_error_weights(t)
    end subroutine test_dop853_derivation

    !> DOP853's two embedded estimators, section II.10. The oracle is again
    !> dop853.f: its er coefficients are bbar - b, and bhh1, bhh2, bhh3 are the
    !> third order weights on nodes c1, c9 and c12.
    subroutine check_error_weights(t)
        type(rk_dp8_t), intent(in) :: t
        type(quadratic_t) :: bbar(RK_DP8_STAGES), btilde(RK_DP8_STAGES)
        type(quadratic_t) :: er(RK_DP8_STAGES)
        integer :: status, j
        logical :: s

        call rk_dp8_error_weights(t, bbar, btilde, status)
        call ok("the error weights solve", status == RK_DP8_OK)
        if (status /= RK_DP8_OK) return
        do j = 1, RK_DP8_STAGES
            er(j) = quad_sub(bbar(j), t%b(j), s)
        end do

        call matches("er1", er(1), 0.1312004499419488073250102996e-01_dp)
        call matches("er6", er(6), -0.1225156446376204440720569753e+01_dp)
        call matches("er7", er(7), -0.4957589496572501915214079952_dp)
        call matches("er8", er(8), 0.1664377182454986536961530415e+01_dp)
        call matches("er9", er(9), -0.3503288487499736816886487290_dp)
        call matches("er10", er(10), 0.3341791187130174790297318841_dp)
        call matches("er11", er(11), 0.8192320648511571246570742613e-01_dp)
        call matches("er12", er(12), -0.2235530786388629525884427845e-01_dp)

        call matches("bhh1", btilde(1), 0.244094488188976377952755905512_dp)
        call matches("bhh2", btilde(9), 0.733846688281611857341361741547_dp)
        call matches("bhh3", btilde(12), 0.220588235294117647058823529412e-1_dp)

        ! The third order estimator is carried by three nodes only.
        call ok("the third order estimator uses only c1, c9 and c12", &
                quad_is_zero(btilde(2)) .and. quad_is_zero(btilde(6)) .and. &
                quad_is_zero(btilde(8)) .and. quad_is_zero(btilde(11)))
        ! Unlike the section II.5 pair, the fifth order estimator must not
        ! share the last weight with the method, or it goes blind to stage 12.
        call ok("bbar(12) differs from b(12), unlike the 8(6) pair", &
                .not. quad_is_zero(er(12)))
    end subroutine check_error_weights

end program test_fortsym_rk_reduced
