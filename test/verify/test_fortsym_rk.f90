program test_fortsym_rk
    ! Order conditions on published Runge-Kutta pairs.
    !
    ! The oracle is Butcher's theory and the published literature, not this
    ! module's own output: Cash-Karp and Dormand-Prince are documented 5(4)
    ! pairs, so every order condition through five has to vanish for the main
    ! weights and through four for the embedded ones, and the fifth-order
    ! conditions have to fail for the embedded weights -- otherwise the pair
    ! would not be an estimator at all.
    !
    ! Rejection is tested by perturbing one published coefficient. A checker
    ! that accepted a corrupted tableau would be worthless, and a residual that
    ! merely got small would mean the arithmetic had stopped being exact.
    use, intrinsic :: iso_fortran_env, only: int64, dp => real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_rk, only: butcher_t, rk_from_rows, rk_is_consistent, &
        rk_attains_order, rk_error_weights, rk_is_fsal, rk_weight_sum, &
        rk_order_residuals, rk_order_residuals_into, rk_error_weights_into, rk_is_zero, &
        rk_order_residuals_real_into
    use fortsym_rk_trees, only: tree_table_t, build_rooted_trees, &
        tree_count_of_order, tree_gamma
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_tree_counts()
    call test_tree_gamma()
    call test_cash_karp()
    call test_dormand_prince()
    call test_corrupted_tableau_is_rejected()
    call test_classical_rk4()
    call test_zero_spellings()
    call test_real_order_residuals()

    write (*, "(a,i0,a,i0,a)") "test_fortsym_rk: ", n_pass, " passed, ", &
        n_fail, " failed"
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

    !> Sloane A000081: rooted trees by node count. An enumeration that drifts
    !> from these counts is either missing conditions or repeating them.
    subroutine test_tree_counts()
        type(tree_table_t) :: trees
        logical :: built
        integer, parameter :: expected(8) = [1, 1, 2, 4, 9, 20, 48, 115]
        integer :: r

        call build_rooted_trees(8, trees, built)
        call ok("rooted trees build to order 8", built)
        if (.not. built) return
        do r = 1, 8
            call ok("rooted tree count at order "//digit(r), &
                    tree_count_of_order(trees, r) == expected(r))
        end do
        call ok("total tree count to order 8", trees%n == sum(expected))
    end subroutine test_tree_counts

    !> gamma on the two order-3 trees is 6 and 3, the standard values behind
    !> the order-3 conditions sum(b c^2) = 1/3 and sum(b A c) = 1/6.
    subroutine test_tree_gamma()
        type(tree_table_t) :: trees
        logical :: built
        integer(int64) :: g
        integer :: k
        logical :: saw_three, saw_six

        call build_rooted_trees(3, trees, built)
        if (.not. built) then
            call ok("gamma fixture builds", .false.)
            return
        end if
        saw_three = .false.
        saw_six = .false.
        do k = 1, trees%n
            if (trees%order(k) /= 3) cycle
            g = tree_gamma(trees, k)
            if (g == 3_int64) saw_three = .true.
            if (g == 6_int64) saw_six = .true.
        end do
        call ok("order-3 trees have gamma 3 and 6", saw_three .and. saw_six)
    end subroutine test_tree_gamma

    subroutine cash_karp(tableau, built)
        type(butcher_t), intent(out) :: tableau
        logical, intent(out) :: built
        character(len=16) :: a_rows(15), b(6), c(6), bhat(6)

        a_rows = [character(len=16) :: &
             "1/5", &
             "3/40", "9/40", &
             "3/10", "-9/10", "6/5", &
             "-11/54", "5/2", "-70/27", "35/27", &
             "1631/55296", "175/512", "575/13824", "44275/110592", &
             "253/4096"]
        b = [character(len=16) :: "37/378", "0", "250/621", "125/594", "0", &
             "512/1771"]
        c = [character(len=16) :: "0", "1/5", "3/10", "3/5", "1", "7/8"]
        bhat = [character(len=16) :: "2825/27648", "0", "18575/48384", &
                "13525/55296", "277/14336", "1/4"]
        call rk_from_rows(6, a_rows, b, c, tableau, built, bhat)
    end subroutine cash_karp

    subroutine test_cash_karp()
        type(butcher_t) :: tableau
        logical :: built, ok_flag
        type(str_t), allocatable :: err(:)
        type(str_t) :: total

        call cash_karp(tableau, built)
        call ok("Cash-Karp tableau parses", built)
        if (.not. built) return

        call ok("Cash-Karp rows sum to their nodes", rk_is_consistent(tableau))
        call ok("Cash-Karp weights attain order 5", &
                rk_attains_order(tableau, tableau%b, 5))
        call ok("Cash-Karp embedded weights attain order 4", &
                rk_attains_order(tableau, tableau%bhat, 4))
        call ok("Cash-Karp embedded weights are not order 5", &
                .not. rk_attains_order(tableau, tableau%bhat, 5))
        call ok("Cash-Karp is not FSAL", .not. rk_is_fsal(tableau))

        total = rk_weight_sum(tableau%b, ok_flag)
        call ok("Cash-Karp weights sum to one", &
                ok_flag .and. chars(total) == "1")

        call rk_error_weights_into(tableau, err, ok_flag)
        call ok("Cash-Karp error weights derive", ok_flag)
        if (.not. ok_flag) return
        ! Published difference for stage 5, where b_5 is zero: -277/14336.
        call ok("Cash-Karp stage-5 error weight matches the literature", &
                chars(err(5)) == "-277/14336")
    end subroutine test_cash_karp

    subroutine test_dormand_prince()
        type(butcher_t) :: tableau
        logical :: built, ok_flag
        type(str_t), allocatable :: err(:)
        character(len=16) :: a_rows(21), b(7), c(7), bhat(7)

        a_rows = [character(len=16) :: &
             "1/5", &
             "3/40", "9/40", &
             "44/45", "-56/15", "32/9", &
             "19372/6561", "-25360/2187", "64448/6561", "-212/729", &
             "9017/3168", "-355/33", "46732/5247", "49/176", "-5103/18656", &
             "35/384", "0", "500/1113", "125/192", "-2187/6784", "11/84"]
        b = [character(len=16) :: "35/384", "0", "500/1113", "125/192", &
             "-2187/6784", "11/84", "0"]
        c = [character(len=16) :: "0", "1/5", "3/10", "4/5", "8/9", "1", "1"]
        bhat = [character(len=16) :: "5179/57600", "0", "7571/16695", &
                "393/640", "-92097/339200", "187/2100", "1/40"]
        call rk_from_rows(7, a_rows, b, c, tableau, built, bhat)
        call ok("Dormand-Prince tableau parses", built)
        if (.not. built) return

        call ok("Dormand-Prince rows sum to their nodes", &
                rk_is_consistent(tableau))
        call ok("Dormand-Prince weights attain order 5", &
                rk_attains_order(tableau, tableau%b, 5))
        call ok("Dormand-Prince embedded weights attain order 4", &
                rk_attains_order(tableau, tableau%bhat, 4))
        call ok("Dormand-Prince embedded weights are not order 5", &
                .not. rk_attains_order(tableau, tableau%bhat, 5))
        call ok("Dormand-Prince is FSAL", rk_is_fsal(tableau))

        call rk_error_weights_into(tableau, err, ok_flag)
        call ok("Dormand-Prince error weights derive", ok_flag)
        if (.not. ok_flag) return
        ! The published error weights, which fortnum previously carried as
        ! hand-subtracted literals.
        call ok("Dormand-Prince error weight 1", chars(err(1)) == "71/57600")
        call ok("Dormand-Prince error weight 3", chars(err(3)) == "-71/16695")
        call ok("Dormand-Prince error weight 4", chars(err(4)) == "71/1920")
        call ok("Dormand-Prince error weight 5", &
                chars(err(5)) == "-17253/339200")
        call ok("Dormand-Prince error weight 6", chars(err(6)) == "22/525")
        call ok("Dormand-Prince error weight 7", chars(err(7)) == "-1/40")
    end subroutine test_dormand_prince

    !> One digit of one published coefficient, changed. Order 5 must fail, and
    !> the residual must be a plainly non-zero rational rather than something
    !> small, which is what distinguishes exact arithmetic from a tolerance.
    subroutine test_corrupted_tableau_is_rejected()
        type(butcher_t) :: tableau
        logical :: built, ok_flag
        type(str_t), allocatable :: residuals(:)
        integer :: k
        logical :: any_nonzero
        character(len=16) :: a_rows(15), b(6), c(6)

        a_rows = [character(len=16) :: &
             "1/5", &
             "3/40", "9/40", &
             "3/10", "-9/10", "6/5", &
             "-11/54", "5/2", "-70/27", "35/27", &
             "1631/55296", "175/512", "575/13824", "44275/110592", &
             "253/4096"]
        b = [character(len=16) :: "37/379", "0", "250/621", "125/594", "0", &
             "512/1771"]
        c = [character(len=16) :: "0", "1/5", "3/10", "3/5", "1", "7/8"]
        call rk_from_rows(6, a_rows, b, c, tableau, built)
        call ok("corrupted tableau still parses", built)
        if (.not. built) return

        call ok("corrupted weights do not attain order 5", &
                .not. rk_attains_order(tableau, tableau%b, 5))
        call ok("corrupted weights do not even attain order 1", &
                .not. rk_attains_order(tableau, tableau%b, 1))

        call rk_order_residuals_into(tableau, tableau%b, 1, residuals, ok_flag)
        call ok("corrupted residuals compute", ok_flag)
        if (.not. ok_flag) return
        any_nonzero = .false.
        do k = 1, size(residuals)
            if (chars(residuals(k)) /= "0") any_nonzero = .true.
        end do
        call ok("corrupted residual is an exact non-zero rational", any_nonzero)
    end subroutine test_corrupted_tableau_is_rejected

    !> The classical fourth-order rule, as an independent check that the order
    !> machinery is not tuned to embedded 5(4) pairs.
    subroutine test_classical_rk4()
        type(butcher_t) :: tableau
        logical :: built
        character(len=16) :: a_rows(6), b(4), c(4)

        a_rows = [character(len=16) :: &
             "1/2", &
             "0", "1/2", &
             "0", "0", "1"]
        b = [character(len=16) :: "1/6", "1/3", "1/3", "1/6"]
        c = [character(len=16) :: "0", "1/2", "1/2", "1"]
        call rk_from_rows(4, a_rows, b, c, tableau, built)
        call ok("classical RK4 parses", built)
        if (.not. built) return

        call ok("classical RK4 is consistent", rk_is_consistent(tableau))
        call ok("classical RK4 attains order 4", &
                rk_attains_order(tableau, tableau%b, 4))
        call ok("classical RK4 does not attain order 5", &
                .not. rk_attains_order(tableau, tableau%b, 5))
    end subroutine test_classical_rk4

    !> Zero has more than one spelling, and consumers decide which stages a
    !> kernel takes by asking whether a weight vanishes. A predicate that only
    !> recognised the literal "0" would put a stage into an argument list on a
    !> platform whose FLINT renders zero some other way, so every spelling has
    !> to be recognised as the same value.
    subroutine test_zero_spellings()
        call ok("plain zero is zero", rk_is_zero(str("0")))
        call ok("signed zero is zero", rk_is_zero(str("-0")))
        call ok("zero over one is zero", rk_is_zero(str("0/1")))
        call ok("zero over many is zero", rk_is_zero(str("0/57600")))
        call ok("a non-zero rational is not zero", &
                .not. rk_is_zero(str("1/57600")))
        call ok("a tiny rational is not zero", &
                .not. rk_is_zero(str("1/1000000000000000000000")))
    end subroutine test_zero_spellings

    !> The real-arithmetic path has to agree with the exact one where both
    !> apply, and has to reject a perturbed coefficient. Classical RK4 is the
    !> fixture because its coefficients are exact in binary, so a correct
    !> tableau gives residuals at rounding level and nothing else.
    subroutine test_real_order_residuals()
        real(dp) :: a(4, 4), b(4)
        real(dp), allocatable :: residuals(:)
        logical :: got

        a = 0.0_dp
        a(2, 1) = 0.5_dp
        a(3, 2) = 0.5_dp
        a(4, 3) = 1.0_dp
        b = [1.0_dp/6.0_dp, 1.0_dp/3.0_dp, 1.0_dp/3.0_dp, 1.0_dp/6.0_dp]

        call rk_order_residuals_real_into(a, b, 4, residuals, got)
        call ok("real residuals compute", got)
        if (.not. got) return
        call ok("real residuals cover every tree to order 4", &
                size(residuals) == 8)
        call ok("classical RK4 satisfies order 4 at rounding level", &
                maxval(abs(residuals)) < 1.0e-15_dp)

        ! One coefficient moved well beyond rounding.
        a(4, 3) = 1.0_dp + 1.0e-6_dp
        call rk_order_residuals_real_into(a, b, 4, residuals, got)
        call ok("a perturbed coefficient shows up in the residuals", &
                got .and. maxval(abs(residuals)) > 1.0e-8_dp)
    end subroutine test_real_order_residuals

    pure function digit(value) result(text)
        integer, intent(in) :: value
        character(len=1) :: text
        write (text, "(i1)") value
    end function digit

end program test_fortsym_rk
