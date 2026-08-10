module fortsym_rk
    !> Butcher tableaux as exact data, and the order conditions that decide
    !> whether one is the method it claims to be.
    !>
    !> A tableau here is never a set of decimal constants. Every coefficient is
    !> an exact rational carried through FLINT, so a residual that should be
    !> zero is exactly zero and not something near it. That is what makes the
    !> order conditions a proof rather than a tolerance check.
    !>
    !> Zero is tested with rk_is_zero rather than by comparing against the
    !> literal "0". The canonical spelling a value renders to is FLINT's to
    !> choose, so a build linked against a different FLINT can spell the same
    !> zero differently, and a literal comparison then silently misclassifies
    !> it. Every zero test here goes through one renderer on both sides.
    !>
    !> The intended use is generation: state a tableau once, verify it, then
    !> derive the stage expressions, the embedded error weights and the FSAL
    !> property from it, instead of transcribing any of them by hand.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_exact, only: exact_add, exact_sub, exact_mul, exact_div, &
        exact_normalize
    use fortsym_rk_trees, only: tree_table_t, build_rooted_trees, tree_gamma
    implicit none
    private

    public :: butcher_t, rk_from_rows, rk_stages
    public :: rk_row_sum_residuals, rk_is_consistent
    public :: rk_order_residuals, rk_attains_order
    public :: rk_error_weights, rk_is_fsal
    public :: rk_weight_sum, rk_is_zero
    public :: rk_order_residuals_real

    integer, parameter :: dp = real64

    type :: butcher_t
        !> Number of stages.
        integer :: stages = 0
        !> Coupling coefficients, exact rationals. a(i, j) is used by stage i
        !> against stage j, and is "0" wherever the method does not couple.
        type(str_t), allocatable :: a(:, :)
        !> Solution weights.
        type(str_t), allocatable :: b(:)
        !> Embedded weights. Unallocated when the method has no embedded pair.
        type(str_t), allocatable :: bhat(:)
        !> Nodes.
        type(str_t), allocatable :: c(:)
    end type butcher_t

contains

    !> Build a tableau from exact rational text.
    !>
    !> `a_rows` is the strictly lower triangle read row by row: stage 2's one
    !> coefficient, then stage 3's two, and so on. Explicit methods have no
    !> other non-zero entries, and writing only the triangle keeps the caller
    !> from having to spell out the zeros.
    !>
    !> Every string is normalised through FLINT on the way in, so "3/40",
    !> "0.075" and "6/80" become the same stored value and a malformed
    !> coefficient is rejected here rather than surfacing later as a wrong
    !> residual.
    subroutine rk_from_rows(stages, a_rows, b, c, tableau, ok, bhat)
        integer, intent(in) :: stages
        character(*), intent(in) :: a_rows(:), b(:), c(:)
        type(butcher_t), intent(out) :: tableau
        logical, intent(out) :: ok
        character(*), intent(in), optional :: bhat(:)

        integer :: i, j, cursor
        logical :: each_ok

        ok = .false.
        if (stages < 1) return
        if (size(b) /= stages .or. size(c) /= stages) return
        if (size(a_rows) /= (stages*(stages - 1))/2) return
        if (present(bhat)) then
            if (size(bhat) /= stages) return
        end if

        tableau%stages = stages
        allocate (tableau%a(stages, stages), tableau%b(stages), &
                  tableau%c(stages))
        do j = 1, stages
            do i = 1, stages
                tableau%a(i, j) = str("0")
            end do
        end do

        cursor = 0
        do i = 2, stages
            do j = 1, i - 1
                cursor = cursor + 1
                tableau%a(i, j) = exact_normalize(a_rows(cursor), each_ok)
                if (.not. each_ok) return
            end do
        end do

        do i = 1, stages
            tableau%b(i) = exact_normalize(b(i), each_ok)
            if (.not. each_ok) return
            tableau%c(i) = exact_normalize(c(i), each_ok)
            if (.not. each_ok) return
        end do

        if (present(bhat)) then
            allocate (tableau%bhat(stages))
            do i = 1, stages
                tableau%bhat(i) = exact_normalize(bhat(i), each_ok)
                if (.not. each_ok) return
            end do
        end if

        ok = .true.
    end subroutine rk_from_rows

    pure function rk_stages(tableau) result(stages)
        type(butcher_t), intent(in) :: tableau
        integer :: stages
        stages = tableau%stages
    end function rk_stages

    !> True when this exact value is zero.
    !>
    !> Both sides are normalised through the same renderer, so this does not
    !> depend on how FLINT chooses to spell a canonical zero.
    function rk_is_zero(value) result(is_zero)
        type(str_t), intent(in) :: value
        logical :: is_zero
        type(str_t) :: canonical, zero
        logical :: left_ok, right_ok

        canonical = exact_normalize(chars(value), left_ok)
        zero = exact_normalize("0", right_ok)
        is_zero = left_ok .and. right_ok .and. &
                  chars(canonical) == chars(zero)
    end function rk_is_zero

    !> sum_j a(i,j) - c(i) for every stage.
    !>
    !> Zero everywhere is the consistency condition. It is the cheapest check
    !> that catches a mistyped coupling coefficient, and it does not depend on
    !> the claimed order.
    function rk_row_sum_residuals(tableau, ok) result(residuals)
        type(butcher_t), intent(in) :: tableau
        logical, intent(out) :: ok
        type(str_t), allocatable :: residuals(:)

        integer :: i, j
        type(str_t) :: row
        logical :: each_ok

        allocate (residuals(tableau%stages))
        ok = .true.
        do i = 1, tableau%stages
            row = str("0")
            do j = 1, tableau%stages
                row = exact_add(chars(row), chars(tableau%a(i, j)), each_ok)
                if (.not. each_ok) then
                    ok = .false.
                    return
                end if
            end do
            residuals(i) = exact_sub(chars(row), chars(tableau%c(i)), each_ok)
            if (.not. each_ok) then
                ok = .false.
                return
            end if
        end do
    end function rk_row_sum_residuals

    function rk_is_consistent(tableau) result(consistent)
        type(butcher_t), intent(in) :: tableau
        logical :: consistent
        type(str_t), allocatable :: residuals(:)
        logical :: ok
        integer :: i

        residuals = rk_row_sum_residuals(tableau, ok)
        consistent = ok
        if (.not. ok) return
        do i = 1, size(residuals)
            if (.not. rk_is_zero(residuals(i))) consistent = .false.
        end do
    end function rk_is_consistent

    !> sum_i w_i - 1, for either weight vector. Order 1 in one line.
    function rk_weight_sum(weights, ok) result(total)
        type(str_t), intent(in) :: weights(:)
        logical, intent(out) :: ok
        type(str_t) :: total
        integer :: i

        total = str("0")
        ok = .true.
        do i = 1, size(weights)
            total = exact_add(chars(total), chars(weights(i)), ok)
            if (.not. ok) return
        end do
    end function rk_weight_sum

    !> The order-condition residuals for every rooted tree up to `order`.
    !>
    !> Entry k is sum_i w_i Phi_i(t_k) - 1/gamma(t_k) for the k-th tree in the
    !> canonical enumeration. The method attains the order exactly when every
    !> entry is zero.
    !>
    !> `weights` selects which solution is being tested, so the same routine
    !> checks the main weights at order p and the embedded ones at p-1.
    function rk_order_residuals(tableau, weights, order, ok) result(residuals)
        type(butcher_t), intent(in) :: tableau
        type(str_t), intent(in) :: weights(:)
        integer, intent(in) :: order
        logical, intent(out) :: ok
        type(str_t), allocatable :: residuals(:)

        type(tree_table_t) :: trees
        type(str_t), allocatable :: phi(:)
        type(str_t) :: total, target_value
        integer :: k, i
        integer(int64) :: gamma
        logical :: each_ok
        character(len=32) :: gamma_text

        allocate (residuals(0))
        call build_rooted_trees(order, trees, ok)
        if (.not. ok) return

        allocate (phi(tableau%stages))
        deallocate (residuals)
        allocate (residuals(trees%n))

        do k = 1, trees%n
            do i = 1, tableau%stages
                phi(i) = tree_phi(tableau, trees, k, i, each_ok)
                if (.not. each_ok) then
                    ok = .false.
                    return
                end if
            end do

            total = str("0")
            do i = 1, tableau%stages
                total = exact_add(chars(total), &
                                  chars(exact_mul(chars(weights(i)), &
                                                  chars(phi(i)), each_ok)), &
                                  each_ok)
                if (.not. each_ok) then
                    ok = .false.
                    return
                end if
            end do

            gamma = tree_gamma(trees, k)
            write (gamma_text, "(i0)") gamma
            target_value = exact_div("1", trim(gamma_text), each_ok)
            if (.not. each_ok) then
                ok = .false.
                return
            end if
            residuals(k) = exact_sub(chars(total), chars(target_value), each_ok)
            if (.not. each_ok) then
                ok = .false.
                return
            end if
        end do
        ok = .true.
    end function rk_order_residuals

    !> True when every order-condition residual up to `order` vanishes exactly.
    function rk_attains_order(tableau, weights, order) result(attains)
        type(butcher_t), intent(in) :: tableau
        type(str_t), intent(in) :: weights(:)
        integer, intent(in) :: order
        logical :: attains

        type(str_t), allocatable :: residuals(:)
        logical :: ok
        integer :: k

        residuals = rk_order_residuals(tableau, weights, order, ok)
        attains = ok
        if (.not. ok) return
        do k = 1, size(residuals)
            if (.not. rk_is_zero(residuals(k))) attains = .false.
        end do
    end function rk_attains_order

    !> The order-condition residuals for a tableau given in floating point.
    !>
    !> Not every published method has exact rational coefficients. DOP853's
    !> nodes involve a square root and its tableau is distributed as decimals,
    !> so there is nothing exact to test and the residuals cannot vanish. What
    !> they can do is sit at the level of the coefficients' own precision, and
    !> a residual far above that is a transcription error rather than rounding.
    !>
    !> `a` is the full square coupling matrix with zeros above the diagonal.
    !> Returns one residual per rooted tree up to `order`.
    function rk_order_residuals_real(a, weights, order, ok) result(residuals)
        real(dp), intent(in) :: a(:, :), weights(:)
        integer, intent(in) :: order
        logical, intent(out) :: ok
        real(dp), allocatable :: residuals(:)

        type(tree_table_t) :: trees
        real(dp), allocatable :: phi(:)
        real(dp) :: total
        integer :: k, i, stages

        allocate (residuals(0))
        stages = size(weights)
        ok = size(a, 1) == stages .and. size(a, 2) == stages
        if (.not. ok) return

        call build_rooted_trees(order, trees, ok)
        if (.not. ok) return

        allocate (phi(stages))
        deallocate (residuals)
        allocate (residuals(trees%n))
        do k = 1, trees%n
            do i = 1, stages
                phi(i) = tree_phi_real(a, trees, k, i)
            end do
            total = 0.0_dp
            do i = 1, stages
                total = total + weights(i)*phi(i)
            end do
            residuals(k) = total - 1.0_dp/real(tree_gamma(trees, k), dp)
        end do
        ok = .true.
    end function rk_order_residuals_real

    recursive function tree_phi_real(a, trees, k, i) result(value)
        real(dp), intent(in) :: a(:, :)
        type(tree_table_t), intent(in) :: trees
        integer, intent(in) :: k, i
        real(dp) :: value
        integer :: j, m, child
        real(dp) :: factor

        value = 1.0_dp
        do m = 1, trees%nchild(k)
            child = trees%child(m, k)
            factor = 0.0_dp
            do j = 1, size(a, 2)
                if (a(i, j) == 0.0_dp) cycle
                factor = factor + a(i, j)*tree_phi_real(a, trees, child, j)
            end do
            value = value*factor
        end do
    end function tree_phi_real

    !> Phi_i(t): 1 at a leaf, otherwise the product over the root's subtrees of
    !> sum_j a_ij Phi_j(subtree).
    recursive function tree_phi(tableau, trees, k, i, ok) result(value)
        type(butcher_t), intent(in) :: tableau
        type(tree_table_t), intent(in) :: trees
        integer, intent(in) :: k, i
        logical, intent(out) :: ok
        type(str_t) :: value

        integer :: j, m, child
        type(str_t) :: factor, term, sub

        value = str("1")
        ok = .true.
        do m = 1, trees%nchild(k)
            child = trees%child(m, k)
            factor = str("0")
            do j = 1, tableau%stages
                if (rk_is_zero(tableau%a(i, j))) cycle
                sub = tree_phi(tableau, trees, child, j, ok)
                if (.not. ok) return
                term = exact_mul(chars(tableau%a(i, j)), chars(sub), ok)
                if (.not. ok) return
                factor = exact_add(chars(factor), chars(term), ok)
                if (.not. ok) return
            end do
            value = exact_mul(chars(value), chars(factor), ok)
            if (.not. ok) return
        end do
    end function tree_phi

    !> b - bhat, the weights of the embedded error estimate.
    !>
    !> Deriving this is the whole point: hand-subtracting the two weight
    !> vectors is where an error estimate silently stops matching its method.
    function rk_error_weights(tableau, ok) result(weights)
        type(butcher_t), intent(in) :: tableau
        logical, intent(out) :: ok
        type(str_t), allocatable :: weights(:)
        integer :: i

        allocate (weights(0))
        ok = allocated(tableau%bhat)
        if (.not. ok) return

        deallocate (weights)
        allocate (weights(tableau%stages))
        do i = 1, tableau%stages
            weights(i) = exact_sub(chars(tableau%b(i)), &
                                   chars(tableau%bhat(i)), ok)
            if (.not. ok) return
        end do
    end function rk_error_weights

    !> True when the last stage row equals the solution weights, so the final
    !> derivative of one step is the first of the next.
    function rk_is_fsal(tableau) result(fsal)
        type(butcher_t), intent(in) :: tableau
        logical :: fsal, ok
        integer :: j, s

        s = tableau%stages
        fsal = s > 1
        if (.not. fsal) return
        do j = 1, s
            if (.not. rk_is_zero(exact_sub(chars(tableau%a(s, j)), &
                                           chars(tableau%b(j)), ok))) then
                fsal = .false.
            end if
            if (.not. ok) fsal = .false.
        end do
    end function rk_is_fsal

end module fortsym_rk
