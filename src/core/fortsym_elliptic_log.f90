module fortsym_elliptic_log
    !! Log-split pieces of the complete elliptic integrals near m = 1,
    !!
    !!   K(1-q) = -ln(q) A_K(q)/2 + B_K(q),   E(1-q) = -ln(q) A_E(q)/2 + B_E(q),
    !!
    !! (DLMF 19.12.1-2, A_K = (2/pi) K(q)).  elliptic_k_log_a(n, q) and
    !! elliptic_k_log_b(n, q) are the n-th q-derivatives of A_K and B_K, as
    !! fortnum ellip_k_log_a(n, q) and ellip_k_log_b(n, q).  The derivative of
    !! order n is the head of order n+1, so repeated differentiation keeps the
    !! accuracy of the series as q -> 0.  The E pieces follow from
    !! E = (1-m) K + 2m(1-m) dK/dm without division:
    !!
    !!   A_E = q A_K - 2q(1-q) A_K',   B_E = q B_K + (1-q) A_K - 2q(1-q) B_K'.
    use fortsym_expr, only: expr_t, num, func, operator(+), operator(-), &
        operator(*)
    implicit none
    private

    public :: elliptic_k_log_a, elliptic_k_log_b
    public :: elliptic_e_log_a, elliptic_e_log_b

    interface elliptic_k_log_a
        module procedure k_log_a_ee
        module procedure k_log_a_ie
    end interface elliptic_k_log_a

    interface elliptic_k_log_b
        module procedure k_log_b_ee
        module procedure k_log_b_ie
    end interface elliptic_k_log_b

contains

    function k_log_a_ee(order, q) result(e)
        type(expr_t), intent(in) :: order, q
        type(expr_t) :: e
        type(expr_t) :: args(2)

        args(1) = order
        args(2) = q
        e = func("elliptic_k_log_a", args)
    end function k_log_a_ee

    function k_log_a_ie(order, q) result(e)
        integer, intent(in) :: order
        type(expr_t), intent(in) :: q
        type(expr_t) :: e

        e = k_log_a_ee(num(q%a, order), q)
    end function k_log_a_ie

    function k_log_b_ee(order, q) result(e)
        type(expr_t), intent(in) :: order, q
        type(expr_t) :: e
        type(expr_t) :: args(2)

        args(1) = order
        args(2) = q
        e = func("elliptic_k_log_b", args)
    end function k_log_b_ee

    function k_log_b_ie(order, q) result(e)
        integer, intent(in) :: order
        type(expr_t), intent(in) :: q
        type(expr_t) :: e

        e = k_log_b_ee(num(q%a, order), q)
    end function k_log_b_ie

    function elliptic_e_log_a(q) result(e)
        type(expr_t), intent(in) :: q
        type(expr_t) :: e

        e = q*k_log_a_ie(0, q) - 2*q*(1 - q)*k_log_a_ie(1, q)
    end function elliptic_e_log_a

    function elliptic_e_log_b(q) result(e)
        type(expr_t), intent(in) :: q
        type(expr_t) :: e

        e = q*k_log_b_ie(0, q) + (1 - q)*k_log_a_ie(0, q) &
            - 2*q*(1 - q)*k_log_b_ie(1, q)
    end function elliptic_e_log_b

end module fortsym_elliptic_log
