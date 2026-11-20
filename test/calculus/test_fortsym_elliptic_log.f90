program test_fortsym_elliptic_log
    ! Log-split pieces of K in K(1-q) = -ln(q) A_K(q)/2 + B_K(q) and their
    ! q-derivatives elliptic_k_log_a(n, q), elliptic_k_log_b(n, q).
    ! Independent oracles:
    !   * K(1-q), E(1-q) and their q-derivatives from the AGM evaluation of
    !     elliptic_k, elliptic_e (no series, no Legendre recursion);
    !   * the derivative rule, checked symbolically and against centred
    !     differences;
    !   * the two numeric branches (series below q = 1/2, Legendre recursion
    !     above) meeting at q = 1/2.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, elliptic_k, elliptic_e, &
        log, operator(+), operator(-), operator(*), operator(/), operator(==)
    use fortsym_elliptic_log, only: elliptic_k_log_a, elliptic_k_log_b, &
        elliptic_e_log_a, elliptic_e_log_b
    use fortsym_diff, only: diff
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir
    use fortsym_print, only: fortran_representable
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, KERNEL_SUBROUTINE
    implicit none

    integer, parameter :: dp = real64
    real(dp), parameter :: points(4) = [1.0e-3_dp, 0.3_dp, 0.7_dp, 0.9_dp]
    type(arena_t), target :: arena
    type(native_engine_t) :: engine
    type(suite_t) :: suite
    type(expr_t) :: q, k_split, e_split, k_exact, e_exact
    type(binding_t) :: bindings
    integer :: nfail, i, n

    nfail = 0
    call arena%init()
    engine = make_native_engine(arena)
    q = sym(arena, "q")
    k_split = -log(q)*elliptic_k_log_a(0, q)/2 + elliptic_k_log_b(0, q)
    e_split = -log(q)*elliptic_e_log_a(q)/2 + elliptic_e_log_b(q)
    k_exact = elliptic_k(1 - q)
    e_exact = elliptic_e(1 - q)

    call suite_begin(suite, "log-split pieces of K")
    call check_identity(suite, engine, "d/dq A_K^(n) = A_K^(n+1)", &
        diff(elliptic_k_log_a(2, q), q) - elliptic_k_log_a(3, q))
    call check_identity(suite, engine, "d/dq B_K^(n) = B_K^(n+1)", &
        diff(elliptic_k_log_b(0, q*q), q) - 2*q*elliptic_k_log_b(1, q*q))
    call suite_end(suite)
    call check("pieces are constant in an unrelated symbol", &
        diff(elliptic_k_log_a(0, q), sym(arena, "x")) == num(arena, 0))

    allocate (bindings%names(1), bindings%values(1))
    bindings%names(1) = str("q")
    bindings%n = 1
    do i = 1, size(points)
        call agree("K(1-q) from its pieces", k_split, k_exact, points(i), &
            1.0e-13_dp)
        call agree("dK(1-q)/dq from its pieces", diff(k_split, q), &
            diff(k_exact, q), points(i), 1.0e-11_dp)
        call agree("d2K(1-q)/dq2 from its pieces", diff(diff(k_split, q), q), &
            diff(diff(k_exact, q), q), points(i), 1.0e-10_dp)
        call agree("E(1-q) from its pieces", e_split, e_exact, points(i), &
            1.0e-13_dp)
        call agree("dE(1-q)/dq from its pieces", diff(e_split, q), &
            diff(e_exact, q), points(i), 1.0e-11_dp)
    end do
    do n = 0, 3
        call meet("A_K branches meet at q = 1/2", elliptic_k_log_a(n, q))
        call meet("B_K branches meet at q = 1/2", elliptic_k_log_b(n, q))
    end do
    bindings%values(1) = 0.0_dp
    call agree_value("A_K'(0) = 1/4", elliptic_k_log_a(1, q), 0.25_dp)
    call agree_value("B_K'(0) = (ln 4 - 1)/4", elliptic_k_log_b(1, q), &
        (log(4.0_dp) - 1.0_dp)/4)
    call test_emission()

    if (nfail /= 0) error stop 1
    write (*, "(a)") "elliptic log-piece checks passed"

contains

    subroutine test_emission()
        type(expr_t) :: roots(1)
        type(kernel_ir_t) :: ir
        type(kernel_emit_spec_t) :: spec
        character(:), allocatable :: message
        logical :: good
        character(:), allocatable :: source

        roots(1) = elliptic_k_log_a(1, q) + elliptic_k_log_b(2, q)
        call check("log pieces are representable", fortran_representable(roots(1)))
        call check("a symbolic order is not representable", &
            .not. fortran_representable(elliptic_k_log_a(sym(arena, "n"), q)))
        call lower_kernel_ir(roots, ir, good, message)
        call check("log-piece IR lowers", good)
        spec%name = str("log_piece_leaf")
        spec%args = [str("q")]
        spec%outputs = [str("value")]
        spec%generator = str("test_fortsym_elliptic_log")
        source = chars(emit_fortran_kernel_ir(ir, spec, good, message))
        call check("log pieces emit", good)
        call check("log pieces import fortnum_special", index(source, &
            "use fortnum_special, only: ellip_k_log_a, ellip_k_log_b") > 0)
        call check("order stays an integer", index(source, "ellip_k_log_b(2, q)") > 0)

        ! emit_kernel, the generators' path, imports the same entry points.
        block
            type(kernel_spec_t) :: kspec

            kspec%name = str("log_piece_kernel")
            kspec%mode = KERNEL_SUBROUTINE
            kspec%args = [str("q")]
            kspec%outputs = [str("value")]
            kspec%generator = str("test_fortsym_elliptic_log")
            source = chars(emit_kernel(roots, kspec))
            call check("emit_kernel imports the log pieces", index(source, &
                "use fortnum_special, only: ellip_k_log_a, ellip_k_log_b") > 0)
            roots(1) = elliptic_k(q) + q
            source = chars(emit_kernel(roots, kspec))
            call check("emit_kernel imports only what is called", index(source, &
                "use fortnum_special, only: ellip_k"//new_line("a")) > 0)
            kspec%special_module = str("my_special")
            source = chars(emit_kernel(roots, kspec))
            call check("emit_kernel honours special_module", &
                index(source, "use my_special, only: ellip_k") > 0)
        end block
    end subroutine test_emission

    subroutine agree(label, f, g, at, tol)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: f, g
        real(dp), intent(in) :: at, tol
        real(dp) :: fv, gv
        logical :: d1, d2

        bindings%values(1) = at
        fv = eval_expr(f, bindings, d1)
        gv = eval_expr(g, bindings, d2)
        call check(label, d1 .and. d2 .and. abs(fv - gv) <= tol*max(1.0_dp, abs(gv)))
        if (.not. (d1 .and. d2 .and. abs(fv - gv) <= tol*max(1.0_dp, abs(gv)))) &
            write (*, "(a,3es24.15)") "  at, got, expected: ", at, fv, gv
    end subroutine agree

    subroutine agree_value(label, f, expected)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: f
        real(dp), intent(in) :: expected
        real(dp) :: fv
        logical :: defined

        fv = eval_expr(f, bindings, defined)
        call check(label, defined .and. abs(fv - expected) <= 1.0e-15_dp)
    end subroutine agree_value

    subroutine meet(label, f)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: f
        real(dp) :: below, at_half
        logical :: d1, d2

        bindings%values(1) = 0.5_dp - 1.0e-12_dp
        below = eval_expr(f, bindings, d1)
        bindings%values(1) = 0.5_dp
        at_half = eval_expr(f, bindings, d2)
        call check(label, d1 .and. d2 .and. &
            abs(below - at_half) <= 1.0e-10_dp*max(1.0_dp, abs(at_half)))
    end subroutine meet

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        if (condition) then
            write (*, "(a)") "PASS "//label
        else
            write (*, "(a)") "FAIL "//label
            nfail = nfail + 1
        end if
    end subroutine check

end program test_fortsym_elliptic_log
