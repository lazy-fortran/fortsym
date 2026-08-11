program test_fortsym_kernel_ir
    ! The IR is checked by interpreting it with an independent evaluator. This
    ! deliberately does not compare serialized IR or source text: a changed
    ! numbering is harmless, a changed value is not.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: chars, str_t
    use fortsym_algebraic, only: algebraic_from_re_im, algebraic_sqrt
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, algebraic_expr, sym, sin_expr => sin, &
        exp_expr => exp, operator(+), operator(*), operator(/), operator(**)
    use fortsym_kernel_ir
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_lowered_dag_has_independent_meaning()
    call test_real_algebraic_literal()
    call test_empty_roots()
    call test_mixed_arenas_are_rejected()

    if (nfail == 0) then
        print *, "test_fortsym_kernel_ir: all checks passed"
    else
        print *, "test_fortsym_kernel_ir: ", nfail, " check(s) FAILED"
        error stop 1
    end if

contains

    subroutine ok(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine ok

    subroutine test_lowered_dag_has_independent_meaning()
        type(arena_t), target :: arena
        type(expr_t) :: x, y, shared, root, roots(2)
        type(kernel_ir_t) :: ir
        logical :: good
        character(:), allocatable :: message
        real(dp) :: x_value, y_value, expected, actual
        real(dp), allocatable :: values(:)
        integer :: k, j, first, last

        call arena%init()
        x = sym(arena, "x")
        y = sym(arena, "y")
        shared = x*y
        root = sin_expr(shared) + exp_expr(x)/(1 + y*y) + (x + 1)**2
        roots(1) = root
        roots(2) = shared

        call lower_kernel_ir(roots, ir, good, message)
        call ok("lowering succeeds", good)
        call ok("lowering has no diagnostic", len(message) == 0)
        call ok("two roots are retained", size(ir%outputs) == 2)
        call ok("lowering produces a nonempty DAG", ir%n_nodes > 0)
        call ok("operand storage is sized", size(ir%operands) == ir%n_operands)

        do k = 1, ir%n_nodes
            first = ir%nodes(k)%first_operand
            last = first + ir%nodes(k)%n_operands - 1
            if (ir%nodes(k)%n_operands > 0) then
                call ok("operands are in range", first >= 1 .and. &
                    last <= ir%n_operands)
                do j = first, last
                    call ok("IR is topological", ir%operands(j) < k .and. &
                        ir%operands(j) >= 1)
                end do
            end if
        end do

        call ok("shared root is referenced by the DAG", any(&
            ir%operands(1:ir%n_operands) == ir%outputs(2)))

        x_value = 0.37_dp
        y_value = -0.81_dp
        allocate(values(ir%n_nodes), source=0.0_dp)
        do k = 1, ir%n_nodes
            values(k) = evaluate_node(ir, k, values, x_value, y_value)
        end do

        expected = sin(x_value*y_value) + exp(x_value)/ &
            (1.0_dp + y_value*y_value) + (x_value + 1.0_dp)**2
        actual = values(ir%outputs(1))
        call ok("independent evaluator agrees for the main root", &
            abs(actual - expected) < 1.0e-14_dp)
        call ok("independent evaluator agrees for the shared root", &
            abs(values(ir%outputs(2)) - x_value*y_value) < 1.0e-14_dp)
    end subroutine test_lowered_dag_has_independent_meaning

    subroutine test_real_algebraic_literal()
        type(arena_t), target :: arena
        type(expr_t) :: root, roots(1)
        type(kernel_ir_t) :: ir
        type(str_t) :: two_text, root_text
        character(:), allocatable :: message
        logical :: bridge_ok, good

        call arena%init()
        two_text = algebraic_from_re_im("2", "0", bridge_ok)
        root_text = algebraic_sqrt(chars(two_text), bridge_ok)
        root = algebraic_expr(arena, chars(root_text), bridge_ok)
        roots(1) = root

        call lower_kernel_ir(roots, ir, good, message)
        call ok("real algebraic literal lowers", good)
        if (.not. good) return
        call ok("real algebraic literal has one output", size(ir%outputs) == 1)
        if (size(ir%outputs) /= 1) return
        call ok("real algebraic literal becomes an IR literal", &
            ir%nodes(ir%outputs(1))%operation == IR_LITERAL)
        if (ir%nodes(ir%outputs(1))%operation /= IR_LITERAL) return
        call ok("real algebraic literal has checked binary64 value", &
            ir%nodes(ir%outputs(1))%value == 1.4142135623730951_dp)
    end subroutine test_real_algebraic_literal

    subroutine test_empty_roots()
        type(expr_t) :: roots(0)
        type(kernel_ir_t) :: ir
        logical :: good
        character(:), allocatable :: message

        call lower_kernel_ir(roots, ir, good, message)
        call ok("empty root list lowers", good)
        call ok("empty root list has no nodes", ir%n_nodes == 0)
        call ok("empty root list has no outputs", size(ir%outputs) == 0)
    end subroutine test_empty_roots

    subroutine test_mixed_arenas_are_rejected()
        type(arena_t), target :: left, right
        type(expr_t) :: roots(2)
        type(kernel_ir_t) :: ir
        logical :: good
        character(:), allocatable :: message

        call left%init()
        call right%init()
        roots(1) = sym(left, "x")
        roots(2) = sym(right, "x")
        call lower_kernel_ir(roots, ir, good, message)
        call ok("mixed arenas are rejected", .not. good)
        call ok("mixed arena diagnostic is useful", &
            index(message, "different arenas") > 0)
    end subroutine test_mixed_arenas_are_rejected

    function evaluate_node(ir, index, values, x_value, y_value) result(value)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index
        real(dp), intent(in) :: values(:), x_value, y_value
        real(dp) :: value
        character(:), allocatable :: name
        integer :: k, operand

        name = chars(ir%nodes(index)%name)
        select case (ir%nodes(index)%operation)
        case (IR_LITERAL)
            value = ir%nodes(index)%value
        case (IR_SYMBOL)
            select case (name)
            case ("x")
                value = x_value
            case ("y")
                value = y_value
            case default
                value = 0.0_dp
            end select
        case (IR_CONSTANT)
            select case (name)
            case ("pi")
                value = acos(-1.0_dp)
            case ("e")
                value = exp(1.0_dp)
            case default
                value = 0.0_dp
            end select
        case (IR_ADD)
            value = 0.0_dp
            do k = 1, ir%nodes(index)%n_operands
                operand = ir%operands(ir%nodes(index)%first_operand + k - 1)
                value = value + values(operand)
            end do
        case (IR_MUL)
            value = 1.0_dp
            do k = 1, ir%nodes(index)%n_operands
                operand = ir%operands(ir%nodes(index)%first_operand + k - 1)
                value = value * values(operand)
            end do
        case (IR_POW)
            value = values(ir%operands(ir%nodes(index)%first_operand)) ** &
                values(ir%operands(ir%nodes(index)%first_operand + 1))
        case (IR_FUNCTION)
            operand = ir%operands(ir%nodes(index)%first_operand)
            select case (name)
            case ("sin")
                value = sin(values(operand))
            case ("exp")
                value = exp(values(operand))
            case default
                value = 0.0_dp
            end select
        case default
            value = 0.0_dp
        end select
    end function evaluate_node

end program test_fortsym_kernel_ir
