module fortsym_kernel_ir
    ! Backend-neutral lowering for scalar expression kernels.
    !
    ! The expression arena is an excellent symbolic representation but it is
    ! not a code-generator interface: it exposes Fortran-owned names, exact
    ! arithmetic, and engine-facing node kinds. This module freezes a reachable
    ! expression DAG into a compact, typed IR. Fortran, CUDA, HIP, and SYCL
    ! emitters can consume the same node and operand arrays without adding
    ! backend syntax to the symbolic printer.
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, &
        NK_BIG_REAL, NK_ALGEBRAIC
    use fortsym_exact, only: exact_to_real
    use fortsym_algebraic, only: algebraic_to_real
    use fortsym_expr, only: expr_t
    use fortsym_string, only: chars, str, str_t
    implicit none
    private

    public :: kernel_ir_t, kernel_ir_node_t, lower_kernel_ir
    public :: IR_LITERAL, IR_SYMBOL, IR_CONSTANT, IR_ADD, IR_MUL, IR_POW, &
        IR_FUNCTION

    integer, parameter :: dp = real64

    integer, parameter :: IR_LITERAL = 1
    integer, parameter :: IR_SYMBOL = 2
    integer, parameter :: IR_CONSTANT = 3
    integer, parameter :: IR_ADD = 4
    integer, parameter :: IR_MUL = 5
    integer, parameter :: IR_POW = 6
    integer, parameter :: IR_FUNCTION = 7

    type :: kernel_ir_node_t
        integer :: operation = 0
        integer :: first_operand = 0
        integer :: n_operands = 0
        real(dp) :: value = 0.0_dp
        type(str_t) :: name
    end type kernel_ir_node_t

    type :: kernel_ir_t
        type(kernel_ir_node_t), allocatable :: nodes(:)
        integer, allocatable :: operands(:)
        integer, allocatable :: outputs(:)
        integer :: n_nodes = 0
        integer :: n_operands = 0
    contains
        procedure :: clear => kernel_ir_clear
    end type kernel_ir_t

contains

    subroutine kernel_ir_clear(self)
        class(kernel_ir_t), intent(inout) :: self

        if (allocated(self%nodes)) deallocate(self%nodes)
        if (allocated(self%operands)) deallocate(self%operands)
        if (allocated(self%outputs)) deallocate(self%outputs)
        self%n_nodes = 0
        self%n_operands = 0
    end subroutine kernel_ir_clear

    !> Lower `roots` into IR. Optional `locals` are symbols standing for
    !> `values`, in evaluation order: each value may read earlier locals.
    !> A local lowers to the IR node of its value, so its definition is
    !> computed once and kept as written instead of being merged into the
    !> expressions that read it, as an arena substitution would.
    !> With `share_products`, a product whose factors include all factors of
    !> an earlier product reads that product instead of multiplying them
    !> again. The arena flattens nested products, so this sharing is only
    !> visible after lowering. It reassociates products, which changes
    !> rounding, and is opt-in.
    subroutine lower_kernel_ir(roots, ir, ok, message, locals, values, share_products)
        type(expr_t), intent(in) :: roots(:)
        type(kernel_ir_t), intent(inout) :: ir
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(expr_t), intent(in), optional :: locals(:), values(:)
        logical, intent(in), optional :: share_products

        type(arena_t), pointer :: arena
        logical, allocatable :: seen(:), is_local(:)
        integer, allocatable :: mapping(:)
        integer :: node_count, operand_count, node_cursor, operand_cursor
        integer :: k, output_node, n_locals

        call ir%clear()
        ok = .false.
        message = ""
        if (size(roots) == 0) then
            allocate(ir%nodes(0), ir%operands(0), ir%outputs(0))
            ok = .true.
            return
        end if
        if (.not. associated(roots(1)%a)) then
            message = "kernel IR: first root is invalid"
            return
        end if
        arena => roots(1)%a
        do k = 1, size(roots)
            if (.not. associated(roots(k)%a)) then
                message = "kernel IR: root is invalid"
                return
            end if
            if (.not. associated(roots(k)%a, arena)) then
                message = "kernel IR: roots belong to different arenas"
                return
            end if
            if (roots(k)%id < 1 .or. roots(k)%id > arena%size()) then
                message = "kernel IR: root node index is invalid"
                return
            end if
        end do

        n_locals = 0
        if (present(locals) .neqv. present(values)) then
            message = "kernel IR: locals and values must be given together"
            return
        end if
        if (present(locals)) n_locals = size(locals)
        if (n_locals > 0) then
            if (size(values) /= n_locals) then
                message = "kernel IR: locals and values differ in size"
                return
            end if
            do k = 1, n_locals
                if (.not. associated(locals(k)%a, arena) .or. &
                    .not. associated(values(k)%a, arena)) then
                    message = "kernel IR: locals belong to a different arena"
                    return
                end if
                if (arena%kind_of(locals(k)%id) /= NK_SYM) then
                    message = "kernel IR: a local must be a symbol"
                    return
                end if
            end do
        end if

        allocate(seen(arena%size()), source=.false.)
        node_count = 0
        operand_count = 0
        ! A local symbol is never an IR node of its own.
        allocate(is_local(arena%size()), source=.false.)
        do k = 1, n_locals
            seen(locals(k)%id) = .true.
            is_local(locals(k)%id) = .true.
        end do
        do k = 1, n_locals
            call count_reachable(arena, values(k)%id, seen, node_count, &
                operand_count, ok, message)
            if (.not. ok) return
        end do
        do k = 1, size(roots)
            call count_reachable(arena, roots(k)%id, seen, node_count, &
                operand_count, ok, message)
            if (.not. ok) return
        end do

        allocate(ir%nodes(node_count), ir%operands(operand_count), &
            ir%outputs(size(roots)))
        allocate(mapping(arena%size()), source=0)
        node_cursor = 0
        operand_cursor = 0
        ok = .true.
        do k = 1, n_locals
            if (mapping(locals(k)%id) > 0) then
                message = "kernel IR: a local is read before its value or declared twice"
                ok = .false.
                return
            end if
            call lower_node(arena, values(k)%id, mapping, ir, node_cursor, &
                operand_cursor, output_node, ok, message, is_local)
            if (.not. ok) return
            mapping(locals(k)%id) = output_node
        end do
        do k = 1, size(roots)
            call lower_node(arena, roots(k)%id, mapping, ir, node_cursor, &
                operand_cursor, output_node, ok, message, is_local)
            if (.not. ok) return
            ir%outputs(k) = output_node
        end do
        ir%n_nodes = node_cursor
        ir%n_operands = operand_cursor
        ! Selections emit a different number of nodes than the arena holds.
        if (size(ir%nodes) /= node_cursor) ir%nodes = ir%nodes(:node_cursor)
        if (size(ir%operands) /= operand_cursor) &
            ir%operands = ir%operands(:operand_cursor)
        if (present(share_products)) then
            if (share_products) call reuse_subproducts(ir)
        end if
    end subroutine lower_kernel_ir

    !> Replace the factors of an earlier product inside a later product by a
    !> reference to it, the largest earlier product first. Operands only
    !> shrink and point to earlier nodes, so the IR stays topological.
    subroutine reuse_subproducts(ir)
        type(kernel_ir_t), intent(inout) :: ir
        integer :: k, j, best, best_size, first, n
        integer, allocatable :: factors(:), inner(:)
        logical, allocatable :: used(:)

        do k = 1, ir%n_nodes
            if (ir%nodes(k)%operation /= IR_MUL) cycle
            do
                n = ir%nodes(k)%n_operands
                if (n < 3) exit
                first = ir%nodes(k)%first_operand
                factors = ir%operands(first:first + n - 1)
                best = 0
                best_size = 1
                do j = 1, k - 1
                    if (ir%nodes(j)%operation /= IR_MUL) cycle
                    if (ir%nodes(j)%n_operands <= best_size) cycle
                    if (ir%nodes(j)%n_operands >= n) cycle
                    inner = ir%operands(ir%nodes(j)%first_operand: &
                        ir%nodes(j)%first_operand + ir%nodes(j)%n_operands - 1)
                    if (.not. contains_multiset(factors, inner)) cycle
                    best = j
                    best_size = size(inner)
                end do
                if (best == 0) exit
                inner = ir%operands(ir%nodes(best)%first_operand: &
                    ir%nodes(best)%first_operand + ir%nodes(best)%n_operands - 1)
                allocate (used(n), source=.false.)
                do j = 1, size(inner)
                    used(findloc(factors .eq. inner(j) .and. .not. used, .true., 1)) = .true.
                end do
                factors = [best, pack(factors, .not. used)]
                deallocate (used)
                ! The rewritten list is shorter, so it fits in the old slots.
                ir%operands(first:first + size(factors) - 1) = factors
                ir%nodes(k)%n_operands = size(factors)
            end do
        end do
    end subroutine reuse_subproducts

    pure function contains_multiset(outer, inner) result(yes)
        integer, intent(in) :: outer(:), inner(:)
        logical :: yes
        logical :: used(size(outer))
        integer :: j, slot

        used = .false.
        yes = .false.
        do j = 1, size(inner)
            slot = findloc(outer .eq. inner(j) .and. .not. used, .true., 1)
            if (slot == 0) return
            used(slot) = .true.
        end do
        yes = .true.
    end function contains_multiset

    recursive subroutine count_reachable(arena, id, seen, node_count, &
            operand_count, ok, message)
        type(arena_t), intent(in) :: arena
        integer, intent(in) :: id
        logical, intent(inout) :: seen(:)
        integer, intent(inout) :: node_count, operand_count
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: k, child

        ok = .false.
        message = ""
        if (id < 1 .or. id > arena%size()) then
            message = "kernel IR: child node index is invalid"
            return
        end if
        if (seen(id)) then
            ok = .true.
            return
        end if
        seen(id) = .true.
        node_count = node_count + 1
        operand_count = operand_count + arena%nargs_of(id)
        do k = 1, arena%nargs_of(id)
            child = arena%arg_of(id, k)
            call count_reachable(arena, child, seen, node_count, operand_count, &
                ok, message)
            if (.not. ok) return
        end do
        ok = .true.
    end subroutine count_reachable

    recursive subroutine lower_node(arena, id, mapping, ir, node_cursor, &
            operand_cursor, node_index, ok, message, is_local)
        type(arena_t), intent(in) :: arena
        integer, intent(in) :: id
        integer, intent(inout) :: mapping(:)
        type(kernel_ir_t), intent(inout) :: ir
        integer, intent(inout) :: node_cursor, operand_cursor
        integer, intent(out) :: node_index
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        logical, intent(in) :: is_local(:)

        integer :: k, child, child_index, kind

        ok = .false.
        message = ""
        node_index = 0
        if (id < 1 .or. id > arena%size()) then
            message = "kernel IR: node index is invalid"
            return
        end if
        if (mapping(id) > 0) then
            node_index = mapping(id)
            ok = .true.
            return
        end if
        if (is_local(id)) then
            message = "kernel IR: a local is read before its value: "// &
                chars(arena%name_of(id))
            return
        end if
        if (arena%kind_of(id) == NK_FUNC) then
            if (chars(arena%name_of(id)) == "Piecewise" .or. &
                chars(arena%name_of(id)) == "If") then
                call lower_selection(arena, id, mapping, ir, node_cursor, &
                    operand_cursor, node_index, ok, message, is_local)
                if (ok) mapping(id) = node_index
                return
            end if
        end if

        do k = 1, arena%nargs_of(id)
            child = arena%arg_of(id, k)
            call lower_node(arena, child, mapping, ir, node_cursor, &
                operand_cursor, child_index, ok, message, is_local)
            if (.not. ok) return
        end do

        call reserve(ir, node_cursor + 1, operand_cursor + arena%nargs_of(id))
        node_cursor = node_cursor + 1
        node_index = node_cursor
        mapping(id) = node_index
        ir%nodes(node_index)%first_operand = operand_cursor + 1
        ir%nodes(node_index)%n_operands = arena%nargs_of(id)
        do k = 1, arena%nargs_of(id)
            child = arena%arg_of(id, k)
            child_index = mapping(child)
            operand_cursor = operand_cursor + 1
            ir%operands(operand_cursor) = child_index
        end do

        kind = arena%kind_of(id)
        select case (kind)
        case (NK_INT, NK_RAT, NK_REAL, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL, &
                NK_ALGEBRAIC)
            ir%nodes(node_index)%operation = IR_LITERAL
            call literal_value(arena, id, ir%nodes(node_index)%value, ok, &
                message)
            if (.not. ok) return
        case (NK_SYM)
            ir%nodes(node_index)%operation = IR_SYMBOL
            ir%nodes(node_index)%name = arena%name_of(id)
        case (NK_CONST)
            ir%nodes(node_index)%operation = IR_CONSTANT
            ir%nodes(node_index)%name = arena%name_of(id)
        case (NK_ADD)
            ir%nodes(node_index)%operation = IR_ADD
        case (NK_MUL)
            ir%nodes(node_index)%operation = IR_MUL
        case (NK_POW)
            ir%nodes(node_index)%operation = IR_POW
        case (NK_FUNC)
            ir%nodes(node_index)%operation = IR_FUNCTION
            ir%nodes(node_index)%name = arena%name_of(id)
        case default
            message = "kernel IR: unsupported arena node kind"
            return
        end select
        ok = .true.
    end subroutine lower_node

    !> Lower `Piecewise(List(List(value, condition), ...), default)` and
    !> `If(condition, value, otherwise)` to FortGen's relational selections
    !> `select_<relation>(l, r, value, otherwise)`. Every branch value is an
    !> ordinary earlier node, so a selection chooses a value and does not
    !> guard the evaluation of the branch it rejects.
    recursive subroutine lower_selection(arena, id, mapping, ir, node_cursor, &
            operand_cursor, node_index, ok, message, is_local)
        type(arena_t), intent(in) :: arena
        integer, intent(in) :: id
        integer, intent(inout) :: mapping(:)
        type(kernel_ir_t), intent(inout) :: ir
        integer, intent(inout) :: node_cursor, operand_cursor
        integer, intent(out) :: node_index
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        logical, intent(in) :: is_local(:)
        integer :: k, branches, pair, value, otherwise, chosen

        ok = .false.
        node_index = 0
        if (chars(arena%name_of(id)) == "If") then
            message = "kernel IR: If needs a condition and two values"
            if (arena%nargs_of(id) /= 3) return
            call lower_node(arena, arena%arg_of(id, 2), mapping, ir, node_cursor, &
                operand_cursor, value, ok, message, is_local)
            if (.not. ok) return
            call lower_node(arena, arena%arg_of(id, 3), mapping, ir, node_cursor, &
                operand_cursor, otherwise, ok, message, is_local)
            if (.not. ok) return
            call lower_condition(arena, arena%arg_of(id, 1), value, otherwise, &
                mapping, ir, node_cursor, operand_cursor, node_index, ok, &
                message, is_local)
            return
        end if

        message = "kernel IR: Piecewise needs a List of {value, condition} pairs"
        if (arena%nargs_of(id) < 1 .or. arena%nargs_of(id) > 2) return
        branches = arena%arg_of(id, 1)
        if (.not. is_list(arena, branches)) return
        do k = 1, arena%nargs_of(branches)
            pair = arena%arg_of(branches, k)
            if (.not. is_list(arena, pair)) return
            if (arena%nargs_of(pair) /= 2) return
        end do
        if (arena%nargs_of(id) == 2) then
            call lower_node(arena, arena%arg_of(id, 2), mapping, ir, node_cursor, &
                operand_cursor, otherwise, ok, message, is_local)
            if (.not. ok) return
        else
            call reserve(ir, node_cursor + 1, operand_cursor)
            node_cursor = node_cursor + 1
            otherwise = node_cursor
            ir%nodes(otherwise)%operation = IR_LITERAL
            ir%nodes(otherwise)%first_operand = operand_cursor + 1
        end if
        ! The first branch whose condition holds wins: build from the last.
        do k = arena%nargs_of(branches), 1, -1
            pair = arena%arg_of(branches, k)
            call lower_node(arena, arena%arg_of(pair, 1), mapping, ir, &
                node_cursor, operand_cursor, value, ok, message, is_local)
            if (.not. ok) return
            call lower_condition(arena, arena%arg_of(pair, 2), value, otherwise, &
                mapping, ir, node_cursor, operand_cursor, chosen, ok, &
                message, is_local)
            if (.not. ok) return
            otherwise = chosen
        end do
        node_index = otherwise
        ok = .true.
        message = ""
    end subroutine lower_selection

    !> The node holding `value` where `condition` holds and `otherwise`
    !> elsewhere. Logical connectives nest selections; they add no node.
    recursive subroutine lower_condition(arena, condition, value, otherwise, &
            mapping, ir, node_cursor, operand_cursor, node_index, ok, message, &
            is_local)
        type(arena_t), intent(in) :: arena
        integer, intent(in) :: condition, value, otherwise
        integer, intent(inout) :: mapping(:)
        type(kernel_ir_t), intent(inout) :: ir
        integer, intent(inout) :: node_cursor, operand_cursor
        integer, intent(out) :: node_index
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        logical, intent(in) :: is_local(:)
        character(:), allocatable :: name, relation
        integer :: k, left, right, held
        logical :: swapped, constant

        ok = .false.
        node_index = 0
        name = ""
        ! Truth values occur both as symbols (native simplification) and as
        ! named constants (inequality solving).
        constant = arena%kind_of(condition) == NK_SYM .or. &
            arena%kind_of(condition) == NK_CONST
        if (constant .or. arena%kind_of(condition) == NK_FUNC) then
            name = chars(arena%name_of(condition))
        else
            name = "(a number or an arithmetic node)"
        end if
        message = "kernel IR: unsupported selection condition "//name
        if (constant) then
            if (name == "True") node_index = value
            if (name == "False") node_index = otherwise
            ok = node_index > 0
            if (ok) message = ""
            return
        end if
        if (arena%kind_of(condition) /= NK_FUNC) return
        swapped = .false.
        select case (name)
        case ("Not")
            if (arena%nargs_of(condition) /= 1) return
            call lower_condition(arena, arena%arg_of(condition, 1), otherwise, &
                value, mapping, ir, node_cursor, operand_cursor, node_index, ok, &
                message, is_local)
            return
        case ("And", "Or")
            if (arena%nargs_of(condition) < 1) return
            held = value
            if (name == "Or") held = otherwise
            do k = arena%nargs_of(condition), 1, -1
                if (name == "And") then
                    call lower_condition(arena, arena%arg_of(condition, k), held, &
                        otherwise, mapping, ir, node_cursor, operand_cursor, &
                        node_index, ok, message, is_local)
                else
                    call lower_condition(arena, arena%arg_of(condition, k), value, &
                        held, mapping, ir, node_cursor, operand_cursor, &
                        node_index, ok, message, is_local)
                end if
                if (.not. ok) return
                held = node_index
            end do
            return
        case ("Less");         relation = "select_lt"
        case ("LessEqual");    relation = "select_le"
        case ("Equal");        relation = "select_eq"
        case ("Unequal");      relation = "select_ne"
        case ("Greater");      relation = "select_lt"; swapped = .true.
        case ("GreaterEqual"); relation = "select_le"; swapped = .true.
        case default
            return
        end select
        if (arena%nargs_of(condition) /= 2) return
        call lower_node(arena, arena%arg_of(condition, 1), mapping, ir, &
            node_cursor, operand_cursor, left, ok, message, is_local)
        if (.not. ok) return
        call lower_node(arena, arena%arg_of(condition, 2), mapping, ir, &
            node_cursor, operand_cursor, right, ok, message, is_local)
        if (.not. ok) return
        ! l > r is r < l, also for an unordered operand.
        if (swapped) then
            k = left
            left = right
            right = k
        end if
        call reserve(ir, node_cursor + 1, operand_cursor + 4)
        node_cursor = node_cursor + 1
        node_index = node_cursor
        ir%nodes(node_index)%operation = IR_FUNCTION
        ir%nodes(node_index)%name = str(relation)
        ir%nodes(node_index)%first_operand = operand_cursor + 1
        ir%nodes(node_index)%n_operands = 4
        ir%operands(operand_cursor + 1:operand_cursor + 4) = &
            [left, right, value, otherwise]
        operand_cursor = operand_cursor + 4
    end subroutine lower_condition

    logical function is_list(arena, id) result(yes)
        type(arena_t), intent(in) :: arena
        integer, intent(in) :: id

        yes = .false.
        if (arena%kind_of(id) /= NK_FUNC) return
        yes = chars(arena%name_of(id)) == "List"
    end function is_list

    !> Grow the node and operand storage to hold at least the given counts.
    subroutine reserve(ir, n_nodes, n_operands)
        type(kernel_ir_t), intent(inout) :: ir
        integer, intent(in) :: n_nodes, n_operands
        type(kernel_ir_node_t), allocatable :: nodes(:)
        integer, allocatable :: operands(:)

        if (n_nodes > size(ir%nodes)) then
            allocate (nodes(max(n_nodes, 2*size(ir%nodes))))
            nodes(:size(ir%nodes)) = ir%nodes
            call move_alloc(nodes, ir%nodes)
        end if
        if (n_operands > size(ir%operands)) then
            allocate (operands(max(n_operands, 2*size(ir%operands))))
            operands(:size(ir%operands)) = ir%operands
            call move_alloc(operands, ir%operands)
        end if
    end subroutine reserve

    subroutine literal_value(arena, id, value, ok, message)
        type(arena_t), intent(in) :: arena
        integer, intent(in) :: id
        real(dp), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        character(:), allocatable :: text
        integer :: ios

        ok = .true.
        message = ""
        select case (arena%kind_of(id))
        case (NK_INT)
            value = real(arena%num_of(id), dp)
        case (NK_RAT)
            value = real(arena%num_of(id), dp)/real(arena%den_of(id), dp)
        case (NK_REAL)
            value = arena%real_of(id)
        case (NK_BIG_INT, NK_BIG_RAT)
            value = exact_to_real(chars(arena%exact_text_of(id)), ok)
            if (.not. ok) message = "kernel IR: exact literal is not finite"
        case (NK_BIG_REAL)
            text = chars(arena%real_text_of(id))
            read(text, *, iostat=ios) value
            if (ios /= 0) then
                ok = .false.
                message = "kernel IR: decimal literal cannot be parsed"
            end if
        case (NK_ALGEBRAIC)
            value = algebraic_to_real(chars(arena%algebraic_text_of(id)), ok)
            if (.not. ok) message = "kernel IR: algebraic value is not a finite real64 literal"
        end select
        if (ok) then
            if (.not. ieee_is_finite(value)) then
                ok = .false.
                message = "kernel IR: literal is not finite"
            end if
        end if
    end subroutine literal_value

end module fortsym_kernel_ir
