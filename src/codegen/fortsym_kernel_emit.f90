module fortsym_kernel_emit
    ! Emit scalar leaves from the backend-neutral kernel IR.
    !
    ! This module is intentionally the only place where Fortran and CUDA
    ! spellings meet. The IR has no launch geometry, residency, or dialect
    ! branches. A generated CUDA leaf can therefore be called by a later
    ! backend-owned global wrapper without making fortsym know about grids or
    ! streams.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_kernel_ir, only: kernel_ir_t, kernel_ir_node_t, IR_LITERAL, &
        IR_SYMBOL, IR_CONSTANT, IR_ADD, IR_MUL, IR_POW, IR_FUNCTION
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_expr, only: expr_t
    use fortsym_quadratic, only: quadratic_t
    use fortsym_dialect, only: dialect, DIA_FORTRAN
    use fortsym_print, only: print_expr_in
    use fortsym_kernel_target, only: TARGET_DEFAULT_VALUE => TARGET_DEFAULT, &
        TARGET_FORTRAN_CPU_VALUE => TARGET_FORTRAN_CPU, &
        TARGET_FORTRAN_OPENMP_TARGET_VALUE => TARGET_FORTRAN_OPENMP_TARGET, &
        TARGET_FORTRAN_OPENACC_VALUE => TARGET_FORTRAN_OPENACC, &
        TARGET_DUAL_VALUE => TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC, &
        TARGET_CUDA_VALUE => TARGET_CUDA, &
        target_directives, target_is_valid, target_name
    implicit none
    private

    public :: kernel_emission_policy_t, kernel_emit_spec_t
    public :: emit_fortran_kernel_ir, emit_cuda_device_ir
    public :: emit_table
    public :: TARGET_DEFAULT, TARGET_FORTRAN_CPU
    public :: TARGET_FORTRAN_OPENMP_TARGET, TARGET_FORTRAN_OPENACC
    public :: TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC, TARGET_CUDA
    public :: target_name

    integer, parameter :: TARGET_DEFAULT = TARGET_DEFAULT_VALUE
    integer, parameter :: TARGET_FORTRAN_CPU = TARGET_FORTRAN_CPU_VALUE
    integer, parameter :: TARGET_FORTRAN_OPENMP_TARGET = &
        TARGET_FORTRAN_OPENMP_TARGET_VALUE
    integer, parameter :: TARGET_FORTRAN_OPENACC = TARGET_FORTRAN_OPENACC_VALUE
    integer, parameter :: TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC = TARGET_DUAL_VALUE
    integer, parameter :: TARGET_CUDA = TARGET_CUDA_VALUE

    !> Named, unconditional source-emission policies. These are structural
    !> choices made from the exact expression before a compiler sees floats;
    !> they do not rely on fast-math reassociation.
    type :: kernel_emission_policy_t
        !> Expand positive integer powers through this degree. Zero disables
        !> expansion while retaining exact neutral-element folding.
        integer :: small_power_limit = 3
        !> Remove exact zero/one neutral elements and fold exact constant powers.
        logical :: fold_exact_constants = .true.
        !> Turn a constant reciprocal into a real literal before emission.
        logical :: eliminate_constant_divisions = .true.
        !> Inline one-use product terms in sums so compilers can form an FMA.
        logical :: shape_fma = .true.
    end type kernel_emission_policy_t

    interface emit_table
        module procedure emit_table_expr_1d
        module procedure emit_table_expr_2d
        module procedure emit_table_quadratic_1d
        module procedure emit_table_quadratic_2d
    end interface emit_table

    integer, parameter :: dp = real64
    integer, parameter :: BACKEND_FORTRAN = 1
    integer, parameter :: BACKEND_CUDA = 2

    type :: kernel_emit_spec_t
        type(str_t) :: name
        type(str_t), allocatable :: args(:)
        type(str_t), allocatable :: outputs(:)
        type(str_t) :: temp_prefix
        !> Stable target identity. The default retains the historical
        !> undecorated IR-emitter output; explicit targets select spelling and
        !> Fortran device decoration without changing the IR.
        integer :: target = TARGET_DEFAULT
        !> Source choices applied to this kernel's lowered IR.
        type(kernel_emission_policy_t) :: policy
        !! Emit the Fortran leaf as a `pure` procedure. Generated kernels only
        !! ever call intrinsics on their arguments, so purity is always sound;
        !! it is opt-in solely so existing committed kernels stay byte-identical
        !! until their consumer asks for it. Ignored by the CUDA backend, which
        !! has no such concept.
        logical :: pure_procedure = .false.
        type(str_t) :: generator
        type(str_t) :: generator_revision
        type(str_t) :: regenerate_command
    end type kernel_emit_spec_t

contains

    !> Emit a typed rank-one constant table. Every value goes through the same
    !> Fortran renderer used by kernel emission; comments are placed after the
    !> continuation marker so they cannot hide a required token.
    function emit_table_expr_1d(name, values, declared_kind, comments, ok, message) &
            result(source)
        character(*), intent(in) :: name, declared_kind
        type(expr_t), intent(in) :: values(:)
        character(*), intent(in), optional :: comments(:)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(strbuf_t) :: b
        integer :: k, shape1(1)

        call validate_table_header(name, declared_kind, size(values), ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        if (present(comments)) then
            if (size(comments) /= size(values)) then
                ok = .false.
                message = "table emitter: comment count does not match table size"
                source = str("")
                return
            end if
        end if

        shape1(1) = size(values)
        call append_table_header(b, name, declared_kind, shape1)
        do k = 1, size(values)
            if (present(comments)) then
                call append_table_value(b, values(k), k == size(values), ok, &
                    message, comments(k), trailer=rank_one_trailer(k == size(values)))
            else
                call append_table_value(b, values(k), k == size(values), ok, message, &
                    trailer=rank_one_trailer(k == size(values)))
            end if
            if (.not. ok) then
                source = str("")
                return
            end if
        end do
        source = b%to_str()
    end function emit_table_expr_1d

    !> Emit a rank-two table as a Fortran RESHAPE over the same flat typed
    !> constructor. Fortran array constructors are rank one; column-major
    !> flattening therefore preserves the declared `(rows, columns)` shape.
    function emit_table_expr_2d(name, values, declared_kind, comments, ok, message) &
            result(source)
        character(*), intent(in) :: name, declared_kind
        type(expr_t), intent(in) :: values(:, :)
        character(*), intent(in), optional :: comments(:, :)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(strbuf_t) :: b
        integer :: i, j, flat, total, shape2(2)

        call validate_table_header(name, declared_kind, size(values), ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        if (present(comments)) then
            if (any(shape(comments) /= shape(values))) then
                ok = .false.
                message = "table emitter: comment shape does not match table shape"
                source = str("")
                return
            end if
        end if

        total = size(values)
        shape2(1) = size(values, 1)
        shape2(2) = size(values, 2)
        call append_table_header(b, name, declared_kind, &
            shape2)
        flat = 0
        do j = 1, size(values, 2)
            do i = 1, size(values, 1)
                flat = flat + 1
                if (present(comments)) then
                    call append_table_value(b, values(i, j), flat == total, ok, &
                        message, comments(i, j), trailer=table_trailer(size(values, 1), &
                        size(values, 2), flat == total))
                else
                    call append_table_value(b, values(i, j), flat == total, ok, message, &
                        trailer=table_trailer(size(values, 1), size(values, 2), flat == total))
                end if
                if (.not. ok) then
                    source = str("")
                    return
                end if
            end do
        end do
        source = b%to_str()
    end function emit_table_expr_2d

    function emit_table_quadratic_1d(name, values, declared_kind, comments, ok, message) &
            result(source)
        character(*), intent(in) :: name, declared_kind
        type(quadratic_t), intent(in) :: values(:)
        character(*), intent(in), optional :: comments(:)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(strbuf_t) :: b
        integer :: k, shape1(1)

        call validate_table_header(name, declared_kind, size(values), ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        if (present(comments)) then
            if (size(comments) /= size(values)) then
                ok = .false.
                message = "table emitter: comment count does not match table size"
                source = str("")
                return
            end if
        end if

        shape1(1) = size(values)
        call append_table_header(b, name, declared_kind, shape1)
        do k = 1, size(values)
            if (present(comments)) then
                call append_quadratic_value(b, values(k), k == size(values), ok, &
                    message, comments(k), trailer=rank_one_trailer(k == size(values)))
            else
                call append_quadratic_value(b, values(k), k == size(values), ok, message, &
                    trailer=rank_one_trailer(k == size(values)))
            end if
            if (.not. ok) then
                source = str("")
                return
            end if
        end do
        source = b%to_str()
    end function emit_table_quadratic_1d

    function emit_table_quadratic_2d(name, values, declared_kind, comments, ok, message) &
            result(source)
        character(*), intent(in) :: name, declared_kind
        type(quadratic_t), intent(in) :: values(:, :)
        character(*), intent(in), optional :: comments(:, :)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(strbuf_t) :: b
        integer :: i, j, flat, total, shape2(2)

        call validate_table_header(name, declared_kind, size(values), ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        if (present(comments)) then
            if (any(shape(comments) /= shape(values))) then
                ok = .false.
                message = "table emitter: comment shape does not match table shape"
                source = str("")
                return
            end if
        end if

        total = size(values)
        shape2(1) = size(values, 1)
        shape2(2) = size(values, 2)
        call append_table_header(b, name, declared_kind, &
            shape2)
        flat = 0
        do j = 1, size(values, 2)
            do i = 1, size(values, 1)
                flat = flat + 1
                if (present(comments)) then
                    call append_quadratic_value(b, values(i, j), flat == total, ok, &
                        message, comments(i, j), trailer=table_trailer(size(values, 1), &
                        size(values, 2), flat == total))
                else
                    call append_quadratic_value(b, values(i, j), flat == total, ok, message, &
                        trailer=table_trailer(size(values, 1), size(values, 2), flat == total))
                end if
                if (.not. ok) then
                    source = str("")
                    return
                end if
            end do
        end do
        source = b%to_str()
    end function emit_table_quadratic_2d

    subroutine append_table_header(b, name, declared_kind, shape)
        type(strbuf_t), intent(inout) :: b
        character(*), intent(in) :: name, declared_kind
        integer, intent(in) :: shape(:)

        call b%append(declared_kind//", parameter :: "//name//"(")
        call b%append(chars(str(shape(1))))
        if (size(shape) == 2) then
            call b%append(",")
            call b%append(chars(str(shape(2))))
        end if
        if (size(shape) == 1) then
            call b%append(") = [ "//declared_kind//" :: &")
        else
            call b%append(") = reshape([ "//declared_kind//" :: &")
        end if
        call b%newline()
    end subroutine append_table_header

    subroutine validate_table_header(name, declared_kind, count, ok, message)
        character(*), intent(in) :: name, declared_kind
        integer, intent(in) :: count
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        ok = .false.
        message = ""
        if (.not. valid_identifier(name)) then
            message = "table emitter: table name is invalid"
            return
        end if
        if (len_trim(declared_kind) == 0) then
            message = "table emitter: declared kind is empty"
            return
        end if
        if (count == 0) then
            message = "table emitter: table is empty"
            return
        end if
        ok = .true.
    end subroutine validate_table_header

    subroutine append_table_value(b, value, last, ok, message, comment, trailer)
        type(strbuf_t), intent(inout) :: b
        type(expr_t), intent(in) :: value
        logical, intent(in) :: last
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        character(*), intent(in), optional :: comment
        character(*), intent(in), optional :: trailer
        type(str_t) :: rendered

        rendered = print_expr_in(value, dialect(DIA_FORTRAN), ok)
        if (.not. ok) then
            message = "table emitter: value is not Fortran-representable"
            return
        end if
        call append_table_text(b, chars(rendered), last, comment, trailer)
        message = ""
    end subroutine append_table_value

    subroutine append_quadratic_value(b, value, last, ok, message, comment, trailer)
        type(strbuf_t), intent(inout) :: b
        type(quadratic_t), intent(in) :: value
        logical, intent(in) :: last
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        character(*), intent(in), optional :: comment
        character(*), intent(in), optional :: trailer

        ok = value%radicand > 1
        if (.not. ok) then
            message = "table emitter: quadratic radicand is invalid"
            return
        end if
        call append_table_text(b, quadratic_fortran_text(value), last, comment, trailer)
        message = ""
    end subroutine append_quadratic_value

    subroutine append_table_text(b, rendered, last, comment, trailer)
        type(strbuf_t), intent(inout) :: b
        character(*), intent(in) :: rendered
        logical, intent(in) :: last
        character(*), intent(in), optional :: comment
        character(*), intent(in), optional :: trailer

        call b%append("    ")
        call b%append(rendered)
        if (last .and. present(trailer)) call b%append(trailer)
        if (.not. last) call b%append(", &")
        if (present(comment)) then
            if (len_trim(comment) > 0) then
                call b%append("  ! ")
                call b%append(trim(comment))
            end if
        end if
        call b%newline()
    end subroutine append_table_text

    function quadratic_fortran_text(value) result(text)
        type(quadratic_t), intent(in) :: value
        character(:), allocatable :: text
        character(:), allocatable :: a, b, root, d

        a = rational_fortran_text(chars(value%a))
        b = rational_fortran_text(chars(value%b))
        d = integer_fortran_text(value%radicand)
        root = "sqrt("//d//")"
        if (b == "0.0d0") then
            text = a
        else if (a == "0.0d0") then
            text = b//"*"//root
        else
            text = a//" + ("//b//")*"//root
        end if
    end function quadratic_fortran_text

    function rational_fortran_text(value) result(text)
        character(*), intent(in) :: value
        character(:), allocatable :: text
        integer :: slash

        slash = index(value, "/")
        if (slash == 0) then
            text = trim(value)//".0d0"
        else
            text = trim(value(:slash - 1))//".0d0/"//trim(value(slash + 1:))//".0d0"
        end if
    end function rational_fortran_text

    function integer_fortran_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(32) :: buffer

        write (buffer, "(i0)") value
        text = trim(buffer)//".0d0"
    end function integer_fortran_text

    function rank_one_trailer(last) result(text)
        logical, intent(in) :: last
        character(:), allocatable :: text

        text = ""
        if (last) text = " ]"
    end function rank_one_trailer

    function table_trailer(rows, columns, last) result(text)
        integer, intent(in) :: rows, columns
        logical, intent(in) :: last
        character(:), allocatable :: text

        text = ""
        if (last) then
            text = "], ["//chars(str(rows))//", "//chars(str(columns))//"]"
            text = text//")"
        end if
    end function table_trailer

    function emit_fortran_kernel_ir(ir, spec, ok, message) result(source)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(kernel_ir_t) :: prepared

        call validate(ir, spec, BACKEND_FORTRAN, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        call apply_emission_policy(ir, spec%policy, prepared, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        source = emit_source(prepared, spec, BACKEND_FORTRAN)
    end function emit_fortran_kernel_ir

    function emit_cuda_device_ir(ir, spec, ok, message) result(source)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(kernel_ir_t) :: prepared

        call validate(ir, spec, BACKEND_CUDA, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        call apply_emission_policy(ir, spec%policy, prepared, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        source = emit_source(prepared, spec, BACKEND_CUDA)
    end function emit_cuda_device_ir

    !> Apply source-level policies to a validated, topological IR. The rewrite
    !> keeps one output node for every input node (or aliases it to a child),
    !> so provenance remains easy to inspect while neutral constants disappear
    !> before declarations and assignments are emitted.
    subroutine apply_emission_policy(input, policy, output, ok, message)
        type(kernel_ir_t), intent(in) :: input
        type(kernel_emission_policy_t), intent(in) :: policy
        type(kernel_ir_t), intent(out) :: output
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        integer, allocatable :: mapping(:), children(:)
        integer :: i, k, node_count, operand_count
        integer :: base, exponent, exponent_value
        integer :: empty_operands(0), pow_operands(2)
        real(dp) :: value
        logical :: folded

        call output%clear()
        ok = .false.
        message = ""
        if (policy%small_power_limit < 0) then
            message = "kernel emitter: small power limit is negative"
            return
        end if
        if (.not. allocated(input%nodes) .or. .not. allocated(input%operands) .or. &
            .not. allocated(input%outputs)) then
            message = "kernel emitter: IR storage is not allocated"
            return
        end if

        allocate (mapping(input%n_nodes), source=0)
        allocate (output%nodes(input%n_nodes))
        allocate (output%operands(input%n_operands))
        allocate (output%outputs(size(input%outputs)))
        node_count = 0
        operand_count = 0

        do i = 1, input%n_nodes
            select case (input%nodes(i)%operation)
            case (IR_LITERAL, IR_SYMBOL, IR_CONSTANT)
                call append_node(output, input%nodes(i)%operation, &
                    empty_operands, input%nodes(i)%value, &
                    input%nodes(i)%name, node_count, operand_count)
                mapping(i) = node_count
            case (IR_FUNCTION)
                allocate (children(input%nodes(i)%n_operands))
                do k = 1, size(children)
                    children(k) = mapping(input%operands( &
                        input%nodes(i)%first_operand + k - 1))
                end do
                call append_node(output, IR_FUNCTION, children, &
                    input%nodes(i)%value, input%nodes(i)%name, node_count, &
                    operand_count)
                mapping(i) = node_count
                deallocate (children)
            case (IR_ADD, IR_MUL)
                allocate (children(input%nodes(i)%n_operands))
                do k = 1, size(children)
                    children(k) = mapping(input%operands( &
                        input%nodes(i)%first_operand + k - 1))
                end do
                call fold_reduction(output, input%nodes(i)%operation, children, &
                    policy%fold_exact_constants, node_count, operand_count, &
                    mapping(i), folded)
                deallocate (children)
            case (IR_POW)
                base = mapping(input%operands(input%nodes(i)%first_operand))
                exponent = mapping(input%operands( &
                    input%nodes(i)%first_operand + 1))
                folded = .false.
                if (policy%fold_exact_constants) then
                    if (literal_equals(output, exponent, 1.0_dp)) then
                        mapping(i) = base
                        folded = .true.
                    else if (literal_equals(output, exponent, 0.0_dp) .and. &
                            .not. literal_equals(output, base, 0.0_dp)) then
                        call append_literal(output, 1.0_dp, node_count, &
                            operand_count, mapping(i))
                        folded = .true.
                    else if (literal_equals(output, base, 1.0_dp)) then
                        call append_literal(output, 1.0_dp, node_count, &
                            operand_count, mapping(i))
                        folded = .true.
                    else if (literal_equals(output, base, 0.0_dp) .and. &
                            literal_integer(output, exponent, exponent_value) .and. &
                            exponent_value > 0) then
                        call append_literal(output, 0.0_dp, node_count, &
                            operand_count, mapping(i))
                        folded = .true.
                    end if
                end if
                if (.not. folded .and. policy%eliminate_constant_divisions .and. &
                    literal_integer(output, exponent, exponent_value) .and. &
                    exponent_value == -1 .and. &
                    literal_value(output, base, value) .and. value /= 0.0_dp) then
                    call append_literal(output, 1.0_dp/value, node_count, &
                        operand_count, mapping(i))
                    folded = .true.
                end if
                if (.not. folded) then
                    pow_operands(1) = base
                    pow_operands(2) = exponent
                    call append_node(output, IR_POW, pow_operands, 0.0_dp, &
                        str(""), node_count, operand_count)
                    mapping(i) = node_count
                end if
            case default
                message = "kernel emitter: policy saw unknown IR operation"
                return
            end select
        end do

        do i = 1, size(input%outputs)
            output%outputs(i) = mapping(input%outputs(i))
        end do
        output%n_nodes = node_count
        output%n_operands = operand_count
        call trim_ir_storage(output)
        ok = .true.
    end subroutine apply_emission_policy

    subroutine append_node(ir, operation, operands, value, name, node_count, &
            operand_count)
        type(kernel_ir_t), intent(inout) :: ir
        integer, intent(in) :: operation, operands(:)
        real(dp), intent(in) :: value
        type(str_t), intent(in) :: name
        integer, intent(inout) :: node_count, operand_count

        node_count = node_count + 1
        ir%nodes(node_count)%operation = operation
        ir%nodes(node_count)%value = value
        ir%nodes(node_count)%name = name
        ir%nodes(node_count)%first_operand = operand_count + 1
        ir%nodes(node_count)%n_operands = size(operands)
        if (size(operands) > 0) then
            ir%operands(operand_count + 1:operand_count + size(operands)) = operands
            operand_count = operand_count + size(operands)
        end if
    end subroutine append_node

    subroutine append_literal(ir, value, node_count, operand_count, index)
        type(kernel_ir_t), intent(inout) :: ir
        real(dp), intent(in) :: value
        integer, intent(inout) :: node_count, operand_count
        integer, intent(out) :: index
        integer :: empty_operands(0)

        call append_node(ir, IR_LITERAL, empty_operands, value, str(""), &
            node_count, operand_count)
        index = node_count
    end subroutine append_literal

    subroutine fold_reduction(ir, operation, children, enabled, node_count, &
            operand_count, index, folded)
        type(kernel_ir_t), intent(inout) :: ir
        integer, intent(in) :: operation, children(:)
        logical, intent(in) :: enabled
        integer, intent(inout) :: node_count, operand_count
        integer, intent(out) :: index
        logical, intent(out) :: folded

        integer, allocatable :: retained(:)
        integer :: k, nretained
        real(dp) :: value

        folded = .false.
        nretained = 0
        allocate (retained(size(children)))
        if (enabled) then
            if (operation == IR_MUL) then
                do k = 1, size(children)
                    if (literal_equals(ir, children(k), 0.0_dp)) then
                        call append_literal(ir, 0.0_dp, node_count, &
                            operand_count, index)
                        deallocate (retained)
                        folded = .true.
                        return
                    end if
                end do
            end if
            do k = 1, size(children)
                if (operation == IR_ADD .and. literal_equals(ir, children(k), &
                    0.0_dp)) cycle
                if (operation == IR_MUL .and. literal_equals(ir, children(k), &
                    1.0_dp)) cycle
                nretained = nretained + 1
                retained(nretained) = children(k)
            end do
        else
            retained = children
            nretained = size(children)
        end if

        if (nretained == 0) then
            value = 0.0_dp
            if (operation == IR_MUL) value = 1.0_dp
            call append_literal(ir, value, node_count, operand_count, index)
            folded = .true.
        else if (nretained == 1) then
            index = retained(1)
            folded = .true.
        else if (nretained < size(children)) then
            call append_node(ir, operation, retained(1:nretained), 0.0_dp, &
                str(""), node_count, operand_count)
            index = node_count
            folded = .true.
        else
            call append_node(ir, operation, children, 0.0_dp, str(""), &
                node_count, operand_count)
            index = node_count
        end if
        deallocate (retained)
    end subroutine fold_reduction

    logical function literal_value(ir, index, value)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index
        real(dp), intent(out) :: value

        value = 0.0_dp
        literal_value = index >= 1 .and. index <= size(ir%nodes)
        if (.not. literal_value) then
            value = 0.0_dp
            return
        end if
        literal_value = ir%nodes(index)%operation == IR_LITERAL
        if (literal_value) value = ir%nodes(index)%value
    end function literal_value

    logical function literal_equals(ir, index, expected)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index
        real(dp), intent(in) :: expected
        real(dp) :: value

        literal_equals = literal_value(ir, index, value) .and. value == expected
    end function literal_equals

    logical function literal_integer(ir, index, value)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index
        integer, intent(out) :: value
        real(dp) :: real_value

        literal_integer = literal_value(ir, index, real_value)
        if (.not. literal_integer) then
            value = 0
            return
        end if
        value = nint(real_value)
        literal_integer = real(value, dp) == real_value
    end function literal_integer

    subroutine trim_ir_storage(ir)
        type(kernel_ir_t), intent(inout) :: ir
        type(kernel_ir_node_t), allocatable :: nodes(:)
        integer, allocatable :: operands(:)

        allocate (nodes(ir%n_nodes))
        if (ir%n_nodes > 0) nodes = ir%nodes(1:ir%n_nodes)
        allocate (operands(ir%n_operands))
        if (ir%n_operands > 0) operands = ir%operands(1:ir%n_operands)
        call move_alloc(nodes, ir%nodes)
        call move_alloc(operands, ir%operands)
    end subroutine trim_ir_storage

    function emit_source(ir, spec, backend) result(source)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        integer, intent(in) :: backend
        type(str_t) :: source
        type(strbuf_t) :: b
        character(:), allocatable :: prefix, rhs
        logical :: ok, emit_openmp, emit_openacc
        logical, allocatable :: skip_nodes(:)
        integer, allocatable :: use_counts(:)
        character(:), allocatable :: message
        integer :: k

        call target_directives(spec%target, .false., .false., emit_openmp, &
            emit_openacc)
        call count_ir_uses(ir, use_counts)
        call mark_fma_children(ir, spec%policy, use_counts, skip_nodes)

        prefix = chars(spec%temp_prefix)
        if (len(prefix) == 0) prefix = "t"

        if (backend == BACKEND_FORTRAN) then
            call b%append("! Generated by fortsym. Do not edit.")
            call b%newline()
            call append_provenance(b, spec, "!")
            if (spec%pure_procedure) call b%append("pure ")
            call b%append("subroutine ")
            call b%append(chars(spec%name))
            call b%append("(")
            call append_arguments(b, spec, .false.)
            call b%append(")")
            call b%newline()
            if (emit_openmp) then
                call b%append("    !$omp declare target")
                call b%newline()
            end if
            if (emit_openacc) then
                call b%append("    !$acc routine seq")
                call b%newline()
            end if
            call b%append("    use, intrinsic :: iso_fortran_env, only: real64")
            call b%newline()
            call b%append("    implicit none")
            call b%newline()
            if (size(spec%args) > 0) then
                call append_declaration(b, "real(real64), intent(in)", spec%args)
            end if
            if (size(spec%outputs) > 0) then
                call append_declaration(b, "real(real64), intent(out)", &
                    spec%outputs)
            end if
            if (count_compounds(ir, skip_nodes) > 0) then
                call append_temporary_declaration(b, prefix, ir, skip_nodes)
            end if
        else
            call b%append("/* Generated by fortsym. Do not edit. */")
            call b%newline()
            call append_provenance(b, spec, "//")
            call b%append("#include <math.h>")
            call b%newline()
            call b%append('extern "C" __device__ __forceinline__')
            call b%newline()
            call b%append("void ")
            call b%append(chars(spec%name))
            call b%append("(")
            call append_arguments(b, spec, .true.)
            call b%append(")")
            call b%newline()
            call b%append("{")
            call b%newline()
        end if

        call b%newline()
        do k = 1, ir%n_nodes
            if (.not. is_compound(ir%nodes(k)%operation)) cycle
            if (skip_nodes(k)) cycle
            rhs = render_node(ir, k, prefix, backend, spec%policy, use_counts, &
                ok, message)
            if (.not. ok) error stop "validated kernel IR became unrenderable"
            if (backend == BACKEND_FORTRAN) then
                call b%append("    ")
                call b%append(prefix)
                call b%append(chars(str(k)))
                call b%append(" = ")
                call b%append(rhs)
            else
                call b%append("    const double ")
                call b%append(prefix)
                call b%append(chars(str(k)))
                call b%append(" = ")
                call b%append(rhs)
                call b%append(";")
            end if
            call b%newline()
        end do

        do k = 1, size(ir%outputs)
            if (backend == BACKEND_FORTRAN) then
                call b%append("    ")
                call b%append(chars(spec%outputs(k)))
                call b%append(" = ")
                call b%append(operand_reference(ir, ir%outputs(k), prefix, &
                    backend))
            else
                call b%append("    *")
                call b%append(chars(spec%outputs(k)))
                call b%append(" = ")
                call b%append(operand_reference(ir, ir%outputs(k), prefix, &
                    backend))
                call b%append(";")
            end if
            call b%newline()
        end do

        if (backend == BACKEND_FORTRAN) then
            call b%append("end subroutine ")
            call b%append(chars(spec%name))
        else
            call b%append("}")
        end if
        call b%newline()
        source = b%to_str()
    end function emit_source

    subroutine validate(ir, spec, backend, ok, message)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        integer, intent(in) :: backend
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: k, j, operand, first, last
        character(:), allocatable :: prefix, temporary

        ok = .false.
        message = ""
        if (.not. valid_identifier(chars(spec%name))) then
            message = "kernel emitter: kernel name is invalid"
            return
        end if
        if (.not. target_is_valid(spec%target)) then
            message = "kernel emitter: target identity is invalid"
            return
        end if
        if (backend == BACKEND_FORTRAN .and. spec%target == TARGET_CUDA) then
            message = "kernel emitter: CUDA target requires CUDA emitter"
            return
        end if
        if (backend == BACKEND_CUDA .and. spec%target /= TARGET_DEFAULT .and. &
            spec%target /= TARGET_CUDA) then
            message = "kernel emitter: non-CUDA target requires Fortran emitter"
            return
        end if
        if (.not. allocated(spec%args)) then
            message = "kernel emitter: argument names are not allocated"
            return
        end if
        if (.not. allocated(spec%outputs)) then
            message = "kernel emitter: output names are not allocated"
            return
        end if
        if (size(spec%outputs) /= size(ir%outputs)) then
            message = "kernel emitter: output names do not match roots"
            return
        end if
        if (.not. allocated(ir%nodes) .or. .not. allocated(ir%operands)) then
            message = "kernel emitter: IR storage is not allocated"
            return
        end if
        if (ir%n_nodes /= size(ir%nodes) .or. &
            ir%n_operands /= size(ir%operands)) then
            message = "kernel emitter: IR counts do not match storage"
            return
        end if
        do k = 1, size(spec%args)
            if (.not. valid_identifier(chars(spec%args(k)))) then
                message = "kernel emitter: argument name is invalid"
                return
            end if
            if (duplicate_name(spec%args, k)) then
                message = "kernel emitter: argument names are duplicated"
                return
            end if
        end do
        do k = 1, size(spec%outputs)
            if (.not. valid_identifier(chars(spec%outputs(k)))) then
                message = "kernel emitter: output name is invalid"
                return
            end if
            if (duplicate_name(spec%outputs, k)) then
                message = "kernel emitter: output names are duplicated"
                return
            end if
            if (is_argument(chars(spec%outputs(k)), spec%args)) then
                message = "kernel emitter: input and output names overlap"
                return
            end if
        end do
        prefix = chars(spec%temp_prefix)
        if (len(prefix) == 0) prefix = "t"
        if (.not. valid_identifier(prefix)) then
            message = "kernel emitter: temporary prefix is invalid"
            return
        end if

        do k = 1, ir%n_nodes
            first = ir%nodes(k)%first_operand
            last = first + ir%nodes(k)%n_operands - 1
            if (ir%nodes(k)%n_operands > 0) then
                if (first < 1 .or. last > ir%n_operands) then
                    message = "kernel emitter: operand slice is invalid"
                    return
                end if
                do j = first, last
                    operand = ir%operands(j)
                    if (operand < 1 .or. operand >= k) then
                        message = "kernel emitter: IR is not topological"
                        return
                    end if
                end do
            end if
            select case (ir%nodes(k)%operation)
            case (IR_LITERAL, IR_SYMBOL, IR_CONSTANT)
                if (ir%nodes(k)%n_operands /= 0) then
                    message = "kernel emitter: atom has operands"
                    return
                end if
            case (IR_ADD, IR_MUL)
                if (ir%nodes(k)%n_operands < 1) then
                    message = "kernel emitter: reduction has no operands"
                    return
                end if
            case (IR_POW)
                if (ir%nodes(k)%n_operands /= 2) then
                    message = "kernel emitter: power does not have two operands"
                    return
                end if
            case (IR_FUNCTION)
                if (ir%nodes(k)%n_operands < 1) then
                    message = "kernel emitter: function has no operands"
                    return
                end if
                if (.not. function_arity_ok(chars(ir%nodes(k)%name), &
                    ir%nodes(k)%n_operands)) then
                    message = "kernel emitter: wrong arity for function "// &
                        chars(ir%nodes(k)%name)
                    return
                end if
                if (.not. supported_function(chars(ir%nodes(k)%name), backend)) then
                    message = "kernel emitter: unsupported "//backend_name(backend)// &
                        " function "//chars(ir%nodes(k)%name)
                    return
                end if
            case default
                message = "kernel emitter: unknown IR operation"
                return
            end select
            if (ir%nodes(k)%operation == IR_SYMBOL) then
                if (.not. is_argument(chars(ir%nodes(k)%name), spec%args)) then
                    message = "kernel emitter: symbol is not an input argument: "// &
                        chars(ir%nodes(k)%name)
                    return
                end if
            else if (ir%nodes(k)%operation == IR_CONSTANT) then
                if (.not. supported_constant(chars(ir%nodes(k)%name), backend)) then
                    message = "kernel emitter: unsupported "//backend_name(backend)// &
                        " constant "//chars(ir%nodes(k)%name)
                    return
                end if
            end if
            if (is_compound(ir%nodes(k)%operation)) then
                temporary = prefix//chars(str(k))
                if (is_argument(temporary, spec%args) .or. &
                    is_argument(temporary, spec%outputs)) then
                    message = "kernel emitter: temporary collides with an argument: "// &
                        temporary
                    return
                end if
            end if
        end do
        do k = 1, size(ir%outputs)
            if (ir%outputs(k) < 1 .or. ir%outputs(k) > ir%n_nodes) then
                message = "kernel emitter: output root is invalid"
                return
            end if
        end do
        ok = .true.
    end subroutine validate

    logical function valid_identifier(name)
        character(*), intent(in) :: name
        integer :: k, code

        valid_identifier = .false.
        if (len(name) == 0) return
        code = iachar(name(1:1))
        if (.not. ((code >= iachar("A") .and. code <= iachar("Z")) .or. &
            (code >= iachar("a") .and. code <= iachar("z")))) return
        do k = 2, len(name)
            code = iachar(name(k:k))
            if (.not. ((code >= iachar("A") .and. code <= iachar("Z")) .or. &
                (code >= iachar("a") .and. code <= iachar("z")) .or. &
                (code >= iachar("0") .and. code <= iachar("9")) .or. &
                code == iachar("_"))) return
        end do
        valid_identifier = .true.
    end function valid_identifier

    logical function duplicate_name(names, index)
        type(str_t), intent(in) :: names(:)
        integer, intent(in) :: index
        integer :: k

        duplicate_name = .false.
        do k = 1, index - 1
            if (chars(names(k)) == chars(names(index))) then
                duplicate_name = .true.
                return
            end if
        end do
    end function duplicate_name

    recursive function render_node(ir, index, prefix, backend, policy, use_counts, &
            ok, message) result(text)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index, backend
        character(*), intent(in) :: prefix
        type(kernel_emission_policy_t), intent(in) :: policy
        integer, intent(in) :: use_counts(:)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        character(:), allocatable :: text
        type(strbuf_t) :: b
        character(:), allocatable :: name, rendered_child
        integer :: k, operand, exponent

        ok = .false.
        message = ""
        select case (ir%nodes(index)%operation)
        case (IR_LITERAL)
            text = literal_text(ir%nodes(index)%value, backend)
        case (IR_SYMBOL)
            text = chars(ir%nodes(index)%name)
        case (IR_CONSTANT)
            name = chars(ir%nodes(index)%name)
            if (name == "pi") then
                text = constant_pi(backend)
            else
                text = constant_e(backend)
            end if
        case (IR_ADD, IR_MUL)
            call b%append("(")
            do k = 1, ir%nodes(index)%n_operands
                if (k > 1) then
                    if (ir%nodes(index)%operation == IR_ADD) then
                        call b%append(" + ")
                    else
                        call b%append(" * ")
                    end if
                end if
                operand = ir%operands(ir%nodes(index)%first_operand + k - 1)
                if (should_inline_fma(ir, index, operand, policy, use_counts)) then
                    rendered_child = render_node(ir, operand, prefix, backend, &
                        policy, use_counts, ok, message)
                    if (.not. ok) return
                    call b%append(rendered_child)
                else
                    call b%append(operand_reference(ir, operand, prefix, backend))
                end if
            end do
            call b%append(")")
            text = chars(b%to_str())
        case (IR_POW)
            operand = ir%operands(ir%nodes(index)%first_operand)
            if (policy%small_power_limit > 0 .and. &
                literal_integer(ir, ir%operands(ir%nodes(index)%first_operand + 1), &
                exponent) .and. exponent >= 2 .and. &
                exponent <= policy%small_power_limit) then
                call b%append("(")
                do k = 1, exponent
                    if (k > 1) call b%append(" * ")
                    call b%append(operand_reference(ir, operand, prefix, backend))
                end do
                call b%append(")")
                text = chars(b%to_str())
                ok = .true.
                return
            end if
            call b%append("(")
            call b%append(operand_reference(ir, operand, prefix, backend))
            if (backend == BACKEND_FORTRAN) then
                call b%append(" ** ")
            else
                call b%append(", ")
                call b%append(operand_reference(ir, &
                    ir%operands(ir%nodes(index)%first_operand + 1), prefix, backend))
                call b%append(")")
                text = "pow"//chars(b%to_str())
                ok = .true.
                return
            end if
            call b%append(operand_reference(ir, &
                ir%operands(ir%nodes(index)%first_operand + 1), prefix, backend))
            call b%append(")")
            text = chars(b%to_str())
        case (IR_FUNCTION)
            name = function_name(chars(ir%nodes(index)%name), backend)
            call b%append(name)
            call b%append("(")
            do k = 1, ir%nodes(index)%n_operands
                if (k > 1) call b%append(", ")
                operand = ir%operands(ir%nodes(index)%first_operand + k - 1)
                call b%append(operand_reference(ir, operand, prefix, backend))
            end do
            call b%append(")")
            text = chars(b%to_str())
        case default
            text = ""
            message = "kernel emitter: cannot render unknown operation"
            return
        end select
        ok = .true.
    end function render_node

    subroutine count_ir_uses(ir, use_counts)
        type(kernel_ir_t), intent(in) :: ir
        integer, allocatable, intent(out) :: use_counts(:)
        integer :: i, k, operand

        allocate (use_counts(ir%n_nodes), source=0)
        do i = 1, ir%n_nodes
            do k = 1, ir%nodes(i)%n_operands
                operand = ir%operands(ir%nodes(i)%first_operand + k - 1)
                use_counts(operand) = use_counts(operand) + 1
            end do
        end do
        do i = 1, size(ir%outputs)
            use_counts(ir%outputs(i)) = use_counts(ir%outputs(i)) + 1
        end do
    end subroutine count_ir_uses

    subroutine mark_fma_children(ir, policy, use_counts, skip_nodes)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emission_policy_t), intent(in) :: policy
        integer, intent(in) :: use_counts(:)
        logical, allocatable, intent(out) :: skip_nodes(:)
        integer :: i, k, operand

        allocate (skip_nodes(ir%n_nodes), source=.false.)
        if (.not. policy%shape_fma) return
        do i = 1, ir%n_nodes
            if (ir%nodes(i)%operation /= IR_ADD) cycle
            do k = 1, ir%nodes(i)%n_operands
                operand = ir%operands(ir%nodes(i)%first_operand + k - 1)
                if (should_inline_fma(ir, i, operand, policy, use_counts)) then
                    skip_nodes(operand) = .true.
                end if
            end do
        end do
    end subroutine mark_fma_children

    logical function should_inline_fma(ir, add_index, operand, policy, use_counts)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: add_index, operand
        type(kernel_emission_policy_t), intent(in) :: policy
        integer, intent(in) :: use_counts(:)

        should_inline_fma = .false.
        if (.not. policy%shape_fma) return
        if (ir%nodes(add_index)%operation /= IR_ADD) return
        if (operand < 1 .or. operand > ir%n_nodes) return
        should_inline_fma = ir%nodes(operand)%operation == IR_MUL .and. &
            ir%nodes(operand)%n_operands >= 2 .and. use_counts(operand) == 1
    end function should_inline_fma

    function operand_reference(ir, index, prefix, backend) result(text)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index, backend
        character(*), intent(in) :: prefix
        character(:), allocatable :: text

        if (is_compound(ir%nodes(index)%operation)) then
            text = prefix//chars(str(index))
        else
            text = render_atom(ir%nodes(index), backend)
        end if
    end function operand_reference

    function render_atom(node, backend) result(text)
        type(kernel_ir_node_t), intent(in) :: node
        integer, intent(in) :: backend
        character(:), allocatable :: text

        select case (node%operation)
        case (IR_LITERAL)
            text = literal_text(node%value, backend)
        case (IR_SYMBOL)
            text = chars(node%name)
        case (IR_CONSTANT)
            if (chars(node%name) == "pi") then
                text = constant_pi(backend)
            else
                text = constant_e(backend)
            end if
        case default
            text = ""
        end select
    end function render_atom

    function literal_text(value, backend) result(text)
        real(dp), intent(in) :: value
        integer, intent(in) :: backend
        character(:), allocatable :: text

        text = chars(str(value, "(es25.16e3)"))
        if (backend == BACKEND_FORTRAN) text = text//"_real64"
        if (value < 0.0_dp) text = "("//text//")"
    end function literal_text

    function constant_pi(backend) result(text)
        integer, intent(in) :: backend
        character(:), allocatable :: text

        if (backend == BACKEND_FORTRAN) then
            text = "acos(-1.0_real64)"
        else
            text = "acos(-1.0)"
        end if
    end function constant_pi

    function constant_e(backend) result(text)
        integer, intent(in) :: backend
        character(:), allocatable :: text

        if (backend == BACKEND_FORTRAN) then
            text = "exp(1.0_real64)"
        else
            text = "exp(1.0)"
        end if
    end function constant_e

    function function_name(name, backend) result(text)
        character(*), intent(in) :: name
        integer, intent(in) :: backend
        character(:), allocatable :: text

        text = name
        if (backend == BACKEND_CUDA) then
            if (name == "abs") text = "fabs"
            if (name == "gamma") text = "tgamma"
        end if
    end function function_name

    logical function supported_function(name, backend)
        character(*), intent(in) :: name
        integer, intent(in) :: backend

        supported_function = .false.
        select case (name)
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", &
                "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", "exp", &
                "log", "sqrt", "abs", "erf", "erfc")
            supported_function = .true.
        case ("gamma")
            supported_function = backend == BACKEND_FORTRAN .or. &
                backend == BACKEND_CUDA
        end select
    end function supported_function

    logical function function_arity_ok(name, n)
        character(*), intent(in) :: name
        integer, intent(in) :: n

        if (name == "atan2") then
            function_arity_ok = n == 2
        else
            function_arity_ok = n == 1
        end if
    end function function_arity_ok

    logical function supported_constant(name, backend)
        character(*), intent(in) :: name
        integer, intent(in) :: backend

        supported_constant = name == "pi" .or. name == "e"
    end function supported_constant

    logical function is_argument(name, args)
        character(*), intent(in) :: name
        type(str_t), intent(in) :: args(:)
        integer :: k

        is_argument = .false.
        do k = 1, size(args)
            if (chars(args(k)) == name) then
                is_argument = .true.
                return
            end if
        end do
    end function is_argument

    logical function is_compound(operation)
        integer, intent(in) :: operation

        is_compound = operation == IR_ADD .or. operation == IR_MUL .or. &
            operation == IR_POW .or. operation == IR_FUNCTION
    end function is_compound

    integer function count_compounds(ir, skip_nodes)
        type(kernel_ir_t), intent(in) :: ir
        logical, intent(in), optional :: skip_nodes(:)
        integer :: k

        count_compounds = 0
        do k = 1, ir%n_nodes
            if (is_compound(ir%nodes(k)%operation)) count_compounds = &
                count_compounds + 1
            if (present(skip_nodes)) then
                if (skip_nodes(k) .and. is_compound(ir%nodes(k)%operation)) then
                    count_compounds = count_compounds - 1
                end if
            end if
        end do
    end function count_compounds

    subroutine append_arguments(b, spec, cuda)
        type(strbuf_t), intent(inout) :: b
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(in) :: cuda
        integer :: k

        do k = 1, size(spec%args)
            if (k > 1) call b%append(", ")
            if (cuda) call b%append("const double ")
            call b%append(chars(spec%args(k)))
        end do
        do k = 1, size(spec%outputs)
            if (size(spec%args) > 0 .or. k > 1) call b%append(", ")
            if (cuda) call b%append("double* ")
            call b%append(chars(spec%outputs(k)))
        end do
    end subroutine append_arguments

    subroutine append_declaration(b, declaration, names)
        type(strbuf_t), intent(inout) :: b
        character(*), intent(in) :: declaration
        type(str_t), intent(in) :: names(:)
        integer :: k

        call b%append("    ")
        call b%append(declaration)
        call b%append(" :: ")
        do k = 1, size(names)
            if (k > 1) call b%append(", ")
            call b%append(chars(names(k)))
        end do
        call b%newline()
    end subroutine append_declaration

    subroutine append_temporary_declaration(b, prefix, ir, skip_nodes)
        type(strbuf_t), intent(inout) :: b
        character(*), intent(in) :: prefix
        type(kernel_ir_t), intent(in) :: ir
        logical, intent(in), optional :: skip_nodes(:)
        integer :: k
        logical :: first

        call b%append("    real(real64) :: ")
        first = .true.
        do k = 1, ir%n_nodes
            if (.not. is_compound(ir%nodes(k)%operation)) cycle
            if (present(skip_nodes)) then
                if (skip_nodes(k)) cycle
            end if
            if (.not. first) call b%append(", ")
            call b%append(prefix//chars(str(k)))
            first = .false.
        end do
        call b%newline()
    end subroutine append_temporary_declaration

    subroutine append_provenance(b, spec, comment)
        type(strbuf_t), intent(inout) :: b
        type(kernel_emit_spec_t), intent(in) :: spec
        character(*), intent(in) :: comment

        call b%append(comment//" Generator: ")
        call b%append(chars(spec%generator))
        call b%newline()
        if (len(chars(spec%generator_revision)) > 0) then
            call b%append(comment//" Generator revision: ")
            call b%append(chars(spec%generator_revision))
            call b%newline()
        end if
        call b%append(comment//" Regenerate with: ")
        if (len(chars(spec%regenerate_command)) > 0) then
            call b%append(chars(spec%regenerate_command))
        else
            call b%append("fo exec ")
            call b%append(chars(spec%generator))
        end if
        call b%newline()
        call b%newline()
    end subroutine append_provenance

    function backend_name(backend) result(name)
        integer, intent(in) :: backend
        character(:), allocatable :: name

        if (backend == BACKEND_CUDA) then
            name = "CUDA"
        else
            name = "Fortran"
        end if
    end function backend_name

end module fortsym_kernel_emit
