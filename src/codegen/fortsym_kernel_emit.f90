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
    implicit none
    private

    public :: kernel_emit_spec_t
    public :: emit_fortran_kernel_ir, emit_cuda_device_ir
    public :: emit_table

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
        integer :: k

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

        call append_table_header(b, name, declared_kind, [size(values)])
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
        integer :: i, j, flat, total

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
        call append_table_header(b, name, declared_kind, &
            [size(values, 1), size(values, 2)])
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
        integer :: k

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

        call append_table_header(b, name, declared_kind, [size(values)])
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
        integer :: i, j, flat, total

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
        call append_table_header(b, name, declared_kind, &
            [size(values, 1), size(values, 2)])
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

        call validate(ir, spec, BACKEND_FORTRAN, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        source = emit_source(ir, spec, BACKEND_FORTRAN)
    end function emit_fortran_kernel_ir

    function emit_cuda_device_ir(ir, spec, ok, message) result(source)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source

        call validate(ir, spec, BACKEND_CUDA, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        source = emit_source(ir, spec, BACKEND_CUDA)
    end function emit_cuda_device_ir

    function emit_source(ir, spec, backend) result(source)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        integer, intent(in) :: backend
        type(str_t) :: source
        type(strbuf_t) :: b
        character(:), allocatable :: prefix, rhs
        logical :: ok
        character(:), allocatable :: message
        integer :: k

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
            if (count_compounds(ir) > 0) then
                call append_temporary_declaration(b, prefix, ir)
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
            rhs = render_node(ir, k, prefix, backend, ok, message)
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

    function render_node(ir, index, prefix, backend, ok, message) result(text)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index, backend
        character(*), intent(in) :: prefix
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        character(:), allocatable :: text
        type(strbuf_t) :: b
        character(:), allocatable :: name
        integer :: k, operand

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
                call b%append(operand_reference(ir, operand, prefix, backend))
            end do
            call b%append(")")
            text = chars(b%to_str())
        case (IR_POW)
            operand = ir%operands(ir%nodes(index)%first_operand)
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

    integer function count_compounds(ir)
        type(kernel_ir_t), intent(in) :: ir
        integer :: k

        count_compounds = 0
        do k = 1, ir%n_nodes
            if (is_compound(ir%nodes(k)%operation)) count_compounds = &
                count_compounds + 1
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

    subroutine append_temporary_declaration(b, prefix, ir)
        type(strbuf_t), intent(inout) :: b
        character(*), intent(in) :: prefix
        type(kernel_ir_t), intent(in) :: ir
        integer :: k
        logical :: first

        call b%append("    real(real64) :: ")
        first = .true.
        do k = 1, ir%n_nodes
            if (.not. is_compound(ir%nodes(k)%operation)) cycle
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
