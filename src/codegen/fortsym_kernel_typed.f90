module fortsym_kernel_typed
    ! Operator-overloaded typed kernel emission.
    !
    ! fortsym_kernel_emit and fortsym_kernel own two Fortran spellings of a
    ! kernel: real(dp) scalar arithmetic and a scalar_type string that still
    ! assumes ordinary intrinsic operators. Automatic differentiation, dual
    ! or interval numbers, and other operator-overloaded numeric types need a
    ! third spelling that emits the *same* CSE'd operation graph through
    ! +, -, *, /, integer powers, and named elementary-function calls only,
    ! so that a caller's own derived type (with its own operator and
    ! function overloads) computes the answer instead of real64 arithmetic.
    !
    ! This module extends the backend-neutral kernel_ir_t lowering already
    ! used by fortsym_kernel_emit rather than duplicating it: lower_kernel_ir
    ! freezes the expression DAG (already hash-consed, so a shared
    ! subexpression is already one IR node with several parents -- CSE is
    ! free) and this module is a second, typed renderer of that same IR.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, strbuf_t, str, chars, operator(//)
    use fortsym_expr, only: expr_t
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir, &
        IR_LITERAL, IR_SYMBOL, IR_CONSTANT, IR_ADD, IR_MUL, IR_POW, IR_FUNCTION
    implicit none
    private

    public :: function_name_map_t, typed_kernel_spec_t, emit_typed_kernel

    integer, parameter :: dp = real64

    !> Canonical fortsym function name -> caller-chosen procedure name (for
    !> example "besselk" -> "my_bessel_k"). A canonical name absent from the
    !> map is emitted verbatim, which is correct whenever the caller's type
    !> overloads the intrinsic name itself through a generic interface --
    !> the common convention for operator-overloaded numeric types, and
    !> exactly how "sin", "cos", "sqrt", and "exp" are expected to resolve.
    type :: function_name_map_t
        type(str_t), allocatable :: canonical(:)
        type(str_t), allocatable :: spelling(:)
    end type function_name_map_t

    !> Everything needed to render one typed kernel subroutine.
    type :: typed_kernel_spec_t
        type(str_t) :: name
        !> Input dummy argument names, in argument order.
        type(str_t), allocatable :: args(:)
        !> Output dummy argument names, one per root expression.
        type(str_t), allocatable :: outputs(:)
        !> Derived-type name used for every argument, output, and temporary
        !> (e.g. "dual_t"). It must already provide +, -, *, /, integer
        !> `**`, and whichever elementary functions the kernel calls.
        type(str_t) :: type_name
        !> Prefix for generated temporaries. Defaults to "t" when empty.
        type(str_t) :: temp_prefix
        !> Empty: real literals are emitted bare (e.g. "2.0_dp"), relying on
        !> a mixed-type operator overload between real(dp) and type_name.
        !> Nonempty: every literal is instead wrapped in a call to this
        !> constructor name, e.g. "dual_t(2.0_dp)".
        type(str_t) :: literal_constructor
        type(function_name_map_t) :: functions
    end type typed_kernel_spec_t

contains

    !> Emit a typed kernel subroutine computing `roots` (one per
    !> spec%outputs entry, in order) purely through +, -, *, /, integer
    !> powers, and named function calls over spec%type_name.
    function emit_typed_kernel(roots, spec, ok, message) result(source)
        type(expr_t), intent(in) :: roots(:)
        type(typed_kernel_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source

        type(kernel_ir_t) :: ir
        type(str_t), allocatable :: ref(:)
        logical, allocatable :: needs_temp(:)
        type(strbuf_t) :: body, decls
        type(str_t) :: prefix, tname
        integer :: k

        source = str("")
        ok = .false.
        message = ""

        if (.not. allocated(spec%args) .or. size(spec%args) == 0) then
            message = "emit_typed_kernel: no argument names supplied"
            return
        end if
        if (.not. allocated(spec%outputs) .or. size(spec%outputs) /= size(roots)) then
            message = "emit_typed_kernel: outputs must have one name per root"
            return
        end if
        if (spec%type_name%is_empty()) then
            message = "emit_typed_kernel: type_name is empty"
            return
        end if

        call lower_kernel_ir(roots, ir, ok, message)
        if (.not. ok) return

        prefix = spec%temp_prefix
        if (prefix%is_empty()) prefix = str("t")
        tname = spec%type_name

        allocate (ref(ir%n_nodes))
        allocate (needs_temp(ir%n_nodes), source=.false.)
        do k = 1, ir%n_nodes
            call render_node(ir, k, spec, ref, needs_temp, body, ok, message)
            if (.not. ok) return
        end do

        call declare_temps(decls, tname, prefix, needs_temp)
        call append_outputs(body, spec, ir, ref)

        source = assemble(spec, decls, body)
        ok = .true.
    end function emit_typed_kernel

    subroutine declare_temps(decls, tname, prefix, needs_temp)
        type(strbuf_t), intent(inout) :: decls
        type(str_t), intent(in) :: tname, prefix
        logical, intent(in) :: needs_temp(:)
        integer :: k, n

        n = count(needs_temp)
        if (n == 0) return
        call decls%append("    type(")
        call decls%append(tname)
        call decls%append(") :: ")
        n = 0
        do k = 1, size(needs_temp)
            if (.not. needs_temp(k)) cycle
            if (n > 0) call decls%append(", ")
            call decls%append(prefix)
            call decls%append(k)
            n = n + 1
        end do
        call decls%newline()
    end subroutine declare_temps

    subroutine append_outputs(body, spec, ir, ref)
        type(strbuf_t), intent(inout) :: body
        type(typed_kernel_spec_t), intent(in) :: spec
        type(kernel_ir_t), intent(in) :: ir
        type(str_t), intent(in) :: ref(:)
        integer :: k

        do k = 1, size(spec%outputs)
            call body%append("    ")
            call body%append(spec%outputs(k))
            call body%append(" = ")
            call body%append(ref(ir%outputs(k)))
            call body%newline()
        end do
    end subroutine append_outputs

    function assemble(spec, decls, body) result(source)
        type(typed_kernel_spec_t), intent(in) :: spec
        type(strbuf_t), intent(inout) :: decls, body
        type(str_t) :: source
        type(strbuf_t) :: out
        integer :: k

        call out%append("subroutine ")
        call out%append(spec%name)
        call out%append("(")
        do k = 1, size(spec%args)
            if (k > 1) call out%append(", ")
            call out%append(spec%args(k))
        end do
        do k = 1, size(spec%outputs)
            call out%append(", ")
            call out%append(spec%outputs(k))
        end do
        call out%append(")")
        call out%newline()
        do k = 1, size(spec%args)
            call out%append("    type(")
            call out%append(spec%type_name)
            call out%append("), intent(in) :: ")
            call out%append(spec%args(k))
            call out%newline()
        end do
        do k = 1, size(spec%outputs)
            call out%append("    type(")
            call out%append(spec%type_name)
            call out%append("), intent(out) :: ")
            call out%append(spec%outputs(k))
            call out%newline()
        end do
        call out%append(decls%to_str())
        call out%append(body%to_str())
        call out%append("end subroutine ")
        call out%append(spec%name)
        call out%newline()
        source = out%to_str()
    end function assemble

    !> Render one topologically-ordered IR node: either bind ref(k) directly
    !> (symbols and literals need no statement) or append one assignment
    !> statement that computes a fresh temporary and bind ref(k) to it.
    subroutine render_node(ir, k, spec, ref, needs_temp, body, ok, message)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: k
        type(typed_kernel_spec_t), intent(in) :: spec
        type(str_t), intent(inout) :: ref(:)
        logical, intent(inout) :: needs_temp(:)
        type(strbuf_t), intent(inout) :: body
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        integer :: op, base_idx, exp_idx
        type(str_t) :: prefix, expr_text, fname

        ok = .true.
        message = ""
        op = ir%nodes(k)%operation

        select case (op)
        case (IR_SYMBOL)
            call find_arg(spec, chars(ir%nodes(k)%name), ref(k), ok)
            if (.not. ok) then
                message = "emit_typed_kernel: symbol not in argument list: "// &
                    chars(ir%nodes(k)%name)
            end if
            return
        case (IR_LITERAL, IR_CONSTANT)
            ref(k) = literal_text(ir%nodes(k)%value, spec)
            return
        end select

        needs_temp(k) = .true.
        prefix = spec%temp_prefix
        if (prefix%is_empty()) prefix = str("t")

        select case (op)
        case (IR_ADD)
            expr_text = infix_text(ir, k, ref, "+")
        case (IR_MUL)
            expr_text = infix_text(ir, k, ref, "*")
        case (IR_POW)
            base_idx = ir%operands(ir%nodes(k)%first_operand)
            exp_idx = ir%operands(ir%nodes(k)%first_operand + 1)
            call power_text(ir, spec, ref, base_idx, exp_idx, expr_text, ok, &
                message)
            if (.not. ok) return
        case (IR_FUNCTION)
            call function_spelling(spec, chars(ir%nodes(k)%name), fname)
            expr_text = call_text(ir, k, ref, fname)
        case default
            ok = .false.
            message = "emit_typed_kernel: unsupported IR operation"
            return
        end select

        call body%append("    ")
        call body%append(prefix)
        call body%append(k)
        call body%append(" = ")
        call body%append(expr_text)
        call body%newline()
        ref(k) = prefix//str(k)
    end subroutine render_node

    function infix_text(ir, k, ref, opchar) result(text)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: k
        type(str_t), intent(in) :: ref(:)
        character(*), intent(in) :: opchar
        type(str_t) :: text
        integer :: j, child

        text = str("(")
        do j = 1, ir%nodes(k)%n_operands
            child = ir%operands(ir%nodes(k)%first_operand + j - 1)
            if (j > 1) text = text//str(" "//opchar//" ")
            text = text//ref(child)
        end do
        text = text//str(")")
    end function infix_text

    function call_text(ir, k, ref, fname) result(text)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: k
        type(str_t), intent(in) :: ref(:)
        type(str_t), intent(in) :: fname
        type(str_t) :: text
        integer :: j, child

        text = fname//str("(")
        do j = 1, ir%nodes(k)%n_operands
            child = ir%operands(ir%nodes(k)%first_operand + j - 1)
            if (j > 1) text = text//str(", ")
            text = text//ref(child)
        end do
        text = text//str(")")
    end function call_text

    !> Only an integer, +-0.5 exponent is supported: every other power is
    !> refused rather than silently emitting a non-integer `**`, which most
    !> operator-overloaded numeric types do not provide.
    subroutine power_text(ir, spec, ref, base_idx, exp_idx, text, ok, message)
        type(kernel_ir_t), intent(in) :: ir
        type(typed_kernel_spec_t), intent(in) :: spec
        type(str_t), intent(in) :: ref(:)
        integer, intent(in) :: base_idx, exp_idx
        type(str_t), intent(out) :: text
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        real(dp) :: expval
        type(str_t) :: fname
        integer :: iexp

        ok = .false.
        message = ""
        if (ir%nodes(exp_idx)%operation /= IR_LITERAL) then
            message = "emit_typed_kernel: only a literal exponent is supported"
            return
        end if
        expval = ir%nodes(exp_idx)%value
        if (abs(expval - nint(expval)) < 1.0e-12_dp) then
            iexp = nint(expval)
            text = ref(base_idx)//str("**")//str(iexp)
        else if (abs(expval - 0.5_dp) < 1.0e-12_dp) then
            call function_spelling(spec, "sqrt", fname)
            text = fname//str("(")//ref(base_idx)//str(")")
        else if (abs(expval + 0.5_dp) < 1.0e-12_dp) then
            call function_spelling(spec, "sqrt", fname)
            text = str("(")//literal_text(1.0_dp, spec)//str("/")//fname// &
                str("(")//ref(base_idx)//str("))")
        else
            message = "emit_typed_kernel: unsupported non-integer, "// &
                "non-half-integer power for typed emission"
            return
        end if
        ok = .true.
    end subroutine power_text

    subroutine find_arg(spec, name, ref, ok)
        type(typed_kernel_spec_t), intent(in) :: spec
        character(*), intent(in) :: name
        type(str_t), intent(out) :: ref
        logical, intent(out) :: ok
        integer :: j

        ok = .false.
        do j = 1, size(spec%args)
            if (chars(spec%args(j)) == name) then
                ref = spec%args(j)
                ok = .true.
                return
            end if
        end do
    end subroutine find_arg

    subroutine function_spelling(spec, canonical_name, name)
        type(typed_kernel_spec_t), intent(in) :: spec
        character(*), intent(in) :: canonical_name
        type(str_t), intent(out) :: name
        integer :: j

        name = str(canonical_name)
        if (.not. allocated(spec%functions%canonical)) return
        do j = 1, size(spec%functions%canonical)
            if (chars(spec%functions%canonical(j)) == canonical_name) then
                name = spec%functions%spelling(j)
                return
            end if
        end do
    end subroutine function_spelling

    function literal_text(value, spec) result(text)
        real(dp), intent(in) :: value
        type(typed_kernel_spec_t), intent(in) :: spec
        type(str_t) :: text
        character(32) :: buf

        write (buf, '(ES24.16E3)') value
        if (spec%literal_constructor%is_empty()) then
            text = str(trim(adjustl(buf))//"_dp")
        else
            text = spec%literal_constructor//str("(")// &
                str(trim(adjustl(buf))//"_dp")//str(")")
        end if
    end function literal_text

end module fortsym_kernel_typed
