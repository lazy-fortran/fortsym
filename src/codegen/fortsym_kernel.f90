module fortsym_kernel
    ! Turning expressions into Fortran.
    !
    ! Common subexpression elimination first, then emission. CSE is unusually
    ! cheap here because the arena is hash-consed: sharing is already recorded,
    ! so finding it is a reference count rather than a search. A subexpression
    ! that appears three times *is* one node with three parents, and all that
    ! remains is to decide which shared nodes deserve a temporary.
    !
    ! The emitted form is deliberately explicit. KiLCA's generated kernels in
    ! this ecosystem declare their temporaries with `implicit real(dp) (s-t)`,
    ! which silently types anything beginning with s or t and turns a misspelled
    ! variable into a fresh zero. Every temporary here is declared.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_ADD, NK_MUL, NK_POW, NK_FUNC
    use fortsym_expr, only: expr_t
    use fortsym_dialect, only: dialect_t, dialect, DIA_FORTRAN
    use fortsym_print, only: print_expr_sub
    implicit none
    private

    public :: kernel_spec_t, cse_result_t, operation_count_t
    public :: cse_analyse, count_operations, emit_kernel, emit_statements
    public :: KERNEL_SUBROUTINE, KERNEL_SNIPPET

    integer, parameter :: dp = real64

    !> Emit a complete subroutine with declarations and intents. The form
    !> fortnum and other consumers link against.
    integer, parameter :: KERNEL_SUBROUTINE = 1
    !> Emit bare statements over names the host scope already has. This is the
    !> `include` pattern genex relies on, where generated text is spliced into
    !> the middle of an existing loop nest.
    integer, parameter :: KERNEL_SNIPPET = 2

    !> Longest line the emitter will produce before continuing. The Fortran
    !> standard allows 132 columns; leaving room for the continuation ampersand
    !> and indentation keeps output inside it with margin.
    integer, parameter :: LINE_LIMIT = 100

    type :: kernel_spec_t
        type(str_t)              :: name
        integer                  :: mode = KERNEL_SUBROUTINE
        !> Input symbol names, in argument order.
        type(str_t), allocatable :: args(:)
        !> Output variable names, one per expression.
        type(str_t), allocatable :: outputs(:)
        !> Prefix for generated temporaries.
        type(str_t)              :: temp_prefix
        !> Name of the module or program that generated this, recorded in the
        !> banner so the file can always be traced back to its generator.
        type(str_t)              :: generator
        !> Exact source revision of the generator dependency. Consumers use
        !> this to reproduce committed output against the same fortsym state.
        type(str_t)              :: generator_revision
        !> Exact command that regenerates the output. Empty retains the
        !> conventional `fo exec <generator>` command.
        type(str_t)              :: regenerate_command
        !> Optional Fortran module wrapper. A module gives consumers an
        !> explicit interface without maintaining a second handwritten one.
        type(str_t)              :: module_name
        !> Mark a generated leaf kernel for sequential OpenACC device calls.
        logical                  :: openacc_routine_seq = .false.
        !> Emit a side-effect-free Fortran subroutine.
        logical                  :: pure_procedure = .false.
    end type kernel_spec_t

    !> Which nodes became temporaries, and in what order they must be assigned.
    type :: cse_result_t
        integer,     allocatable :: ids(:)
        type(str_t), allocatable :: names(:)
        integer                  :: n = 0
    end type cse_result_t

    !> Arithmetic work represented by an emitted expression DAG. Counts are
    !> structural and deterministic: a hash-consed node is counted once even
    !> when several outputs share it, matching the work after CSE. They are
    !> metadata for comparing generated variants, never a substitute for
    !> measured runtime.
    type :: operation_count_t
        integer :: additions = 0
        integer :: multiplications = 0
        integer :: divisions = 0
        integer :: powers = 0
        integer :: functions = 0
        integer :: total = 0
    end type operation_count_t

contains

    !> Count distinct arithmetic nodes across all roots. An n-ary canonical
    !> sum or product costs n-1 binary operations. Negative integer powers are
    !> printed as divisions and are therefore classified as divisions rather
    !> than powers.
    function count_operations(roots) result(counts)
        type(expr_t), intent(in) :: roots(:)
        type(operation_count_t) :: counts

        logical, allocatable :: visited(:)
        type(arena_t), pointer :: a
        integer :: k

        if (size(roots) == 0) return
        a => roots(1)%a
        allocate (visited(a%size()), source=.false.)
        do k = 1, size(roots)
            call count_node(a, roots(k)%id, visited, counts)
        end do
        counts%total = counts%additions + counts%multiplications + &
            counts%divisions + counts%powers + counts%functions
    end function count_operations

    recursive subroutine count_node(a, id, visited, counts)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical, intent(inout) :: visited(:)
        type(operation_count_t), intent(inout) :: counts
        integer :: k, kind

        if (visited(id)) return
        visited(id) = .true.
        do k = 1, a%nargs_of(id)
            call count_node(a, a%arg_of(id, k), visited, counts)
        end do

        kind = a%kind_of(id)
        select case (kind)
        case (NK_RAT)
            if (a%den_of(id) /= 1) counts%divisions = counts%divisions + 1
        case (NK_ADD)
            counts%additions = counts%additions + max(0, a%nargs_of(id) - 1)
        case (NK_MUL)
            counts%multiplications = counts%multiplications + &
                max(0, a%nargs_of(id) - 1)
        case (NK_POW)
            if (is_reciprocal_power(a, id)) then
                counts%divisions = counts%divisions + 1
            else
                counts%powers = counts%powers + 1
            end if
        case (NK_FUNC)
            counts%functions = counts%functions + 1
        end select
    end subroutine count_node

    !> Decide which shared subexpressions deserve a temporary, and order the
    !> assignments so every temporary is defined before it is used.
    !>
    !> A node qualifies when it is referenced more than once *and* is worth
    !> naming. Atoms are excluded: a symbol or a small integer costs nothing to
    !> repeat and naming it would make the output longer and slower to read.
    function cse_analyse(roots, prefix) result(res)
        type(expr_t), intent(in) :: roots(:)
        character(*), intent(in) :: prefix
        type(cse_result_t)       :: res

        integer, allocatable :: refs(:)
        logical, allocatable :: emitted(:)
        type(arena_t), pointer :: a
        integer :: k, capacity

        res%n = 0
        if (size(roots) == 0) then
            allocate (res%ids(0), res%names(0))
            return
        end if

        a => roots(1)%a
        capacity = a%size()
        allocate (refs(capacity), source=0)
        allocate (emitted(capacity), source=.false.)
        allocate (res%ids(capacity), res%names(capacity))

        ! Count parents. Each root counts once even when two roots share it.
        do k = 1, size(roots)
            call count_refs(a, roots(k)%id, refs)
        end do

        ! Post-order, so a temporary's own dependencies are assigned first.
        do k = 1, size(roots)
            call collect(a, roots(k)%id, refs, emitted, res, prefix)
        end do
    end function cse_analyse

    recursive subroutine count_refs(a, id, refs)
        type(arena_t), intent(in)    :: a
        integer,       intent(in)    :: id
        integer,       intent(inout) :: refs(:)
        integer :: k

        refs(id) = refs(id) + 1
        ! Recurse only the first time. Hash-consing means the subtree below a
        ! shared node is identical on every visit, so descending again would
        ! inflate the counts of everything beneath it.
        if (refs(id) > 1) return
        do k = 1, a%nargs_of(id)
            call count_refs(a, a%arg_of(id, k), refs)
        end do
    end subroutine count_refs

    recursive subroutine collect(a, id, refs, emitted, res, prefix)
        type(arena_t),      intent(in)    :: a
        integer,            intent(in)    :: id
        integer,            intent(in)    :: refs(:)
        logical,            intent(inout) :: emitted(:)
        type(cse_result_t), intent(inout) :: res
        character(*),       intent(in)    :: prefix
        integer :: k

        if (emitted(id)) return
        emitted(id) = .true.

        do k = 1, a%nargs_of(id)
            call collect(a, a%arg_of(id, k), refs, emitted, res, prefix)
        end do

        if (refs(id) > 1 .and. worth_naming(a, id)) then
            res%n = res%n + 1
            res%ids(res%n) = id
            res%names(res%n) = str(prefix//chars(str(res%n)))
        end if
    end subroutine collect

    !> Compound nodes only. Naming a symbol or a literal would cost a line and
    !> save nothing.
    !>
    !> Reciprocal powers are excluded too. The printer renders x**(-1) as a
    !> division rather than as a power, so a temporary holding one is never
    !> referenced and becomes a dead store -- computed, declared, and unused.
    pure function worth_naming(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        integer :: k
        k = a%kind_of(id)
        yes = k == NK_ADD .or. k == NK_MUL .or. k == NK_POW .or. k == NK_FUNC
        if (k == NK_POW) yes = .not. is_reciprocal_power(a, id)
    end function worth_naming

    !> A power whose exponent is a negative integer, which the printer emits as
    !> a division.
    pure function is_reciprocal_power(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        integer :: e_id
        yes = .false.
        e_id = a%arg_of(id, 2)
        if (a%kind_of(e_id) /= NK_INT) return
        yes = a%num_of(e_id) < 0_int64
    end function is_reciprocal_power

    !> The assignment statements: every temporary, then every output.
    function emit_statements(roots, spec, res) result(s)
        type(expr_t),       intent(in) :: roots(:)
        type(kernel_spec_t), intent(in) :: spec
        type(cse_result_t), intent(in) :: res
        type(str_t)                    :: s

        type(strbuf_t)  :: b
        type(dialect_t) :: d
        type(expr_t)    :: tmp
        integer :: k

        d = dialect(DIA_FORTRAN)

        do k = 1, res%n
            tmp%a => roots(1)%a
            tmp%id = res%ids(k)
            ! Each temporary may refer to earlier temporaries but not to itself,
            ! so the substitution table is truncated to what precedes it.
            call append_assignment(b, chars(res%names(k)), &
                chars(print_expr_sub(tmp, d, &
                res%ids(1:k - 1), &
                res%names(1:k - 1))))
        end do

        do k = 1, size(roots)
            call append_assignment(b, chars(spec%outputs(k)), &
                chars(print_expr_sub(roots(k), d, &
                res%ids(1:res%n), &
                res%names(1:res%n))))
        end do

        s = b%to_str()
    end function emit_statements

    !> One assignment, wrapped to the line limit.
    subroutine append_assignment(b, lhs, rhs)
        type(strbuf_t), intent(inout) :: b
        character(*),   intent(in)    :: lhs, rhs
        call append_wrapped(b, "    "//lhs//" = "//rhs)
    end subroutine append_assignment

    !> Emit a statement, breaking it across continuation lines when it exceeds
    !> the limit.
    !>
    !> Breaks are placed at operators and argument separators, never inside a
    !> name or a numeric literal -- splitting `1.0e-8_dp` would change its
    !> value, and splitting an identifier would not compile.
    subroutine append_wrapped(b, text)
        type(strbuf_t), intent(inout) :: b
        character(*),   intent(in)    :: text
        integer :: start, cut, n

        n = len(text)
        start = 1

        do while (n - start + 1 > LINE_LIMIT)
            cut = last_break(text, start, start + LINE_LIMIT - 1)
            if (cut <= start) exit ! nowhere safe to break; emit long
            call b%append(text(start:cut))
            call b%append(" &")
            call b%newline()
            call b%append("        ")
            start = cut + 1
        end do

        call b%append(text(start:n))
        call b%newline()
    end subroutine append_wrapped

    !> Rightmost position in [from, to] after which a break is safe.
    pure function last_break(text, from, to) result(cut)
        character(*), intent(in) :: text
        integer,      intent(in) :: from, to
        integer                  :: cut
        integer :: k
        character :: c

        cut = 0
        do k = to, from + 1, -1
            c = text(k:k)
            if (c == "+" .or. c == "-" .or. c == "*" .or. c == "/" .or. &
                c == "," .or. c == ")") then
                ! A sign belonging to an exponent is part of the literal.
                if ((c == "+" .or. c == "-") .and. k > from) then
                    if (is_exponent_sign(text, k)) cycle
                end if
                cut = k
                return
            end if
        end do
    end function last_break

    !> True when this + or - is the sign of a floating-point exponent, as in
    !> 1.0e-8. Breaking there would silently change the constant.
    pure function is_exponent_sign(text, k) result(yes)
        character(*), intent(in) :: text
        integer,      intent(in) :: k
        logical                  :: yes
        character :: prev
        yes = .false.
        if (k <= 1) return
        prev = text(k - 1:k - 1)
        yes = prev == "e" .or. prev == "E" .or. prev == "d" .or. prev == "D"
    end function is_exponent_sign

    !> A complete kernel: banner, subroutine header, declarations, body.
    function emit_kernel(roots, spec) result(s)
        type(expr_t),        intent(in) :: roots(:)
        type(kernel_spec_t), intent(in) :: spec
        type(str_t)                     :: s

        type(strbuf_t)     :: b, body
        type(cse_result_t) :: res

        res = cse_analyse(roots, chars(spec%temp_prefix))

        call append_banner(b, spec)

        if (spec%mode == KERNEL_SNIPPET) then
            ! Bare statements over host-scope names. No header, no declarations:
            ! the including scope owns them.
            call b%append(chars(emit_statements(roots, spec, res)))
            s = b%to_str()
            return
        end if

        if (len(chars(spec%module_name)) > 0) then
            call b%append("module ")
            call b%append(chars(spec%module_name))
            call b%newline()
            call b%append("    implicit none")
            call b%newline()
            call b%append("    private")
            call b%newline()
            call b%append("    public :: ")
            call b%append(chars(spec%name))
            call b%newline()
            call b%append("contains")
            call b%newline()
            call b%newline()
            call append_subroutine(body, roots, spec, res)
            call append_indented(b, chars(body%to_str()))
            call b%newline()
            call b%append("end module ")
            call b%append(chars(spec%module_name))
            call b%newline()
        else
            call append_subroutine(b, roots, spec, res)
        end if

        s = b%to_str()
    end function emit_kernel

    subroutine append_subroutine(b, roots, spec, res)
        type(strbuf_t), intent(inout) :: b
        type(expr_t), intent(in) :: roots(:)
        type(kernel_spec_t), intent(in) :: spec
        type(cse_result_t), intent(in) :: res
        type(strbuf_t) :: header
        integer :: k

        if (spec%pure_procedure) call header%append("pure ")
        call header%append("subroutine ")
        call header%append(chars(spec%name))
        call header%append("(")
        do k = 1, size(spec%args)
            if (k > 1) call header%append(", ")
            call header%append(chars(spec%args(k)))
        end do
        do k = 1, size(spec%outputs)
            call header%append(", ")
            call header%append(chars(spec%outputs(k)))
        end do
        call header%append(")")
        call append_wrapped(b, chars(header%to_str()))

        if (spec%openacc_routine_seq) then
            call b%append("    !$acc routine seq")
            call b%newline()
        end if
        call b%append("    use, intrinsic :: iso_fortran_env, only: dp => real64")
        call b%newline()
        call b%append("    implicit none")
        call b%newline()

        call declare(b, "intent(in)", spec%args)
        call declare(b, "intent(out)", spec%outputs)

        if (res%n > 0) then
            ! Temporaries are declared explicitly, never left to an implicit
            ! typing rule.
            call declare(b, "", res%names(1:res%n))
        end if

        call b%newline()
        call b%append(chars(emit_statements(roots, spec, res)))
        call b%newline()
        call b%append("end subroutine ")
        call b%append(chars(spec%name))
        call b%newline()
    end subroutine append_subroutine

    subroutine append_indented(b, text)
        type(strbuf_t), intent(inout) :: b
        character(*), intent(in) :: text
        integer :: first, k

        first = 1
        do k = 1, len(text)
            if (text(k:k) /= new_line("a")) cycle
            if (k > first) then
                call b%append("    ")
                call b%append(text(first:k - 1))
            end if
            call b%newline()
            first = k + 1
        end do
        if (first <= len(text)) then
            call b%append("    ")
            call b%append(text(first:))
        end if
    end subroutine append_indented

    subroutine declare(b, attribute, names)
        type(strbuf_t), intent(inout) :: b
        character(*),   intent(in)    :: attribute
        type(str_t),    intent(in)    :: names(:)
        type(strbuf_t) :: declaration
        integer :: k

        if (size(names) == 0) return

        call declaration%append("    real(dp)")
        if (len(attribute) > 0) then
            call declaration%append(", ")
            call declaration%append(attribute)
        end if
        call declaration%append(" :: ")
        do k = 1, size(names)
            if (k > 1) call declaration%append(", ")
            call declaration%append(chars(names(k)))
        end do
        call append_wrapped(b, chars(declaration%to_str()))
    end subroutine declare

    !> Provenance banner.
    !>
    !> The one thing that must never be lost is how to regenerate this file.
    !> KiLCA's generated kernels in this ecosystem carry an "automatically
    !> generated" note and no generator, so they can no longer be reproduced or
    !> trusted; naming the generator here, plus the CI job that reruns it, is
    !> what keeps that from happening again.
    subroutine append_banner(b, spec)
        type(strbuf_t),      intent(inout) :: b
        type(kernel_spec_t), intent(in)    :: spec

        call b%append("! Generated by fortsym. Do not edit.")
        call b%newline()
        call b%append("! Generator: ")
        call b%append(chars(spec%generator))
        call b%newline()
        if (len(chars(spec%generator_revision)) > 0) then
            call b%append("! Generator revision: ")
            call b%append(chars(spec%generator_revision))
            call b%newline()
        end if
        call b%append("! Regenerate with: ")
        if (len(chars(spec%regenerate_command)) > 0) then
            call b%append(chars(spec%regenerate_command))
        else
            call b%append("fo exec ")
            call b%append(chars(spec%generator))
        end if
        call b%newline()
        call b%newline()
    end subroutine append_banner

end module fortsym_kernel
