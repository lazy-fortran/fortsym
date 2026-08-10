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
    use fortsym_string, only: str_t, strbuf_t, str, chars, compare_str, &
        operator(==)
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_ADD, NK_MUL, NK_POW, &
        NK_FUNC
    use fortsym_expr, only: expr_t
    use fortsym_dialect, only: dialect_t, dialect, DIA_FORTRAN
    use fortsym_print, only: print_expr_sub, fortran_roots_representable
    use fortsym_eval, only: free_symbols_of
    use fortsym_kernel_target, only: TARGET_DEFAULT_VALUE => TARGET_DEFAULT, &
        TARGET_FORTRAN_CPU_VALUE => TARGET_FORTRAN_CPU, &
        TARGET_FORTRAN_OPENMP_TARGET_VALUE => TARGET_FORTRAN_OPENMP_TARGET, &
        TARGET_FORTRAN_OPENACC_VALUE => TARGET_FORTRAN_OPENACC, &
        TARGET_DUAL_VALUE => TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC, &
        TARGET_CUDA_VALUE => TARGET_CUDA, &
        target_directives, target_is_valid, target_name
    implicit none
    private

    public :: kernel_spec_t, cse_result_t, operation_count_t
    public :: operation_cost_record_t
    public :: cse_analyse, count_operations, operation_cost
    public :: emit_cost_record, emit_kernel, emit_statements
    public :: KERNEL_SUBROUTINE, KERNEL_SNIPPET
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
        !> Optional declaration suffixes such as "(1)" or "(:)" for inputs.
        type(str_t), allocatable :: arg_shapes(:)
        !> Declared output arguments. Several expressions may target elements
        !> of one declared array through output_references.
        type(str_t), allocatable :: outputs(:)
        !> Optional declaration suffixes for outputs.
        type(str_t), allocatable :: output_shapes(:)
        !> Optional assignment targets, one per expression, such as
        !> "jacobian(1,1)" and "jacobian(1,2)".
        type(str_t), allocatable :: output_references(:)
        !> Prefix for generated temporaries.
        type(str_t)              :: temp_prefix
        !> Fortran type used for inputs, outputs, and temporaries. Empty
        !> defaults to real(dp); complex(dp) supports holomorphic kernels.
        type(str_t)              :: scalar_type
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
        !> Stable target identity. TARGET_DEFAULT retains the legacy logical
        !> flags for byte-identical committed consumers; an explicit target
        !> overrides both legacy flags.
        integer                  :: target = TARGET_DEFAULT
        !> Mark a generated leaf for compilation on an OpenMP target device.
        !> This only annotates the procedure; scheduling and data movement
        ! remain the consuming application's responsibility.
        logical                  :: openmp_declare_target = .false.
        !> Mark a generated leaf kernel for sequential OpenACC device calls.
        logical                  :: openacc_routine_seq = .false.
        !> Request NVFORTRAN source-directed inlining. The compiler honors this
        !> only with -Minline=pragma; other Fortran compilers treat it as a
        !> comment. Keeping the request in generated source lets hot device
        !> leaves opt in without imposing NVIDIA flags on every consumer.
        logical                  :: nvfortran_inline = .false.
        !> Emit a side-effect-free Fortran subroutine.
        logical                  :: pure_procedure = .false.
        !> Emit an elemental subroutine. Elemental dummy arguments must remain
        !> scalar; use arg_shapes/output_shapes for explicit array kernels.
        logical                  :: elemental_procedure = .false.
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
        !> Arithmetic FLOPs count additions, multiplications, and divisions.
        !> A direct product term in a sum remains two FLOPs when it can be
        !> emitted as one FMA instruction.
        integer :: flops = 0
        !> Structural arithmetic/call instructions, with FMA candidates
        !> collapsed from one multiply plus one add. This is not a machine
        !> disassembly count: powers and named functions remain one opaque
        !> operation each.
        integer :: instructions = 0
        !> Number of direct product terms in sums eligible for FMA shaping.
        integer :: fma_candidates = 0
    end type operation_count_t

    !> Machine-readable cost metadata for one generated kernel. The total is
    !> counted over the union of the hash-consed root DAGs; per_root entries
    !> are counted independently so each output can be attributed as well.
    type :: operation_cost_record_t
        type(operation_count_t) :: total
        type(operation_count_t), allocatable :: per_root(:)
        type(str_t), allocatable :: transcendental_names(:)
        integer, allocatable :: transcendental_counts(:)
    end type operation_cost_record_t

contains

    !> Count distinct arithmetic nodes across all roots. An n-ary canonical
    !> sum or product costs n-1 binary operations. Negative integer powers are
    !> printed as divisions and are therefore classified as divisions rather
    !> than powers.
    function count_operations(roots) result(counts)
        type(expr_t), intent(in) :: roots(:)
        type(operation_count_t) :: counts

        logical, allocatable :: visited(:), fma_seen(:)
        type(arena_t), pointer :: a
        integer :: k

        if (size(roots) == 0) return
        a => roots(1)%a
        allocate (visited(a%size()), source=.false.)
        allocate (fma_seen(a%size()), source=.false.)
        do k = 1, size(roots)
            call count_node(a, roots(k)%id, visited, fma_seen, counts)
        end do
        call finish_operation_count(counts)
    end function count_operations

    subroutine finish_operation_count(counts)
        type(operation_count_t), intent(inout) :: counts

        counts%total = counts%additions + counts%multiplications + &
            counts%divisions + counts%powers + counts%functions
        counts%flops = counts%additions + counts%multiplications + counts%divisions
        counts%instructions = counts%additions + counts%multiplications + &
            counts%divisions + counts%powers + counts%functions - &
            counts%fma_candidates
    end subroutine finish_operation_count

    recursive subroutine count_node(a, id, visited, fma_seen, counts)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical, intent(inout) :: visited(:)
        logical, intent(inout) :: fma_seen(:)
        type(operation_count_t), intent(inout) :: counts
        integer :: k, kind, child

        if (visited(id)) return
        visited(id) = .true.
        do k = 1, a%nargs_of(id)
            call count_node(a, a%arg_of(id, k), visited, fma_seen, counts)
        end do

        kind = a%kind_of(id)
        select case (kind)
        case (NK_RAT)
            if (a%den_of(id) /= 1) counts%divisions = counts%divisions + 1
        case (NK_ADD)
            counts%additions = counts%additions + max(0, a%nargs_of(id) - 1)
            do k = 1, a%nargs_of(id)
                child = a%arg_of(id, k)
                if (a%kind_of(child) == NK_MUL .and. .not. fma_seen(child)) then
                    fma_seen(child) = .true.
                    counts%fma_candidates = counts%fma_candidates + 1
                end if
            end do
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

    !> Build the structured cost record that accompanies a kernel. Function
    !> heads are aggregated over the distinct DAG and sorted so construction
    !> history cannot change the machine-readable report.
    function operation_cost(roots) result(record)
        type(expr_t), intent(in) :: roots(:)
        type(operation_cost_record_t) :: record

        type(arena_t), pointer :: a
        type(str_t), allocatable :: names(:)
        integer, allocatable :: name_counts(:)
        logical, allocatable :: visited(:)
        integer :: k, n_names

        record%total = count_operations(roots)
        allocate (record%per_root(size(roots)))
        do k = 1, size(roots)
            record%per_root(k) = count_operations(roots(k:k))
        end do

        if (size(roots) == 0) then
            allocate (record%transcendental_names(0))
            allocate (record%transcendental_counts(0))
            return
        end if

        a => roots(1)%a
        allocate (names(a%size()))
        allocate (name_counts(a%size()), source=0)
        allocate (visited(a%size()), source=.false.)
        n_names = 0
        do k = 1, size(roots)
            call collect_function_nodes(a, roots(k)%id, visited, names, &
                name_counts, n_names)
        end do
        call sort_function_names(names, name_counts, n_names)
        allocate (record%transcendental_names(n_names))
        allocate (record%transcendental_counts(n_names))
        if (n_names > 0) then
            record%transcendental_names = names(1:n_names)
            record%transcendental_counts = name_counts(1:n_names)
        end if
    end function operation_cost

    recursive subroutine collect_function_nodes(a, id, visited, names, counts, n)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical, intent(inout) :: visited(:)
        type(str_t), intent(inout) :: names(:)
        integer, intent(inout) :: counts(:)
        integer, intent(inout) :: n
        integer :: k, j
        type(str_t) :: name

        if (visited(id)) return
        visited(id) = .true.
        do k = 1, a%nargs_of(id)
            call collect_function_nodes(a, a%arg_of(id, k), visited, names, &
                counts, n)
        end do
        if (a%kind_of(id) /= NK_FUNC) return

        name = a%name_of(id)
        do j = 1, n
            if (names(j) == name) then
                counts(j) = counts(j) + 1
                return
            end if
        end do
        n = n + 1
        names(n) = name
        counts(n) = 1
    end subroutine collect_function_nodes

    subroutine sort_function_names(names, counts, n)
        type(str_t), intent(inout) :: names(:)
        integer, intent(inout) :: counts(:)
        integer, intent(in) :: n
        type(str_t) :: name
        integer :: count, i, j

        do i = 2, n
            name = names(i)
            count = counts(i)
            j = i - 1
            do while (j >= 1)
                if (compare_str(names(j), name) <= 0) exit
                names(j + 1) = names(j)
                counts(j + 1) = counts(j)
                j = j - 1
            end do
            names(j + 1) = name
            counts(j + 1) = count
        end do
    end subroutine sort_function_names

    !> Serialize operation_cost as a compact JSON object. The schema is stable
    !> enough for manifests while the explicit semantics field prevents readers
    !> from mistaking structural counts for a machine disassembly.
    function emit_cost_record(roots) result(s)
        type(expr_t), intent(in) :: roots(:)
        type(str_t) :: s

        type(operation_cost_record_t) :: record
        type(strbuf_t) :: b
        integer :: k

        record = operation_cost(roots)
        call b%append('{"schema":"fortsym.operation_cost.v1",')
        call b%append('"semantics":"distinct hash-consed DAG nodes; per-root entries are '// &
            'counted independently; fma_candidates are direct product terms in sums",')
        call b%append('"totals":')
        call append_count_json(b, record%total)
        call b%append(',"roots":[')
        do k = 1, size(record%per_root)
            if (k > 1) call b%append(',')
            call b%append('{"index":')
            call b%append(k)
            call b%append(',"operations":')
            call append_count_json(b, record%per_root(k))
            call b%append('}')
        end do
        call b%append('],"transcendentals":{')
        do k = 1, size(record%transcendental_names)
            if (k > 1) call b%append(',')
            call append_json_string(b, chars(record%transcendental_names(k)))
            call b%append(':')
            call b%append(record%transcendental_counts(k))
        end do
        call b%append('}}')
        s = b%to_str()
    end function emit_cost_record

    subroutine append_count_json(b, counts)
        type(strbuf_t), intent(inout) :: b
        type(operation_count_t), intent(in) :: counts

        call b%append('{"additions":')
        call b%append(counts%additions)
        call b%append(',"multiplications":')
        call b%append(counts%multiplications)
        call b%append(',"divisions":')
        call b%append(counts%divisions)
        call b%append(',"powers":')
        call b%append(counts%powers)
        call b%append(',"functions":')
        call b%append(counts%functions)
        call b%append(',"total":')
        call b%append(counts%total)
        call b%append(',"flops":')
        call b%append(counts%flops)
        call b%append(',"instructions":')
        call b%append(counts%instructions)
        call b%append(',"fma_candidates":')
        call b%append(counts%fma_candidates)
        call b%append('}')
    end subroutine append_count_json

    subroutine append_json_string(b, text)
        type(strbuf_t), intent(inout) :: b
        character(*), intent(in) :: text
        integer :: k, code

        call b%append('"')
        do k = 1, len(text)
            code = iachar(text(k:k))
            select case (code)
            case (8)
                call b%append('\\b')
            case (9)
                call b%append('\\t')
            case (10)
                call b%append('\\n')
            case (12)
                call b%append('\\f')
            case (13)
                call b%append('\\r')
            case (34)
                call b%append('\\"')
            case (92)
                call b%append('\\\\')
            case default
                call b%append(text(k:k))
            end select
        end do
        call b%append('"')
    end subroutine append_json_string

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
    function emit_statements(roots, spec, res, ok, prechecked) result(s)
        type(expr_t),       intent(in) :: roots(:)
        type(kernel_spec_t), intent(in) :: spec
        type(cse_result_t), intent(in) :: res
        logical, intent(out), optional :: ok
        logical, intent(in), optional :: prechecked
        type(str_t)                    :: s

        type(strbuf_t)  :: b
        type(dialect_t) :: d
        type(expr_t)    :: tmp
        integer :: k
        logical :: valid

        d = dialect(DIA_FORTRAN)
        if (present(prechecked)) then
            valid = prechecked
        else
            valid = fortran_roots_representable(roots)
        end if
        if (present(ok)) ok = valid
        if (.not. valid) then
            s = str("")
            return
        end if

        do k = 1, res%n
            tmp%a => roots(1)%a
            tmp%id = res%ids(k)
            ! Each temporary may refer to earlier temporaries but not to itself,
            ! so the substitution table is truncated to what precedes it.
            call append_assignment(b, chars(res%names(k)), &
                chars(print_expr_sub(tmp, d, &
                res%ids(1:k - 1), &
                res%names(1:k - 1), prechecked=.true.)))
        end do

        do k = 1, size(roots)
            if (allocated(spec%output_references)) then
                call append_assignment(b, chars(spec%output_references(k)), &
                    chars(print_expr_sub(roots(k), d, &
                    res%ids(1:res%n), res%names(1:res%n), &
                    prechecked=.true.)))
            else
                call append_assignment(b, chars(spec%outputs(k)), &
                    chars(print_expr_sub(roots(k), d, &
                    res%ids(1:res%n), res%names(1:res%n), &
                    prechecked=.true.)))
            end if
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
            if (start <= n) then
                if (text(start:start) == " ") start = start + 1
            end if
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
                ! The exponentiation operator is one two-character token.
                ! Never place a continuation between its two asterisks.
                if (c == "*" .and. ((k > from .and. text(k - 1:k - 1) == "*") .or. &
                    (k < len(text) .and. text(k + 1:k + 1) == "*"))) cycle
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
    function emit_kernel(roots, spec, ok, cost_record) result(s)
        type(expr_t),        intent(in) :: roots(:)
        type(kernel_spec_t), intent(in) :: spec
        logical, intent(out), optional :: ok
        type(str_t), intent(out), optional :: cost_record
        type(str_t)                     :: s

        type(strbuf_t)     :: b, body
        type(cse_result_t) :: res
        logical :: valid, emit_openmp, emit_openacc

        valid = fortran_roots_representable(roots)
        if (valid) valid = kernel_symbols_are_declared(roots, spec%args)
        if (present(ok)) ok = valid
        if (present(cost_record)) cost_record = str("")
        if (.not. valid) then
            s = str("")
            return
        end if
        if (.not. target_is_valid(spec%target)) then
            error stop "invalid kernel target"
        end if
        call target_directives(spec%target, spec%openmp_declare_target, &
            spec%openacc_routine_seq, emit_openmp, emit_openacc)
        if (present(cost_record)) cost_record = emit_cost_record(roots)

        if (emit_openmp) then
            if (len(chars(spec%module_name)) == 0) then
                error stop "OpenMP device emission requires module_name"
            end if
        end if
        if (allocated(spec%arg_shapes)) then
            if (size(spec%arg_shapes) /= size(spec%args)) then
                error stop "arg_shapes must match args"
            end if
        end if
        if (allocated(spec%output_shapes)) then
            if (size(spec%output_shapes) /= size(spec%outputs)) then
                error stop "output_shapes must match outputs"
            end if
        end if
        if (allocated(spec%output_references)) then
            if (size(spec%output_references) /= size(roots)) then
                error stop "output_references must match expressions"
            end if
        else if (size(spec%outputs) /= size(roots)) then
            error stop "outputs must match expressions without output_references"
        end if
        if (spec%elemental_procedure) then
            if (allocated(spec%arg_shapes) .or. &
                allocated(spec%output_shapes)) then
                error stop "elemental procedures require scalar arguments"
            end if
        end if

        if (len(chars(spec%temp_prefix)) > 0) then
            res = cse_analyse(roots, chars(spec%temp_prefix))
        else
            res = cse_analyse(roots, "t")
        end if

        call append_banner(b, spec)

        if (spec%mode == KERNEL_SNIPPET) then
            ! Bare statements over host-scope names. No header, no declarations:
            ! the including scope owns them.
            call b%append(chars(emit_statements(roots, spec, res, &
                prechecked=.true.)))
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
            call append_subroutine(body, roots, spec, res, emit_openmp, emit_openacc)
            call append_indented(b, chars(body%to_str()))
            call b%newline()
            call b%append("end module ")
            call b%append(chars(spec%module_name))
            call b%newline()
        else
            call append_subroutine(b, roots, spec, res, emit_openmp, emit_openacc)
        end if

        s = b%to_str()
    end function emit_kernel

    !> Refuse a kernel whose expression DAG contains a symbol absent from the
    !> declared input interface. Otherwise implicit-none compilation catches
    !> the generator defect only after invalid source has already been written.
    function kernel_symbols_are_declared(roots, args) result(valid)
        type(expr_t), intent(in) :: roots(:)
        type(str_t), allocatable, intent(in) :: args(:)
        type(str_t), allocatable :: symbols(:)
        logical :: valid, found
        integer :: i, j, k

        valid = .true.
        do k = 1, size(roots)
            symbols = free_symbols_of(roots(k))
            do i = 1, size(symbols)
                found = .false.
                if (allocated(args)) then
                    do j = 1, size(args)
                        if (symbol_matches_argument(chars(symbols(i)), &
                            chars(args(j)))) found = .true.
                    end do
                end if
                if (.not. found) then
                    valid = .false.
                    return
                end if
            end do
        end do
    end function kernel_symbols_are_declared

    function symbol_matches_argument(symbol_name, argument_name) result(matches)
        character(*), intent(in) :: symbol_name, argument_name
        logical :: matches
        integer :: argument_length, symbol_length

        argument_length = len_trim(argument_name)
        symbol_length = len_trim(symbol_name)
        matches = same_fortran_name(symbol_name(:symbol_length), &
            argument_name(:argument_length))
        if (matches) return
        matches = symbol_length > argument_length + 2
        if (.not. matches) return
        matches = same_fortran_name(symbol_name(:argument_length), &
            argument_name(:argument_length))
        if (.not. matches) return
        matches = symbol_name(argument_length + 1:argument_length + 1) == "(" .and. &
            symbol_name(symbol_length:symbol_length) == ")"
    end function symbol_matches_argument

    pure function same_fortran_name(left, right) result(matches)
        character(*), intent(in) :: left, right
        logical :: matches
        integer :: i

        matches = len(left) == len(right)
        if (.not. matches) return
        do i = 1, len(left)
            if (lower_ascii(left(i:i)) /= lower_ascii(right(i:i))) then
                matches = .false.
                return
            end if
        end do
    end function same_fortran_name

    pure function lower_ascii(character_value) result(lower)
        character, intent(in) :: character_value
        character :: lower

        if (character_value >= "A" .and. character_value <= "Z") then
            lower = achar(iachar(character_value) + iachar("a") - iachar("A"))
        else
            lower = character_value
        end if
    end function lower_ascii

    subroutine append_subroutine(b, roots, spec, res, emit_openmp, emit_openacc)
        type(strbuf_t), intent(inout) :: b
        type(expr_t), intent(in) :: roots(:)
        type(kernel_spec_t), intent(in) :: spec
        type(cse_result_t), intent(in) :: res
        logical, intent(in) :: emit_openmp, emit_openacc
        type(strbuf_t) :: header
        character(:), allocatable :: scalar_type
        integer :: k

        scalar_type = chars(spec%scalar_type)
        if (len(scalar_type) == 0) scalar_type = "real(dp)"

        if (spec%nvfortran_inline) then
            call b%append("!NVF$ INLINE")
            call b%newline()
        end if
        if (spec%pure_procedure) call header%append("pure ")
        if (spec%elemental_procedure) call header%append("elemental ")
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

        if (emit_openmp) then
            call b%append("    !$omp declare target")
            call b%newline()
        end if
        if (emit_openacc) then
            call b%append("    !$acc routine seq")
            call b%newline()
        end if
        call b%append("    use, intrinsic :: iso_fortran_env, only: dp => real64")
        call b%newline()
        call b%append("    implicit none")
        call b%newline()

        if (allocated(spec%arg_shapes)) then
            call declare(b, scalar_type, "intent(in)", spec%args, spec%arg_shapes)
        else
            call declare(b, scalar_type, "intent(in)", spec%args)
        end if
        if (allocated(spec%output_shapes)) then
            call declare( &
                b, scalar_type, "intent(out)", spec%outputs, spec%output_shapes)
        else
            call declare(b, scalar_type, "intent(out)", spec%outputs)
        end if

        if (res%n > 0) then
            ! Temporaries are declared explicitly, never left to an implicit
            ! typing rule.
            call declare(b, scalar_type, "", res%names(1:res%n))
        end if

        call b%newline()
        call b%append(chars(emit_statements(roots, spec, res, &
            prechecked=.true.)))
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

    subroutine declare(b, scalar_type, attribute, names, shapes)
        type(strbuf_t), intent(inout) :: b
        character(*),   intent(in)    :: scalar_type
        character(*),   intent(in)    :: attribute
        type(str_t),    intent(in)    :: names(:)
        type(str_t),    intent(in), optional :: shapes(:)
        type(strbuf_t) :: declaration
        integer :: k

        if (size(names) == 0) return

        call declaration%append("    ")
        call declaration%append(scalar_type)
        if (len(attribute) > 0) then
            call declaration%append(", ")
            call declaration%append(attribute)
        end if
        call declaration%append(" :: ")
        do k = 1, size(names)
            if (k > 1) call declaration%append(", ")
            call declaration%append(chars(names(k)))
            if (present(shapes)) call declaration%append(chars(shapes(k)))
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
