module fortsym_arena
    ! The expression store: a hash-consed directed acyclic graph.
    !
    ! fortsym owns its own representation rather than borrowing an engine's.
    ! That is what lets the frontend stay engine-agnostic: SymEngine, Yacas,
    ! SymPy and Maxima are all backends that convert to and from this, and none
    ! of their data models reaches the public API.
    !
    ! Nodes live in one flat array and are referred to by integer index, never
    ! by pointer. Three consequences make the rest of fortsym simpler:
    !
    !   - Copying an expression copies an integer.
    !   - Every node is interned, so structurally identical subtrees are stored
    !     once and share an index. Equality is therefore an integer comparison
    !     rather than a tree walk, and common subexpression elimination becomes
    !     a counting pass over existing sharing instead of a search for it.
    !   - There are no cycles to worry about and nothing to finalize per node.
    !
    ! Canonicalization is deliberately shallow. Sums and products are flattened
    ! and their operands put in a structural semantic order, so x+y and y+x
    ! intern to the same node and construction history cannot change printed or
    ! generated output. No attempt is made to collect like terms or simplify;
    ! universal power identities are the one construction-time exception.
    ! Algebra is the engines' job; this layer only has to be a faithful, shared,
    ! comparable structure.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str_t, str, chars, compare_str, operator(//), &
        operator(==)
    use fortsym_exact, only: exact_normalize
    use fortsym_algebraic, only: algebraic_normalize
    implicit none
    private

    public :: arena_t
    public :: NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_CONST, NK_ADD, NK_MUL, &
        NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL, NK_ALGEBRAIC
    public :: node_kind_name

    integer, parameter :: dp = real64

    !> Node kinds. NK_CONST covers named mathematical constants and domain
    !> sentinels (pi, e, i, oo, zoo, nan) that must not decay into a floating
    !> literal.
    integer, parameter :: NK_INT = 1
    integer, parameter :: NK_RAT = 2
    integer, parameter :: NK_REAL = 3
    integer, parameter :: NK_SYM = 4
    integer, parameter :: NK_CONST = 5
    integer, parameter :: NK_ADD = 6
    integer, parameter :: NK_MUL = 7
    integer, parameter :: NK_POW = 8
    integer, parameter :: NK_FUNC = 9
    integer, parameter :: NK_BIG_INT = 10
    integer, parameter :: NK_BIG_RAT = 11
    !> A bounded decimal result from requested-precision numeric evaluation.
    !> Unlike NK_REAL, its text is retained so printing cannot silently throw
    !> away the digits the caller requested.
    integer, parameter :: NK_BIG_REAL = 12
    !> A canonical exact real or complex algebraic value in FLINT qqbar1 form.
    integer, parameter :: NK_ALGEBRAIC = 13

    integer, parameter :: MAX_BIG_REAL_TEXT = 4096

    integer, parameter :: INITIAL_NODES = 256
    integer, parameter :: INITIAL_ARGS = 512
    integer, parameter :: INITIAL_NAMES = 32
    integer, parameter :: INITIAL_NAME_BUCKETS = 64
    !> Bucket count is a power of two so the modulo is a mask.
    integer, parameter :: INITIAL_BUCKETS = 1024

    !> One node. Fixed-size components only: no allocatable parts, so the node
    !> array grows with a plain copy and needs no per-element finalization.
    type :: node_t
        integer        :: kind = 0
        integer(int64) :: num = 0_int64 !< NK_INT value, NK_RAT numerator
        integer(int64) :: den = 1_int64 !< NK_RAT denominator, always > 0
        real(dp)       :: rval = 0.0_dp !< NK_REAL value
        integer        :: name = 0 !< index into the name table
        integer        :: first_arg = 0 !< start of this node's slice of args
        integer        :: n_args = 0
        integer(int64) :: hash = 0_int64
        integer        :: next = 0 !< next node in the same hash bucket
    end type node_t

    type :: arena_t
        type(node_t), allocatable :: nodes(:)
        integer                   :: n_nodes = 0
        !> Incremented whenever the node store is cleared. Expression handles
        !> retain this value so a reset cannot make an old node index look live.
        integer(int64)             :: generation = 0_int64
        !> Flattened child indices; a node owns args(first_arg : first_arg+n_args-1).
        integer,      allocatable :: args(:)
        integer                   :: n_args = 0
        type(str_t),  allocatable :: names(:)
        integer(int64), allocatable :: name_hashes(:)
        integer,      allocatable :: name_next(:)
        integer,      allocatable :: name_buckets(:)
        integer                   :: n_names = 0
        integer,      allocatable :: buckets(:)
    contains
        procedure :: init => arena_init
        procedure :: clear => arena_clear
        procedure :: size => arena_size
        procedure :: generation_value => arena_generation

        procedure :: int => arena_int
        procedure :: rat => arena_rat
        procedure :: exact => arena_exact
        procedure :: real => arena_real
        procedure :: real_text => arena_real_text
        procedure :: algebraic => arena_algebraic
        procedure :: sym => arena_sym
        procedure :: const => arena_const
        procedure :: add => arena_add
        procedure :: mul => arena_mul
        procedure :: pow => arena_pow
        procedure :: func => arena_func

        procedure :: kind_of => arena_kind_of
        procedure :: num_of => arena_num_of
        procedure :: den_of => arena_den_of
        procedure :: real_of => arena_real_of
        procedure :: real_text_of => arena_real_text_of
        procedure :: algebraic_text_of => arena_algebraic_text_of
        procedure :: name_of => arena_name_of
        procedure :: exact_text_of => arena_exact_text_of
        procedure :: nargs_of => arena_nargs_of
        procedure :: arg_of => arena_arg_of
        procedure :: node_count => arena_node_count
        procedure :: operation_count => arena_operation_count
    end type arena_t

contains

    pure function node_kind_name(kind) result(s)
        integer, intent(in) :: kind
        type(str_t)         :: s
        select case (kind)
        case (NK_INT);   s = str("integer")
        case (NK_RAT);   s = str("rational")
        case (NK_REAL);  s = str("real")
        case (NK_SYM);   s = str("symbol")
        case (NK_CONST); s = str("constant")
        case (NK_ADD);   s = str("add")
        case (NK_MUL);   s = str("mul")
        case (NK_POW);   s = str("pow")
        case (NK_FUNC);  s = str("function")
        case (NK_BIG_INT); s = str("arbitrary-precision integer")
        case (NK_BIG_RAT); s = str("arbitrary-precision rational")
        case (NK_BIG_REAL); s = str("arbitrary-precision real")
        case (NK_ALGEBRAIC); s = str("algebraic")
        case default;    s = str("unknown")
        end select
    end function node_kind_name

    ! ------------------------------------------------------- arena setup --

    subroutine arena_init(self)
        class(arena_t), intent(inout) :: self
        call arena_clear(self)
        allocate (self%nodes(INITIAL_NODES))
        allocate (self%args(INITIAL_ARGS))
        allocate (self%names(INITIAL_NAMES))
        allocate (self%name_hashes(INITIAL_NAMES), source=0_int64)
        allocate (self%name_next(INITIAL_NAMES), source=0)
        allocate (self%name_buckets(INITIAL_NAME_BUCKETS), source=0)
        allocate (self%buckets(INITIAL_BUCKETS), source=0)
        self%n_nodes = 0
        self%n_args = 0
        self%n_names = 0
    end subroutine arena_init

    subroutine arena_clear(self)
        class(arena_t), intent(inout) :: self
        self%generation = self%generation + 1_int64
        if (allocated(self%nodes)) deallocate (self%nodes)
        if (allocated(self%args)) deallocate (self%args)
        if (allocated(self%names)) deallocate (self%names)
        if (allocated(self%name_hashes)) deallocate (self%name_hashes)
        if (allocated(self%name_next)) deallocate (self%name_next)
        if (allocated(self%name_buckets)) deallocate (self%name_buckets)
        if (allocated(self%buckets)) deallocate (self%buckets)
        self%n_nodes = 0
        self%n_args = 0
        self%n_names = 0
    end subroutine arena_clear

    pure function arena_size(self) result(n)
        class(arena_t), intent(in) :: self
        integer                    :: n
        n = self%n_nodes
    end function arena_size

    pure function arena_generation(self) result(generation)
        class(arena_t), intent(in) :: self
        integer(int64)              :: generation
        generation = self%generation
    end function arena_generation

    !> Ensure the arena is usable even if the caller forgot to call init. A CAS
    !> is used interactively often enough that failing on a missing init would
    !> be a needless sharp edge.
    subroutine ensure_ready(self)
        class(arena_t), intent(inout) :: self
        if (.not. allocated(self%nodes)) call arena_init(self)
    end subroutine ensure_ready

    ! ------------------------------------------------------------ growth --

    subroutine grow_nodes(self)
        class(arena_t), intent(inout) :: self
        type(node_t), allocatable :: bigger(:)
        if (self%n_nodes < size(self%nodes)) return
        allocate (bigger(size(self%nodes)*2))
        bigger(1:self%n_nodes) = self%nodes(1:self%n_nodes)
        call move_alloc(bigger, self%nodes)
    end subroutine grow_nodes

    subroutine grow_args(self, needed)
        class(arena_t), intent(inout) :: self
        integer,        intent(in)    :: needed
        integer, allocatable :: bigger(:)
        integer :: cap
        if (self%n_args + needed <= size(self%args)) return
        cap = size(self%args)
        do while (cap < self%n_args + needed)
            cap = cap*2
        end do
        allocate (bigger(cap), source=0)
        bigger(1:self%n_args) = self%args(1:self%n_args)
        call move_alloc(bigger, self%args)
    end subroutine grow_args

    ! ------------------------------------------------------- name table --

    !> Intern a symbol, head, or arbitrary exact payload. Exact coefficients
    !> need not be few, so the text pool has its own resizing hash table.
    function intern_name(self, name) result(id)
        class(arena_t), intent(inout) :: self
        character(*),   intent(in)    :: name
        integer                       :: id
        type(str_t), allocatable :: bigger(:)
        integer(int64), allocatable :: bigger_hashes(:)
        integer, allocatable :: bigger_next(:)
        integer(int64) :: h
        integer :: i, b

        call ensure_ready(self)
        h = hash_name(name)
        b = name_bucket_for_count(h, size(self%name_buckets))
        i = self%name_buckets(b)
        do while (i /= 0)
            if (self%name_hashes(i) == h) then
                if (self%names(i) == name) then
                    id = i
                    return
                end if
            end if
            i = self%name_next(i)
        end do

        if (self%n_names >= size(self%name_buckets)) then
            call grow_name_buckets(self)
            b = name_bucket_for_count(h, size(self%name_buckets))
        end if
        if (self%n_names >= size(self%names)) then
            allocate (bigger(size(self%names)*2))
            bigger(1:self%n_names) = self%names(1:self%n_names)
            call move_alloc(bigger, self%names)
            allocate (bigger_hashes(size(self%name_hashes)*2), source=0_int64)
            bigger_hashes(1:self%n_names) = self%name_hashes(1:self%n_names)
            call move_alloc(bigger_hashes, self%name_hashes)
            allocate (bigger_next(size(self%name_next)*2), source=0)
            bigger_next(1:self%n_names) = self%name_next(1:self%n_names)
            call move_alloc(bigger_next, self%name_next)
        end if

        self%n_names = self%n_names + 1
        self%names(self%n_names) = str(name)
        self%name_hashes(self%n_names) = h
        self%name_next(self%n_names) = self%name_buckets(b)
        self%name_buckets(b) = self%n_names
        id = self%n_names
    end function intern_name

    pure function hash_name(name) result(h)
        character(*), intent(in) :: name
        integer(int64)           :: h
        integer(int64), parameter :: OFFSET = -3750763034362895579_int64
        integer :: i

        h = OFFSET
        do i = 1, len(name)
            h = mix(h, int(iachar(name(i:i)), int64))
        end do
    end function hash_name

    pure function name_bucket_for_count(h, count) result(b)
        integer(int64), intent(in) :: h
        integer,        intent(in) :: count
        integer                    :: b
        b = int(iand(ishft(h, -13), int(count - 1, int64))) + 1
    end function name_bucket_for_count

    subroutine grow_name_buckets(self)
        class(arena_t), intent(inout) :: self
        integer, allocatable :: larger(:)
        integer :: b, i

        allocate (larger(2*size(self%name_buckets)), source=0)
        do i = 1, self%n_names
            b = name_bucket_for_count(self%name_hashes(i), size(larger))
            self%name_next(i) = larger(b)
            larger(b) = i
        end do
        call move_alloc(larger, self%name_buckets)
    end subroutine grow_name_buckets

    ! ------------------------------------------------------------ hashing --

    !> Overflow-free bitwise mixing over the fields that define a node. Any two
    !> nodes that must be considered identical have to hash the same, so this
    !> mixes exactly the fields that node_equal compares. Rotations, shifts,
    !> and XOR have defined integer semantics; signed multiplication overflow
    !> does not.
    pure function mix(h, v) result(r)
        integer(int64), intent(in) :: h, v
        integer(int64)             :: r
        integer(int64), parameter :: SALT = -7046029254386353131_int64

        r = ieor(ishftc(h, 21), v)
        r = ieor(r, SALT)
        r = ieor(r, ishft(r, -29))
        r = ieor(r, ishft(r, 17))
    end function mix

    pure function hash_fields(kind, num, den, rval, name, argv) result(h)
        integer,        intent(in) :: kind
        integer(int64), intent(in) :: num, den
        real(dp),       intent(in) :: rval
        integer,        intent(in) :: name
        integer,        intent(in) :: argv(:)
        integer(int64)             :: h
        integer(int64), parameter :: OFFSET = -3750763034362895579_int64
        integer :: i

        h = OFFSET
        h = mix(h, int(kind, int64))
        h = mix(h, num)
        h = mix(h, den)
        h = mix(h, transfer(rval, 0_int64))
        h = mix(h, int(name, int64))
        do i = 1, size(argv)
            h = mix(h, int(argv(i), int64))
        end do
    end function hash_fields

    pure function bucket_of(self, h) result(b)
        class(arena_t), intent(in) :: self
        integer(int64), intent(in) :: h
        integer                    :: b
        b = bucket_for_count(h, size(self%buckets))
    end function bucket_of

    pure function bucket_for_count(h, count) result(b)
        integer(int64), intent(in) :: h
        integer,        intent(in) :: count
        integer                    :: b
        ! Bucket count is a power of two, so masking replaces a modulo. The
        ! shift first discards the low bits, which FNV mixes least.
        b = int(iand(ishft(h, -13), int(count - 1, int64))) + 1
    end function bucket_for_count

    !> Double and rebuild the bucket table before its average chain exceeds
    !> one node. Node indices and argument slices stay unchanged; only the
    !> collision links are reconstructed.
    subroutine grow_buckets(self)
        class(arena_t), intent(inout) :: self
        integer, allocatable :: larger(:)
        integer :: b, i

        allocate (larger(2*size(self%buckets)), source=0)
        do i = 1, self%n_nodes
            b = bucket_for_count(self%nodes(i)%hash, size(larger))
            self%nodes(i)%next = larger(b)
            larger(b) = i
        end do
        call move_alloc(larger, self%buckets)
    end subroutine grow_buckets

    !> Structural identity for interning.
    pure function node_equal(self, idx, kind, num, den, rval, name, argv) &
            result(same)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx, kind, name
        integer(int64), intent(in) :: num, den
        real(dp),       intent(in) :: rval
        integer,        intent(in) :: argv(:)
        logical                    :: same
        integer :: i

        same = .false.
        if (self%nodes(idx)%kind /= kind) return
        if (self%nodes(idx)%n_args /= size(argv)) return
        if (self%nodes(idx)%name /= name) return
        if (self%nodes(idx)%num /= num) return
        if (self%nodes(idx)%den /= den) return
        ! Bit comparison, not numeric: interning must not fuse two distinct
        ! literals, and it must keep -0.0 and 0.0 apart because they print
        ! differently in generated code.
        if (transfer(self%nodes(idx)%rval, 0_int64) /= transfer(rval, 0_int64)) &
            return

        do i = 1, size(argv)
            if (self%args(self%nodes(idx)%first_arg + i - 1) /= argv(i)) return
        end do

        same = .true.
    end function node_equal

    !> Return the index of a node with these fields, creating it only if no
    !> identical node exists. This is the single point where nodes are made.
    function intern(self, kind, num, den, rval, name, argv) result(idx)
        class(arena_t), intent(inout) :: self
        integer,        intent(in)    :: kind, name
        integer(int64), intent(in)    :: num, den
        real(dp),       intent(in)    :: rval
        integer,        intent(in)    :: argv(:)
        integer                       :: idx
        integer(int64) :: h
        integer :: b, cur, i

        call ensure_ready(self)

        h = hash_fields(kind, num, den, rval, name, argv)
        b = bucket_of(self, h)

        cur = self%buckets(b)
        do while (cur /= 0)
            if (self%nodes(cur)%hash == h) then
                if (node_equal(self, cur, kind, num, den, rval, name, argv)) then
                    idx = cur
                    return
                end if
            end if
            cur = self%nodes(cur)%next
        end do

        if (self%n_nodes >= size(self%buckets)) then
            call grow_buckets(self)
            b = bucket_of(self, h)
        end if

        call grow_nodes(self)
        call grow_args(self, size(argv))

        self%n_nodes = self%n_nodes + 1
        idx = self%n_nodes

        self%nodes(idx)%kind = kind
        self%nodes(idx)%num = num
        self%nodes(idx)%den = den
        self%nodes(idx)%rval = rval
        self%nodes(idx)%name = name
        self%nodes(idx)%hash = h
        self%nodes(idx)%n_args = size(argv)
        self%nodes(idx)%first_arg = self%n_args + 1

        do i = 1, size(argv)
            self%args(self%n_args + i) = argv(i)
        end do
        self%n_args = self%n_args + size(argv)

        ! Chain onto the bucket. Prepending keeps insertion O(1); lookup walks
        ! the chain and compares the full hash before the field-by-field test.
        self%nodes(idx)%next = self%buckets(b)
        self%buckets(b) = idx
    end function intern

    ! ------------------------------------------------------ constructors --

    function arena_int(self, value) result(idx)
        class(arena_t), intent(inout) :: self
        integer(int64), intent(in)    :: value
        integer                       :: idx
        integer :: none(0)
        idx = intern(self, NK_INT, value, 1_int64, 0.0_dp, 0, none)
    end function arena_int

    !> Rational, stored in lowest terms with a positive denominator so that
    !> 2/4, 1/2 and -1/-2 all intern to one node.
    recursive function arena_rat(self, numer, denom) result(idx)
        class(arena_t), intent(inout) :: self
        integer(int64), intent(in)    :: numer, denom
        integer                       :: idx
        integer(int64), parameter :: MIN_I64 = -huge(0_int64) - 1_int64
        integer(int64) :: n, d, g
        integer :: none(0)
        logical :: valid

        n = numer
        d = denom
        if (d == 0_int64) then
            ! A zero denominator has no node. Callers get the integer zero
            ! rather than a poisoned arena; division by zero is diagnosed at the
            ! operator level where there is a status channel to report it.
            idx = arena_int(self, 0_int64)
            return
        end if
        ! Neither -MIN_I64 nor mod(MIN_I64, -1) is representable. Normalize
        ! these sign-sensitive inputs in the arbitrary exact domain before
        ! attempting any machine negation or Euclidean remainder.
        if (d == MIN_I64 .or. (d < 0_int64 .and. n == MIN_I64)) then
            idx = arena_exact(self, &
                chars(str(numer)//"/"//str(denom)), valid)
            if (.not. valid) idx = 0
            return
        end if
        if (d < 0_int64) then
            n = -n
            d = -d
        end if
        ! Pass the signed numerator directly. abs(min_int64) is not
        ! representable, while Euclid's remainders become bounded by the
        ! positive denominator after the first step.
        g = gcd_i64(n, d)
        if (g > 1_int64) then
            n = n/g
            d = d/g
        end if
        if (d == 1_int64) then
            idx = arena_int(self, n)
        else
            idx = intern(self, NK_RAT, n, d, 0.0_dp, 0, none)
        end if
    end function arena_rat

    !> Canonical exact scalar of arbitrary size. Values representable in the
    !> compact int64 node kinds retain that fast path; only larger values store
    !> their canonical FLINT rendering in the arena name table.
    recursive function arena_exact(self, value, ok) result(idx)
        class(arena_t), intent(inout) :: self
        character(*),   intent(in)    :: value
        logical,        intent(out), optional :: ok
        integer                       :: idx
        type(str_t) :: canonical
        character(:), allocatable :: text, numerator, denominator
        integer(int64) :: n, d
        integer :: slash, ios_n, ios_d, name_id
        integer :: none(0)
        logical :: valid

        canonical = exact_normalize(value, valid)
        if (present(ok)) ok = valid
        if (.not. valid) then
            idx = 0
            return
        end if

        text = chars(canonical)
        slash = index(text, "/")
        if (slash == 0) then
            read (text, *, iostat=ios_n) n
            if (ios_n == 0) then
                idx = arena_int(self, n)
                return
            end if
            name_id = intern_name(self, text)
            idx = intern(self, NK_BIG_INT, 0_int64, 1_int64, 0.0_dp, &
                name_id, none)
            return
        end if

        numerator = text(:slash - 1)
        denominator = text(slash + 1:)
        read (numerator, *, iostat=ios_n) n
        read (denominator, *, iostat=ios_d) d
        if (ios_n == 0 .and. ios_d == 0) then
            idx = arena_rat(self, n, d)
            return
        end if
        name_id = intern_name(self, text)
        idx = intern(self, NK_BIG_RAT, 0_int64, 1_int64, 0.0_dp, &
            name_id, none)
    end function arena_exact

    pure function gcd_i64(a, b) result(g)
        integer(int64), intent(in) :: a, b
        integer(int64)             :: g
        integer(int64) :: x, y, t
        x = a
        y = b
        do while (y /= 0_int64)
            t = mod(x, y)
            x = y
            y = t
        end do
        g = abs(x)
    end function gcd_i64

    function arena_real(self, value) result(idx)
        class(arena_t), intent(inout) :: self
        real(dp),       intent(in)    :: value
        integer                       :: idx
        integer :: none(0)
        idx = intern(self, NK_REAL, 0_int64, 1_int64, value, 0, none)
    end function arena_real

    !> Bounded decimal result retaining more precision than real64. The text is
    !> produced by the MPFR-backed evaluator, but the arena still checks the
    !> ownership boundary so a malformed or unbounded value cannot enter the
    !> shared expression graph.
    function arena_real_text(self, value, ok) result(idx)
        class(arena_t), intent(inout) :: self
        character(*),   intent(in)    :: value
        logical,        intent(out)   :: ok
        integer                       :: idx
        integer :: none(0), name_id

        ok = valid_real_text(value)
        if (.not. ok) then
            idx = 0
            return
        end if
        name_id = intern_name(self, value)
        idx = intern(self, NK_BIG_REAL, 0_int64, 1_int64, 0.0_dp, name_id, none)
    end function arena_real_text

    !> Canonical exact real or complex algebraic value. The qqbar1 payload is
    !> retained losslessly in the arena name table, so an algebraic atom can
    !> participate in structural expressions without exposing a FLINT object.
    function arena_algebraic(self, value, ok) result(idx)
        class(arena_t), intent(inout) :: self
        character(*),   intent(in)    :: value
        logical,        intent(out)   :: ok
        integer                       :: idx
        integer :: none(0), name_id
        type(str_t) :: canonical

        canonical = algebraic_normalize(value, ok)
        if (.not. ok) then
            idx = 0
            return
        end if
        name_id = intern_name(self, chars(canonical))
        idx = intern(self, NK_ALGEBRAIC, 0_int64, 1_int64, 0.0_dp, &
            name_id, none)
    end function arena_algebraic

    function arena_sym(self, name) result(idx)
        class(arena_t), intent(inout) :: self
        character(*),   intent(in)    :: name
        integer                       :: idx
        integer :: none(0), id
        id = intern_name(self, name)
        idx = intern(self, NK_SYM, 0_int64, 1_int64, 0.0_dp, id, none)
    end function arena_sym

    function arena_const(self, name) result(idx)
        class(arena_t), intent(inout) :: self
        character(*),   intent(in)    :: name
        integer                       :: idx
        integer :: none(0), id
        id = intern_name(self, name)
        idx = intern(self, NK_CONST, 0_int64, 1_int64, 0.0_dp, id, none)
    end function arena_const

    !> Sum. Nested sums are flattened and operands sorted, so that x+(y+z),
    !> (x+y)+z and z+y+x all reach the same node.
    function arena_add(self, operands) result(idx)
        class(arena_t), intent(inout) :: self
        integer,        intent(in)    :: operands(:)
        integer                       :: idx
        integer                       :: pair(2)
        integer, allocatable :: flat(:)

        ! Binary arithmetic is the dominant construction path.  When neither
        ! operand is already an addition, flattening cannot change the list,
        ! so keep the pair on the stack and avoid two heap allocations.
        if (size(operands) == 2) then
            if (self%nodes(operands(1))%kind /= NK_ADD .and. &
                self%nodes(operands(2))%kind /= NK_ADD) then
                pair = operands
                call sort_pair(self, pair)
                idx = intern(self, NK_ADD, 0_int64, 1_int64, 0.0_dp, 0, pair)
                return
            end if
        end if

        call flatten(self, operands, NK_ADD, flat)
        if (size(flat) == 1) then
            idx = flat(1)
            return
        end if
        call sort_semantic(self, flat)
        idx = intern(self, NK_ADD, 0_int64, 1_int64, 0.0_dp, 0, flat)
    end function arena_add

    function arena_mul(self, operands) result(idx)
        class(arena_t), intent(inout) :: self
        integer,        intent(in)    :: operands(:)
        integer                       :: idx
        integer                       :: pair(2)
        integer, allocatable :: flat(:)

        if (size(operands) == 2) then
            if (self%nodes(operands(1))%kind /= NK_MUL .and. &
                self%nodes(operands(2))%kind /= NK_MUL) then
                pair = operands
                call sort_pair(self, pair)
                idx = intern(self, NK_MUL, 0_int64, 1_int64, 0.0_dp, 0, pair)
                return
            end if
        end if

        call flatten(self, operands, NK_MUL, flat)
        if (size(flat) == 1) then
            idx = flat(1)
            return
        end if
        call sort_semantic(self, flat)
        idx = intern(self, NK_MUL, 0_int64, 1_int64, 0.0_dp, 0, flat)
    end function arena_mul

    !> Splice any operand that is itself an `outer` node into the operand list,
    !> so associativity never produces two shapes for one expression.
    subroutine flatten(self, operands, outer, flat)
        class(arena_t),       intent(in)  :: self
        integer,              intent(in)  :: operands(:), outer
        integer, allocatable, intent(out) :: flat(:)
        integer :: i, j, n

        n = 0
        do i = 1, size(operands)
            if (self%nodes(operands(i))%kind == outer) then
                n = n + self%nodes(operands(i))%n_args
            else
                n = n + 1
            end if
        end do

        allocate (flat(n))
        n = 0
        do i = 1, size(operands)
            if (self%nodes(operands(i))%kind == outer) then
                do j = 1, self%nodes(operands(i))%n_args
                    n = n + 1
                    flat(n) = self%args(self%nodes(operands(i))%first_arg + j - 1)
                end do
            else
                n = n + 1
                flat(n) = operands(i)
            end if
        end do
    end subroutine flatten

    pure subroutine sort_pair(self, values)
        class(arena_t), intent(in) :: self
        integer, intent(inout) :: values(2)
        integer :: swap

        if (compare_nodes(self, values(1), values(2)) <= 0) return
        swap = values(1)
        values(1) = values(2)
        values(2) = swap
    end subroutine sort_pair

    !> Bottom-up merge sort by stable structure. Expansion can feed this up to
    !> the documented term bound, so quadratic insertion sorting is not safe.
    !> One scratch allocation serves every merge pass.
    pure subroutine sort_semantic(self, v)
        class(arena_t), intent(in)    :: self
        integer, intent(inout) :: v(:)
        integer, allocatable :: scratch(:)
        integer :: width, left, middle, right, i, j, out

        if (size(v) < 2) return
        allocate (scratch(size(v)))
        width = 1
        do while (width < size(v))
            left = 1
            do while (left <= size(v))
                middle = left + min(width, size(v) - left + 1)
                right = middle - 1 + min(width, size(v) - middle + 1)
                i = left
                j = middle
                out = left
                do while (i < middle .and. j <= right)
                    if (compare_nodes(self, v(i), v(j)) <= 0) then
                        scratch(out) = v(i)
                        i = i + 1
                    else
                        scratch(out) = v(j)
                        j = j + 1
                    end if
                    out = out + 1
                end do
                do while (i < middle)
                    scratch(out) = v(i)
                    i = i + 1
                    out = out + 1
                end do
                do while (j <= right)
                    scratch(out) = v(j)
                    j = j + 1
                    out = out + 1
                end do
                left = right + 1
            end do
            v = scratch
            if (width > size(v)/2) exit
            width = 2*width
        end do
    end subroutine sort_semantic

    !> Total structural order for nodes in one arena. Node and name-table
    !> indices are deliberately absent: both depend on construction history.
    !> The precedence retains fortsym's readable convention of symbols before
    !> compound terms and numeric coefficients last.
    recursive pure function compare_nodes(self, left, right) result(relation)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: left, right
        integer                    :: relation
        integer(int64) :: left_bits, right_bits
        integer :: i, left_kind, right_kind, left_rank, right_rank

        relation = 0
        if (left == right) return

        left_kind = self%nodes(left)%kind
        right_kind = self%nodes(right)%kind
        left_rank = semantic_kind_rank(left_kind)
        right_rank = semantic_kind_rank(right_kind)
        relation = compare_default_int(left_rank, right_rank)
        if (relation /= 0) return
        relation = compare_default_int(left_kind, right_kind)
        if (relation /= 0) return

        select case (left_kind)
        case (NK_INT)
            relation = compare_int64(self%nodes(left)%num, &
                self%nodes(right)%num)
            return
        case (NK_RAT)
            relation = compare_int64(self%nodes(left)%num, &
                self%nodes(right)%num)
            if (relation /= 0) return
            relation = compare_int64(self%nodes(left)%den, &
                self%nodes(right)%den)
            return
        case (NK_REAL)
            left_bits = transfer(self%nodes(left)%rval, 0_int64)
            right_bits = transfer(self%nodes(right)%rval, 0_int64)
            relation = compare_int64(left_bits, right_bits)
            return
        case (NK_SYM, NK_CONST, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL, &
                NK_ALGEBRAIC)
            relation = compare_str(self%names(self%nodes(left)%name), &
                self%names(self%nodes(right)%name))
            if (relation /= 0) return
        end select

        do i = 1, min(self%nodes(left)%n_args, self%nodes(right)%n_args)
            relation = compare_nodes(self, &
                self%args(self%nodes(left)%first_arg + i - 1), &
                self%args(self%nodes(right)%first_arg + i - 1))
            if (relation /= 0) return
        end do
        relation = compare_default_int(self%nodes(left)%n_args, &
            self%nodes(right)%n_args)
    end function compare_nodes

    pure function semantic_kind_rank(kind) result(rank)
        integer, intent(in) :: kind
        integer             :: rank

        select case (kind)
        case (NK_SYM);   rank = 1
        case (NK_CONST); rank = 2
        case (NK_FUNC);  rank = 3
        case (NK_POW);   rank = 4
        case (NK_MUL);   rank = 5
        case (NK_ADD);   rank = 6
        case (NK_INT);   rank = 7
        case (NK_BIG_INT); rank = 8
        case (NK_RAT);   rank = 9
        case (NK_BIG_RAT); rank = 10
        case (NK_REAL);  rank = 11
        case (NK_BIG_REAL); rank = 12
        case (NK_ALGEBRAIC); rank = 13
        case default;    rank = 14
        end select
    end function semantic_kind_rank

    pure function compare_default_int(left, right) result(relation)
        integer, intent(in) :: left, right
        integer             :: relation

        if (left < right) then
            relation = -1
        else if (left > right) then
            relation = 1
        else
            relation = 0
        end if
    end function compare_default_int

    pure function compare_int64(left, right) result(relation)
        integer(int64), intent(in) :: left, right
        integer                     :: relation

        if (left < right) then
            relation = -1
        else if (left > right) then
            relation = 1
        else
            relation = 0
        end if
    end function compare_int64

    !> Power is binary and non-commutative, so operands keep their order.
    function arena_pow(self, base, expo) result(idx)
        class(arena_t), intent(inout) :: self
        integer,        intent(in)    :: base, expo
        integer                       :: idx
        integer :: args(2)
        character(:), allocatable :: exponent_name

        if (self%nodes(expo)%kind == NK_INT .and. &
            self%nodes(expo)%num == 0_int64) then
            idx = self%int(1_int64)
            return
        end if
        if (self%nodes(expo)%kind == NK_INT .and. &
            self%nodes(expo)%num == 1_int64) then
            idx = base
            return
        end if
        if (self%nodes(base)%kind == NK_INT .and. &
            self%nodes(base)%num == 1_int64) then
            if (self%nodes(expo)%kind == NK_CONST) then
                exponent_name = chars(self%name_of(expo))
                if (exponent_name == "oo" .or. exponent_name == "zoo" .or. &
                    exponent_name == "nan") then
                    idx = self%const("nan")
                    return
                end if
            end if
            idx = base
            return
        end if
        if (self%nodes(base)%kind == NK_FUNC .and. &
            self%nodes(base)%n_args == 1 .and. &
            self%nodes(expo)%kind == NK_INT .and. &
            self%nodes(expo)%num == 2_int64 .and. &
            chars(self%name_of(base)) == "sqrt") then
            idx = self%arg_of(base, 1)
            return
        end if

        args(1) = base
        args(2) = expo
        idx = intern(self, NK_POW, 0_int64, 1_int64, 0.0_dp, 0, args)
    end function arena_pow

    !> Function application. Arguments keep their order: f(x,y) is not f(y,x).
    function arena_func(self, name, fargs) result(idx)
        class(arena_t), intent(inout) :: self
        character(*),   intent(in)    :: name
        integer,        intent(in)    :: fargs(:)
        integer                       :: idx
        integer :: id
        id = intern_name(self, name)
        idx = intern(self, NK_FUNC, 0_int64, 1_int64, 0.0_dp, id, fargs)
    end function arena_func

    ! ---------------------------------------------------------- accessors --

    pure function arena_kind_of(self, idx) result(k)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx
        integer                    :: k
        k = self%nodes(idx)%kind
    end function arena_kind_of

    pure function arena_num_of(self, idx) result(v)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx
        integer(int64)             :: v
        v = self%nodes(idx)%num
    end function arena_num_of

    pure function arena_den_of(self, idx) result(v)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx
        integer(int64)             :: v
        v = self%nodes(idx)%den
    end function arena_den_of

    pure function arena_real_of(self, idx) result(v)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx
        real(dp)                   :: v
        v = self%nodes(idx)%rval
    end function arena_real_of

    pure function arena_real_text_of(self, idx) result(s)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx
        type(str_t)                :: s
        if (self%nodes(idx)%kind == NK_BIG_REAL .and. &
            self%nodes(idx)%name > 0) then
            s = self%names(self%nodes(idx)%name)
        else
            s = str("")
        end if
    end function arena_real_text_of

    pure function arena_algebraic_text_of(self, idx) result(s)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx
        type(str_t)                :: s

        if (self%nodes(idx)%kind == NK_ALGEBRAIC .and. &
            self%nodes(idx)%name > 0) then
            s = self%names(self%nodes(idx)%name)
        else
            s = str("")
        end if
    end function arena_algebraic_text_of

    pure function valid_real_text(value) result(valid)
        character(*), intent(in) :: value
        logical                  :: valid
        integer :: n, p, digits_before, digits_after, exponent_digits
        character :: c

        valid = .false.
        if (len(value) == 0 .or. len(value) > MAX_BIG_REAL_TEXT) return
        if (index(value, achar(0)) /= 0) return
        n = len_trim(value)
        if (n /= len(value)) return

        p = 1
        if (value(p:p) == "+" .or. value(p:p) == "-") then
            p = p + 1
            if (p > n) return
        end if

        digits_before = 0
        do while (p <= n)
            c = value(p:p)
            if (c < "0" .or. c > "9") exit
            digits_before = digits_before + 1
            p = p + 1
        end do

        digits_after = 0
        if (p <= n) then
            if (value(p:p) == ".") then
                p = p + 1
                do while (p <= n)
                    c = value(p:p)
                    if (c < "0" .or. c > "9") exit
                    digits_after = digits_after + 1
                    p = p + 1
                end do
            end if
        end if
        if (digits_before + digits_after == 0) return

        exponent_digits = 0
        if (p <= n) then
            if (value(p:p) == "e" .or. value(p:p) == "E") then
                p = p + 1
                if (p > n) return
                if (value(p:p) == "+" .or. value(p:p) == "-") then
                    p = p + 1
                    if (p > n) return
                end if
                do while (p <= n)
                    c = value(p:p)
                    if (c < "0" .or. c > "9") exit
                    exponent_digits = exponent_digits + 1
                    p = p + 1
                end do
                if (exponent_digits == 0) return
            end if
        end if

        valid = p > n
    end function valid_real_text

    pure function arena_name_of(self, idx) result(s)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx
        type(str_t)                :: s
        if (self%nodes(idx)%name > 0) then
            s = self%names(self%nodes(idx)%name)
        else
            s = str("")
        end if
    end function arena_name_of

    !> Canonical base-ten spelling for any exact scalar node.
    pure function arena_exact_text_of(self, idx) result(s)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx
        type(str_t)                :: s

        select case (self%nodes(idx)%kind)
        case (NK_INT)
            s = str(self%nodes(idx)%num)
        case (NK_RAT)
            s = str(self%nodes(idx)%num)//"/"//str(self%nodes(idx)%den)
        case (NK_BIG_INT, NK_BIG_RAT)
            s = self%names(self%nodes(idx)%name)
        case default
            s = str("")
        end select
    end function arena_exact_text_of

    pure function arena_nargs_of(self, idx) result(n)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx
        integer                    :: n
        n = self%nodes(idx)%n_args
    end function arena_nargs_of

    pure function arena_arg_of(self, idx, k) result(a)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx, k
        integer                    :: a
        a = self%args(self%nodes(idx)%first_arg + k - 1)
    end function arena_arg_of

    !> Number of distinct nodes reachable from idx. Because the graph is
    !> hash-consed this counts shared subtrees once, which makes it a measure of
    !> the work a generated kernel does rather than of how the expression
    !> happens to be written.
    function arena_node_count(self, idx) result(n)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx
        integer                    :: n
        logical, allocatable :: seen(:)
        allocate (seen(self%n_nodes), source=.false.)
        n = 0
        call visit(self, idx, seen, n)
    end function arena_node_count

    recursive subroutine visit(self, idx, seen, n)
        class(arena_t), intent(in)    :: self
        integer,        intent(in)    :: idx
        logical,        intent(inout) :: seen(:)
        integer,        intent(inout) :: n
        integer :: i
        if (seen(idx)) return
        seen(idx) = .true.
        n = n + 1
        do i = 1, self%nodes(idx)%n_args
            call visit(self, self%args(self%nodes(idx)%first_arg + i - 1), seen, n)
        end do
    end subroutine visit

    !> Count operation occurrences in the expression tree. Unlike node_count,
    !> repeated references to an interned child count once per occurrence, as
    !> they do in SymPy's count_ops. N-ary sums and products contribute one
    !> operation per link between operands.
    recursive function arena_operation_count(self, idx) result(n)
        class(arena_t), intent(in) :: self
        integer,        intent(in) :: idx
        integer                    :: n, i, child, exponent
        logical                    :: has_reciprocal

        has_reciprocal = .false.
        select case (self%nodes(idx)%kind)
        case (NK_ADD, NK_MUL)
            n = max(0, self%nodes(idx)%n_args - 1)
        case (NK_POW, NK_FUNC, NK_RAT, NK_BIG_RAT)
            n = 1
        case default
            n = 0
        end select
        do i = 1, self%nodes(idx)%n_args
            child = self%args(self%nodes(idx)%first_arg + i - 1)
            if (self%nodes(idx)%kind == NK_MUL) then
                if (self%nodes(child)%kind == NK_POW) then
                    if (self%nodes(child)%n_args == 2) then
                        exponent = self%args(self%nodes(child)%first_arg + 1)
                        if (self%nodes(exponent)%kind == NK_INT) then
                            if (self%nodes(exponent)%num == -1_int64) then
                                has_reciprocal = .true.
                            end if
                        end if
                    end if
                end if
            end if
            n = n + self%operation_count(child)
        end do
        if (has_reciprocal) n = n - 1
    end function arena_operation_count

end module fortsym_arena
