module fortsym_string
    ! Strings for fortsym: a value type and an append buffer.
    !
    ! Fortran's fixed-length CHARACTER is the wrong tool for a symbolic library.
    ! Expressions have no length bound, so a fixed buffer is either too small or
    ! mostly padding, and padding forces TRIM at every use. TRIM is not merely
    ! noise: it hides the difference between a string that ends in spaces and one
    ! that was padded, and it copies. fortsym therefore uses deferred-length
    ! allocatable character throughout, wrapped in str_t so strings can live in
    ! arrays and derived types.
    !
    ! There is no TRIM anywhere above this module, and no fixed-length character
    ! variable outside the two integer/real formatting helpers below, where the
    ! standard gives no alternative. Those helpers slice by the written length
    ! and hand back an exact-length result, so padding never escapes.
    !
    ! strbuf_t exists because building generated code by repeated
    !     s = s // more
    ! is quadratic: every concatenation reallocates and copies the whole string.
    ! A kernel with thousands of statements makes that the dominant cost. The
    ! buffer grows geometrically and copies once, at the end.
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    implicit none
    private

    public :: str_t, strbuf_t
    public :: str, chars, len_str
    public :: operator(//), operator(==), operator(/=)
    public :: assignment(=)

    integer, parameter :: dp = real64

    ! Smallest buffer a strbuf_t allocates. Below this the growth arithmetic
    ! costs more than the memory it saves.
    integer, parameter :: MIN_CAPACITY = 64

    !> An immutable string value of exactly the length of its contents.
    type :: str_t
        character(:), allocatable, private :: data
    contains
        procedure :: len => str_len
        procedure :: chars => str_chars
        procedure :: is_empty => str_is_empty
        procedure :: sub => str_sub
        procedure :: starts_with => str_starts_with
        procedure :: ends_with => str_ends_with
        procedure :: index_of => str_index_of
    end type str_t

    !> Append-only builder with amortised O(1) append and a single final copy.
    type :: strbuf_t
        character(:), allocatable, private :: data
        integer,                  private :: length = 0
    contains
        procedure, private :: buf_append_chars
        procedure, private :: buf_append_str
        procedure, private :: buf_append_int
        procedure, private :: buf_append_real
        generic   :: append => buf_append_chars, buf_append_str, &
            buf_append_int, buf_append_real
        procedure :: newline => buf_newline
        procedure :: len => buf_len
        procedure :: chars => buf_chars
        procedure :: to_str => buf_to_str
        procedure :: reset => buf_reset
    end type strbuf_t

    !> str(...) builds a str_t from a character, an integer, or a real.
    interface str
        module procedure str_from_chars
        module procedure str_from_int32
        module procedure str_from_int64
        module procedure str_from_real
    end interface str

    interface chars
        module procedure str_chars_fn
    end interface chars

    interface len_str
        module procedure str_len_fn
    end interface len_str

    interface assignment(=)
        module procedure str_assign_from_chars
    end interface assignment(=)

    interface operator(//)
        module procedure concat_str_str
        module procedure concat_str_chars
        module procedure concat_chars_str
    end interface operator(//)

    interface operator(==)
        module procedure eq_str_str
        module procedure eq_str_chars
        module procedure eq_chars_str
    end interface operator(==)

    interface operator(/=)
        module procedure ne_str_str
        module procedure ne_str_chars
        module procedure ne_chars_str
    end interface operator(/=)

contains

    ! ------------------------------------------------------------- str_t --

    pure function str_from_chars(c) result(s)
        character(*), intent(in) :: c
        type(str_t)              :: s
        s%data = c
    end function str_from_chars

    !> Decimal form of an integer, no padding. The fixed local buffer is the one
    !> the standard forces on us: an internal write needs a target of known
    !> length. i0 never pads on the left, so the written length is exact and the
    !> slice below is the whole value.
    pure function str_from_int32(i) result(s)
        integer(int32), intent(in) :: i
        type(str_t)                :: s
        character(len=12) :: buf
        integer           :: n
        write (buf, '(i0)') i
        n = len_trim(buf)
        s%data = buf(1:n)
    end function str_from_int32

    pure function str_from_int64(i) result(s)
        integer(int64), intent(in) :: i
        type(str_t)                :: s
        character(len=24) :: buf
        integer           :: n
        write (buf, '(i0)') i
        n = len_trim(buf)
        s%data = buf(1:n)
    end function str_from_int64

    !> Decimal form of a real. The default format round-trips through real64:
    !> 17 significant digits is the smallest count that recovers every double
    !> exactly, which matters because generated kernels must reproduce the
    !> constants the CAS computed, not an approximation of them.
    !>
    !> ES with 16 decimals gives those 17 significant digits, and the field must
    !> be wide enough for all of sign, leading digit, point, decimals, E,
    !> exponent sign and three exponent digits -- 24 characters. A field that is
    !> even one short does not truncate, it fills with asterisks, so the width
    !> here carries a digit of slack.
    pure function str_from_real(x, fmt) result(s)
        real(dp),     intent(in)           :: x
        character(*), intent(in), optional :: fmt
        type(str_t)                        :: s
        character(len=64) :: buf
        integer           :: n
        if (present(fmt)) then
            write (buf, fmt) x
        else
            write (buf, '(es25.16e3)') x
        end if
        buf = adjustl(buf)
        n = len_trim(buf)
        s%data = buf(1:n)
    end function str_from_real

    pure subroutine str_assign_from_chars(s, c)
        type(str_t),  intent(out) :: s
        character(*), intent(in)  :: c
        s%data = c
    end subroutine str_assign_from_chars

    pure function str_len(self) result(n)
        class(str_t), intent(in) :: self
        integer                  :: n
        if (allocated(self%data)) then
            n = len(self%data)
        else
            n = 0
        end if
    end function str_len

    pure function str_len_fn(s) result(n)
        type(str_t), intent(in) :: s
        integer                 :: n
        n = s%len()
    end function str_len_fn

    !> The contents, at exactly their own length. An unallocated str_t reads as
    !> empty rather than being an error, so callers need no allocation guard.
    pure function str_chars(self) result(c)
        class(str_t), intent(in)  :: self
        character(:), allocatable :: c
        if (allocated(self%data)) then
            c = self%data
        else
            c = ""
        end if
    end function str_chars

    pure function str_chars_fn(s) result(c)
        type(str_t), intent(in)   :: s
        character(:), allocatable :: c
        c = s%chars()
    end function str_chars_fn

    pure function str_is_empty(self) result(yes)
        class(str_t), intent(in) :: self
        logical                  :: yes
        yes = self%len() == 0
    end function str_is_empty

    !> Substring from first to last inclusive, clamped to the actual extent so
    !> an out-of-range request yields a shorter string instead of a crash.
    pure function str_sub(self, first, last) result(s)
        class(str_t), intent(in) :: self
        integer,      intent(in) :: first, last
        type(str_t)              :: s
        character(:), allocatable :: c
        integer :: lo, hi
        c = self%chars()
        lo = max(1, first)
        hi = min(len(c), last)
        if (lo > hi) then
            s%data = ""
        else
            s%data = c(lo:hi)
        end if
    end function str_sub

    pure function str_starts_with(self, prefix) result(yes)
        class(str_t), intent(in) :: self
        character(*), intent(in) :: prefix
        logical                  :: yes
        character(:), allocatable :: c
        c = self%chars()
        yes = len(prefix) <= len(c)
        if (yes) yes = c(1:len(prefix)) == prefix
    end function str_starts_with

    pure function str_ends_with(self, suffix) result(yes)
        class(str_t), intent(in) :: self
        character(*), intent(in) :: suffix
        logical                  :: yes
        character(:), allocatable :: c
        c = self%chars()
        yes = len(suffix) <= len(c)
        if (yes) yes = c(len(c) - len(suffix) + 1:) == suffix
    end function str_ends_with

    !> Position of the first occurrence of needle, or 0 when absent.
    pure function str_index_of(self, needle) result(pos)
        class(str_t), intent(in) :: self
        character(*), intent(in) :: needle
        integer                  :: pos
        pos = index(self%chars(), needle)
    end function str_index_of

    ! --------------------------------------------------------- operators --

    pure function concat_str_str(a, b) result(s)
        type(str_t), intent(in) :: a, b
        type(str_t)             :: s
        s%data = a%chars()//b%chars()
    end function concat_str_str

    pure function concat_str_chars(a, b) result(s)
        type(str_t),  intent(in) :: a
        character(*), intent(in) :: b
        type(str_t)              :: s
        s%data = a%chars()//b
    end function concat_str_chars

    pure function concat_chars_str(a, b) result(s)
        character(*), intent(in) :: a
        type(str_t),  intent(in) :: b
        type(str_t)              :: s
        s%data = a//b%chars()
    end function concat_chars_str

    ! Comparison is exact, and that takes deliberate work: Fortran's intrinsic
    ! CHARACTER comparison blank-pads the shorter operand, so "x" == "x " is
    ! .true. That is precisely the padding confusion this module exists to
    ! remove, and a tokenizer or code emitter that inherited it would silently
    ! treat distinct strings as equal. Every comparison therefore tests length
    ! first and only then content.
    pure function eq_chars_exact(a, b) result(yes)
        character(*), intent(in) :: a, b
        logical                  :: yes
        yes = len(a) == len(b)
        if (yes .and. len(a) > 0) yes = a == b
    end function eq_chars_exact

    pure function eq_str_str(a, b) result(yes)
        type(str_t), intent(in) :: a, b
        logical                 :: yes
        yes = eq_chars_exact(a%chars(), b%chars())
    end function eq_str_str

    pure function eq_str_chars(a, b) result(yes)
        type(str_t),  intent(in) :: a
        character(*), intent(in) :: b
        logical                  :: yes
        yes = eq_chars_exact(a%chars(), b)
    end function eq_str_chars

    pure function eq_chars_str(a, b) result(yes)
        character(*), intent(in) :: a
        type(str_t),  intent(in) :: b
        logical                  :: yes
        yes = eq_chars_exact(a, b%chars())
    end function eq_chars_str

    pure function ne_str_str(a, b) result(yes)
        type(str_t), intent(in) :: a, b
        logical                 :: yes
        yes = .not. eq_str_str(a, b)
    end function ne_str_str

    pure function ne_str_chars(a, b) result(yes)
        type(str_t),  intent(in) :: a
        character(*), intent(in) :: b
        logical                  :: yes
        yes = .not. eq_str_chars(a, b)
    end function ne_str_chars

    pure function ne_chars_str(a, b) result(yes)
        character(*), intent(in) :: a
        type(str_t),  intent(in) :: b
        logical                  :: yes
        yes = .not. eq_chars_str(a, b)
    end function ne_chars_str

    ! ---------------------------------------------------------- strbuf_t --

    !> Grow to hold at least `needed` characters. Capacity doubles, so appending
    !> n characters one at a time costs O(n) copies in total rather than O(n^2).
    pure subroutine buf_reserve(self, needed)
        class(strbuf_t), intent(inout) :: self
        integer,         intent(in)    :: needed
        character(:), allocatable :: bigger
        integer :: cap

        if (.not. allocated(self%data)) then
            allocate (character(len=max(MIN_CAPACITY, needed)) :: self%data)
            return
        end if

        if (needed <= len(self%data)) return

        cap = len(self%data)
        do while (cap < needed)
            cap = cap*2
        end do

        allocate (character(len=cap) :: bigger)
        bigger(1:self%length) = self%data(1:self%length)
        call move_alloc(bigger, self%data)
    end subroutine buf_reserve

    pure subroutine buf_append_chars(self, c)
        class(strbuf_t), intent(inout) :: self
        character(*),    intent(in)    :: c
        if (len(c) == 0) return
        call buf_reserve(self, self%length + len(c))
        self%data(self%length + 1:self%length + len(c)) = c
        self%length = self%length + len(c)
    end subroutine buf_append_chars

    pure subroutine buf_append_str(self, s)
        class(strbuf_t), intent(inout) :: self
        type(str_t),     intent(in)    :: s
        call buf_append_chars(self, s%chars())
    end subroutine buf_append_str

    pure subroutine buf_append_int(self, i)
        class(strbuf_t), intent(inout) :: self
        integer(int32),  intent(in)    :: i
        call buf_append_chars(self, chars(str(i)))
    end subroutine buf_append_int

    pure subroutine buf_append_real(self, x)
        class(strbuf_t), intent(inout) :: self
        real(dp),        intent(in)    :: x
        call buf_append_chars(self, chars(str(x)))
    end subroutine buf_append_real

    pure subroutine buf_newline(self)
        class(strbuf_t), intent(inout) :: self
        call buf_append_chars(self, new_line("a"))
    end subroutine buf_newline

    pure function buf_len(self) result(n)
        class(strbuf_t), intent(in) :: self
        integer                     :: n
        n = self%length
    end function buf_len

    !> The accumulated text, at exactly its own length. The buffer's spare
    !> capacity is never visible to the caller.
    pure function buf_chars(self) result(c)
        class(strbuf_t), intent(in) :: self
        character(:), allocatable   :: c
        if (self%length == 0 .or. .not. allocated(self%data)) then
            c = ""
        else
            c = self%data(1:self%length)
        end if
    end function buf_chars

    pure function buf_to_str(self) result(s)
        class(strbuf_t), intent(in) :: self
        type(str_t)                 :: s
        s = buf_chars(self)
    end function buf_to_str

    !> Drop the contents but keep the allocation, so a buffer reused across many
    !> kernels stops reallocating after the first one.
    pure subroutine buf_reset(self)
        class(strbuf_t), intent(inout) :: self
        self%length = 0
    end subroutine buf_reset

end module fortsym_string
