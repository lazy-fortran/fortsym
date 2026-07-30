module fortsym_proc
    ! Running an external computer algebra system and reading its answer.
    !
    ! Tier 2 engines are reached as separate programs. That is a licence
    ! boundary before it is an engineering one: a process boundary with
    ! arms-length data keeps GPL engines out of fortsym's MIT link closure
    ! (LEGAL.md section 2). It also means a crashing or hanging engine cannot
    ! take the build down with it.
    !
    ! Transport is a temporary file, a shell redirection and a second temporary
    ! file, rather than fork/exec with pipes. A pipe implementation would need a
    ! C shim and careful handling of partial reads and SIGPIPE, to save a few
    ! milliseconds on calls that already cost tens of milliseconds because an
    ! interpreter has to start. The simple mechanism is the right one here.
    !
    ! Answers are framed by markers the engine itself prints. Interpreters emit
    ! banners, load messages and prompts that vary between versions -- Maxima on
    ! ECL announces which FASL files it loaded -- so scraping "the last line" is
    ! not reliable. Everything outside the markers is ignored.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_size_t
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortsym_string, only: str_t, strbuf_t, str, chars
    implicit none
    private

    public :: proc_available, proc_run

    !> Printed by the engine immediately before and after the answer.
    character(*), parameter, public :: MARK_BEGIN = "<<<FORTSYM"
    character(*), parameter, public :: MARK_END = "FORTSYM>>>"
    integer, parameter :: MAX_REPLY_LINE_BYTES = 16*1024*1024

    interface
        function fsym_make_temp_directory(path, size) bind(C) result(ok)
            import :: c_char, c_int, c_size_t
            character(c_char), intent(out) :: path(*)
            integer(c_size_t), value       :: size
            integer(c_int)                 :: ok
        end function fsym_make_temp_directory

        function fsym_remove_temp_directory(path) bind(C) result(ok)
            import :: c_char, c_int
            character(c_char), intent(in) :: path(*)
            integer(c_int)                :: ok
        end function fsym_remove_temp_directory
    end interface

contains

    !> Is this program on PATH?
    !>
    !> A missing engine is an ordinary condition, not an error: fortsym works
    !> with whatever is installed and records the rest as unavailable.
    function proc_available(program) result(yes)
        character(*), intent(in) :: program
        logical                  :: yes
        integer :: stat, cmdstat
        character(:), allocatable :: probe

        probe = "command -v "//program//" >/dev/null 2>&1"
        call execute_command_line(probe, wait=.true., exitstat=stat, &
            cmdstat=cmdstat)
        yes = cmdstat == 0 .and. stat == 0
    end function proc_available

    !> Feed `input` to `program` on standard input and return the lines the
    !> engine printed between the markers.
    !>
    !> ok is false when the program could not be run or printed no framed
    !> answer. The caller treats that as "this engine declined", never as a
    !> reason to stop.
    subroutine proc_run(program, input, lines, n_lines, ok, timeout_seconds)
        character(*),              intent(in)  :: program, input
        type(str_t), allocatable,  intent(out) :: lines(:)
        integer,                   intent(out) :: n_lines
        logical,                   intent(out) :: ok
        integer, optional,         intent(in)  :: timeout_seconds

        character(:), allocatable :: directory, in_path, out_path, command
        integer :: unit, ios, stat, cmdstat, limit
        logical :: made

        limit = 30
        if (present(timeout_seconds)) limit = timeout_seconds

        n_lines = 0
        ok = .false.
        allocate (lines(0))

        call temp_paths(directory, in_path, out_path, made)
        if (.not. made) return

        open (newunit=unit, file=in_path, status="replace", action="write", &
            iostat=ios)
        if (ios /= 0) then
            call remove_directory(directory)
            return
        end if
        write (unit, "(a)") input
        close (unit)

        ! timeout bounds a hung engine. Without it a backend that waits for
        ! input it will never get would stall the whole test run.
        command = "timeout "//chars(str(limit))//" "//program// &
            " < "//in_path//" > "//out_path//" 2>&1"
        call execute_command_line(command, wait=.true., exitstat=stat, &
            cmdstat=cmdstat)

        call read_framed(out_path, lines, n_lines, ok)

        call remove_file(in_path)
        call remove_file(out_path)
        call remove_directory(directory)
    end subroutine proc_run

    !> A private directory created atomically prevents another user from
    !> replacing either redirection endpoint with a symlink between creation
    !> and execution.
    subroutine temp_paths(directory, in_path, out_path, ok)
        character(:), allocatable, intent(out) :: directory, in_path, out_path
        logical,                   intent(out) :: ok
        integer, parameter :: BUFFER_SIZE = 64
        character(c_char) :: buffer(BUFFER_SIZE)
        integer :: k

        buffer = c_null_char
        ok = fsym_make_temp_directory(buffer, int(BUFFER_SIZE, c_size_t)) /= 0
        if (.not. ok) return

        directory = ""
        do k = 1, BUFFER_SIZE
            if (buffer(k) == c_null_char) exit
            directory = directory//achar(iachar(buffer(k)))
        end do
        in_path = directory//"/input"
        out_path = directory//"/output"
    end subroutine temp_paths

    !> Collect the lines strictly between the markers.
    subroutine read_framed(path, lines, n_lines, ok)
        character(*),             intent(in)  :: path
        type(str_t), allocatable, intent(out) :: lines(:)
        integer,                  intent(out) :: n_lines
        logical,                  intent(out) :: ok

        type(str_t), allocatable :: buffer(:), bigger(:)
        character(:), allocatable :: text
        integer :: unit, ios, cut
        logical :: inside, eof, too_long

        n_lines = 0
        ok = .false.
        allocate (buffer(32))
        inside = .false.
        too_long = .false.

        open (newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) then
            allocate (lines(0))
            return
        end if

        do
            call read_record(unit, text, eof, too_long)
            if (eof .or. too_long) exit
            text = rstrip(text)

            if (index(text, MARK_END) > 0) then
                ok = .true.
                exit
            end if

            if (inside) then
                if (len(text) > 0) then
                    if (n_lines >= size(buffer)) then
                        allocate (bigger(size(buffer)*2))
                        bigger(1:n_lines) = buffer(1:n_lines)
                        call move_alloc(bigger, buffer)
                    end if
                    n_lines = n_lines + 1
                    buffer(n_lines) = str(text)
                end if
                cycle
            end if

            cut = index(text, MARK_BEGIN)
            if (cut > 0) inside = .true.
        end do

        close (unit)
        if (too_long) ok = .false.

        allocate (lines(n_lines))
        if (n_lines > 0) lines(1:n_lines) = buffer(1:n_lines)
    end subroutine read_framed

    !> Read one complete formatted record without truncating it. A bounded
    !> refusal is safer than returning a valid-looking prefix of a large exact
    !> integer or expression.
    subroutine read_record(unit, text, eof, too_long)
        integer,                   intent(in)  :: unit
        character(:), allocatable, intent(out) :: text
        logical,                   intent(out) :: eof, too_long
        character(len=4096) :: chunk
        type(strbuf_t) :: b
        integer :: ios, n

        eof = .false.
        too_long = .false.
        do
            n = 0
            read (unit, "(a)", advance="no", size=n, iostat=ios) chunk
            if (n > 0) then
                if (b%len() > MAX_REPLY_LINE_BYTES - n) then
                    too_long = .true.
                    text = ""
                    return
                end if
                call b%append(chunk(1:n))
            end if

            if (ios == iostat_eor) then
                text = b%chars()
                return
            else if (ios == iostat_end) then
                text = b%chars()
                eof = b%len() == 0
                return
            else if (ios /= 0) then
                text = ""
                eof = .true.
                return
            end if
        end do
    end subroutine read_record

    !> CAS printers may leave insignificant trailing blanks; remove them here
    !> and nowhere else.
    pure function rstrip(raw) result(text)
        character(*), intent(in)  :: raw
        character(:), allocatable :: text
        integer :: n
        n = len(raw)
        do while (n > 0)
            if (raw(n:n) /= " ") exit
            n = n - 1
        end do
        text = raw(1:n)
    end function rstrip

    subroutine remove_file(path)
        character(*), intent(in) :: path
        integer :: unit, ios
        open (newunit=unit, file=path, status="old", iostat=ios)
        if (ios == 0) close (unit, status="delete")
    end subroutine remove_file

    subroutine remove_directory(path)
        character(*), intent(in) :: path
        character(c_char), allocatable :: c_path(:)
        integer :: k, ignored

        allocate (c_path(len(path) + 1))
        do k = 1, len(path)
            c_path(k) = path(k:k)
        end do
        c_path(len(path) + 1) = c_null_char
        ignored = fsym_remove_temp_directory(c_path)
    end subroutine remove_directory

end module fortsym_proc
