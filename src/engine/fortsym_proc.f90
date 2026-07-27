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
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: str_t, str, chars
    implicit none
    private

    public :: proc_available, proc_run

    !> Printed by the engine immediately before and after the answer.
    character(*), parameter, public :: MARK_BEGIN = "<<<FORTSYM"
    character(*), parameter, public :: MARK_END = "FORTSYM>>>"

    !> Distinguishes temporary files of concurrent runs.
    integer, save :: call_counter = 0

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

        character(:), allocatable :: in_path, out_path, command
        integer :: unit, ios, stat, cmdstat, limit

        limit = 30
        if (present(timeout_seconds)) limit = timeout_seconds

        n_lines = 0
        ok = .false.
        allocate (lines(0))

        call temp_paths(in_path, out_path)

        open (newunit=unit, file=in_path, status="replace", action="write", &
            iostat=ios)
        if (ios /= 0) return
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
    end subroutine proc_run

    !> Unique-enough paths for one call. The clock supplies the entropy and the
    !> counter separates calls made within one clock tick.
    subroutine temp_paths(in_path, out_path)
        character(:), allocatable, intent(out) :: in_path, out_path
        integer(int64) :: tick
        character(:), allocatable :: stem

        call system_clock(tick)
        call_counter = call_counter + 1
        stem = "/tmp/fortsym_"//chars(str(tick))//"_"//chars(str(call_counter))
        in_path = stem//".in"
        out_path = stem//".out"
    end subroutine temp_paths

    !> Collect the lines strictly between the markers.
    subroutine read_framed(path, lines, n_lines, ok)
        character(*),             intent(in)  :: path
        type(str_t), allocatable, intent(out) :: lines(:)
        integer,                  intent(out) :: n_lines
        logical,                  intent(out) :: ok

        type(str_t), allocatable :: buffer(:), bigger(:)
        character(len=4096) :: raw
        character(:), allocatable :: text
        integer :: unit, ios, cut
        logical :: inside

        n_lines = 0
        ok = .false.
        allocate (buffer(32))
        inside = .false.

        open (newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) then
            allocate (lines(0))
            return
        end if

        do
            read (unit, "(a)", iostat=ios) raw
            if (ios /= 0) exit
            text = rstrip(raw)

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

        allocate (lines(n_lines))
        if (n_lines > 0) lines(1:n_lines) = buffer(1:n_lines)
    end subroutine read_framed

    !> Trailing blanks come from the fixed-length read buffer, not from the
    !> engine, so they are removed here and nowhere else.
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

end module fortsym_proc
