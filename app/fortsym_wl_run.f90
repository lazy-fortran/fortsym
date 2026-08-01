program fortsym_wl_run
    ! Run a Wolfram-language script through fortsym's supported subset.
    !
    !   fortsym-wl <script.wl>
    !
    ! Prints one line per top-level assignment:
    !
    !   R<TAB><name><TAB><value in Wolfram InputForm>
    !   T<TAB><seconds>
    !
    ! and, for anything it declined, one line on standard error:
    !
    !   UNSUPPORTED: <name>: <what stopped it>
    !
    ! The protocol is deliberately the same one fortsym-bench uses for Mathics,
    ! so the two run the same corpus file and their output is compared without
    ! either side knowing which it is.
    !
    ! Refusal exits non-zero *only* when nothing at all could be produced. A
    ! script with nineteen good results and one refusal reports the nineteen:
    ! collapsing that into a single failure would hide the coverage that exists.
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, int64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_dialect, only: dialect, DIA_WOLFRAM
    use fortsym_print, only: print_expr_in
    use fortsym_engine, only: wall_seconds
    use fortsym_wl, only: wl_session_t, wl_binding_t, wl_session_begin, &
        wl_run_source, wl_binding_count, wl_binding_at
    implicit none

    character(:), allocatable :: path, source
    type(arena_t), target :: a
    type(wl_session_t) :: session
    type(wl_binding_t) :: b
    integer :: k, n, produced, refused
    real(kind(1.0d0)) :: t0, elapsed
    character :: tab

    tab = char(9)

    if (command_argument_count() < 1) then
        write (error_unit, "(a)") "usage: fortsym-wl <script.wl>"
        error stop 2
    end if

    call read_argument(1, path)
    call read_file(path, source)

    call a%init()
    call wl_session_begin(session, a)

    t0 = wall_seconds()
    call wl_run_source(session, source)
    elapsed = wall_seconds() - t0

    n = wl_binding_count(session)
    produced = 0
    refused = 0

    do k = 1, n
        b = wl_binding_at(session, k)
        if (b%ok) then
            write (output_unit, "(a)") "R"//tab//chars(b%name)//tab// &
                chars(print_expr_in(b%value, dialect(DIA_WOLFRAM)))
            produced = produced + 1
        else
            write (error_unit, "(a)") "UNSUPPORTED: "//chars(b%name)//": "// &
                chars(b%message)
            refused = refused + 1
        end if
    end do

    write (output_unit, "(a,f0.6)") "T"//tab, elapsed

    ! Nothing usable came out. Exit non-zero so the harness records it as a
    ! refusal rather than as an empty agreement.
    if (produced == 0) then
        if (refused == 0) then
            write (error_unit, "(a)") "UNSUPPORTED: no top-level assignments"
        end if
        error stop 1
    end if

contains

    subroutine read_argument(k, value)
        integer,                   intent(in)  :: k
        character(:), allocatable, intent(out) :: value
        integer :: n
        call get_command_argument(k, length=n)
        allocate (character(n) :: value)
        call get_command_argument(k, value)
    end subroutine read_argument

    !> Read a whole file. Stream access with a size query rather than a line
    !> loop, because a corpus script has no line-length guarantee and a fixed
    !> buffer would silently truncate an expression into one that still parses.
    subroutine read_file(name, contents)
        character(*),              intent(in)  :: name
        character(:), allocatable, intent(out) :: contents
        integer :: unit, ios, bytes

        open (newunit=unit, file=name, access="stream", form="unformatted", &
              status="old", action="read", iostat=ios)
        if (ios /= 0) then
            write (error_unit, "(a)") "cannot open "//name
            error stop 2
        end if
        inquire (unit=unit, size=bytes)
        allocate (character(bytes) :: contents)
        if (bytes > 0) read (unit, iostat=ios) contents
        close (unit)
        if (ios /= 0) then
            write (error_unit, "(a)") "cannot read "//name
            error stop 2
        end if
    end subroutine read_file

end program fortsym_wl_run
