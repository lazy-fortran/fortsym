program fortsym_wl_to_f90
    ! Translate a bounded Wolfram assignment stream into a Fortran subroutine.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortsym_string, only: str_t, chars
    use fortsym_wl_f90, only: translate_wl_assignments
    implicit none

    character(:), allocatable :: input_path, output_path, source, message
    type(str_t) :: code
    logical :: ok
    integer :: unit, ios

    if (command_argument_count() /= 2) then
        write (error_unit, "(a)") &
            "usage: fortsym_wl_to_f90 input.wl output.f90"
        error stop 2
    end if
    call read_argument(1, input_path)
    call read_argument(2, output_path)
    call read_file(input_path, source)

    code = translate_wl_assignments(source, ok, message)
    if (.not. ok) then
        write (error_unit, "(a)") "translation refused: "//message
        error stop 1
    end if

    open (newunit=unit, file=output_path, status="replace", action="write", &
        iostat=ios)
    if (ios /= 0) then
        write (error_unit, "(a)") "cannot open output "//output_path
        error stop 2
    end if
    write (unit, "(a)") chars(code)
    close (unit)

contains

    subroutine read_argument(k, value)
        integer,                   intent(in)  :: k
        character(:), allocatable, intent(out) :: value
        integer :: n
        call get_command_argument(k, length=n)
        allocate (character(n) :: value)
        call get_command_argument(k, value)
    end subroutine read_argument

    subroutine read_file(name, contents)
        character(*),              intent(in)  :: name
        character(:), allocatable, intent(out) :: contents
        integer :: unit, ios, bytes

        open (newunit=unit, file=name, access="stream", form="unformatted", &
            status="old", action="read", iostat=ios)
        if (ios /= 0) then
            write (error_unit, "(a)") "cannot open input "//name
            error stop 2
        end if
        inquire (unit=unit, size=bytes)
        allocate (character(bytes) :: contents)
        if (bytes > 0) read (unit, iostat=ios) contents
        close (unit)
        if (ios /= 0) then
            write (error_unit, "(a)") "cannot read input "//name
            error stop 2
        end if
    end subroutine read_file

end program fortsym_wl_to_f90
