program test_consumer_audit_driver
    implicit none

    integer :: command_status
    integer :: exit_status
    character(len=256) :: command_message

    call execute_command_line( &
        "python3 test/audit/test_consumer_audit.py", &
        wait=.true., &
        exitstat=exit_status, &
        cmdstat=command_status, &
        cmdmsg=command_message)

    if (command_status /= 0) then
        write (*, '(a)') trim(command_message)
        error stop "could not run consumer audit tests"
    end if
    if (exit_status /= 0) then
        error stop "consumer audit tests failed"
    end if

    write (*, '(a)') "consumer audit behavioral tests: OK"
end program test_consumer_audit_driver
