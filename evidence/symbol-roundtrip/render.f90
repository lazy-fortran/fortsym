program render_symbol_regression
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot, only: figure, barh, plot, xlabel, title, &
        xlim, ylim, set_yticks, text, savefig_with_status
    implicit none
    real(dp), parameter :: orange(3) = [.8353_dp, .3686_dp, 0._dp]
    real(dp), parameter :: blue(3) = [0._dp, .4471_dp, .6980_dp]
    real(dp), parameter :: dark(3) = [.2_dp, .2_dp, .2_dp]
    real(dp) :: before, after, reference, decay
    character(1024) :: input, output
    character(128) :: line
    character(32) :: number
    character(20) :: labels(2)
    integer :: unit, ios, status

    if (command_argument_count() /= 2) error stop 'Input CSV and output PNG required'
    call get_command_argument(1, input)
    call get_command_argument(2, output)
    open(newunit=unit, file=trim(input), status='old', action='read', iostat=ios)
    if (ios /= 0) error stop 'Cannot open input CSV'
    read(unit, '(a)', iostat=ios) line
    if (ios /= 0) error stop 'Missing CSV header'
    read(unit, *, iostat=ios) before, after, reference, decay
    close(unit)
    if (ios /= 0) error stop 'Invalid CSV values'
    if (.not. all(ieee_is_finite([before, after, reference, decay]))) &
        error stop 'Nonfinite CSV values'

    call figure(figsize=[9.4_dp, 4.7_dp])
    call title('Synthetic test: T = 22 + 40 e; supplied e = 0.75')
    call barh([2._dp], [before], height=.48_dp, color=orange, edgecolor=orange)
    call barh([1._dp], [after], height=.48_dp, color=blue, edgecolor=blue)
    call plot([reference, reference], [.55_dp, 2.6_dp], color=dark, linestyle='--')
    labels = [character(20) :: 'Before repair', 'After repair']
    call set_yticks([2._dp, 1._dp], labels)
    call xlim(0._dp, 153._dp)
    call ylim(.45_dp, 2.85_dp)
    call xlabel('Synthetic endpoint temperature (degrees Celsius)')
    write(number, '(f8.2)') before
    call text(before + 2._dp, 2._dp, trim(adjustl(number)), font_size=13._dp)
    write(number, '(f8.2)') after
    call text(after + 2._dp, 1._dp, trim(adjustl(number)), font_size=13._dp)
    call text(reference + 2._dp, 2.65_dp, 'Analytic reference: 52.00', font_size=12._dp)
    call savefig_with_status(trim(output), status)
    if (status /= 0) error stop 'PNG rendering failed'
end program render_symbol_regression
