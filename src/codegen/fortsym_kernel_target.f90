module fortsym_kernel_target
    ! Stable target identities shared by the kernel emitters.
    !
    ! The integer values are part of the evidence-manifest contract. New
    ! targets must be appended rather than renumbering existing identities.
    implicit none
    private

    public :: TARGET_DEFAULT, TARGET_FORTRAN_CPU
    public :: TARGET_FORTRAN_OPENMP_TARGET, TARGET_FORTRAN_OPENACC
    public :: TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC, TARGET_CUDA
    public :: target_directives, target_is_valid, target_name

    !> Preserve the pre-descriptor API: its legacy logical flags decide the
    !> decorations. For the IR emitter, this means an undecorated CPU leaf.
    integer, parameter :: TARGET_DEFAULT = -1
    !> Host Fortran with no device directive.
    integer, parameter :: TARGET_FORTRAN_CPU = 0
    !> Fortran leaf callable from an OpenMP target region.
    integer, parameter :: TARGET_FORTRAN_OPENMP_TARGET = 1
    !> Fortran leaf callable from an OpenACC device routine.
    integer, parameter :: TARGET_FORTRAN_OPENACC = 2
    !> Compatibility spelling for the historical dual-annotated leaf.
    integer, parameter :: TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC = 3
    !> CUDA device leaf; decoration is emitted by the CUDA backend itself.
    integer, parameter :: TARGET_CUDA = 4

contains

    pure function target_is_valid(target) result(valid)
        integer, intent(in) :: target
        logical :: valid

        select case (target)
        case (TARGET_DEFAULT, TARGET_FORTRAN_CPU, &
                TARGET_FORTRAN_OPENMP_TARGET, TARGET_FORTRAN_OPENACC, &
                TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC, TARGET_CUDA)
            valid = .true.
        case default
            valid = .false.
        end select
    end function target_is_valid

    pure function target_name(target) result(name)
        integer, intent(in) :: target
        character(:), allocatable :: name

        select case (target)
        case (TARGET_DEFAULT)
            name = "default"
        case (TARGET_FORTRAN_CPU)
            name = "fortran_cpu"
        case (TARGET_FORTRAN_OPENMP_TARGET)
            name = "fortran_openmp_target"
        case (TARGET_FORTRAN_OPENACC)
            name = "fortran_openacc"
        case (TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC)
            name = "fortran_openmp_target+fortran_openacc"
        case (TARGET_CUDA)
            name = "cuda"
        case default
            name = "invalid"
        end select
    end function target_name

    !> Resolve target selection into source directives. TARGET_DEFAULT uses the
    !> legacy flags so committed consumers remain byte-identical; explicit
    !> targets override those flags completely.
    pure subroutine target_directives(target, legacy_openmp, legacy_openacc, &
            emit_openmp, emit_openacc)
        integer, intent(in) :: target
        logical, intent(in) :: legacy_openmp, legacy_openacc
        logical, intent(out) :: emit_openmp, emit_openacc

        emit_openmp = .false.
        emit_openacc = .false.
        select case (target)
        case (TARGET_DEFAULT)
            emit_openmp = legacy_openmp
            emit_openacc = legacy_openacc
        case (TARGET_FORTRAN_OPENMP_TARGET)
            emit_openmp = .true.
        case (TARGET_FORTRAN_OPENACC)
            emit_openacc = .true.
        case (TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC)
            emit_openmp = .true.
            emit_openacc = .true.
        case (TARGET_FORTRAN_CPU, TARGET_CUDA)
            continue
        end select
    end subroutine target_directives

end module fortsym_kernel_target
