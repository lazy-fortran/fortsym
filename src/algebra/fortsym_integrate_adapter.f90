module fortsym_integrate_adapter
    ! Binding-safe subroutine boundary for the verified indefinite integrator.
    ! Keep this adapter outside the public C-ABI module: the latter exports a
    ! symbol named `fortsym_integrate`, which must not participate in Fortran
    ! name resolution for the native owner call.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    use fortsym_integrate, only: integrate
    implicit none
    private

    public :: verified_antiderivative

contains

    subroutine verified_antiderivative(a, e, var, f, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e, var
        type(expr_t),              intent(out)   :: f
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why

        f = integrate(a, e, var, ok, why, real_domain=.false.)
    end subroutine verified_antiderivative

end module fortsym_integrate_adapter
