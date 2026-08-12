module fortsym_registry
    ! Explicit capability registry for optional geometry and physics toolkits.
    !
    ! Registration is ordinary Fortran data and procedure-pointer assignment.
    ! There is no linker discovery, module initialization side effect, or
    ! dependency from the expression arena to any toolkit owner.
    implicit none
    private

    integer, parameter, public :: REGISTRY_MAX_TOOLKITS = 32
    integer, parameter, public :: REGISTRY_NAME_LENGTH = 64
    integer, parameter, public :: REGISTRY_VERSION_LENGTH = 24
    integer, parameter, public :: REGISTRY_OWNER_LENGTH = 64
    integer, parameter, public :: REGISTRY_DESCRIPTION_LENGTH = 160
    integer, parameter, public :: REGISTRY_MESSAGE_LENGTH = 256

    abstract interface
        subroutine toolkit_probe(ok, message)
            import :: REGISTRY_MESSAGE_LENGTH
            logical, intent(out) :: ok
            character(len=REGISTRY_MESSAGE_LENGTH), intent(out) :: message
        end subroutine toolkit_probe
    end interface

    type, public :: toolkit_record_t
        character(len=REGISTRY_NAME_LENGTH) :: name = ""
        character(len=REGISTRY_VERSION_LENGTH) :: version = ""
        character(len=REGISTRY_OWNER_LENGTH) :: owner = ""
        character(len=REGISTRY_DESCRIPTION_LENGTH) :: capability = ""
        character(len=REGISTRY_DESCRIPTION_LENGTH) :: refusal = ""
        logical :: enabled = .true.
        procedure(toolkit_probe), pointer, nopass :: probe => null()
    end type toolkit_record_t

    type, public :: toolkit_registry_t
        private
        type(toolkit_record_t) :: entry(REGISTRY_MAX_TOOLKITS)
        integer :: count = 0
    end type toolkit_registry_t

    public :: toolkit_probe
    public :: registry_init, registry_register, registry_find, registry_count
    public :: registry_name, registry_version, registry_owner
    public :: registry_capability, registry_refusal, registry_enabled
    public :: registry_probe, register_builtin_geometry

contains

    !> Reset an explicit registry. This is the only initialization required.
    subroutine registry_init(registry)
        type(toolkit_registry_t), intent(inout) :: registry
        integer :: k

        registry%count = 0
        do k = 1, REGISTRY_MAX_TOOLKITS
            registry%entry(k)%name = ""
            registry%entry(k)%version = ""
            registry%entry(k)%owner = ""
            registry%entry(k)%capability = ""
            registry%entry(k)%refusal = ""
            registry%entry(k)%enabled = .true.
            nullify(registry%entry(k)%probe)
        end do
    end subroutine registry_init

    !> Register one named capability. Duplicate names and malformed records are
    !> refused so a later optional package cannot silently replace an owner.
    subroutine registry_register(registry, name, version, owner, capability, &
            refusal, accepted, message, probe)
        type(toolkit_registry_t), intent(inout) :: registry
        character(len=*), intent(in) :: name, version, owner, capability, refusal
        logical, intent(out) :: accepted
        character(len=REGISTRY_MESSAGE_LENGTH), intent(out) :: message
        procedure(toolkit_probe), pointer, optional :: probe
        integer :: slot

        accepted = .false.
        message = ""
        if (.not. valid_text(name, REGISTRY_NAME_LENGTH)) then
            message = "registry: toolkit name is empty or too long"
            return
        end if
        if (.not. valid_text(version, REGISTRY_VERSION_LENGTH)) then
            message = "registry: toolkit version is empty or too long"
            return
        end if
        if (.not. valid_text(owner, REGISTRY_OWNER_LENGTH)) then
            message = "registry: toolkit owner is empty or too long"
            return
        end if
        if (.not. valid_text(capability, REGISTRY_DESCRIPTION_LENGTH)) then
            message = "registry: toolkit capability is empty or too long"
            return
        end if
        if (.not. valid_text(refusal, REGISTRY_DESCRIPTION_LENGTH)) then
            message = "registry: toolkit refusal boundary is empty or too long"
            return
        end if
        if (registry%count >= REGISTRY_MAX_TOOLKITS) then
            message = "registry: toolkit capacity exhausted"
            return
        end if
        slot = registry_find(registry, name)
        if (slot /= 0) then
            message = "registry: toolkit name is already registered"
            return
        end if

        slot = registry%count + 1
        call copy_text(registry%entry(slot)%name, name)
        call copy_text(registry%entry(slot)%version, version)
        call copy_text(registry%entry(slot)%owner, owner)
        call copy_text(registry%entry(slot)%capability, capability)
        call copy_text(registry%entry(slot)%refusal, refusal)
        registry%entry(slot)%enabled = .true.
        nullify(registry%entry(slot)%probe)
        if (present(probe)) registry%entry(slot)%probe => probe
        registry%count = slot
        accepted = .true.
        message = "registered"
    end subroutine registry_register

    pure function registry_find(registry, name) result(slot)
        type(toolkit_registry_t), intent(in) :: registry
        character(len=*), intent(in) :: name
        integer :: slot

        slot = 0
        if (len_trim(name) == 0) return
        do slot = 1, registry%count
            if (trim(registry%entry(slot)%name) == trim(name)) return
        end do
        slot = 0
    end function registry_find

    pure function registry_count(registry) result(count)
        type(toolkit_registry_t), intent(in) :: registry
        integer :: count

        count = registry%count
    end function registry_count

    pure function registry_name(registry, slot) result(value)
        type(toolkit_registry_t), intent(in) :: registry
        integer, intent(in) :: slot
        character(len=REGISTRY_NAME_LENGTH) :: value

        value = ""
        if (.not. valid_slot(registry, slot)) return
        value = registry%entry(slot)%name
    end function registry_name

    pure function registry_version(registry, slot) result(value)
        type(toolkit_registry_t), intent(in) :: registry
        integer, intent(in) :: slot
        character(len=REGISTRY_VERSION_LENGTH) :: value

        value = ""
        if (.not. valid_slot(registry, slot)) return
        value = registry%entry(slot)%version
    end function registry_version

    pure function registry_owner(registry, slot) result(value)
        type(toolkit_registry_t), intent(in) :: registry
        integer, intent(in) :: slot
        character(len=REGISTRY_OWNER_LENGTH) :: value

        value = ""
        if (.not. valid_slot(registry, slot)) return
        value = registry%entry(slot)%owner
    end function registry_owner

    pure function registry_capability(registry, slot) result(value)
        type(toolkit_registry_t), intent(in) :: registry
        integer, intent(in) :: slot
        character(len=REGISTRY_DESCRIPTION_LENGTH) :: value

        value = ""
        if (.not. valid_slot(registry, slot)) return
        value = registry%entry(slot)%capability
    end function registry_capability

    pure function registry_refusal(registry, slot) result(value)
        type(toolkit_registry_t), intent(in) :: registry
        integer, intent(in) :: slot
        character(len=REGISTRY_DESCRIPTION_LENGTH) :: value

        value = ""
        if (.not. valid_slot(registry, slot)) return
        value = registry%entry(slot)%refusal
    end function registry_refusal

    pure function registry_enabled(registry, slot) result(enabled)
        type(toolkit_registry_t), intent(in) :: registry
        integer, intent(in) :: slot
        logical :: enabled

        enabled = .false.
        if (.not. valid_slot(registry, slot)) return
        enabled = registry%entry(slot)%enabled
    end function registry_enabled

    !> Run the optional probe associated with one toolkit, if any.
    subroutine registry_probe(registry, name, available, message)
        type(toolkit_registry_t), intent(in) :: registry
        character(len=*), intent(in) :: name
        logical, intent(out) :: available
        character(len=REGISTRY_MESSAGE_LENGTH), intent(out) :: message
        integer :: slot

        available = .false.
        message = ""
        slot = registry_find(registry, name)
        if (slot == 0) then
            message = "registry: toolkit is not registered"
            return
        end if
        if (.not. registry%entry(slot)%enabled) then
            message = "registry: toolkit is disabled"
            return
        end if
        if (associated(registry%entry(slot)%probe)) then
            call registry%entry(slot)%probe(available, message)
        else
            available = .true.
            message = "available"
        end if
    end subroutine registry_probe

    !> Explicitly register the geometry owners shipped by this build. This is
    !> deliberately a call, not hidden module initialization, so applications
    !> can choose a smaller registry or add private toolkits beside these.
    subroutine register_builtin_geometry(registry, accepted, message)
        type(toolkit_registry_t), intent(inout) :: registry
        logical, intent(out) :: accepted
        character(len=REGISTRY_MESSAGE_LENGTH), intent(out) :: message
        logical :: one_accepted

        accepted = .false.
        message = ""
        call register_one(registry, "chart", "0.1", "fortsym_chart", &
            "coordinate charts, bases, Jacobians, and vector calculus", &
            "requires a valid fixed-three-dimensional chart", one_accepted, message)
        if (.not. one_accepted) return
        call register_one(registry, "metric", "0.1", "fortsym_metric", &
            "covariant metrics, inverse metrics, volume, and signatures", &
            "requires a nondegenerate metric and matching arena", one_accepted, message)
        if (.not. one_accepted) return
        call register_one(registry, "tensor", "0.1", "fortsym_tensor", &
            "typed indexed components, variance, density, and contractions", &
            "supports rank zero through five in one chart arena", one_accepted, message)
        if (.not. one_accepted) return
        call register_one(registry, "form", "0.1", "fortsym_form", &
            "coordinate exterior forms, wedge, d, star, and Lie calculus", &
            "fixed-three-dimensional forms refuse invalid degree or arena", one_accepted, message)
        if (.not. one_accepted) return
        call register_one(registry, "connection", "0.1", "fortsym_connection", &
            "covariant derivatives, geodesics, and curvature tensors", &
            "requires coordinates for metric-induced connections", one_accepted, message)
        if (.not. one_accepted) return
        call register_one(registry, "relativity", "0.1", "fortsym_relativity", &
            "dimension-aware pseudo-Riemannian metrics through four dimensions", &
            "requires a valid signature and nondegenerate metric", one_accepted, message)
        if (.not. one_accepted) return
        call register_one(registry, "magnetic", "0.1", "fortsym_magnetic", &
            "flux-coordinate fields, densities, and Fourier magnetic views", &
            "requires chart-owned field components and compatible coordinates", one_accepted, message)
        if (.not. one_accepted) return
        call register_one(registry, "flux", "0.1", "fortsym_flux", &
            "Clebsch, straight-field-line, Boozer, and Hamada descriptors", &
            "residual identities refuse unsupported coordinate assumptions", one_accepted, message)
        if (.not. one_accepted) return
        call register_one(registry, "maxwell", "0.1", "fortsym_maxwell", &
            "form-level field strength and gauge residuals", &
            "requires compatible metric-free form degrees", one_accepted, message)
        if (.not. one_accepted) return
        call register_one(registry, "fourier_fem", "0.1", "fortsym_magnetic_weak", &
            "Albert-Biro-Lainer mode branches and weak-form metadata", &
            "n=0 and n/=0 branches retain explicit refusal metadata", one_accepted, message)
        if (.not. one_accepted) return
        accepted = .true.
        message = "built-in geometry toolkits registered"
    end subroutine register_builtin_geometry

    subroutine register_one(registry, name, version, owner, capability, refusal, &
            accepted, message)
        type(toolkit_registry_t), intent(inout) :: registry
        character(len=*), intent(in) :: name, version, owner, capability, refusal
        logical, intent(out) :: accepted
        character(len=REGISTRY_MESSAGE_LENGTH), intent(out) :: message

        call registry_register(registry, name, version, owner, capability, &
            refusal, accepted, message)
    end subroutine register_one

    pure function valid_slot(registry, slot) result(valid)
        type(toolkit_registry_t), intent(in) :: registry
        integer, intent(in) :: slot
        logical :: valid

        valid = slot >= 1 .and. slot <= registry%count
    end function valid_slot

    pure function valid_text(value, capacity) result(valid)
        character(len=*), intent(in) :: value
        integer, intent(in) :: capacity
        logical :: valid

        valid = len_trim(value) > 0
        if (.not. valid) return
        valid = len_trim(value) <= capacity
    end function valid_text

    subroutine copy_text(target, source)
        character(len=*), intent(out) :: target
        character(len=*), intent(in) :: source
        integer :: length

        target = ""
        length = len_trim(source)
        if (length > 0) target(1:length) = source(1:length)
    end subroutine copy_text

end module fortsym_registry
