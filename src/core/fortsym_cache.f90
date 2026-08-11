module fortsym_cache
    ! Small persistent caches for transformations whose results are expression
    ! node ids. The cache owns only node ids; the arena remains the owner of
    ! every expression and its lifetime. Pair results are built from the same
    ! single-result cache implementation so cache growth and invalidation do
    ! not have parallel copies.
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    public :: expr_cache_t, expr_pair_cache_t

    type :: expr_cache_t
        integer, allocatable :: value(:)
        integer(int64) :: generation = -1_int64
    contains
        procedure :: clear => cache_clear
        procedure :: lookup => cache_lookup
        procedure :: store => cache_store
    end type expr_cache_t

    type :: expr_pair_cache_t
        type(expr_cache_t) :: first
        type(expr_cache_t) :: second
    contains
        procedure :: clear => pair_cache_clear
        procedure :: lookup => pair_cache_lookup
        procedure :: store => pair_cache_store
    end type expr_pair_cache_t

contains

    subroutine cache_clear(self)
        class(expr_cache_t), intent(inout) :: self

        if (allocated(self%value)) self%value = 0
        self%generation = -1_int64
    end subroutine cache_clear

    subroutine cache_prepare(self, capacity, generation)
        class(expr_cache_t), intent(inout) :: self
        integer, intent(in) :: capacity
        integer(int64), intent(in) :: generation
        integer, allocatable :: larger(:)
        integer :: needed, old_size, new_size

        needed = max(1, capacity)
        if (self%generation /= generation) then
            if (allocated(self%value)) self%value = 0
            self%generation = generation
        end if

        if (.not. allocated(self%value)) then
            allocate (self%value(needed), source=0)
            return
        end if
        if (size(self%value) >= needed) return

        old_size = size(self%value)
        new_size = max(needed, 2*old_size)
        allocate (larger(new_size), source=0)
        larger(1:old_size) = self%value
        call move_alloc(larger, self%value)
    end subroutine cache_prepare

    logical function cache_lookup(self, id, capacity, generation, value)
        class(expr_cache_t), intent(inout) :: self
        integer, intent(in) :: id, capacity
        integer(int64), intent(in) :: generation
        integer, intent(out) :: value

        call cache_prepare(self, capacity, generation)
        value = 0
        cache_lookup = .false.
        if (id < 1 .or. id > size(self%value)) return
        if (self%value(id) <= 0) return
        value = self%value(id)
        cache_lookup = .true.
    end function cache_lookup

    subroutine cache_store(self, id, capacity, generation, value)
        class(expr_cache_t), intent(inout) :: self
        integer, intent(in) :: id, capacity, value
        integer(int64), intent(in) :: generation

        if (id < 1 .or. value < 1) return
        call cache_prepare(self, capacity, generation)
        if (id > size(self%value)) return
        self%value(id) = value
    end subroutine cache_store

    subroutine pair_cache_clear(self)
        class(expr_pair_cache_t), intent(inout) :: self

        call self%first%clear()
        call self%second%clear()
    end subroutine pair_cache_clear

    logical function pair_cache_lookup(self, id, capacity, generation, first, second)
        class(expr_pair_cache_t), intent(inout) :: self
        integer, intent(in) :: id, capacity
        integer(int64), intent(in) :: generation
        integer, intent(out) :: first, second

        first = 0
        second = 0
        pair_cache_lookup = self%first%lookup(id, capacity, generation, first)
        if (.not. pair_cache_lookup) return
        pair_cache_lookup = self%second%lookup(id, capacity, generation, second)
        if (.not. pair_cache_lookup) first = 0
    end function pair_cache_lookup

    subroutine pair_cache_store(self, id, capacity, generation, first, second)
        class(expr_pair_cache_t), intent(inout) :: self
        integer, intent(in) :: id, capacity, first, second
        integer(int64), intent(in) :: generation

        if (id < 1 .or. first < 1 .or. second < 1) return
        call self%first%store(id, capacity, generation, first)
        call self%second%store(id, capacity, generation, second)
    end subroutine pair_cache_store

end module fortsym_cache
