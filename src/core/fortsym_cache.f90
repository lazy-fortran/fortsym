module fortsym_cache
    ! Small persistent caches for transformations whose results are expression
    ! node pairs. The cache owns only node ids; the arena remains the owner of
    ! every expression and its lifetime.
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    public :: expr_pair_cache_t

    type :: expr_pair_cache_t
        integer, allocatable :: first(:)
        integer, allocatable :: second(:)
        integer(int64) :: generation = -1_int64
    contains
        procedure :: clear => pair_cache_clear
        procedure :: lookup => pair_cache_lookup
        procedure :: store => pair_cache_store
    end type expr_pair_cache_t

contains

    subroutine pair_cache_clear(self)
        class(expr_pair_cache_t), intent(inout) :: self

        if (allocated(self%first)) self%first = 0
        if (allocated(self%second)) self%second = 0
        self%generation = -1_int64
    end subroutine pair_cache_clear

    subroutine pair_cache_prepare(self, capacity, generation)
        class(expr_pair_cache_t), intent(inout) :: self
        integer, intent(in) :: capacity
        integer(int64), intent(in) :: generation
        integer, allocatable :: larger_first(:), larger_second(:)
        integer :: needed, old_size, new_size

        needed = max(1, capacity)
        if (self%generation /= generation) then
            if (allocated(self%first)) self%first = 0
            if (allocated(self%second)) self%second = 0
            self%generation = generation
        end if

        if (.not. allocated(self%first)) then
            allocate (self%first(needed), source=0)
            allocate (self%second(needed), source=0)
            return
        end if
        if (size(self%first) >= needed) return

        old_size = size(self%first)
        new_size = max(needed, 2*old_size)
        allocate (larger_first(new_size), source=0)
        allocate (larger_second(new_size), source=0)
        larger_first(1:old_size) = self%first
        larger_second(1:old_size) = self%second
        call move_alloc(larger_first, self%first)
        call move_alloc(larger_second, self%second)
    end subroutine pair_cache_prepare

    logical function pair_cache_lookup(self, id, capacity, generation, first, second)
        class(expr_pair_cache_t), intent(inout) :: self
        integer, intent(in) :: id, capacity
        integer(int64), intent(in) :: generation
        integer, intent(out) :: first, second

        call pair_cache_prepare(self, capacity, generation)
        first = 0
        second = 0
        pair_cache_lookup = .false.
        if (id < 1 .or. id > size(self%first)) return
        if (self%first(id) <= 0 .or. self%second(id) <= 0) return
        first = self%first(id)
        second = self%second(id)
        pair_cache_lookup = .true.
    end function pair_cache_lookup

    subroutine pair_cache_store(self, id, capacity, generation, first, second)
        class(expr_pair_cache_t), intent(inout) :: self
        integer, intent(in) :: id, capacity, first, second
        integer(int64), intent(in) :: generation

        if (id < 1 .or. first < 1 .or. second < 1) return
        call pair_cache_prepare(self, capacity, generation)
        if (id > size(self%first)) return
        self%first(id) = first
        self%second(id) = second
    end subroutine pair_cache_store

end module fortsym_cache
