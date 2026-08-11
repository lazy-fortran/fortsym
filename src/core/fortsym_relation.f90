module fortsym_relation
    ! Symbolic equality and ordering constructors.
    !
    ! Relation heads stay private node vocabulary. The public Fortran names
    ! remain short and snake_case; the Python compatibility layer maps SymPy's
    ! Eq/Ne/Gt/Ge/Lt/Le names onto these same nodes.
    use fortsym_expr, only: expr_t
    implicit none
    private

    public :: equal, unequal, less, less_equal, greater, greater_equal

contains

    function equal(left, right) result(relation)
        type(expr_t), intent(in) :: left, right
        type(expr_t) :: relation
        relation = make_relation("Equal", left, right)
    end function equal

    function unequal(left, right) result(relation)
        type(expr_t), intent(in) :: left, right
        type(expr_t) :: relation
        relation = make_relation("Unequal", left, right)
    end function unequal

    function less(left, right) result(relation)
        type(expr_t), intent(in) :: left, right
        type(expr_t) :: relation
        relation = make_relation("Less", left, right)
    end function less

    function less_equal(left, right) result(relation)
        type(expr_t), intent(in) :: left, right
        type(expr_t) :: relation
        relation = make_relation("LessEqual", left, right)
    end function less_equal

    function greater(left, right) result(relation)
        type(expr_t), intent(in) :: left, right
        type(expr_t) :: relation
        relation = make_relation("Greater", left, right)
    end function greater

    function greater_equal(left, right) result(relation)
        type(expr_t), intent(in) :: left, right
        type(expr_t) :: relation
        relation = make_relation("GreaterEqual", left, right)
    end function greater_equal

    function make_relation(name, left, right) result(relation)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: left, right
        type(expr_t) :: relation
        integer :: ids(2)

        ids(1) = left%id
        ids(2) = right%id
        relation%a => left%a
        relation%id = left%a%func(name, ids)
        relation%generation = left%generation
    end function make_relation

end module fortsym_relation
