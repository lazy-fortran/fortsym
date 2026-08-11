module fortsym_latex
    ! A small, write-only LaTeX artifact writer for expression results.
    use fortsym_expr, only: expr_t
    use fortsym_print, only: print_expr_latex
    use fortsym_string, only: str_t, str, chars, operator(//)
    implicit none
    private

    public :: latex_t

    type :: latex_equation_t
        type(str_t) :: label
        type(str_t) :: macro
        type(str_t) :: body
        type(str_t) :: assumption
    end type latex_equation_t

    type :: latex_t
        private
        type(str_t), allocatable :: symbol_names(:)
        type(str_t), allocatable :: symbol_values(:)
        type(latex_equation_t), allocatable :: equations(:)
        integer :: n_equations = 0
        type(str_t) :: derivation
    contains
        procedure, public :: name => latex_name
        procedure, public :: eq => latex_equation
        procedure, public :: relation => latex_relation
        procedure, public :: source => latex_source
        procedure, public :: write => latex_write
        procedure, public :: clear => latex_clear
    end type latex_t

contains

    subroutine latex_name(self, fortsym_name, latex_value, ok, message)
        class(latex_t), intent(inout) :: self
        character(*), intent(in) :: fortsym_name, latex_value
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: message
        integer :: k

        if (present(ok)) ok = .false.
        if (present(message)) message = ""
        call ensure_name_storage(self)
        do k = 1, size(self%symbol_names)
            if (chars(self%symbol_names(k)) /= fortsym_name) cycle
            if (chars(self%symbol_values(k)) == latex_value) then
                if (present(ok)) ok = .true.
                return
            end if
            if (present(message)) message = &
                "symbol notation already registered for "//fortsym_name
            return
        end do

        call append_name(self, str(fortsym_name), str(latex_value))
        if (present(ok)) ok = .true.
    end subroutine latex_name

    subroutine latex_equation(self, label, expression, assumption, ok, message)
        class(latex_t), intent(inout) :: self
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: expression
        character(*), intent(in), optional :: assumption
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: message
        type(str_t) :: body
        logical :: rendered
        character(:), allocatable :: detail, macro
        integer :: k

        if (present(ok)) ok = .false.
        if (present(message)) message = ""
        if (len(label) == 0) then
            if (present(message)) message = "equation label is empty"
            return
        end if

        macro = latex_macro_name(label)
        do k = 1, self%n_equations
            if (chars(self%equations(k)%label) == label) then
                if (present(message)) message = &
                    "equation label already registered: "//label
                return
            end if
            if (chars(self%equations(k)%macro) == macro) then
                if (present(message)) message = "equation labels collide: "//label// &
                    " and "//chars(self%equations(k)%label)
                return
            end if
        end do

        call ensure_name_storage(self)
        body = print_expr_latex(expression, self%symbol_names, self%symbol_values, &
            rendered, detail)
        if (.not. rendered) then
            if (present(message)) message = detail
            return
        end if

        call append_equation(self, str(label), str(macro), body, &
            optional_string(assumption))
        if (present(ok)) ok = .true.
    end subroutine latex_equation

    subroutine latex_relation(self, label, left, right, assumption, ok, message)
        class(latex_t), intent(inout) :: self
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: left, right
        character(*), intent(in), optional :: assumption
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: message
        type(str_t) :: left_body, right_body
        type(str_t) :: body
        logical :: rendered
        character(:), allocatable :: detail, macro
        integer :: k

        if (present(ok)) ok = .false.
        if (present(message)) message = ""
        if (len(label) == 0) then
            if (present(message)) message = "equation label is empty"
            return
        end if

        macro = latex_macro_name(label)
        do k = 1, self%n_equations
            if (chars(self%equations(k)%label) == label) then
                if (present(message)) message = &
                    "equation label already registered: "//label
                return
            end if
            if (chars(self%equations(k)%macro) == macro) then
                if (present(message)) message = "equation labels collide: "//label// &
                    " and "//chars(self%equations(k)%label)
                return
            end if
        end do

        call ensure_name_storage(self)
        left_body = print_expr_latex(left, self%symbol_names, self%symbol_values, &
            rendered, detail)
        if (.not. rendered) then
            if (present(message)) message = detail
            return
        end if
        right_body = print_expr_latex(right, self%symbol_names, self%symbol_values, &
            rendered, detail)
        if (.not. rendered) then
            if (present(message)) message = detail
            return
        end if

        body = str(chars(left_body)//" = "//chars(right_body))
        call append_equation(self, str(label), str(macro), body, &
            optional_string(assumption))
        if (present(ok)) ok = .true.
    end subroutine latex_relation

    subroutine latex_source(self, derivation)
        class(latex_t), intent(inout) :: self
        character(*), intent(in) :: derivation

        self%derivation = str(derivation)
    end subroutine latex_source

    subroutine latex_write(self, path, ok, message)
        class(latex_t), intent(in) :: self
        character(*), intent(in) :: path
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: message
        integer :: unit, ios, k
        character(:), allocatable :: derivation

        if (present(ok)) ok = .false.
        if (present(message)) message = ""
        open (newunit=unit, file=path, status="replace", action="write", &
            iostat=ios)
        if (ios /= 0) then
            if (present(message)) message = "cannot open LaTeX output "//path
            return
        end if

        write (unit, "(a)") "% generated by fortsym -- do not edit"
        derivation = chars(self%derivation)
        if (len(derivation) > 0) then
            write (unit, "(a)") "% derivation: "//derivation
        end if
        do k = 1, self%n_equations
            write (unit, "(a)") "\newcommand{\"// &
                chars(self%equations(k)%macro)//"}{"// &
                chars(self%equations(k)%body)//"}"
            if (self%equations(k)%assumption%len() > 0) then
                write (unit, "(a)") "\newcommand{\assumption"// &
                    chars(self%equations(k)%macro)//"}{"// &
                    chars(self%equations(k)%assumption)//"}"
            end if
        end do
        close (unit)
        if (present(ok)) ok = .true.
    end subroutine latex_write

    subroutine latex_clear(self)
        class(latex_t), intent(inout) :: self

        if (allocated(self%symbol_names)) deallocate (self%symbol_names)
        if (allocated(self%symbol_values)) deallocate (self%symbol_values)
        if (allocated(self%equations)) deallocate (self%equations)
        self%n_equations = 0
        self%derivation = str("")
    end subroutine latex_clear

    subroutine ensure_name_storage(self)
        class(latex_t), intent(inout) :: self

        if (.not. allocated(self%symbol_names)) allocate (self%symbol_names(0))
        if (.not. allocated(self%symbol_values)) allocate (self%symbol_values(0))
    end subroutine ensure_name_storage

    subroutine append_name(self, name, value)
        class(latex_t), intent(inout) :: self
        type(str_t), intent(in) :: name, value
        type(str_t), allocatable :: larger_names(:), larger_values(:)
        integer :: old_size, new_size

        old_size = size(self%symbol_names)
        new_size = max(4, 2*old_size)
        allocate (larger_names(new_size), larger_values(new_size))
        if (old_size > 0) then
            larger_names(1:old_size) = self%symbol_names
            larger_values(1:old_size) = self%symbol_values
        end if
        larger_names(old_size + 1) = name
        larger_values(old_size + 1) = value
        call move_alloc(larger_names, self%symbol_names)
        call move_alloc(larger_values, self%symbol_values)
    end subroutine append_name

    subroutine append_equation(self, label, macro, body, assumption)
        class(latex_t), intent(inout) :: self
        type(str_t), intent(in) :: label, macro, body, assumption
        type(latex_equation_t), allocatable :: larger(:)
        integer :: old_size, new_size

        if (.not. allocated(self%equations)) then
            allocate (self%equations(0))
        end if
        old_size = size(self%equations)
        if (old_size == 0) then
            new_size = 4
        else if (self%n_equations >= old_size) then
            new_size = 2*old_size
        else
            new_size = old_size
        end if
        if (new_size > old_size) then
            allocate (larger(new_size))
            if (old_size > 0) larger(1:old_size) = self%equations
            call move_alloc(larger, self%equations)
        end if
        self%n_equations = self%n_equations + 1
        self%equations(self%n_equations)%label = label
        self%equations(self%n_equations)%macro = macro
        self%equations(self%n_equations)%body = body
        self%equations(self%n_equations)%assumption = assumption
    end subroutine append_equation

    function optional_string(value) result(text)
        character(*), intent(in), optional :: value
        type(str_t) :: text

        if (present(value)) then
            text = str(value)
        else
            text = str("")
        end if
    end function optional_string

    function latex_macro_name(label) result(macro)
        character(*), intent(in) :: label
        character(:), allocatable :: macro
        character(:), allocatable :: word
        type(str_t) :: built
        character :: c
        integer :: k

        built = str("eq")
        do k = 1, len(label)
            c = label(k:k)
            if ((c >= "A" .and. c <= "Z") .or. (c >= "a" .and. c <= "z")) then
                built = built//str(c)
            else if (c >= "0" .and. c <= "9") then
                word = digit_word(c)
                built = built//str(word)
            else if (c == "_") then
                built = built//str("Underscore")
            else if (c == "-") then
                built = built//str("Hyphen")
            else
                built = built//str("X")
            end if
        end do
        macro = chars(built)
    end function latex_macro_name

    function digit_word(c) result(word)
        character, intent(in) :: c
        character(:), allocatable :: word

        select case (c)
        case ("0"); word = "Zero"
        case ("1"); word = "One"
        case ("2"); word = "Two"
        case ("3"); word = "Three"
        case ("4"); word = "Four"
        case ("5"); word = "Five"
        case ("6"); word = "Six"
        case ("7"); word = "Seven"
        case ("8"); word = "Eight"
        case ("9"); word = "Nine"
        case default; word = "X"
        end select
    end function digit_word

end module fortsym_latex
