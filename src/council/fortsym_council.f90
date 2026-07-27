module fortsym_council
    ! Several engines, one answer -- and a record of who said what.
    !
    ! Three jobs, kept apart because they have different correctness
    ! requirements and conflating them would quietly weaken all three:
    !
    !   1. Oracle. Engines vote on a zero verdict. Agreement raises confidence;
    !      DISAGREEMENT IS A REPORTED FINDING, not something to average away. If
    !      one engine says ZERO and another says NONZERO, one of them is wrong,
    !      and that is worth more than either answer.
    !
    !   2. Tournament. Each engine proposes a simplified form. Every proposal is
    !      first VERIFIED EQUIVALENT to the original, then ranked by node count.
    !      Verification is what makes this safe: a wrong proposal is discarded
    !      before it can win on being small.
    !
    !   3. Fallback. When the preferred engine returns UNKNOWN, ask the others.
    !      Capability differences are real -- SymEngine cannot integrate, Yacas
    !      cannot simplify trigonometry -- so a second opinion often decides what
    !      the first could not.
    !
    ! Timing is recorded on every call, so a benchmark table is a by-product of
    ! ordinary use rather than a separate harness.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_expr, only: expr_t, operator(-), operator(==)
    use fortsym_engine, only: engine_t, engine_result_t, VERDICT_UNKNOWN, &
        VERDICT_TRUE, VERDICT_FALSE, verdict_name
    implicit none
    private

    public :: council_t, council_vote_t, member_ref_t
    public :: council_add, council_zero_test, council_best_form
    public :: council_report, council_benchmark_table

    integer, parameter :: dp = real64
    integer, parameter :: MAX_MEMBERS = 8

    !> One engine's answer, kept alongside the others rather than folded in.
    type :: council_vote_t
        type(str_t) :: engine
        integer     :: verdict = VERDICT_UNKNOWN
        logical     :: answered = .false.
        real(dp)    :: seconds = 0.0_dp
    end type council_vote_t

    !> Result of a council decision, including every vote behind it.
    type :: council_decision_t
        integer                        :: verdict = VERDICT_UNKNOWN
        logical                        :: disagreement = .false.
        type(council_vote_t), allocatable :: votes(:)
        integer                        :: n_votes = 0
    end type council_decision_t
    public :: council_decision_t

    !> A polymorphic engine slot. Fortran needs the indirection to hold a
    !> heterogeneous collection of engine subclasses.
    type :: member_ref_t
        class(engine_t), allocatable :: eng
    end type member_ref_t

    !> Running totals per engine, accumulated across every call.
    type :: member_stats_t
        integer  :: calls = 0
        integer  :: answered = 0
        integer  :: decided = 0
        real(dp) :: seconds = 0.0_dp
    end type member_stats_t

    type :: council_t
        type(member_ref_t)   :: members(MAX_MEMBERS)
        type(member_stats_t) :: stats(MAX_MEMBERS)
        integer              :: n = 0
        !> Findings: cases where engines contradicted each other.
        integer              :: n_disagreements = 0
        type(strbuf_t)       :: findings
    end type council_t

contains

    !> Add an engine. Unavailable engines are dropped here rather than checked
    !> at every use site, so the rest of the code never asks whether a tool is
    !> installed.
    subroutine council_add(c, eng)
        type(council_t), intent(inout) :: c
        class(engine_t), intent(in)    :: eng

        if (.not. eng%available) return
        if (c%n >= MAX_MEMBERS) return
        c%n = c%n + 1
        allocate (c%members(c%n)%eng, source=eng)
    end subroutine council_add

    !> Ask every engine whether an expression is identically zero.
    !>
    !> A single ZERO or NONZERO from any engine decides the verdict, since both
    !> are positive claims and the engines here only make them when they can
    !> justify them. Two engines making *opposite* positive claims is a
    !> contradiction, recorded as a finding, and the decision falls back to
    !> UNKNOWN -- with the engines in conflict there is no honest verdict left.
    function council_zero_test(c, e, label) result(d)
        type(council_t),        intent(inout) :: c
        type(expr_t),           intent(in)    :: e
        character(*), optional, intent(in)    :: label
        type(council_decision_t)              :: d

        type(engine_result_t) :: r
        integer :: k, n_true, n_false

        allocate (d%votes(c%n))
        d%n_votes = 0
        n_true = 0
        n_false = 0

        do k = 1, c%n
            r = c%members(k)%eng%zero_test(e)

            c%stats(k)%calls = c%stats(k)%calls + 1
            c%stats(k)%seconds = c%stats(k)%seconds + r%seconds
            if (r%ok) c%stats(k)%answered = c%stats(k)%answered + 1
            if (r%verdict /= VERDICT_UNKNOWN) &
                c%stats(k)%decided = c%stats(k)%decided + 1

            d%n_votes = d%n_votes + 1
            d%votes(d%n_votes)%engine = c%members(k)%eng%name
            d%votes(d%n_votes)%verdict = r%verdict
            d%votes(d%n_votes)%answered = r%ok
            d%votes(d%n_votes)%seconds = r%seconds

            if (r%verdict == VERDICT_TRUE) n_true = n_true + 1
            if (r%verdict == VERDICT_FALSE) n_false = n_false + 1
        end do

        if (n_true > 0 .and. n_false > 0) then
            ! Contradiction. Both cannot be right, so neither is trusted.
            d%disagreement = .true.
            d%verdict = VERDICT_UNKNOWN
            c%n_disagreements = c%n_disagreements + 1
            call record_finding(c, d, label)
        else if (n_true > 0) then
            d%verdict = VERDICT_TRUE
        else if (n_false > 0) then
            d%verdict = VERDICT_FALSE
        else
            d%verdict = VERDICT_UNKNOWN
        end if
    end function council_zero_test

    subroutine record_finding(c, d, label)
        type(council_t),          intent(inout) :: c
        type(council_decision_t), intent(in)    :: d
        character(*), optional,   intent(in)    :: label
        integer :: k

        call c%findings%append("DISAGREEMENT")
        if (present(label)) then
            call c%findings%append(": ")
            call c%findings%append(label)
        end if
        call c%findings%newline()
        do k = 1, d%n_votes
            call c%findings%append("    ")
            call c%findings%append(chars(d%votes(k)%engine))
            call c%findings%append(" -> ")
            call c%findings%append(chars(verdict_name(d%votes(k)%verdict)))
            call c%findings%newline()
        end do
    end subroutine record_finding

    !> The smallest form any engine can produce that is provably the same
    !> expression.
    !>
    !> Verification comes first and is not optional. An engine that simplifies
    !> incorrectly would otherwise win precisely because its wrong answer is
    !> smaller, and generated kernels would inherit the error. `verifier` is the
    !> engine trusted to decide `proposal - original == 0`; it should be the one
    !> with a real decision procedure.
    function council_best_form(c, e, verifier) result(best)
        type(council_t), intent(inout) :: c
        type(expr_t),    intent(in)    :: e
        class(engine_t), intent(inout) :: verifier
        type(expr_t)                   :: best

        type(engine_result_t) :: proposal, check
        integer :: k, best_size, size_k

        best = e
        best_size = e%node_count()

        do k = 1, c%n
            proposal = c%members(k)%eng%simplify(e)

            c%stats(k)%calls = c%stats(k)%calls + 1
            c%stats(k)%seconds = c%stats(k)%seconds + proposal%seconds
            if (proposal%ok) c%stats(k)%answered = c%stats(k)%answered + 1

            if (.not. proposal%ok) cycle

            size_k = proposal%value%node_count()
            if (size_k >= best_size) cycle

            ! Smaller is only better if it is the same function.
            check = verifier%zero_test(proposal%value - e)
            if (check%verdict /= VERDICT_TRUE) cycle

            best = proposal%value
            best_size = size_k
        end do
    end function council_best_form

    !> Human-readable account of one decision: what each engine said and how
    !> long it took.
    function council_report(d) result(s)
        type(council_decision_t), intent(in) :: d
        type(str_t)                          :: s
        type(strbuf_t) :: b
        integer :: k

        call b%append("verdict: ")
        call b%append(chars(verdict_name(d%verdict)))
        if (d%disagreement) call b%append("  [ENGINES DISAGREE]")
        call b%newline()
        do k = 1, d%n_votes
            call b%append("    ")
            call b%append(chars(d%votes(k)%engine))
            call b%append(": ")
            call b%append(chars(verdict_name(d%votes(k)%verdict)))
            call b%append("  (")
            call b%append(chars(str(d%votes(k)%seconds, '(f9.5)')))
            call b%append(" s)")
            call b%newline()
        end do
        s = b%to_str()
    end function council_report

    !> Benchmark table over everything the council has been asked so far.
    !>
    !> "decided" is the column that matters alongside time: an engine that
    !> answers instantly by returning UNKNOWN is not fast, it is absent.
    function council_benchmark_table(c) result(s)
        type(council_t), intent(in) :: c
        type(str_t)                 :: s
        type(strbuf_t) :: b
        integer :: k
        real(dp) :: per_call

        call b%append("engine          calls  decided   total s   per call s")
        call b%newline()
        call b%append("--------------------------------------------------------")
        call b%newline()

        do k = 1, c%n
            per_call = 0.0_dp
            if (c%stats(k)%calls > 0) &
                per_call = c%stats(k)%seconds/real(c%stats(k)%calls, dp)

            call b%append(pad(chars(c%members(k)%eng%name), 14))
            call b%append(lpad(chars(str(c%stats(k)%calls)), 7))
            call b%append(lpad(chars(str(c%stats(k)%decided)), 9))
            call b%append(lpad(chars(str(c%stats(k)%seconds, '(f9.4)')), 10))
            call b%append(lpad(chars(str(per_call, '(f11.6)')), 13))
            call b%newline()
        end do

        if (c%n_disagreements > 0) then
            call b%newline()
            call b%append("disagreements: ")
            call b%append(chars(str(c%n_disagreements)))
            call b%newline()
        end if

        s = b%to_str()
    end function council_benchmark_table

    pure function pad(t, width) result(r)
        character(*), intent(in)  :: t
        integer,      intent(in)  :: width
        character(:), allocatable :: r
        integer :: k
        r = t
        do k = len(t) + 1, width
            r = r//" "
        end do
    end function pad

    pure function lpad(t, width) result(r)
        character(*), intent(in)  :: t
        integer,      intent(in)  :: width
        character(:), allocatable :: r
        integer :: k
        r = t
        do k = len(t) + 1, width
            r = " "//r
        end do
    end function lpad

end module fortsym_council
