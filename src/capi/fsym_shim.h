/* fsym_shim -- fortsym's own C ABI over the parts of SymEngine that its
 * C wrapper (symengine/cwrapper.h) does not reach.
 *
 * fortsym binds cwrapper.h directly: it is a stable, MIT-licensed C ABI whose
 * express purpose is to be a binding substrate, so re-wrapping its ~250 entry
 * points would be cost without benefit. This header covers only what cwrapper.h
 * genuinely cannot do:
 *
 *   1. series() and simplify() live in symengine/series.h and
 *      symengine/simplify.h and are never exposed to C.
 *   2. Rendering a basic to text through cwrapper hands back a malloc'd char*
 *      that the caller must release with basic_str_free. Doing that dance from
 *      Fortran means either leaking on an error path or rendering twice to
 *      learn the length. The render/fetch pair below renders once into a
 *      shim-owned buffer and lets Fortran size its string from the result.
 *   3. Several cwrapper declarations sit behind #ifdef HAVE_SYMENGINE_*.
 *      Those macros are invisible to Fortran, and referencing a symbol that a
 *      given SymEngine build omits is a link error. The probes report what the
 *      linked library actually supports.
 *   4. FLINT exact rational and algebraic values need an ownership-safe,
 *      resource-bounded C surface; no FLINT allocation crosses into Fortran.
 *
 * Every entry point returns symengine_exceptions_t (0 == success) unless it
 * returns a value directly; C++ exceptions are caught at the boundary and never
 * cross into Fortran.
 */

#ifndef FSYM_SHIM_H
#define FSYM_SHIM_H

#include <stddef.h>
#include <stdint.h>

#include <symengine/cwrapper.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ---------------------------------------------------------------- probes -- */

/*! Non-zero when the linked SymEngine was built with the LLVM JIT, i.e. when
 *  the llvm_double_visitor_* family is present. */
int fsym_have_llvm(void);

/*! Non-zero when the linked SymEngine was built against MPFR, i.e. when
 *  basic_evalf honours a precision above double. */
int fsym_have_mpfr(void);

/*! SymEngine's version string. Borrowed, valid for the process lifetime. */
const char *fsym_symengine_version(void);

/* ----------------------------------------------------------- processes -- */

/*! Atomically create a mode-0700 temporary directory from a fixed fortsym
 *  template and copy its NUL-terminated path into `path`. */
int fsym_make_temp_directory(char *path, size_t size);

/*! Remove an empty directory created by fsym_make_temp_directory. */
int fsym_remove_temp_directory(const char *path);

/* ---------------------------------------------------------------- strings -- */

/*! Text rendering modes for fsym_str_render. */
enum {
    FSYM_STR_DEFAULT = 0, /*!< SymEngine's own printer */
    FSYM_STR_CCODE = 1,   /*!< C-code printer */
    FSYM_STR_LATEX = 2,   /*!< LaTeX printer */
    FSYM_STR_JULIA = 3    /*!< Julia printer */
};

/*! Render s in the given mode into a thread-local shim buffer and report the
 *  length in bytes, excluding the terminator. On failure the buffer is emptied
 *  and 0 is returned. The buffer stays valid until this thread renders again,
 *  so the caller must fetch before rendering anything else. */
size_t fsym_str_render(const basic s, int mode);

/*! Copy up to n bytes of this thread's rendered buffer into buf. No terminator
 *  is written: Fortran sizes the destination from fsym_str_render's return
 *  value. Returns the number of bytes copied. */
size_t fsym_str_fetch(char *buf, size_t n);

/* ------------------------------------------------------ exact arithmetic -- */

/*! Binary operations accepted by fsym_exact_binary. */
enum {
    FSYM_EXACT_ADD = 1,
    FSYM_EXACT_SUB = 2,
    FSYM_EXACT_MUL = 3,
    FSYM_EXACT_DIV = 4
};

/*! Parse and canonicalize a base-ten integer or rational using FLINT fmpq.
 *  The result is retained in one thread-local pending-result slot for
 *  fsym_exact_fetch. A second exact operation on the same thread overwrites
 *  that slot, so callers must fetch immediately and must not re-enter an exact
 *  operation between render and fetch. Returns its byte length, or zero for
 *  malformed input, zero denominator, or the local byte budget. */
size_t fsym_exact_normalize(const char *value);

/*! Apply an exact binary operation to canonical or noncanonical base-ten
 *  integer/rational strings. Division by zero and malformed input fail. */
size_t fsym_exact_binary(const char *left, const char *right, int operation);

/*! Raise an exact integer/rational to a signed machine exponent. A negative
 *  power of zero fails instead of entering FLINT with an invalid denominator.
 *  Absolute exponents above the local one-million resource bound fail. */
size_t fsym_exact_pow_si(const char *base, int64_t exponent);

/*! Fetch the pending exact result. The ownership contract matches
 *  fsym_str_fetch: no terminator is written and no FLINT allocation escapes. */
size_t fsym_exact_fetch(char *buf, size_t n);

/*! Convert an exact integer/rational in the finite normal binary64 range to
 *  nearest-even binary64 through a 53-bit MPFR value without separately
 *  overflowing numerator and denominator. Subnormal and overflow-range inputs
 *  are conservatively refused. */
int fsym_exact_get_d(const char *value, double *result);

/*! Non-zero only when the dynamically resolved FLINT fmpq_add symbol belongs to a shared
 *  object. The fo test gate uses this to reject a static-only -lflint path. */
int fsym_flint_is_shared(void);

/*! Non-zero only when mpfr_get_d resolves from a shared object. */
int fsym_mpfr_is_shared(void);

/*! Compare a binary64 observation with a decimal high-precision reference.
 *  The returned error is the absolute difference divided by the binary64 ulp
 *  at the reference value.  The reference is parsed at 512 bits, and no
 *  rounded binary64 copy of it is used for the subtraction. */
int fsym_mpfr_ulp_error(const char *reference, double observed,
                        double *error);

/* ---------------------------------------------------- algebraic numbers -- */

/*! Binary operations accepted by fsym_algebraic_binary. */
enum {
    FSYM_ALGEBRAIC_ADD = 1,
    FSYM_ALGEBRAIC_SUB = 2,
    FSYM_ALGEBRAIC_MUL = 3,
    FSYM_ALGEBRAIC_DIV = 4
};

/*! Unary operations accepted by fsym_algebraic_unary. */
enum {
    FSYM_ALGEBRAIC_CONJ = 1,
    FSYM_ALGEBRAIC_SQRT = 2
};

/*! Normalize a lossless qqbar1 serialization. Values are represented by their
 *  primitive minimal-polynomial coefficients and zero-based index in FLINT's
 *  canonical conjugate-root order. Degree, coefficient height, input, and
 *  output budgets are enforced before a result is retained. */
size_t fsym_algebraic_normalize(const char *value);

/*! Project an exact real qqbar1 atom to a uniquely rounded finite normal
 *  binary64 value using a FLINT Arb enclosure. Non-real, subnormal, overflow,
 *  and rounding-boundary values are refused. */
int fsym_algebraic_get_d(const char *value, double *result);

/*! Non-zero when a canonical qqbar1 atom has exact rational real and
 *  imaginary components and can cross into SymEngine's exact complex domain. */
int fsym_algebraic_symengine_supported(const char *value);

/*! Convert a supported Gaussian-rational qqbar1 atom to an exact SymEngine
 *  expression. Higher-degree or otherwise non-rational atoms return
 *  SYMENGINE_NOT_IMPLEMENTED. */
CWRAPPER_OUTPUT_TYPE fsym_algebraic_to_symengine(basic out,
                                                  const char *value);

/*! Construct the imaginary unit, or an exact Gaussian rational from base-ten
 *  rational real and imaginary parts. */
size_t fsym_algebraic_i(void);
size_t fsym_algebraic_from_re_im(const char *real_part,
                                 const char *imag_part);
/*! Return the exact real or imaginary qqbar component. */
size_t fsym_algebraic_re(const char *value);
size_t fsym_algebraic_im(const char *value);

/*! Apply bounded exact arithmetic in FLINT's qqbar domain. Division by zero,
 *  negative powers of zero, and resource-limit crossings fail. Square root
 *  uses FLINT's documented principal branch. */
size_t fsym_algebraic_binary(const char *left, const char *right,
                             int operation);
size_t fsym_algebraic_unary(const char *value, int operation);
size_t fsym_algebraic_pow_si(const char *base, int64_t exponent);

/*! Return exact signs of the real and imaginary parts (-1, 0, or +1).
 *  Malformed or over-budget input returns zero without writing a verdict. */
int fsym_algebraic_signs(const char *value, int *real_sign, int *imag_sign);

/*! Fetch the pending algebraic serialization. The ownership and immediate
 *  fetch contract matches fsym_exact_fetch. */
size_t fsym_algebraic_fetch(char *buf, size_t n);

/* ------------------------------------------------------------- transforms -- */

/*! Assign to out the series expansion of ex in var, truncated to prec terms.
 *  var must be a Symbol. Wraps SymEngine::series. */
CWRAPPER_OUTPUT_TYPE fsym_series(basic out, const basic ex, const basic var,
                                 unsigned int prec);

/*! Assign to out a simplified form of ex.
 *
 *  SymEngine::simplify on its own is weak -- it leaves sin(x)**2+cos(x)**2,
 *  (x**2-1)/(x-1) and exp(log(x)) untouched -- so this is not a passthrough.
 *  It generates several candidate forms and returns the one with the lowest
 *  operation count, which is the strategy SymPy's top-level simplify uses. */
CWRAPPER_OUTPUT_TYPE fsym_simplify(basic out, const basic ex);

/*! Number of operations in ex, the complexity measure fsym_simplify ranks by. */
unsigned int fsym_count_ops(const basic ex);

/*! Verdicts from fsym_zero_test. */
enum {
    FSYM_ZERO_UNKNOWN = 0, /*!< undecided; the caller should fall back to a probe */
    FSYM_ZERO_TRUE = 1,    /*!< identically zero, established symbolically */
    FSYM_ZERO_FALSE = 2    /*!< not identically zero */
};

/*! Decide whether ex is identically zero.
 *
 *  The procedure puts ex into exponential normal form (SymEngine::rewrite_as_exp),
 *  normalizes exponent arguments so exp(-I*(x+y)) and exp(-I*x-I*y) coincide,
 *  rewrites exp(n*log(u)) as u**n, folds the result onto a single rational
 *  function, and expands the numerator. That is a decision procedure for the
 *  field generated by exponentials of rational-linear arguments, which covers
 *  the trigonometric, hyperbolic and rational identities in practice.
 *
 *  It deliberately returns FSYM_ZERO_FALSE for expressions that vanish only on
 *  part of the domain -- atan(x)+atan(1/x)-pi/2 and sqrt(x**2)-x among them.
 *  Those are not identities, and reporting them as zero would be wrong.
 *
 *  FSYM_ZERO_UNKNOWN means the algebra did not close: rewrite_as_exp does not
 *  handle every function (gamma, erf and the Bessel family have no exponential
 *  form), and the node-count guard aborts an expansion that would blow up. The
 *  caller decides what to do -- fortsym_check falls back to a randomized
 *  high-precision numeric probe and reports the weaker verdict honestly.
 *
 *  max_nodes bounds intermediate growth; pass 0 for the default. */
int fsym_zero_test(const basic ex, unsigned long max_nodes);

/*! Assign to out the exponential normal form used by fsym_zero_test. Exposed
 *  because it is the canonical form worth inspecting when a check fails. */
CWRAPPER_OUTPUT_TYPE fsym_normal_form(basic out, const basic ex);

#ifdef __cplusplus
}
#endif

#endif /* FSYM_SHIM_H */
