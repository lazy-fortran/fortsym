/* fsym_yacas -- a C ABI over the Yacas engine.
 *
 * Yacas is C++ with no C interface of its own, so a shim is unavoidable; this
 * one keeps that surface as small as the job allows: create an engine, evaluate
 * a string, fetch the result or the error.
 *
 * Yacas is not a pure C++ engine. Almost all of its algebra lives in .ys script
 * files that the engine loads at run time, so an engine that has not been
 * pointed at its script library can barely parse an expression -- it does not
 * even know the precedence of `^`. fsym_yacas_new therefore takes the scripts
 * directory and fails cleanly if the library cannot be loaded, rather than
 * returning a handle that silently misparses everything.
 *
 * Results are fetched through a render/fetch pair, as in fsym_shim, so no
 * malloc'd pointer has to be freed on a Fortran error path.
 */

#ifndef FSYM_YACAS_H
#define FSYM_YACAS_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct fsym_yacas fsym_yacas;

/*! Create an engine and load its script library from `scripts_dir`.
 *
 *  Returns NULL when the engine cannot be created or the script library cannot
 *  be loaded. A NULL result means the backend is unavailable, which the caller
 *  reports and skips; it is never fatal. */
fsym_yacas *fsym_yacas_new(const char *scripts_dir);

void fsym_yacas_free(fsym_yacas *self);

/*! Evaluate `expression` and hold the result for fetching.
 *
 *  Returns 1 on success, 0 when Yacas reported an error. On failure the message
 *  is available through fsym_yacas_error_len / _fetch, because a backend that
 *  cannot say *why* it declined is much harder to debug than one that can. */
int fsym_yacas_eval(fsym_yacas *self, const char *expression);

/*! Length in bytes of the last result, excluding any terminator. */
size_t fsym_yacas_result_len(fsym_yacas *self);

/*! Copy up to n bytes of the last result into buf; returns bytes copied. */
size_t fsym_yacas_result_fetch(fsym_yacas *self, char *buf, size_t n);

size_t fsym_yacas_error_len(fsym_yacas *self);
size_t fsym_yacas_error_fetch(fsym_yacas *self, char *buf, size_t n);

#ifdef __cplusplus
}
#endif

#endif /* FSYM_YACAS_H */
