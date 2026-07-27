/* Implementation of fortsym's Yacas shim. See fsym_yacas.h.
 *
 * The whole implementation is conditional. Yacas is fetched and built by the
 * CMake path, which defines FORTSYM_HAVE_YACAS; the fpm path has no way to
 * build a C++ dependency from source, so this file must still compile there
 * without Yacas's headers. When it is absent the entry points are present but
 * report the engine unavailable, which is exactly what a caller already handles
 * for every optional backend.
 */

#include <cstring>

#include "fsym_yacas.h"

#ifdef FORTSYM_HAVE_YACAS

#include <sstream>
#include <string>

#include <yacas/yacas.h>

namespace
{

/*! Escape a path for embedding in a Yacas string literal. A Windows-style path
 *  or one containing a quote would otherwise terminate the literal early and
 *  turn a configuration mistake into a parse error somewhere unrelated. */
std::string escape(const std::string &s)
{
    std::string out;
    out.reserve(s.size() + 8);
    for (char c : s) {
        if (c == '\\' || c == '"') {
            out.push_back('\\');
        }
        out.push_back(c);
    }
    return out;
}

/*! Yacas terminates every result with a semicolon -- Result() for 1+1 is "2;",
 *  not "2". Left in place it reaches the expression parser as trailing garbage,
 *  so it is removed once, here, rather than at each call site. */
std::string strip_terminator(const std::string &s)
{
    size_t n = s.size();
    while (n > 0 && (s[n - 1] == ';' || s[n - 1] == ' ' || s[n - 1] == '\n')) {
        --n;
    }
    return s.substr(0, n);
}

} // namespace

struct fsym_yacas {
    /* Yacas writes side-band output to a stream. It is captured rather than
     * left pointing at stdout, so an engine call cannot scribble over a
     * Fortran test's own output. */
    std::ostringstream sink;
    CYacas engine;
    std::string result;
    std::string error;

    fsym_yacas() : sink(), engine(sink) {}
};

extern "C" {

fsym_yacas *fsym_yacas_new(const char *scripts_dir)
{
    if (scripts_dir == nullptr) {
        return nullptr;
    }

    fsym_yacas *self = nullptr;
    try {
        self = new fsym_yacas();

        // Point the engine at its script library and load the entry point.
        // Without this Yacas cannot even parse `x^2`, because the operator
        // precedences themselves are defined in the scripts.
        const std::string dir = escape(std::string(scripts_dir));
        self->engine.Evaluate("DefaultDirectory(\"" + dir + "/\");");
        if (self->engine.IsError()) {
            delete self;
            return nullptr;
        }

        self->engine.Evaluate("Load(\"yacasinit.ys\");");
        if (self->engine.IsError()) {
            delete self;
            return nullptr;
        }

        // Confirm the library really loaded. A DefaultDirectory pointing
        // somewhere plausible but wrong can leave the engine half-initialised,
        // and a backend that reports itself available and then misparses every
        // expression is worse than one that reports itself missing.
        self->engine.Evaluate("Simplify(1+1);");
        if (self->engine.IsError() ||
            strip_terminator(self->engine.Result()) != "2") {
            delete self;
            return nullptr;
        }
    } catch (...) {
        delete self;
        return nullptr;
    }

    return self;
}

void fsym_yacas_free(fsym_yacas *self)
{
    delete self;
}

int fsym_yacas_eval(fsym_yacas *self, const char *expression)
{
    if (self == nullptr || expression == nullptr) {
        return 0;
    }

    self->result.clear();
    self->error.clear();

    try {
        self->engine.Evaluate(std::string(expression));
        if (self->engine.IsError()) {
            self->error = self->engine.Error();
            return 0;
        }
        self->result = strip_terminator(self->engine.Result());
    } catch (const std::exception &e) {
        self->error = e.what();
        return 0;
    } catch (...) {
        self->error = "unknown Yacas failure";
        return 0;
    }

    return 1;
}

size_t fsym_yacas_result_len(fsym_yacas *self)
{
    return self == nullptr ? 0 : self->result.size();
}

size_t fsym_yacas_result_fetch(fsym_yacas *self, char *buf, size_t n)
{
    if (self == nullptr || buf == nullptr) {
        return 0;
    }
    const size_t count = n < self->result.size() ? n : self->result.size();
    if (count > 0) {
        std::memcpy(buf, self->result.data(), count);
    }
    return count;
}

size_t fsym_yacas_error_len(fsym_yacas *self)
{
    return self == nullptr ? 0 : self->error.size();
}

size_t fsym_yacas_error_fetch(fsym_yacas *self, char *buf, size_t n)
{
    if (self == nullptr || buf == nullptr) {
        return 0;
    }
    const size_t count = n < self->error.size() ? n : self->error.size();
    if (count > 0) {
        std::memcpy(buf, self->error.data(), count);
    }
    return count;
}

} /* extern "C" */

#else /* !FORTSYM_HAVE_YACAS */

extern "C" {

/* Built without Yacas. Reporting unavailable is the same answer the caller
 * gets when the script library cannot be loaded, so no path above this needs a
 * second kind of absence to handle. */

fsym_yacas *fsym_yacas_new(const char *) { return nullptr; }
void fsym_yacas_free(fsym_yacas *) {}
int fsym_yacas_eval(fsym_yacas *, const char *) { return 0; }
size_t fsym_yacas_result_len(fsym_yacas *) { return 0; }
size_t fsym_yacas_result_fetch(fsym_yacas *, char *, size_t) { return 0; }
size_t fsym_yacas_error_len(fsym_yacas *) { return 0; }
size_t fsym_yacas_error_fetch(fsym_yacas *, char *, size_t) { return 0; }

} /* extern "C" */

#endif /* FORTSYM_HAVE_YACAS */
