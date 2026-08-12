#include "fortsym.h"

#include <type_traits>

static_assert(std::is_pointer<fortsym_arena *>::value, "arena is opaque");
static_assert(std::is_pointer<fortsym_expr *>::value, "expression is opaque");

int main() { return fortsym_abi_version() == 40 ? 0 : 1; }
