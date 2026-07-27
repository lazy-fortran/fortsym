#!/usr/bin/env bash
# Report what fortsym can use on this machine, and print the command to install
# what is missing.
#
# This script never installs anything. Package installation touches the whole
# system, needs privilege, and is the user's decision -- so the command is
# printed for review rather than run. Nothing here is required to build fortsym:
# Tier 1 is linked or fetched by the build itself, and every Tier 2 engine is
# optional.

set -uo pipefail

missing_pacman=()
have_any_optional=0

say() { printf '%s\n' "$*"; }
ok() { printf '  \033[32m✓\033[0m %-12s %s\n' "$1" "$2"; }
no() { printf '  \033[33m-\033[0m %-12s %s\n' "$1" "$2"; }

check() {
    # check <label> <probe-command> <pacman-package> <role>
    local label="$1" probe="$2" package="$3" role="$4"
    if eval "$probe" >/dev/null 2>&1; then
        ok "$label" "$role"
        have_any_optional=1
    else
        no "$label" "$role"
        [ -n "$package" ] && missing_pacman+=("$package")
    fi
}

say ""
say "fortsym environment"
say "==================="
say ""
say "Required to build:"
check "gfortran"  "command -v gfortran"          "gcc-fortran" "Fortran compiler"
check "g++"       "command -v g++"                "gcc"         "C++ compiler for the SymEngine shim"
check "cmake"     "command -v cmake"              "cmake"       "primary build path"
check "git"       "command -v git"                "git"         "fetches Yacas and, optionally, SymEngine"

say ""
say "Tier 1, linked in-process:"
check "symengine" "test -f /usr/include/symengine/cwrapper.h" "symengine" \
      "MIT, the workhorse engine"
check "flint"     "test -f /usr/include/flint/flint.h"        "flint" \
      "LGPL, polynomial GCD behind rational cancellation"
say "  · yacas       LGPL, built from source by the build itself (no package needed)"

say ""
say "Tier 2, optional, run as separate processes:"
check "maxima"    "command -v maxima"                    "maxima" \
      "GPL, integration and factorization"
check "sympy"     "python3 -c 'import sympy'"            "python-sympy" \
      "BSD, broad simplification"
check "giac"      "command -v giac"                      "giac" \
      "GPL, optional extra voter"
check "fricas"    "command -v fricas"                    "fricas" \
      "BSD, optional extra voter"

say ""
if [ ${#missing_pacman[@]} -eq 0 ]; then
    say "Everything fortsym knows about is present."
else
    say "Missing, and the command to install them:"
    say ""
    say "    sudo pacman -S --needed ${missing_pacman[*]}"
    say ""
    say "None of this is required. fortsym builds and tests with Tier 1 alone;"
    say "each missing Tier 2 engine simply means one fewer voter in the council."
fi

say ""
say "Wolfram and Mathematica are deliberately not used. See LEGAL.md section 5."
say ""
