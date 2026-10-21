#!/bin/bash
# The same native test selection runs in a build container or a heavy Slurm tier.
set -euo pipefail
src=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$src"
if [ -n "${SLURM_JOB_ID:-}" ]; then
    root=$(dirname "$src")
    tier=$(cat "$root/tier.txt")
    . "$root/task.env"
    . /home/ert/opt/kin6d-env.sh
    export PATH=/home/ert/.local/bin:$PATH
else
    tier=${1:?usage: ci/run.sh TIER}
    root=$(mktemp -d "${RUNNER_TEMP:-/tmp}/native-ci.XXXXXX")
    export FC=/usr/bin/gfortran CC=/usr/bin/gcc CXX=/usr/bin/g++
    export OPENBLAS_NUM_THREADS=1 MPLBACKEND=Agg
fi
unset DISPLAY
export CMAKE_TOOLCHAIN_FILE=$src/ci/cluster-toolchain.cmake
if [ -d "$root/mirrors" ]; then
    export GIT_CONFIG_GLOBAL=$root/gitconfig
    : > "$GIT_CONFIG_GLOBAL"
    git config --global protocol.file.allow always
    shopt -s nullglob
    for mirror in "$root"/mirrors/*/*.git; do
        slug=${mirror#"$root/mirrors/"}; slug=${slug%.git}
        git config --global --add "url.file://$mirror.insteadOf" "https://github.com/$slug.git"
        git config --global --add "url.file://$mirror.insteadOf" "https://github.com/$slug"
        git config --global --add "url.file://$mirror.insteadOf" "git@github.com:$slug.git"
    done
fi
export GITHUB_WORKSPACE=$src
export GITHUB_ENV=$root/github-env GITHUB_PATH=$root/github-path GITHUB_OUTPUT=$root/github-output
: > "$GITHUB_ENV"; : > "$GITHUB_PATH"; : > "$GITHUB_OUTPUT"
export SCI_CPUS=${SCI_CPUS:-8}
export CMAKE_BUILD_PARALLEL_LEVEL=$SCI_CPUS CTEST_PARALLEL_LEVEL=$SCI_CPUS
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
export RUNNER_TEMP=$root/tmp
mkdir -p "$RUNNER_TEMP" ci-out
{ hostname; /usr/bin/gfortran --version | head -1; git rev-parse HEAD; } | tee ci-out/toolchain.txt
load_job_environment() {
    while IFS='=' read -r key value; do
        [[ $key =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]] && export "$key=$value"
    done < "$GITHUB_ENV"
    while IFS= read -r item; do [ -z "$item" ] || export PATH="$item:$PATH"; done < "$GITHUB_PATH"
}
case "$tier" in
  ci-python-package)
    python3 -m venv "$root/venv"
    . "$root/venv/bin/activate"
    export CCACHE_DIR="$GITHUB_WORKSPACE/.ccache"
    export CCACHE_MAXSIZE="500M"
    echo 'Build the native shared library'
    (
cmake -S . -B build -G Ninja \
  -DCMAKE_BUILD_TYPE=Release -DFORTSYM_BUILD_TESTING=OFF
cmake --build build -j "${SCI_CPUS:-8}"

    )
    load_job_environment
    echo 'Build and install the wheel in a clean environment'
    (
# shellcheck disable=SC2016
python3 -m venv .venv
.venv/bin/python -m pip install --quiet build
.venv/bin/python -m build --wheel --outdir wheelhouse python
.venv/bin/python -m pip install --quiet wheelhouse/*.whl
# shellcheck disable=SC2016
bash -c '
  set -eu
  library_path="$GITHUB_WORKSPACE/build/lib"
  for flag in ${NIX_LDFLAGS:-}; do
    case "$flag" in
      -L*) library_path="$library_path:${flag#-L}" ;;
    esac
  done
  export LD_LIBRARY_PATH="$library_path${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
  export FORTSYM_LIBRARY="$GITHUB_WORKSPACE/build/lib/libfortsym.so"
  site_packages="$("$GITHUB_WORKSPACE/.venv/bin/python" -c "import sysconfig; print(sysconfig.get_path(\"purelib\"))")"
  export PYTHONPATH="$site_packages"
  exec python3 -
' <<'PY'
from fractions import Fraction
import fortsym
import fortsym.sympy as sp

x = fortsym.Symbol("x")
assert str(((x + 1) ** 2).expand()) == "x**2 + 2*x + 1"
assert "1/3" in str(x + Fraction(1, 3))
assert fortsym.Integer(2**100).exact_text == str(2**100)
sx = sp.Symbol("x")
assert isinstance(sx + 1, sp.Add)
assert sp.simplify(sp.diff(sp.exp(sx), sx)) == sp.exp(sx)
PY

    )
    load_job_environment
    ;;
  ci-provenance)
    export CCACHE_DIR="$GITHUB_WORKSPACE/.ccache"
    export CCACHE_MAXSIZE="500M"
    echo 'Check new compatibility sources'
    (
git diff --name-only --diff-filter=A \
  "${SCI_BASE_SHA}...${SCI_HEAD_SHA}" \
  > changed-files.txt
python3 scripts/check_provenance.py \
  --changed-files changed-files.txt \
  --provenance doc/provenance.md

    )
    load_job_environment
    ;;
  ci-cmake)
    export CCACHE_DIR="$GITHUB_WORKSPACE/.ccache"
    export CCACHE_MAXSIZE="500M"
    echo Configure
    (
cmake -S . -B build -G Ninja \
  -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_C_COMPILER_LAUNCHER=ccache \
  -DCMAKE_CXX_COMPILER_LAUNCHER=ccache

    )
    load_job_environment
    echo Build
    (
cmake --build build -j "${SCI_CPUS:-8}"
    )
    load_job_environment
    echo Test
    (
# shellcheck disable=SC2016
bash -c '
  set -eu
  library_path="$GITHUB_WORKSPACE/build/lib"
  for flag in ${NIX_LDFLAGS:-}; do
    case "$flag" in
      -L*) library_path="$library_path:${flag#-L}" ;;
    esac
  done
  export LD_LIBRARY_PATH="$library_path${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
  exec ctest --test-dir build --output-on-failure
'

    )
    load_job_environment
    ;;
  ci-generated-up-to-date)
    export CCACHE_DIR="$GITHUB_WORKSPACE/.ccache"
    export CCACHE_MAXSIZE="500M"
    echo 'Configure and build generators'
    (
cmake -S . -B build -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_C_COMPILER_LAUNCHER=ccache \
  -DCMAKE_CXX_COMPILER_LAUNCHER=ccache
cmake --build build -j "${SCI_CPUS:-8}"

    )
    load_job_environment
    echo 'Regenerate and diff'
    (
set -euo pipefail
shopt -s nullglob
generated=(src/generated/*.f90)
if [ ${#generated[@]} -eq 0 ]; then
  echo "no generated sources yet; nothing to regenerate"
  exit 0
fi
for file in "${generated[@]}"; do
  gen=$(sed -n 's/^! Generator: //p' "$file" | head -n 1)
  if [ -z "$gen" ]; then
    echo "$file carries no '! Generator:' header line" >&2
    exit 1
  fi
  if [ ! -x "build/bin/$gen" ]; then
    echo "$file names generator '$gen', which the build did not produce" >&2
    exit 1
  fi
  echo "regenerating $file with $gen"
  "build/bin/$gen"
done
git diff --exit-code -- src/generated

    )
    load_job_environment
    ;;
  *) echo "unknown CI tier: $tier" >&2; exit 2 ;;
esac
