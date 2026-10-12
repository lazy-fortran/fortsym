#!/bin/sh
# Strong scaling of the generated batch drivers (M2): deterministic scalar-leaf
# variant and SIMD variant. Usage: run_kernel_batch_scaling.sh BUILD_DIR WORK_DIR [threads...]
# WORK_DIR receives the generated sources; on a cluster node copy the two
# kbench_*.f90 files and run only the compile/run lines below.
set -eu
build=$1; work=$2; shift 2
[ $# -gt 0 ] || set -- 1 2 4 8 16 32
mkdir -p "$work"
"$build/bin/gen_kernel_batch_bench" "$work"
gfortran -O3 -march=native -fopenmp -ffp-contract=off -Wall -Wextra \
    -o "$work/kbench_run" "$work/kbench_modules.f90" "$work/kbench_main.f90"
OMP_PLACES=cores OMP_PROC_BIND=spread "$work/kbench_run" "$@"
