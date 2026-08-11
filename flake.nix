{
  # Reproducible toolchain for fortsym.
  #
  # cmake/deps/flint.cmake and cmake/deps/mpfr.cmake pin FLINT and MPFR to an
  # exact version and fail configuration on anything else, and SymEngine is not
  # packaged for every distribution at all. That combination is unbuildable on a
  # stock CI image: no apt or dnf repository happens to carry precisely those
  # three versions at once, and the ones that do drift out from under the pin on
  # their own schedule. Nix fixes the whole dependency closure in flake.lock, so
  # CI, a contributor's machine and a release build compile against identical
  # libraries, and a version bump is a reviewable lockfile diff rather than a
  # CI-image surprise.
  description = "fortsym: computer algebra with a Fortran interface";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Tier 1: linked in-process, required to build at all. Yacas is Tier 1
        # too but the build fetches and compiles it itself (cmake/deps/yacas.cmake
        # explains why), so it needs git rather than a package here.
        tier1 = with pkgs; [
          cmake
          ninja
          gfortran
          symengine
          flint
          mpfr
          gmp
          git
          python3
        ];

        # Tier 2: optional engines run as separate processes. Their absence must
        # never break a build, which is exactly what the Tier 1 CI job proves by
        # not having them.
        tier2 = with pkgs; [
          maxima
          (python3.withPackages (ps: [ ps.sympy ]))
        ];

        # ccache earns its place on the Yacas build specifically: it is the one
        # large C++ compile in the tree and it changes only when its pinned tag
        # does, so a warm cache turns it into a few seconds of linking.
        devTools = with pkgs; [ ccache pkg-config ];

        mkShell = extra: pkgs.mkShell {
          packages = tier1 ++ devTools ++ extra;

          # The exact-version guards read these headers directly, so a mismatch
          # here is worth catching in the shell rather than 30 seconds into a
          # CMake configure.
          shellHook = ''
            export FORTSYM_FLINT_VERSION=${pkgs.flint.version}
            export FORTSYM_MPFR_VERSION=${pkgs.mpfr.version}
            export FORTSYM_SYMENGINE_VERSION=${pkgs.symengine.version}
          '';
        };
      in
      {
        devShells = {
          # Tier 1 only. What every contributor and every consumer must be able
          # to build with.
          default = mkShell [ ];

          # Tier 1 plus the optional engines, so the council has several voters
          # and the cross-engine oracle is exercised.
          tier2 = mkShell tier2;
        };
      });
}
