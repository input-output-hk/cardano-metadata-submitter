############################################################################
# cardano-metadata-submitter Nix build
#
# fixme: document top-level attributes and how to build them
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize ghc and profiling (see ./nix/haskell.nix):
, config ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-metadata-submitter --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
#   nixpkgs  = ../nixpkgs;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with iohk overlays.
, pkgs ? import ./nix {
    inherit system crossSystem config sourcesOverride;
  }
}:
# commonLib include iohk-nix utilities, our util.nix and nixpkgs lib.
with pkgs; with commonLib;
let


  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages cardanoMetadataSubmitterPackages);

  haskellPackagesMusl64 = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages pkgs.pkgsCross.musl64.cardanoMetadataSubmitterPackages);

  metadataSubmitterTarball = pkgs.runCommandNoCC "metadata-submitter-tarball" { buildInputs = [ pkgs.gnutar gzip ]; } ''
    cp ${haskellPackagesMusl64.cardano-metadata-submitter.components.exes.cardano-metadata-submitter}/bin/cardano-metadata-submitter ./
    mkdir -p $out/nix-support
    tar -czvf $out/cardano-metadata-submitter.tar.gz cardano-metadata-submitter
    echo "file binary-dist $out/cardano-metadata-submitter.tar.gz" > $out/nix-support/hydra-build-products
  '';

  self = {
    inherit haskellPackages check-hydra metadataSubmitterTarball;

    inherit (haskellPackages.cardano-metadata-submitter.identifier) version;
    # Grab the executable component of our package.
    inherit (haskellPackages.cardano-metadata-submitter.components.exes)
      cardano-metadata-submitter;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
      # Example of a linting script used by Buildkite.
      lint-fuzz = callPackage ./nix/check-lint-fuzz.nix {};
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };

    # Attrset of PDF builds of LaTeX documentation.
    docs = pkgs.callPackage ./docs/default.nix {};
  };
in
  self
