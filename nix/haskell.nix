############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################

{ lib
, stdenv
, haskell-nix
, buildPackages
# Pass in any extra programs necessary for the build as function arguments.
# TODO: Declare packages required by the build.
, makeWrapper

, config ? {}
# GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc8102"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
}:

let
  src = haskell-nix.haskellLib.cleanGit {
    name = "cardano-metadata-submitter";
    src = ../.;
  };

  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject {
      inherit src;
      compiler-nix-name = compiler;
    }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject  {
    inherit src;
    compiler-nix-name = compiler;

    # these extras will provide additional packages
    # ontop of the package set derived from cabal resolution.
    pkg-def-extras = [(hackage: {
      packages = {
          # Win32 = hackage.Win32."2.8.3.0".revisions.default;
      };
    })];

    modules = [
      {
        packages.cardano-metadata-submitter.configureFlags = [ "--ghc-option=-Werror" ];
        enableLibraryProfiling = profiling;
      }

      # Add dependencies
      {
        packages.cardano-metadata-submitter = {
          # components.tests.unit.build-tools = [ ];

          # How to set environment variables for builds
          #preBuild = "export NETWORK=testnet";

          # How to add program depdendencies for benchmarks
          # TODO: remove if not applicable
          # components.benchmarks.cardano-metadata-submitter-bench = {
          #   build-tools = [ makeWrapper ];
          #   postInstall = ''
          #     makeWrapper \
          #       $out/bin/cardano-metadata-submitter-bench \
          #       $out/bin/cardano-metadata-submitter-bench-wrapped \
          #       --prefix PATH : ${cowsay}/bin
          #   '';
          # };

        };
      }

      # Misc. build fixes for dependencies
      {
        # Cut down iohk-monitoring deps
        # Note that this reflects flags set in stack.yaml.
        packages.iohk-monitoring.flags = {
          disable-ekg = true;
          disable-examples = true;
          disable-graylog = true;
          disable-gui = true;
          disable-prometheus = true;
          disable-systemd = true;
        };

        # Katip has Win32 (>=2.3 && <2.6) constraint
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;

        # some packages are missing identifier.name:
        packages.Win32.package.identifier.name = "Win32";
        packages.cryptonite-openssl.package.identifier.name = "cryptonite-openssl";
        packages.file-embed-lzma.package.identifier.name = "file-embed-lzma";
        packages.singletons.package.identifier.name = "singletons";
        packages.terminfo.package.identifier.name = "terminfo";
        packages.conduit.package.identifier.name = "conduit";
        packages.ekg.package.identifier.name = "ekg";
        packages.iohk-monitoring.package.identifier.name = "iohk-monitoring";
      }

      (lib.optionalAttrs stdenv.hostPlatform.isWindows {
        # Disable cabal-doctest tests by turning off custom setups
        packages.comonad.package.buildType = lib.mkForce "Simple";
        packages.distributive.package.buildType = lib.mkForce "Simple";
        packages.lens.package.buildType = lib.mkForce "Simple";
        packages.nonempty-vector.package.buildType = lib.mkForce "Simple";
        packages.semigroupoids.package.buildType = lib.mkForce "Simple";

        # Make sure we use a buildPackages version of happy
        packages.pretty-show.components.library.build-tools = [ buildPackages.haskell-nix.haskellPackages.happy ];

        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];
      })
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
        # systemd can't be statically linked
        packages.cardano-config.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
        packages.cardano-node.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
      })
      # Musl libc fully static build
      (lib.optionalAttrs stdenv.hostPlatform.isMusl (let
        # Module options which adds GHC flags and libraries for a fully static build
        fullyStaticOptions = {
          enableShared = false;
          enableStatic = true;
        };
      in
        {
          packages = lib.genAttrs projectPackages (name: fullyStaticOptions);

          # Haddock not working and not needed for cross builds
          doHaddock = false;
        }
      ))

      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];
      })
    ];
  };

in
  pkgSet
