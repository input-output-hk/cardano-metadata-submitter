pkgs: _: with pkgs; {
  cardanoMetadataSubmitterPackages = import ./haskell.nix {
    inherit
      config
      lib
      stdenv
      haskell-nix
      buildPackages
      makeWrapper;
  };
  inherit (cardanoMetadataSubmitterPackages.cardano-metadata-submitter.components.exes) cardano-metadata-submitter;
}
