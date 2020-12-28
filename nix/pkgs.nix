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
}
