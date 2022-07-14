with import (builtins.fetchGit {
  name = "nixos-release-22.05";
  url = https://github.com/nixos/nixpkgs/;
  ref = "release-22.05";
}) {};

stdenv.mkDerivation {
  name = "hedgehog-golden";

  buildInputs = with haskell.packages.ghc8107; [
    cabal-install
    ghc
    ghcid
    libiconv
  ];
}
