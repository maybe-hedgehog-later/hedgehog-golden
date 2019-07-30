with import (builtins.fetchGit {
  name = "nixos-release-19.03";
  url = https://github.com/nixos/nixpkgs/;
  ref = "release-19.03";
  rev = "caacbe98ce1d9cb6c8aab9d5f210dab6bb689c8a";
}) {};

stdenv.mkDerivation {
  name = "hedgehog-golden";

  shellHook = ''
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
  '';

  buildInputs = with haskell.packages.ghc864; [
    cabal-install
    hdevtools
    hoogle
    hlint
    ghcid
    ghc
    libiconv
  ];
}
