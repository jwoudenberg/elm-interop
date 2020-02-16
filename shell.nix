{ pkgs ? import <nixpkgs> { } }:

let
  ormolu = pkgs.haskellPackages.callCabal2nix "ormolu" (pkgs.fetchFromGitHub {
    owner = "tweag";
    repo = "ormolu";
    rev = "3abadaefa5e190ff346f9aeb309465ac890495c2";
    sha256 = "0vqrb12bsp1dczff3i5pajzhjwz035rxg8vznrgj5p6j7mb2vcnd";
  }) { };
in pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hindent
    pkgs.libiconv
    pkgs.ncurses
    pkgs.stack
    pkgs.zlib
    ormolu
  ];
}
