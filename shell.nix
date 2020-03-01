{ }:

let
  pkgsPath = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = sha256;
  };

  pkgs = import pkgsPath { };

  rev = "e89b21504f3e61e535229afa0b121defb52d2a50";

  sha256 = "0jqcv3rfki3mwda00g66d27k6q2y7ca5mslrnshfpbdm7j8ya0kj";

  ormolu = pkgs.haskellPackages.callCabal2nix "ormolu" (pkgs.fetchFromGitHub {
    owner = "tweag";
    repo = "ormolu";
    rev = "3abadaefa5e190ff346f9aeb309465ac890495c2";
    sha256 = "0vqrb12bsp1dczff3i5pajzhjwz035rxg8vznrgj5p6j7mb2vcnd";
  }) { };
in pkgs.mkShell {
  buildInputs = [
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.haskellPackages.ghcid
    pkgs.libiconv
    pkgs.ncurses
    pkgs.stack
    pkgs.zlib
    ormolu
  ];
}
