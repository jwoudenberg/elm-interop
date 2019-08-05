{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hindent
    pkgs.libiconv
    pkgs.ncurses
    pkgs.stack
    pkgs.zlib
  ];
}
