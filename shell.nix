{ pkgs ? import ./nixpkgs.nix }:

pkgs.haskellPackages.shellFor {
  packages = p: [ (pkgs.callPackage ./default.nix { }) ];
  buildInputs = [
    pkgs.cabal-install
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    pkgs.libiconv
    pkgs.ormolu
    pkgs.ncurses
    pkgs.stack
    pkgs.zlib
  ];

  HEDGEHOG_COLOR = 1;
}
