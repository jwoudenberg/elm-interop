let
  pkgsPath = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/bf1b50cbc8ffe9747758d089e3148406a7ce5c21.tar.gz";
    sha256 = "0clczc8n7415i7pcqs1my8ydf0sijkcwqw6c36dgn998kdrgknh8";
  };
  pkgs = import pkgsPath { config = {}; };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.libiconv
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hlint
    pkgs.haskellPackages.hindent
  ];
}
