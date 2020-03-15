let
  # This comes from https://nixos.org/channels/
  #
  # Pick a release (e.g. nixpkgs-unstable) and open the `git-revision`
  # file. It will contain a revision hash. Copy and paste it below.
  rev = "13e7a3e11272159b9b1fc41ec67f53c1088412ff";
  # Generate the SHA256 hash for this revision's tarball.
  #
  #   $ nix-prefetch-url --unpack --type sha256 \
  #   >   https://github.com/NixOS/nixpkgs/archive/${rev-defined-above}.tar.gz
  sha256 = "1d6byw99i4wjwdz501r6b12i8nifwl86gjd43cjvg8f2d78fazpg";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = sha256;
  };
in import nixpkgs { }
