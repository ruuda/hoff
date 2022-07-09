let
  pkgs = import ../nixpkgs-pinned.nix {};

  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.containers
    p.unordered-containers
  ]);
in
  pkgs.buildEnv {
    name = "hoff-tools-devenv";
    paths = [ pkgs.glibcLocales ghc ];
  }
