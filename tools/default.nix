let
  pkgs = import ./nixpkgs-pinned.nix {};

  python = pkgs.python310.withPackages (p: [
    p.numpy
    p.matplotlib
  ]);
in
  pkgs.buildEnv {
    name = "hoff-tools-devenv";
    paths = [ pkgs.glibcLocales python ];
  }
