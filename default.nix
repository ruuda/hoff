let
  pkgs = import (import ./nixpkgs-pinned.nix) {};

  # A minimal version of Git. Hoff should need nothing more, in particular
  # it should not need Perl support.
  gitMinimal = pkgs.git.override {
    svnSupport = false;
    perlSupport = false;
    guiSupport = false;
    withManual = false;
    pythonSupport = false;
    withpcre2 = false;
  };
in
  pkgs.buildEnv {
    name = "hoff-devenv";
    # TODO: Use gitMinimal one https://github.com/NixOS/nixpkgs/pull/74213 lands
    # on nixpkgs-unstable. (It is in staging at the time of writing.)
    paths = [ pkgs.git pkgs.stack ];
  }
