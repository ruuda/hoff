let
  pkgs = import (import ./nixpkgs-pinned.nix) {};
in
  pkgs.buildEnv {
    name = "hoff-devenv";
    paths = [
      pkgs.dpkg
      pkgs.git
      pkgs.shellcheck
      pkgs.stack
    ];
  }
