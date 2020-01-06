let
  pkgs = import (import ./nixpkgs-pinned.nix) {};
in
  pkgs.buildEnv {
    name = "hoff-devenv";
    paths = [
      pkgs.dpkg
      pkgs.fakeroot
      pkgs.git
      pkgs.stack
    ];
  }
