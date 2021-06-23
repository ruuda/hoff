let
  pkgs = import (import ./nixpkgs-pinned.nix) {};
in
  pkgs.buildEnv {
    name = "hoff-devenv";
    paths = [
      pkgs.dpkg
      # Use `fakeroot` from the OS
      # The version shipped with nix might not be compatible with system's libs
      # and cause segfaults
      # pkgs.fakeroot
      pkgs.git
      pkgs.shellcheck
      pkgs.stack
    ];
  }
