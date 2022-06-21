let
  pkgs = import ./nixpkgs-pinned.nix {};
in
  pkgs.buildEnv {
    name = "hoff-devenv";
    paths = [
      pkgs.niv
      pkgs.dpkg
      pkgs.git
      pkgs.shellcheck
      pkgs.stack
      pkgs.haskellPackages.haskell-language-server
    ];
  }
