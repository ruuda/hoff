let
  pkgs = import ./nixpkgs-pinned.nix {};
in
  pkgs.buildEnv {
    name = "hoff-devenv";
    paths = [
      pkgs.dia
      pkgs.dpkg
      pkgs.git
      pkgs.haskellPackages.haskell-language-server
      pkgs.niv
      pkgs.shellcheck
      pkgs.stack
    ];
  }
