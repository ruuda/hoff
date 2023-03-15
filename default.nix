# Optionally put the specified nix version of the package in the environment
{ environment ? "default"
}:
let
  pkgs = import ./nixpkgs-pinned.nix {};

  defaultEnv = pkgs.buildEnv {
    name = "hoff-devenv";
    paths = [
      pkgs.dia
      pkgs.dpkg
      pkgs.git
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.stylish-haskell
      pkgs.niv
      pkgs.shellcheck
      pkgs.stack
    ];
  };

  environments = {
    default = defaultEnv;
    shell = pkgs.mkShell { packages = [defaultEnv]; };
  };
in
  environments."${environment}"
