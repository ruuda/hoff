let
  pkgs = import ./nix/nixpkgs-pinned.nix {};
in
  pkgs.haskellPackages.hoff
