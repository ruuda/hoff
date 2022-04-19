let
  pkgs = import ./nixpkgs-pinned.nix {};
  hoff = pkgs.callPackage ./hoff.nix {};
in
  hoff
