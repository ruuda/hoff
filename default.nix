let
  nixpkgs = import ./nixpkgs-pinned.nix {};
  hoff = nixpkgs.callPackage ./hoff.nix {};
in
  hoff
