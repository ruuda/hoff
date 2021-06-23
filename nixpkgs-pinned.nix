let
  sources = import ./nix/sources.nix;
  our_niv = _: pkgs: { niv = (import sources.niv {}).niv; }; # use niv from sources
  nixpkgs = import sources.nixpkgs {
    overlays = [ our_niv ] ;
  };
in
nixpkgs
