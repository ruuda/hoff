{ overlays ? [] # additional overlays
, config ? {} # Imported configuration
}:
let
  sources = import ./nix/sources.nix;
  our_niv = _: pkgs: { niv = (import sources.niv {}).niv; }; # use niv from sources
  nixpkgs = import sources.nixpkgs {
    overlays = [ our_niv ] ++ overlays ;
    config = {
      imports = [
        config
      ];
    };
  };
in
nixpkgs
