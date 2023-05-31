let
  pkgs = import ./nix/nixpkgs-pinned.nix { };
  # Needed for the locale-archive.
in pkgs.glibcLocales
