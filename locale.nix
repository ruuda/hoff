let
  pkgs = import ./nix/nixpkgs-pinned.nix {};
in
  # Needed for the locale-archive.
  pkgs.glibcLocales
