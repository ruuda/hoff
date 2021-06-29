let
  pkgs = import ./nixpkgs-pinned.nix {};
in
  # Needed for the locale-archive.
  pkgs.glibcLocales
