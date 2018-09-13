{ pkgs ?
  # Default to a pinned version of Nixpkgs. The actual revision of the Nixpkgs
  # repository is stored in a separate file (as a fetchTarball Nix expression).
  # We then fetch that revision from Github and import it. The revision should
  # periodically be updated to be the last commit of Nixpkgs.
  import (import ./nixpkgs-pinned.nix) {}
}:

with pkgs;

let
  gitignore = import (fetchTarball {
    url = "https://github.com/siers/nix-gitignore/archive/6f5b5d4c30e76bd9c013820f54244baeed6ad7dc.tar.gz";
    sha256 = "0dnwv3yfmfncabqgspin1dshiaykbqh3iymn7y6d048fmlkdf272";
  }) { inherit lib; };

  hoff = haskell.lib.buildStackProject {
    name = "hoff";
    src = gitignore.gitignoreSource ./.;
    buildInputs = [ git ];
  };

in
  hoff
