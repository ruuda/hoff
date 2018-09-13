{ghc}:

with (import (import ./nixpkgs-pinned.nix) {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ git zlib ];
}
