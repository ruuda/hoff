# This nix expression is used by stack for setting up the build environment.
# It referenced in `stack.yaml` so that stack uses it by default.
let
    nixpkgs = import ./nixpkgs-pinned.nix {};
    mkDependencies = import ./nix/haskell-dependencies.nix;
in
    nixpkgs.haskell.lib.buildStackProject {
        name = "sharkmachine";
        # Not the GHC given by stack!
        ghc = nixpkgs.haskellPackages.ghcWithHoogle mkDependencies;
        buildInputs = with nixpkgs; [
            git glibcLocales
        ];
    }
