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

  # The Stackage snapshot that Hoff depends on, precompiled.
  hoffDeps = stdenv.mkDerivation {
    name = "hoff-deps";
    buildInputs = [ stack nix ];
    phases = [ "unpackPhase" "buildPhase" ];
    # Take only the input files we need to get Stack going, and those that Stack
    # needs to determine what to build from the snapshot. This target needs to
    # be rebuilt every time one of the inputs changes, so we'd like to avoid
    # rebuilding as much as possible.
    unpackPhase = ''
      cp ${./hoff.cabal} hoff.cabal
      cp ${./nixpkgs-pinned.nix} nixpkgs-pinned.nix
      cp ${./nixpkgs} nixpkgs
      cp ${./shell.nix} shell.nix
      cp ${./stack.yaml} stack.yaml
    '';
    buildPhase = ''
      stack build --nix --stack-root $out --only-snapshot --test
    '';
  };

  hoff = stdenv.mkDerivation {
    name = "hoff";
    src = gitignore.gitignoreSource ./.;
    buildInputs = [ git stack nix hoffDeps ];
    phases = [ "unpackPhase" "buildPhase" "checkPhase" "installPhase" ];
    doCheck = true;
    buildPhase = ''
      stack build --nix --stack-root ${hoffDeps}
    '';
    checkPhase = ''
      stack test --nix --stack-root ${hoffDeps}
    '';
    installPhase = ''
      mkdir -p $out/bin
      stack install --nix --stack-root ${hoffDeps} --local-bin-path $out/bin
    '';
  };

in
  hoff
