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
    buildPhase = builtins.concatStringsSep " " [
      "stack build"
      "--nix"
      "--stack-root $out"
      "--only-snapshot"
      "--test"
      # NOTE: split-objs is essential here. It's a poor man's dead code
      # elimination, but in particular it ensures that some string constants in
      # one library (I suspect Warp) do not end up in the .rodata section of the
      # final binary, because they are not referenced. Without split-objs, the
      # Nix store path of this derivation ends up in the binary, and then Nix
      # considers the entire Stackage snapshot here part of the closure.
      "--split-objs"
    ];
  };

  hoff = stdenv.mkDerivation {
    name = "hoff";
    buildInputs = [ git stack nix hoffDeps ];
    phases = [ "unpackPhase" "buildPhase" "checkPhase" "installPhase" ];
    doCheck = true;
    unpackPhase = ''
      cp ${./hoff.cabal} hoff.cabal
      cp ${./license} license
      cp ${./nixpkgs-pinned.nix} nixpkgs-pinned.nix
      cp ${./nixpkgs} nixpkgs
      cp ${./readme.md} readme.md
      cp ${./shell.nix} shell.nix
      cp ${./stack.yaml} stack.yaml
      cp -r ${./app} app
      cp -r ${./package} package
      cp -r ${./src} src
      cp -r ${./static} static
      cp -r ${./tests} tests
    '';
    buildPhase = builtins.concatStringsSep " " [
      "stack build"
      "--nix"
      "--stack-root ${hoffDeps}"
      # See also the note about --split-objs in hoffDeps.
      "--split-objs"
    ];
    checkPhase = builtins.concatStringsSep " " [
      "stack test"
      "--nix"
      "--stack-root ${hoffDeps}"
      "--split-objs"
    ];
    installPhase = builtins.concatStringsSep " " [
      "mkdir -p $out/bin"
      "\n"
      "stack install"
      "--nix"
      "--stack-root ${hoffDeps}"
      "--local-bin-path $out/bin"
      "--split-objs"
    ];
  };

  # Put together the filesystem by copying from and symlinking to the Nix store.
  # We need to do this, because unfortunately, "mksquashfs /foo/bar" will create
  # a file system with bar in the root. So we cannot pass absolute paths to the
  # store. To work around this, copy all of them, so we can run mksquashfs on
  # the properly prepared directory. Then for symlinks, they are copied
  # verbatim, with the path inside the $out directory. So these we symlink
  # directly to the store, not to the copies in $out. So in the resulting image,
  # those links will point to the right places.
  imageDir = stdenv.mkDerivation {
    name = "hoff-filesystem";
    buildInputs = [ hoff git ];
    buildCommand = ''
      # Although we only need /nix/store and /usr/bin, we need to create the
      # other directories too so systemd can mount the API virtual filesystems
      # there, when the image is used. For /var, for systemd-nspawn only /var is
      # sufficient, but in a unit with PrivateTmp=true, we also need /var/tmp,
      # because systemd mounts a tmpfs there. /run is not needed by the systemd
      # unit, but it is required by systemd-nspawn, so we add it too.
      mkdir -p $out/dev
      mkdir -p $out/etc/hoff
      mkdir -p $out/nix/store
      mkdir -p $out/proc
      mkdir -p $out/run
      mkdir -p $out/sys
      mkdir -p $out/tmp
      mkdir -p $out/usr/bin
      mkdir -p $out/var/cache/hoff
      mkdir -p $out/var/hoff
      mkdir -p $out/var/tmp
      ln -s /usr/bin $out/bin
      ln -s ${hoff}/bin/hoff $out/usr/bin/hoff
      ln -s ${git}/bin/git $out/usr/bin/git
      closureInfo=${closureInfo { rootPaths = [ hoff git ]; }}
      for file in $(cat $closureInfo/store-paths); do
        echo "copying $file"
        cp --archive $file $out/nix/store
      done
    '';
  };

in
  imageDir
  # hoff
