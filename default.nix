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

  # Make an override of the Git package in Nixpkgs without bells and whistles,
  # to cut down on the closure size. These features are nice for interactive
  # use, but not needed for Hoff which only scripts against the parts of Git
  # that don't need e.g. Perl or Python support.
  gitMinimal = git.override {
    svnSupport = false;
    perlSupport = false;
    guiSupport = false;
    withManual = false;
    pythonSupport = false;
    withpcre2 = false;
  };

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
      mkdir package
      cp ${./hoff.cabal} hoff.cabal
      cp ${./license} license
      cp ${./nixpkgs-pinned.nix} nixpkgs-pinned.nix
      cp ${./nixpkgs} nixpkgs
      cp ${./package/example-config.json} package/example-config.json
      cp ${./readme.md} readme.md
      cp ${./shell.nix} shell.nix
      cp ${./stack.yaml} stack.yaml
      cp -r ${./app} app
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
    buildInputs = [ hoff gitMinimal ];
    buildCommand = ''
      # Although we only need /nix/store and /usr/bin, we need to create the
      # other directories too so systemd can mount the API virtual filesystems
      # there, when the image is used. For /var, for systemd-nspawn only /var is
      # sufficient, but in a unit with PrivateTmp=true, we also need /var/tmp,
      # because systemd mounts a tmpfs there. /run is not needed by the systemd
      # unit, but it is required by systemd-nspawn, so we add it too. And
      # finally, systemd wants to create symlinks in /lib for some reason and
      # that fails on a readonly filesystem.
      mkdir -p $out/dev
      mkdir -p $out/etc/hoff
      mkdir -p $out/lib
      mkdir -p $out/nix/store
      mkdir -p $out/proc
      mkdir -p $out/run
      mkdir -p $out/sys
      mkdir -p $out/tmp
      mkdir -p $out/usr/bin
      mkdir -p $out/usr/lib/systemd/system
      mkdir -p $out/var/cache/hoff
      mkdir -p $out/var/lib/hoff
      mkdir -p $out/var/tmp

      # Make a localtime so that systemd-nspawn can bind-mount the host
      # localtime over it, mainly to suppress the warning.
      touch $out/etc/localtime

      ln -s /usr/bin $out/bin
      ln -s ${coreutils}/bin/true  $out/usr/bin/true
      ln -s ${gitMinimal}/bin/git  $out/usr/bin/git
      ln -s ${hoff}/bin/hoff       $out/usr/bin/hoff
      ln -s ${openssh}/bin/ssh     $out/usr/bin/ssh

      cp ${./package/hoff.service} $out/usr/lib/systemd/system/hoff.service
      cp ${./package/os-release}   $out/usr/lib/os-release

      closureInfo=${closureInfo { rootPaths = [ hoff gitMinimal ]; }}
      for file in $(cat $closureInfo/store-paths); do
        echo "copying $file"
        cp --archive $file $out/nix/store
      done
    '';
  };

in
  stdenv.mkDerivation {
    name = "hoff.img";

    nativeBuildInputs = [ squashfsTools ];
    buildInputs = [ imageDir ];

    buildCommand =
      ''
        # Generate the squashfs image. Pass the -no-fragments option to make
        # the build reproducible; apparently splitting fragments is a
        # nondeterministic multithreaded process. Also set processors to 1 for
        # the same reason.
        mksquashfs ${imageDir} $out \
          -no-fragments      \
          -processors 1      \
          -all-root          \
          -b 1048576         \
          -comp xz           \
          -Xdict-size 100%   \
      '';
  }
