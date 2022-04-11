{ lib, haskellPackages, nix-gitignore, git, coreutils, openssh }:
  haskellPackages.mkDerivation {
    pname = "hoff";
    version = "0.25";

    src =
      let
        # We do not want to include all files, because that leads to a lot of things that nix
        # has to copy to the temporary build directory that we don't want to have in there
        # (e.g. the `.stack-work` directory, the `.git` directory, etc.)
        prefixWhitelist = builtins.map builtins.toString [
          ./app
          ./package
          ./src
          ./static
          ./tests
          ./tools
          ./hoff.cabal
          ./stack.yaml
          ./license
        ];
        # Compute source based on whitelist
        whitelistFilter = path: _type: lib.any (prefix: lib.hasPrefix prefix path) prefixWhitelist;
        gitignore = builtins.readFile ./.gitignore;
        gitignoreFilter = nix-gitignore.gitignoreFilterPure whitelistFilter gitignore ./.;
        whitelistedSrc = lib.cleanSourceWith {
          src = lib.cleanSource ./.;
          filter = gitignoreFilter;
        };
      in
        whitelistedSrc;

    isLibrary = false;
    isExecutable = true;


    executableToolDepends = [
      git coreutils openssh
    ];

    testDepends = [
      git coreutils openssh
    ];


    libraryHaskellDepends =
      with haskellPackages; [
        aeson
        aeson-pretty
        blaze-html
        blaze-markup
        bytestring
        containers
        cryptonite
        directory
        extra
        file-embed
        filepath
        free
        github
        hspec
        hspec-core
        http-client
        http-conduit
        http-types
        memory
        monad-logger
        optparse-applicative
        process
        process-extras
        scotty
        stm
        text
        text-format
        time
        uuid
        vector
        wai
        warp
        warp-tls
      ];

    homepage = "https://github.com/channable/hoff";

    license = ./license;
  }
