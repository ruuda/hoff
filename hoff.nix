{ lib, haskellPackages, nix-gitignore, git, coreutils, openssh, glibcLocales, makeWrapper }:
let
  haskellLibs = import ./nix/haskell-dependencies.nix haskellPackages;
in
  haskellPackages.mkDerivation {
    pname = "hoff";
    version = "0.28.2"; # please keep consistent with hoff.cabal

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

    buildTools = [ makeWrapper ];

    postInstall = ''
      # Set LOCALE_ARCHIVE so that glibc can find the locales it needs when running on Ubuntu
      # machines.
      wrapProgram $out/bin/hoff --set LOCALE_ARCHIVE ${glibcLocales}/lib/locale/locale-archive
    '';

    isLibrary = false;
    isExecutable = true;


    executableToolDepends = [
      git coreutils openssh
    ];

    testDepends = [
      git coreutils openssh
    ];


    libraryHaskellDepends = haskellLibs;

    homepage = "https://github.com/channable/hoff";

    license = lib.licenses.asl20;
  }
