# Extract our Haskell dependencies from the set of all haskell packages in
# nixpkgs, and add the external packages that are not included in nixpkgs.
# Whenever a new dependency is introduced, it has to be  added to the list
# below.
#
# `haskellPackages` is an attribute set containing all available Haskell
# packages in the nixpkgs instantiation.
haskellPackages: with haskellPackages; [
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
]
