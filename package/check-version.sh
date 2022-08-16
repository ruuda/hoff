#!/bin/bash
#
# Checks if the version number is consistent across different files:
# * hoff.cabal
# * hoff.nix
# * src/Version.hs
#
# This is intended to be called on CI (Semaphore).
#
# We could overengineer this by:
#
# * Including the check in test/Spec.hs or test/*.hs;
#
# * using Cabal-syntax's GenericPackageDescription function
#   to "properly" parse the version in the cabal file;
#
# * similarly "properly" parsing hoff.nix thorugh Haskell.
#
# This is simpler, easier and effective.

cabal_version=$(
	grep "^version:" <hoff.cabal |
	sed 's/.*version: *//;  s/ .*//'
)
source_version=$(
	grep "^version *=" <src/Version.hs |
	sed 's/.*version *= *"//; s/".*//'
)
nix_version=$(
	grep "^[\t ]*version *=" <hoff.nix |
	sed 's/.*version *= *"//; s/".*//'
)

[ "$cabal_version" = "$source_version" ] && [ "$nix_version" = "$source_version" ] && exit 0

cat <<MESSAGE
mismatch in versions in hoff.cabal, hoff.nix and src/Version.hs:
* hoff.cabal:     $cabal_version
* hoff.nix:       $nix_version
* src/Version.hs: $source_version
MESSAGE
exit 1
