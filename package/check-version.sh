#!/bin/bash
#
# Checks if the version number in hoff.cabal is the same as src/Version.hs
#
# This is intended to be called on Semaphore.
#
# One alternative to this scripts would be to add this test directly on
# test/*.hs while imposing a dependency on Cabal-syntax there.
# Then we could "properly" parse the Cabal file to get the version:
#
# https://hackage.haskell.org/package/Cabal-syntax-3.8.1.0/docs/Distribution-Types-GenericPackageDescription.html#t:GenericPackageDescription
#
# But for a simple check like this, that may be overengineering.

cabal_version=$( grep "^version:"   <hoff.cabal     | sed 's/.*version: *//;  s/ .*//')
source_version=$(grep "^version *=" <src/Version.hs | sed 's/.*version *= *"//; s/".*//')

[ "$cabal_version" = "$source_version" ] && exit 0

cat <<MESSAGE
mismatch in versions in hoff.cabal and src/Version.hs:
* hoff.cabal:     $cabal_version
* src/Version.hs: $source_version
MESSAGE
exit 1
