#!/bin/bash
#
# Checks if the version number in hoff.cabal is the same as src/Version.hs

cabal_version=$( grep "^version:"   <hoff.cabal     | sed 's/.*version: *//;  s/ .*//')
source_version=$(grep "^version *=" <src/Version.hs | sed 's/.*version *= *"//; s/".*//')

[ "$cabal_version" = "$source_version" ] && exit 0

cat <<MESSAGE
mismatch in versions in hoff.cabal and src/Version.hs:
* hoff.cabal:     $cabal_version
* src/Version.hs: $source_version
MESSAGE
exit 1
