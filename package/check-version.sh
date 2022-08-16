#!/bin/bash
#
# Checks if the version number is consistent between hoff.cabal and hoff.nix
cabal_version=$(
	grep "^version:" <hoff.cabal |
	sed 's/.*version: *//;  s/ .*//'
)
nix_version=$(
	grep "^[\t ]*version *=" <hoff.nix |
	sed 's/.*version *= *"//; s/".*//'
)

[ "$cabal_version" = "$nix_version" ] && exit 0

cat <<MESSAGE
mismatch in versions between hoff.cabal and hoff.nix:
* hoff.cabal:     $cabal_version
* hoff.nix:       $nix_version
MESSAGE
exit 1
