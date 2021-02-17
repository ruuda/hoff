#!/bin/bash

# Entrypoint for building a package on Semaphore CI and shipping to our apt
# file host. Only builds a package when the current build is a git tag.

set -efuo pipefail

if [[ "$SEMAPHORE" != "true" ]]; then
    echo "Run this script from a Semaphore job"
    exit 1
fi

# Get the version from the git history. Strip the first `v`
# character as dpkg really wants versions to start with a digit.
VERSION="$(git describe | cut -c2-)"
export VERSION

# Change to the directory of the current script so that we can
# execute `build-package.sh` from the right location. VERSION
# has already been set by the logic above.
cd "$(dirname "$0")"
fakeroot ./build-package.sh

if [[ "$SEMAPHORE_GIT_REF_TYPE" != "tag" ]]; then
    echo "Not on a tagged build. Skipping ship step."
    exit 0
fi

PKGFILE="hoff_$VERSION-1.deb"
FREIGHT_HOST="freight@archive-external.channable.com"

scp "$PKGFILE" "$FREIGHT_HOST:/tmp/$PKGFILE"

# Shellcheck false positive. We want the client side versions
# of these variables, not the server side versions.
# shellcheck disable=SC2087
ssh -T "$FREIGHT_HOST" <<END
freight add "/tmp/${PKGFILE}" apt/xenial apt/bionic
freight-cache
rm /tmp/${PKGFILE}
END
