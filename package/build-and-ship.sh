#!/bin/bash

# Build Hoff and ship it to Freight

set -efuo pipefail

# Get the version from the git history. Strip the first `v`
# character as dpkg really wants versions to start with a digit.
VERSION="$(git describe | cut -c2-)"
export VERSION

# Change to the directory of the current script so that we can
# execute `build-package.sh` from the right location. VERSION
# has already been set by the logic above.
cd "$(dirname "$0")"
./build-package.sh

PKGFILE="hoff_$VERSION-1.deb"
FREIGHT_HOST="archive-petrol"

gcloud compute scp --tunnel-through-iap "$PKGFILE" "$FREIGHT_HOST:/tmp/$PKGFILE"

# Shellcheck false positive. We want the client side versions
# of these variables, not the server side versions.
# shellcheck disable=SC2087
gcloud compute ssh --tunnel-through-iap "$FREIGHT_HOST" -- -T <<END
sudo -iu freight freight add "/tmp/${PKGFILE}" apt/bionic apt/focal
sudo -iu freight freight-cache
rm /tmp/${PKGFILE}
END
