#!/bin/bash

# This script builds a .deb package from the binaries in the .stack-work
# directory. It is intended to be run on a Debian-like system.
#
#   Usage: VERSION=0.0.0 ./build-package.sh

# Fail early of any of the commands below fail.
set -eo pipefail

if [ -z "$VERSION" ]; then
  echo 'VERSION must be set to build a package.'
  exit 1
fi

# build the package with Stack
./build-binary

# Set package filename.
PKGNAME="hoff_$VERSION-1"

# Recreate the file system layout as it should be on the target machine.
mkdir -p "$PKGNAME/DEBIAN"
mkdir -p "$PKGNAME/etc/hoff"
mkdir -p "$PKGNAME/lib/systemd/system"
mkdir -p "$PKGNAME/usr/bin"
cp "$(stack path --local-install-root)/bin/hoff" "$PKGNAME/usr/bin/"
cp hoff.service        "$PKGNAME/lib/systemd/system"
cp example-config.json "$PKGNAME/etc/hoff/config.json"
cp github-known-hosts  "$PKGNAME/etc/hoff/github-known-hosts"

# All files will be owned by root (see 'dpkg-deb' call below). The config file is world-readable, because the
# daemon user needs to be able to read it.
chmod o+r       "$PKGNAME/etc/hoff/config.json"

# Hash the config file. The postinst script uses this to determine whether it
# needs to inform the user to update the config, after a new install.
EXAMPLE_CONFIG_SHA256=$(sha256sum example-config.json | cut --characters=1-64)
export EXAMPLE_CONFIG_SHA256

# Write the package metadata file, substituting environment variables in the
# template file.
envsubst < deb-control  > "$PKGNAME/DEBIAN/control"
envsubst < deb-postinst > "$PKGNAME/DEBIAN/postinst"
cp deb-conffiles "$PKGNAME/DEBIAN/conffiles"
chmod +x "$PKGNAME/DEBIAN/postinst"

dpkg-deb --root-owner-group --build "$PKGNAME"

# Finally clean up the package directory.
rm -fr "$PKGNAME"
