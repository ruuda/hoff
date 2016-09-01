#!/bin/sh

# This script builds a .deb package from the binaries in the .stack-work
# directory. It is intended to be run on a Debian-like system.
#
#   Usage: VERSION=0.0.0 ./build-package.sh

# Fail early of any of the commands below fail.
set -e

if [ -z "$VERSION" ]; then
  echo '$VERSION must be set to build a package.'
  exit -1
fi

# Set package filename.
PKGNAME="hoff_$VERSION-1"

# Recreate the file system layout as it should be on the target machine.
mkdir -p "$PKGNAME/DEBIAN"
mkdir -p "$PKGNAME/usr/bin"
mkdir -p "$PKGNAME/lib/systemd/system"
cp "$(stack path --local-install-root)/bin/hoff" "$PKGNAME/usr/bin/"
cp hoff.service "$PKGNAME/lib/systemd/system"

# Write the package metadata file, substituting environment variables in the
# template file.
cat deb-control | envsubst > "$PKGNAME/DEBIAN/control"
cp deb-postinst "$PKGNAME/DEBIAN/postinst"
chmod +x "$PKGNAME/DEBIAN/postinst"

dpkg-deb --build "$PKGNAME"

# Finally clean up the package directory.
rm -fr "$PKGNAME"
