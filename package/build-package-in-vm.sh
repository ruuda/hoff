#!/bin/sh

# This script automates building a .deb package in a Debian-like virtual
# machine managed by Vagrant.
#
#   Usage: VERSION=0.0.0 ./build-package-in-vm.sh

# Fail early of any of the commands below fail.
set -e

if [ -z "$VERSION" ]; then
  echo '$VERSION must be set to build a package.'
  exit -1
fi

PKGFNAME="hoff_$VERSION-1.deb"

# Boot a virtual machine, optionally initializing it if it hasn't been used
# before.
vagrant up

# Inside the virtual machine, build the binary and then the package. The current
# repository is mounted at /vagrant in the virtual machine.
vagrant ssh -c "cd /vagrant/package && ./build-binary.sh"
vagrant ssh -c "cd /vagrant/package && VERSION=$VERSION ./build-package.sh"

# Pull the produced package from the virtual machine by catting it through ssh.
# Only using cat caused corrupted files, likely due to encoding issues, and
# transferring the data as base64 fixes that.
vagrant ssh -c "cat /vagrant/package/$PKGFNAME | base64" | base64 --decode --ignore-garbage > $PKGFNAME

# Power off the virtual machine, but do not destroy it. When using it next time
# it will have the build tools and dependencies installed already, which will
# speed up things a lot.
vagrant halt
