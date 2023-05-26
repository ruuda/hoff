#!/bin/sh

# This script invokes Cabal to do a release build. It is intended to be run on
# on the same system that packages the binaries, right before build-package.sh.

# Fail early if any of the commands below fail.
set -e

# When running in a virtual machine, if we inherited a dirty working directory
# from the host machine, be sure to clean it.
cabal clean

# Build with --split-objs to produce a smaller binary, and enable optimizations.
cabal build -j --enable-split-sections  --enable-optimization=2

# Finally run the tests to make sure that everything still works.
cabal test -j --enable-split-sections --enable-optimization=2
