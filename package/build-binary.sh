#!/bin/sh

# This script invokes Stack to do a release build. It is intended to be run on
# on the same system that packages the binaries, right before build-package.sh.

# Fail early if any of the commands below fail.
set -e

# Download the right version of GHC.
stack setup

# When running in a virtual machine, if we inherited a dirty Stack working
# directory from the host machine, be sure to clean it.
stack clean

# Build with --split-objs to produce a smaller binary, and enable optimizations.
stack build --split-objs --ghc-options -O2

# Finally run the tests to make sure that everything still works.
stack test --split-objs --ghc-options -O2
