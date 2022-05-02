#!/bin/bash

# Shared commands for the set-up phase of jobs

# This file is NOT usable with Semaphore's "commands_file" property, as this
# puts all the comments in this file in the build output and requires us to
# write all build steps on one line each. Instead, this file must be executed
# in the build pipelines where we use it.

# We don't use `set -x` here to show the run commands, because this would show
# authentication tokens in the build output. Instead, `set -v` prints the
# commands "as they are read".
# We do want to exit immediately after encountering an error.
# We also cannot use "set -u", as Semaphore's tooling uses unset variables in
# some places (which we can't change).
set -evo pipefail

# Clear disk space by removing docker related services, mounts and files.
sudo systemctl stop docker.service docker.socket

if [[ $(findmnt /var/lib/docker) ]]; then
  sudo umount /var/lib/docker
fi

if [[ -e "/dev/nbd0" ]]; then
  sudo qemu-nbd --disconnect /dev/nbd0
fi

# Clear disk space by removing tooling that we don't use. This is needed to run
# on the smallest machine type Semaphore offers, as this machine type has too
# little disk space to build the package due to a lot of extra tooling that we
# don't need. Removing these directories clears about 4 GiB.
sudo rm -rf \
    /home/semaphore/.rbenv \
    /home/semaphore/.kerl \
    /home/semaphore/.nvm \
    /home/semaphore/.phpbrew \
    /home/semaphore/.kiex \
    /opt/google \
    /opt/firefox-esr \
    /opt/firefox-esr-prev \
    /usr/local/golang \
    /mnt/docker.qcow2
