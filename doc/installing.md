# Installing

This document details how to install Hoff on your own server. I will be using
a server running Ubuntu 16.04 here.

The application consists of a single binary that opens a server at a configured
port, and then runs until it is killed. Log messages are written to stdout. This
makes the application work well with systemd.

## Building a package

TODO: Better compiling guide. For now, navigate to the `package` directory on
a development machine and run

    $ ./build-binary.sh
    $ VERSION=0.0.0 ./build-package.sh

This will produce a deb package that you can copy to your server and install.

## Installing the package

On the server, install the package:

    $ sudo dpkg --install hoff_0.0.0-1.deb

This will do several things:

 * Install the `hoff` binary in `/usr/bin`.
 * Create the `git` user under which the daemon will run.
 * Create an example config file at `/etc/hoff.json`.

Edit the config file, then enable and start the daemon:

    $ sudo --edit /etc/hoff.json
    $ sudo systemctl enable hoff
    $ sudo systemctl start hoff

Verify that everything is up and running:

    $ sudo systemctl status hoff

## Adding a repository

TODO: Generate Git credentials, check out the repository.
TODO: Configure GitHub.
