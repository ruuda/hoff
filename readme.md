# Hoff

Hoff is a bot for GitHub that enforces a clean history, and ensures that master
always builds.

[![Build Status][travis-img]][travis]

Hoff intends to replace the merge button in the GitHub UI. Unlike GitHub\* and
many other tools, Hoff integrates changes into master using a *rebase*. This
keeps the history clean, free of random fork points and merge commits. (TODO:
In the future Hoff will also enforce a commit message format.)

\* [This is no longer true.](https://github.com/blog/2243-rebase-and-merge-pull-requests)

Furthermore, Hoff implements the [Not Rocket Science Principle][not-rocket-science]
of software engineering:

> Automatically maintain a repository of code that always passes all the tests.

The application watches a repository for new pull requests. Once a pull request
has been approved (through an LGTM comment left by a reviewer), it integrates
the changes into master, and pushes those to a testing branch. When CI reports a
successful build for this branch, master is forwarded to it. If the build fails,
the commits never make it into master, keeping the build green at all times.

## Installing

See the [installation guide](doc/installing.md) if you want to run a self-hosted
version of Hoff.

TODO: Write a proper guide to build a package.
TODO: Publish official deb packages?

## Building

Hoff is written in Haskell and builds with [Stack][stack]:

    $ stack setup
    $ stack build
    $ stack test

To run the application locally:

    $ cp package/example-config.json config.json
    $ stack exec hoff config.json

You probably want to edit the config file before running.

To run Hoff on a server, you can build a self-contained squashfs file system
image with [Nix][nix]:

    $ nix build --out-link hoff.img

The image includes Hoff and all of its dependencies (Git, SSH). You can run it
with systemd. TODO: Make it work with portablectl.

## Further reading

More information is available in the doc directory:

 * [Background](doc/background.md): My original intention was more ambitious
   than building a GitHub bot. This document gives some background about what I
   want to build.
 * [Approach](doc/approach.md): Progress is made in many small steps. This
   document outlines the current goals.
 * [Design](doc/design.md): Outlines the architecture of the application.
 * [Installing](doc/installing.md): The installation guide.

## License

Hoff is free software. It is licensed under the [Apache 2.0][apache2] license.
It may be used both for commercial and non-commercial use under the conditions
given in the license.

[travis-img]:         https://travis-ci.org/ruuda/hoff.svg?branch=master
[travis]:             https://travis-ci.org/ruuda/hoff
[not-rocket-science]: https://graydon2.dreamwidth.org/1597.html
[stack]:              https://haskellstack.org
[nix]:                https://nixos.org/nix/
[apache2]:            https://www.apache.org/licenses/LICENSE-2.0
