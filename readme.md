# Hoff

Hoff is a bot for GitHub that enforces a clean history, and ensures that master
always builds.

Hoff intends to replace the merge button in the GitHub UI.  Hoff integrates
changes into master using a *rebase*. This keeps the history clean, free of
random fork points and merge commits.  (TODO: In the future Hoff will also
enforce a commit message format.)

Furthermore, Hoff implements the [Not Rocket Science Principle][not-rocket-science]
of software engineering:

> Automatically maintain a repository of code that always passes all the tests.

The application watches a repository for new pull requests. Once a pull request
has been approved (through an LGTM comment left by a reviewer), it integrates
the changes into master, and pushes those to a testing branch. When CI reports a
successful build for this branch, master is forwarded to it. If the build fails,
the commits never make it into master, keeping the build green at all times.


## Using Hoff

Supposing Hoff is set up to listen for the comment prefix `@hoffbot` with a
matching GitHub user, you use it by commenting on a PR with any of the
following commands:

* `@hoffbot merge`: rebase then merge;
* `@hoffbot merge and tag`: rebase, merge then tag.
* `@hoffbot merge and deploy`: rebase, merge, tag then deploy;

For all the commands, Hoff will wait for the builds to pass after rebasing and
before merging.  When the PR is merged, Hoff automatically closes it and
deletes the PR branch.

On Fridays, by default, Hoff refuses to do the above actions.  To force merges
on Fridays, simply add `on friday` at the end of your commands, like so:

* `@hoffbot merge on friday`;
* `@hoffbot merge and tag on friday`.
* `@hoffbot merge and deploy on friday`;


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
    $ cp package/example-config.json config.json
    $ sudo systemd-nspawn \
      --ephemeral         \
      --image hoff.img    \
      --bind-ro=$PWD:/etc \
      -- /usr/bin/hoff /etc/config.json

The image includes Hoff and all of its dependencies (Git, SSH). You can run it
with systemd. TODO: Make it work with portablectl.

You can also build Hoff as a deb package by running:
`./package/build-and-ship.sh`

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

[not-rocket-science]: https://graydon2.dreamwidth.org/1597.html
[stack]:              https://haskellstack.org
[nix]:                https://nixos.org/nix/
[apache2]:            https://www.apache.org/licenses/LICENSE-2.0
