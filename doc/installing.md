# Installing

This document details how to install Hoff on your own server. I will be using
a server running Ubuntu 18.04 here.

The application consists of a single binary that opens a server at a configured
port, and then runs until it is killed. Log messages are written to stdout. This
makes the application work well with systemd.

## Building a package

Enter an environment with development dependencies available through [Nix][nix]:

    $ nix shell --file default.nix

There is a script to build a Debian package:

    $ cd package
    $ ./build-binary.sh
    $ VERSION=1 fakeroot ./build-package.sh

Alternatively, you can build the binary only, and assemble your own package:

    $ stack build
    $ $(stack path --local-install-root)/bin/hoff

The systemd service file is in `package/hoff.service`.

## Installing the package

On the server, install the package:

    $ sudo dpkg --install hoff_0.0.0-1.deb

This will do several things:

 * Install the `hoff` binary in `/usr/bin`.
 * Create the `hoff` user under which the daemon will run.
 * Create an example config file at `/etc/hoff/config.json`.

Enable the daemon to start it automatically at boot, and start it now:

    $ sudo systemctl enable hoff
    $ sudo systemctl start hoff

Verify that everything is up and running:

    $ sudo systemctl status hoff

## Setting up the user

The systemd service file included runs Hoff as the `hoff` user. The Debian
package creates it, but we need to do some further setup for files owned by this
user. You can also add the user manually:

    $ sudo useradd --system --user-group --no-create-home hoff

The application needs a key pair to connect to GitHub. Because the `hoff` system
user has no home directory, we will put it in `/etc/hoff` instead. The Debian
package creates that directory, but the `hoff` user has no write access in it,
so we create the files with the right owner before calling `ssh-keygen`.

    $ sudo touch /etc/hoff/id_ed25519{,.pub}
    $ sudo chown hoff:hoff /etc/hoff/id_ed25519{,.pub}
    $ sudo --user hoff ssh-keygen -t ed25519 -f /etc/hoff/id_ed25519
    $ sudo chmod u=rw,g=,o= /etc/hoff/id_ed25519
    $ sudo chmod u=rw,g=r,o=r /etc/hoff/id_ed25519.pub

Leave the passphrase empty to allow the key to be used without human
interaction. To tell SSH where the key is, we also create an SSH config file:

    $ echo "IdentitiesOnly yes"                | sudo tee --append /etc/hoff/ssh_config
    $ echo "IdentityFile /etc/hoff/id_ed25519" | sudo tee --append /etc/hoff/ssh_config
    $ echo "CheckHostIP no"                    | sudo tee --append /etc/hoff/ssh_config
    $ sudo chown hoff:hoff /etc/hoff/ssh_config
    $ sudo chmod u=rw,g=,o= /etc/hoff/ssh_config

Here we also set `CheckHostIP no`, so SSH does not emit a warning when the IP
address of a host changes. Hoff mounts a file that contains GitHub's public key
at `/etc/ssh/ssh_known_hosts`, so there is no need to accept any
[fingerprints][fingerprints]. Because the `ssh_known_hosts` file is readonly, we
can *only* connect to GitHub, and only if the public key that we baked into the
package has not changed. Furthermore, for testing (and also in general) it is
useful to prevent SSH from trying all keys it can find; it should
only use the provided file, so we set `IdentitiesOnly=yes`.

Finally, we need a GitHub account that will be used for fetching and pushing. I
recommend creating a separate account for this purpose. On GitHub, add the
public key to the new account. Paste the output of `cat /etc/hoff/id_ed25519.pub`
into the key field under “SSH and GPG keys”.

## Setting up directories

Hoff writes two things to the file system per configured repository:

 * A checkout of the repository.
 * A state file, to persist the internal state (open issues, etc.).

A good place to store these is in `/var/lib/hoff`. The Debian package creates a
`checkouts` and a `state` subdirectory there, owned by the `hoff` user so it can
create files and subdirectories. We could also create them manually:

    $ sudo mkdir --parents /var/lib/hoff/{checkouts,state}
    $ sudo chown hoff:hoff /var/lib/hoff/{checkouts,state}

## Adding a repostory

To add a repository, we need to add an entry to the config file. I’ll be using
the repository `ruuda/bogus` in this example. Add this to the `projects` key in
the config file (e.g. with `sudo --edit /etc/hoff/config.json`):

    {
      "owner": "ruuda",
      "repository": "bogus",
      "branch": "master",
      "testBranch": "testing",
      "checkout": "/var/lib/hoff/checkouts/ruuda/bogus",
      "stateFile": "/var/lib/hoff/state/ruuda/bogus.json"
    }

The meaning of the fields is as follows:

 * *Owner*: The GitHub user or organization that owns the repository.
   In my case `ruuda`.
 * *Repository*: The GitHub repository to manage. In my case `bogus`.
 * *Branch*: The branch to integrate changes into. `master` in most cases.
 * *TestBranch*: The branch that changes are pushed to to trigger a CI build.
   The application will force-push to this branch, so it should not be used for
   other purposes. I used `testing`.
 * *Checkout*: The full path to the checkout.
 * *StateFile*: The path to the file where the daemon saves its state, so it
   can remember the set of open pull requests across restarts. TODO: urge to
   back up this file regularly.

On GitHub, add the bot account to this repository as a collaborator, to give it
push access (and pull access in the case of a private repository). Note that
after adding the bot as a collaborator, you need to accept the invitation from
the bot account. (TODO: automate this via the API.)

When Hoff starts, it will clone the repository if it does not yet exist. It also
creates the state file if it does not exist.

## Global configuration

There are a few global options in the config file too:

 * *Secret*: The secret used to verify the authenticity of GitHub webhooks.
   You can run `head --bytes 32 /dev/urandom | base64` to generate a secure
   256-bit secret that doesn’t require any character to be escaped in the json
   file.
 * *AccessToken*: A GitHub API access token for the bot user. This is used to
   leave comments on behalf of the bots.
 * *Port*: The port at which the webhook server is exposed. The systemd unit
   ensures that the daemon has permissions to run on priviliged ports (such as
   80 and 443) without having to run as root.
 * *TLS*: Can be used to make the server serve https instead of insecure http.
   See the [TLS guide](tls.md) for more details. Set to `null` to disable TLS.
 * *Trigger.commentPrefix*: Specifies the prefix that makes Hoff interpret a
   comment as a command directed at it. Setting this to the username of the bot
   account makes for natural conversations on GitHub, but a different prefix
   could be used too. In my case, I set it to `@hoffbot`.

Finally, there is some Git config for the bot user, under the *user* key. *Name*
and *email* are used for the Git committer metadata. *SshConfigFile* should
point to `/etc/hoff/ssh_config` as [created previously](#setting-up-the-user).

Restart the daemon to pick up the new configuration, and verify that it started
properly:

    $ sudo systemctl restart hoff
    $ sudo systemctl status hoff

## Setting up webhooks

On GitHub, go to the repository settings and add a new webhook. The payload url
should be `http://yourserver.com/hook/github`, with content type
application/json. Enter the secret generated in the previous section, and select
the following events to be delivered:

 * *Pull request*, to make the daemon aware of new or closed pull requests.
 * *Issue comment*, to listen for LGTM stamps.
 * *Pull request reviews*, to listen for LGTM stamps in review summaries.
 * *Status*, to get updates on the build status from a linked CI service.

GitHub will deliver a ping event, and if everything is okay a green checkmark
will appear in the list of configured webhooks. On the server, we can see that
the webhook was received:

    $ sudo journalctl --pager-end --unit hoff
    > ...
    > Sep 04 21:37:41 hoffbuild hoff[2860]: [Debug] github loop received event: Ping

That’s it! You can now open a pull request and leave an LGTM comment to see the
application in action. Remember to also set up a CI service like Travis CI to
provide the build status updates.

[fingerprints]: https://help.github.com/articles/github-s-ssh-key-fingerprints/
[nix]:          https://nixos.org/nix
