# TLS

Hoff has has a built-in webserver ([Warp][warp]) that can either serve http or
https. To enable https, point `tls.keyFile` and `tls.certFile` to the right
key and certificate file in the configuration file.

[warp]: https://www.stackage.org/package/warp

## Self-signed

For local testing purposes, you can generate a self-signed certificate:

    openssl genrsa -out key.pem 2048
    openssl req -new -key key.pem -out certificate.csr
    openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem

## Let’s Encrypt

[Let’s Encrypt][letsencrypt] is a certificate authority that can be used in a
fully automated way in production. The [Certbot][certbot] client works well in
combination with Hoff. In this section a server running Ubuntu 16.04 is assumed.

First install Certbot:

    $ sudo apt install letsencrypt

We will be using the “standalone” mode of Certbot. This mode temporarily runs a
webserver when a certificate is requested or renewed, so it cannot run while
Hoff itself is running. Fortunately Certbot can execute commands to stop and
start other services before and after renewing a certificate, so the entire
renewal process can be automated. Only the initial request needs to be done
manually.

Stop Hoff because Certbot will need access to the same ports:

    $ sudo systemctl stop hoff

Next we request an initial certificate. Note that unfortunately Ubuntu does not
use the standard `certbot` command. Instead the command is called `letsencrypt`.
Don’t forget to open port 443 in your firewall if you had not done so already.

    $ sudo ufw allow 443
    $ sudo letsencrypt certonly --standalone

Update the configuration file to point to the newly generated certificate:

    $ sudo -e /etc/hoff.json

Make sure to set the appropriate values for `tls`, and set the port to 443.

```json
{
  "port": 443,
  "tls": {
    "keyFile": "/etc/letsencrypt/live/example.com/privkey.pem",
    "certFile": "/etc/letsencrypt/live/example.com/fullchain.pem"
  }
}
```

Before we can start Hoff again, we need to fix the permissions of the
certificate directories. Currently they are accessible only to `root`, but Hoff
runs as the `git` user. To fix this, create a new user group `certaccess`, and
put `git` in this group:

    $ sudo addgroup certaccess
    $ sudo usermod --append --groups certaccess git

Next, change the group owner of the certificate directories to `certaccess`,
and make the directories group-readable and searchable:

    $ sudo chown root:certaccess /etc/letsencrypt/archive
    $ sudo chown root:certaccess /etc/letsencrypt/live
    $ sudo chmod g+rx /etc/letsencrypt/archive
    $ sudo chmod g+rx /etc/letsencrypt/live

Now we can start Hoff again:

    $ sudo systemctl start hoff
    $ sudo systemctl status hoff

TODO: Write a systemd unit that renews with

    $ certbot renew --pre-hook "systemctl stop hoff" --post-hook "systemctl start hoff"

[letsencrypt]: https://letsencrypt.org/
[certbot]:     https://certbot.eff.org/
