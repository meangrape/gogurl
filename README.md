# gogurl

gogurl is a URL shortener/redirector.
If you enter [go/mail](http://go/mail) in your browser,
it will redirect you to your Google inbox (assuming
you have created the database entry).

Some other examples:
* go/cal -- Google calendar
* go/drive -- Google drive
* go/docs -- documentation
* go/gh -- Github
* go/koreana -- [Koreana's](http://koreanaboston.com/) reservations page
* go/flour -- [Flour Bakery's](https://flourbakery.com/) order page

It's meant to be hosted at [go](http://go/) in your local domain.
The fastest, most fool-proof, non-scalable way is an /etc/hosts entry.
If you control your office DNS, then make a searchable ".local" domain
and create go as a CNAME.

NB: gogurl doesn't support SSL or authentication of any kind. If you care,
**which you should**, put an nginx server in front.

# Usage
Usage: **gogurl [-p|--port PORT] [-d|--database DATABASE]**

**Available options:**

  *-h,--help*                Show this help text

  *-p,--port PORT*           port (default: 8082)

  *-d,--database DATABASE*   Sqlite database connection (default: "db/links.db")

# API
**GET /**
  The root URL presents a list of all short names sorted in descending order of
  hits (the more people use a go/url, the higher it appears.) Each item is
  clickable. Take a [look](#interface).

**GET /links**
  redirects to [go](https://go/) -- this is because we want to redirect through the
  nginx server in front of gogurl; we don't want to redirect straight to gogurl.

**POST /links**
  when sent the correct JSON, makes a new link
  ```json
  {
   "name": "shortname",
   "url":  "long_url",
  }
   ```

Here's the invocation to use if you're on the gogurl server:
   `curl -XPOST -H "Content-Type: application/json" -d '{"name":"mail","url":"https://mail.google.com/a/sentenai.com"}' http://go/links`

Here's how to curl it remotely, with a client certificate:
   `curl -XPOST -H "Content-Type: application/json" -E ./meangrape.pem -d
   '{"name":"mail","url":"https://mail.google.com"}'
   https://go.sentenai.net/links`

   You'll notice the full domain name. This is because in nginx, in order to
   stop TLS complaining at us, we use `go.domain.tld` as the server_name with an
   alias to `go`. If you create a self-signed certificate for "go" this won't be
   a problem.

**GET /links/$name/edit/$newurl**
  Edits the target URL for a name
  The new URL must be HTTP-encoded

**GET /links/$name/delete**
  Deletes the named shortcut

# Installation

If you don't have [stack](https://docs.haskellstack.org/en/stable/README/) installed, install it.

Checkout the git repository, `git clone https://github.com/jayed/gogurl.git
./gogurl`

Run `stack setup`; if you have installed stack from your OS package manager, or
installed it some time ago, you might need to run `stack upgrade` and then
`stack setup`. Once it finishes downloading and installing GHC, run `stack
build` to download the dependencies and compile them and the `gogurl` binary.
`stack install` will copy `gogurl` into `~/.local/bin`.

Currently, it creates a pool of 5 database connections and serves on \*:8082
from a sqlite3 database in db/links.db. The first is hard-coded in app/Main.hs.
The serving port and database connection string have defaults, but are
configurable via the `-p` and `-d` command-line flags.

# SQLite WAL mode

It's recommended to run sqlite in [WAL mode](https://www.sqlite.org/wal.html)
since this is a multi-reader/multi-writer application. You set this mode by
running `PRAGMA journal_mode=WAL;` in a sqlite connection and restarting
gogurl. This is a persistent setting and need only be done once.

# Nginx

You will want nginx in front of gogurl. One: you don't want people sniffing on
the URLs you use. Two: you don't want people to overwrite your links
with URLs pointing to malicious software. To be clear, gogurl does *absolutely
nothing to enforce security of any kind on it's own.*

Creating client certificates is out of scope for this documentation, but that's
my preferred method for controlling website access. The relevant portion of my
nginx config looks like this:

  ```nginx
  server {
  listen       443 ssl;
  server_name  go.$domain.$tld go;

  add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;

  #access_log  logs/host.access.log  main;
  ssl_certificate        certs/$YOUR.crt;
  ssl_certificate_key    certs/$YOUR.key;
  ssl_client_certificate certs/ca.crt;
  ssl_verify_client      on;
  ssl_dhparam            certs/dhparam.pem;

  ssl_session_cache    shared:SSL:1m;
  ssl_session_timeout  5m;

  ssl_protocols TLSv1.1 TLSv1.2;
  ssl_prefer_server_ciphers  on;
  ssl_ciphers  ECDH+AESGCM:ECDH+AES256:ECDH+AES128:DHE+AES128:!ADH:!AECDH:!MD5!aNULL;

  location / {
      proxy_pass http://127.0.0.1:8082;
  }
  ```

# <a name="interface">UI</a>

![gogurl UI screenshot](/docs/images/gogurl.png?raw=true "gogurl
interface")

# Contributors

[Mitchell W Rosen](https://github.com/mitchellwrosen)

[Matt Brandly](https://github.com/brandly)

[Nick Lawler](https://github.con/xilnocas)

[Brendan Kohler](https://github.com/xnomagichash)

[Jay Edwards](https://github.com/jayed)
