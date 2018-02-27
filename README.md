# gogurl

gogurl is a URL shortener/redirector.
So, if you go to "http://go/mail" in your browser,
it will redirect you to your Google inbox (assuming
you have created the database entry).

Some other examples:
* go/cal -- Google calendar
* go/drive -- Google drive
* go/docs -- documentation
* go/gh -- Github
* go/koreana -- [Koreana's](http://koreanaboston.com/) reservations page
* go/flour -- [Flour Bakery's](https://flourbakery.com/) order page

It's meant to be hosted at "go" in your local domain.
The fastest, most fool-proof way is to make an /etc/hosts entry.
If you control your office DNS, then make ".local" or something similar
and create go as a CNAME.

NB: gogurl doesn't support SSL or authentication of any kind. If you care,
which you should, put an nginx server in front.

At that point you can access the various API endpoints:

**GET /**
  prints all the links we know about
  This is terrible, ugly, and only barely useful.
  But I'm tired of learning about Haskell and want to
  get this deployed.

**GET /links**
  redirects to http://go/ -- this is because we want to redirect through the
  nginx server in front of gogurl; we don't want to redirect straight to gogurl.

**POST /links**
  when sent the correct JSON, makes a new link
  ```json
  {
   "name": "shortname",
   "url":  "long_url",
   "hits": 0,
   "created_at: "2018-01-01T00.00.00Z"
   }
   ```

Here's the invocation to use if you're on the gogurl server:
   `curl -XPOST -H "Content-Type: application/json" -d '{"name":"mail","url":"https://mail.google.com/a/sentenai.com", "hits":0, "created_at"
:"2018-02-27T11:46:36Z"}' http://go/links`

Here's how to curl it remotely, with a client certificate:
   `curl -XPOST -H "Content-Type: application/json" -E ./meangrape.pem -d
   '{"name":"pro","url":"https://graphs.sentenai.net/prometheus", "hits":0,
   "created_at":"2018-02-27T11:46:36Z"}' https://go.sentenai.net/links`

   You'll notice the full domain name. This is because in nginx, in order to
   stop TLS complaining at us, we use `go.domain.tld` as the server_name with an
   alias to `go`

**GET /links/$name/edit/$newurl**
  Edits the target URL for a name
  The new URL must be HTTP-encoded

**GET /links/$name/delete**
  Deletes the named shortcut
