# gogurl

gogurl is a URL shortener/redirector.
So, if you go to "https://go/mail" in your browser,
it will redirect you to your Google inbox (assuming
you have created the database entry).

go/cal -- Google calendar
go/drive -- Google drive
go/docs -- docs.sentenai.com

It's meant to be hosted at "go" in your local domain.
The fastest, most fool-proof way is to make an /etc/hosts entry.
If you control your office DNS, then make ".local" or something similar
and create go as a CNAME.

NB: gogurl doesn't support SSL or authentication of any kind. If you care,
which you should, put an nginx server in front.

At that point you can access the various API endpoints:

**GET /**
  prints all the links we know about

**GET /links**
  redirects to /

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

   `curl -XPOST -H "Content-Type: application/json" -d '{"name":"mail","url":"https://mail.google.com/a/sentenai.com", "hits":0, "created_at"
:"2018-02-27T11:46:36Z"}' http://go/links`

**GET /links/<name>/edit/<new URL>**
  Edits the target URL for a name
  The new URL must be HTTP-encoded

**GET /links/<name>/delete**
  Deletes the named shortcut
