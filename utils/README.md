# Command-line utilities

`editgo` is broken. The server dislikes HTTP encoded URLs.

This is a small collection of bash scripts that wrap curl and make it easy to
interact with a **gogurl** server. One tool, `editgo` relies on the presence of
Perl and its URI:Escape module. `listgo` relies on Perl's HTML::Strip module
which is not part of the base install.

Each tool contains two variables that need to be edited:
*$CERT* should point to your client certificate on the filesystem.
*$SERVER* should point to your **gogurl** server's FQDN (not URI).

`addgo gcp http://console.google.com`

`rmgo gcp`

`getgo gcp`

`listgo`

``` ~~editgo gcp http://a-different-url.google.com~~```

All except `getgo` and `listgo` return the server's JSON response. `getgo` returns the URL
attached to a short name.
