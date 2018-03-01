# Command-line utilities

This is a small collection of bash scripts that wrap curl and make it easy to
interact with a gogurl server. One tool, `editgo` relies on the presence of
Perl and its URI:Escape module.

Each tool contains two variables that need to be edited:
$CERT should point to your client certificate on the filesystem.
$SERVER should point to your gogurl server's FQDN (not URL).

`addgo gcp http://console.google.com`
`rmgo gcp`
`editgo gcp http://a-different-url.google.com`
`getgo gcp`
`listgo`

All except `getgo` and `listgo` return the server's JSON response. `getgo` returns the URL
attached to a short name. `listgo` returns all short names and associated URLs.