# `(import (letloop www))`

## `(www-request method url headers body)`

Make a request using `method`, against `url` with `headers`, and
`body`. Requires `curl` command line tool. Returns the code, headers,
and body.

## `(www-form-urlencoded-read string)`

Read percent encoded form `string` into an association list of symbol,
and strings. That is the default serialization format of html forms.

## `(www-request-line-uri-split string)`

Returns the list made of the string components of an uri `string`.

## `(www-query-read string)`

Alias for `www-form-urlencoded-read` that is easier to read when
working on an uri query string.

## `(www-uri-read string)`

Read an uri `string`, and return three values:

1. The list of path components, or `#f`;
2. The list associating symbol to string based on the query, or `#f`;
3. The fragment string, or `#f`

## `(www-host-read string)`

Read the host `string` into a list of components spliting by `.`.


