## `(letloop sqlite3)`

### Abstract

SQLite3 library is a database that implements SQL query language
on-top of a robust, and well tested storage backend.

At this time, it is only possible to bind a variable to a bytevector,
and query integers and bytevectors.

### `(sqlite3-open filename)`

Open a database at `FILENAME`; return a handle.

### `(sqlite3-close database)`

Close `DATABASE`.

### `(sqlite3-prepare database sql)`

In `DATABASE`, prepare a query `SQL`; return a handle over the
prepared query, called a `statement`.

### `(sqlite3-reset statement)`

Reset a `STATEMENT`.

### `(sqlite3-bind-blob statement index bytevector)`

Against `STATEMENT` bind at the variable positioned at `INDEX` the
value `BYTEVECTOR`. The first variable in the query associated with
`STATEMENT` has the index one (`1`).

### `(sqlite3-step statement)`

Step throught the rows result of the query associated with
`STATEMENT`, return `#true` if there is more rows, otherwise return
`#false`.

### `(sqlite3-column-blob statement index)`

Inside `STATEMENT`, retrieve the value positioned at `INDEX` as a
`bytevector`.

### `(sqlite3-column-int statement index)`

Inside `STATEMENT`, retrieve the value positioned at `INDEX` as an
integer.
