---
title: An Ordered Key-Value Store by the makers of SQLite
banner: https://hyper.dev/static/okdb.png
date: 2023-10-15
abstract: Started as a new backend for SQLite, it can be embedded in your Scheme program for use as a simple, and but powerful storage programming interface.
---

# `(import (letloop lsm1))`

This library expose SQLite3 LSM extension.

## `(lsm1-begin db level)`

Start a transaction, or a nested transaction.

## `(lsm1-close db)`

Close database.

## `(lsm1-commit db level)`

Commit current transaction up-to `LEVEL`.

## `(lsm1-config db config value)`

Configure `db` with `CONFIG`, and `VALUE`...

## `(lsm1-cursor-close cursor)`

Close cursor `CURSOR`.

## `(lsm1-cursor-first cursor)`

Move the cursor to the first position, the start of the key space.

## `(lsm1-cursor-key cursor)`

Return the key associated with the current position of `CURSOR`.

## `(lsm1-cursor-last cursor)`

Move the cursor to the last position, the end of the key space.

## `(lsm1-cursor-next cursor)`

Move `CURSOR` to the next position.

## `(lsm1-cursor-open db)`

Open a cursor.

## `(lsm1-cursor-prev cursor)`

Move `CURSOR` to the previous position.

## `(lsm1-cursor-seek cursor key strategy)`

Seek `KEY` using one of the following strategy:

- 'less-than-or-equal-fast
- 'less-than-or-equal
- 'equal
- 'greater-than-or-equal

## `(lsm1-cursor-valid? cursor)`

Return `#true` if `CURSOR` is at a valid position.

## `(lsm1-cursor-value cursor)`

Return the value associated with the current position of `CURSOR`.

## `(lsm1-delete db key)`

If any, remove `KEY` and its value.

## `(lsm1-insert db key value)`

Insert `KEY` with `VALUE`, or update the value associated with `KEY`.

## `(lsm1-new)`

Return a handle over a database. See `lsm1-config`, and `lsm1-open`.

## `(lsm1-open db filename)`

Against `DB`, open `FILENAME`.

## `(lsm1-rollback db level)`

Rollback current transaction up-to `LEVEL`.
