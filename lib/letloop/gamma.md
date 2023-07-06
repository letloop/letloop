# `(import (letloop gamma))`

Predicate-based generic dispatch.

## `(make-gamma name)`

Create a gamma called `name`.

## `(gamma instance predicate procedure)`

`instance` will call `procedure` when arguments of `instance` satisfy
`predicate?`.
