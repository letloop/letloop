# `(import (letloop cffi))`

## `(call-with-errno thunk proc)` syntax

Call `THUNK` and capture POSIX `errno`, and call `PROC` with the
output of `THUNK`, and the captured errno.

## `(with-lock objects body ...)` syntax

Lock every object in `OBJECTS` with `lock-object`, and unlocks objects
after evaluating `body ...`.

## `(bytevector-pointer bytevector)`

Return the memory address as an integer where is located
`BYTEVECTOR`'s bytes. The returned integer can be passed to a foreign
function that expects `char *`.

## `(strerror code)`

Return a human readable string describing `code`. `strerror` must be
called on errno values.
