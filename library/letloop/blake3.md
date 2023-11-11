# `(import (letloop blake3))`

Variable length cryptographic hash linked to official C shared library
release.

## `(make-blake3)`

Return a handle called `hasher` over blake3 algorithm's state. 

Useful when the hash computation has to be spread over time such as
when the input bytes are bigger than available volatile memory.

## `(blake3-update! hasher bytevector)`

Update the state of `hasher` with the given `bytevector`.

## `(blake3-finalize hasher)`

Return the bytevector computed by `hasher`.

## `(blake3 bytevector)`

Return the blake3 hash of bytevector as a bytevector of length 32.
