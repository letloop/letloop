# `(import (letloop argon2))`

Argon2 is a configurable memory-hard, and cpu-hard password
cryptographic hashing function which can be used to hash passwords for
credential storage, key derivation, or maybe others.

## `(argon2id-encode salt password)`

Hash `password` using `salt`.

## `(argon2id-verify bytevector other)`

Verify that `other` is the same password that was used to generate
`bytevector` with `argon2id-encode`.
