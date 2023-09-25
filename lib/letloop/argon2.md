# `(import (letloop argon2))`

Argon2 is the winner of the Password Hashing Competition (PHC). Argon2
is a memory-hard password hashing function which can be used to hash
passwords for credential storage, key derivation, or other
applications.

## `(argon2id-verify bytevector other)`

Verify that `BYTEVECTOR` is the hash of `OTHER`.

## `(argon2id-encode salt password)`

Hash `PASSWORD` using `SALT`.
