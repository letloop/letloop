## `(import (letloop unknown))`

### Protocol

#### Registration

#. The client compute a verifier based on a salt, identity, and password using the procedure `unknown-compute-verifier`
#. The client send to the server the salt, identity, and verifier;
#. The server stores the triplet: identity, password, salt.

#### Authentication

#. The client initiates authentication by sending its identifier;
#. The server replies with the salt associated with that identifier;
#. The client compute a value called `A` and send it to the server;
#. The server compute a value called `B` and send it to the client;
#. At this point, client, and server should have the same shared secret called `K`;
#. The server sends `M2` to proove it knows `K`
#. The client sends `M1` to proove it knows `K`

### `(make-unknown-client-verifier parameter salt identity password) → bytevector?`

Client, and server must use the same `PARAMETER`. Returns a bytevector
that must be sent to the server with `SALT`, and `IDENTITY` for
registering the client.

### `(make-unknown-server parameter nonce salt identity verifier)`

Return an object `unknown-server`, that can be used to compute the
objects necessary to authenticate a client.

### `(make-unknown-client parameter nonce salt identity password)`

Return an object `unknown-client`, that can be used to compute the
objects necessary to authenticate with a server.

### `(unknown-server-A! server A)`

Associate `A` with the server. It is then possible to call
`unknown-server-K`, `unknown-server-M2`, and
`unknown-server-check-M1`.

### `(unknown-client-B! client B)`

Associate `B` with the client. It is then possible to call
`unknown-client-K`, `unknown-client-M2`, and
`unknown-client-check-M1`.

### `(unknown-server-K server)`

Return the wanna be shared secret.

### `(unknown-server-M2 server)`

Return `M2` that must be sent to the client.

### `(unknown-server-check-M1? server M1)`

Verify that the value sent by the client `M1` is valid.

### `(unknown-client-K client)`

Return the wanna be shared secret.

### `(unknown-client-M1 client)`

Return `M2` that must be sent to the client.

### `(unknown-client-check-M2? client M2)`

Verify that the value sent by the server `M2` is valid.
