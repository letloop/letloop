(library (letloop unknown)
  (export make-unknown-client-verifier
          make-unknown-server
          make-unknown-client
          unknown-server-A!
          unknown-client-B!
          unknown-server-K
          unknown-server-M2
          unknown-server-check-M1?
          unknown-client-K
          unknown-client-M1
          unknown-client-check-M2?
          ~check-unknown-000)
  (import (chezscheme)
          (letloop bytevector)
          (letloop blake3)
          (letloop argon2)
          (letloop r999))

  ;; The SRP Authentication and Key Exchange System
  ;;
  ;; ref: https://tools.ietf.org/html/rfc2945
  ;; ref: http://srp.stanford.edu/doc.html
  ;; ref: https://en.wikipedia.org/wiki/Secure_Remote_Password_protocol
  ;;
  ;; TODO: Use unicode NFKD normalization to avoid problems because of
  ;; the input method

  (define pk
    (lambda args
      (display ";; ") (write args)(newline)
      (flush-output-port)
      (car (reverse args))))

  (define ->bytevector integer->bytevector-little-endian)
  (define ->integer bytevector-little-endian->integer)

  (define-record-type* <unknown>
    (make-unknown~ name proc)
    unknown?
    (name unknown-name)
    (proc unknown-proc))

  (define make-unknown
    (lambda (name length)
      (let ((bytevector #f)
            (integer #f)
            (frozen #f)
            (length length))
        (make-unknown~
         name
         (lambda (message type . object)
           (case message
             (ref (case type
                    (name name)
                    (integer (or (and frozen integer)
                                 (error 'letloop "no set")))
                    (bytevector (or (and frozen bytevector)
                                    (error 'letloop "no set")))))
             (set (case type
                    (integer (or (and frozen
                                      (error 'letloop
                                             "unknown frozen"
                                             name))
                                 (begin
                                   (set! frozen #t)
                                   (set! integer (car object))
                                   (set! bytevector (->bytevector (car object) length)))))
                    (bytevector (or (and frozen
                                         (error 'letloop
                                                "unknown frozen"
                                                name))
                                    (begin
                                      (set! frozen #t)
                                      (set! integer (->integer (car object)))
                                      (set! bytevector (car object)))))))))))))

  (define unknown-integer
    (lambda (unknown)
      ((unknown-proc unknown) 'ref 'integer)))

  (define unknown-bytevector
    (lambda (unknown)
      ((unknown-proc unknown) 'ref 'bytevector)))

  (define unknown-integer!
    (lambda (unknown value)
      ((unknown-proc unknown) 'set 'integer value)))

  (define unknown-bytevector!
    (lambda (unknown value)
      ((unknown-proc unknown) 'set 'bytevector value)))

  (define bytevector->unknown
    (lambda (name bytevector)
      (define out (make-unknown name (bytevector-length bytevector)))
      (unknown-bytevector! out bytevector)
      out))

  ;; REFERENCE RFC with those numbers

  (define UNKNOWN-N-2048
    (bytevector 172 107 219 65 50 74 154 155 241 102 222 94 19 137 88 47 175 114 182 101 25 135 238 7 252 49 146 148 61 181 96 80 163 115 41 203 180 160 153 237 129 147 224 117 119 103 161 61 213 35 18 171 75 3 49 13 205 127 72 169 218 4 253 80 232 8 57 105 237 183 103 176 207 96 149 23 154 22 58 179 102 26 5 251 213 250 170 232 41 24 169 150 47 11 147 184 85 249 121 147 236 151 94 234 168 13 116 10 219 244 255 116 115 89 208 65 213 195 62 167 29 40 30 68 107 20 119 59 202 151 180 58 35 251 128 22 118 189 32 122 67 108 100 129 241 210 185 7 135 23 70 26 91 157 50 230 136 248 119 72 84 69 35 181 36 176 213 125 94 167 122 39 117 210 236 250 3 44 251 219 245 47 179 120 97 96 39 144 4 229 122 230 175 135 78 115 3 206 83 41 156 204 4 28 123 195 8 216 42 86 152 243 168 208 195 130 113 174 53 248 233 219 251 182 148 181 200 3 216 159 122 228 53 222 35 109 82 95 84 117 155 101 227 114 252 214 142 242 15 167 17 31 158 74 255 115))

  (define-record-type* <unknown-parameter>
    (make-unknown-parameter generator byte-count N)
    unknown-parameter
    (generator unknown-parameter-generator)
    (byte-count unknown-parameter-byte-count)
    (N unknown-parameter-N))

  (define PARAMETER-2048
    (let ((length (/ 2048 8))) ;; 256 bytes
      (make-unknown-parameter
       (let ((generator (make-unknown 'generator 256)))
         (unknown-integer! generator 2)
         generator)
       length
       (let ((N (make-unknown 'N-2048 256)))
         (unknown-bytevector! N UNKNOWN-N-2048)
         N))))

  (define unknown-compute-x
    (lambda (salt identity password)

      (define ip (argon2id
                  (unknown-bytevector salt)
                  (bytevector-append (unknown-bytevector identity) (unknown-bytevector  password))))
      (define x (make-unknown 'x (bytevector-length ip)))
      (unknown-bytevector! x ip)
      x))

  (define make-unknown-client-verifier
    (lambda (parameter salt~ identity~ password~)
      (define salt (bytevector->unknown 'salt salt~))
      (define identity (bytevector->unknown 'identity identity~))
      (define password (bytevector->unknown 'password password~))

      ;; TODO: what is 256?
      (define verifier (make-unknown 'verifier 256))
      (unknown-integer! verifier
                        (expt-mod
                         (unknown-integer
                          (unknown-parameter-generator parameter))
                         (unknown-integer
                          (unknown-compute-x salt identity password))
                         (unknown-integer
                          (unknown-parameter-N parameter))))
      (unknown-bytevector verifier)))

  (define unknown-compute-k
    (lambda (parameter)
      (define k (make-unknown 'k 32))
      (define hasher (make-blake3))
      (blake3-update! hasher
                      (unknown-bytevector
                       (unknown-parameter-N parameter)))
      (blake3-update! hasher
                      (unknown-bytevector
                       (unknown-parameter-generator parameter)))
      (unknown-bytevector! k (blake3-finalize hasher 32))
      k))

  (define unknown-compute-B
    (lambda (parameter k v b)
      (define generator (unknown-parameter-generator parameter))
      (define N (unknown-parameter-N parameter))
      (define B (make-unknown 'B 256))
      (unknown-integer! B (modulo (+ (unknown-integer v)
                                     (expt-mod (unknown-integer generator)
                                               (unknown-integer b)
                                               (unknown-integer N)))
                                  (unknown-integer N)))
      B))

  (define unknown-compute-A
    (lambda (parameter a)
      ;; Where does the following come from?
      (define A (make-unknown 'A 256))
      (define generator (unknown-parameter-generator parameter))
      (define N (unknown-parameter-N parameter))
      (assert (<= 256 (bitwise-bit-count (unknown-integer a))))
      (unknown-integer! A
                        (expt-mod (unknown-integer generator)
                                  (unknown-integer a)
                                  (unknown-integer N)))
      A))

  (define unknown-compute-u
    (lambda (A B)
      (define u (make-unknown 'u 32))
      (define hasher (make-blake3))
      (blake3-update! hasher (unknown-bytevector A))
      (blake3-update! hasher (unknown-bytevector B))
      (unknown-bytevector! u (blake3-finalize hasher 32))
      u))

  (define unknown-client-compute-S
    (lambda (parameter x a B u)
      (define N (unknown-parameter-N parameter))
      (define g (unknown-parameter-generator parameter))
      (define client-S (make-unknown 'S 256))

      (unless (< 0
                 (unknown-integer B)
                 (unknown-integer (unknown-parameter-N parameter)))
        (error 'letloop "B must be between 1 and N - 1" B))

      (unknown-integer! client-S
                        (expt-mod (- (unknown-integer B)
                                     (expt-mod (unknown-integer g)
                                               (unknown-integer x)
                                               (unknown-integer N)))
                                  (+ (unknown-integer a)
                                     (* (unknown-integer u)
                                        (unknown-integer x)))
                                  (unknown-integer N)))
      client-S))

  (define unknown-server-compute-S
    (lambda (parameter v A b u)
      (define N (unknown-parameter-N parameter))
      (define server-S (make-unknown 'S 256))

      (unless (< 0 (unknown-integer A) (unknown-integer (unknown-parameter-N parameter)))
        (error 'letloop "A must be between 1 and N - 1" A))

      (unknown-integer! server-S
                        (expt-mod (* (unknown-integer A)
                                     (expt-mod (unknown-integer v)
                                               (unknown-integer u)
                                               (unknown-integer N)))
                                  (unknown-integer b)
                                  (unknown-integer N)))
      server-S))

  (define unknown-compute-K
    (lambda (S)
      (define K (make-unknown 'K 32))
      (unknown-bytevector! K (blake3 (unknown-bytevector S)))
      K))

  (define unknown-compute-M1
    (lambda (who parameter I s A B K)

      (define magic
        (lambda ()
          (define N (unknown-parameter-N parameter))
          (define g (unknown-parameter-generator parameter))
          (blake3
           (->bytevector
            (bitwise-xor
             (->integer (blake3 (unknown-bytevector N)))
             (->integer (blake3 (unknown-bytevector g))))
            32))))

      (define hasher (make-blake3))
      (define M1 (make-unknown (cons who 'M1) 32))

      (blake3-update! hasher (magic))
      (blake3-update! hasher (blake3 (unknown-bytevector I)))
      (blake3-update! hasher (unknown-bytevector s))
      (blake3-update! hasher (unknown-bytevector A))
      (blake3-update! hasher (unknown-bytevector B))
      (blake3-update! hasher (unknown-bytevector K))
      (unknown-bytevector! M1 (blake3-finalize hasher 32))
      M1))

  (define unknown-compute-M2
    (lambda (who parameter A M K)
      (define hasher (make-blake3))
      (define M2 (make-unknown (cons who 'M2) 32))
      (blake3-update! hasher (unknown-bytevector A))
      (blake3-update! hasher (unknown-bytevector M))
      (blake3-update! hasher (unknown-bytevector K))
      (unknown-bytevector! M2 (blake3-finalize hasher 32))
      M2))

  (define unknown-bytevector=?
    (lambda (bytevector other)
      ;; XXX: TODO: wanna be constant-time comparison. A drop in the
      ;; ocean compared to our non-constant-time modexp operations,
      ;; but still good practice.
      (if (not (fx=? (bytevector-length bytevector)
                     (bytevector-length other)))
          #f
          (let loop ((index (bytevector-length bytevector))
                     (out #t))
            (if (fxzero? index)
                out
                (let ((index (fx- index 1)))
                  (loop (fx- index 1)
                        (and out
                             (fx=? (bytevector-u8-ref bytevector index)
                                   (bytevector-u8-ref other index))))))))))

  (define-record-type* <unknown-client>
    (make-unknown-client~ parameter salt identity k x a A B K M1 M2)
    unknown-client?
    (parameter unknown-client-parameter)
    (salt unknown-client-salt)
    (identity unknown-client-identity)
    (k unknown-client-k)
    (x unknown-client-x)
    (a unknown-client-a)
    (A unknown-client-A~)
    (B unknown-client-B unknown-client-B!!)
    ;; K aka session key
    (K unknown-client-K~ unknown-client-K!)
    (M1 unknown-client-M1~ unknown-client-M1!)
    (M2 unknown-client-M2 unknown-client-M2!))

  (define unknown-client-A
    (lambda (client)
      (unknown-bytevector (unknown-client-A~ client))))

  (define (client-debug client)
    (list (list 'client 'K (unknown-bytevector (unknown-client-K~ client)))
          (list 'client 'M1 (unknown-bytevector (unknown-client-M1~ client)))
          (list 'client 'M2 (unknown-bytevector (unknown-client-M2 client)))))

  (define make-random-unknown
    (lambda (name length)
      (define out (make-unknown name length))
      (unknown-bytevector! out (bytevector-random length))
      out))

  (define make-unknown-client
    (lambda (parameter secret~ salt~ identity~ password~)
      (define secret (bytevector->unknown 'client-server secret~))
      (define salt (bytevector->unknown 'salt salt~))
      (define identity (bytevector->unknown 'identity identity~))
      (define password (bytevector->unknown 'password password~))

      (make-unknown-client~ parameter
                            salt
                            identity
                            (unknown-compute-k parameter)
                            (unknown-compute-x salt
                                               identity
                                               password)
                            secret ;; aka. a
                            (unknown-compute-A parameter secret)
                            #f
                            #f
                            #f
                            #f)))

  (define unknown-client-B!
    (lambda (client B~)
      (define B (bytevector->unknown 'client-B B~))

      ;; safeguard, assert B mod N is not zero
      (define i (assert (not (= 0
                                (mod (unknown-integer B)
                                     (unknown-integer
                                      (unknown-parameter-N
                                       (unknown-client-parameter client))))))))

      (define parameter (unknown-client-parameter client))

      (define S
        (unknown-client-compute-S
         (unknown-client-parameter client)
         (unknown-client-x client)
         (unknown-client-a client)
         B
         (let ((u (unknown-compute-u (unknown-client-A~ client) B)))
           (assert (not (= 0 (unknown-integer u))))
           u)))

      (unknown-client-B!! client B)
      (unknown-client-K! client (unknown-compute-K S))
      (unknown-client-M1! client
                          (unknown-compute-M1 'client
                                              parameter
                                              (unknown-client-identity client)
                                              (unknown-client-salt client)
                                              (unknown-client-A~ client)
                                              B
                                              (unknown-client-K~ client)))
      (unknown-client-M2! client
                          (unknown-compute-M2 'client parameter
                                              (unknown-client-A~ client)
                                              (unknown-client-M1~ client)
                                              (unknown-client-K~ client)))))

  (define unknown-client-check-M2?
    (lambda (client server-M2~)
      (define server-M2 (bytevector->unknown 'server-M2 server-M2~))

      (unknown-bytevector=?
       (unknown-bytevector (unknown-client-M2 client))
       (unknown-bytevector server-M2))))

  (define-record-type* <unknown-server>
    (make-unknown-server% parameter salt identity v b k B K M1 M2)
    unknown-server?
    (parameter unknown-server-parameter)
    (salt unknown-server-salt)
    (identity unknown-server-identity)
    (v unknown-server-v)
    (b unknown-server-b)
    (k unknown-server-k)
    (B unknown-server-B~ unknown-server-B!)
    (K unknown-server-K~ unknown-server-K!)
    (M1 unknown-server-M1 unknown-server-M1!)
    (M2 unknown-server-M2~ unknown-server-M2!))

  (define unknown-server-B
    (lambda (server)
      (unknown-bytevector (unknown-server-B~ server))))

  (define (server-debug server)
    (list (list 'server 'K (unknown-bytevector (unknown-server-K~ server)))
          (list 'server 'M1 (unknown-bytevector (unknown-server-M1 server)))
          (list 'server 'M2 (unknown-bytevector (unknown-server-M2~ server)))))

  (define unknown-server-M2
    (lambda (server)
      (unknown-bytevector (unknown-server-M2~ server))))

  (define unknown-client-M1
    (lambda (client)
      (unknown-bytevector (unknown-client-M1~ client))))

  (define make-unknown-server
    (lambda (parameter secret~ salt~ identity~ verifier~)
      (define secret (bytevector->unknown 'server-secret secret~))
      (define salt (bytevector->unknown 'salt salt~))
      (define identity (bytevector->unknown 'identity identity~))
      (define verifier (bytevector->unknown 'verifier verifier~))

      (make-unknown-server% parameter
                            salt
                            identity
                            verifier
                            secret
                            (unknown-compute-k parameter)
                            #f
                            #f
                            #f
                            #f)))

  (define unknown-server-A!
    (lambda (server A~)
      (define A (bytevector->unknown 'A A~))

      (define i (assert (not (= 0
                                (mod (unknown-integer A)
                                     (unknown-integer
                                      (unknown-parameter-N
                                       (unknown-server-parameter server))))))))

      (define parameter (unknown-server-parameter server))
      (define B (unknown-compute-B parameter
                                   (unknown-compute-k parameter)
                                   (unknown-server-v server)
                                   (unknown-server-b server)))

      (define u (unknown-compute-u A B))

      (define S
        (unknown-server-compute-S parameter
                                  (unknown-server-v server)
                                  A
                                  (unknown-server-b server)
                                  u))

      (unknown-server-B! server B)
      (unknown-server-K! server (unknown-compute-K S))

      (unknown-server-M1! server (unknown-compute-M1
                                  'server
                                  parameter
                                  (unknown-server-identity server)
                                  (unknown-server-salt server)
                                  A
                                  (unknown-server-B~ server)
                                  (unknown-server-K~ server)))
      (unknown-server-M2! server
                          (unknown-compute-M2 'server parameter
                                              A
                                              (unknown-server-M1 server)
                                              (unknown-server-K~ server)))))

  (define unknown-server-K
    (lambda (server)
      (unknown-bytevector (unknown-server-K~ server))))

  (define unknown-client-K
    (lambda (client)
      (unknown-bytevector (unknown-client-K~ client))))

  (define unknown-server-check-M1?
    (lambda (server client-M1~)
      (define client-M1 (bytevector->unknown 'client-M1 client-M1~))

      (unknown-bytevector=?
       (unknown-bytevector (unknown-server-M1 server))
       (unknown-bytevector client-M1))))

  ;; apply

  (define ~check-unknown-000
    (lambda ()
      (define salt (unknown-bytevector (make-random-unknown 'salt 25)))
      (define identity (unknown-bytevector (make-random-unknown 'identity 25)))
      (define password (unknown-bytevector (make-random-unknown 'password 25)))

      ;; The client compute an identifier based on salt, identity, and
      ;; password.
      (define verifier (make-unknown-client-verifier PARAMETER-2048
                                                     salt
                                                     identity
                                                     password))

      ;; the server knowns only about salt, identity, and verifier.
      (define server
        (make-unknown-server PARAMETER-2048
                             (unknown-bytevector (make-random-unknown 'server-secret 1536))
                             salt
                             identity
                             verifier))

      ;; Client knows about salt, identity, and password.
      (define client
        (make-unknown-client PARAMETER-2048
                             (unknown-bytevector (make-random-unknown 'client-secret 1536))
                             salt
                             identity
                             password))

      (unknown-server-A! server
                         (unknown-client-A client))

      (unknown-client-B! client
                         (unknown-server-B server))

      ;; (for-each pk (server-debug server))
      ;; (for-each pk (client-debug client))


      (and
       ;; client, and server know about K
       (equal? (unknown-server-K server)
               (unknown-client-K client))
       ;; client and server can verify that the other side knows about
       ;; the same K.

       ;; Server must verify the client proof of K first.
       (unknown-server-check-M1? server (unknown-client-M1 client))

       ;; The Client must also verify the server proof
       (unknown-client-check-M2? client (unknown-server-M2 server)))))

)
