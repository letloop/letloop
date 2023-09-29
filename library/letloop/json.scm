(library (letloop json)

  (export json-nesting-depth-limit
          json-null?
          json-error?
          json-error-reason
          json-read
          json-write
          jsonify
          unjson
          ~check-letloop-json
          )

  (import (chezscheme))
  (import (letloop r999))
  (import (scheme generator))


  (define pk
    (lambda args
      (display ";; " (current-error-port))
      (write args (current-error-port))
      (newline (current-error-port))
      (flush-output-port (current-error-port))
      (car (reverse args))))

  (include "letloop/json/body.scm")

  (define ~check-letloop-json
    (lambda ()
      (define (json->obj->json->obj filepath)
        ;; Take in JSON by filepath, convert it a scheme object; then
        ;; convert it to JSON string with json-write, and json-read it back
        ;; to a Scheme object. This is made so, to be sure it is possible to
        ;; read original JSON text, and that what json-write produce can
        ;; also be read.

        ;; The output is a Scheme object

        (define (call-with-input-string string proc)
          (call-with-port (open-input-string string) proc))

        (define (call-with-output-string proc)
          (let ((port (open-output-string)))
            (proc port)
            (let ((string (get-output-string port)))
              (close-port port)
              string)))

        (call-with-input-string
         (call-with-output-string
          (lambda (port)
            (json-write (call-with-input-file filepath json-read) port)))
         (lambda (port)
           (json-read port))))

      (define LETLOOP_ROOT (let ((LETLOOP_ROOT (getenv "LETLOOP_ROOT")))
                             (unless LETLOOP_ROOT
                               (format #t "You need to enter the loop!")
                               (exit 42))
                             LETLOOP_ROOT))

      (define json-test-suite (string-append LETLOOP_ROOT "/data/json-test-suite/"))
      (define checks (directory-list json-test-suite))

      (define on-error
        (lambda (exc error)
          (write exc)
          (write error)
          ;; sloppy: check that there is an expected error, if it is the
          ;; case let the test pass green.
          (if (file-regular? error)
              #t
              (begin
                ;; (newline)
                ;; (write exc)
                ;; (newline)
                #f))))

      ;; (define outxy raise)

      (define (dodo name)
        (let* ((input (string-append json-test-suite "/" name "/input.json"))
               (output (string-append json-test-suite "/" name "/output.scm"))
               (error (string-append json-test-suite "/" name "/error.txt"))
               (outxy (guard (exc (else (on-error exc error)))
                             (display "** ")
                             (display name)
                             (display " => ")
                             (let ((obj (json->obj->json->obj input)))
                               (equal? obj (pk (call-with-input-file output read)))))))
          (display outxy)
          (newline)
          outxy))

      (define (every predicate? objects)
        (if (null? objects)
            #t
            (if (predicate? (car objects))
                (every predicate? (cdr objects))
                #f)))

      (every values (map dodo checks))))

  )
