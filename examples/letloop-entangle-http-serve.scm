(import (chezscheme) (letloop http) (letloop entangle))



(define pk
  (lambda args
    (display ";; ")(write args)(newline)
    (flush-output-port)
    (car (reverse args))))

(define http-read-bytes
  (lambda (read)
    (define index 0)
    (define bv (bytevector))

    (lambda ()
      (assert (bytevector? bv))
      (when (fx=? (bytevector-length bv) index)
        (set! bv (read))
        (set! index 0))
      (assert (bytevector? bv))
      (if (and bv (< index (bytevector-length bv)))
          (let ((byte (bytevector-u8-ref bv index)))
            (set! index (fx+ index 1))
            byte)
          (error 'socket-closed bv index )))))

(define message "Hello, World!")

(define handle
  (lambda (read write close)
    (guard (ex (else (pk 'handle (apply format #f (condition-message ex)
                                        (condition-irritants ex)))))
      (call-with-values (lambda () (http-request-read (http-read-bytes read)))
        (lambda (method uri version headers body)
          (when method
            (http-response-write write "HTTP/1.1" 200 "Found" '()
                                 (string->utf8 message))))))
    (close)))

(define main
  (lambda ()
    (call-with-values (lambda () (entangle-tcp-serve "0.0.0.0" port))
      (lambda (accept close)
        (format #t "HTTP server running at http://127.0.0.1:~a\n" port)
        (let loop ()
          (guard (ex (else
                      (pk 'accept (apply format #f
                                         (condition-message ex)
                                         (condition-irritants ex)))))
            (call-with-values accept
              (lambda (read write close)
                (entangle-spawn 
                                (lambda ()
                                  (handle read write close))))))
          (loop))))))


(define port (string->number (cadr (command-line))))

(make-entangle)
(entangle-spawn main)
(entangle-run)
