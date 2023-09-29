(library (letloop www)
  (export http-request ~check-www-000 ~check-www-001)
  (import (chezscheme) (letloop http) (scheme generator))

  (define pk
    (lambda args
      (write args)
      (newline)
      (car (reverse args))))

  (define http-request
    (lambda (method url headers body)

      (define command-run
        (lambda (command input)
          (call-with-values (lambda () (open-process-ports command))
            (lambda (stdin stdout stderr pid)
              (put-bytevector stdin input)
              (close-port stdin)
              (let ((out (get-bytevector-all stdout)))
                (close-port stdout)
                (close-port stderr)
                out)))))

      (guard (ex (else (format #f (condition-message ex) (condition-irritants ex)) (values #f #f #f)))
        (define headers* (map (lambda (x) (format #f "~a: ~a" (car x) (cdr x))) headers))
        (call-with-values (lambda ()
                            (http-response-read
                             (bytevector->generator
                              (command-run (format #f "curl --http1.1 -X ~s -i ~a ~{-H ~s ~}" method url headers*)
                                           body))))
          (lambda (version code reason headers body)
            (values code headers (body)))))))


  (define ~check-www-000
    (lambda ()
      (call-with-values (lambda () (http-request 'GET "https://hyper.dev" '() (bytevector)))
        (lambda (code headers body)
          (= code 200)))))

  (define ~check-www-001
    (lambda ()
      (call-with-values (lambda () (http-request 'GET "https://hyper.dev" '(("\"foobar" . "ok") (x-letloop . "yes")) (bytevector)))
        (lambda (code headers body)
          (= code 200)))))

  )
