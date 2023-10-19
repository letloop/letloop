(library (letloop www)
  (export www-request www-host-read www-uri-read www-query-read
          www-form-urlencoded-read
          ~check-www-000 ~check-www-001 ~check-www-002 ~check-www-002-bis
          ~check-www-003)
  (import (chezscheme) (letloop http) (letloop match) (scheme generator))

  (define pk
    (lambda args
      (display ";; ")
      (write args)
      (newline)
      (flush-output-port)
      (car (reverse args))))

  (define www-request
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
      (call-with-values (lambda () (www-request 'GET "https://hyper.dev" '() (bytevector)))
        (lambda (code headers body)
          (= code 200)))))

  (define ~check-www-001
    (lambda ()
      (call-with-values (lambda () (www-request 'GET "https://hyper.dev" '(("\"foobar" . "ok") (x-letloop . "yes")) (bytevector)))
        (lambda (code headers body)
          (= code 200)))))

  (define string->list*
    (lambda (x)
      (if x (string->list x) '())))
  
  (define percent-decode
    (lambda (string)
      (let loop ((chars (string->list* string))
                 (out '()))
        (match chars
          (() (list->string (reverse out)))
          ((#\+ ,rest ...)
           (loop rest (cons #\space out)))
          ((#\% ,a ,b ,rest ...)
           (loop rest (cons
                       (integer->char
                        (string->number
                         (list->string (list a b))
                         16))
                       out)))
          ((,char . ,rest) (loop rest (cons char out)))))))

  (define www-form-urlencoded-read
    ;; content-type: application/x-www-form-urlencoded
    (lambda (string)

      (define form-item-split
        (lambda (string)
          (let loop ((chars (string->list* string))
                     (out '()))
            (match chars
              (() (list (string->symbol (list->string (reverse out)))))
              ((#\= . ,rest) (cons (string->symbol (percent-decode (list->string (reverse out))))
                                   (percent-decode (list->string rest))))
              ((,char . ,rest) (loop rest (cons char out)))))))

      (let loop ((chars (string->list* string))
                 (out '(())))
        (match chars
          (() (reverse (cons (form-item-split (list->string (reverse (car out)))) (cdr out))))
          ((#\& . ,rest) (loop (cdr chars)
                               (cons* (list)
                                      (form-item-split (list->string (reverse (car out))))
                                      (cdr out))))
          ;; very rare case, TODO: spec ref.
          ((#\; . ,rest) (loop (cdr chars)
                               (cons* (list)
                                      (form-item-split (list->string (reverse (car out))))
                                      (cdr out))))
          ((,char . ,rest) (loop (cdr chars) (cons (cons char (car out)) (cdr out))))))))

  (define www-request-line-uri-split
    (lambda (string)
      (let loop ((chars (string->list* string))
                 (out '()))
        (match chars
          (() (reverse out))
          ((#\/ . ,rest) (loop rest (cons* (list)
                                           (percent-decode
                                            (list->string
                                             (reverse (car out))))
                                           (cdr out))))
          ((,char . ,rest) (loop rest (cons (cons char (car out))
                                            (cdr out))))))))

  (define www-query-read www-form-urlencoded-read)

  (define string-find
    (lambda (string char)
      (let loop ((chars (string->list* string))
                 (index 0))
        (if (null? chars)
            #f
            (if (char=? char (car chars))
                index
                (loop (cdr chars) (fx+ index 1)))))))

  (define www-uri-read
    (lambda (string)

      (define path-split
        (lambda (string)
          (when (and (not (string=? string "")) (char=? #\/ (string-ref string 0)))
            (set! string (substring string 1 (string-length string))))

          (when (and (not (string=? string "")) (char=? #\/ (string-ref string (fx- (string-length string) 1))))
            (set! string (substring string 0 (fx- (string-length string) 1))))

          (if (string=? "" string)
              '()
              (let loop ((chars (string->list* string))
                         (out '(())))
                (match chars
                  (() (reverse (cons (percent-decode (list->string (reverse (car out))))
                                     (cdr out))))
                  ((#\/ . ,rest) (loop rest (cons* '()
                                                   (percent-decode (list->string (reverse (car out))))
                                                   (cdr out))))
                  ((,char . ,rest) (loop rest (cons (cons char (car out))
                                                    (cdr out)))))))))

      (define path #f)
      (define query #f)
      (define fragment #f)

      (let ((index (string-find string #\#)))
        (when index
          (set! fragment (substring string (fx+ index 1) (string-length string)))
          (set! string (substring string 0 index))))

      (let ((index (string-find string #\?)))
        (when index
          (set! query (substring string (fx+ index 1) (string-length string)))
          (set! string (substring string 0 index))))

      (set! path string)

      (values (and path (path-split path)) (and query (www-query-read query)) fragment)))

  (define www-host-read
    (lambda (string)
      (define port #f)
      
      (define index (string-find string #\:))

      (when (and index (not (= index (string-length string))))
        (set! port (string->number
                    (substring string (+ index 1)
                               (string-length string)))))
      
      (when index
        (set! string (substring string 0 index)))
      
      (let loop ((chars (string->list* string))
                 (out '(())))
        (match chars
          (()  (cons (reverse (cons (list->string
                                     (reverse (car out)))
                                    (cdr out)))
                     port))
          ((#\. . ,rest) (loop rest
                               (cons* '()
                                      (list->string (reverse (car out)))
                                      (cdr out))))
          ((,char . ,rest) (loop rest (cons (cons char (car out))
                                            (cdr out))))))))

  (define ~check-www-002
    (lambda ()
      (call-with-values (lambda ()
                          (www-uri-read
                           "/foo/b%33r/baz/?q=world+peace&s=1&now#README"))
        (lambda uri
          (assert (equal? uri  '(("foo" "b3r" "baz")
                                 ((q . "world peace")
                                  (s . "1")
                                  (now))
                                 "README")))))))

  (define ~check-www-002-bis
    (lambda ()
      (call-with-values (lambda ()
                          (www-uri-read
                           ""))
        (lambda uri
          (assert (equal? uri  '(()
                                 #f
                                 #f)))))))
  
  (define ~check-www-003
    (lambda ()
      (assert (equal? (cons '("foo" "bar" "baz" "qux" "example") 9999)
                      (www-host-read "foo.bar.baz.qux.example:9999")))))
  

  )
