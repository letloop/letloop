(library (letloop http)

  (export http-error?
          http-error-message
          http-error-payload
          http-request-read
          http-request-write
          http-response-read
          http-response-write

          ;; XXX: Disable tests
          #;~check-letloop-http
          )

  (import (chezscheme)
          (letloop r999))

  (define pk
    (lambda args
      (write args)(newline)
      (flush-output-port)
      (car (reverse args))))

  ;; <http-error>

  (define-record-type* <http-error>
    (make-http-error message payload)
    http-error?
    (message http-error-message)
    (payload http-error-payload))

  ;; helpers

  (define raise-unexpected-end-of-file
    (lambda ()
      (raise (make-http-error "Unexpected end of file" (list 'unexpected-end-of-file)))))

  (define raise-invalid
    (lambda (uid)
      (raise (make-http-error "Invalid" (list 'invalid uid)))))

  (define byte-space 32)
  (define byte-carriage-return 13)
  (define byte-linefeed 10) ;; aka. newline

  (define generator->list
    (lambda (generator)
      (let loop ((out '()))
        (let ((object (generator)))
          (if (eof-object? object)
              (reverse out)
              (loop (cons object out)))))))

  (define every
    (lambda (predicate? objects)
      (if (null? objects)
          #t
          (if (predicate? (car objects))
              (every predicate? (cdr objects))
              #f))))

  (define (bytevector-append . bvs)
    (assert (every bytevector? bvs))
    (let* ((total (apply fx+ (map bytevector-length bvs)))
           (out (make-bytevector total)))
      (let loop ((bvs bvs)
                 (index 0))
        (unless (null? bvs)
          (bytevector-copy! (car bvs) 0 out index (bytevector-length (car bvs)))
          (loop (cdr bvs) (fx+ index (bytevector-length (car bvs))))))
      out))

  (define generator->bytevector
    (lambda (generator n*)
      (lambda ()
        (if (fxzero? n*)
            (eof-object)
            (let loop ((n n*)
                       (out '()))
              (if (fxzero? n)
                  (begin (set! n* 0) (u8-list->bytevector (reverse out)))
                  (loop (fx- n 1) (cons (generator) out))))))))

  (define http-line-read
    (lambda (generator)
      (let loopx ((out '()))
        (let ((byte (generator)))
          (cond
           ((and (fx=? byte byte-linefeed) (fx=? (car out) byte-carriage-return))
            (reverse (cdr out)))
           ;; linefeed may not appear in the middle of request-line,
           ;; response-line, or header line.
           ((and (fx=? byte byte-linefeed) (not (fx=? (car out) byte-carriage-return)))
            (raise-invalid 1))
           ((eof-object? byte)
            (raise-unexpected-end-of-file))
           (else (loopx (cons byte out))))))))


  (define http-headers-read
    (lambda (generator)

      (define massage*
        (lambda (chars)
          (let loop ((chars chars))
            (if (null? chars)
                '()
                (if (char=? (car chars) #\space)
                    (loop (cdr chars))
                    chars)))))

      (define massage
        (lambda (string)
          (let loopx ((chars (reverse (massage* (reverse (massage* (string->list string))))))
                      (key '()))
            (if (null? chars)
                (raise-invalid 5)
                (let ((char (car chars)))
                  (if (char=? char #\:)
                      (cons (string->symbol (string-downcase (list->string (reverse (massage* key)))))
                            (string-downcase (list->string (massage* (reverse (massage* (reverse (cdr chars))))))))
                      (loopx (cdr chars) (cons (car chars) key))))))))

      (let loopy ((out '()))
        (let ((bytes (http-line-read generator)))
          (if (null? bytes)
              (reverse (map (lambda (x) (massage (utf8->string (apply bytevector x)))) out))
              (loopy (cons bytes out)))))))

  (define http-chunked-read
    (lambda (generator)

      (define massage
        (lambda (generator size)
          (let ((bytevector (make-bytevector size)))
            (let loop ((index 0))
              (unless (fx=? index size)
                (bytevector-u8-set! bytevector index (generator))
                (loop (fx+ index 1))))
            bytevector)))

      (define continue
        (lambda ()
          (let ((chunk-size (string->number (utf8->string (u8-list->bytevector (http-line-read generator))) 16)))
            (unless chunk-size
              (raise-invalid 11))
            (if (fxzero? chunk-size)
                (begin
                  (set! continue eof-object)
                  (eof-object))
                (let ((bytevector (massage generator chunk-size)))
                  (http-line-read generator)
                  bytevector)))))

      (lambda ()
        (continue))))

  (define http-body-read
    (lambda (generator headers)
      (let ((content-length (let ((value (assq 'content-length headers)))
                              (if value
                                  (string->number (cdr value))
                                  #f))))
        (if content-length
            (generator->bytevector generator content-length)
            (let ((chunked? (let ((value (assq 'transfer-encoding headers)))
                              (and value (string=? (cdr value) "chunked")))))
              (if chunked?
                  (http-chunked-read generator)
                  eof-object))))))

  ;; http-request-read

  (define http-request-read
    (lambda (generator)
      ;; GENERATOR must yield one byte at a time

      (define request-line-read
        (lambda (generator)

          (define massage
            (lambda (bytes)
              (let loop ((bytes bytes)
                         (chunk '())
                         (out '()))
                (if (null? bytes)
                    (if (null? chunk)
                        (raise-invalid 3)
                        (reverse (cons (utf8->string (apply bytevector (reverse chunk))) out)))
                    (let ((byte (car bytes)))
                      (if (and (fx=? byte byte-space) (not (null? chunk)))
                          (loop (cdr bytes) '() (cons (utf8->string (apply bytevector (reverse chunk))) out))
                          (loop (cdr bytes) (cons byte chunk) out)))))))

          (let ((strings (massage (http-line-read generator))))
            (unless (fx=? (length strings) 3)
              (raise-invalid 2))
            (values (string->symbol (car strings)) (cadr strings) (string->symbol (caddr strings))))))

      (guard (ex (else (values #f #f #f #f #f)))
        (call-with-values (lambda () (request-line-read generator))
          (lambda (method uri version)
            (call-with-values (lambda () (http-headers-read generator))
              (lambda (headers)
                (values method uri version headers (http-body-read generator headers)))))))))

  ;; http-request-write

  (define http-request-write
    (lambda (accumulator method target version headers body)
      (let* ((request-line (format #f "~a ~a ~a\r\n" method target version))
             (headers (apply string-append (map (lambda (x) (format #f "~a: ~a\r\n" (car x) (cdr x))) headers))))
        (accumulator (string->utf8 (string-append request-line headers "\r\n")))
        (let loop ()
          (let ((bytevector (body)))
            (unless (eof-object? bytevector)
              (accumulator bytevector)
              (loop)))))))

  ;; http-response-read

  (define http-response-read
    (lambda (generator)
      ;; GENERATOR must yield one byte at a time

      (define response-line-read
        (lambda (generator)

          (define massage
            (lambda (bytes)
              (let loop ((bytes bytes)
                         (chunk '())
                         (out '()))
                (if (null? bytes)
                    (if (null? chunk)
                        (raise-invalid 7)
                        (reverse (cons (utf8->string (apply bytevector (reverse chunk))) out)))
                    (let ((byte (car bytes)))
                      (if (and (fx=? byte byte-space) (not (null? chunk)))
                          (loop (cdr bytes) '() (cons (utf8->string (apply bytevector (reverse chunk))) out))
                          (loop (cdr bytes) (cons byte chunk) out)))))))

          (let ((strings (massage (http-line-read generator))))
            (values (string->symbol (car strings)) (string->number (cadr strings)) #f))))

      (call-with-values (lambda () (response-line-read generator))
        (lambda (version code reason)
          (call-with-values (lambda () (http-headers-read generator))
            (lambda (headers)
              (values version code reason headers (http-body-read generator headers))))))))

  ;; http-response-write

  (define http-response-write
    (lambda (accumulator version code reason headers body)

      (define massage*
        (lambda (generator)
          (let loop ((out '()))
            (let ((bytevector (generator)))
              (if (eof-object? bytevector)
                  (apply bytevector-append (reverse out))
                  (loop (cons bytevector out)))))))

      (define tranfer-encoding-chunked?
        (lambda (pair)
          (and (eq? (car pair) 'tranfer-encoding)
               (string=? (cdr pair) "chunked"))))

      (define massage**
        (lambda (headers content-length)
          (cond
           ((null? headers) (list (cons 'content-length content-length)))
           ((tranfer-encoding-chunked? (car headers)) (cons (cons 'content-length content-length) (cdr headers)))
           (else (cons (car headers) (massage** (cdr headers) content-length))))))

      (define massage
        (lambda (headers body)
          (let ((bytevector body))
            (values (massage** headers (bytevector-length bytevector)) bytevector))))

      (assert (or (pair? headers) (null? headers)))
      (assert (bytevector? body))

      (call-with-values (lambda () (massage headers body))
        (lambda (headers body)
          (let* ((response-line (format #f "~a ~a ~a\r\n" version code reason))
                 (headers (apply string-append (map (lambda (x) (format #f "~a: ~a\r\n" (car x) (cdr x))) headers))))
            (accumulator (string->utf8 (string-append response-line headers "\r\n")))
            (accumulator body))))))

  ;; there is simpler way to do the following, but I will need it later

  (define ~check-letloop-http
    (lambda ()

      (define call-with-binary-input-file
        (lambda (string proc)
          (let ((port (open-file-input-port string)))
            (call-with-values (lambda () (proc port))
              (lambda args
                (close-port port)
                (apply values args))))))


      (define read-bytevector
        (lambda (port)
          (let loop ((out '()))
            (let ((byte (get-u8 port)))
              (if (eof-object? byte)
                  (apply bytevector (reverse out))
                  (loop (cons byte out)))))))

      (define call-with-input-string
        (lambda (string proc)
          (let ((port (open-input-string string)))
            (call-with-values (lambda () (proc port))
              (lambda args
                (close-port port)
                (apply values args))))))

      ;;

      (define (list->generator objects)
        (lambda ()
          (if (null? objects)
              (eof-object)
              (let ((object (car objects)))
                (set! objects (cdr objects))
                object))))

      (define (make-bytevector-accumulator)
        (let ((out '()))
          (lambda (bytevector)
            (if (eof-object? bytevector)
                (apply bytevector-append (reverse out))
                (set! out (cons bytevector out))))))
      ;;

      (define request-string->scheme
        (lambda (string)
          (call-with-values (lambda () (http-request-read (list->generator (bytevector->u8-list (string->utf8 string)))))
            (lambda (method uri version headers body)
              (let loop ((out '()))
                (let ((bytevector (body)))
                  (if (eof-object? bytevector)
                      (list method uri version headers (utf8->string (apply bytevector-append (reverse out))))
                      (loop (cons bytevector out)))))))))


      (define bytevector->generator
        (lambda (bytevector)
          (list->generator (bytevector->u8-list bytevector))))

      (define request-scheme->string
        (lambda (method uri version headers body)
          (let ((accumulator (make-bytevector-accumulator)))
            (http-request-write accumulator method uri version headers (list->generator (list (string->utf8 body))))
            (let ((bytevector (accumulator (eof-object))))
              (utf8->string bytevector)))))

      (define (request-massage string)
        (request-string->scheme (apply request-scheme->string (request-string->scheme string))))



      (define response-string->scheme
        (lambda (string)
          (call-with-values (lambda () (http-response-read (list->generator (bytevector->u8-list (string->utf8 string)))))
            (lambda (version code reason headers body)
              (let loop ((out '()))
                (let ((bytevector (body)))
                  (if (eof-object? bytevector)
                      (list version code reason headers (utf8->string (apply bytevector-append (reverse out))))
                      (loop (cons bytevector out)))))))))

      (define response-scheme->string
        (lambda (version code reason headers body)
          (let ((accumulator (make-bytevector-accumulator)))
            (http-response-write accumulator version code reason headers (list->generator (list (string->utf8 body))))
            (let ((bytevector (accumulator (eof-object))))
              (utf8->string bytevector)))))

      (define (response-massage string)
        (response-string->scheme (apply response-scheme->string (response-string->scheme string))))

      ;;

      (define (massage* port)
        (format #f "\"~a\"" (apply string-append
                                   (map (lambda (c) (if (char=? c #\") "\\\"" (list->string (list c))))
                                        (string->list (utf8->string (read-bytevector port)))))))

      (define error-display
        (lambda (e)
          (if (condition? e)
              (display-condition e)
              (display e))
          (newline)))

      (define check-one-request
        (lambda (filepath)
          (call-with-binary-input-file
           filepath
           (lambda (port)
             (call-with-input-string (massage* port)
                                     (lambda (port)
                                       (guard (e (else (error-display e) #f))
                                              (request-massage (read port)))))))))


      (define check-one-response
        (lambda (filepath)
          (call-with-binary-input-file
           filepath
           (lambda (port)
             (call-with-input-string (massage* port)
                                     (lambda (port)
                                       (guard (e (else (error-display e) #f))
                                              (response-massage (read port)))))))))

      (define LETLOOP_ROOT (let ((LETLOOP_ROOT (getenv "LETLOOP_ROOT")))
                             (unless LETLOOP_ROOT
                               (format #t "You need to enter the loop!")
                               (exit 42))
                             LETLOOP_ROOT))

      (define http-test-suite (string-append LETLOOP_ROOT "/data/http11-test-suite/"))

      (define (check-request case fail)
        (let ((input (format #f "~a/requests/~a/input.txt" http-test-suite case)))
          (format #t "** Checking ~a\n" input)
          (let ((actual (check-one-request input)))
            (if actual
                (let ((expected (call-with-input-file (format #f "~a/requests/~a/output.scm" http-test-suite case) read)))
                  (when (not (equal? actual expected))
                    (pretty-print actual)
                    (format #t "failed with: ~a !\n" input)
                    (flush-output-port)
                    (fail #f)))
                (unless (file-regular? (format "~a/requests/~a/error" http-test-suite case))
                  (format #t "error with: ~a !\n" input)
                  (flush-output-port)
                  (fail #f))))
          #t))

      (define request
        (call/cc (lambda (fail)
                   (length (map
                            (lambda (case)
                              (check-request case fail))
                            (directory-list
                             (string-append http-test-suite
                                            "/requests")))))))

      (define (check-response case fail)
        (let ((input (format #f "~a/responses/~a/input.txt" http-test-suite case)))
          (format #t "** Checking ~a\n" input)
          (let ((actual (check-one-response input)))
            (if actual
                (let ((expected (call-with-input-file (format #f "~a/responses/~a/output.scm" http-test-suite case) read)))
                  (when (not (equal? actual expected))
                    (pretty-print actual)
                    (format #t "failed with: ~a !\n" input)
                    (flush-output-port)
                    (fail #f)))
                (unless (file-regular?
                         (format "~a/responses/~a/error"
                                 http-test-suite case))
                  (format #t "error with: ~a !\n" input)
                  (flush-output-port)
                  (fail #f)))))
        #t)

      (define response
        (call/cc (lambda (fail)
                   (length
                    (map
                     (lambda (case) (check-response case fail))
                     (directory-list
                      (string-append http-test-suite
                                     "/responses")))))))

      (assert (and request response)))))
