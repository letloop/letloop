#!chezscheme
(import (chezscheme))
(import (letloop match))
(import (letloop lsm1 okvs))
(import (letloop commonmark))
(import (letloop entangle))
(import (letloop http))
(import (letloop html))
(import (letloop byter))
(import (scheme generator))
(import (letloop www))
(import (letloop literally))


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

(define render
  (lambda (title body)
    `(html
      (@ (lang "en"))
      (head
       (meta (@ (charset "utf-8")))
       (meta (@ (name "viewport")
                (content "width=device-width, initial-scale=1")))
       (link (@ (href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css")
                (rel "stylesheet")
                (integrity "sha384-T3c6CoIi6uLrA9TneNEoa7RxnatzjcDSCmG1MXxSR1GAsXEV/Dwwykc2MPK8M2HN")
                (crossorigin "anonymous")))
       (link (@ (href "https://hyper.dev/static/main.css")
                (rel "stylesheet")))
        (title ,title))
      (body
       (canvas (@ (id "stars")
                  (width "1720")
                  (height "1073")))
       (div (@ (id "root")) ,@body)
       (script (@ (type "text/javascript")
                  (src "https://hyper.dev/static/stars.js")))))))


(define literally-read
  (lambda (string)
    (pk (literally (open-input-string string)))))

(define message "Hello, World!")

(define index-body
  `(body (h1 "be.hyper.dev")
         (form (@ (method "POST"))
               (p (input (@ (type text) (name "pretty-name"))))
               (p (input (@ (type submit) (value "Create that page")))))))

(define generator->bytevector
  (lambda (g)
    (byter-concatenate (generator->list g))))

(define call-with-database
  (lambda (proc . args)
    (define db (make-okvs "wiki-symbolic.db"))
    (call-with-values (lambda () (call-with-okvs-transaction db
                                   (lambda (tx) (apply proc tx args))))
      (lambda args
        (okvs-close db)
        (apply values args)))))

(define maybe-set!
  (lambda (b u)
    (call-with-database
     (lambda (txn)
       (if (okvs-query txn b)
           #f
           (begin
             (okvs-set! txn b u)
             #t))))))

(define bytevector-random
  (lambda (length)
    (u8-list->bytevector
     (map (lambda (x) (random 256)) (iota length)))))

(define ref
  (lambda (s)
    (call-with-database
     (lambda (txn)
       (define o (okvs-query txn (string->utf8 s)))
       (if o (utf8->string o) #f)))))

(define hexify
  (lambda (x)
    (format #f "~{~x~}" (bytevector->u8-list x))))

(define http-redirect
  (lambda (write location)
    (http-response-write write "HTTP/1.1" 301 "Redirection"
                         `((location . ,location))
                         (string->utf8 (format #f "Rediction to ~a" location)))))

(define normalized
  (lambda (name)


    (define drop-while
      (lambda (objects predicate?)
        (let loop ((objects objects))
          (if (null? objects)
              (list)
              (if (predicate? (car objects))
                  (loop (cdr objects))
                  objects)))))

    (define boring?
      (lambda (object)
        (or (null? object)
            (char=? (car object) #\-))))

    (define chars (string->list (string-downcase name)))
    (let loop ((chars chars)
               (out '()))
        (if (null? chars)
            (list->string
             (apply
              append (drop-while
                      (reverse (drop-while out boring?)) boring?)))
            (let ((x (car chars))
                  (previous (and (not (null? out))
                                 (not (null? (car out)))
                                 (caar out))))
              (if (member x (string->list "0123456789abcdefghijklmnopqrstuvwxyz=!"))
                  (loop (cdr chars) (cons (list x) out))
                  (if (or (and previous (char=? previous #\λ))
                          (and previous (char=? previous #\-)))
                      (loop (cdr chars) (cons (list) out))
                      (loop (cdr chars) (cons (list #\-) out)))))))))


(define code "# Library `(tools)`

## Procedure `(main)`

```scheme
(define main (lambda () \"echo alpha bravo symbolic wiki\"))
```

")

(define frob
  (lambda (code)
    (make-application code)))

(define make-application
  (lambda (code)


    (define massage
      (lambda (exp)
        (match exp
          ((library ,name
             (export main) ,exp ...)
           `(module (main)
              ,exp ...)))))

    ;; TODO: rename import and module
    (define myenv
      (let ((env (environment '(chezscheme))))
        (copy-environment env #t)))

    ;; TODO: handle unreadable, error to report to the user
    (define target (massage code))
    (define i (eval target myenv))
    (define main (eval 'main myenv))

    main))

(define handle
  (lambda (read write close)
    (guard (ex (else (pk 'handle (apply format #f (condition-message ex)
                                        (condition-irritants ex)))))
      (call-with-values (lambda () (http-request-read (http-read-bytes read)))
        (lambda (method uri version headers body)
          (call-with-values (lambda () (www-uri-read uri))
            (lambda (path query fragment)
              (match (pk 'request (cons method path))

                ((GET "u" ,pretty-name ,uid)
                 (http-response-write
                  write "HTTP/1.1" 200 "Found" '()
                  (string->utf8
                   (html-write
                    (render "be.hyper.dev"
                            `(body
                              ,@(cdr (pk (html-read (commonmark-read code))))))))))

                ((GET "u" ,pretty-name ,uid "editor")
                 (http-response-write
                  write "HTTP/1.1" 200 "Found" '()
                  (string->utf8
                   (html-write
                    (render "be.hyper.dev"
                            `(body
                              (form (@ (method "POST"))
                                    (h1 (@ (sytle "color: rgb(250 51 132)"))
                                        "Let's muse about " (code ,pretty-name) "!")
                                    (a
                                     (@
                                      (href
                                       ,(format #f
                                               "/u/~a/~a1/apply/" pretty-name uid)))
                                      "apply")
                                    (textarea
                                     (@ (rows 43)
                                        (name code)
                                        (style "font-family: monospace, mono")
                                        (class "form-control"))
                                     ,code)
                                    (button (@ (class "form-control"))
                                            "Save, please!"))))))))
                ((GET "u" ,pretty-name ,uid "apply")
                 (http-response-write
                  write "HTTP/1.1" 200 "Found" '()
                  (string->utf8
                   (html-write
                    (render "be.hyper.dev"
                            `(body
                              ,((frob (literally-read code)))))))))
                ((POST "u" ,pretty-name ,uid "editor")
                 (set! code
                   (cdr (assq 'code
                              (www-form-urlencoded-read
                               (utf8->string
                                (generator->bytevector body))))))

                 (http-redirect write (string-append "/u/" pretty-name "/" uid "/editor/")))
                ((GET)
                 (http-response-write
                  write "HTTP/1.1" 200 "Found" '()
                  (string->utf8
                   (html-write
                    (render "be.hyper.dev"
                            index-body)))))
                ((POST)
                 (define pretty-name
                   (normalized
                    (cdr (assq 'pretty-name
                               (www-form-urlencoded-read
                                (utf8->string
                                 (generator->bytevector body)))))))
                 (define uid (number->string (random (expt 2 64)) 36))
                 (http-redirect write (string-append "/u/" pretty-name "/" uid "/editor/")))
                (,_
                 (http-response-write write "HTTP/1.1" 404 "Found" '()
                                      (string->utf8
                                       (html-write
                                        (render "be.hyper.dev"
                                                (list '(h1 "Nothing here")))))))))))))


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
