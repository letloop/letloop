(library (letloop literally)

  (export literally literally-format ~check-literally-000
          letloop-literally)

  (import (chezscheme)
          (letloop match)
          (only (srfi srfi-13) string-prefix?))

  (define pk
    (lambda args
      (write args)
      (newline)
      (flush-output-port)
      (car (reverse args))))

  (define-syntax assume
    (syntax-rules ()
      ((assume test message)
       (let ((test* test))
         (if test*
             test*
             (error 'literally message))))))

  (define call-with-input-string
    (lambda (string proc)
      (define port (open-input-string string))
      (call-with-values (lambda () (proc port))
        (lambda args
          (close-port port)
          (apply values args)))))

  (define string->sexp
    (lambda (string)
      (call-with-input-string
       string
       (lambda (port)
         (let loop ((out '()))
           (define x (read port))
           (if (eof-object? x)
               (reverse out)
               (loop (cons x out))))))))

  (define literally
    (lambda (port)
      (define header-title (get-line port))
      (define v0 (assume (string-prefix? "# Library `" header-title)
                         "Invalid markdown file, header title is not: # Library `"))
      (define name (call-with-input-string
                    (substring header-title
                               (string-length "# Library `")
                               (- (string-length header-title) 1))
                    read))
      (define code+procedures
        (let loop ((code '())
                   (procedures '()))
          (define line (get-line port))
          (cond
           ((eof-object? line) (cons (reverse code) procedures))
           ((string-prefix? "## Procedure `" line)
            (let ((procedure
                   (car (call-with-input-string
                         (substring line
                                    (string-length "## Procedure `")
                                    (- (string-length line) 1))
                         read))))
              (loop code (cons procedure procedures))))
           ((string-prefix? "```scheme" line)
            (let loopy ((lines '()))
              (let ((line (get-line port)))
                (if (string-prefix? "```" line)
                    (loop (cons (string->sexp
                                 (apply string-append (reverse lines)))
                                code)
                          procedures)
                    (loopy (cons line lines))))))
           (else (loop code procedures)))))

      (define body (apply append (car code+procedures)))
      (define public-procedures (cdr code+procedures))

      ;; add checks, and benchmarks
      (define tildes
        (let loop ((body body)
                   (out '()))
          (if (null? body)
              (reverse out)
              (match (car body)
                ((define ,name (lambda ,rest ...))
                 (if (or (string-prefix? "~check-" (symbol->string name))
                         (string-prefix? "~benchmark-" (symbol->string name)))
                     (loop (cdr body) (cons name out))
                     (loop (cdr body) out)))
                (,e (loop (cdr body) out))))))

      (define exports (append public-procedures tildes))


      `(library ,name
         (export ,@exports)

         ,@body)))

  (define literally-format-string
    (lambda (sexp)
      (define out (open-output-string))
      (pretty-print sexp out)
      (get-output-string out)))

  (define literally-format
    (case-lambda
      ((sexp) (literally-format-string sexp))
      ((port sexp) (pretty-print sexp port))))

  (define ~check-literally-000
    (lambda ()
      (define my-markdown-library "# Library `(my-markdown-library)`

```scheme
(import (chezscheme))
```

## Procedure `(foo bar qux)`

```scheme
(define foo (lambda (bar qux) 42))

(define ~check-my-markdown-library (lambda () #t))

(define ~benchmark-my-markdown-library (lambda () (bar 1337)))

```")
      (define expected '(library (my-markdown-library)
                          (export foo ~check-my-markdown-library ~benchmark-my-markdown-library)
                          (import (chezscheme))
                          (define foo (lambda (bar qux) 42))
                          (define ~check-my-markdown-library (lambda () #t))
                          (define ~benchmark-my-markdown-library (lambda () (bar 1337)))))
      (equal? (literally (open-input-string my-markdown-library))
              expected)))

  (define letloop-literally
    (lambda (filepath)

      (define basename
        (lambda (string)
          (let loop ((index (string-length string)))
            (if (char=? (string-ref string (- index 1)) #\/)
                (substring string index (string-length string))
                (loop (- index 1))))))

      (define directory
        (lambda (string)
          (let loop ((index (string-length string)))
            (if (char=? (string-ref string (- index 1)) #\/)
                (substring string 0 (- index 1))
                (loop (- index 1))))))

      (define (make-filepath filepath)
        (cond
         ((string=? filepath ".") (current-directory))
         ((char=? (string-ref filepath 0) #\/) filepath)
         (else (string-append (current-directory) "/" filepath))))

      (define filepath* (make-filepath filepath))

      (if (string=? "README.md" (pk 'basename (basename filepath*)))
          (call-with-output-file (string-append (pk 'directory (directory (directory filepath*)))
                                                "/"
                                                (basename (directory filepath*))
                                                ".scm")
            (lambda (port)
              (literally-format port (call-with-input-file filepath* literally))))
          (call-with-output-file (string-append filepath* ".scm")
            (lambda (port)
              (literally-format port (call-with-input-file filepath* literally))))))))
