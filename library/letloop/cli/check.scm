#!chezscheme
(library (letloop cli check)
  (export letloop-check)
  (import (chezscheme) (letloop match) (letloop cli))
     (define ftw
     (lambda (directory)
       (let loop ((paths (map (lambda (x) (string-append directory "/" x)) (directory-list directory)))
                  (out '()))
         (if (null? paths)
             out
             (if (file-directory? (car paths))
                 (loop (append (ftw (car paths)) (cdr paths))
                       out)
                 (loop (cdr paths) (cons (car paths) out)))))))

   (define (guess string)
     (cond
      ((file-directory? string) (values 'directory (make-filepath string)))
      ((file-exists? string)
       (values 'file (make-filepath string)))
      ;; the first char is a dot, the associated path is neither a file
      ;; or directory, hence it is prolly an extension... breaks when the
      ;; user made a typo in a file or directory name.
      ((char=? (string-ref string 0) #\.)
       (values 'extension string))
      (else (values 'unknown string))))

   (define (make-temporary-directory prefix)

     (define stdlib (load-shared-object #f))

     (define mkdtemp
       (foreign-procedure "mkdtemp" (string) string))

     (let ((input (string-append prefix "-XXXXXX")))
       (mkdtemp input)))

  (define timestamp
    (lambda ()
      (number->string (time-second (current-time)))))
  
  (define make-accumulator
    (lambda ()
      (let ((out '()))
        (lambda (object)
          (if (eof-object? object)
              out
              (set! out (cons object out)))))))
  
  (define (make-filepath filepath)
    (cond
     ((string=? filepath ".") (current-directory))
     ((char=? (string-ref filepath 0) #\/) filepath)
     (else (string-append (current-directory) "/" filepath))))
  
  (define letloop-check
    (lambda (arguments)

      (define errors (make-accumulator))

      (define fail-fast? #f)
      (define dry-run? #f)
      (define extensions '())
      (define directories '())
      (define files '())
      (define alloweds '())

      (define massage-keywords!
        (lambda (keywords)
          (unless (null? keywords)
            (case (caar keywords)
              (--fail-fast (set! fail-fast? #t))
              (--dry-run (set! dry-run? #t))
              (else (errors (format #f "Unknown keywords: ~a" (caar keywords))))))))

      (define massage-standalone!
        (lambda (standalone)
          (unless (null? standalone)
            (call-with-values (lambda () (guess (car standalone)))
              (lambda (type string*)
                (case type
                  (directory (set! directories (cons string* directories)))
                  (extension (set! extensions (cons string* extensions)))
                  (file (set! files (cons string* files)))
                  (unknown (errors (format #f "Unknown flying object: ~a" (car standalone)))))))
            (massage-standalone! (cdr standalone)))))

      (define (maybe-library-exports library-name)
        (guard (ex (else #f))
          (eval `(library-exports ',library-name) (environment '(chezscheme) library-name))))

      (define maybe-read-library
        (lambda (file)
          (pk 'maybe-read-library file)
          (let ((sexp (guard (ex (else #f))
                        (call-with-input-file file read))))
            (if (not sexp)
                (begin
                  (pk 'maybe-read-library "File is unreadable as Scheme file" file)
                  '())
                (if (and (pair? sexp)
                         (eq? (car sexp) 'library)
                         (pair? (cdr sexp))
                         (pair? (cadr sexp)))
                    (let ((exports (maybe-library-exports (cadr sexp))))
                      (if exports
                          (begin
                            (pk 'maybe-read-library "valid" file
                                (reverse (map (lambda (x) (cons (cadr sexp) x)) exports))))
                          (begin
                            (pk 'maybe-read-library "no interesting exports")
                            '())))
                    ;; Oops!
                    (begin
                      (pk 'maybe-read-library "not a valid scheme library file" file)
                      '()))))))

      (define string-prefix?
        (lambda (x y)
          (let ([n (string-length x)])
            (and (fx<= n (string-length y))
                 (let prefix? ([i 0])
                   (or (fx= i n)
                       (and (char=? (string-ref x i) (string-ref y i))
                            (prefix? (fx+ i 1)))))))))
      (define allow?
        (lambda (x)
          ;; Does it look like a check procedure
          (and (string-prefix? "~check-" (symbol->string (cdr x)))
               (or (null? alloweds)
                   (member (cdr x) alloweds)
                   (member (car x) alloweds)))))

      (define discover
        (lambda (directories)
          (define files (apply append (map ftw directories)))
          (filter allow? (apply append (map maybe-read-library files)))))

      (define uniquify
        (lambda (objects)
          (let loop ((objects objects)
                     (out '()))
            (if (null? objects)
                (map car out)
                (if (assoc (car objects) out)
                    (loop (cdr objects) out)
                    (loop (cdr objects)
                          (cons (cons (car objects) #t) out)))))))

      (define build-check-program
        (lambda (spec fail-fast?)
          ;; TODO: add prefix to import, and rename procedures
          (define libraries (pk 'libraries (reverse (uniquify (map car spec)))))
          (define procedures (map cdr spec))

          (pk 'program
              `(begin
                 (define errored? #f)

                 (display "* Will run tests from the following libraries:\n")
                 (for-each
                  (lambda (x)
                    (format #t "** ~a\n" x)) ',libraries)

                 (newline)
                 (let loop ((thunks (list ,@procedures)))
                   (unless (null? thunks)
                     (format #t "* Checking `~a`:\n" (car thunks))
                     (guard (ex (else
                                 (if (condition? ex)
                                     (display-condition ex)
                                     (write ex))
                                 (newline)
                                 (display "** ERROR!\n")
                                 (if ,fail-fast?
                                     (begin (newline)
                                            (exit 1))
                                     (begin (newline)
                                            (set! errored? #t)))))
                       (let ((out ((car thunks))))
                         (if (and (not (eq? out (void)))
                                  out)
                             (begin
                               (newline)
                               (display "** SUCCESS\n"))
                             (begin
                               (newline)
                               (display "** FAILED\n")
                               (if ,fail-fast?
                                   (begin (newline)
                                          (exit 1))
                                   (set! errored? #t))))))
                     (loop (cdr thunks))))
                 (newline)
                 (when errored? (exit 1))))))

      (call-with-values (lambda () (cli-read arguments))
        (lambda (keywords standalone extra)
          (massage-keywords! keywords)
          (massage-standalone! standalone)
          ;; TODO: Moar error handling
          (set! alloweds (map (lambda (x) (read (open-input-string x))) extra))))

      (compile-profile 'source)

      (maybe-display-errors-then-exit errors)

      (library-directories directories)
      (source-directories directories)

      (unless (null? extensions)
        (library-extensions extensions))

      (let* ((temporary-directory (pk 'tmp
              (make-temporary-directory
               (string-append "/tmp/letloop-check-"
                              (timestamp)))))
             (check (string-append temporary-directory "/check.scm"))
             (checks (or (and (not (null? files))
                              (filter allow?
                                      (apply append
                                             (map maybe-read-library
                                                  files))))
                         (discover directories)))
             (program (build-check-program checks fail-fast?)))

        (when (null? checks)
          (format #t "* Error, no checks found!\n")
          (exit 2))

        (if dry-run?
            (let ((libraries (pk 'libraries (reverse (uniquify (map car checks)))))
                  (thunks (map cdr checks)))

              (format #t "* Dry run from the following libraries:\n\n")
              (for-each
               (lambda (x)
                 (format #t "** ~a\n" x)) libraries)
              (format #t "* Dry checks:\n\n")
              (for-each
               (lambda (thunk)
                 (format #t "** Dry checking `~a`:\n" (car thunks)))
               thunks))

            (begin
              ;; Change directory to TEMPORARY-DIRECTORY to produce
              ;; the profile dump along the CHECK file.
              (current-directory temporary-directory)

              (dynamic-wind
                  (lambda () (void))
                  (lambda () (eval program (copy-environment (apply environment '(chezscheme)
                                                                    (reverse (uniquify (map car checks))))
                                                             #t)))
                  (lambda ()
                    (guard (ex (else (void)))
                      (profile-dump-html)
                      (format (current-output-port) "* Coverage profile can be found at: ~a/profile.html\n" temporary-directory))))))))))
