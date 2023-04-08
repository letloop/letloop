#!chezscheme
(import (chezscheme))


(define LETLOOP_DEBUG (getenv "LETLOOP_DEBUG"))

(meta define read-string
      (lambda (p)
        (let loop ([x (read-char p)]
                   [out '()])
          (if (eof-object? x)
              (begin (close-input-port p)
                     (list->string (reverse out)))
              (loop (read-char p)
                    (cons x out))))))

(meta define (run/output command)
      (call-with-values (lambda ()
                          (open-process-ports command 'line (current-transcoder)))
        (lambda (stdin stdout stderr pid)
          (read-string stdout))))

(define-syntax include-filename-as-string
  (lambda (x)
    (syntax-case x ()
      [(k filename)
       (let ([fn (datum filename)])
         (with-syntax ([exp (read-string (open-input-file fn))])
                      #'exp))])))

(define (pk . args)
  (when LETLOOP_DEBUG
    (display ";;; " (current-error-port))
    (write args (current-error-port))
    (newline (current-error-port))
    (flush-output-port (current-error-port)))
  (car (reverse args)))

(define (string-join strings)
  (let loop ((strings strings)
             (out '()))
    (if (null? strings)
        (apply string-append strings)
        (loop (cdr strings) (cons* " " string out)))))

(define letloop.md (include-filename-as-string "letloop.md"))
(define letloop.nfo (include-filename-as-string "letloop.nfo"))

;; Include git commit

(define-syntax include-git-describe
  (lambda (x)
    (syntax-case x ()
      [(k)
       (let ([fn (datum filename)])
         (with-syntax ([exp (run/output "git describe --tags --dirty")])
                      #'exp))])))

;; Include some files

(define main.c (include-filename-as-string "main.c"))

(define git-describe (let ((out (include-git-describe)))
                       (if (= (string-length out) 0)
                           "dev"
                           (substring out 0 (fx- (string-length out) 1)))))

(define-syntax include-date
  (lambda (x)
    (syntax-case x ()
      [(k)
       (let ([fn (datum filename)])
         (with-syntax ([exp (run/output "date")])
                      #'exp))])))

(define build-date (let ((date (include-date)))
                     (substring date 0 (fx- (string-length date) 1))))

(define make-accumulator
  (lambda ()
    (let ((out '()))
      (lambda (object)
        (if (eof-object? object)
            out
            (set! out (cons object out)))))))

(define (display-usage usage)
  (newline)
  (display letloop.nfo)
  (newline)
  (newline)
  (display usage)
  (newline)
  (write `(tag ,git-describe))
  (newline)
  (write `(build-date ,build-date))
  (newline)
  (write `(homepage "https://github.com/letloop/"))
  (newline))

(define letloop-usage
  (lambda ()
    (display-usage letloop.md)
    (exit 1)))

(define list-index
  (lambda (predicate? objects)
    (let loop ((index 0)
               (objects objects))
      (if (null? objects)
          #f
          (if (predicate? (car objects))
              index
              (loop (fx+ index 1) (cdr objects)))))))

(define (command-line-parse arguments)

  ;; Given the following ARGUMENTS:
  ;;
  ;;   '("--foo=bar" "--qux" "-vvv" "name" "another" "--" "olive" "extra")
  ;;
  ;; command-line-parse returns the following values:
  ;;
  ;;   (values '((--foo . "bar") (--qux . #t) (-vvv . #t)) '("name" "other") '("olive" "extra"))
  ;;
  ;; Standalone arguments e.g. "name" and "other" and extra arguments
  ;; e.g. "olive" and "extra" are returned in the same order as
  ;; found in ARGUMENTS.

  (define keyword/value
    (lambda (string)
      (define index (list-index (lambda (x) (char=? x #\=)) (string->list string)))

      (if (not index)
          (values (string->symbol string) #t)
          (values (string->symbol (substring string 0 index)) (substring string (fx+ index 1) (string-length string))))))

  (let loop ((arguments arguments)
             (keywords '())
             (standalone '()))
    (if (null? arguments)
        (begin
          (pk 'keywords keywords)
          (pk 'standalone (reverse standalone))
          (pk 'extra '())
          (values keywords (reverse standalone) '()))
        (let ((head (car arguments)))
          (cond
           ((string=? head "--")
            (pk 'keywords keywords)
            (pk 'standalone (reverse standalone))
            (pk 'extra (cdr arguments))
            (values keywords (reverse standalone) (cdr arguments)))
           ((char=? (string-ref head 0) #\-)
            (call-with-values (lambda () (keyword/value head))
              (lambda (key value)
                (loop (cdr arguments) (cons (cons key value) keywords) standalone))))
           (else (loop (cdr arguments) keywords (cons head standalone))))))))

(define (make-filepath filepath)
  (cond
   ((string=? filepath ".") (current-directory))
   ((char=? (string-ref filepath 0) #\/) filepath)
   (else (string-append (current-directory) "/" filepath))))

(define ref
  (lambda (alist key default)
    (if (null? alist)
        default
        (if (equal? (caar alist) key)
            (cdar alist)
            (ref (cdr alist) key default)))))

(define stdlib (load-shared-object #f))

(define mkdtemp
  (foreign-procedure "mkdtemp" (string) string))

(define (make-temporary-directory prefix)
  (let ((input (string-append prefix "-XXXXXX")))
    (mkdtemp input)))

(define (system* command)
  (unless (fxzero? (system command))
    (error 'letloop "System command failed" command)))

(define (maybe-display-errors-then-exit errors)
  (let ((errors (errors (eof-object))))
    (unless (null? errors)
      (display "* Ooops :|")
      (newline)
      (for-each (lambda (x) (display "** ") (display x) (newline)) (reverse errors))
      (exit 1))))

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

(define letloop-compile
  (lambda (arguments)

    (define program.boot.scm
      '(let ([program-name
              (foreign-procedure "program_name" () string)])
         (scheme-program
          (lambda (fn . fns)
            (command-line (cons (program-name) fns))
            (command-line-arguments fns)
            (load-program fn)))))

    ;; parse ARGUMENTS, and set the following variables:

    (define extensions '())
    (define directories '())
    (define program.scm #f)
    (define a.out #f)
    (define dev? #f)
    (define optimize-level* 0)
    (define extra '())
    (define chez-home #f)

    (define errors (make-accumulator))

    (define massage-standalone!
      (lambda (standalone)
        (unless (null? standalone)
          (call-with-values (lambda () (guess (car standalone)))
            (lambda (type string*)
              (case type
                (directory (set! directories (cons string* directories)))
                (extension (set! extensions (cons string* extensions)))
                (file (if program.scm
                          (errors (format #f "You can compile only one file at a time, maybe remove: ~a" (car standalone)))
                          (set! program.scm string*)))
                (unknown (if a.out
                             (errors (format #f "You provided more than one file that does not exists, maybe remove: ~a" (car standalone)))
                             (set! a.out string*))))))
          (massage-standalone! (cdr standalone)))))

    (define massage-keywords!
      (lambda (keywords)
        (unless (null? keywords)
          (let ((keyword (car keywords)))
            (cond
             ((and (eq? (car keyword) '--dev) (not (string? (cdr keyword))))
              (set! dev? #t))
             ((and (eq? (car keyword) '--optimize-level)
                   (string->number (cdr keyword))
                   (<= 0 (string->number (cdr keyword)) 3))
              (set! optimize-level* (string->number (cdr keyword))))
             (else (errors (format #f "Dubious keyword: ~a" (car keyword))))))
          (massage-keywords! (cdr keywords)))))

    (define massage-directories!
      (lambda ()
        (let loop ((directories* directories))
          (if (null? directories*)
              (errors "Please provide a directory containing both `scheme.h` and `kernel.o` from Chez")
              (if (and (file-exists? (string-append (car directories*) "/scheme.h"))
                       (file-exists? (string-append (car directories*) "/kernel.o")))
                  (set! chez-home (car directories*))
                  (loop (cdr directories*)))))))

    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords standalone extra*)
        (massage-standalone! standalone)
        (massage-keywords! keywords)
        (massage-directories!)
        (set! extra extra*)))

    (unless program.scm
      (errors "You need to provide one program file PROGRAM.SCM to compile, that will be read and transmogriffied."))

    (unless a.out
      (errors "You need to provide one target file A.OUT that will be created, and stuffed with bits."))

    (maybe-display-errors-then-exit errors)

    ;; All is well, proceed with compilation.
    (let ((temporary-directory (make-temporary-directory "/tmp/letloop-compile")))

      (unless (null? directories)
        (library-directories directories)
        (source-directories directories))

      (optimize-level optimize-level*)

      (unless (null? extensions)
        (library-extensions extensions))

      (when dev?
        ;; TODO: Link to documentation to help me remember what the
        ;; following does.
        (generate-allocation-counts #t)
        (generate-instruction-counts #t))

      (compile-imported-libraries #t)
      (generate-wpo-files #t)

      ;; create program.boot

      (call-with-output-file (string-append temporary-directory "/program.boot.scm")
        (lambda (port)
          (write program.boot.scm port)))

      (make-boot-file (string-append temporary-directory "/program.boot")
                      '()
                      (string-append chez-home "/petite.boot")
                      (string-append chez-home "/scheme.boot")
                      (string-append temporary-directory "/program.boot.scm"))

      ;; create main.c

      (system* (format #f "cp ~a ~a/program.scm" program.scm temporary-directory))

      (compile-program (string-append temporary-directory "/program.scm"))

      (pk 'compile-whole-program
          (compile-whole-program (string-append temporary-directory "/program.wpo")
                                 (string-append temporary-directory "/program.chez")
                                 #t))

      (call-with-output-file (string-append temporary-directory "/main.c")
        (lambda (port)
          (let ((boot (bytevector->u8-list
                       (get-bytevector-all
                        (open-file-input-port
                         (string-append temporary-directory "/program.boot")))))
                (program (bytevector->u8-list
                          (get-bytevector-all
                           (open-file-input-port
                            (string-append temporary-directory "/program.chez"))))))
            (display (format #f main.c boot program) port))))

      ;; create the output executable

      (system* (string-append "cp "
                              chez-home "/scheme.h"
                              " "
                              temporary-directory "/scheme.h"))

      (system* (string-append "cp "
                              chez-home "/kernel.o"
                              " "
                              temporary-directory "/kernel.o"))

      ;; XXX: pass -fno-lto to disable link-time optimization, because
      ;; kernel.o may have been compiled with the different version of
      ;; GCC or LLVM that is not the current cc. It only works when
      ;; kernel.o has been compiled to included both intermediate
      ;; code, and binary.
      ;;
      ;; ref: https://stackoverflow.com/a/72249840/140837
      (system* (format #f "cc -march=native ~a ~a/main.c ~a/kernel.o -o ~a -ldl -lz -llz4 -lm -luuid -lpthread -fno-lto"
                       (string-join extra)
                       temporary-directory
                       temporary-directory
                       a.out)))))

(define (letloop-exec arguments)

  ;; parse ARGUMENTS, and set the following variables:

  (define extensions '())
  (define directories '())
  (define dev? #f)
  (define optimize-level* 0)
  (define extra '())
  (define program.scm #f)

  (define errors (make-accumulator))

  (define massage-standalone!
    (lambda (standalone)
      (unless (null? standalone)
        (call-with-values (lambda () (guess (car standalone)))
          (lambda (type string*)
            (case type
              (directory (set! directories (cons string* directories)))
              (extension (set! extensions (cons string* extensions)))
              (file (if program.scm
                        (errors (format #f "Already registred a file to execute, maybe remove: ~a" (car standalone)))
                        (set! program.scm string*)))
              (unknown (errors (format #f "Directory does not exists: ~a" (car standalone)))))))
        (massage-standalone! (cdr standalone)))))

  (define massage-keywords!
    (lambda (keywords)
      (unless (null? keywords)
        (let ((keyword (car keywords)))
          (cond
           ((and (eq? (car keyword) '--dev) (not (string? (cdr keyword))))
            (set! dev? #t))
           ((and (eq? (car keyword) '--optimize-level)
                 (string->number (cdr keyword))
                 (<= 0 (string->number (cdr keyword) 3)))
            (set! optimize-level* (string->number (cdr keyword))))
           (else (errors (format #f "Dubious keyword: ~a" (car keyword))))))
        (massage-keywords! (cdr keywords)))))

  (call-with-values (lambda () (command-line-parse arguments))
    (lambda (keywords standalone extra*)
      (massage-standalone! standalone)
      (massage-keywords! keywords)
      (set! extra extra*)))

  (maybe-display-errors-then-exit errors)

  (unless (null? directories)
    (library-directories directories)
    (source-directories directories))

  (when optimize-level*
    (optimize-level optimize-level*))

  (unless (null? extensions)
    (library-extensions extensions))

  (when dev?
    (compile-profile 'source)
    (generate-allocation-counts #t)
    (generate-instruction-counts #t)
    (debug-on-exception #t))

  (command-line (cons program.scm extra))

  (load-program program.scm)

  (when dev?
    (profile-dump-html)))

(define (letloop-repl arguments)

  ;; parse ARGUMENTS, and set the following variables:

  (define extensions '())
  (define directories '())
  (define dev? #f)
  (define optimize-level* 0)
  (define extra '())

  (define errors (make-accumulator))

  (define massage-standalone!
    (lambda (standalone)
      (unless (null? standalone)
        (call-with-values (lambda () (guess (car standalone)))
          (lambda (type string*)
            (case type
              (directory (set! directories (cons string* directories)))
              (extension (set! extensions (cons string* extensions)))
              (file (errors (format #f "Does not support files: ~a" (car standalone))))
              (unknown (errors (format #f "Directory does not exists: ~a" (car standalone)))))))
        (massage-standalone! (cdr standalone)))))

  (define massage-keywords!
    (lambda (keywords)
      (unless (null? keywords)
        (let ((keyword (car keywords)))
          (cond
           ((and (eq? (car keyword) '--dev) (not (string? (cdr keyword))))
            (set! dev? #t))
           ((and (eq? (car keyword) '--optimize-level)
                 (string->number (cdr keyword))
                 (<= 0 (string->number (cdr keyword) 3)))
            (set! optimize-level* (string->number (cdr keyword))))
           (else (errors (format #f "Dubious keyword: ~a" (car keyword))))))
        (massage-keywords! (cdr keywords)))))

  (call-with-values (lambda () (command-line-parse arguments))
    (lambda (keywords standalone extra*)
      (massage-standalone! standalone)
      (massage-keywords! keywords)
      (set! extra extra*)))

  (unless (null? extra)
    (errors (format #f "No support for extra arguments: ~a" extra)))

  (maybe-display-errors-then-exit errors)

  (unless (null? directories)
    (library-directories directories)
    (source-directories directories))

  (when optimize-level*
    (optimize-level optimize-level*))

  (unless (null? extensions)
    (library-extensions extensions))

  (when dev?
    (generate-allocation-counts #t)
    (generate-instruction-counts #t)
    (debug-on-exception #t))

  (let loop ()
    (display "\033[32m#;letl∞p #;\033[m ")
    (let ((expr (read)))
      (unless (eof-object? expr)
        (call-with-values
            (lambda ()
              (guard (ex
                      ((condition? ex)
                       (display "\033[31m;; raised condition:\033[m ")
                       (display-condition ex)
                       (newline))
                      (else
                       (display "\033[31m;; raised:\033[m ")
                       (write ex)
                       (newline)))
                (eval expr)))
          (lambda args
            (unless (null? args)
              (for-each (lambda (x)
                          (unless (eq? x (void))
                            (display "\033[34m#;\033[m ")
                            (write x)
                            (newline)))
                        args))
            (loop)))))))

(define file->library
  (lambda (filename)
    (define sexp (call-with-input-file filename read))
    (define library (cadr sexp))

    (cons library (eval `(library-exports ',library) (environment '(chezscheme) library)))))

(define append-map
  (lambda (proc objs)
    (apply append (map proc objs))))

(define letloop-check
  (lambda (arguments)
    (define errors (make-accumulator))

    (define fail-fast? #f)
    (define extensions '())
    (define directories '())
    (define files '())
    (define alloweds '())


    (define timestamp
      (lambda ()
        (number->string (time-second (current-time)))))

    (define massage-keywords!
      (lambda (keywords)
        (unless (null? keywords)
          (case (caar keywords)
            (--fail-fast (set! fail-fast? #t))
            (else (errors (format #f "Unkown keywords: ~a" (caar keywords))))))))

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
                          (pk 'maybe-read-library "valid" file)
                          (reverse (map (lambda (x) (cons (cadr sexp) x)) exports)))
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

        (define files (apply append (map ftw directories)))

        (filter allow? (apply append (map maybe-read-library files)))))

    (define uniquify
      (lambda (objects)
        (let loop ((objects objects)
                   (out '()))
          (if (null? objects)
              (map car out)
              (if (ref out (car objects) #f)
                  (loop (cdr objects) out)
                  (loop (cdr objects)
                        (cons (cons (car objects) #t) out)))))))

    (define build-check-program
      (lambda (spec fail-fast?)
        ;; TODO: add prefix to import, and rename procedures
        (define libraries (pk 'libraries (reverse (uniquify (map car spec)))))
        (define procedures (map cdr spec))

        (pk 'program `((import (chezscheme) ,@libraries)

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
                          (display "** FAILED!\n")
                          (if (condition? ex)
                              (display-condition ex)
                              (write ex))
                          (if ,fail-fast?
                              (begin (newline)
                                     (exit 1))
                              (begin (newline)
                                     (set! errored? #t)))))
                ((car thunks))
                (display "* SUCCESS\n"))
              (loop (cdr thunks))))
          (newline)
          (when errored? (exit 1))))))

    (call-with-values (lambda () (command-line-parse arguments))
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

    ;; TODO: discover libraries based on the variable directories

    (let* ((temporary-directory
            (make-temporary-directory
             (string-append "/tmp/letloop-check-"
                            (timestamp))))
           (check (string-append temporary-directory "/check.scm"))
           (program (build-check-program
                     (or (and (not (null? files))
                              (filter allow?
                                      (apply append
                                             (map maybe-read-library
                                                  files))))
                         (discover directories))
                     fail-fast?)))
      (call-with-output-file check
        (lambda (port)
          (let loop ((program program))
            (unless (null? program)
              (pretty-print (car program) port)
              (loop (cdr program))))))

      ;; TODO: why change the current-directory?
      (current-directory temporary-directory)
      (letloop-exec (list "--dev" "." check))
      (format (current-output-port) "* Coverage profile can be found at: ~a/profile.html\n" temporary-directory))))


;; TODO: make it default to be more interoperable with R7RS code, put
;; it at startup time inside letloop-compile?
;;
;; (self-evaluating-vectors #t)

(when (null? (cdr (command-line)))
  (letloop-usage)
  (exit 1))

(case (cadr (pk (command-line)))
  (("check") (letloop-check (cddr (command-line))))
  (("compile") (letloop-compile (cddr (command-line))))
  (("exec") (letloop-exec (cddr (command-line))))
  (("repl") (letloop-repl (cddr (command-line))))
  ;; (("html" "write" ,file) (letloop-html-write file))
  ;; (("html" "read" ,file) (letloop-html-read file))
  ;; (("html" "bootstrap" ,file) (letloop-html-bootstrap file))
  (else (letloop-usage) (exit 1)))
