(import (chezscheme))


  (define LETLOOP_DEBUG (getenv "LETLOOP_DEBUG"))

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

  (define ftw*
    (lambda (directory)
      (let loop ((paths (map (lambda (x) (string-append directory "/" x)) (directory-list directory)))
                 (out '()))
        (if (null? paths)
            out
            (if (file-directory? (car paths))
                (loop (append (ftw* (car paths)) (cdr paths))
                      (cons (car paths) out))
                (loop (cdr paths) out))))))

  (meta define (pk* . args)
        (display ";;; " (current-error-port))
        (write args (current-error-port))
        (newline (current-error-port))
        (flush-output-port (current-error-port))
        (car (reverse args)))

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

(define basename-without-extension
  (lambda (filename)
    (let loop ((index (string-length filename)))
      (if (char=? (string-ref filename (- index 1)) #\.)
          (substring filename 0 (- index 1))
          (loop (- index 1))))))

  (meta define basename
        (lambda (string)
          (let loop ((index (string-length string)))
            (if (char=? (string-ref string (- index 1)) #\/)
                (substring string index (string-length string))
                (loop (- index 1))))))

  (meta define scheme-binarypath
        (lambda ()
          (let* ((out
                  (run/output
                   (format #f "readlink -n /proc/~a/exe"
                           (get-process-id)))))
            (substring out 0 (- (string-length out)
                                (string-length (basename out)))))))

  (meta define binarypath->scheme-home
        (lambda (scheme what)
          (call/cc
           (lambda (k)
             (for-each
              (lambda (path)
                (call-with-values (lambda () (scheme-version-number))
                  (lambda args
                    (let* ((version (let loop ((args args)
                                               (out '()))
                                      (if (null? args)
                                          (apply string-append (reverse (cdr out)))
                                          (loop (cdr args)
                                                (cons* "."
                                                       (number->string (car args))
                                                       out)))))
                           (prefix (run/output (format #f "realpath $(ls -d ~a/../lib/csv~a*) | tr -d '\n'" path version))))
                      (let ((out (format #f "~a/~a/~a" prefix (machine-type) what)))
                        (when (file-exists? out)
                          (k out)))))))
              (list scheme "/usr/local/bin/" "/usr/bin/"))))))

  (define-syntax include-chez-file
    (lambda (x)
      (syntax-case x ()
        [(k filename)
         (let* ([fn (datum filename)]
                [fn (binarypath->scheme-home (scheme-binarypath) fn)])
           (with-syntax ([exp (get-bytevector-all (open-file-input-port fn))])
             #'exp))])))

  (define filepath->bytevector
    (lambda (filepath)
      (define port (open-file-input-port filepath))
      (define out (get-bytevector-all port))
      (close-port port)
      out))

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
           (with-syntax ([exp (run/output "git describe --always --tags --dirty")])
             #'exp))])))

  (define-syntax include-scheme-version
    (lambda (x)
      (syntax-case x ()
        ((k)
         (with-syntax ((exp (scheme-version)))
                      #'exp)))))

  (define letloop-scheme-version (include-scheme-version))

  (define-syntax include-git-branch
    (lambda (x)
      (syntax-case x ()
        [(k)
         (let ([fn (datum filename)])
           (with-syntax ([exp (run/output "git branch --show-current")])
             #'exp))])))

  (define-syntax include-git-head
    (lambda (x)
      (syntax-case x ()
        [(k)
         (let ([fn (datum filename)])
           (with-syntax ([exp (run/output "git rev-parse --short HEAD")])
             #'exp))])))

  ;; Include some files

  (define main.c (include-filename-as-string "main.c"))

  (define letloop-tag (let ((describe (include-git-describe))
                            (branch (include-git-branch)))
                        (if (and (fxzero? (string-length describe))
                                 (fxzero? (string-length branch)))
                            (include-git-head)
                            (string-append (if (string=? branch "")
                                               ;; when the action checkout a tag,
                                               ;; according to git there is no branch
                                               "main"
                                               (substring branch 0 (fx- (string-length branch) 1)))
                                           "-"
                                           (substring describe 0 (fx- (string-length describe) 1))))))

  (define-syntax include-date
    (lambda (x)
      (syntax-case x ()
        [(k)
         (let ([fn (datum filename)])
           (with-syntax ([exp (run/output "date +\"%Y-%m-%dT%H:%M:%S%z\"")])
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
    (write `(scheme ,letloop-scheme-version))
    (newline)
    (write `(tag ,letloop-tag))
    (newline)
    (write `(build-date ,build-date))
    (newline)
    (write `(homepage "https://github.com/letloop/"))
    (newline))

  (define letloop-usage
    (lambda ()
      (display-usage letloop.md)))

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

  (define dev!
    (lambda (active?)
      (when active?
        (compile-profile 'source))
      (import-notify active?)
      (generate-allocation-counts active?)
      (generate-covin-files active?)
      (generate-inspector-information active?)
      (generate-instruction-counts active?)
      (generate-interrupt-trap active?)
      (generate-procedure-source-information active?)
      (generate-profile-forms active?)
      (debug-on-exception active?)))

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

      (define (string-suffix? s1 s2)

        (define (%string-suffix-length s1 start1 end1 s2 start2 end2)
          (let* ((delta (min (- end1 start1) (- end2 start2)))
                 (start1 (- end1 delta)))

            (if (and (eq? s1 s2) (= end1 end2))		; EQ fast path
                delta
                (let lp ((i (- end1 1)) (j (- end2 1)))	; Regular path
                  (if (or (< i start1)
                          (not (char=? (string-ref s1 i)
                                       (string-ref s2 j))))
                      (- (- end1 i) 1)
                      (lp (- i 1) (- j 1)))))))

        (define (%string-suffix? s1 start1 end1 s2 start2 end2)
          (let ((len1 (- end1 start1)))
            (and (<= len1 (- end2 start2))	; Quick check
                 (= len1 (%string-suffix-length s1 start1 end1
                                                s2 start2 end2)))))

        (let ((start1 0)
              (end1 (string-length s1))
              (start2 0)
              (end2 (string-length s2)))

          (%string-suffix? s1 start1 end1 s2 start2 end2)))

    (define letloop-discover-libraries
        (lambda ()

          (define library-prepare
            (lambda (root+filepath)

              (define any
                (lambda (p? os)
                  (let loop ((os os))
                    (if (null? os)
                        #f
                        (if (p? (car os))
                            (car os)
                            (loop (cdr os)))))))

              (define extension
                (lambda (filepath)
                  (any (lambda (x) (string-suffix? x filepath))
                       (map car (library-extensions)))))

              (define do
                (lambda (root filepath sexp)
                  (let ((name (and (pair? sexp)
                                   ;; TODO: use match to do this.
                                   (eq? (car sexp) 'library)
                                   (pair? (cdr sexp))
                                   (pair? (cadr sexp))
                                   (cadr sexp))))
                    (if (not name)
                        #f
                        (let* ((filepath* (substring filepath
                                                     (+ (string-length root) 1)
                                                     (string-length filepath)))
                               (filepath** (substring filepath*
                                                      0
                                                      (- (string-length filepath*)
                                                         (string-length (extension filepath*)))))
                               (other (string-split (make-char-predicate #\/) filepath**))
                               (other* (map string->symbol other)))
                          (and (equal? name other*)
                               (list filepath root filepath** name)))))))

              (define root (car root+filepath))
              (define filepath (cdr root+filepath))

              (if (not (extension filepath))
                  #f
                  (guard (ex (else (pk 'library? "File is unreadable as Scheme file" filepath) #f))
                    (do root filepath (call-with-input-file filepath read))))))

          (define ftw
            (lambda (root directory)
                  (let loop ((paths (map (lambda (x) (string-append directory "/" x))
                                         (guard (ex (else (list)))
                                           (directory-list directory))))
                             (out '()))
                    (if (null? paths)
                        out
                        (if (file-directory? (car paths))
                            (loop (append (map cdr (ftw root (car paths))) (cdr paths))
                                  out)
                            (loop (cdr paths) (cons (cons root (car paths)) out)))))))

          (define ref
            (lambda (alist key default)
              (if (null? alist)
                  default
                  (if (equal? (caar alist) key)
                      (cdar alist)
                      (ref (cdr alist) key default)))))

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

          (define directories
            (uniquify
             (apply append (map (lambda (x)
                                  (if (not (pair? x))
                                      (list x)
                                      (list (car x) (cdr x))))
                                (library-directories)))))
          (apply append (map (lambda (x) (filter values (map library-prepare (ftw x x)))) directories))))

      (define (string-split char-delimiter? string)
        (define (maybe-add a b parts)
          (if (= a b) parts (cons (substring string a b) parts)))
        (let ((n (string-length string)))
          (let loop ((a 0) (b 0) (parts '()))
            (if (< b n)
                (if (not (char-delimiter? (string-ref string b)))
                    (loop a (+ b 1) parts)
                    (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
                (reverse (maybe-add a b parts))))))

      (define make-char-predicate
        (lambda (char)
          (lambda (object)
            (char=? char object))))

      ;; parse ARGUMENTS, and set the following variables:

      (define extensions '())
      (define directories '())
      (define program.scm #f)
      (define files '())
      (define dev? #f)
      (define optimize-level* 0)
      (define extra '())

      (define errors (make-accumulator))

      (define basename
        (lambda (string)
          (let loop ((index (string-length string)))
            (if (char=? (string-ref string (- index 1)) #\/)
                (substring string index (string-length string))
                (loop (- index 1))))))

      (define massage-standalone!
        (lambda (standalone)
          (unless (null? standalone)
            (call-with-values (lambda () (guess (car standalone)))
              (lambda (type string*)
                (case type
                  (directory (set! directories (cons string* directories)))
                  (extension (set! extensions (cons string* extensions)))
                  (file (set! program.scm string*))
                  (unknown (errors (format #f "Dubious argument: ~a" string*))))))
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

      (define string-join
        (lambda (strings delimiter)
          (let loop ((out (list delimiter))
                     (strings strings))
            (if (null? strings)
                (apply string-append (reverse (cdr out)))
                (loop (cons* "/" (car strings) out)
                      (cdr strings))))))

      (define mkdir*
        (lambda (path)
          (let loop ((directories (reverse
                                   (cdr (reverse (string-split (make-char-predicate #\/) path)))))
                     (out '()))
            (unless (null? directories)
              (let ((target (string-join (append '() (reverse out)
                                                 (list (car directories))) "/")))
                (unless (file-directory? target)
                  (mkdir target))
                (loop (cdr directories) (cons (car directories) out)))))
          path))

      (define .so
        (lambda (x)
          (string-append (basename-without-extension x) ".so")))

      (define maybe-compile-file*
        (lambda (f)
          (guard (ex (else (void)))
            (maybe-compile-file f))))

      (call-with-values (lambda () (command-line-parse arguments))
        (lambda (keywords standalone extra*)
          (massage-standalone! standalone)
          (massage-keywords! keywords)
          (set! extra extra*)))

      (maybe-display-errors-then-exit errors)

      (unless (null? directories)
        (library-directories (append directories (library-directories)))
        (source-directories (append directories (source-directories))))

      (optimize-level optimize-level*)

      (unless (null? extensions)
        (library-extensions extensions))

      (dev! dev?)

      ;;(compile-imported-libraries #t)
      (generate-wpo-files #t)

      (for-each maybe-compile-file* (map car (letloop-discover-libraries)))
      (pk (maybe-compile-file program.scm))
      (for-each pk (map car (letloop-discover-libraries)))
      (apply make-boot-file "letloop.boot"
                            (list "scheme")
                            (.so program.scm)
                            (pk (filter file-exists?
                                    (map .so (map car (letloop-discover-libraries))))))))



(letloop-compile (list "library/" "letloop.scm"))
