
(suppress-greeting #t)

(scheme-start 
 (lambda args
   (define LETLOOP_DEBUG (getenv "LETLOOP_DEBUG"))

   (define letloop-main
     (lambda (args)
       (when (null? args)
         (letloop-usage)
         (exit 0))

       (case (string->symbol (car args))
         ((benchmark) (letloop-benchmark (cdr args)))
         ((check) (letloop-check (cdr args)))
         ((compile) (letloop-compile (cdr args)))
         ((exec) (letloop-exec (cdr args)))
         ((repl) (letloop-repl (cdr args)))
         (else (letloop-usage) (exit 1)))))

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
     (display "Usage:\n")
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
         (library-directories directories)
         (source-directories directories))

       (optimize-level optimize-level*)

       (unless (null? extensions)
         (library-extensions extensions))

       (dev! dev?)

       (generate-wpo-files #t)

       (for-each maybe-compile-file* (map car (letloop-discover-libraries)))
       (when program.scm
         (pk (maybe-compile-file (pk 'program.scm program.scm))))
       (apply make-boot-file "letloop.boot"
              (list "scheme")
              (append (if program.scm (list (.so program.scm)) (list))
                      (pk 'only-existing-library
                          (filter file-exists?
                            (map .so (map car (letloop-discover-libraries)))))))))



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

     (dev! dev?)

     (command-line (cons program.scm extra))

     (dynamic-wind
         (lambda () (void))
         (lambda () (load-program program.scm))
         (lambda ()
           (guard (ex (else #f))
             (define dir ((foreign-procedure "letloop-directory" () string)))
             (for-each delete-file (ftw dir))
             (for-each delete-directory (ftw* dir))
             (delete-directory dir))
           (when dev?
             (profile-dump-html)))))

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
       (library-directories (append directories (library-directories)))
       (source-directories (append directories (source-directories))))

     (when optimize-level*
       (optimize-level optimize-level*))

     (unless (null? extensions)
       (library-extensions extensions))

     (dev! dev?)

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

   (define letloop-benchmark
     (lambda (arguments)

       (define errors (make-accumulator))

       (define extensions '())
       (define directories '())
       (define files '())
       (define unknown '())
       (define N 10)

       (define massage-standalone!
         (lambda (standalone)
           (unless (null? standalone)
             (call-with-values (lambda () (guess (car standalone)))
               (lambda (type string*)
                 (case type
                   (directory (set! directories (cons string* directories)))
                   (extension (set! extensions (cons string* extensions)))
                   (file (set! files (cons string* files)))
                   (unknown
                    (if (string->number string*)
                        (set! N (string->number string*))
                        (set! unknown (cons string* unknown)))))))
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

       (define build-benchmark-program
         (lambda (files unknown)
           (let* ((file (car files))
                  (unknown (string->symbol (car unknown)))
                  (name+exports (file->library file))
                  (name (car name+exports))
                  (exports (cdr name+exports)))
             `((import (chezscheme) (only ,name ,unknown)
                       (only (scheme time) current-jiffy))

               (define start (current-jiffy))

               (let loop ((n ,N))
                 (unless (fxzero? n)
                   (,unknown)
                   (loop (fx- n 1))))

               (format #t "Average nanoseconds spent per thunk ~a:\n~a\n"
                       ,unknown
                       (truncate (/ (- (current-jiffy) start) ,N)))))))

       (call-with-values (lambda () (command-line-parse arguments))
         (lambda (keywords standalone extra)
           (massage-standalone! standalone)))

       (maybe-display-errors-then-exit errors)
       (library-directories directories)
       (source-directories directories)

       (unless (null? extensions)
         (library-extensions extensions))

       (let* ((temporary-directory
               (make-temporary-directory
                (string-append "/tmp/letloop/benchmark-"
                               (timestamp))))
              (benchmark (string-append temporary-directory "/benchmark.scm"))
              (program (build-benchmark-program files unknown)))

         (call-with-output-file benchmark
           (lambda (port)
             (let loop ((program program))
               (unless (null? program)
                 (pretty-print (car program) port)
                 (loop (cdr program))))))

         (letloop-exec (cons benchmark (reverse directories))))))

   (define timestamp
     (lambda ()
       (number->string (time-second (current-time)))))

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
                 (if (ref out (car objects) #f)
                     (loop (cdr objects) out)
                     (loop (cdr objects)
                           (cons (cons (car objects) #t) out)))))))

       (define build-check-program
         (lambda (spec fail-fast?)
           ;; TODO: add prefix to import, and rename procedures
           (define libraries (pk 'libraries (reverse (uniquify (map car spec)))))
           (define procedures (map cdr spec))

           (pk 'program
               `((import (chezscheme) ,@libraries)

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

       (let* ((temporary-directory
               (make-temporary-directory
                (string-append "/tmp/letloop/check-"
                               (timestamp))))
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
               (call-with-output-file check
                 (lambda (port)
                   (let loop ((program program))
                     (unless (null? program)
                       (pretty-print (car program) port)
                       (loop (cdr program))))))

               ;; Change directory to TEMPORARY-DIRECTORY to produce
               ;; the profile dump along the CHECK file.
               (current-directory temporary-directory)
               (letloop-exec (list "--dev" "." check))
               (format (current-output-port) "* Coverage profile can be found at: ~a/profile.html\n" temporary-directory))))))

   (letloop-main args)))
