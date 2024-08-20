#!chezscheme
(library (letloop cli compile)
  (export letloop-compile letloop-compile*)
  (import (chezscheme) (letloop match) (letloop cli))

(define letloop-compile* (lambda () (letloop-compile (command-line-arguments))))

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

(define (guess string)
  (pk 'guess string)
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

(define any
  (lambda (p? os)
    (memq #t (map p? os))))

(define maybe-library?
  (lambda (filepath)
    (any (lambda (x) (string-suffix? x filepath))
        (map car (library-extensions)))))

(define and=> (lambda (x proc) (and x (proc x))))

(define basename-without-extension
  (lambda (filename)
    (let loop ((index (string-length filename)))
      (if (char=? (string-ref filename (- index 1)) #\.)
          (substring filename 0 (- index 1))
          (loop (- index 1))))))

(define maybe-library-name
  (lambda (filename)  

    (if (not (maybe-library? filename))
      #f
      (and=> (guard (ex (else #f))
               (call-with-input-file filename read))
             (lambda (sexp) (and=> (match sexp 
                                     ((library (,name ...) ,body ...) name)
                                     (,_ #f))
                                   (lambda (name)
                                     (guard (ex (else #f))
                                       (and (eval #t (environment name)) name)))))))))
   (define (pk . args)
     (display ";;; " (current-error-port))
     (write args (current-error-port))
     (newline (current-error-port))
     (flush-output-port (current-error-port))
     (car (reverse args)))


(define ftw
  (lambda (root)
    (let loop ((paths (list root))
               (out '()))
      (if (null? paths)
          out
          (if (file-directory? (car paths))
              (loop (append (map (lambda (x) (string-append (car paths) "/" x))
                                 (directory-list (car paths)))
                            (cdr paths))
                    out)
              (loop (cdr paths) (cons (car paths) out)))))))

(define letloop-discover-libraries
  (lambda ()
    (define root+filepaths (apply append (map (lambda (root) (map (lambda (f) (cons (car root) f)) (ftw (car root))))
                                              (library-directories))))
    (filter (lambda (root+filepath) (maybe-library-name (cdr root+filepath))) root+filepaths)))

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

(define basename
  (lambda (string)
    (let loop ((index (string-length string)))
      (if (char=? (string-ref string (- index 1)) #\/)
          (substring string index (string-length string))
          (loop (- index 1))))))

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

(define (maybe-display-errors-then-exit errors)
  (let ((errors (errors (eof-object))))
    (unless (null? errors)
      (display "* Ooops :|")
      (newline)
      (for-each (lambda (x) (display "** ") (display x) (newline)) (reverse errors))
      (exit 1))))

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


(define letloop-compile
  (lambda (arguments)

    ;; parse ARGUMENTS, and set the following variables:

    (define extensions '())
    (define directories '())
    (define main #f)
    (define library.scm #f)
    (define dev? #f)
    (define optimize-level* 0)
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
                (file (set! library.scm string*))
                (unknown (set! main string*)))))
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

    (call-with-values (lambda () (cli-read arguments))
      (lambda (keywords standalone extra*)
        (massage-standalone! standalone)
        (massage-keywords! keywords)
        (unless (null? extra*)
          (errors (format #f "Unexpected extra: ~a" extra*)))))

    (maybe-display-errors-then-exit errors)

    (unless (null? directories)
      ;; XXX: override existing library directories, in particular the current
      ;; directory.
      (library-directories directories)
      (source-directories directories))

    (optimize-level optimize-level*)

    (unless (null? extensions)
      (library-extensions (append extensions (library-extensions))))

    (dev! dev?)

    (generate-wpo-files #t)

    (for-each maybe-compile-file* (map cdr (letloop-discover-libraries)))

    (when main
      (call-with-output-file "/tmp/letloop.scm"
        (lambda (port)
          (write '(suppress-greeting #t) port)
          (write `(import ,(pk library.scm (maybe-library-name library.scm))) port)
          (write `(scheme-start ,(string->symbol main)) port)) 'truncate)
      (set! program.scm "/tmp/letloop.scm")
      (maybe-compile-file (pk program.scm)))

    (apply make-boot-file
           (pk 'out (string-append (if program.scm
                              (basename-without-extension (pk 'program.scm program.scm))
                              "./library")
                          ".boot"))
          (list "scheme")
          (append (filter file-exists? (map .so (map cdr (letloop-discover-libraries))))
                  (if program.scm (list (.so program.scm)) (list)))))))
