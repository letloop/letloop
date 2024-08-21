#!chezscheme
(library (letloop cli exec)
  (export letloop-exec)
  (import (chezscheme) (letloop cli))

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
    (define (letloop-exec arguments)

      ;; parse ARGUMENTS, and set the following variables:

      (define extensions '())
      (define directories '())
      (define dev? #f)
      (define optimize-level* 0)
      (define extra '())
      (define program.scm #f)
      (define library.scm #f)
      (define main #f)

      (define errors (make-accumulator))

      (define massage-standalone!
        (lambda (standalone)
          (unless (null? standalone)
            (call-with-values (lambda () (guess (car standalone)))
              (lambda (type string*)
                (case type
                  (directory (set! directories (cons string* directories)))
                  (extension (set! extensions (cons string* extensions)))
                  (file (if library.scm
                            (errors (format #f "Already registred a library to execute, maybe remove: ~a" (car standalone)))
                            (set! library.scm string*)))
                  (unknown (if main
                            (errors (format #f "Already registred a main procedure, maybe remove: ~a" (car standalone)))
                            (set! main string*))))))
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

      (call-with-values (lambda () (cli-read arguments))
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
        (library-extensions (append extensions (library-extensions))))

      (dev! dev?)

      (dynamic-wind
          (lambda () (void))
          (lambda () (eval (cons (string->symbol main) extra) (environment '(chezscheme) (maybe-library-name library.scm))))
          (lambda ()
            (when dev?
              (profile-dump-html))))))


