(library (letloop cli)
  (export cli-read cli-write)
  (import (chezscheme)
          (only (scheme list) list-index))

  (define pk
    (lambda args
      (when (getenv "DEBUG_CLI")
        (display ";; " (current-error-port))
        (write args (current-error-port))
        (newline (current-error-port))
        (flush-output-port (current-error-port)))
      (car (reverse args))))

  (define cli-write
    (lambda (flags standalone extra)
      (raise 'not-implemented)))
  
  (define (cli-read arguments) 
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
             (else (loop (cdr arguments) keywords (cons head standalone)))))))))
