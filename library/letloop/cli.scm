(library (letloop cli)
  (export cli-read cli-write maybe-library-name pk maybe-display-errors-then-exit dev!)
  (import (chezscheme)
          (letloop match)
          (only (scheme list) list-index))

  (define pk
    (lambda args
      (when (getenv "DEBUG_CLI")
        (display ";; " (current-error-port))
        (write args (current-error-port))
        (newline (current-error-port))
        (flush-output-port (current-error-port)))
      (car (reverse args))))

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


(define (maybe-display-errors-then-exit errors)
  (let ((errors (errors (eof-object))))
    (unless (null? errors)
      (display "* Ooops :|")
      (newline)
      (for-each (lambda (x) (display "** ") (display x) (newline)) (reverse errors))
      (exit 1))))

(define and=> (lambda (v proc) (and v (proc v))))

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
