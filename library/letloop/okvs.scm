(library (letloop okvs)
  (export okvs-empty?
          okvs?
          okvs-error?
          okvs-close
          okvs-cursor?
          okvs-transaction?
          okvs-transaction-read-only?
          okvs-root
          okvs-handle?
          okvs-key-max-length
          okvs-value-max-length
          okvs-transaction-timeout
          okvs-transaction-max-bytes
          okvs-transaction-context
          okvs-transaction-context!
          make-okvs-parameter
          okvs-begin-hook
          okvs-pre-commit-hook
          okvs-post-commit-hook
          okvs-rollback-hook
          call-with-okvs-transaction
          call-with-okvs-transaction-read-only
          okvs-approximate-key-count
          okvs-approximate-byte-count
          okvs-set!
          okvs-clear!
          call-with-okvs-cursor
          okvs-valid?
          okvs-next
          okvs-previous
          okvs-key
          okvs-value
          okvs-query
          okvs->alist
          okvs-read
          okvs-write
          okvs-zigzag

          make-okvs-checks)

  (import (chezscheme) (letloop gamma) (letloop hook) (letloop byter)
          (scheme generator)
          (only (srfi srfi-1) delete-duplicates))

  (define-syntax define-gamma
    (syntax-rules ()
      ((define-gamma name)
       (define name (make-gamma 'name)))
      ((define-gamma name fallback)
       (define name (make-gamma 'name fallback)))))

  (define-gamma okvs-empty?)
  (define-gamma okvs-valid?)
  (define-gamma okvs? (lambda args #f))
  (define-gamma okvs-error? (lambda args #f))
  (define-gamma okvs-close)
  (define-gamma okvs-cursor? (lambda args #f))
  (define-gamma okvs-transaction? (lambda args #f))
  (define-gamma okvs-transaction-read-only? (lambda args #f))
  (define-gamma okvs-root)

  (define okvs-handle?
    (case-lambda
      ;; OBJECT is an okvs object from the same library as OKVS
      ((okvs object) (and (okvs? okvs)
                          (okvs-handle? object)
                          (eq? (okvs-root object) okvs)))
      ;; OBJECT is an okvs of the registred okvs implementation.
      ((object) (or (okvs? object)
                    (okvs-transaction? object)
                    (okvs-transaction-read-only? object)
                    (okvs-cursor? object)))))

  (define-gamma okvs-key-max-length)
  (define-gamma okvs-value-max-length)
  (define-gamma okvs-transaction-timeout)
  (define-gamma okvs-transaction-max-bytes)
  (define-gamma okvs-transaction-context)
  (define-gamma okvs-transaction-context!)

  (define make-okvs-parameter
    (lambda (default)
      (define key (gensym "okvs-transaction-parameter"))
      (case-lambda
        ((tx) (okvs-transaction-context tx key default))
        ((tx value) (okvs-transaction-context! tx key value)))))

  (define-syntax okvs-parameterize
    (syntax-rules ()
      ((_ tx ((parameter value) ...) body ...)
       ;; use dynamic wind, or explicitly forbid abnormal exit
       (let ((b* (parameter tx)) ...)
         (parameter tx value) ...
         (call-with-values (lambda () body ...)
           (lambda args
             (parameter tx b*) ...
             (apply values args)))))))

  (define-gamma okvs-begin-hook)
  (define-gamma okvs-pre-commit-hook)
  (define-gamma okvs-post-commit-hook)
  (define-gamma okvs-rollback-hook)
  (define-gamma call-with-okvs-transaction)
  (define-gamma call-with-okvs-transaction-read-only)
  (define-gamma okvs-approximate-key-count)
  (define-gamma okvs-approximate-byte-count)
  (define-gamma okvs-set!)
  (define-gamma okvs-clear!)
  (define-gamma call-with-okvs-cursor)
  (define-gamma okvs-next)
  (define-gamma okvs-previous)
  (define-gamma okvs-key)
  (define-gamma okvs-value)
  (define-gamma okvs-query)
  (define-gamma okvs->alist)
  (define-gamma okvs-write)
  (define-gamma okvs-read)

  ;; checks

  (define-syntax check
    (syntax-rules ()
      ((check v)
       (let ((v* v))
         (eq? v* #t)))
      ((check a b)
       (let ((a* a)
             (b* b))
         (check (equal? a* b*))))))

  (define pk
    (lambda args
      (write args)(newline)
      (flush-output-port)
      (car (reverse args))))

  (define okvs-checks (list))

  (define add!
    (lambda (name proc)
      (set! okvs-checks (cons (cons name proc) okvs-checks))
      proc))

  (define-syntax define*
    (syntax-rules ()
      ((define* name proc)
       (define name (add! 'name proc)))))

  (define* check-okvs2-000
    (values (lambda (make-okvs)
              ;; a database can be closed
              (check (okvs-close (make-okvs))))))

  (define* check-okvs2-001
    (values (lambda (make-okvs)
              ;; a fresh database is empty
              (check (okvs-empty? (make-okvs))))))

  (define* check-okvs2-002
    (values (lambda (make-okvs)
              ;; okvs-key-max-size default value
              (check (< 1 (okvs-key-max-length (make-okvs)))))))

  (define* check-okvs2-004
    (values (lambda (make-okvs)
              ;; okvs-value-max-size default value
              (check (< 1 (okvs-value-max-length (make-okvs)))))))

  (define* check-okvs2-005
    (values (lambda (make-okvs)
              ;; transaction can run
              (check (call-with-okvs-transaction (make-okvs) (lambda (tx) #t))))))

  (define* check-okvs2-006
    (values (lambda (make-okvs)
              ;; raise inside a transaction is propagated
              (define singleton '(check-raise))
              (check
               (guard (ex (else (eq? ex singleton)))
                 (call-with-okvs-transaction (make-okvs) (lambda (tx) (raise singleton)))
                 #f)))))

  (define* check-okvs2-006-bis
    (values (lambda (make-okvs)
              ;; raise exception, use procedure failure

              (define raise-zero '(check-raise-zero))
              (define raise-one '(check-raise-one))

              (check
               (guard (ex (else (eq? raise-one ex)))
                 (call-with-okvs-transaction (make-okvs)
                   (lambda (tx) (raise raise-zero))
                   (lambda (ex)
                     (check (eq? raise-zero ex))
                     (raise raise-one))))))))

  (define* check-okvs2-007
    (values (lambda (make-okvs)
              ;; transaction can run, check exception, use success

              (check (= 42
                        (call-with-okvs-transaction (make-okvs)
                          (lambda (tx) 41)
                          raise
                          (lambda (x) (+ x 1))))))))

  (define* check-okvs2-008
    (values (lambda (make-okvs)
              ;; okvs parameter is set inside transaction
              (define my-okvs-parameter (make-okvs-parameter 'check-okvs-symbol))

              (check
               (eq? 'check-okvs-symbol
                    (call-with-okvs-transaction (make-okvs)
                      (lambda (tx)
                        (my-okvs-parameter tx))))))))

  (define* check-okvs2-009
    (values (lambda (make-okvs)
              ;; okvs parameter can be set inside transaction
              (define my-okvs-parameter (make-okvs-parameter 'check-okvs-symbol))

              (check
               (eq? 'my-okvs-parameter-reset
                    (call-with-okvs-transaction (make-okvs)
                      (lambda (tx)
                        (my-okvs-parameter tx 'my-okvs-parameter-reset)
                        (my-okvs-parameter tx))))))))

  (define* check-okvs2-010
    (values (lambda (make-okvs)
              ;; okvs parameter can be parameterized
              (define my-okvs-parameter (make-okvs-parameter 'init))

              (check
               (eq? 'set
                    (call-with-okvs-transaction (make-okvs)
                      (lambda (tx)
                        (check (eq? 'init (my-okvs-parameter tx)))
                        (okvs-parameterize tx ((my-okvs-parameter 'set))
                                           (my-okvs-parameter tx)))))))))

  (define* check-okvs2-011
    (values (lambda (make-okvs)
              ;; okvs parameter does not leak across transactions
              (define my-okvs-parameter (make-okvs-parameter 'init))

              (check
               (eq? 'set
                    (call-with-okvs-transaction (make-okvs)
                      (lambda (tx)
                        (check (eq? 'init (my-okvs-parameter tx)))
                        (okvs-parameterize tx ((my-okvs-parameter 'set))
                                           (my-okvs-parameter tx))))))
              (check
               (eq? 'init
                    (call-with-okvs-transaction (make-okvs)
                      (lambda (tx)
                        (my-okvs-parameter tx))))))))

  (define* check-okvs2-012
    (values (lambda (make-okvs)
              ;; okvs begin hook
              (define okvs (make-okvs))
              (define my-okvs-parameter (make-okvs-parameter 'init))
              (define value #f)

              (hook-add! (okvs-begin-hook okvs)
                         (lambda (tx)
                           (set! value (my-okvs-parameter tx))))

              (check
               (eq? 'init
                    (call-with-okvs-transaction okvs
                      (lambda (tx)
                        (my-okvs-parameter tx)))))

              (check value 'init))))

  (define* check-okvs2-013
    (values (lambda (make-okvs)
              ;; okvs pre commit hook
              (define okvs (make-okvs))
              (define called? #f)

              (hook-add! (okvs-pre-commit-hook okvs)
                         (lambda (tx)
                           (set! called? #t)))

              (call-with-okvs-transaction okvs
                (lambda (tx)
                  (values)))

              (check called?))))

  (define* check-okvs2-014
    (values (lambda (make-okvs)
              ;; okvs post commit hook
              (define okvs (make-okvs))
              (define called? #f)

              (hook-add! (okvs-post-commit-hook okvs)
                         (lambda (tx)
                           (set! called? #t)))

              (call-with-okvs-transaction okvs
                (lambda (tx)
                  (values)))

              (check called?))))

  (define* check-okvs2-015
    (values (lambda (make-okvs)
              ;; okvs rollback commit hook

              (define okvs (make-okvs))
              (define okvs-post-commit-called? #f)
              (define okvs-rollback-called? #f)

              (hook-add! (okvs-post-commit-hook okvs)
                         (lambda (tx)
                           (set! okvs-post-commit-called? #t)))

              (hook-add! (okvs-rollback-hook okvs)
                         (lambda (tx)
                           (set! okvs-rollback-called? #t)))

              (check (eq? 'check-okvs2-015
                          (call-with-okvs-transaction okvs
                            (lambda (tx)
                              (raise 'check-okvs2-015))
                            values)))

              (check (not okvs-post-commit-called?))
              (check okvs-rollback-called?))))

  (define* check-okvs2-016
    (values (lambda (make-okvs)
              ;; okvs call-with-okvs-transaction
              (define okvs (make-okvs))

              (check (not (call-with-okvs-transaction okvs
                            (lambda (tx)
                              (okvs-query tx (bytevector 42))))))
              (check (call-with-okvs-transaction okvs
                       (lambda (tx)
                         (okvs-set! tx (bytevector 42) (bytevector 42))
                         #t)))
              (check (equal?
                      (bytevector 42)
                      (call-with-okvs-transaction okvs
                        (lambda (tx)
                          (okvs-query tx (bytevector 42)))))))))

  (define* check-okvs2-017
    (values (lambda (make-okvs)
              ;; okvs call-with-okvs-transaction-read-only
              (define okvs (make-okvs))

              (check (call-with-okvs-transaction okvs
                       (lambda (tx)
                         (okvs-set! tx (bytevector 42) (bytevector 42))
                         #t)))

              (check (equal?
                      (bytevector 42)
                      (call-with-okvs-transaction-read-only okvs
                                                            (lambda (tx)
                                                              (okvs-query tx (bytevector 42)))))))))

  (define* check-okvs2-018
    (lambda (make-okvs)
      ;; okvs call-with-okvs-transaction-read-only read-only
      (define okvs (make-okvs))

      (check (okvs-error?
              (call-with-okvs-transaction-read-only
               okvs
               (lambda (tx)
                 (okvs-set! tx (bytevector 42) (bytevector 42))
                 #t)
               values)))))

  (define* check-okvs2-019
    (values (lambda (make-okvs)
              ;; okvs-approximate-key-count
              (define okvs (make-okvs))
              (check (= (okvs-approximate-key-count okvs) 0)))))

  (define* check-okvs2-020
    (values (lambda (make-okvs)
              ;; okvs-approximate-key-count
              (define okvs (make-okvs))

              (check (call-with-okvs-transaction okvs
                       (lambda (tx)
                         (okvs-set! tx (bytevector 42) (bytevector 42))
                         #t)))

              (check (= (okvs-approximate-key-count okvs) 1)))))

  (define* check-okvs2-021
    (values (lambda (make-okvs)
              ;; okvs-approximate-key-count
              (define okvs (make-okvs))

              (let loop ((index 100))
                (unless (zero? index)
                  (call-with-okvs-transaction okvs
                    (lambda (tx)
                      (okvs-set! tx (bytevector index) (bytevector index))))
                  (loop (- index 1))))

              (check (= (okvs-approximate-key-count okvs) 100)))))

  (define* check-okvs2-022
    (values (lambda (make-okvs)
              ;; okvs-approximate-byte-count
              (define okvs (make-okvs))
              (check (= (okvs-approximate-byte-count okvs) 0)))))

  (define* check-okvs2-023
    (values (lambda (make-okvs)
              ;; okvs-approximate-byte-count
              (define okvs (make-okvs))

              (check (call-with-okvs-transaction okvs
                       (lambda (tx)
                         (okvs-set! tx (bytevector 42) (bytevector 42))
                         #t)))

              (check (= (okvs-approximate-byte-count okvs) 2)))))

  (define* check-okvs2-024
    (values (lambda (make-okvs)
              ;; okvs-approximate-byte-count
              (define okvs (make-okvs))

              (let loop ((index 100))
                (unless (zero? index)
                  (call-with-okvs-transaction okvs
                    (lambda (tx)
                      (okvs-set! tx (bytevector index) (bytevector index))))
                  (loop (- index 1))))

              (check (= (okvs-approximate-byte-count okvs) (* 2 100))))))

  (define* check-okvs2-025
    (values (lambda (make-okvs)

              (define okvs (make-okvs))

              (check (not (okvs-query okvs (bytevector 42))))
              (okvs-set! okvs (bytevector 42) (bytevector 42))
              (check (equal? (bytevector 42)
                             (okvs-query okvs (bytevector 42))))
              (okvs-clear! okvs (bytevector 42))
              (check (not (okvs-query okvs (bytevector 42)))))))

  (define* check-okvs2-026
    (values (lambda (make-okvs)
              ;; okvs-approximate-byte-count
              (define okvs (make-okvs))

              (check (= 0 (okvs-approximate-key-count okvs)))

              (let loop ((index 100))
                (unless (zero? index)
                  (call-with-okvs-transaction okvs
                    (lambda (tx)
                      (okvs-set! tx (bytevector index) (bytevector index))))
                  (loop (- index 1))))

              (check (= 100 (okvs-approximate-key-count okvs)))

              (okvs-clear! okvs (bytevector 00) (bytevector 255))

              (check (= 0 (okvs-approximate-key-count okvs))))))

  (define* check-okvs2-027
    (values (lambda (make-okvs)
              (define okvs (make-okvs))

              (call-with-okvs-cursor okvs (bytevector 13 37)
                (lambda (cursor position)
                  (check (not cursor))
                  (check (not position)))))))

  (define bytevector-compare*
    (lambda (a b)
      (case (byter-compare a b)
        ((smaller) 'key-before)
        ((bigger) 'key-after)
        ((equal) 'key-exact))))

  (define* check-okvs2-028
    (values (lambda (make-okvs)
              (define okvs (make-okvs))

              (okvs-set! okvs (bytevector 13 38) (bytevector 42))
              (okvs-set! okvs (bytevector 13 36) (bytevector 42))

              (call-with-okvs-cursor okvs (bytevector 13 37)
                (lambda (cursor position)
                  (check position
                         (bytevector-compare* (okvs-key cursor)
                                              (bytevector 13 37))))))))

  (define* check-okvs2-029
    (values (lambda (make-okvs)
              (define okvs (make-okvs))

              (okvs-set! okvs (bytevector 13 38) (bytevector 42))

              (call-with-okvs-cursor okvs (bytevector 13 37)
                (lambda (cursor position)
                  (check position
                         (bytevector-compare* (okvs-key cursor)
                                              (bytevector 13 37))))))))

  (define* check-okvs2-030
    (values (lambda (make-okvs)
              (define okvs (make-okvs))

              (okvs-set! okvs (bytevector 13 36) (bytevector 42))
              (okvs-set! okvs (bytevector 13 37) (bytevector 42))
              (okvs-set! okvs (bytevector 13 38) (bytevector 42))

              (call-with-okvs-cursor okvs (bytevector 13 37)
                (lambda (cursor position)
                  (check position
                         (bytevector-compare* (bytevector 13 37)
                                              (okvs-key cursor))))))))

  (define* check-okvs2-031
    (values (lambda (make-okvs)
              (define okvs (make-okvs))

              (okvs-set! okvs (bytevector 13 36) (bytevector 36))
              (okvs-set! okvs (bytevector 13 37) (bytevector 37))
              (okvs-set! okvs (bytevector 13 38) (bytevector 38))

              (call-with-okvs-cursor okvs (bytevector 13 37)
                (lambda (cursor position)
                  (check 'key-exact position)
                  (check (okvs-previous cursor))
                  (check (bytevector 36)
                         (okvs-value cursor)))))))

  (define* check-okvs2-032
    (values (lambda (make-okvs)
              (define okvs (make-okvs))

              (okvs-set! okvs (bytevector 13 36) (bytevector 36))
              (okvs-set! okvs (bytevector 13 37) (bytevector 37))
              (okvs-set! okvs (bytevector 13 38) (bytevector 38))

              (check (bytevector 37)
                     (okvs-query okvs (bytevector 13 37))))))

  (define* check-okvs2-033
    (values (lambda (make-okvs)
              (define okvs (make-okvs))

              (okvs-set! okvs (bytevector 13 36) (bytevector 36))
              (okvs-set! okvs (bytevector 13 37) (bytevector 37))
              (okvs-set! okvs (bytevector 13 38) (bytevector 38))

              (check (list (cons (bytevector 13 36)
                                 (bytevector 36))
                           (cons (bytevector 13 37)
                                 (bytevector 37)))
                     (okvs-query okvs
                                 (bytevector 13 36)
                                 (bytevector 13 38))))))

  (define* check-okvs2-034
    (values (lambda (make-okvs)
              (define okvs (make-okvs))

              (okvs-set! okvs (bytevector 13 36) (bytevector 36))
              (okvs-set! okvs (bytevector 13 37) (bytevector 37))
              (okvs-set! okvs (bytevector 13 38) (bytevector 38))

              (check (list (cons (bytevector 13 38)
                                 (bytevector 38))
                           (cons (bytevector 13 37)
                                 (bytevector 37)))
                     (okvs-query okvs
                                 (bytevector 13 38)
                                 (bytevector 13 36))))))

  (define make-seed
    (lambda ()
      (let* ((now (current-time))
             (seed (* (time-second now) (time-nanosecond now))))
        (+ (modulo seed (expt 2 32)) 1))))

  (define random-byte
    (lambda ()
      (random 256)))

  (define bytevector-max
    (lambda (n)
      (apply bytevector
             (map (lambda (x) 255)
                  (iota n)))))

  (define random-bytevector-key
    (lambda (n)
      (let loop ((out '(00))
                 (length (random n)))
        (if (fxzero? length)
            (apply bytevector out)
            (loop (cons (random-byte) out)
                  (fx- length 1))))))

  (define random-alist
    (lambda (n)
      (let loop ((length (+ (random n) 1))
                 (out '()))
        (if (zero? length)
            out
            (loop (- length 1)
                  (cons (cons (random-bytevector-key n)
                              (bytevector (random 256) (random 256)))
                        out))))))

  (define less?
    (lambda (x y)
      (case (byter-compare (car x) (car y))
        ((smaller) #t)
        (else #f))))

  (define-syntax do-times
    (syntax-rules ()
      ((do-times n body ...)
       (let loop ((index n))
         (unless (zero? index)
           (let () body ...)
           (loop (- index 1)))))))

  (define* check-okvs2-fuzz-000
    (values (lambda (make-okvs)
              (do-times 1 #;(if (getenv "OKVS2_SEED") 1 (expt 10 3))
                (define okvs (make-okvs))
                (define seed (string->number (or (getenv "OKVS2_SEED")
                                                 (number->string (make-seed)))))
                (define alist* (let ()
                                 (display (string-append "*** OKVS2_SEED="
                                                         (number->string seed) "\n"))
                                 (random-seed seed)
                                 (random-alist (okvs-key-max-length okvs))))


                (let loop ((alist alist*))
                  (if (null? alist)
                      (let ((expected (list-sort less? alist*))
                            (given (okvs-query okvs
                                               (bytevector 00)
                                               (bytevector-max (okvs-key-max-length okvs)))))
                        (check expected given))
                      (begin
                        (okvs-set! okvs (caar alist) (cdar alist))
                        (loop (cdr alist))))))
              #t)))

  (define call-with-temporary-directory
    (lambda (proc)
      (define stdlib (load-shared-object #f))

      (define mkdtemp
        (foreign-procedure "mkdtemp" (string) string))

      (define (make-temporary-directory prefix)
        (let ((input (string-append prefix "-XXXXXX")))
          (mkdtemp input)))

      (define tmp (make-temporary-directory "/tmp/check-okvs2-"))
      (call-with-values (lambda () (proc tmp))
        (lambda args
          ;; That is the expected behavior that when proc raise an
          ;; exception, the temporary directory is not deleted.
          (delete-directory tmp)
          (apply values args)))))

  (define* check-okvs2-035
    (lambda (make-okvs)
      (define okvs (make-okvs))
      (okvs-set! okvs (bytevector 42) (bytevector 42))
      (assert (equal? (okvs-query okvs (bytevector 42))
                      (bytevector 42)))
      (okvs-set! okvs (bytevector 42) #f)
      (assert (equal? (okvs-query okvs (bytevector 42))
                      #f))
      (okvs-set! okvs (bytevector 42) '(qux))
      (assert (equal? (okvs-query okvs (bytevector 42))
                      '(qux)))
      (okvs-clear! okvs (bytevector 42))
      (assert (equal? (okvs-query okvs (bytevector 42))
                      #f))))

  (define random-okvs
    (lambda (make-okvs n)
      (define out (make-okvs))
      (for-each (lambda (x) (okvs-set! out (car x) (cdr x))) (random-alist n))
      out))

  (define every
    (lambda (xs)
      (or (null? xs)
          (and (car xs)
               (every (cdr xs))))))

  (define gte!
    (lambda (c key)
      (case (byter-compare (okvs-key c) key)
        (smaller (and (okvs-next c) (gte! c key)))
        (equal 'equal)
        (bigger 'bigger))))

  (define kvlist-sort
    (lambda (kv)
      (list-sort less? kv)))

  (define okvs-zigzag
    (lambda (okvsx start end)
      (make-coroutine-generator
       (lambda (yield)
         (define cxkv (let loop ((okvsx okvsx)
                                 (i 0)
                                 (cx '())
                                 (out '()))
                        (if (null? okvsx)
                            (cons cx (if (null? out) #f
                                         (let* ((x (kvlist-sort out))
                                                (k (car (car x)))
                                                (v (cdr (car x))))
                                           (cons (car (byter-read k)) v))))
                            (call-with-okvs-cursor (car okvsx) start
                              (lambda (cursor position)
                                (if (okvs-valid? cursor)
                                    (loop (cdr okvsx)
                                          (fx+ i 1)
                                          (cons cursor cx)
                                          (cons (cons (byter-write (cons (okvs-key cursor) i))
                                                      (okvs-value cursor))
                                                out))
                                    (loop (cdr okvsx) i cx out)))))))
         (unless (and (null? (car cxkv))
                      (not (cdr cxkv)))
           (yield (cdr cxkv))
           (let loop ((cx (car cxkv))
                      (key (and (cdr cxkv) (cadr cxkv))))
             (unless (null? cx)
               (let ((ckv (filter values (map (lambda (x)
                                                (when (eq? (gte! x key) 'equal)
                                                  (okvs-next x))
                                                (and (okvs-valid? x) (list x (okvs-key x) (okvs-value x)))) cx))))
                 (unless (null? ckv)
                   (let ((kv (kvlist-sort (map cdr ckv))))
                     (yield (apply cons (car kv)))
                     (loop (map car ckv)
                           (caar kv))))))))))))

  (define okvs-merge
    (lambda (okvs . others)
      (generator-for-each (lambda (x) (okvs-set! okvs (car x) (cdr x))) (okvs-zigzag others (bytevector 00) (bytevector 255 255)))
      okvs))

  (define* check-okvs-zigzag-cursor
    (lambda (make-okvs)
      (define okvsx (map (lambda _ (random-okvs make-okvs 5)) (iota 5)))
      (define expected (delete-duplicates (list-sort less? (apply append (map okvs->alist okvsx))) (lambda (a b) (bytevector=? (car a) (car b)))))
      (equal? (map car expected)
              (map car (generator->list (okvs-zigzag okvsx (bytevector 00) (bytevector-max (okvs-key-max-length (make-okvs)))))))))

  (define* check-okvs-zigzag-cursor-fuzz
    (lambda (make-okvs)
      (do-times (if (getenv "LETLOOP_SEED") 1 (expt 10 2))
        (define seed (random-seed
                      (string->number (or (getenv "LETLOOP_SEED")
                                          (number->string (make-seed))))))
        (define okvsx (map (lambda _ (random-okvs make-okvs 255)) (iota (+ (random 42) 1))))
        (define expected (delete-duplicates (list-sort less? (apply append (map okvs->alist okvsx))) (lambda (a b) (bytevector=? (car a) (car b)))))
        (define zz (generator->list (okvs-zigzag okvsx (bytevector 00) (bytevector-max (okvs-key-max-length (make-okvs))))))
        (assert (equal? (map car expected) (map car zz))))
      #t))

  (define make-okvs-checks
    (lambda (make-okvs)
      (for-each (lambda (check)
                  (format #t "** ~a\n" check)
                  (assert ((cdr check) make-okvs)))
                (reverse okvs-checks))
      #t))

  )
