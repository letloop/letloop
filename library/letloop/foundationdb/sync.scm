(library (letloop foundationdb sync)

  (export
   make-fdb
   fdb-close
   call-with-fdb
   call-with-fdb-transaction
   fdb-set!
   fdb-clear!
   fdb-query
   strinc
   ~check-letloop-foundationdb-sync-000
   ~check-letloop-foundationdb-sync-001
   ~check-letloop-foundationdb-sync-002
   ~check-letloop-foundationdb-sync-003
   ~fuzz
   )

  (import (chezscheme)
          (rename (only (letloop byter) byter-compare)
                  (byter-compare byter-bytevector-compare))
          (letloop foundationdb base)
          (letloop foundationdb helpers)
          (letloop foundationdb pack))

  (begin

    (define-record-type* <fdb>
      (make-fdb% database)
      fdb?
      (database fdb-database))

    (define-record-type* <transaction>
      (make-transaction pointer)
      okvs-transaction?
      (pointer transaction-pointer))

    (define network-thread-mutex (make-mutex))
    (define network-thread-setup? #f)

    (define (fdb-init!)
      (with-mutex network-thread-mutex
        (unless network-thread-setup?
          (set! network-thread-setup? #t)
          (fdb-select-api-version 710)
          (fdb-setup-network!)
          (fork-thread (lambda () (fdb-run-network))))))

    (define make-fdb
      (case-lambda
       (() (make-fdb #f))
       ((cluster-file)
        (fdb-init!)
        (make-fdb% (fdb-create-database cluster-file)))))

    (define call-with-fdb
      (lambda (proc)
        (define fdb (make-fdb))
        (call-with-values (lambda () (proc fdb))
          (lambda args
            (fdb-close fdb)
            (apply values args)))))

    (define (fdb-close fdb)
      (fdb-database-destroy (fdb-database fdb)))

    (define (fdb-transaction-begin fdb)
      (make-transaction (fdb-database-create-transaction (fdb-database fdb))))

    (define (fdb-future-block-until-ready* future)
      (unless (fdb-future-ready? future)
        (fdb-future-block-until-ready future)))

    (define (fdb-transaction-commit* transaction)
      (let ((future (fdb-transaction-commit! (transaction-pointer transaction))))
        (fdb-future-block-until-ready* future)
        (let ((error (fdb-future-get-error future)))
          (fdb-future-destroy future)
          error)))

    (define (fdb-transaction-rollback transaction)
      (fdb-transaction-cancel! (transaction-pointer transaction))
      (fdb-transaction-destroy (transaction-pointer transaction)))

    (define call-with-fdb-transaction
      (lambda (fdb proc)
        (let ((tx (fdb-transaction-begin fdb)))
          (let retry ()
            (call-with-values (lambda () (proc tx))
              (lambda out
                ;; try to commit
                (let ((error (fdb-transaction-commit* tx)))
                  (if (fxzero? error)
                      (begin ;; success
                        (fdb-transaction-destroy (transaction-pointer tx))
                        (apply values out))
                      ;; there is an error, retry or raise
                      (let ((future (fdb-transaction-on-error (transaction-pointer tx) error)))
                        (fdb-future-block-until-ready* future)
                        (let ((error (fdb-future-get-error future)))
                          (fdb-future-destroy future)
                          ;; XXX: The official python bindings will
                          ;; neither reset the transaction, nor rollback
                          ;; the failed transaction in case of retry...
                          (if (fxzero? error)
                              (retry)
                              (begin
                                (fdb-transaction-rollback tx)
                                (fdb-error error)))))))))))))

    (define (fdb-ref tx key)
      (let ((future (fdb-transaction-get (transaction-pointer tx)
                                         key
                                         #f)))
        (fdb-future-block-until-ready* future)
        (let ((error (fdb-future-get-error future)))
          (if (fxzero? error)
              (let ((value (fdb-future-get-value future)))
                (fdb-future-destroy future)
                value)
              (begin
                (fdb-future-destroy future)
                (fdb-error error))))))

    (define (fdb-set! tx key value)
      (fdb-transaction-set! (transaction-pointer tx) key value))

    (define (fdb-delete! transaction key)
      (fdb-transaction-clear! (transaction-pointer transaction) key))

    (define fdb-clear!
      (case-lambda
       ((txn key)
        (fdb-delete! txn key))
       ((txn key other)
        (fdb-transaction-clear-range! (transaction-pointer txn) key other))))

    (define (fdb-range transaction
                       start-key
                       start-include?
                       end-key
                       end-include?
                       reverse?)
      (let iterate ((iteration 0)
                    (start-include? start-include?)
                    (start-key start-key)
                    (out '()))
        ;; XXX: Key selectors are a pain
        ;; ref: https://apple.github.io/foundationdb/api-c.html#key-selectors
        (let ((future (fdb-transaction-get-range-page (transaction-pointer transaction)
                                                      start-key
                                                      start-include?
                                                      1
                                                      end-key
                                                      end-include?
                                                      1
                                                      -1
                                                      -1
                                                      -2 ;; WANT_ALL
                                                      iteration
                                                      #f
                                                      reverse?)))
          (fdb-future-block-until-ready* future)
          (let ((error (fdb-future-get-error future)))
            (if (fxzero? error)
                (call-with-values (lambda () (fdb-future-get-range-page future))
                  (lambda (range more?)
                    (fdb-future-destroy future)
                    (if more?
                        (iterate (fx+ iteration 1)
                                 #t
                                 ;; TODO: do better than this.
                                 (caar (reverse range))
                                 (append out range))
                        (append out range))))
                (begin
                  (fdb-future-destroy future)
                  (fdb-error error)))))))

    (define fdb-query
      (case-lambda
       ((txn key)
        (fdb-ref txn key))
       ((txn key other)
        (case (byter-bytevector-compare key other)
          (smaller (fdb-range txn key #f other #f #f))
          (bigger (fdb-range txn other #t key #t #t))
          (equal (error '(letloop foundationdb sync) "Invalid fdb-query, KEY, and OTHER are equal."))))))

    (define (strinc bytevector)
      "Return the first bytevector that is not prefix of BYTEVECTOR"
      ;; See https://git.io/fj34F, TODO: OPTIMIZE
      (let ((bytes (reverse (bytevector->u8-list bytevector))))
        ;; strip #xFF
        (let loop ((out bytes))
          (when (null? out)
            (error 'foundationdb
                   "BYTEVECTOR must contain at least one byte not equal to #xFF."
                   bytevector))
          (if (= (car out) #xFF)
              (loop (cdr out))
              (set! bytes out)))
        ;; increment first byte, reverse and return the bytevector
        (u8-list->bytevector (reverse (cons (+ 1 (car bytes)) (cdr bytes))))))

  (define fdb-reset!
    (lambda (fdb)
      (call-with-fdb-transaction fdb
        (lambda (txn)
          (fdb-clear! txn (bytevector) (bytevector 254 254))))))

  (define ~check-letloop-foundationdb-sync-000
    (lambda ()
      (assert (equal? #f
                      (call-with-fdb
                        (lambda (fdb)
                          (fdb-reset! fdb)
                          (call-with-fdb-transaction fdb
                            (lambda (txn)
                              (fdb-query txn (bytevector 42))))))))))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (keyword args ...) body ...)
       (define-syntax keyword
         (syntax-rules ()
           ((keyword args ...) body ...))))))

  (define-syntax-rule (do-times n body ...)
    (begin
      (let loop ((index n))
        (when (= (modulo index 100) 0)
          (unless (zero? index)
            body ...
            (loop (- index 1)))))
      #t))

  (define ~check-letloop-foundationdb-sync-001
    (lambda ()
      (do-times
       (expt 10 3)
       (assert (equal? (bytevector 101)
                       (call-with-fdb
                         (lambda (fdb)
                           (fdb-reset! fdb)
                           (call-with-fdb-transaction fdb
                             (lambda (txn)
                               (fdb-set! txn (bytevector 42) (bytevector 101))
                               (fdb-query txn (bytevector 42)))))))))))

  (define ~check-letloop-foundationdb-sync-002
    (lambda ()
      (call-with-fdb
        (lambda (fdb)
          (fdb-reset! fdb)
          (call-with-fdb-transaction fdb
            (lambda (txn)
              (fdb-set! txn (bytevector 42) (bytevector 101))))
          (assert (equal? (bytevector 101)
                          (call-with-fdb-transaction fdb
                            (lambda (txn)
                              (fdb-query txn (bytevector 42))))))))))

  (define bytevector-random
    (lambda (length)
      (let loop ((length length)
                 (out '()))
        (if (fxzero? length)
            (apply bytevector out)
            (loop (fx- length 1)
                  (cons (random 256) out))))))

  (define (bytevector-append . bvs)
    (let* ((total (apply fx+ (map bytevector-length bvs)))
           (out (make-bytevector total)))
      (let loop ((bvs bvs)
                 (index 0))
        (unless (null? bvs)
          (bytevector-copy! (car bvs) 0 out index (bytevector-length (car bvs)))
          (loop (cdr bvs) (fx+ index (bytevector-length (car bvs))))))
      out))

  (define ~fuzz
    (lambda ()
      (call-with-fdb
        (lambda (fdb)

          (do-times (expt 10 3)
            (do-times (expt 10 3)
              (call-with-fdb-transaction fdb
                (lambda (tx)
                  #t)))

            (do-times (expt 10 3)
              (fdb-reset! fdb))

            (do-times (expt 10 3)
              (fdb-reset! fdb))
            
            (do-times (expt 10 3)
              (call-with-fdb-transaction fdb
                (lambda (tx)
                  (fdb-ref tx (bytevector 42)))))

            (do-times (expt 10 3)
              (call-with-fdb-transaction fdb
                (lambda (tx)
                  (fdb-set! tx (bytevector 42) (bytevector 42)))))

            (do-times (expt 10 3)
              (call-with-fdb-transaction fdb
                (lambda (tx)
                  (fdb-ref tx (bytevector 42)))))

            (do-times (expt 10 3)
              (call-with-fdb-transaction fdb
                (lambda (tx)
                  (fdb-set! tx (bytevector 42) (bytevector 42))
                  (fdb-ref tx (bytevector 42)))))
            
            (do-times (expt 10 3)
              (call-with-fdb-transaction fdb
                (lambda (tx)
                  (fdb-query tx (bytevector 42) (bytevector 43)))))
            
            (do-times (expt 10 3)
              (call-with-fdb-transaction fdb
                (lambda (tx)
                  (fdb-delete! tx (bytevector 42)))))

            (do-times (expt 10 3)
              (call-with-fdb-transaction fdb
                (lambda (tx)
                  (fdb-set! tx (bytevector 42) (bytevector 42))
                  (fdb-ref tx (bytevector 42))
                  (fdb-delete! tx (bytevector 42))
                  (fdb-query tx (bytevector 42) (bytevector 43)))))

            (do-times (expt 10 3)
              (call-with-fdb-transaction fdb
                (lambda (tx)
                  (fdb-set! tx (bytevector 42) (bytevector 42))
                  (fdb-ref tx (bytevector 42))
                  (fdb-clear! tx (bytevector) (bytevector 255))
                  (fdb-query tx (bytevector 42) (bytevector 43))))))))))
  
  (define ~check-letloop-foundationdb-sync-003
    (lambda ()
      (call-with-fdb
        (lambda (fdb)
          (fdb-reset! fdb)
          (call-with-fdb-transaction fdb
            (lambda (txn)
              (fdb-set! txn (bytevector-append (bytevector 0)) (bytevector 0))
              (fdb-set! txn (bytevector-append (bytevector 1) (bytevector-random 4)) (bytevector 1))
              (fdb-set! txn (bytevector-append (bytevector 2) (bytevector-random 4)) (bytevector 2))
              (fdb-set! txn (bytevector-append (bytevector 3) (bytevector-random 4)) (bytevector 3))
              (fdb-set! txn (bytevector-append (bytevector 4) (bytevector-random 4)) (bytevector 4))
              (fdb-set! txn (bytevector-append (bytevector 5) (bytevector-random 4)) (bytevector 5))
              (fdb-set! txn (bytevector-append (bytevector 6) (bytevector-random 4)) (bytevector 6))
              (fdb-set! txn (bytevector-append (bytevector 7)) (bytevector 7))
              (void)))

          (call-with-fdb-transaction fdb
            (lambda (txn)
              (let ((out (fdb-query txn (bytevector 1) (bytevector 7))))
                (assert (= 6 (length out)))
                (assert (= 1 (bytevector-u8-ref (caar out) 0)))
                (assert (= 6 (bytevector-u8-ref (caar (reverse out)) 0))))

              (let ((out (fdb-query txn (bytevector 0) (bytevector 8))))
                (assert (= 8 (length out)))
                (assert (= 0 (bytevector-u8-ref (caar out) 0)))
                (assert (= 7 (bytevector-u8-ref (caar (reverse out)) 0))))

              (let ((out (fdb-query txn (bytevector 8) (bytevector 0))))
                (assert (= 7 (length out))))

              (let ((out (fdb-query txn (bytevector 7) (bytevector 0))))
                (assert (= 7 (length out))))

              (let ((out (fdb-query txn (bytevector 7) (bytevector 1))))
                (assert (= 7 (length out))))))))))))
