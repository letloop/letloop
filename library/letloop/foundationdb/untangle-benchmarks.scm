(import (chezscheme)
        (letloop foundationdb untangle)
        (letloop untangle)
        (letloop foundationdb pack))


(define (pk . args)
  (write args)(newline)(flush-output-port)
  (car (reverse args)))

(define (bytevector-random count)
  (let ((bv (make-bytevector count)))
    (let loop ((index count))
      (unless (fxzero? index)
        (bytevector-u8-set! bv (fx- index 1) (random 256))
        (loop (fx- index 1))))
    bv))

(define (insert! tx prefix count)
  (let insert ((count count))
    (if (fxzero? count)
        'ok
        (let ((key* (bytevector-random 64)))
          (let ((key (pack prefix key*))
                (value (bytevector-random (expt 10 2))))
            (fdb-set! tx key value)
            (insert (fx- count 1)))))))

(define (count? tx prefix)
  (pk 'counting?)
  (fdb-query tx (pack prefix) (strinc (pack prefix))))


(define my-prefix (bytevector-random 64))
(define my-prefix2 (bytevector-random 64))

(define (debugx range)
  ;; (for-each (lambda (kv) (pk 'debug (unpack (car kv)))) range)
  (length range))

(pk 'test)
(with-untangle
 (call-with-fdb
   (lambda (db)

     (call-with-fdb-transaction db
       (lambda (txn)
         (fdb-clear! txn (bytevector) (bytevector 254 254))))

     (let loop ((count 100))
       (unless (fxzero? count)
         (call-with-fdb-transaction db (lambda (tx) (insert! tx my-prefix 1000)))
         (loop (fx- count 1))))

     (pk 'count0...)
     (pk 'count0 (debugx (call-with-fdb-transaction db (lambda (tx) (count? tx my-prefix)))))

     (pk 'out1 (call-with-fdb-transaction db (lambda (tx) (insert! tx my-prefix2 1000))))

     (pk 'count1 (debugx (call-with-fdb-transaction db (lambda (tx) (count? tx my-prefix2)))))
     (untangle-stop))))
