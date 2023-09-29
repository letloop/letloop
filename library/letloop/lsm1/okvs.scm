(library (letloop lsm1 okvs)
  (export make-okvs
          okvs-close
          call-with-okvs-transaction
          okvs-query
          okvs-set!
          okvs-clear!
          ~check-okvs-000
          ~check-okvs-001
          ~check-okvs-002
          ~check-okvs-003
          ~check-okvs-004
          ~check-okvs-005
          ~check-okvs-006
          ~check-okvs-007
          ~check-okvs-008
          ~check-okvs-009
          )
  (import (chezscheme)
          (letloop r999)
          (letloop byter)
          (letloop lsm1))

  (begin

    (define-record-type* <db>
      (make-okvs~ path lsm)
      db?
      (path okvs-path)
      (lsm okvs-lsm))

    (define make-okvs
      (lambda (path)
        (define lsm (lsm1-new))
        (lsm1-open lsm path)
        (make-okvs~ path lsm)))

    (define okvs-close
      (lambda (db)
        (lsm1-close (okvs-lsm db))))

    (define call-with-okvs-transaction
      (lambda (db proc)
        (lsm1-begin (okvs-lsm db) 0)
        (guard (ex (else (lsm1-rollback (okvs-lsm db) 0) (raise ex)))
          (call-with-values (lambda () (proc db))
            (lambda args
              (lsm1-commit (okvs-lsm db) 0)
              (apply values args))))))

    (define okvs-set!
      (lambda (db key value)
        (lsm1-insert (okvs-lsm db) key value)))

    (define okvs-query-value
      (lambda (db key)
        (define cursor (lsm1-cursor-open (okvs-lsm db)))
        (lsm1-cursor-seek cursor key 'equal)
        (if (not (lsm1-cursor-valid? cursor))
            (begin
              (lsm1-cursor-close cursor)
              #f)
            (let ((value (lsm1-cursor-value cursor)))
              (lsm1-cursor-close cursor)
              value))))

    (define okvs-query-range-ascending
      (lambda (db key other)
        (define cursor (lsm1-cursor-open (okvs-lsm db)))

        (define continue
          (lambda ()
            (if (not (lsm1-cursor-valid? cursor))
                (begin
                  (set! continue eof-object)
                  (lsm1-cursor-close cursor)
                  (eof-object))
                (let ((key (lsm1-cursor-key cursor)))
                  (case (byter-compare key other)
                    ((equal bigger)
                     (set! continue eof-object)
                     (lsm1-cursor-close cursor)
                     (eof-object))
                    (else (let ((value (lsm1-cursor-value cursor)))
                            (lsm1-cursor-next cursor)
                            (cons key value))))))))

        (lsm1-cursor-seek cursor key 'greater-than-or-equal)

        (lambda ()
          (continue))))

    (define okvs-query-range-descending
      (lambda (db key other)
        (define cursor (lsm1-cursor-open (okvs-lsm db)))

        (define continue
          (lambda ()
            (if (not (lsm1-cursor-valid? cursor))
                (begin
                  (set! continue eof-object)
                  (lsm1-cursor-close cursor)
                  (eof-object))
                (let ((key (lsm1-cursor-key cursor)))
                  (case (byter-compare key other)
                    ((equal smaller)
                     (set! continue eof-object)
                     (lsm1-cursor-close cursor)
                     (eof-object))
                    (else (let ((value (lsm1-cursor-value cursor)))
                            (lsm1-cursor-prev cursor)
                            (cons key value))))))))

        (lsm1-cursor-seek cursor key 'less-than-or-equal)

        (lambda ()
          (continue))))

    (define okvs-query-range
      (lambda (db key other)
        (case (byter-compare key other)
          ((equal) (error 'db "KEY, and OTHER bounds are equal, invalid okvs-query"))
          ((smaller) (okvs-query-range-ascending db key other))
          ((bigger) (okvs-query-range-descending db key other)))))

    (define okvs-query
      (case-lambda
       ((db key) (okvs-query-value db key))
       ((db key other) (okvs-query-range db key other))))

    (define okvs-clear-key!
      (lambda (db key)
        (lsm1-delete (okvs-lsm db) key)))

    (define okvs-clear-range!
      (lambda (db key other)

        (define generator-for-each
          (lambda (proc g)
            (define o (g))
            (unless (eof-object? o)
              (proc o)
              (generator-for-each proc g))))

        (generator-for-each (lambda (key+value) (okvs-clear! db (car key+value))) (okvs-query db key other))))

    (define okvs-clear!
      (case-lambda
       ((db key) (okvs-clear-key! db key))
       ((db key other) (okvs-clear-range! db key other))))

    (define call-with-temporary-directory
      (lambda (proc)

        (define stdlib (load-shared-object #f))

        (define mkdtemp
          (foreign-procedure "mkdtemp" (string) string))

        (define (make-temporary-directory prefix)
          (let ((input (string-append prefix "-XXXXXX")))
            (mkdtemp input)))

        (define tmp (make-temporary-directory "letloop-lsm-okvs"))

        (call-with-values (lambda () (proc tmp))

          (lambda args
            ;; TODO: delete directory
            (apply values args)))))

    (define call-with-database
      (lambda (proc)
        (call-with-temporary-directory
         (lambda (directory)
           (define db (make-okvs (string-append directory "/letloop.db")))
           (let ((out (proc db)))
             (okvs-close db)
             out)))))

    (define call-with-database*
      (lambda (directory proc)
        (define db (make-okvs (string-append directory "/letloop.db")))
        (let ((out (proc db)))
          (okvs-close db)
          out)))

    (define pk
      (lambda args
        (when (getenv "LETLOOP_DEBUG_DB")
          (display ";; ")
          (write args)
          (newline)
          (flush-output-port))
        (car (reverse args))))

    (define ~check-okvs-000
      (lambda ()
        (call-with-database
         (lambda (db)
           #t))))

    (define ~check-okvs-001
      (lambda ()
        (call-with-database
         (lambda (db)
           (call-with-okvs-transaction db
             (lambda (db)
               (not (okvs-query db (bytevector 42)))))))))

    (define ~check-okvs-002
      (lambda ()
        (call-with-database
         (lambda (db)
           (call-with-okvs-transaction db
             (lambda (db)
               (okvs-set! db (bytevector 42) (bytevector 13 37))
               (equal? (bytevector 13 37) (okvs-query db (bytevector 42)))))))))

    (define generator->list
      (lambda (g)
        (let loop ((out '()))
          (let ((o (g)))
            (if (eof-object? o)
                (reverse out)
                (loop (cons o out)))))))

    (define ~check-okvs-003
      (lambda ()
        (call-with-database
         (lambda (db)
           (call-with-okvs-transaction db
             (lambda (db)
               (equal? (list) (generator->list (okvs-query db (bytevector 13) (bytevector 37))))))))))

    (define ~check-okvs-004
      (lambda ()
        (call-with-database
         (lambda (db)
           (okvs-set! db (bytevector 33) (bytevector 33))
           (call-with-okvs-transaction db
             (lambda (db)
               (equal? (list (cons (bytevector 33)
                                   (bytevector 33)))
                       (generator->list (okvs-query db (bytevector 13) (bytevector 37))))))))))

    (define ~check-okvs-005
      (lambda ()
        (call-with-database
         (lambda (db)
           (okvs-set! db (bytevector 0) (bytevector 0))
           (okvs-set! db (bytevector 33) (bytevector 33))
           (okvs-set! db (bytevector 13) (bytevector 13))
           (okvs-set! db (bytevector 37) (bytevector 37))
           (okvs-set! db (bytevector 255) (bytevector 255))
           (call-with-okvs-transaction db
             (lambda (db)
               (equal? (list (cons (bytevector 13) (bytevector 13))
                             (cons (bytevector 33) (bytevector 33)))
                       (generator->list (okvs-query db (bytevector 13) (bytevector 37))))))))))

    (define ~check-okvs-006
      (lambda ()
        (call-with-database
         (lambda (db)
           (okvs-set! db (bytevector 0) (bytevector 0))
           (okvs-set! db (bytevector 33) (bytevector 33))
           (okvs-set! db (bytevector 13) (bytevector 13))
           (okvs-set! db (bytevector 37) (bytevector 37))
           (okvs-set! db (bytevector 255) (bytevector 255))
           (call-with-okvs-transaction db
             (lambda (db)
               (equal? (list (cons (bytevector 37) (bytevector 37))
                             (cons (bytevector 33) (bytevector 33)))
                       (generator->list (okvs-query db (bytevector 37) (bytevector 13))))))))))

    (define ~check-okvs-007
      (lambda ()
        (call-with-database
         (lambda (db)
           (okvs-set! db (bytevector 0) (bytevector 0))
           (okvs-set! db (bytevector 33) (bytevector 33))
           (okvs-set! db (bytevector 13) (bytevector 13))
           (okvs-set! db (bytevector 37) (bytevector 37))
           (okvs-set! db (bytevector 255) (bytevector 255))
           (okvs-clear! db (bytevector 33))
           (call-with-okvs-transaction db
             (lambda (db)
               (equal? (list (cons (bytevector 37) (bytevector 37)))
                       (generator->list (okvs-query db (bytevector 37) (bytevector 13))))))))))

    (define ~check-okvs-008
      (lambda ()
        (call-with-database
         (lambda (db)
           (okvs-set! db (bytevector 0) (bytevector 0))
           (okvs-set! db (bytevector 33) (bytevector 33))
           (okvs-set! db (bytevector 13) (bytevector 13))
           (okvs-set! db (bytevector 37) (bytevector 37))
           (okvs-set! db (bytevector 255) (bytevector 255))
           (call-with-okvs-transaction db
             (lambda (db)
               (okvs-clear! db (bytevector 0) (bytevector 255 255))))
           (call-with-okvs-transaction db
             (lambda (db)
               (equal? (list)
                       (generator->list (okvs-query db (bytevector 0) (bytevector 255 255))))))))))

    (define ~check-okvs-009
      (lambda ()
        (call-with-temporary-directory
         (lambda (directory)
           (call-with-database* directory
                                (lambda (db)
                                  (okvs-set! db (bytevector 33) (bytevector 33))
                                  (call-with-okvs-transaction db
                                    (lambda (db)
                                      (equal? (list (cons (bytevector 33)
                                                          (bytevector 33)))
                                              (generator->list (okvs-query db (bytevector 13) (bytevector 37))))))))
           (call-with-database* directory
                                (lambda (db)
                                  (call-with-okvs-transaction db
                                    (lambda (db)
                                      (equal? (list (cons (bytevector 33)
                                                          (bytevector 33)))
                                              (generator->list (okvs-query db (bytevector 13) (bytevector 37))))))))))))

    ))
