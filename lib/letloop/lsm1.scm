#!chezscheme
(library (letloop lsm1)

  (export
   lsm1-begin
   lsm1-close
   lsm1-commit
   lsm1-config
   lsm1-cursor-close
   lsm1-cursor-first
   lsm1-cursor-key
   lsm1-cursor-last
   lsm1-cursor-next
   lsm1-cursor-open
   lsm1-cursor-prev
   lsm1-cursor-seek
   lsm1-cursor-valid?
   lsm1-cursor-value
   lsm1-delete
   lsm1-insert
   lsm1-new
   lsm1-open
   lsm1-rollback

   ~check-lsm1-000
   ~check-lsm1-001

   )

  (import (chezscheme))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           ((keyword args ...) body))))))

  (define (bytevector->pointer bv)
    (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

  ;; TODO: take a list of objects.
  (define (call-with-lock obj thunk)
    (lock-object obj)
    (call-with-values thunk
      (lambda out
        (unlock-object obj)
        (apply values out))))

    ;; ffi helpers

    (define (make-double-pointer)
      (foreign-alloc 8))

    (define-syntax-rule (dereference  pointer)
      (foreign-ref 'void* pointer 0))

    (define-syntax-rule (ftype->pointer ftype)
      (ftype-pointer-address ftype))

    (define-syntax-rule (foreign-procedure* return ptr args ...)
      (foreign-procedure ptr (args ...) return))

    ;; sqlite lsm extension bindings

    (define lsm.so (load-shared-object "lsm.so"))

    (define (error->message code)
      (case code
        ((1) "ERROR")
        ((5) "BUSY")
        ((7) "NO MEMORY")
        ((8) "READ ONLY")
        ((10) "IO ERROR")
        ((11) "CORRUPT")
        ((13) "FULL")
        ((14) "CAN NOT OPEN")
        ((15) "PROTOCOL")
        ((21) "MISUSE")
        ((50) "MISMATCH")
        (else "UNKNOWN ERROR")))

    (define-syntax-rule (check sym code)
      (let ((code* code))
        (unless (zero? code*)
          (error 'lsm (error->message code*) code* sym))))

    (define lsm1-new
      (let ((proc (foreign-procedure* int "lsm_new" void* void*)))
        (lambda ()
          (let ((out (make-double-pointer)))
            (check 'lsm1-new (proc 0 out))
            (dereference out)))))

    (define lsm1-close
      (let ((proc (foreign-procedure* int "lsm_close" void*)))
        (lambda (db)
          (check 'lsm1-close (proc db)))))

    ;; TODO: replace with symbols
    (define lsm1-config
      (let ((proc (foreign-procedure* int "lsm_config" void* int void*)))
        (lambda (db config value)
          (let ((pointer (make-double-pointer)))
            (foreign-set! 'int pointer 0 value)
            (check 'lsm1-config (proc db config pointer))))))

    (define lsm1-open
      (let ((proc (foreign-procedure "lsm_open" (void* string) int)))
        (lambda (db filename)
          (check 'lsm1-open (proc db filename)))))

    (define lsm1-begin
      (let ((proc (foreign-procedure* int "lsm_begin" void* int)))
        (lambda (db level)
          (check 'lsm1-begin (proc db level)))))

    (define lsm1-commit
      (let ((proc (foreign-procedure* int "lsm_commit" void* int)))
        (lambda (db level)
          (check 'lsm1-commit (proc db level)))))

    (define lsm1-rollback
      (let ((proc (foreign-procedure* int "lsm_rollback" void* int)))
        (lambda (db level)
          (check 'lsm1-rollback (proc db level)))))

    (define lsm1-insert
      (let ((proc (foreign-procedure* int "lsm_insert" void* void* int void* int)))
        (lambda (db key value)
          (call-with-lock key
            (lambda ()
              (call-with-lock value
                (lambda ()
                  (check 'lsm1-insert
                         (proc db
                               (bytevector->pointer key)
                               (bytevector-length key)
                               (bytevector->pointer value)
                               (bytevector-length value))))))))))

    (define lsm1-delete
      (let ((proc (foreign-procedure* int "lsm_delete" void* void* int)))
        (lambda (db key)
          (call-with-lock key
            (lambda ()
              (check 'lsm1-delete
                     (proc db
                           (bytevector->pointer key)
                           (bytevector-length key))))))))

    (define lsm1-cursor-open
      (let ((proc (foreign-procedure* int "lsm_csr_open" void* void*)))
        (lambda (db)
          (let ((out (make-double-pointer)))
            (check 'lsm1-cursor-open
                   (proc db out))
            (dereference out)))))

    (define lsm1-cursor-close
      (let ((proc (foreign-procedure* int "lsm_csr_close" void*)))
        (lambda (cursor)
          (check 'lsm1-cursor-close (proc cursor)))))

    (define (->seek symbol)
      (case symbol
        ((less-than-or-equal-fast) -2)
        ((less-than-or-equal) -1)
        ((equal) 0)
        ((greater-than-or-equal) 1)
        (else (error 'lsm1 "unknown seek strategy"))))

    (define lsm1-cursor-seek
      (let ((proc (foreign-procedure* int "lsm_csr_seek" void* void* int int)))
        (lambda (cursor key strategy)
          (call-with-lock key
            (lambda ()
              (check 'lsm1-cursor-seek
                     (proc cursor
                           (bytevector->pointer key)
                           (bytevector-length key)
                           (->seek strategy))))))))

    (define lsm1-cursor-first
      (let ((proc (foreign-procedure* int "lsm_csr_first" void*)))
        (lambda (cursor)
          (check 'lsm1-cursor-first (proc cursor)))))

    (define lsm1-cursor-last
      (let ((proc (foreign-procedure* int "lsm_csr_last" void*)))
        (lambda (cursor)
          (check 'lsm1-cursor-last (proc cursor)))))

    (define lsm1-cursor-next
      (let ((proc (foreign-procedure* int "lsm_csr_next" void*)))
        (lambda (cursor)
          (check 'lsm1-cursor-next (proc cursor)))))

    (define lsm1-cursor-prev
      (let ((proc (foreign-procedure* int "lsm_csr_prev" void*)))
        (lambda (cursor)
          (check 'lsm1-cursor-prev (proc cursor)))))

    (define lsm1-cursor-valid?
      (let ((proc (foreign-procedure* int "lsm_csr_valid" void*)))
        (lambda (cursor)
          (= (proc cursor) 1))))

    (define lsm1-cursor-key
      (let ((proc (foreign-procedure* int "lsm_csr_key" void* void* void*)))
        (lambda (cursor)
          (let ((data* (make-double-pointer))
                (length* (make-double-pointer)))
            (check 'lsm1-cursor-key (proc cursor data* length*))
            ;; copy the data into a scheme bytevector
            (let* ((data (dereference data*))
                   (length (foreign-ref 'int length* 0))
                   (bytevector (make-bytevector length)))
              (let loop ((index (- length 1)))
                (unless (< index 0)
                  (let ((value (foreign-ref 'unsigned-8 data index)))
                    (bytevector-u8-set! bytevector index value)
                    (loop (- index 1)))))
              bytevector)))))

    (define lsm1-cursor-value
      (let ((proc (foreign-procedure* int "lsm_csr_value" void* void* void*)))
        (lambda (cursor)
          (let ((data* (make-double-pointer))
                (length* (make-double-pointer)))
            (check 'lsm1-cursor-value (proc cursor data* length*))
            ;; copy the data into a scheme bytevector
            (let* ((data (dereference data*))
                   (length (foreign-ref 'int length* 0))
                   (bytevector (make-bytevector length)))
              (let loop ((index (- length 1)))
                (unless (< index 0)
                  (let ((value (foreign-ref 'unsigned-8 data index)))
                    (bytevector-u8-set! bytevector index value)
                    (loop (- index 1)))))
              bytevector)))))

    (define call-with-temporary-file
      (lambda (proc)

        (define stdlib (load-shared-object #f))

        (define mkstemp
          (foreign-procedure "mkstemp" (string) int))

        (define close
          (foreign-procedure "close" (int) int))

        (define (make-temporary-file prefix)
          (let ((out (string-append prefix "-XXXXXX")))
            (close (mkstemp out))
            out))

        (define tmp (make-temporary-file "letloop-lsm1-"))

        (call-with-values (lambda () (proc tmp))
          (lambda args
            (delete-file tmp)
            (apply values args)))))

    (define pk
      (lambda args
        (display ";; ")
        (write args)
        (newline)
        (car (reverse args))))

    (define ~check-lsm1-000
      (lambda ()
        (call-with-temporary-file
         (lambda (file)
           (define lsm (lsm1-new))
           (lsm1-open lsm file)
           (lsm1-close lsm)
           #t))))

    (define ~check-lsm1-001
      (lambda ()
        (call-with-temporary-file
         (lambda (file)
           (define lsm (lsm1-new))
           (define ignore0 (lsm1-open lsm file))
           (define ignore1
             (let ((cursor (lsm1-cursor-open lsm)))
               (lsm1-cursor-seek cursor (bytevector 42) 'less-than-or-equal)
               (lsm1-cursor-close cursor)))
           (lsm1-close lsm)
           #t))))

    )
