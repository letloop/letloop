#!chezscheme
(library (letloop foundationdb base)

  (export
   fdb-error
   fdb-error-ref
   fdb-error-predicate
   fdb-select-api-version
   fdb-setup-network!
   fdb-run-network
   fdb-stop-network
   fdb-future-cancel!
   fdb-future-release-memory
   fdb-future-destroy
   fdb-future-block-until-ready
   fdb-future-ready?
   fdb-future-set-callback!
   fdb-future-get-error
   fdb-future-get-int64
   fdb-future-get-key
   fdb-future-get-value
   fdb-future-get-range-page
   fdb-create-database
   fdb-database-destroy
   fdb-database-create-transaction
   fdb-transaction-destroy
   fdb-transaction-cancel!
   fdb-transaction-get
   fdb-transaction-get-range-page
   fdb-transaction-set!
   fdb-transaction-atomic-op!
   fdb-transaction-clear!
   fdb-transaction-clear-range!
   fdb-transaction-commit!
   fdb-transaction-on-error
   fdb-transaction-reset!
   fdb-transaction-get-estimated-range-size-bytes
   )

  (import (chezscheme) (letloop foundationdb helpers))

  (begin

    (define-syntax define-syntax-rule
      (syntax-rules ()
        ((define-syntax-rule (keyword args ...) body)
         (define-syntax keyword
           (syntax-rules ()
             ((keyword args ...) body))))))

    (define-syntax-rule (with-lock objects e ...)
      (begin
        (for-each lock-object objects)
        (call-with-values (lambda () e ...)
          (lambda args
            (for-each unlock-object objects)
            (apply values args)))))

    (define (bytevector-pointer bv)
      (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

    ;; ffi helpers

    (define (make-double-pointer)
      ;; Instead of foreign-alloc, let's use a bytevector to create
      ;; the double pointer from a Scheme object so that it is handled
      ;; by Chez garbage collector; That rely on #%$ private procedure
      ;; tho; also it is more dangerous, it might lead to memory
      ;; error; copy the pointed to data as soon as possible; or the
      ;; garbage collector will reap it. Dangerous life.
      (bytevector-pointer (make-bytevector 8)))

    (define-syntax-rule (pointer-dereference pointer)
      (foreign-ref 'void* pointer 0))

    ;; the following sugar is pointless.

    (define-syntax-rule (foreign-procedure* return ptr args ...)
      (foreign-procedure ptr (args ...) return))

    ;; foundationdb bindings

    (define foundationdb (load-shared-object "/nix/store/rlnbpd2m5dm09y66gcsgd8cwml3pdfdh-foundationdb-7.1.30-lib/lib/libfdb_c.so"))

    ;;
    ;; foundationdb enums
    ;;

    ;; TODO: add foundationdb enums

    ;;
    ;; foundationdb bindings
    ;;

    (define fdb-error-ref
      (let ((func (foreign-procedure* string "fdb_get_error" int)))
        (lambda (code)
          (func code))))

    (define-syntax-rule (check code)
      (let ((code* code))
        (unless (fxzero? code*)
          ;; TODO: wrap with a Chez condition to ease debugging.
          (raise (list 'foundationdb code* (fdb-error-ref code*))))))

    (define (fdb-error code)
      (raise (list 'foundationdb (fdb-error-ref code) code)))

    (define fdb-error-predicate
      (let ((func (foreign-procedure* int "fdb_error_predicate" int int)))
        (lambda (predicate-test code)
          (fxzero? (func predicate-test code)))))


    (define fdb-select-api-version
      (let ((func (foreign-procedure* int "fdb_select_api_version_impl" int int)))
        (lambda (version)
          (check (func version 710)))))

    ;; TODO: implement network options, requires enums
    ;;
    ;; (define fdb-network-set-option
    ;;   (let ((func (fdb error "fdb_network_set_option" enum POINTER ffi:int)))
    ;;     (lambda (option value length)
    ;;       (check (func option value length)))))

    (define fdb-setup-network!
      (let ((func (foreign-procedure* int "fdb_setup_network")))
        (lambda ()
          (check (func)))))

    (define fdb-run-network
      ;; XXX: Here __collect_safe is required, otherwise the network
      ;; thread will lock the other threads during garbage collection.
      ;; Maybe, disabling GC for the network thread is better?
      (let ((func (foreign-procedure __collect_safe "fdb_run_network" () int)))
        (lambda ()
          (func))))

    (define fdb-stop-network
      (let ((func (foreign-procedure* int "fdb_stop_network")))
        (lambda ()
          (check (func)))))

    ;; TODO:
    ;;
    ;; (define fdb-add-network-thread-completion-hook
    ;;   (let ((func (foreign-procedure* int "fdb_add_network_thread_completion_hook" void*)))
    ;;     (lambda (thunk)
    ;;       (check (func (ffi:procedure->pointer ffi:void thunk (list ffi:void)))))))

    (define-ftype %keyvalue
      (packed (struct
               (key void*)
               (key-length int)
               (value void*)
               (value-length int))))

    (define fdb-future-cancel!
      (let ((func (foreign-procedure* void "fdb_future_cancel" void*)))
        (lambda (future)
          (func future))))

    (define fdb-future-release-memory
      (let ((func (foreign-procedure* void "fdb_future_release_memory" void*)))
        (lambda (future)
          (func future))))

    (define fdb-future-destroy
      (let ((func (foreign-procedure* void "fdb_future_destroy" void*)))
        (lambda (future)
          (func future))))

    (define fdb-future-block-until-ready
      (let ((func (foreign-procedure* int "fdb_future_block_until_ready" void*)))
        (lambda (future)
          (check (func future)))))

    (define fdb-future-ready?
      (let ((func (foreign-procedure* int "fdb_future_is_ready" void*)))
        (lambda (future)
          (fx=? (func future) 1))))

    (define (fdb-future-callback proc)
      ;; XXX: This mostly a helper.  Only pass the the FUTURE to the
      ;; callback; the DATA is ignored because it can be done with
      ;; Scheme using a closure.
      (let ((code (foreign-callable (lambda (future data) (proc future)) (void* void*) void)))
        ;; It is required to lock CODE to avoid that the garbage
        ;; collector does its job if / when code disappears from
        ;; Scheme code, hence leading to a memory error later when C
        ;; side try to call the callback. CODE must be unlocked when
        ;; it is not anymore useful.
        (lock-object code)
        (values code (foreign-callable-entry-point code))))

    (define fdb-future-set-callback!
      (let ((fdb-future-set-callback! (foreign-procedure* int "fdb_future_set_callback" void* void* void*)))
        (lambda (future code)
          (check (fdb-future-set-callback! future code 0)))))

    (define fdb-future-get-error
      (let ((func (foreign-procedure* int "fdb_future_get_error" void*)))
        (lambda (future)
          ;; Even if fdb_future_get_error returns an fdb_error_t, do
          ;; not call check, hence avoid to call raise.  It is the
          ;; responsability of a higher level to respond correctly to
          ;; the error.
          (func future))))

    (define fdb-future-get-int64
      (let ((func (foreign-procedure* int "fdb_future_get_int64" void* void*)))
        (lambda (future)
          (let ((pointer (foreign-alloc 8)))
            (with-lock (list pointer)
              (func future pointer)
              (foreign-ref 'integer-64 pointer 0))))))

    (define (pointer->bytevector pointer length)
      ;; Copy a memory region starting at POINTER of LENGTH into a
      ;; bytevector.
      (let ((out (make-bytevector length)))
        (let loop ((index length))
          (unless (fxzero? index)
            (let ((index (fx- index 1)))
              (bytevector-u8-set! out index (foreign-ref 'unsigned-8 pointer index))
              (loop index))))
        out))

    (define fdb-future-get-key
      ;; TODO: FIXME like fdb-future-get-value
      (let ((func (foreign-procedure* int "fdb_future_get_key" void* void* void*)))
        (lambda (future)
          (let ((key (foreign-alloc 8))
                (length (foreign-alloc 8)))
            (check (func future key length))
            (pointer->bytevector key (foreign-ref 'int length 0))))))

    (define fdb-future-get-value
      (let ((func (foreign-procedure* int "fdb_future_get_value" void* void* void* void*)))
        (lambda (future)
          (let ((present? (foreign-alloc 8))
                (value (foreign-alloc 8))
                (length (foreign-alloc 8)))
            (check (func future present? value length))
            (let ((present? (foreign-ref 'int present? 0))
                  (value (foreign-ref 'void* value 0))
                  (length (foreign-ref 'int length 0)))
              (if (fxzero? present?)
                  #f
                  (pointer->bytevector value length)))))))

    (define fdb-future-get-range-page
      ;; Note the return value more? flag.
      (let ((func (foreign-procedure* void* "fdb_future_get_keyvalue_array" void* void* void* void*)))
        (lambda (future)

          (define (%keyvalue->bytevectors pointer)
            (let* ((kv (make-ftype-pointer %keyvalue pointer))
                   (key-length (ftype-ref %keyvalue (key-length) kv))
                   (value-length (ftype-ref %keyvalue (value-length) kv)))
              (cons (pointer->bytevector (ftype-ref %keyvalue (key) kv) key-length)
                    (pointer->bytevector (ftype-ref %keyvalue (value) kv) value-length))))

          (let ((out (foreign-alloc 8))
                (count (foreign-alloc 8))
                (more? (foreign-alloc 8)))
            (check (func future out count more?))
            ;; Dereference double pointers as soon as possible, since
            ;; they are backed by a bytevector, and GC might reap
            ;; them...
            (let ((out (pointer-dereference out))
                  (count (foreign-ref 'int count 0))
                  (more? (fx=? (foreign-ref 'int more? 0) 1)))
              ;; Iterate in reverse order to avoid a call to the
              ;; procedure reverse.
              (let loopx ((index count)
                          (out* '()))
                (if (fxzero? index)
                    (values out* more?)
                    (let ((index (fx- index 1)))
                      (let ((item (%keyvalue->bytevectors (+ out
                                                             (fx* (ftype-sizeof %keyvalue)
                                                                  index)))))
                        (loopx index (cons item out*)))))))))))

    (define fdb-create-database
      (let ((func (foreign-procedure "fdb_create_database" (string void*) int)))
        (lambda (maybe-cluster-file)
          (let ((out (foreign-alloc 8)))
            (check (func maybe-cluster-file out))
            (pointer-dereference out)))))

    (define fdb-database-destroy
      (let ((func (foreign-procedure* void "fdb_database_destroy" void*)))
        (lambda (database)
          (func database))))

    ;; TODO: fdb_database_set_option

    (define fdb-database-create-transaction
      (let ((func (foreign-procedure* int "fdb_database_create_transaction" void* void*)))
        (lambda (database)
          (let ((out (foreign-alloc 8)))
            (with-lock (list out)
              (check (func database out))
              (pointer-dereference out))))))

    (define fdb-transaction-destroy
      (let ((func (foreign-procedure* void "fdb_transaction_destroy" void*)))
        (lambda (transaction)
          (func transaction))))

    (define fdb-transaction-cancel!
      (let ((func (foreign-procedure* void "fdb_transaction_cancel" void*)))
        (lambda (transaction)
          (func transaction))))

    ;; TODO: fdb_transaction_set_option

    ;; TODO: fdb_tranaction_set_read_version

    ;; TODO: fdb_tranaction_get_read_version

    (define fdb-transaction-get
      (let ((func (foreign-procedure* void* "fdb_transaction_get" void* void* int int)))
        (lambda (transaction key snapshot?)
          (func transaction
                (bytevector-pointer key)
                (bytevector-length key)
                (if snapshot? 1 0)))))

    ;; TODO: fdb_transaction_get_key

    (define fdb-transaction-get-range-page
      ;; https://apple.github.io/foundationdb/api-c.html#c.fdb_transaction_get_range
      (let ((func (foreign-procedure* void*
                                      "fdb_transaction_get_range"
                                      void* ;; tr
                                      void* ;; begin key name
                                      int ;; begin's key name length
                                      int ;; begin's or equal
                                      int ;; begin's offset
                                      void* ;; end key name
                                      int ;; end's key name length
                                      int ;; end's or equal
                                      int ;; end's offset
                                      int ;; limit
                                      int ;; target bytes
                                      int ;; mode
                                      int ;; iteration
                                      int ;; snapshot?
                                      int ;; reverse?
                                      )))
        (lambda (transaction
                 begin-key
                 begin-or-equal?
                 begin-offset
                 end-key
                 end-or-equal?
                 end-offset
                 limit
                 target-bytes
                 mode
                 iteration
                 snapshot?
                 reverse?)
          (with-lock (list begin-key end-key)
            (func transaction
                  (bytevector-pointer begin-key)
                  (bytevector-length begin-key)
                  (if begin-or-equal? 1 0)
                  begin-offset
                  (bytevector-pointer end-key)
                  (bytevector-length end-key)
                  (if end-or-equal? 1 0)
                  end-offset
                  0
                  target-bytes
                  mode
                  iteration
                  (if snapshot? 1 0)
                  (if reverse? 1 0))))))

    (define fdb-transaction-set!
      (let ((func (foreign-procedure* void
                                      "fdb_transaction_set"
                                      void* ;; tr
                                      void* ;; key-name
                                      int ;; key-name length
                                      void* ;; value
                                      int ;; value length
                                      )))
        (lambda (transaction key value)
          (assert (fx<=? (bytevector-length key) (expt 10 4)))
          (assert (fx<=? (bytevector-length value) (expt 10 5)))
          (func transaction
                (bytevector-pointer key)
                (bytevector-length key)
                (bytevector-pointer value)
                (bytevector-length value)))))

    (define fdb-transaction-atomic-op!
      (let ((func (foreign-procedure* void
                                      "fdb_transaction_atomic_op"
                                      void* ;; tr
                                      void* ;; key-name
                                      int ;; key-name length
                                      void* ;; param
                                      int ;; param length
                                      int ;; operation type
                                      )))
        (lambda (transaction key param operation-type)
              (func transaction
                    (bytevector-pointer key)
                    (bytevector-length key)
                    (bytevector-pointer param)
                    (bytevector-length param)
                    operation-type))))

    (define fdb-transaction-clear!
      (let ((func (foreign-procedure* void
                                      "fdb_transaction_clear"
                                      void* ;; tr
                                      void* ;; key-name
                                      int ;; key-name length
                                      )))
        (lambda (transaction key)
            (func transaction
                  (bytevector-pointer key)
                  (bytevector-length key)))))

    (define fdb-transaction-clear-range!
      (let ((func (foreign-procedure* void
                                      "fdb_transaction_clear_range"
                                      void*
                                      void*
                                      int
                                      void*
                                      int)))
        (lambda (transaction begin end)
          (func transaction
                (bytevector-pointer begin)
                (bytevector-length begin)
                (bytevector-pointer end)
                (bytevector-length end)))))

    ;; TODO: fdb_transaction_watch

    (define fdb-transaction-commit!
      (let ((func (foreign-procedure* void*
                                      "fdb_transaction_commit"
                                      void*)))
        (lambda (transaction)
          (func transaction))))

    ;; TODO: fdb_transaction_get_commited_version

    ;; TODO: fdb_transaction_get_approximate_size

    (define fdb-transaction-on-error
      (let ((func (foreign-procedure* void*
                                      "fdb_transaction_on_error"
                                      void*
                                      int)))
        (lambda (transaction code)
          (func transaction code))))

    (define fdb-transaction-reset!
      (let ((func (foreign-procedure* void "fdb_transaction_reset" void*)))
        (lambda (transaction)
          (func transaction))))

    (define fdb-transaction-get-estimated-range-size-bytes
      (let ((func
             (foreign-procedure* void* "fdb_transaction_get_estimated_range_size_bytes"
                                 void* void* int void* int)))
        (lambda (transaction key value)
          (with-lock (list key value)
            (func transaction
                  (bytevector-pointer key) (bytevector-length key)
                  (bytevector-pointer value) (bytevector-length value))))))

    ;; TODO: fdb_transaction_add_conflict_range

    ))
