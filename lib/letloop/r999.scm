(library (letloop r999)

  (export define-record-type*
          ~check-define-record-type*-000
          ~check-define-record-type*-001)

  (import (chezscheme))

  (define-syntax define-record-type*
    (lambda (stx)
      (syntax-case stx ()
        ((_ <type>
            uid
            (constructor constructor-tag ...)
            predicate?
            (field-tag accessor setter ...) ...)

         (and (for-all identifier?
                       #'(<type> constructor constructor-tag ... predicate?
                                 field-tag ... accessor ... setter ... ...))
              (for-all (lambda (s) (<= 0 (length s) 1))
                       #'((setter ...) ...))
              (for-all (lambda (ct)
                         (memp (lambda (ft) (bound-identifier=? ct ft))
                               #'(field-tag ...)))
                       #'(constructor-tag ...)))
         (with-syntax (((field-clause ...)
                        (map (lambda (clause)
                               (if (= 2 (length clause))
                                   #`(immutable . #,clause)
                                   #`(mutable . #,clause)))
                             #'((field-tag accessor setter ...) ...)))
                       ((unspec-tag ...)
                        (remp (lambda (ft)
                                (memp (lambda (ct) (bound-identifier=? ft ct))
                                      #'(constructor-tag ...)))
                              #'(field-tag ...))))
                      #'(define-record-type (<type> constructor predicate?)
                          (nongenerative uid)
                          (protocol (lambda (ctor)
                                      (lambda (constructor-tag ...)
                                        (define unspec-tag) ...
                                        (ctor field-tag ...))))
                          (fields field-clause ...))))

        ((_ <type> (constructor constructor-tag ...)
            predicate?
            (field-tag accessor setter ...) ...)

         (and (for-all identifier?
                       #'(<type> constructor constructor-tag ... predicate?
                                 field-tag ... accessor ... setter ... ...))
              (for-all (lambda (s) (<= 0 (length s) 1))
                       #'((setter ...) ...))
              (for-all (lambda (ct)
                         (memp (lambda (ft) (bound-identifier=? ct ft))
                               #'(field-tag ...)))
                       #'(constructor-tag ...)))
         (with-syntax (((field-clause ...)
                        (map (lambda (clause)
                               (if (= 2 (length clause))
                                   #`(immutable . #,clause)
                                   #`(mutable . #,clause)))
                             #'((field-tag accessor setter ...) ...)))
                       ((unspec-tag ...)
                        (remp (lambda (ft)
                                (memp (lambda (ct) (bound-identifier=? ft ct))
                                      #'(constructor-tag ...)))
                              #'(field-tag ...))))
                      #'(define-record-type (<type> constructor predicate?)
                          ;; XXX: The following expression sets the
                          ;; record type unique identifier, hence it
                          ;; is preferable that all record types have
                          ;; a different <type>. It is a good idea I
                          ;; can't forsee cases where <type> must be
                          ;; <foobar> instead of <letloop-foobar>,
                          ;; except that record instances are
                          ;; representation in the REPL are slightly
                          ;; longer, and given the uid is not random
                          ;; it is also more readable.
                          (nongenerative <type>)
                          (protocol (lambda (ctor)
                                      (lambda (constructor-tag ...)
                                        (define unspec-tag) ...
                                        (ctor field-tag ...))))
                          (fields field-clause ...)))))))

  (define ~check-define-record-type*-000
    (lambda ()
      (define-record-type* <mytest>
        (make-mytest value)
        mytest?
        (value mytest-value mytest-value!))

      (define mytest (make-mytest 42))

      (assert (mytest? mytest))
      (assert (= (mytest-value mytest) 42))

        (mytest-value! mytest 101)

      (assert (= (mytest-value mytest) 101))))

  (define ~check-define-record-type*-001
    (lambda ()
      ;; given
      (define-record-type* <mytest>
        (make-mytest value)
        mytest?
        (value mytest-value mytest-value!))

      (define mytest (make-mytest 42))

      ;; when the instance is serialized, and read
      (define bytevector (call-with-bytevector-output-port
                          (lambda (port)
                            (fasl-write mytest port))))

      (define mytest* (let ((port (open-bytevector-input-port bytevector)))
                        (fasl-read port)))

      ;; it is the same object
      (assert (= (mytest-value mytest) (mytest-value mytest*))))))
