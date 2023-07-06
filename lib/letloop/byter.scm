(library (letloop byter)

  (export byter-write
          byter-read
          byter-compare
          ~check-byter-000
          ~check-byter-001
          ~check-byter-002
          ~check-byter-003
          ~check-byter-004
          ~check-byter-005
          ~check-byter-006/random
          ~check-byter-007/random
          ~check-byter-008
          ~check-byter-009
          ~check-byter-010
          ~check-byter-011
          ~check-byter-012
          ~check-byter-100
          ~check-byter-101
          ~check-byter-102
          ~check-byter-998/seed
          ~check-byter-998/random
          ~check-byter-999/seed
          ~check-byter-999/random)

  (import (chezscheme))

  (define pk
    (lambda args
      (display ";; ")
      (write args)
      (newline)
      (flush-output-port)
      (car (reverse args))))

  ;; TODO: move to (letloop bytevector)

  (define (byter-next-prefix bytevector)
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
      (u8-list->bytevector (reverse (cons (fx+ 1 (car bytes)) (cdr bytes))))))

  (define (byter-append . bvs)
    (let* ((total (apply fx+ (map bytevector-length bvs)))
           (out (make-bytevector total)))
      (let loop ((bvs bvs)
                 (index 0))
        (unless (null? bvs)
          (bytevector-copy! (car bvs) 0 out index (bytevector-length (car bvs)))
          (loop (cdr bvs) (fx+ index (bytevector-length (car bvs))))))
      out))

  ;; TODO: rename bytevector-slice, and move to (letloop bytevector)
  (define subbytes
    (case-lambda
      ((bv start end)
       (unless (<= 0 start end (bytevector-length bv))
         (error 'subbytes "Invalid indices" bv start end))
       (if (and (fxzero? start)
                (fx=? end (bytevector-length bv)))
           bv
           (let ((ret (make-bytevector (fx- end start))))
             (bytevector-copy! bv start
                               ret 0 (fx- end start))
             ret)))
      ((bv start)
       (subbytes bv start (bytevector-length bv)))))

  (define byter-false #x00)
  (define byter-true #x01)
  (define byter-pair #x02)
  (define byter-null #x03)
  (define byter-vector #x04)
  (define byter-vector-end #x05)
  (define byter-bytevector #x06)
  (define byter-string #x07)
  (define byter-symbol #x08)

  ;; before zero ...
  (define byter-zero #x20)
  ;; ... after zero

  (define byter-escape #xFF)

  (define boolean-compare
    (lambda (a b)
      (if (eq? a b)
          'equal
          (if (not a)
              'smaller
              'bigger))))

  (define byter-spec-find
    (lambda (object)
      (find (lambda (spec) ((car spec) object)) byter-spec)))

  (define byter-compare*
    (lambda (object other)
      (let ((object-spec (byter-spec-find object))
            (other-spec (byter-spec-find other)))
        (if (eq? object-spec other-spec)
            (let ((comparator (list-ref object-spec 2)))
              (comparator object other))
            (let ((object-tag (list-ref object-spec 1))
                  (other-tag (list-ref other-spec 1)))
              (integer-compare object-tag other-tag))))))

  (define byter-integer?
    (lambda (x)
      (and (number? x)
           (exact? x)
           (< x (expt 2 64)))))

  (define integer-compare
    (lambda (a b)
      (if (< a b)
          'smaller
          (if (= a b)
              'equal
              'bigger))))

  (define pair-compare
    (lambda (a b)
      (case (byter-compare* (car a) (car b))
        ((smaller) 'smaller)
        ((bigger) 'bigger)
        ((equal) (byter-compare* (cdr a) (cdr b))))))

  (define vector-compare
    (lambda (a b)
      (let ((end (fxmin (vector-length a)
                        (vector-length b))))
        (let loop ((index 0))
          (if (fx=? end index)
              (if (fx=? (vector-length a)
                        (vector-length b))
                  'equal
                  (if (fx<? (vector-length a)
                            (vector-length b))
                      'smaller
                      'bigger))
              (case (byter-compare* (vector-ref a index)
                                    (vector-ref b index))
                ((bigger) 'bigger)
                ((smaller) 'smaller)
                ((equal) (loop (fx+ index 1)))))))))

  (define (byter-compare bytevector other)
    ;; Returns the symbol 'smaller if BYTEVECTOR is before OTHER, if
    ;; they are equal return the symbol 'equal, and otherwise returns
    ;; the symbol 'bigger.
    (let ((end (fxmin (bytevector-length bytevector)
                      (bytevector-length other))))
      (let loop ((index 0))
        (if (fx=? end index)
            ;; BYTEVECTOR and OTHER are equal until index; BYTEVECTOR
            ;; is smaller lexicographically, if it is smaller in
            ;; length.
            (if (fx=? (bytevector-length bytevector)
                      (bytevector-length other))
                'equal
                (if (fx<? (bytevector-length bytevector)
                          (bytevector-length other))
                    'smaller
                    'bigger))
            (let ((delta (fx- (bytevector-u8-ref bytevector index)
                              (bytevector-u8-ref other index))))
              (if (fxzero? delta)
                  (loop (fx+ 1 index))
                  (if (fxnegative? delta)
                      'smaller
                      'bigger)))))))

  (define string-compare
    (lambda (a b)
      (if (string<? a b)
          'smaller
          (if (string=? a b)
              'equal
              'bigger))))

  (define symbol-compare
    (lambda (a b)
      (string-compare (symbol->string a) (symbol->string b))))

  (define byter-spec
    (list (list boolean? byter-false boolean-compare)
          (list byter-integer? byter-zero integer-compare)
          (list pair? byter-pair pair-compare)
          (list null? byter-null (lambda (a b) 'equal))
          (list vector? byter-vector vector-compare)
          (list bytevector? byter-bytevector byter-compare)
          (list string? byter-string string-compare)
          (list symbol? byter-symbol symbol-compare)))

  ;; helpers

  (define bytevector-accumulator
    (lambda ()
      (let ((bytes '())
            (length 0))
        (lambda (maybe-byte)
          (if (eof-object? maybe-byte)
              (let ((out (make-bytevector length)))
                (let loop ((index length)
                           (bytes bytes))
                  (if (fxzero? index)
                      out
                      (let ((index (fx- index 1)))
                        (bytevector-u8-set! out index (car bytes))
                        (loop index (cdr bytes))))))
              (begin
                (set! bytes (cons maybe-byte bytes))
                (set! length (fx+ length 1))))))))

  (define bytevector-for-each
    (lambda (proc bytevector)
      (let loop ((index 0))
        (unless (fx=? index (bytevector-length bytevector))
          (proc (bytevector-u8-ref bytevector index))
          (loop (fx+ index 1))))))

  ;; packing

  (define (byter-bytevector-pack accumulator tag bytevector)
    (accumulator tag)
    (let loop ((index 0))
      (unless (fx=? index (bytevector-length bytevector))
        (let ((byte (bytevector-u8-ref bytevector index)))
          (if (fxzero? byte)
              (begin ;; escape null byte
                (accumulator #x00)
                (accumulator byter-escape))
              (accumulator byte))
          (loop (fx+ index 1)))))
    (accumulator #x00))

  (define byter-string-pack
    (lambda (accumulator object)
      (byter-bytevector-pack accumulator
                             byter-string
                             (string->utf8 object))))

  (define byter-symbol-pack
    (lambda (accumulator object)
      (byter-bytevector-pack accumulator
                             byter-symbol
                             (string->utf8 (symbol->string object)))))

  (define byter-bytevector-unpack
    (lambda (bytevector index)
      (let ((out (bytevector-accumulator)))
        (let loop ((index (fx+ index 1)))
          (if (fxzero? (bytevector-u8-ref bytevector index))
              (cond
               ;; end of input
               ((fx=? (fx+ index 1) (bytevector-length bytevector))
                (values (out (eof-object)) (fx+ index 1)))
               ;; escaped null bytes
               ((fx=? (bytevector-u8-ref bytevector (fx+ index 1)) byter-escape)
                (out #x00)
                (loop (fx+ index 2)))
               ;; end of bytevector
               (else (values (out (eof-object)) (fx+ index 1))))
              ;; just a byte
              (begin
                (out (bytevector-u8-ref bytevector index))
                (loop (fx+ index 1))))))))

  (define byter-string-unpack
    (lambda (bytevector index)
      (call-with-values (lambda () (byter-bytevector-unpack bytevector index))
        (lambda (bytevector index)
          (values (utf8->string bytevector) index)))))

  (define byter-symbol-unpack
    (lambda (bytevector index)
      (call-with-values (lambda () (byter-bytevector-unpack bytevector index))
        (lambda (bytevector index)
          (values (string->symbol (utf8->string bytevector)) index)))))

  (define integer->bytevector
    (lambda (integer)
      (let ((bytevector (make-bytevector 8)))
        (bytevector-u64-set! bytevector 0 integer 'big)
        bytevector)))

  (define bytevector->integer
    (lambda (bytevector)
      (bytevector-u64-ref bytevector 0 'big)))

  (define byter-positive-integer-pack
    (lambda (accumulator integer)
      (define bytevector (integer->bytevector integer))
      ;; There is necessarly a byte that is not zero, because integer
      ;; is not zero.
      (define zero-count-from-the-left
        (let loop ((index 0))
          (if (fxzero? (bytevector-u8-ref bytevector index))
              (loop (fx+ index 1))
              index)))
      (define byter-zero-shift (fx- 8 zero-count-from-the-left))
      (define byter-code (fx+ byter-zero byter-zero-shift))
      (accumulator byter-code)
      (let loop ((index zero-count-from-the-left))
        (unless (fx=? index 8)
          (accumulator (bytevector-u8-ref bytevector index))
          (loop (fx+ index 1))))))

  (define byter-positive-integer-unpack
    (lambda (bytevector index)
      (define out (make-bytevector 8 0))
      (define length (fx- (bytevector-u8-ref bytevector index) byter-zero))
      (define start (fx- 8 length))
      (define end (fx+ index length 1))
      (let loop ((index (fx+ index 1))
                 (other start))
        (unless (fx=? index end)
          (bytevector-u8-set! out other (bytevector-u8-ref bytevector index))
          (loop (fx+ index 1) (fx+ other 1))))
      (values (bytevector->integer out) (fx+ index length 1))))

  (define byter-negative-integer-pack
    (lambda (accumulator integer)
      (define bytevector (integer->bytevector (- integer)))
      ;; There is necessarly a byte that is not zero, because integer
      ;; is not zero.
      (define zero-count-from-the-left
        (let loop ((index 0))
          (if (fxzero? (bytevector-u8-ref bytevector index))
              (loop (fx+ index 1))
              index)))
      (define byter-zero-shift (fx- 8 zero-count-from-the-left))
      (define byter-code (fx- byter-zero byter-zero-shift))
      (accumulator byter-code)
      (let loop ((index zero-count-from-the-left))
        (unless (fx=? index 8)
          (accumulator (bitwise-xor (bytevector-u8-ref bytevector
                                                       index)
                                    #xFF))
          (loop (fx+ index 1))))))

  (define byter-negative-integer-unpack
    (lambda (bytevector index)
      (define out (make-bytevector 8 0))
      (define length (fx- byter-zero (bytevector-u8-ref bytevector index)))
      (define start (fx- 8 length))
      (define end (fx+ index length 1))
      (let loop ((index (fx+ index 1))
                 (other start))
        (unless (fx=? index end)
          (bytevector-u8-set! out other
                              (bitwise-xor (bytevector-u8-ref bytevector index)
                                           #xFF))
          (loop (fx+ index 1) (fx+ other 1))))
      (values (- (bytevector->integer out)) (fx+ index length 1))))

  (define byter-write
    (case-lambda
      ((object)
       (byter-write object (bytevector-accumulator)))
      ((object accumulator)
       (cond
        ((pair? object)
         (accumulator byter-pair)
         (byter-write (car object) accumulator)
         (byter-write (cdr object) accumulator))
        ((eq? object #f) (accumulator byter-false))
        ((eq? object #t) (accumulator byter-true))
        ((null? object) (accumulator byter-null))
        ((bytevector? object) (byter-bytevector-pack accumulator
                                                     byter-bytevector
                                                     object))
        ((and (number? object)
              (exact? object)
              (< (abs object) (expt 2 64)))
         (if (zero? object)
             (accumulator byter-zero)
             (if (positive? object)
                 (byter-positive-integer-pack accumulator object)
                 (byter-negative-integer-pack accumulator object))))
        ((string? object) (byter-string-pack accumulator object))
        ((symbol? object) (byter-symbol-pack accumulator object))
        ((vector? object)
         (accumulator byter-vector)
         (vector-for-each
          (lambda (object) (byter-write object accumulator))
          object)
         (accumulator byter-vector-end))
        (else (error 'byter-write "Unsupported type" object)))
       (accumulator (eof-object)))))

  (define byter-base-unpack
    (lambda (bytevector index)
      (let ((tag (bytevector-u8-ref bytevector index)))
        (case tag
          ((#x00) (values #f (fx+ index 1)))
          ((#x01) (values #t (fx+ index 1)))
          ((#x02) (byter-pair-unpack bytevector index))
          ((#x03) (values '() (fx+ index 1)))
          ((#x04) (byter-vector-unpack bytevector (fx+ index 1) '() 0))
          ((#x06) (byter-bytevector-unpack bytevector index))
          ((#x07) (byter-string-unpack bytevector index))
          ((#x08) (byter-symbol-unpack bytevector index))
          ((#x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F) (byter-negative-integer-unpack bytevector index))
          ((#x20) (values 0 (fx+ index 1)))
          ((#x21 #x22 #x23 #x24 #x25 #x26 #x27 #x28) (byter-positive-integer-unpack bytevector index))
          (else (error 'byter-read "Unsupported type with tag" (number->string tag 16)))))))

  (define byter-read
    (lambda (bytevector)
      (call-with-values (lambda () (byter-base-unpack bytevector 0))
        (lambda (out index) out))))

  (define byter-pair-unpack
    (lambda (bytevector index)
      (call-with-values (lambda () (byter-base-unpack bytevector (fx+ index 1)))
        (lambda (out index)
          (call-with-values (lambda () (byter-base-unpack bytevector index))
            (lambda (out* index)
              (values (cons out out*) index)))))))

  (define byter-vector-unpack
    (lambda (bytevector index out length)

      (define massage
        (lambda (objects length)
          (define out (make-vector length))
          (let loop ((index length)
                     (objects objects))
            (unless (fxzero? index)
              (vector-set! out (fx- index 1) (car objects))
              (loop (fx- index 1) (cdr objects))))
          out))

      (let ((tag (bytevector-u8-ref bytevector index)))
        (if (fx=? tag byter-vector-end)
            (values (massage out length) (fx+ index 1))
            (call-with-values (lambda ()
                                (byter-base-unpack bytevector index))
              (lambda (out* index)
                (byter-vector-unpack bytevector
                                     index
                                     (cons out* out)
                                     (fx+ length 1))))))))

  ;; tests

  (define ~check-byter-000
    (lambda ()
      (eq? #f (byter-read (byter-write #f)))))

  (define ~check-byter-001
    (lambda ()
      (eq? #t (byter-read (byter-write #t)))))

  (define ~check-byter-002
    (lambda ()
      (null? (byter-read (byter-write '())))))

  (define ~check-byter-003
    (lambda ()
      (equal? (bytevector 13 37) (byter-read (byter-write (bytevector 13 37))))))

  (define ~check-byter-004
    (lambda ()
      (let loop ((power 65))
        (if (fxzero? power)
            #t
            (let ((number (- (expt 2 (fx- power 1)) 1)))
              (assert (= number (byter-read (byter-write number))))
              (loop (fx- power 1)))))))

  (define ~check-byter-005
    (lambda ()
      (let loop ((power 65))
        (if (fxzero? power)
            #t
            (let ((number (- (- (expt 2 (fx- power 1)) 1))))
              (assert (= number (byter-read (byter-write number))))
              (loop (fx- power 1)))))))

  (define ~check-byter-006/random
    (lambda ()
      (let loop ((i 1000))
        (if (fxzero? i)
            #t
            (let ((number (random (expt 2 64))))
              (assert (= number (byter-read (byter-write number))))
              (loop (fx- i 1)))))))

  (define ~check-byter-007/random
    (lambda ()
      (let loop ((i 1000))
        (if (fxzero? i)
            #t
            (let ((number (- (random (expt 2 64)))))
              (assert (= number (byter-read (byter-write number))))
              (loop (fx- i 1)))))))

  (define ~check-byter-008
    (lambda ()
      (string=? "azul" (byter-read (byter-write "azul")))))

  (define ~check-byter-009
    (lambda ()
      (eq? 'grenouille (byter-read (byter-write 'grenouille)))))

  (define ~check-byter-010
    (lambda ()
      (equal? (bytevector) (byter-read (byter-write (bytevector))))))

  (define ~check-byter-011
    (lambda ()
      (equal? (bytevector 0) (byter-read (byter-write (bytevector 0))))))

  (define ~check-byter-012
    (lambda ()
      (equal? (bytevector 0 0 0) (byter-read (byter-write (bytevector 0 0 0))))))

  (define ~check-byter-100
    (lambda ()
      (define expected (list 'symbolics
                             #t
                             #f
                             0
                             (bytevector 13 37)
                             "az ul inu"
                             42
                             -42
                             1337
                             -1337
                             (expt 2 32)
                             (- (expt 2 32))
                             (- (expt 2 64) 1)
                             (- (- (expt 2 64) 1))))
      (equal? (byter-read (byter-write expected)) expected)))

  (define ~check-byter-101
    (lambda ()
      (define base (list 'symbolics
                         #t
                         #f
                         0
                         (bytevector 13 37)
                         "az ul inu"
                         42
                         -42
                         1337
                         -1337
                         (expt 2 32)
                         (- (expt 2 32))
                         (- (expt 2 64) 1)
                         (- (- (expt 2 64) 1))))
      (define expected (list->vector base))
      (equal? (byter-read (byter-write expected)) expected)))

  (define ~check-byter-102
    (lambda ()
      (define base (list 'symbolics
                         #t
                         #f
                         0
                         (bytevector 13 37)
                         "az ul inu"
                         42
                         -42
                         1337
                         -1337
                         (expt 2 32)
                         (- (expt 2 32))
                         (- (expt 2 64) 1)
                         (- (- (expt 2 64) 1))))
      (define expected (cons (list->vector base) base))
      (equal? (byter-read (byter-write expected)) expected)))

  (define random-object-max-complexity (expt 10 4))

  (define random-object-complexity (make-parameter random-object-max-complexity))

  (define random-object-exhaustion-singleton (cons 'exhaustion 'sentinel))

  (define random-object-exhaustion
    (lambda ()
      random-object-exhaustion-singleton))

  (define random-object-exhaustion?
    (lambda (object)
      (eq? object random-object-exhaustion-singleton)))

  (define random-bytevector
    (lambda ()

      (define random-byte
        (lambda ()
          (random 256)))

      (let ((length (random (random-object-complexity))))
        (random-object-complexity (fx- (random-object-complexity) length))
        (if (fxzero? length)
            (bytevector)
            (let loop ((out '())
                       (length length))
              (if (fxzero? length)
                  (u8-list->bytevector out)
                  (loop (cons (random-byte) out)
                        (fx- length 1))))))))

  (define random-string
    (lambda ()
      (let loop ()
        (guard (ex (else "byter-string"))
          (utf8->string (random-bytevector))))))

  (define random-symbol
    (lambda ()
      (let loop ()
        (guard (ex (else 'byter-symbol))
          (string->symbol (random-string))))))

  (define random-vector-item
    (lambda (vector)
      (define index (random (vector-length vector)))
      (vector-ref vector index)))

  (define random-integer
    (lambda ()
      ;; TODO: replace (expt 2 64) with a bigger power when bigint are supported
      (* (random-vector-item (vector -1 +1)) (random (expt 2 64)))))

  ;; TODO: add support for inexact numbers?

  (define make-seed
    (lambda ()
      (let* ((now (current-time))
             (seed (* (time-second now) (time-nanosecond now))))
        (+ (modulo seed (expt 2 32)) 1))))

  (define byter-random-object
    (case-lambda
      (() (byter-random-object (make-seed)))
      ((seed)
       (string-append "*** LETLOOP_BYTER_SEED=" (number->string seed))
       (random-seed seed)
       (let loop ()
         (random-object-complexity random-object-max-complexity)
         (let ((object (random-object)))
           (if (random-object-exhaustion? object)
               (loop)
               (values seed object)))))))

  (define random-pair
    (lambda ()
      (if (fx<? (random-object-complexity) 2)
          (random-object-exhaustion)
          (begin
            (random-object-complexity (fx- (random-object-complexity) 2))
            (let ((x (random-object))
                  (y (random-object)))
              (if (or (random-object-exhaustion? x)
                      (random-object-exhaustion? y))
                  (random-object-exhaustion)
                  (cons x y)))))))

  (define random-vector
    (lambda ()
      (let ((length (random (random-object-complexity))))
        (random-object-complexity (fx- (random-object-complexity) length))
        (if (fxzero? length)
            (vector)
            (let loop ((out '())
                       (length length))
              (if (or (fxzero? length)
                      (and (not (null? out))
                           (random-object-exhaustion? (car out))))
                  (apply vector (cdr out))
                  (loop (cons (random-object) out)
                        (fx- length 1))))))))

  (define random-object
    (lambda ()
      (random-object-complexity (fx- (random-object-complexity) 1))
      (if (fx<=? (random-object-complexity) 0)
          (random-object-exhaustion)
          (let ((generator
                 (random-vector-item
                  (vector (lambda () #f)
                          (lambda () #t)
                          (lambda () '())
                          random-integer
                          random-bytevector
                          random-string
                          random-symbol
                          random-pair
                          random-vector
                          ;; without the following generated object
                          ;; will always have a complexity equal to
                          ;; random-object-max-complexity
                          random-object-exhaustion
                          ))))
            (generator)))))

  (define ~check-byter-998/seed
    (lambda ()
      (define seed (string->number (or (getenv "LETLOOP_BYTER_SEED") "1")))
      (call-with-values (lambda () (byter-random-object seed))
        (lambda (seed object)
          (equal? object (byter-read (byter-write object)))))))

  (define ~check-byter-998/random
    (lambda ()
      (let loop ((i (expt 2 (string->number (or (getenv "LETLOOP_BYTER_N") "8")))))
        (if (fxzero? i)
            #t
            (call-with-values (lambda () (byter-random-object))
              (lambda (seed object)
                (assert (equal? object (byter-read (byter-write object))))
                (loop (fx- i 1))))))))

  (define make-comparator
    (lambda (object other)
      (lambda (a b) (eq? (byter-compare* a b)
                         (byter-compare* object other)))))

  (define ~check-byter-999/seed
    (lambda ()
      (define seed (string->number (or (getenv "LETLOOP_BYTER_SEED") "1")))
      (call-with-values (lambda () (byter-random-object seed))
        (lambda (seed object)
          (call-with-values (lambda () (byter-random-object seed))
            (lambda (seed other)
              (let ((comparator (make-comparator object other)))
                (comparator (byter-write object) (byter-write other)))))))))

  (define ~check-byter-999/random
    (lambda ()
      (let loop ((i (expt 2 (string->number (or (getenv "LETLOOP_BYTER_N") "8")))))
        (if (fxzero? i)
            #t
            (let ((seed (make-seed)))
              (call-with-values (lambda () (byter-random-object seed))
                (lambda (seed object)
                  (call-with-values (lambda () (byter-random-object seed))
                    (lambda (seed other)
                      (let ((comparator (make-comparator object other)))
                        (assert (comparator (byter-write object) (byter-write other)))
                        (loop (fx- i 1)))))))))))))
