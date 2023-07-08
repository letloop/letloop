(import (chezscheme))
(import (letloop byter))
(import (letloop dxdb lbst))


(define make-random-lbst
  (lambda (seed n m)

    (define random-bytevector
      (lambda (length)
        (define bytevector (make-bytevector 1 #;(fx+ (random length) 1)))
        (let loop ((length (bytevector-length bytevector)))
          (unless (fxzero? length)
            (bytevector-u8-set! bytevector (fx- length 1) (random 100 #;256))
            (loop (fx- length 1))))
        bytevector))

    (define random-choice
      (lambda (xs)
        (list-ref xs (random (length xs)))))

    (define make
      (lambda ()

        (let loop ((lbst (make-lbst))
                   (n n))
          (if (fxzero? n)
              lbst
              (let ((key (random-bytevector (fx+ 1 (random m))))
                    (value (random-bytevector 1)))
                (loop (lbst-set lbst key value)
                      (fx- n 1)))))))

    (random-seed seed)

    (let loop ((lbst (make)))
      (define alist (lbst->alist lbst))
      (define start (car (random-choice alist)))
      (define end (car (random-choice alist)))
      (case (byter-compare start end)
        (equal (loop (make)))
        (smaller (values lbst start end))
        (bigger (values lbst end start))))))

(define (main)

  (define seed (string->number
                (or (getenv "SEED")
                    (number->string
                     (modulo
                      (time-nanosecond (current-time))
                      (expt 2 32))))))

  (define within?
    (lambda (a b c)
      (case (byter-compare b a)
        (smaller #f)
        (else (case (byter-compare b c)
                (bigger #f)
                ((smaller equal) #t))))))

  (define lbst-key-name
    (lambda (lbst)
      (let ((string (format #f "\"~a\"" (lbst-key lbst))))
        (format #f "\"~a\"" (substring string 6 (- (string-length string) 2))))))

  (define-values (lbst start end) (make-random-lbst seed 10 3))

  (display "digraph g {")

  (let loop ((todo (list lbst)))
    (unless (null? todo)
      (let ((lbst (car todo)))
        (if (not lbst)
            (loop (cdr todo))
            (begin
              (when (within? start (lbst-key lbst) end)
                (display "\t")
                (display (lbst-key-name lbst))
                (if (or (equal? start (lbst-key lbst))
                        (equal? end (lbst-key lbst)))
                    (display " [color=\"red\"]")
                    (display " [color=\"green\"]"))
                (display ";")
                (newline))

              (when (lbst-left lbst)
                (display "\t")
                (display (lbst-key-name lbst))
                (display " -> ")
                (display (lbst-key-name (lbst-left lbst)))
                (display " [label=left]")
                (display ";")
                (newline))

              (when (lbst-right lbst)
                (display "\t")
                (display (lbst-key-name lbst))
                (display " -> ")
                (display (lbst-key-name (lbst-right lbst)))
                (display " [label=right]")
                (display ";")
                (newline))

              (loop (cons* (lbst-left lbst)
                           (lbst-right lbst)
                           (cdr todo))))))))

  (display "\n}")
  (flush-output-port)
  (newline))

(main)
