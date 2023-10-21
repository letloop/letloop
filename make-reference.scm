(import (chezscheme))
(import (letloop blake3))


(define pk
  (lambda args
    (display ";; ")
    (write args)
    (newline)
    (car (reverse args))))    

(define drop-while
  (lambda (objects predicate?)
    (let loop ((objects objects))
      (if (null? objects)
          (list)
          (if (predicate? (car objects))
              (loop (cdr objects))
              objects)))))

(define boring?
  (lambda (object)
    (or (null? object)
        (char=? (car object) #\-))))
 
(define normalized
  (lambda (name)
    (define chars (string->list (string-downcase (symbol->string name))))
    (let loop ((chars chars)
               (out '()))
        (if (null? chars)
            (list->string
             (apply
              append (drop-while (reverse (drop-while out boring?))
                                 boring?)))
            (let ((x (car chars))
                  (previous (and (not (null? out))
                                 (not (null? (car out)))
                                 (caar out))))
              (if (member x (string->list "0123456789abcdefghijklmnopqrstuvwxyz=!"))
                  (loop (cdr chars) (cons (list x) out))
                  (if (or (and previous (char=? previous #\λ))
                          (and previous (char=? previous #\-)))
                      (loop (cdr chars) (cons (list) out))
                      (loop (cdr chars) (cons (list #\-) out)))))))))

(define hexify
  (lambda (x)
    (format #f "~{~x~}" (bytevector->u8-list x))))

(define make-name-hash
  (lambda (name)
    (define hash (number->string
                  (string->number
                   (hexify
                    (blake3 (string->utf8 (symbol->string name)))) 16) 36))
    (define normal (normalized name))
    (if (string=? normal "")
        hash
        (format #f "~a-~a" normal hash))))

(define dollop
  (lambda (name)
    (define name-hash (make-name-hash name))
    (format #t "mkdir -p library/chezscheme/~a/\n" name-hash)
    (format #t "echo \"\n# ~a\n\n\" >> library/chezscheme/~a/index.md\n" name name-hash)))

(for-each dollop (library-exports '(chezscheme)))
