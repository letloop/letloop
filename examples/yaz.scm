(import (chezscheme))


(define transliteration
  (let ((mapping (list
                  (cons "ⵄ" "ya2")
                  (cons "ⵉ" "yi")
                  (cons "ⵎ" "yam")
                  (cons "ⵓ" "you")
                  (cons "ⵔ" "yar")
                  (cons "ⵛ" "yach")

                  )))
    (map (lambda (x) (cons (string->list (cdr x)) (car x))) mapping)))


(define lookup
  (lambda (x* c*)
    (let loopx ((x* x*))
      (if (null? x*)
          #f
          (let ((e* (car (car x*)))
                (t (cdr (car x*))))
            (let loopy ((c* c*)
                        (e* e*))

              (cond
               ((and (null? c*) (null? e*)) t)
               ((null? e*) (loopx (cdr x*)))
               ((null? c*) (loopx (cdr x*)))
               (else
                (if (char=? (car c*) (car e*))
                    (loopy (cdr c*) (cdr e*))
                    (loopx (cdr x*)))))))))))

(define pk
  (lambda args
    (newline)
    (display ";; ") (write args) (newline)
    (car (reverse args))))

(newline)
(let loop ((line '())
           (buffer '()))
  (newline)
  (unless (null? line)
    (display "text: ") (display (apply string-append (reverse line))) (newline))
  (unless (null? buffer)
    (display "keys: ") (display (list->string (reverse buffer))) (newline))
  (display "one key press then [enter] > ")
  (let ((char (read-char)))
    (cond
     ((eof-object? char) (exit #t))
     ((char=? char #\newline)
      (loop line buffer))
     ((char=? char #\space)
      (let ((tifinigh (lookup transliteration (reverse buffer))))
        (if tifinigh
            (loop (cons tifinigh line)
                  '())
            (loop line buffer))))
     (else
      (loop line (cons char buffer))))))
