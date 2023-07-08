(library (letloop gamma)
  (export make-gamma
          gamma
          ~check-gamma-000
          ~check-gamma-001
          ~check-gamma-002
          )
  (import (chezscheme))

  (define letloop-gamma-singleton '(letloop-gamma-singleton))

  (define gamma-apply
    (lambda (name spec args fallback)
      (let loop ((spec spec))
        (if (null? spec)
            (fallback)
            (let ((p? (caar spec))
                  (p (cdar spec)))
              (if (p? args)
                  (apply p args)
                  (loop (cdr spec))))))))

  (define pk
    (lambda args
      (write args)
      (newline)
      (car (reverse args))))

  (define make-gamma
    (case-lambda
      ((name) (make-gamma name
                          (lambda ()
                            (error 'gamma "Gamma implementation not found: ~a"
                                   name))))
      ((name fallback)
       (define specification '())
       (lambda args
         (if (and (pair? args)
                  (eq? letloop-gamma-singleton
                       (car args)))
             (let ((predicate (cadr args))
                   (procedure (caddr args)))
               (set! specification (cons (cons predicate procedure) specification)))
             (gamma-apply name specification args fallback))))))

  (define gamma
    (lambda (g predicate? procedure)
      (g letloop-gamma-singleton predicate? procedure)
      procedure))

  (define ~check-gamma-000
    (lambda ()
      (define magic (make-gamma 'magic))

      (define magic-null? (gamma magic null? (lambda () 'magic-null)))

      (define magic-pair? (gamma magic pair? (lambda args (cons 'magic-pair args))))

      (equal? (magic 1 2 3 'viva 'scheme)
              (cons 'magic-pair
                    (list 1 2 3 'viva 'scheme)))))

  (define ~check-gamma-001
    (lambda ()
      (define magic (make-gamma 'magic))

      (guard (ex (else #t))
        (magic 1 2 3 4 5)
        #f)))

  (define ~check-gamma-002
    (lambda ()
      (define magic (make-gamma 'magic))

      (define magic-null (gamma magic null? (lambda () 'magic-null)))

      (define magic-length? (gamma magic (lambda args (= (length args) 42))
                                   (lambda args (cons 'magic-pair args))))

      (guard (ex (else #t))
        (magic 42)
        #f))))
