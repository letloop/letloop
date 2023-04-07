(library (check-success)

  (export answer check~check-success-000)
  (import (chezscheme))

  (define answer
    (lambda ()
      42))

  (define check~check-success-000
    (lambda ()
      (assert (= (answer) 42)))))
