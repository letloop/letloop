(library (check-failure)

  (export elite check~check-failure-000)
  (import (chezscheme))

  (define elite
    (lambda ()
      1337))

  (define check~check-failure-000
    (lambda ()
      (assert (= (elite) 42)))))
