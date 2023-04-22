(library (check-fail)

  (export elite ~check-000-check-fail)
  (import (chezscheme))

  (define elite
    (lambda ()
      1337))

  (define ~check-000-check-fail
    (lambda ()
      (assert (= (elite) 42)))))
