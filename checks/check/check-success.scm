(library (check-success)

  (export answer ~check-000-check-success)
  (import (chezscheme))

  (define answer
    (lambda ()
      42))

  (define ~check-000-check-success
    (lambda ()
      (= (answer) 42))))
