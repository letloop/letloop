(library (check-success)

  (export answer ~check-000-check-success ~benchmark-000)
  (import (chezscheme))

  (define answer
    (lambda ()
      42))

  (define ~benchmark-000
    (lambda ()
      (sleep (make-time 'time-duration 1 1))))

  (define ~check-000-check-success
    (lambda ()
      (= (answer) 42))))
