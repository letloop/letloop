(library (check-error)

  (export ~check-000-error)
  (import (chezscheme))

  (define ~check-000-error
    (lambda ()
      (error 'check-error "a synthetic error"))))
