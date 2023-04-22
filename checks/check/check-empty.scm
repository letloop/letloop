(library (check-empty)

  (export nop)
  (import (chezscheme))

  (define nop
    (lambda ()
      nop)))
