(library (codex base)
  (export codex-usage)
  (import (chezscheme))

  (define codex-usage
    (lambda ()
      (write '(define codex (lambda () codex)))
      (newline)
      (flush-output-port))))
